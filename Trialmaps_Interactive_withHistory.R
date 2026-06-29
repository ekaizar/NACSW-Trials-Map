################################################################################
# NACSW Trials Map Data Collection Script
# Updated 2026: now captures COMPLETED (past) trials as well as upcoming ones.
#
# Why this update:
#   /calendar/trials already lists every trial back to 2012 on a single page,
#   sorted newest-first. Upcoming trials render in a "rich" format (a main row
#   plus a detail row with the event website link). Completed trials collapse
#   to a single bare row: <tr class="outertr"><div class="views-row">
#   <td>DATE</td><td>EVENT TEXT</td></div></tr> -- no classes, no link.
#
#   The previous parser only matched td.event-start / td.event-name, so it saw
#   upcoming trials only. This version detects an event row by "first cell looks
#   like a YYYY-MM-DD date," which works for BOTH layouts from the same single
#   request. No search form, no pagination, no per-year fetching needed.
################################################################################

# Load required libraries
library(rvest)
library(dplyr)
library(tidyr)
library(tidygeocoder)

# Set options
options(timeout = 300)

################################################################################
# CONFIGURATION
################################################################################

# How far back to keep trials. The page contains everything to 2012, but the
# map only needs ~1 year of history (its default window is 1 year past ->
# 2 years future), and geocoding 14 years of unique locations on the first run
# would be very slow against OSM/Nominatim. 2 years gives a comfortable buffer.
#
# To pull the ENTIRE archive instead, set YEARS_BACK <- 100 (expect a long
# first run while new locations are geocoded; subsequent runs use the cache).
YEARS_BACK <- 2

# Drop trials whose listing is marked "TRIAL CANCELLED".
DROP_CANCELLED <- TRUE

cat("\n============================================================\n")
cat("🗺️  NACSW Trials Map - Data Collection\n")
cat("============================================================\n\n")

################################################################################
# STEP 1: SCRAPE TRIAL DATA FROM NACSW WEBSITE
################################################################################

cat("📥 Step 1: Scraping trial data from NACSW website...\n")

# URL of the NACSW trials calendar.
# No trailing slash: that form serves the calendar directly (the trailing-slash
# variant can add a redirect hop).
url <- "https://www.nacsw.net/calendar/trials"

# Fetch the page. The NACSW server tends to serve a different (non-calendar)
# response to generic library user-agents, so request with a browser-like UA.
ua <- paste0("Mozilla/5.0 (Windows NT 10.0; Win64; x64) ",
             "AppleWebKit/537.36 (KHTML, like Gecko) ",
             "Chrome/124.0.0.0 Safari/537.36")

# The page now lists the FULL archive (back to 2012), which the server can be
# slow to render -- a 60s timeout often gives "0 bytes received". Allow plenty
# of time and retry a few times on a slow/transient attempt.
FETCH_TIMEOUT <- 240   # seconds per attempt
FETCH_TRIES   <- 4     # attempts before giving up

webpage <- tryCatch({
  if (requireNamespace("httr", quietly = TRUE)) {
    resp <- httr::RETRY(
      "GET", url,
      httr::user_agent(ua),
      httr::add_headers(
        Accept = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
        `Accept-Language` = "en-US,en;q=0.9"
      ),
      httr::timeout(FETCH_TIMEOUT),
      times      = FETCH_TRIES,
      pause_base = 5,    # backoff grows between attempts
      pause_cap  = 30,
      quiet      = FALSE
    )
    httr::stop_for_status(resp)
    read_html(httr::content(resp, as = "text", encoding = "UTF-8"))
  } else {
    # Fallback if httr isn't installed
    options(timeout = max(getOption("timeout"), FETCH_TIMEOUT))
    read_html(url)
  }
}, error = function(e) {
  stop("❌ Error: Could not retrieve the NACSW page after ", FETCH_TRIES,
       " attempts: ", conditionMessage(e),
       "\n   The full-archive page can be slow; try re-running, or lower the ",
       "history load with a larger YEARS_BACK only if that helps the server.")
})

# --- Helpers ---------------------------------------------------------------

# Matches a date at the start of a cell, e.g. "2026-06-27".
date_pat <- "^\\s*\\d{4}-\\d{2}-\\d{2}"

# Get the trimmed text of every descendant <td> of a row. Using descendant
# (not direct-child) selection is essential: completed-trial rows nest their
# <td> cells inside a <div class="views-row">, so the cells are one level
# deeper than in upcoming rows. rvest's html_elements(row, "td") returns
# descendants, which covers both layouts.
td_text_vec <- function(row) {
  tds <- row %>% html_elements("td")
  if (length(tds) == 0) return(character(0))
  vapply(tds, function(td) trimws(html_text2(td)), character(1))
}

# --- Walk the rows ---------------------------------------------------------

all_rows <- webpage %>% html_elements("tr")

# Diagnostics: confirm we actually received the calendar page.
page_title <- webpage %>% html_element("title") %>% html_text2()
n_event_rows <- sum(vapply(all_rows, function(r) {
  t <- td_text_vec(r); length(t) >= 2 && grepl(date_pat, t[1])
}, logical(1)))

cat("  • Page title:", page_title, "\n")
cat("  • <tr> rows found:", length(all_rows), "\n")
cat("  • date-led event rows found:", n_event_rows, "\n")

if (n_event_rows == 0) {
  cat("\n⚠️  No date-led event rows were found. Either the page layout\n")
  cat("    changed, or the server returned a non-calendar / blocked page.\n")
  cat("    First 2000 characters of what was received:\n\n")
  cat(substr(as.character(webpage), 1, 2000), "\n\n")
  stop("❌ Error: No events found. Website structure may have changed again, ",
       "or access was blocked.")
}

trials_raw  <- list()
trial_index <- 0
n_rows <- length(all_rows)

i <- 1
while (i <= n_rows) {
  cells <- td_text_vec(all_rows[[i]])

  # An event row (either layout) starts with a date in its first cell and has
  # at least a second cell holding the event text.
  if (length(cells) >= 2 && grepl(date_pat, cells[1])) {

    date       <- substr(trimws(cells[1]), 1, 10)   # keep just YYYY-MM-DD
    event_text <- trimws(cells[2])

    # Find the event website link. For upcoming trials it lives in the detail
    # row(s) that follow this main row; completed trials have none. Scan
    # forward until the next event row, taking the first real http(s) link
    # (ignoring the "#" placeholder in the name cell and any mailto: links).
    actual_event_link <- NA_character_
    j <- i + 1
    while (j <= n_rows) {
      jcells <- td_text_vec(all_rows[[j]])
      if (length(jcells) >= 2 && grepl(date_pat, jcells[1])) break  # next event

      links <- all_rows[[j]] %>% html_elements("a") %>% html_attr("href")
      for (link in links) {
        if (!is.na(link) && grepl("^https?://", link) && !grepl("^mailto:", link)) {
          actual_event_link <- link
          break
        }
      }
      if (!is.na(actual_event_link)) break
      j <- j + 1
    }

    trial_index <- trial_index + 1
    trials_raw[[trial_index]] <- data.frame(
      Date      = date,
      EventText = event_text,
      EventLink = actual_event_link,
      stringsAsFactors = FALSE
    )
  }

  i <- i + 1
}

if (length(trials_raw) == 0) {
  stop("❌ Error: Found date-led rows but parsed 0 events.")
}

trials_raw <- bind_rows(trials_raw)

# Remove any empty rows
trials_raw <- trials_raw %>%
  filter(!is.na(Date), !is.na(EventText), EventText != "")

cat("✓ Scraped", nrow(trials_raw), "events from website\n\n")

################################################################################
# STEP 2: PARSE AND CLEAN DATA
################################################################################

cat("🧹 Step 2: Parsing and cleaning data...\n")

# Drop cancelled trials (the listing prefixes the name with "TRIAL CANCELLED").
if (DROP_CANCELLED) {
  n_before <- nrow(trials_raw)
  trials_raw <- trials_raw %>%
    filter(!grepl("TRIAL CANCELLED", EventText, ignore.case = TRUE))
  cat("  • Removed", n_before - nrow(trials_raw), "cancelled trials\n")
}

# Parse event text. Format: "Trial Types - Location hosted by Host"
# Example: "NW3/ELT Trials - Waymart, PA hosted by Your Dog's Place, LLC"
trials_clean <- trials_raw %>%
  mutate(
    # Clean date format
    Date = as.Date(Date),

    # Strip any stray "TRIAL CANCELLED" text if cancelled rows were kept
    EventText = trimws(gsub("TRIAL CANCELLED", "", EventText, ignore.case = TRUE)),

    # Extract trial types (everything before " - ")
    TrialTypes_Raw = sub(" -.*", "", EventText),

    # Clean up trial types - remove " Trials" and normalize separators
    TrialTypes = gsub(" Trials?", "", TrialTypes_Raw),
    TrialTypes = gsub("/", ", ", TrialTypes),
    TrialTypes = trimws(TrialTypes),

    # Extract location and host (everything after " - ")
    LocationHost = sub(".*? - ", "", EventText),

    # Extract location (everything before " hosted by")
    Location = sub(" hosted by.*", "", LocationHost),
    Location = trimws(Location),

    # Extract host (everything after "hosted by ")
    Host = sub(".*hosted by ", "", LocationHost),
    Host = trimws(Host),

    # Count number of trial types
    EventCount = lengths(regmatches(TrialTypes, gregexpr(",", TrialTypes))) + 1
  ) %>%
  select(-EventText, -TrialTypes_Raw, -LocationHost) %>%
  filter(!is.na(Date), !is.na(Location), Location != "")

# Keep only trials within the configured look-back window (future always kept).
earliest_date <- Sys.Date() - 365L * YEARS_BACK
n_before <- nrow(trials_clean)
trials_clean <- trials_clean %>% filter(Date >= earliest_date)
cat("  • Date cutoff:", format(earliest_date), "(keeping", YEARS_BACK,
    "years back + all future)\n")
cat("  • Dropped", n_before - nrow(trials_clean), "trials older than cutoff\n")

cat("✓ Cleaned", nrow(trials_clean), "valid trials\n")
cat("  • Date range:", format(min(trials_clean$Date)), "to",
    format(max(trials_clean$Date)), "\n")
cat("  • Unique locations:", length(unique(trials_clean$Location)), "\n\n")

################################################################################
# STEP 3: GEOCODE LOCATIONS (WITH USA SUFFIX)
################################################################################

cat("📍 Step 3: Geocoding locations...\n")

# Prepare location strings with ", USA" suffix for accurate geocoding
trials_clean <- trials_clean %>%
  mutate(
    Location_Geocode = case_when(
      grepl("USA|United States|U.S.A.|U.S.", Location, ignore.case = TRUE) ~ Location,
      TRUE ~ paste0(Location, ", USA")
    )
  )

cat("  • Added USA suffix to",
    sum(!grepl("USA|United States", trials_clean$Location, ignore.case = TRUE)),
    "locations\n")

# Load existing geocode cache (if it exists)
cache_file <- "geocode_cache.csv"

if (file.exists(cache_file)) {
  geocode_cache <- read.csv(cache_file, stringsAsFactors = FALSE)
  cat("  • Loaded", nrow(geocode_cache), "cached locations\n")
} else {
  geocode_cache <- data.frame(
    Location = character(), lat = numeric(), long = numeric(),
    stringsAsFactors = FALSE
  )
  cat("  • No cache found, will geocode all locations\n")
}

# Find locations that need geocoding
locations_to_geocode <- trials_clean %>%
  distinct(Location_Geocode) %>%
  filter(!Location_Geocode %in% geocode_cache$Location)

if (nrow(locations_to_geocode) > 0) {
  cat("  • Geocoding", nrow(locations_to_geocode), "new locations...\n")
  cat("    (Pulling past trials adds historical towns; the FIRST run after\n")
  cat("     this update may take a while. Cached afterward.)\n")

  new_coords <- locations_to_geocode %>%
    geocode(Location_Geocode, method = 'osm', verbose = FALSE)

  # tidygeocoder may return the longitude column as 'lon' depending on version.
  # Standardize to 'long' so new cache rows match the existing cache schema.
  if ("lon" %in% names(new_coords) && !("long" %in% names(new_coords))) {
    new_coords <- new_coords %>% rename(long = lon)
  }
  
  failed_geocodes <- new_coords %>% filter(is.na(lat) | is.na(long))
  if (nrow(failed_geocodes) > 0) {
    cat("  ⚠️  Warning:", nrow(failed_geocodes), "locations failed to geocode:\n")
    print(failed_geocodes$Location_Geocode)
  }

  new_coords <- new_coords %>% rename(Location = Location_Geocode)
  geocode_cache <- bind_rows(geocode_cache, new_coords)
  write.csv(geocode_cache, cache_file, row.names = FALSE)
  cat("  ✓ Geocoded and cached", nrow(new_coords), "new locations\n")
} else {
  cat("  ✓ All locations already in cache\n")
}

# Join coordinates to trials
trials_with_coords <- trials_clean %>%
  left_join(geocode_cache, by = c("Location_Geocode" = "Location")) %>%
  rename(Latitude = lat, Longitude = long)

missing_coords <- trials_with_coords %>% filter(is.na(Latitude) | is.na(Longitude))
if (nrow(missing_coords) > 0) {
  cat("  ⚠️  Warning:", nrow(missing_coords), "trials have missing coordinates\n")
}

cat("\n")

################################################################################
# STEP 4: GROUP TRIALS BY DATE AND LOCATION
################################################################################

cat("📦 Step 4: Grouping trials by date and location...\n")

JITTER_AMOUNT <- 0.05  # About 3-5 miles, visible at state level

trials_grouped <- trials_with_coords %>%
  filter(!is.na(Latitude), !is.na(Longitude)) %>%
  group_by(Date, Location) %>%
  summarize(
    Host      = first(Host),
    EventLink = first(EventLink),
    # Combine all event types across rows for this date+location, de-duplicated
    # (e.g. "NW3, ELT" + "ELT" -> "NW3, ELT"). unique() preserves first-seen
    # order, so level ordering is retained.
    TrialTypes = {
      t <- trimws(unlist(strsplit(TrialTypes, ",\\s*")))
      paste(unique(t[t != ""]), collapse = ", ")
    },
    Latitude_Base  = first(Latitude),
    Longitude_Base = first(Longitude),
    .groups = 'drop'
  ) %>%
  mutate(
    EventCount = lengths(strsplit(TrialTypes, ",\\s*")),
    Latitude   = Latitude_Base  + runif(n(), -JITTER_AMOUNT, JITTER_AMOUNT),
    Longitude  = Longitude_Base + runif(n(), -JITTER_AMOUNT, JITTER_AMOUNT)
  ) %>%
  select(-Latitude_Base, -Longitude_Base)

cat("✓ Grouped into", nrow(trials_grouped), "unique trials (one pin per trial)\n")
cat("  • Total event types:", sum(trials_grouped$EventCount), "\n\n")

################################################################################
# STEP 5: CREATE FINAL DATASET
################################################################################

cat("💾 Step 5: Creating final dataset...\n")

trials_final <- trials_grouped %>%
  select(Date, Location, Host, EventLink, TrialTypes, EventCount, Latitude, Longitude) %>%
  arrange(Date, Location)

date_range <- range(trials_final$Date)
unique_locations <- length(unique(trials_final$Location))

cat("✓ Final dataset ready\n")
cat("  • Trials:", nrow(trials_final), "\n")
cat("  • Date range:", format(date_range[1]), "to", format(date_range[2]), "\n")
cat("  • Unique locations:", unique_locations, "\n\n")

################################################################################
# STEP 6: SAVE TO CSV
################################################################################

cat("💾 Step 6: Saving to CSV file...\n")

output_file <- "trials_for_google_sheets.csv"
write.csv(trials_final, output_file, row.names = FALSE)

cat("✓ CSV file created:", output_file, "\n")
cat("  • File size:", file.size(output_file), "bytes\n\n")

################################################################################
# COMPLETION SUMMARY
################################################################################

cat("============================================================\n")
cat("✅ SUCCESS! Data collection complete.\n")
cat("============================================================\n\n")

cat("📊 Summary:\n")
cat("  • Total trials:", nrow(trials_final), "\n")
cat("  • Total events:", sum(trials_final$EventCount), "\n")
cat("  • Date range:", format(date_range[1]), "to", format(date_range[2]), "\n")
cat("  • Unique locations:", unique_locations, "\n\n")

cat("💡 Notes:\n")
cat("  • Now parses BOTH upcoming (rich) and completed (bare) trial rows.\n")
cat("  • Past trials have no event website link (EventLink = NA).\n")
cat("  • YEARS_BACK =", YEARS_BACK, "controls how far back history is kept.\n")
cat("  • Cancelled trials", if (DROP_CANCELLED) "are removed." else "are kept.", "\n\n")
