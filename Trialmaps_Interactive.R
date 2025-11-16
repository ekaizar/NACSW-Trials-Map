library(rvest)
library(xml2)
library(dplyr)
library(tidyr)
library(tidygeocoder)

# URL of the website
url <- "https://nacsw.net/calendar/trials"

# Read the HTML content from the website
cat("Scraping NACSW website...\n")
webpage <- read_html(url)

# Extract dates
trialdates <- webpage %>% html_nodes(".event-start")
trialdates <- xml_text(trialdates)

if(length(trialdates) == 0) {
  stop("Error: No trial dates found. The website structure may have changed.")
}
cat("✓ Found", length(trialdates), "trial dates\n")

# Extract event names
trialinfo <- webpage %>% html_nodes(".event-name")
trialinfo_text <- xml_text(trialinfo)

if(length(trialinfo_text) == 0) {
  stop("Error: No trial information found. The website structure may have changed.")
}
cat("✓ Found", length(trialinfo_text), "events\n")

# Extract event links from hidden detail rows
cat("Extracting event links...\n")

# The website structure has:
# 1. Visible row with event-id in the toggler link
# 2. Hidden row with class="event{ID}" containing the premium link

# First, get all event IDs from the toggler links
toggler_nodes <- webpage %>% html_nodes("a.toggler")
event_ids <- toggler_nodes %>% html_attr("event-id")

cat("  Found", length(event_ids), "event IDs\n")

# Now extract the premium link from each hidden detail row
triallinks <- sapply(event_ids, function(event_id) {
  # Find the hidden row for this event
  row_selector <- paste0(".event", event_id)
  detail_row <- webpage %>% html_nodes(row_selector)
  
  if(length(detail_row) > 0) {
    # Look for the link after "Premium details" text
    all_links <- detail_row %>% html_nodes("a") %>% html_attr("href")
    
    # Filter out mailto links and get the first http link
    http_links <- all_links[grepl("^http", all_links, ignore.case = TRUE)]
    
    if(length(http_links) > 0) {
      # Return the first http link (should be the premium details link)
      return(http_links[1])
    }
  }
  
  # If no link found, return NA
  return(NA)
})

# Count successful extractions
successful_links <- sum(!is.na(triallinks) & triallinks != "")
cat("✓ Extracted", successful_links, "specific event links\n")

# For events without specific links, use the calendar page
triallinks[is.na(triallinks) | triallinks == ""] <- "https://nacsw.net/calendar/trials"

default_count <- sum(triallinks == "https://nacsw.net/calendar/trials")
cat("  Specific event links:", successful_links, "\n")
cat("  Default calendar links:", default_count, "\n")

# Verify we have the right number of links
if(length(triallinks) != length(trialinfo_text)) {
  cat("⚠ Warning: Link count mismatch\n")
  cat("  Events:", length(trialinfo_text), "\n")
  cat("  Links:", length(triallinks), "\n")
  
  # Adjust if needed
  if(length(triallinks) < length(trialinfo_text)) {
    triallinks <- c(triallinks, rep("https://nacsw.net/calendar/trials", 
                                   length(trialinfo_text) - length(triallinks)))
  } else if(length(triallinks) > length(trialinfo_text)) {
    triallinks <- triallinks[1:length(trialinfo_text)]
  }
}

# Parse trial information
trialtype <- sub(" Trial[s]? -.*", "", trialinfo_text)
triallocation <- sub(".* Trial[s]? - (.*?) hosted by.*", "\\1", trialinfo_text)
trialhost <- sub(".* hosted by ", "", trialinfo_text)

# Create initial data frame
trials_df <- data.frame(
  Date = trialdates, 
  Type = trialtype, 
  Location = triallocation, 
  Host = trialhost,
  EventLink = triallinks,
  stringsAsFactors = FALSE
)

# Remove cancelled trials
trials_df <- trials_df %>% filter(!grepl("TRIAL CANCELLED", Type))

# Separate trial types
df_separated <- trials_df %>% 
  separate(Type, into = c("Type1", "Type2", "Type3", "Type4", "Type5"), 
           sep = "/", fill = "right")

# Convert to long format
df_long <- df_separated %>%
  pivot_longer(cols = starts_with("Type"), names_to = "Type", values_to = "TrialType") %>%
  select(-Type) %>% 
  filter(!is.na(TrialType)) %>% 
  distinct()

# Geocode locations with robust error handling
cat("\nGeocoding locations...\n")

# Get unique locations to geocode
unique_locations <- unique(df_long$Location)
cat("(Processing", length(unique_locations), "unique locations - this may take 5-10 minutes)\n")
cat("Note: Using free geocoding service - please be patient\n\n")

# Check for cached geocoding results
cache_file <- "geocode_cache.csv"
if(file.exists(cache_file)) {
  cat("Found geocoding cache file - loading previous results\n")
  geocode_lookup <- read.csv(cache_file, stringsAsFactors = FALSE)
  cached_locations <- geocode_lookup$Location
  locations_to_geocode <- setdiff(unique_locations, cached_locations)
  cat("  Cached locations:", length(cached_locations), "\n")
  cat("  New locations to geocode:", length(locations_to_geocode), "\n")
} else {
  geocode_lookup <- data.frame(
    Location = character(),
    latitude = numeric(),
    longitude = numeric(),
    stringsAsFactors = FALSE
  )
  locations_to_geocode <- unique_locations
}

# Function to geocode with retry logic
geocode_with_retry <- function(location, max_retries = 3) {
  for(attempt in 1:max_retries) {
    tryCatch({
      # Add delay to respect rate limits (1 request per second)
      Sys.sleep(1.1)
      
      result <- geocode(
        data.frame(address = location), 
        address, 
        method = 'osm',
        quiet = TRUE
      )
      
      if(!is.na(result$lat) && !is.na(result$long)) {
        return(list(lat = result$lat, long = result$long, success = TRUE))
      } else {
        if(attempt < max_retries) {
          cat("  Retry", attempt, "for:", location, "\n")
          Sys.sleep(2)  # Wait longer before retry
        }
      }
    }, error = function(e) {
      if(attempt < max_retries) {
        cat("  Error on attempt", attempt, "for:", location, "- retrying...\n")
        Sys.sleep(3)  # Wait even longer after error
      } else {
        cat("  ✗ Failed to geocode:", location, "\n")
      }
    })
  }
  
  return(list(lat = NA, long = NA, success = FALSE))
}

# Geocode each unique location with progress
if(length(locations_to_geocode) > 0) {
  cat("\nGeocoding", length(locations_to_geocode), "new locations:\n")
  failed_locations <- c()
  
  for(i in 1:length(locations_to_geocode)) {
    location <- locations_to_geocode[i]
    
    # Show progress
    if(i %% 10 == 0 || i == 1) {
      cat(sprintf("Progress: %d/%d (%.1f%%)\n", i, length(locations_to_geocode), i/length(locations_to_geocode)*100))
    }
    
    # Geocode with retry
    result <- geocode_with_retry(location)
    
    if(result$success) {
      geocode_lookup <- rbind(geocode_lookup, data.frame(
        Location = location,
        latitude = result$lat,
        longitude = result$long,
        stringsAsFactors = FALSE
      ))
      
      # Save cache every 10 locations
      if(i %% 10 == 0) {
        write.csv(geocode_lookup, cache_file, row.names = FALSE)
      }
    } else {
      failed_locations <- c(failed_locations, location)
    }
  }
  
  # Save final cache
  write.csv(geocode_lookup, cache_file, row.names = FALSE)
  cat("\n✓ Geocoding cache saved to", cache_file, "\n")
} else {
  cat("All locations already cached - skipping geocoding!\n")
}

cat("✓ Geocoding complete\n")
cat("  Successfully geocoded:", nrow(geocode_lookup), "locations\n")

# Join geocoded coordinates back to the main data
df_geocoded <- df_long %>%
  left_join(geocode_lookup, by = "Location")

# Check for geocoding failures
failed_geocoding <- sum(is.na(df_geocoded$latitude))
if(failed_geocoding > 0) {
  failed_locs <- unique(df_geocoded$Location[is.na(df_geocoded$latitude)])
  cat("⚠ Warning:", failed_geocoding, "rows could not be geocoded.\n")
  cat("  Failed locations (", length(failed_locs), "unique):\n", sep="")
  for(loc in head(failed_locs, 10)) {
    cat("    -", loc, "\n")
  }
  if(length(failed_locs) > 10) {
    cat("    ... and", length(failed_locs) - 10, "more\n")
  }
  cat("  These events will be excluded from the map.\n")
  
  # Remove rows that couldn't be geocoded
  df_geocoded <- df_geocoded %>% filter(!is.na(latitude) & !is.na(longitude))
  cat("  Continuing with", nrow(df_geocoded), "successfully geocoded events\n")
}

# Group events by trial (Location + Date + Host)
# Each trial gets one pin with all event types listed
cat("\nGrouping events by trial...\n")
df_trials <- df_geocoded %>%
  group_by(Date, Location, Host, EventLink, latitude, longitude) %>%
  summarise(
    TrialTypes = paste(sort(unique(TrialType)), collapse=", "),
    EventCount = n(),
    .groups = 'drop'
  ) %>%
  mutate(
    Title = paste0(Location, " - ", Date)
  )

cat("✓ Grouped into", nrow(df_trials), "unique trials from", sum(df_trials$EventCount), "events\n")

# Function to add circular jitter for overlapping points
# MASSIVELY INCREASED JITTER for state-level visibility
add_smart_jitter <- function(df) {
  df %>%
    group_by(Location) %>%  # Group by location only (not date)
    mutate(
      n_at_location = n(),
      event_index = row_number() - 1,
      # VERY LARGE jitter for state-level visibility
      # These values are ~10x larger than before
      jitter_radius = case_when(
        n_at_location == 1 ~ 0,
        n_at_location <= 4 ~ 0.05,   # ~5.5 km (was 55m)
        n_at_location <= 8 ~ 0.07,   # ~7.7 km (was 77m)
        TRUE ~ 0.10                   # ~11 km (was 111m)
      ),
      # Arrange in a circle around the original point
      angle = (2 * pi * event_index) / n_at_location,
      # Apply jitter
      Latitude = latitude + jitter_radius * cos(angle),
      Longitude = longitude + jitter_radius * sin(angle)
    ) %>%
    ungroup() %>%
    select(-n_at_location, -event_index, -jitter_radius, -angle, -latitude, -longitude)
}

# Add jitter to overlapping locations
cat("\nAdding large jitter for state-level visibility...\n")
df_jittered <- add_smart_jitter(df_trials)
cat("✓ Jitter applied\n")

# Create final dataset for Google Sheets
cat("\nCreating final dataset...\n")
df_final <- df_jittered %>%
  mutate(
    # Format date as YYYY-MM-DD for better filtering
    Date = as.character(Date)
  ) %>%
  select(Date, Location, Host, EventLink, Title, TrialTypes, EventCount, Latitude, Longitude) %>%
  arrange(Date, Location)

# Write CSV for Google Sheets upload
write.csv(df_final, file="trials_for_google_sheets.csv", row.names = FALSE)
cat("✓ CSV file created\n")
cat("✓ CSV file created\n")

cat("\n" , paste(rep("=", 60), collapse=""), "\n")
cat("✅ SUCCESS! Data collection complete.\n")
cat(paste(rep("=", 60), collapse=""), "\n\n")

cat("📊 Summary:\n")
cat("  • Total trials:", nrow(df_final), "\n")
cat("  • Total events:", sum(df_final$EventCount), "\n")
cat("  • Date range:", min(df_final$Date), "to", max(df_final$Date), "\n")
cat("  • Unique locations:", length(unique(df_final$Location)), "\n")

cat("\n📋 Next Steps:\n")
cat("  1. Open your Google Sheet (or create a new one)\n")
cat("  2. File → Import → Upload → trials_for_google_sheets.csv\n")
cat("  3. Choose 'Replace current sheet'\n")
cat("  4. Click 'Import data'\n")
cat("  5. Your map will automatically update!\n\n")

cat("💡 First time setup? See SETUP_GUIDE.md for detailed instructions.\n")
cat("📍 Note: One pin per trial, with all event types listed.\n")
cat("🔍 Jitter increased 10x for state-level visibility!\n\n")
