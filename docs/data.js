const DATA_UPDATED = "August 22, 2026";
const TRIALS_DATA = 
[
  {
    "Date": "2024-08-24",
    "Location": "Johnstown, NY",
    "Host": "My Dog Smells LLC",
    "TrialTypes": "ELT, NW1",
    "EventCount": 2,
    "Latitude": 43.0501,
    "Longitude": -74.3822
  },
  {
    "Date": "2024-08-29",
    "Location": "White Plains, NY",
    "Host": "For the Love of Dogs NY, LLC",
    "TrialTypes": "NW3, NW2, NW1",
    "EventCount": 3,
    "Latitude": 40.9878,
    "Longitude": -73.7184
  },
  {
    "Date": "2024-08-31",
    "Location": "Dunkirk, NY",
    "Host": "Do Over Dog Training",
    "TrialTypes": "ELT-S, NW2, NW1",
    "EventCount": 3,
    "Latitude": 42.5018,
    "Longitude": -79.3804
  },
  {
    "Date": "2024-08-31",
    "Location": "Jefferson, OH",
    "Host": "Barns And Noses, LLC",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 40.3415,
    "Longitude": -80.7754
  },
  {
    "Date": "2024-08-31",
    "Location": "Lafayette, IN",
    "Host": "Outside The Box Dog Training",
    "TrialTypes": "NW3, NW1",
    "EventCount": 2,
    "Latitude": 40.3862,
    "Longitude": -86.8832
  },
  {
    "Date": "2024-09-01",
    "Location": "Colesville, MD",
    "Host": "Red Huskies",
    "TrialTypes": "ELT-P, ELT-S, L3C",
    "EventCount": 3,
    "Latitude": 39.063,
    "Longitude": -76.9953
  },
  {
    "Date": "2024-09-01",
    "Location": "Watertown, WI",
    "Host": "Think Pawsitive Dog Training",
    "TrialTypes": "NW2, NW1",
    "EventCount": 2,
    "Latitude": 43.2388,
    "Longitude": -88.7008
  },
  {
    "Date": "2024-09-07",
    "Location": "Ames, IA",
    "Host": "KBP Dog Training",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 42.0592,
    "Longitude": -93.5828
  },
  {
    "Date": "2024-09-07",
    "Location": "Bloomington, MN",
    "Host": "St Paul Dog Training Club",
    "TrialTypes": "ELT-S, NW2",
    "EventCount": 2,
    "Latitude": 44.8654,
    "Longitude": -93.3337
  },
  {
    "Date": "2024-09-07",
    "Location": "Manchester, NH",
    "Host": "The Big Sniff, LLC",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 43.031,
    "Longitude": -71.488
  },
  {
    "Date": "2024-09-07",
    "Location": "Scotts Mills, OR",
    "Host": "Canine Discovery Corps",
    "TrialTypes": "NW1, L1E, NW2",
    "EventCount": 3,
    "Latitude": 45.033,
    "Longitude": -122.6839
  },
  {
    "Date": "2024-09-13",
    "Location": "Flint, MI",
    "Host": "Every Dog Nosework",
    "TrialTypes": "NW3, ELT, ELT-P",
    "EventCount": 3,
    "Latitude": 43.025,
    "Longitude": -83.6443
  },
  {
    "Date": "2024-09-13",
    "Location": "Frederick, MD",
    "Host": "Red Huskies",
    "TrialTypes": "ELT-P, NW3",
    "EventCount": 2,
    "Latitude": 39.3676,
    "Longitude": -77.4011
  },
  {
    "Date": "2024-09-13",
    "Location": "New Milford, PA",
    "Host": "Your Dog's Place, LLC",
    "TrialTypes": "ELT, NW3, ELT-S, NW2",
    "EventCount": 4,
    "Latitude": 41.8704,
    "Longitude": -75.762
  },
  {
    "Date": "2024-09-14",
    "Location": "Carlisle, PA",
    "Host": "Nose-It-All, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 40.2237,
    "Longitude": -77.1598
  },
  {
    "Date": "2024-09-14",
    "Location": "Helena, MT",
    "Host": "Nosework Breakfast Club",
    "TrialTypes": "NW3, NW1, L1E",
    "EventCount": 3,
    "Latitude": 46.5834,
    "Longitude": -112.0187
  },
  {
    "Date": "2024-09-14",
    "Location": "Loma Mar, CA",
    "Host": "The Bay Team",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 37.2344,
    "Longitude": -122.2629
  },
  {
    "Date": "2024-09-14",
    "Location": "Loveland, CO",
    "Host": "Paws 4 Thought Dog Training, LLC",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 40.3713,
    "Longitude": -105.0511
  },
  {
    "Date": "2024-09-20",
    "Location": "Easton, MD",
    "Host": "Fair Play Labradors",
    "TrialTypes": "ELT-P, NW2, L1V",
    "EventCount": 3,
    "Latitude": 38.7427,
    "Longitude": -76.1256
  },
  {
    "Date": "2024-09-21",
    "Location": "North Bend, WA",
    "Host": "Northwest K9 Sniffers",
    "TrialTypes": "NW3, L1C, L1I",
    "EventCount": 3,
    "Latitude": 47.4522,
    "Longitude": -121.8182
  },
  {
    "Date": "2024-09-21",
    "Location": "Tuftonboro, NH",
    "Host": "The Big Sniff, LLC",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 43.7266,
    "Longitude": -71.285
  },
  {
    "Date": "2024-09-21",
    "Location": "White Salmon, WA",
    "Host": "Sharon Smith",
    "TrialTypes": "NW1, ELT-S, NW3",
    "EventCount": 3,
    "Latitude": 45.6958,
    "Longitude": -121.4545
  },
  {
    "Date": "2024-09-27",
    "Location": "Estes Park, CO",
    "Host": "Mountain Dogs, LLC",
    "TrialTypes": "SMT, ELT-S",
    "EventCount": 2,
    "Latitude": 40.3534,
    "Longitude": -105.5511
  },
  {
    "Date": "2024-09-27",
    "Location": "Richmond, VA",
    "Host": "Paws Plus Training, LLC",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 37.5815,
    "Longitude": -77.4445
  },
  {
    "Date": "2024-09-27",
    "Location": "Turlock, CA",
    "Host": "Two Nosey Girls",
    "TrialTypes": "L2I, L3I, ELT-S",
    "EventCount": 3,
    "Latitude": 37.4456,
    "Longitude": -120.8056
  },
  {
    "Date": "2024-09-28",
    "Location": "Grandview, TX",
    "Host": "North Texas Nosework Club",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 32.2375,
    "Longitude": -97.2075
  },
  {
    "Date": "2024-09-28",
    "Location": "Pittsburgh, PA",
    "Host": "Steel City Nosework, LLC",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 40.433,
    "Longitude": -79.965
  },
  {
    "Date": "2024-09-28",
    "Location": "Reedsport, OR",
    "Host": "Wells Creek Dog Training",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 43.6978,
    "Longitude": -124.0795
  },
  {
    "Date": "2024-09-28",
    "Location": "Waynesboro, PA",
    "Host": "Nose-It-All, LLC",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 39.7589,
    "Longitude": -77.5732
  },
  {
    "Date": "2024-09-29",
    "Location": "Glenwood, NY",
    "Host": "Do Over Dog Training",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 42.6142,
    "Longitude": -78.6472
  },
  {
    "Date": "2024-10-03",
    "Location": "New Rochelle, NY",
    "Host": "For the Love of Dogs NY LLC",
    "TrialTypes": "NW3, NW2, NW1",
    "EventCount": 3,
    "Latitude": 40.9582,
    "Longitude": -73.7692
  },
  {
    "Date": "2024-10-04",
    "Location": "Golden, CO",
    "Host": "K9 Nosin’ Around, Inc.",
    "TrialTypes": "NW2, NW1",
    "EventCount": 2,
    "Latitude": 39.8014,
    "Longitude": -105.1971
  },
  {
    "Date": "2024-10-04",
    "Location": "Mechanicsburg, PA",
    "Host": "Nose-It-All, LLC",
    "TrialTypes": "SMT, L3V, L2V",
    "EventCount": 3,
    "Latitude": 40.2108,
    "Longitude": -76.9923
  },
  {
    "Date": "2024-10-05",
    "Location": "Centralia, WA",
    "Host": "About Face K9 Academy & Let's Talk Dogs, LLC",
    "TrialTypes": "ELT-S, NW1",
    "EventCount": 2,
    "Latitude": 46.7072,
    "Longitude": -122.9651
  },
  {
    "Date": "2024-10-05",
    "Location": "Copake, NY",
    "Host": "Saints2Source, LLC",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 42.0586,
    "Longitude": -73.5966
  },
  {
    "Date": "2024-10-05",
    "Location": "Crosslake, MN",
    "Host": "Nose 2 Tail Dog Training, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 46.63,
    "Longitude": -94.1338
  },
  {
    "Date": "2024-10-05",
    "Location": "Nashua, NH",
    "Host": "The Big Sniff, LLC",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 42.7595,
    "Longitude": -71.4549
  },
  {
    "Date": "2024-10-05",
    "Location": "New Paltz, NY",
    "Host": "Pat Tetrault and Dominique Manpel",
    "TrialTypes": "NW2, ELT-S, L2I",
    "EventCount": 3,
    "Latitude": 41.7706,
    "Longitude": -74.0658
  },
  {
    "Date": "2024-10-05",
    "Location": "Sandwich, IL",
    "Host": "For Your K9",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 41.622,
    "Longitude": -88.5734
  },
  {
    "Date": "2024-10-05",
    "Location": "Troy, VA",
    "Host": "Your Dog Knows, LLC",
    "TrialTypes": "L1I, L2I, L1C, L2C",
    "EventCount": 4,
    "Latitude": 37.9025,
    "Longitude": -78.2082
  },
  {
    "Date": "2024-10-05",
    "Location": "West Bend, WI",
    "Host": "Think Pawsitive Dog Training",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 43.4488,
    "Longitude": -88.1812
  },
  {
    "Date": "2024-10-07",
    "Location": "Monterey, CA",
    "Host": "CalCoastal Dog Owners Group",
    "TrialTypes": "L1I, NW2, L2I",
    "EventCount": 3,
    "Latitude": 36.2441,
    "Longitude": -121.4127
  },
  {
    "Date": "2024-10-11",
    "Location": "South Haven, MN",
    "Host": "St Paul Dog Training Club",
    "TrialTypes": "SMT, ELT",
    "EventCount": 2,
    "Latitude": 45.3362,
    "Longitude": -94.2139
  },
  {
    "Date": "2024-10-11",
    "Location": "Walbridge, OH",
    "Host": "Robin Ford Dog Training",
    "TrialTypes": "NW1, NW2, NW3",
    "EventCount": 3,
    "Latitude": 41.5553,
    "Longitude": -83.4797
  },
  {
    "Date": "2024-10-12",
    "Location": "Homer Glen, IL",
    "Host": "Paws for Scent",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 41.5916,
    "Longitude": -87.9241
  },
  {
    "Date": "2024-10-12",
    "Location": "Lafayette Hill, PA",
    "Host": "Sniff Sniff Hooray",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 40.114,
    "Longitude": -75.2045
  },
  {
    "Date": "2024-10-12",
    "Location": "Sedona, AZ",
    "Host": "Release Canine LLC",
    "TrialTypes": "ELT, ELT-S, NW2, NW1",
    "EventCount": 4,
    "Latitude": 34.8482,
    "Longitude": -111.778
  },
  {
    "Date": "2024-10-18",
    "Location": "Calhan, CO",
    "Host": "Mountain Dogs, LLC",
    "TrialTypes": "ELT, NW3, ELT-S",
    "EventCount": 3,
    "Latitude": 39.0683,
    "Longitude": -104.2987
  },
  {
    "Date": "2024-10-18",
    "Location": "Loganville, GA",
    "Host": "Canine Country Academy, LLC",
    "TrialTypes": "NW2, NW1",
    "EventCount": 2,
    "Latitude": 33.792,
    "Longitude": -83.9087
  },
  {
    "Date": "2024-10-18",
    "Location": "Newfoundland, PA",
    "Host": "Your Dog's Place, LLC",
    "TrialTypes": "L1V, ELT-S, L2C, L1E",
    "EventCount": 4,
    "Latitude": 41.333,
    "Longitude": -75.3495
  },
  {
    "Date": "2024-10-18",
    "Location": "Rossville, GA",
    "Host": "Camelot Shepherds, Inc",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 35.0269,
    "Longitude": -85.2817
  },
  {
    "Date": "2024-10-19",
    "Location": "Ferndale, WA",
    "Host": "Nose Work Magic",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 48.8452,
    "Longitude": -122.6359
  },
  {
    "Date": "2024-10-19",
    "Location": "Griffith, IN",
    "Host": "Outside the Box, LLC",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 41.5581,
    "Longitude": -87.4192
  },
  {
    "Date": "2024-10-19",
    "Location": "Kilmarnock, VA",
    "Host": "Red Huskies",
    "TrialTypes": "NW3, L1E, NW2",
    "EventCount": 3,
    "Latitude": 37.7426,
    "Longitude": -76.3299
  },
  {
    "Date": "2024-10-19",
    "Location": "Kingston, IL",
    "Host": "Common Scents K9",
    "TrialTypes": "NW1, L2C, NW2",
    "EventCount": 3,
    "Latitude": 42.1309,
    "Longitude": -88.7483
  },
  {
    "Date": "2024-10-19",
    "Location": "Lakeville, MN",
    "Host": "St Paul Dog Training Club",
    "TrialTypes": "ELT-P, NW1, L1E",
    "EventCount": 3,
    "Latitude": 44.629,
    "Longitude": -93.217
  },
  {
    "Date": "2024-10-19",
    "Location": "Round Rock, TX",
    "Host": "Heng Ten K9 Training",
    "TrialTypes": "NW3, ELT-S, NW1",
    "EventCount": 3,
    "Latitude": 30.4922,
    "Longitude": -97.6787
  },
  {
    "Date": "2024-10-19",
    "Location": "Yamhill, OR",
    "Host": "Nose Work Detectives, LLC",
    "TrialTypes": "L1C, L1V, L2C, L2V",
    "EventCount": 4,
    "Latitude": 45.2291,
    "Longitude": -123.2105
  },
  {
    "Date": "2024-10-22",
    "Location": "Astoria, OR",
    "Host": "Nosework Detectives, LLC",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 46.1533,
    "Longitude": -123.8373
  },
  {
    "Date": "2024-10-25",
    "Location": "Fishkill, NY",
    "Host": "Pat Tetrault and Dominique Manpel",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 41.5455,
    "Longitude": -73.8725
  },
  {
    "Date": "2024-10-25",
    "Location": "Palmyra, VA",
    "Host": "Paws Plus Training, LLC",
    "TrialTypes": "ELT, NW3, ELT-S, NW1",
    "EventCount": 4,
    "Latitude": 37.8658,
    "Longitude": -78.2385
  },
  {
    "Date": "2024-10-26",
    "Location": "Columbia City, IN",
    "Host": "2 Nose You Is 2 Loves You",
    "TrialTypes": "ELT, NW1",
    "EventCount": 2,
    "Latitude": 41.1575,
    "Longitude": -85.5237
  },
  {
    "Date": "2024-10-26",
    "Location": "Columbus, MT",
    "Host": "Nikki Markle of Canine Connection",
    "TrialTypes": "ELT-S, NW2, ELT",
    "EventCount": 3,
    "Latitude": 45.6441,
    "Longitude": -109.2635
  },
  {
    "Date": "2024-10-26",
    "Location": "Grand Junction, CO",
    "Host": "Mountain Dogs, LLC",
    "TrialTypes": "NW2, NW1",
    "EventCount": 2,
    "Latitude": 39.032,
    "Longitude": -108.5498
  },
  {
    "Date": "2024-10-26",
    "Location": "Hammond, LA",
    "Host": "Dog Gone Right, LLC",
    "TrialTypes": "ELT-S, NW1, NW3",
    "EventCount": 3,
    "Latitude": 30.5384,
    "Longitude": -90.4404
  },
  {
    "Date": "2024-10-26",
    "Location": "Medford, NJ",
    "Host": "K9 InScentives",
    "TrialTypes": "NW3, NW1",
    "EventCount": 2,
    "Latitude": 39.9387,
    "Longitude": -74.8329
  },
  {
    "Date": "2024-10-26",
    "Location": "Poland Springs, ME",
    "Host": "Virginia Howe",
    "TrialTypes": "NW1, NW3",
    "EventCount": 2,
    "Latitude": 44.0568,
    "Longitude": -70.3733
  },
  {
    "Date": "2024-10-26",
    "Location": "Suring, WI",
    "Host": "Clever Sniffers, LLC",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 44.9521,
    "Longitude": -88.406
  },
  {
    "Date": "2024-10-26",
    "Location": "Welches, OR",
    "Host": "Trust Your Dog K9 Events",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 45.3754,
    "Longitude": -121.9557
  },
  {
    "Date": "2024-10-26",
    "Location": "West Friendship, MD",
    "Host": "Red Huskies",
    "TrialTypes": "NW3, L2C, NW2",
    "EventCount": 3,
    "Latitude": 39.2582,
    "Longitude": -76.9446
  },
  {
    "Date": "2024-10-26",
    "Location": "Winterset, IA",
    "Host": "KBP Dog Training",
    "TrialTypes": "NW2, NW1",
    "EventCount": 2,
    "Latitude": 41.2864,
    "Longitude": -93.9983
  },
  {
    "Date": "2024-10-27",
    "Location": "San Martin, CA",
    "Host": "B. L. McMutts",
    "TrialTypes": "L1V, L2V",
    "EventCount": 2,
    "Latitude": 37.0434,
    "Longitude": -121.6439
  },
  {
    "Date": "2024-11-01",
    "Location": "Denton, MD",
    "Host": "Fair Play Labradors",
    "TrialTypes": "NW3, ELT-S, L1C, NW2, L1E",
    "EventCount": 5,
    "Latitude": 38.8385,
    "Longitude": -75.7818
  },
  {
    "Date": "2024-11-01",
    "Location": "Guerneville, CA",
    "Host": "Jen Huot",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 38.5628,
    "Longitude": -122.9505
  },
  {
    "Date": "2024-11-01",
    "Location": "Red Feather Lakes, CO",
    "Host": "Beyond Elevation K9 Training",
    "TrialTypes": "NW1, NW2, NW3",
    "EventCount": 3,
    "Latitude": 40.8366,
    "Longitude": -105.5932
  },
  {
    "Date": "2024-11-02",
    "Location": "Callaway, VA",
    "Host": "Canny K9 Companions LLC",
    "TrialTypes": "ELT-S, NW1, NW2",
    "EventCount": 3,
    "Latitude": 36.9675,
    "Longitude": -80.0599
  },
  {
    "Date": "2024-11-02",
    "Location": "Greenview, IL",
    "Host": "Capitol Canine Dog Sports",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 40.0746,
    "Longitude": -89.6994
  },
  {
    "Date": "2024-11-02",
    "Location": "Kennebunkport, ME",
    "Host": "Elizabeth Dutton",
    "TrialTypes": "ELT-P, NW3",
    "EventCount": 2,
    "Latitude": 43.3124,
    "Longitude": -70.4891
  },
  {
    "Date": "2024-11-02",
    "Location": "Mays Landing, NJ",
    "Host": "Rotts-n-Notts Nosework, LLC",
    "TrialTypes": "NW3, L1E, NW1",
    "EventCount": 3,
    "Latitude": 39.4355,
    "Longitude": -74.7418
  },
  {
    "Date": "2024-11-02",
    "Location": "Mill Spring, NC",
    "Host": "Foothills Canine Academy, LLC",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 35.2524,
    "Longitude": -82.1169
  },
  {
    "Date": "2024-11-02",
    "Location": "Shawnee, OK",
    "Host": "The Doggie Spot, LLC",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 35.2923,
    "Longitude": -96.89
  },
  {
    "Date": "2024-11-02",
    "Location": "Valencia, CA",
    "Host": "Pink Biscuit K9s",
    "TrialTypes": "L1I, L2I, L1C, L2C",
    "EventCount": 4,
    "Latitude": 34.3883,
    "Longitude": -118.5718
  },
  {
    "Date": "2024-11-02",
    "Location": "Wappingers Falls, NY",
    "Host": "Top Notch Dogs, LLC",
    "TrialTypes": "ELT-P, ELT-S, L1C",
    "EventCount": 3,
    "Latitude": 41.5857,
    "Longitude": -73.9182
  },
  {
    "Date": "2024-11-03",
    "Location": "McMinnville, OR",
    "Host": "Doglandia LLC and Carol Forsberg",
    "TrialTypes": "NW2, ELT",
    "EventCount": 2,
    "Latitude": 45.1941,
    "Longitude": -123.1457
  },
  {
    "Date": "2024-11-04",
    "Location": "Duluth, GA",
    "Host": "Right Choice Dog Training, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 33.9774,
    "Longitude": -84.1281
  },
  {
    "Date": "2024-11-08",
    "Location": "Montrose, CO",
    "Host": "Mountain Dogs, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 38.4805,
    "Longitude": -107.8394
  },
  {
    "Date": "2024-11-09",
    "Location": "Canoga Park, CA",
    "Host": "Pink Biscuit K9s",
    "TrialTypes": "NW1, L3I, L3C",
    "EventCount": 3,
    "Latitude": 34.199,
    "Longitude": -118.5703
  },
  {
    "Date": "2024-11-09",
    "Location": "Eldred, NY",
    "Host": "Pocono Nose Work",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 41.5102,
    "Longitude": -74.9134
  },
  {
    "Date": "2024-11-09",
    "Location": "Escondido, CA",
    "Host": "Anita Cheesman and Jessica Koester",
    "TrialTypes": "NW1, L1C",
    "EventCount": 2,
    "Latitude": 33.0952,
    "Longitude": -117.0317
  },
  {
    "Date": "2024-11-09",
    "Location": "Huntsville, AL",
    "Host": "Sniffers Anonymous",
    "TrialTypes": "NW3, L1I, L1C",
    "EventCount": 3,
    "Latitude": 34.7374,
    "Longitude": -86.5923
  },
  {
    "Date": "2024-11-09",
    "Location": "Milton, NH",
    "Host": "Lucky Dog Events",
    "TrialTypes": "NW3, L2I, NW2",
    "EventCount": 3,
    "Latitude": 43.3965,
    "Longitude": -71.0071
  },
  {
    "Date": "2024-11-09",
    "Location": "Moline, IL",
    "Host": "Fur Better Fur Worse, LLC",
    "TrialTypes": "ELT-S",
    "EventCount": 1,
    "Latitude": 41.5174,
    "Longitude": -90.5154
  },
  {
    "Date": "2024-11-09",
    "Location": "New Rochelle, NY",
    "Host": "For the Love of Dogs NY, LLC",
    "TrialTypes": "L3I, L3C, NW1, NW2",
    "EventCount": 4,
    "Latitude": 40.9466,
    "Longitude": -73.7883
  },
  {
    "Date": "2024-11-09",
    "Location": "Schaumburg, IL",
    "Host": "Northwest Obedience Club Inc.",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 42.0663,
    "Longitude": -88.12
  },
  {
    "Date": "2024-11-10",
    "Location": "Odessa, FL",
    "Host": "Hoppin’ in the Hills",
    "TrialTypes": "NW2, NW1",
    "EventCount": 2,
    "Latitude": 28.1745,
    "Longitude": -82.5815
  },
  {
    "Date": "2024-11-11",
    "Location": "Escondido, CA",
    "Host": "Anita Cheesman and Jessica Koester",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 33.1502,
    "Longitude": -117.1216
  },
  {
    "Date": "2024-11-11",
    "Location": "Paso Robles, CA",
    "Host": "Gentle Touch Pet Training",
    "TrialTypes": "L1C, L2I, ELT-S, NW2",
    "EventCount": 4,
    "Latitude": 35.5894,
    "Longitude": -120.6883
  },
  {
    "Date": "2024-11-15",
    "Location": "Harrington, DE",
    "Host": "Shamrock Pot of Gold K9 Scenter",
    "TrialTypes": "NW3, ELT, ELT-P, NW2, NW1",
    "EventCount": 5,
    "Latitude": 38.9143,
    "Longitude": -75.539
  },
  {
    "Date": "2024-11-15",
    "Location": "Rancho Cucamonga, CA",
    "Host": "Agile Paws Dog Sports",
    "TrialTypes": "L2C, NW2, NW1, L1C",
    "EventCount": 4,
    "Latitude": 34.092,
    "Longitude": -117.6179
  },
  {
    "Date": "2024-11-16",
    "Location": "Auburn, WA",
    "Host": "Northwest K9 Sniffers",
    "TrialTypes": "ELT-S, NW1, L2C, L2I",
    "EventCount": 4,
    "Latitude": 47.2834,
    "Longitude": -122.2029
  },
  {
    "Date": "2024-11-16",
    "Location": "Foxborough, MA",
    "Host": "MasterPeace Dog Training",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 42.0414,
    "Longitude": -71.2807
  },
  {
    "Date": "2024-11-16",
    "Location": "Marble Falls, TX",
    "Host": "Scent Work Across Texas",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 30.6091,
    "Longitude": -98.3169
  },
  {
    "Date": "2024-11-16",
    "Location": "Nevada City, CA",
    "Host": "Sierra Sniffing Canines",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 39.3011,
    "Longitude": -120.9843
  },
  {
    "Date": "2024-11-16",
    "Location": "Tucson, AZ",
    "Host": "Patience Unlimited Professional Dog Training",
    "TrialTypes": "NW1, L1C, L1E, L1I",
    "EventCount": 4,
    "Latitude": 32.2365,
    "Longitude": -110.9553
  },
  {
    "Date": "2024-11-16",
    "Location": "Yanceyville, NC",
    "Host": "Whole Dog Institute, LLC",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 36.4098,
    "Longitude": -79.3044
  },
  {
    "Date": "2024-11-23",
    "Location": "Coburg, OR",
    "Host": "Kiddie Christie",
    "TrialTypes": "L1E, NW1, NW3",
    "EventCount": 3,
    "Latitude": 44.0958,
    "Longitude": -123.1028
  },
  {
    "Date": "2024-11-23",
    "Location": "Delta, CO",
    "Host": "Mountain Dogs, LLC",
    "TrialTypes": "NW3, ELT-P",
    "EventCount": 2,
    "Latitude": 38.8832,
    "Longitude": -107.8513
  },
  {
    "Date": "2024-11-23",
    "Location": "Fork Union, VA",
    "Host": "Your Dog Knows LLC",
    "TrialTypes": "NW3, L3I, L1V",
    "EventCount": 3,
    "Latitude": 37.7362,
    "Longitude": -78.2538
  },
  {
    "Date": "2024-11-23",
    "Location": "Kintnersville, PA",
    "Host": "Paws n' Sniff",
    "TrialTypes": "NW1, L1E, ELT-S",
    "EventCount": 3,
    "Latitude": 40.6042,
    "Longitude": -75.1959
  },
  {
    "Date": "2024-11-23",
    "Location": "Saltsburg, PA",
    "Host": "Steel City Nosework, LLC",
    "TrialTypes": "NW3, ELT-S, NW1",
    "EventCount": 3,
    "Latitude": 40.4721,
    "Longitude": -79.4826
  },
  {
    "Date": "2024-11-23",
    "Location": "Smyrna, TN",
    "Host": "Dogs Have Amazing Noses LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 35.9425,
    "Longitude": -86.522
  },
  {
    "Date": "2024-11-29",
    "Location": "Capo Beach/Dana Point, CA",
    "Host": "JavaK9s",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 33.4991,
    "Longitude": -117.6565
  },
  {
    "Date": "2024-11-29",
    "Location": "Foxborough, MA",
    "Host": "Tracey Costa",
    "TrialTypes": "ELT, L1C, NW2",
    "EventCount": 3,
    "Latitude": 42.0893,
    "Longitude": -71.254
  },
  {
    "Date": "2024-11-30",
    "Location": "Cottage Grove, MN",
    "Host": "Gretchen Hofheins-Wackerfuss",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 44.8718,
    "Longitude": -92.9107
  },
  {
    "Date": "2024-11-30",
    "Location": "Cumming, GA",
    "Host": "Georgia Nosework",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 34.2199,
    "Longitude": -84.1668
  },
  {
    "Date": "2024-11-30",
    "Location": "Green Bay, WI",
    "Host": "NEWK9 Scent Work LLC",
    "TrialTypes": "ELT-S, ELT",
    "EventCount": 2,
    "Latitude": 44.5369,
    "Longitude": -87.9994
  },
  {
    "Date": "2024-11-30",
    "Location": "Lebanon, NJ",
    "Host": "Sirius K-9 Solutions",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 40.6279,
    "Longitude": -74.8453
  },
  {
    "Date": "2024-11-30",
    "Location": "Los Osos, CA",
    "Host": "Central Coast Nosework Club, Inc.",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 35.2994,
    "Longitude": -120.851
  },
  {
    "Date": "2024-11-30",
    "Location": "Plant City, FL",
    "Host": "Hoppin’ in the Hills",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 27.9839,
    "Longitude": -82.1655
  },
  {
    "Date": "2024-11-30",
    "Location": "Worcester, PA",
    "Host": "Sniff Sniff Hooray",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 40.165,
    "Longitude": -75.308
  },
  {
    "Date": "2024-12-06",
    "Location": "Salem, WI",
    "Host": "Loving Paws Dog Training LLC",
    "TrialTypes": "ELT-P, ELT",
    "EventCount": 2,
    "Latitude": 42.5519,
    "Longitude": -88.0767
  },
  {
    "Date": "2024-12-06",
    "Location": "Ypsilanti, MI",
    "Host": "Every Dog Nosework",
    "TrialTypes": "ELT-P, ELT-S, L2C",
    "EventCount": 3,
    "Latitude": 42.2546,
    "Longitude": -83.6439
  },
  {
    "Date": "2024-12-07",
    "Location": "Batavia, NY",
    "Host": "Do Over Dog Training",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 43.0288,
    "Longitude": -78.2282
  },
  {
    "Date": "2024-12-07",
    "Location": "Bowie, MD",
    "Host": "Red Huskies",
    "TrialTypes": "ELT-P, ELT-S",
    "EventCount": 2,
    "Latitude": 38.9788,
    "Longitude": -76.753
  },
  {
    "Date": "2024-12-07",
    "Location": "Centralia, WA",
    "Host": "Let's Talk Dogs, LLC and About Face K9 Academy",
    "TrialTypes": "ELT-P, NW2",
    "EventCount": 2,
    "Latitude": 46.7669,
    "Longitude": -122.9494
  },
  {
    "Date": "2024-12-07",
    "Location": "Chester Springs, PA",
    "Host": "Patricia Grassey",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 40.0973,
    "Longitude": -75.6361
  },
  {
    "Date": "2024-12-07",
    "Location": "DeLeon Springs, FL",
    "Host": "River Poodles Training, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 29.1057,
    "Longitude": -81.3691
  },
  {
    "Date": "2024-12-07",
    "Location": "Fillmore, CA",
    "Host": "Pink Biscuit K9s",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 34.4444,
    "Longitude": -118.9448
  },
  {
    "Date": "2024-12-07",
    "Location": "Newfoundland, PA",
    "Host": "Your Dog's Place, LLC",
    "TrialTypes": "L3V, NW2, ELT",
    "EventCount": 3,
    "Latitude": 41.2899,
    "Longitude": -75.3212
  },
  {
    "Date": "2024-12-07",
    "Location": "Owenton, KY",
    "Host": "Clermont County Dog Training Club",
    "TrialTypes": "ELT, NW1, NW2",
    "EventCount": 3,
    "Latitude": 38.5232,
    "Longitude": -84.8575
  },
  {
    "Date": "2024-12-09",
    "Location": "Stockton, CA",
    "Host": "Two Nosey Girls",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 37.9211,
    "Longitude": -121.2656
  },
  {
    "Date": "2024-12-13",
    "Location": "Pittstown, NJ",
    "Host": "Shamrock Pot of Gold K9 Scenter",
    "TrialTypes": "NW3, ELT-P, ELT-S, NW1",
    "EventCount": 4,
    "Latitude": 40.6272,
    "Longitude": -74.9871
  },
  {
    "Date": "2024-12-14",
    "Location": "Ontario, CA",
    "Host": "Agile Paws Dog Sports",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 34.0776,
    "Longitude": -117.6738
  },
  {
    "Date": "2024-12-21",
    "Location": "Cedar Park, TX",
    "Host": "Scent Work Across Texas",
    "TrialTypes": "L1V, L2I, NW1, L2E",
    "EventCount": 4,
    "Latitude": 30.5248,
    "Longitude": -97.8081
  },
  {
    "Date": "2024-12-21",
    "Location": "Jefferson, GA",
    "Host": "Right Choice Dog Training, LLC",
    "TrialTypes": "NW2, NW1",
    "EventCount": 2,
    "Latitude": 32.9939,
    "Longitude": -82.3959
  },
  {
    "Date": "2024-12-21",
    "Location": "Marriottsville, MD",
    "Host": "Red Huskies",
    "TrialTypes": "ELT-P, ELT-S, NW1",
    "EventCount": 3,
    "Latitude": 39.3955,
    "Longitude": -76.8494
  },
  {
    "Date": "2024-12-27",
    "Location": "Crownsville, MD",
    "Host": "Red Huskies",
    "TrialTypes": "ELT-P, ELT, ELT-S",
    "EventCount": 3,
    "Latitude": 39.0412,
    "Longitude": -76.5532
  },
  {
    "Date": "2024-12-28",
    "Location": "Bellingham, WA",
    "Host": "Nose Work Magic",
    "TrialTypes": "L1V, ELT-S, ELT",
    "EventCount": 3,
    "Latitude": 48.719,
    "Longitude": -122.5229
  },
  {
    "Date": "2024-12-28",
    "Location": "Cumming, GA",
    "Host": "Georgia Nosework",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 34.2245,
    "Longitude": -84.0989
  },
  {
    "Date": "2024-12-28",
    "Location": "Salem, OR",
    "Host": "Helix Fairweather & Doglandia, LLC",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 44.923,
    "Longitude": -123.0097
  },
  {
    "Date": "2024-12-28",
    "Location": "Williamsburg, VA",
    "Host": "Blockade Runners Flyball",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 37.279,
    "Longitude": -76.7483
  },
  {
    "Date": "2024-12-29",
    "Location": "Waukesha, WI",
    "Host": "Think Pawsitive Dog Training",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 43.1,
    "Longitude": -88.3187
  },
  {
    "Date": "2024-12-31",
    "Location": "Strasburg, IL",
    "Host": "Kudos for Canines, LLC",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 39.3719,
    "Longitude": -88.6653
  },
  {
    "Date": "2025-01-03",
    "Location": "Brockport, NY",
    "Host": "Savvy Dog Sports",
    "TrialTypes": "ELT-S, ELT",
    "EventCount": 2,
    "Latitude": 43.1718,
    "Longitude": -77.8956
  },
  {
    "Date": "2025-01-03",
    "Location": "Emmitsburg, MD",
    "Host": "Red Huskies",
    "TrialTypes": "ELT, ELT-P, ELT-S",
    "EventCount": 3,
    "Latitude": 39.6978,
    "Longitude": -77.362
  },
  {
    "Date": "2025-01-04",
    "Location": "Bonsall, CA",
    "Host": "Linda Buchanan",
    "TrialTypes": "NW2, ELT",
    "EventCount": 2,
    "Latitude": 33.284,
    "Longitude": -117.1768
  },
  {
    "Date": "2025-01-09",
    "Location": "Centreville, MD",
    "Host": "Fair Play Labradors",
    "TrialTypes": "ELT-S, NW3, ELT, ELT-P",
    "EventCount": 4,
    "Latitude": 39.0049,
    "Longitude": -76.0198
  },
  {
    "Date": "2025-01-10",
    "Location": "Hartfield, VA",
    "Host": "Paws Plus Training, LLC",
    "TrialTypes": "NW3, NW1, NW2, ELT",
    "EventCount": 4,
    "Latitude": 37.5255,
    "Longitude": -76.4678
  },
  {
    "Date": "2025-01-11",
    "Location": "Greensboro, NC",
    "Host": "Dog Fun Forever, LLC",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 36.0398,
    "Longitude": -79.8357
  },
  {
    "Date": "2025-01-11",
    "Location": "Lithia, FL",
    "Host": "Hoppin’ in the Hills",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 27.8727,
    "Longitude": -82.2679
  },
  {
    "Date": "2025-01-13",
    "Location": "Oakdale, CA",
    "Host": "Two Nosey Girls",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 37.7273,
    "Longitude": -120.8818
  },
  {
    "Date": "2025-01-18",
    "Location": "Clanton, AL",
    "Host": "Daphne Melillo",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 32.8369,
    "Longitude": -86.623
  },
  {
    "Date": "2025-01-18",
    "Location": "Elmira, OR",
    "Host": "Wells Creek Dog Training",
    "TrialTypes": "ELT, L1I, NW2",
    "EventCount": 3,
    "Latitude": 44.0623,
    "Longitude": -123.3542
  },
  {
    "Date": "2025-01-18",
    "Location": "Flemington, NJ",
    "Host": "Shamrock Pot of Gold K9 Scenter",
    "TrialTypes": "ELT-S, L2I, L2C, ELT",
    "EventCount": 4,
    "Latitude": 40.505,
    "Longitude": -74.8791
  },
  {
    "Date": "2025-01-18",
    "Location": "Marble Falls, TX",
    "Host": "Heng Ten K9 Training",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 30.5374,
    "Longitude": -98.3255
  },
  {
    "Date": "2025-01-18",
    "Location": "Melrose, FL",
    "Host": "River Poodles Training, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 29.7543,
    "Longitude": -82.0725
  },
  {
    "Date": "2025-01-18",
    "Location": "New Rochelle, NY",
    "Host": "For the Love of Dogs NY, LLC",
    "TrialTypes": "NW2, NW3, ELT-S",
    "EventCount": 3,
    "Latitude": 40.9369,
    "Longitude": -73.7708
  },
  {
    "Date": "2025-01-18",
    "Location": "Redlands, CA",
    "Host": "Agile Paws Dog Sports",
    "TrialTypes": "ELT, NW2",
    "EventCount": 2,
    "Latitude": 34.0793,
    "Longitude": -117.2021
  },
  {
    "Date": "2025-01-18",
    "Location": "Sheridan, OR",
    "Host": "Helix Fairweather & Doglandia, LLC",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 45.0725,
    "Longitude": -123.3942
  },
  {
    "Date": "2025-01-25",
    "Location": "Danielsville, GA",
    "Host": "Right Choice Dog Training, LLC",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 34.102,
    "Longitude": -83.2074
  },
  {
    "Date": "2025-01-25",
    "Location": "Tecumseh, OK",
    "Host": "The Doggie Spot, LLC",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 35.2166,
    "Longitude": -96.9009
  },
  {
    "Date": "2025-01-31",
    "Location": "Vista, CA",
    "Host": "Anita Cheesman and Jessica Koester",
    "TrialTypes": "ELT-S, NW3",
    "EventCount": 2,
    "Latitude": 33.1872,
    "Longitude": -117.1936
  },
  {
    "Date": "2025-02-01",
    "Location": "Northridge, CA",
    "Host": "Scentwork.org",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 34.2343,
    "Longitude": -118.5393
  },
  {
    "Date": "2025-02-08",
    "Location": "Murfreesboro, TN",
    "Host": "Dogs Have Amazing Noses LLC",
    "TrialTypes": "ELT, NW1",
    "EventCount": 2,
    "Latitude": 35.8758,
    "Longitude": -86.3638
  },
  {
    "Date": "2025-02-08",
    "Location": "Veneta, OR",
    "Host": "Kiddy Christie",
    "TrialTypes": "NW3, L1C, NW1",
    "EventCount": 3,
    "Latitude": 44.0306,
    "Longitude": -123.3076
  },
  {
    "Date": "2025-02-14",
    "Location": "Honey Brook, PA",
    "Host": "The Sniffing Hound",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 40.0918,
    "Longitude": -75.9266
  },
  {
    "Date": "2025-02-15",
    "Location": "Bellingham, WA",
    "Host": "Nose Work Magic",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 48.723,
    "Longitude": -122.4926
  },
  {
    "Date": "2025-02-15",
    "Location": "Flemington, NJ",
    "Host": "Shamrock Pot of Gold K9 Scenter",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 40.4763,
    "Longitude": -74.8747
  },
  {
    "Date": "2025-02-15",
    "Location": "Lakewood, NJ",
    "Host": "Rotts-n-Notts Nosework, LLC",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 40.083,
    "Longitude": -74.2476
  },
  {
    "Date": "2025-02-15",
    "Location": "Lutherville-Timonium, MD",
    "Host": "Red Huskies",
    "TrialTypes": "L3C, L1I, NW2",
    "EventCount": 3,
    "Latitude": 39.4705,
    "Longitude": -76.6132
  },
  {
    "Date": "2025-02-15",
    "Location": "Medford, NJ",
    "Host": "Sniff Sniff Hooray",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 39.9019,
    "Longitude": -74.7972
  },
  {
    "Date": "2025-02-15",
    "Location": "Modesto, CA",
    "Host": "Two Nosey Girls",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 37.6054,
    "Longitude": -121.0213
  },
  {
    "Date": "2025-02-15",
    "Location": "White Plains, NY",
    "Host": "For the Love of Dogs NY, LLC",
    "TrialTypes": "L1I, L2C, NW3",
    "EventCount": 3,
    "Latitude": 41.0158,
    "Longitude": -73.7134
  },
  {
    "Date": "2025-02-15",
    "Location": "Wilson, NC",
    "Host": "Whole Dog Institute, LLC",
    "TrialTypes": "NW3, NW1",
    "EventCount": 2,
    "Latitude": 35.7525,
    "Longitude": -77.918
  },
  {
    "Date": "2025-02-16",
    "Location": "Chino, CA",
    "Host": "Agile Paws Dog Sports",
    "TrialTypes": "ELT, ELT-S",
    "EventCount": 2,
    "Latitude": 34.0071,
    "Longitude": -117.6868
  },
  {
    "Date": "2025-02-22",
    "Location": "Albuquerque, NM",
    "Host": "The Can Do K9, LLC",
    "TrialTypes": "NW1, NW3",
    "EventCount": 2,
    "Latitude": 35.105,
    "Longitude": -106.6674
  },
  {
    "Date": "2025-02-23",
    "Location": "Benson, AZ",
    "Host": "Patience Unlimited Professional Dog Training",
    "TrialTypes": "NW3, ELT-S, NW2",
    "EventCount": 3,
    "Latitude": 31.9446,
    "Longitude": -110.2894
  },
  {
    "Date": "2025-02-24",
    "Location": "Paso Robles, CA",
    "Host": "Gentle Touch Pet Training",
    "TrialTypes": "NW3, L2V, L1V",
    "EventCount": 3,
    "Latitude": 35.5801,
    "Longitude": -120.6962
  },
  {
    "Date": "2025-02-28",
    "Location": "San Rafael, CA",
    "Host": "Marin Humane",
    "TrialTypes": "L1C, ELT-S, ELT",
    "EventCount": 3,
    "Latitude": 38.021,
    "Longitude": -122.5754
  },
  {
    "Date": "2025-03-01",
    "Location": "Augusta, NJ",
    "Host": "Shamrock Pot of Gold K9 Scenter",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 41.0875,
    "Longitude": -74.7245
  },
  {
    "Date": "2025-03-01",
    "Location": "Keystone Heights, FL",
    "Host": "River Poodles Training, LLC",
    "TrialTypes": "NW3, ELT-P",
    "EventCount": 2,
    "Latitude": 29.7419,
    "Longitude": -82.0707
  },
  {
    "Date": "2025-03-01",
    "Location": "Oakville, WA",
    "Host": "About Face K9 Academy and Let's Talk Dogs, LLC",
    "TrialTypes": "NW3, L1C, L1I",
    "EventCount": 3,
    "Latitude": 46.8261,
    "Longitude": -123.2432
  },
  {
    "Date": "2025-03-01",
    "Location": "Pomfret, MD",
    "Host": "Nose-It-All, LLC",
    "TrialTypes": "ELT-S, NW2, ELT",
    "EventCount": 3,
    "Latitude": 38.6138,
    "Longitude": -77.0549
  },
  {
    "Date": "2025-03-01",
    "Location": "Santa Paula, CA",
    "Host": "Pink Biscuit K9s",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 34.3496,
    "Longitude": -119.0359
  },
  {
    "Date": "2025-03-01",
    "Location": "Youngwood, PA",
    "Host": "Steel City Nosework, LLC",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 40.2728,
    "Longitude": -79.5317
  },
  {
    "Date": "2025-03-02",
    "Location": "Shawnee, OK",
    "Host": "The Doggie Spot, LLC",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 35.3335,
    "Longitude": -96.8855
  },
  {
    "Date": "2025-03-07",
    "Location": "Elgin, IL",
    "Host": "For Your K9",
    "TrialTypes": "L1C, L2C, L1I, L2I",
    "EventCount": 4,
    "Latitude": 42.0091,
    "Longitude": -88.2922
  },
  {
    "Date": "2025-03-07",
    "Location": "Spring City, PA",
    "Host": "Patricia Grassey",
    "TrialTypes": "NW3, ELT, ELT-S, L1I",
    "EventCount": 4,
    "Latitude": 40.2008,
    "Longitude": -75.5468
  },
  {
    "Date": "2025-03-07",
    "Location": "Stokesdale , NC",
    "Host": "K9 Nose Adventures, LLC",
    "TrialTypes": "ELT, NW1, NW2, L1C, L1I",
    "EventCount": 5,
    "Latitude": 36.1935,
    "Longitude": -79.9497
  },
  {
    "Date": "2025-03-08",
    "Location": "Farmville, VA",
    "Host": "Paws Plus Training, LLC",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 37.2853,
    "Longitude": -78.4287
  },
  {
    "Date": "2025-03-08",
    "Location": "Fort Collins, CO",
    "Host": "Beyond Elevation K9 Training",
    "TrialTypes": "NW1, NW2, ELT",
    "EventCount": 3,
    "Latitude": 40.6343,
    "Longitude": -105.0922
  },
  {
    "Date": "2025-03-08",
    "Location": "Foxboro , MA",
    "Host": "Bay State Sniffers",
    "TrialTypes": "L1C, ELT-S, L1I, L3I",
    "EventCount": 4,
    "Latitude": 42.108,
    "Longitude": -71.3068
  },
  {
    "Date": "2025-03-08",
    "Location": "Rome, GA",
    "Host": "Southeast Scent Work Alliance, LLC",
    "TrialTypes": "ELT, NW2",
    "EventCount": 2,
    "Latitude": 34.2842,
    "Longitude": -85.1409
  },
  {
    "Date": "2025-03-08",
    "Location": "Winterset, IA",
    "Host": "KBP Dog Training",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 41.3809,
    "Longitude": -94.0393
  },
  {
    "Date": "2025-03-10",
    "Location": "Riverside, CA",
    "Host": "Linda Buchanan",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 33.9414,
    "Longitude": -117.3888
  },
  {
    "Date": "2025-03-14",
    "Location": "Phoenix, AZ",
    "Host": "Successful Sniffer",
    "TrialTypes": "NW3, ELT-S, NW1, NW2",
    "EventCount": 4,
    "Latitude": 33.4763,
    "Longitude": -112.0452
  },
  {
    "Date": "2025-03-14",
    "Location": "Phoenix, MD",
    "Host": "Oriole Dog Training Club",
    "TrialTypes": "NW3, L2I, NW2",
    "EventCount": 3,
    "Latitude": 39.566,
    "Longitude": -76.6177
  },
  {
    "Date": "2025-03-15",
    "Location": "Blaine, WA",
    "Host": "Nose Work Magic",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 48.9828,
    "Longitude": -122.7351
  },
  {
    "Date": "2025-03-15",
    "Location": "Califon (formerly Pomona), NY",
    "Host": "Shamrock Pot of Gold K9 Scenter",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 41.1868,
    "Longitude": -74.0755
  },
  {
    "Date": "2025-03-15",
    "Location": "Gainesville, GA",
    "Host": "Georgia Nosework",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 34.3404,
    "Longitude": -83.808
  },
  {
    "Date": "2025-03-15",
    "Location": "Kent, WA",
    "Host": "Northwest K9 Sniffers",
    "TrialTypes": "ELT-S, L1V, L1I",
    "EventCount": 3,
    "Latitude": 47.377,
    "Longitude": -122.2389
  },
  {
    "Date": "2025-03-15",
    "Location": "Pflugerville, TX",
    "Host": "Scent Work Across Texas",
    "TrialTypes": "ELT-S, NW2, L1C, L1E",
    "EventCount": 4,
    "Latitude": 30.3979,
    "Longitude": -97.6564
  },
  {
    "Date": "2025-03-15",
    "Location": "Thaxton, VA",
    "Host": "Canny K9 Companions LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 37.3289,
    "Longitude": -79.6289
  },
  {
    "Date": "2025-03-15",
    "Location": "Westminster, MD",
    "Host": "Red Huskies",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 39.5965,
    "Longitude": -77.0165
  },
  {
    "Date": "2025-03-17",
    "Location": "Corralitos, CA",
    "Host": "CalCoastal Dog Owners Group",
    "TrialTypes": "NW2, NW1",
    "EventCount": 2,
    "Latitude": 36.9669,
    "Longitude": -121.7524
  },
  {
    "Date": "2025-03-22",
    "Location": "Johnstown, NY",
    "Host": "My Dog Smells LLC",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 42.9953,
    "Longitude": -74.3834
  },
  {
    "Date": "2025-03-22",
    "Location": "Salem, WI",
    "Host": "Loving Paws Dog Training LLC",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 42.5826,
    "Longitude": -88.1218
  },
  {
    "Date": "2025-03-22",
    "Location": "Shelbyville, TN",
    "Host": "Dogs Have Amazing Noses LLC",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 35.491,
    "Longitude": -86.4685
  },
  {
    "Date": "2025-03-22",
    "Location": "Tampa, FL",
    "Host": "Hoppin’ in the Hills",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 27.9029,
    "Longitude": -82.502
  },
  {
    "Date": "2025-03-22",
    "Location": "Wakefield, VA",
    "Host": "Paws Plus Training, LLC",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 36.9771,
    "Longitude": -77.0153
  },
  {
    "Date": "2025-03-23",
    "Location": "Rapid City, SD",
    "Host": "Two Paws Up Dog Training, LLC",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 44.0581,
    "Longitude": -103.2292
  },
  {
    "Date": "2025-03-23",
    "Location": "Upland, CA",
    "Host": "Agile Paws Dog Sports",
    "TrialTypes": "NW1",
    "EventCount": 1,
    "Latitude": 34.0574,
    "Longitude": -117.6006
  },
  {
    "Date": "2025-03-28",
    "Location": "Dobbs Ferry, NY",
    "Host": "For the Love of Dogs NY, LLC",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 41.0369,
    "Longitude": -73.8757
  },
  {
    "Date": "2025-03-28",
    "Location": "Flint, MI",
    "Host": "Every Dog Nosework",
    "TrialTypes": "SMT, ELT-S, L2I",
    "EventCount": 3,
    "Latitude": 43.0449,
    "Longitude": -83.7209
  },
  {
    "Date": "2025-03-28",
    "Location": "Frederick, MD",
    "Host": "Red Huskies",
    "TrialTypes": "ELT-P, ELT, ELT-S",
    "EventCount": 3,
    "Latitude": 39.3866,
    "Longitude": -77.4029
  },
  {
    "Date": "2025-03-28",
    "Location": "Grand Junction, CO",
    "Host": "Mountain Dogs, LLC",
    "TrialTypes": "NW3, L1C, NW2, NW1",
    "EventCount": 4,
    "Latitude": 39.097,
    "Longitude": -108.5396
  },
  {
    "Date": "2025-03-28",
    "Location": "Salem, OR",
    "Host": "Kristina Leipzig, Doglandia LLC and Carol Forsberg",
    "TrialTypes": "ELT-S",
    "EventCount": 1,
    "Latitude": 44.9658,
    "Longitude": -123.0122
  },
  {
    "Date": "2025-03-28",
    "Location": "Shady Hills, FL",
    "Host": "Hoppin’ in the Hills",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 28.4079,
    "Longitude": -82.5762
  },
  {
    "Date": "2025-03-29",
    "Location": "Clinton, WI",
    "Host": "George and Shannon Carpenter",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 42.5489,
    "Longitude": -88.8836
  },
  {
    "Date": "2025-03-29",
    "Location": "Gilbertsville, PA",
    "Host": "Sniff Sniff Hooray",
    "TrialTypes": "NW2, ELT",
    "EventCount": 2,
    "Latitude": 40.356,
    "Longitude": -75.5693
  },
  {
    "Date": "2025-03-29",
    "Location": "Goleta, CA",
    "Host": "All Fur Fun",
    "TrialTypes": "ELT-P, NW3",
    "EventCount": 2,
    "Latitude": 34.3904,
    "Longitude": -119.8097
  },
  {
    "Date": "2025-03-29",
    "Location": "Kennett Square, PA",
    "Host": "The Sniffing Hound",
    "TrialTypes": "ELT-S, ELT",
    "EventCount": 2,
    "Latitude": 39.8225,
    "Longitude": -75.7404
  },
  {
    "Date": "2025-03-29",
    "Location": "LeRoy, IL",
    "Host": "Kudos for Canines",
    "TrialTypes": "NW3, L1C, L2I",
    "EventCount": 3,
    "Latitude": 42.4954,
    "Longitude": -88.7576
  },
  {
    "Date": "2025-03-29",
    "Location": "Olathe, KS",
    "Host": "Brookside Pet Training Studio for Dogs",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 38.8393,
    "Longitude": -94.8448
  },
  {
    "Date": "2025-03-30",
    "Location": "East Windsor, CT",
    "Host": "Lucky Dog Events",
    "TrialTypes": "L2V, NW2, NW3",
    "EventCount": 3,
    "Latitude": 41.9153,
    "Longitude": -72.6456
  },
  {
    "Date": "2025-04-03",
    "Location": "Alpharetta, GA",
    "Host": "Georgia Nosework",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 34.0783,
    "Longitude": -84.3036
  },
  {
    "Date": "2025-04-04",
    "Location": "Easton, MD",
    "Host": "Fair Play Labradors",
    "TrialTypes": "SMT, L1V, L2V",
    "EventCount": 3,
    "Latitude": 38.7259,
    "Longitude": -76.0951
  },
  {
    "Date": "2025-04-05",
    "Location": "Genoa, IL",
    "Host": "Common Scents K9 Scent Work Club of Elgin",
    "TrialTypes": "NW2, NW3",
    "EventCount": 2,
    "Latitude": 42.0869,
    "Longitude": -88.7099
  },
  {
    "Date": "2025-04-05",
    "Location": "Kittanning, PA",
    "Host": "Steel City Nosework, LLC",
    "TrialTypes": "ELT, ELT-S, NW2",
    "EventCount": 3,
    "Latitude": 40.858,
    "Longitude": -79.5704
  },
  {
    "Date": "2025-04-05",
    "Location": "Maple Falls, WA",
    "Host": "Nose Work Magic",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 48.9184,
    "Longitude": -122.1115
  },
  {
    "Date": "2025-04-05",
    "Location": "North Java, NY",
    "Host": "Do Over Dog Training",
    "TrialTypes": "ELT-S, L2C, L3E",
    "EventCount": 3,
    "Latitude": 42.6528,
    "Longitude": -78.3016
  },
  {
    "Date": "2025-04-05",
    "Location": "Somis, CA",
    "Host": "Pink Biscuit K9s",
    "TrialTypes": "ELT, ELT-S, NW1",
    "EventCount": 3,
    "Latitude": 34.2999,
    "Longitude": -118.9606
  },
  {
    "Date": "2025-04-05",
    "Location": "Tucson, AZ",
    "Host": "Patience Unlimited Professional Dog Training",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 32.1832,
    "Longitude": -110.9649
  },
  {
    "Date": "2025-04-05",
    "Location": "Woodstock, IL",
    "Host": "Northwest Obedience Club Inc.",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 42.3525,
    "Longitude": -88.4433
  },
  {
    "Date": "2025-04-11",
    "Location": "Sequim, WA",
    "Host": "Sarah Becker, Sea Change Canine LLC & Carol Forsberg",
    "TrialTypes": "NW2, NW1",
    "EventCount": 2,
    "Latitude": 48.0689,
    "Longitude": -123.0919
  },
  {
    "Date": "2025-04-12",
    "Location": "Auburn, WA",
    "Host": "Northwest K9 Sniffers",
    "TrialTypes": "NW3, L1C, L1E",
    "EventCount": 3,
    "Latitude": 47.3295,
    "Longitude": -122.2732
  },
  {
    "Date": "2025-04-12",
    "Location": "Boone, IA",
    "Host": "KBP Dog Training",
    "TrialTypes": "ELT, NW2",
    "EventCount": 2,
    "Latitude": 42.0412,
    "Longitude": -93.9416
  },
  {
    "Date": "2025-04-12",
    "Location": "Burton, OH",
    "Host": "Barns And Noses, LLC",
    "TrialTypes": "ELT, ELT-S, NW1",
    "EventCount": 3,
    "Latitude": 41.4488,
    "Longitude": -81.1574
  },
  {
    "Date": "2025-04-12",
    "Location": "Carlisle, PA",
    "Host": "Nose-It-All, LLC",
    "TrialTypes": "ELT, ELT-S, L2E",
    "EventCount": 3,
    "Latitude": 40.1629,
    "Longitude": -77.2296
  },
  {
    "Date": "2025-04-12",
    "Location": "Laramie, WY",
    "Host": "Mountain Dogs, LLC",
    "TrialTypes": "ELT, NW3, ELT-S, NW2",
    "EventCount": 4,
    "Latitude": 41.3574,
    "Longitude": -105.5638
  },
  {
    "Date": "2025-04-12",
    "Location": "Michigan City, IN",
    "Host": "Indiana Scentwork",
    "TrialTypes": "NW3, ELT-S, NW2",
    "EventCount": 3,
    "Latitude": 41.7354,
    "Longitude": -86.8703
  },
  {
    "Date": "2025-04-12",
    "Location": "Peekskill, NY",
    "Host": "For the Love of Dogs NY, LLC",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 41.2541,
    "Longitude": -73.962
  },
  {
    "Date": "2025-04-12",
    "Location": "Rhinebeck, NY",
    "Host": "Top Notch Dogs, LLC",
    "TrialTypes": "ELT, L1C, L2C",
    "EventCount": 3,
    "Latitude": 41.9313,
    "Longitude": -73.9162
  },
  {
    "Date": "2025-04-12",
    "Location": "Starke, FL",
    "Host": "River Poodles Training, LLC",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 29.9572,
    "Longitude": -82.1412
  },
  {
    "Date": "2025-04-14",
    "Location": "Sacramento, CA",
    "Host": "Two Nosey Girls",
    "TrialTypes": "L2E, L3E, ELT",
    "EventCount": 3,
    "Latitude": 38.6084,
    "Longitude": -121.5426
  },
  {
    "Date": "2025-04-18",
    "Location": "Asheboro, NC",
    "Host": "K9 Nose Adventures, LLC",
    "TrialTypes": "ELT, NW3, ELT-S, L2C",
    "EventCount": 4,
    "Latitude": 35.7182,
    "Longitude": -79.7942
  },
  {
    "Date": "2025-04-18",
    "Location": "Grand Junction, CO",
    "Host": "Mountain Dogs, LLC",
    "TrialTypes": "ELT, NW3, NW1, NW2",
    "EventCount": 4,
    "Latitude": 39.0223,
    "Longitude": -108.6065
  },
  {
    "Date": "2025-04-18",
    "Location": "Palmer, MA",
    "Host": "HeavenScent Sniffers",
    "TrialTypes": "ELT, NW3, NW1, NW2",
    "EventCount": 4,
    "Latitude": 42.1759,
    "Longitude": -72.2927
  },
  {
    "Date": "2025-04-18",
    "Location": "Rochester, NY",
    "Host": "Tami Sullivan",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 43.1518,
    "Longitude": -77.6248
  },
  {
    "Date": "2025-04-19",
    "Location": "Brooksville, FL",
    "Host": "Hoppin’ in the Hills",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 28.5822,
    "Longitude": -82.3414
  },
  {
    "Date": "2025-04-19",
    "Location": "Kunkletown, PA",
    "Host": "Paws n' Sniff",
    "TrialTypes": "NW3, L3E, ELT-S",
    "EventCount": 3,
    "Latitude": 40.8835,
    "Longitude": -75.4929
  },
  {
    "Date": "2025-04-24",
    "Location": "Concord, CA",
    "Host": "The Bay Team",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 38.0264,
    "Longitude": -122.0754
  },
  {
    "Date": "2025-04-25",
    "Location": "Eagan, MN",
    "Host": "St Paul Dog Training Club",
    "TrialTypes": "ELT, NW2, L2C, L3I",
    "EventCount": 4,
    "Latitude": 44.85,
    "Longitude": -93.2104
  },
  {
    "Date": "2025-04-26",
    "Location": "Decatur, GA",
    "Host": "Right Choice Dog Training LLC",
    "TrialTypes": "L1C, L1I",
    "EventCount": 2,
    "Latitude": 30.8559,
    "Longitude": -84.5506
  },
  {
    "Date": "2025-04-26",
    "Location": "Ellicottville, NY",
    "Host": "Do Over Dog Training",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 42.2327,
    "Longitude": -78.6667
  },
  {
    "Date": "2025-04-26",
    "Location": "FT. Pierce, FL",
    "Host": "Obedience Training Club of Palm Beach County",
    "TrialTypes": "L1E, NW2, NW1, L1C",
    "EventCount": 4,
    "Latitude": 27.4015,
    "Longitude": -80.2823
  },
  {
    "Date": "2025-04-26",
    "Location": "Greenfield, WI",
    "Host": "Loving Paws Dog Training LLC",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 42.9786,
    "Longitude": -87.9554
  },
  {
    "Date": "2025-04-26",
    "Location": "Hammond, LA",
    "Host": "Dog Gone Right",
    "TrialTypes": "NW1, NW2, ELT",
    "EventCount": 3,
    "Latitude": 30.504,
    "Longitude": -90.4447
  },
  {
    "Date": "2025-04-26",
    "Location": "Kingston, NH",
    "Host": "The Big Sniff, LLC",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 42.9475,
    "Longitude": -71.0196
  },
  {
    "Date": "2025-04-26",
    "Location": "Lyons, OR",
    "Host": "Canine Discovery Corps",
    "TrialTypes": "L1I, L2C, ELT",
    "EventCount": 3,
    "Latitude": 44.7899,
    "Longitude": -122.5764
  },
  {
    "Date": "2025-04-26",
    "Location": "Newtown, PA",
    "Host": "K9 Nosen Around, LLC",
    "TrialTypes": "L1V, NW1, L2V, NW2",
    "EventCount": 4,
    "Latitude": 40.267,
    "Longitude": -74.9688
  },
  {
    "Date": "2025-04-26",
    "Location": "Northampton, MA",
    "Host": "Lucky Dog Events",
    "TrialTypes": "ELT-P, ELT-S, L1C",
    "EventCount": 3,
    "Latitude": 42.2804,
    "Longitude": -72.6029
  },
  {
    "Date": "2025-04-26",
    "Location": "Ocoee, TN",
    "Host": "Camelot Shepherds, Inc.",
    "TrialTypes": "NW1, NW2, NW3",
    "EventCount": 3,
    "Latitude": 35.0968,
    "Longitude": -84.7135
  },
  {
    "Date": "2025-04-26",
    "Location": "Red Feather Lakes, CO",
    "Host": "Beyond Elevation K9",
    "TrialTypes": "NW2, NW3",
    "EventCount": 2,
    "Latitude": 40.7896,
    "Longitude": -105.5498
  },
  {
    "Date": "2025-04-26",
    "Location": "Traverse City, MI",
    "Host": "Every Dog Nosework",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 44.7767,
    "Longitude": -85.6507
  },
  {
    "Date": "2025-04-26",
    "Location": "West Friendship, MD",
    "Host": "Red Huskies",
    "TrialTypes": "NW1, NW2, L2V, L1C",
    "EventCount": 4,
    "Latitude": 39.3178,
    "Longitude": -76.9174
  },
  {
    "Date": "2025-05-01",
    "Location": "Gainesville, GA",
    "Host": "Georgia Nosework",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 34.2695,
    "Longitude": -83.8731
  },
  {
    "Date": "2025-05-02",
    "Location": "Faribault, MN",
    "Host": "St Paul Dog Training Club",
    "TrialTypes": "NW3, ELT-P, L3E, L3C",
    "EventCount": 4,
    "Latitude": 43.6325,
    "Longitude": -93.9949
  },
  {
    "Date": "2025-05-02",
    "Location": "Nyack, NY",
    "Host": "Waggin Work",
    "TrialTypes": "NW3, ELT, ELT-S",
    "EventCount": 3,
    "Latitude": 41.0424,
    "Longitude": -73.9148
  },
  {
    "Date": "2025-05-03",
    "Location": "Alexis, IL",
    "Host": "Kudos for Canines",
    "TrialTypes": "NW3, ELT-P",
    "EventCount": 2,
    "Latitude": 41.1064,
    "Longitude": -90.5797
  },
  {
    "Date": "2025-05-03",
    "Location": "Ashby, MA",
    "Host": "Carolyn Barney dba Dogs!",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 42.672,
    "Longitude": -71.8092
  },
  {
    "Date": "2025-05-03",
    "Location": "Columbus, MT",
    "Host": "Canine Connection",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 45.6476,
    "Longitude": -109.2924
  },
  {
    "Date": "2025-05-03",
    "Location": "Gray Court, SC",
    "Host": "Foothills Canine Academy, LLC",
    "TrialTypes": "L1V, NW1, NW3",
    "EventCount": 3,
    "Latitude": 34.6507,
    "Longitude": -82.0764
  },
  {
    "Date": "2025-05-03",
    "Location": "Redwood City, CA",
    "Host": "B. L. McMutts",
    "TrialTypes": "NW2, NW1",
    "EventCount": 2,
    "Latitude": 37.4399,
    "Longitude": -122.2773
  },
  {
    "Date": "2025-05-03",
    "Location": "Sandy, OR",
    "Host": "Trust Your Dog K9 Events",
    "TrialTypes": "NW2, NW3",
    "EventCount": 2,
    "Latitude": 45.4284,
    "Longitude": -122.2766
  },
  {
    "Date": "2025-05-03",
    "Location": "Santa Paula, CA",
    "Host": "Pink Biscuit K9s",
    "TrialTypes": "ELT, NW1, NW2",
    "EventCount": 3,
    "Latitude": 34.3613,
    "Longitude": -119.0607
  },
  {
    "Date": "2025-05-03",
    "Location": "White Salmon, WA",
    "Host": "Trisha Thompson and Sharon Smith",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 45.6818,
    "Longitude": -121.5183
  },
  {
    "Date": "2025-05-09",
    "Location": "South Sterling, PA",
    "Host": "Paws n' Sniff",
    "TrialTypes": "L3C, L2I, NW2",
    "EventCount": 3,
    "Latitude": 41.2738,
    "Longitude": -75.321
  },
  {
    "Date": "2025-05-09",
    "Location": "Warwick, NY",
    "Host": "Top Notch Dogs, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 41.2223,
    "Longitude": -74.3701
  },
  {
    "Date": "2025-05-10",
    "Location": "Alexander, NY",
    "Host": "Do Over Dog Training",
    "TrialTypes": "L1V, L2I, L1C, L3V",
    "EventCount": 4,
    "Latitude": 42.9282,
    "Longitude": -78.2643
  },
  {
    "Date": "2025-05-10",
    "Location": "Denton, MD",
    "Host": "Fair Play Labradors",
    "TrialTypes": "NW3, ELT-S",
    "EventCount": 2,
    "Latitude": 38.8878,
    "Longitude": -75.827
  },
  {
    "Date": "2025-05-10",
    "Location": "Poland Springs, ME",
    "Host": "Virginia Howe",
    "TrialTypes": "NW2, ELT",
    "EventCount": 2,
    "Latitude": 44.0697,
    "Longitude": -70.3316
  },
  {
    "Date": "2025-05-10",
    "Location": "Rainier, WA",
    "Host": "Rachelle Bailey-Austin/About Face K9 Academy & Dorothy Turley/Let's Talk Dogs, LLC",
    "TrialTypes": "ELT-S, NW2",
    "EventCount": 2,
    "Latitude": 46.9408,
    "Longitude": -122.676
  },
  {
    "Date": "2025-05-10",
    "Location": "Santa Barbara, CA",
    "Host": "All Fur Fun",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 34.3779,
    "Longitude": -119.6835
  },
  {
    "Date": "2025-05-13",
    "Location": "Paso Robles, CA",
    "Host": "Gentle Touch Pet Training",
    "TrialTypes": "L2C, NW1",
    "EventCount": 2,
    "Latitude": 35.6326,
    "Longitude": -120.6716
  },
  {
    "Date": "2025-05-16",
    "Location": "Montrose, CO",
    "Host": "Mountain Dogs, LLC",
    "TrialTypes": "ELT, NW3, ELT-P",
    "EventCount": 3,
    "Latitude": 38.4665,
    "Longitude": -107.8515
  },
  {
    "Date": "2025-05-16",
    "Location": "Watsonville, CA",
    "Host": "CalCoastal Dog Owners Group",
    "TrialTypes": "ELT-S, L2C, L3C",
    "EventCount": 3,
    "Latitude": 36.939,
    "Longitude": -121.7703
  },
  {
    "Date": "2025-05-17",
    "Location": "Burien, WA",
    "Host": "Northwest K9 Sniffers",
    "TrialTypes": "ELT, L2V, L2I",
    "EventCount": 3,
    "Latitude": 47.4497,
    "Longitude": -122.3646
  },
  {
    "Date": "2025-05-17",
    "Location": "Cobleskill, NY",
    "Host": "The Brainy Canine",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 42.6872,
    "Longitude": -74.4861
  },
  {
    "Date": "2025-05-17",
    "Location": "Emmitsburg, MD",
    "Host": "Red Huskies",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 39.7202,
    "Longitude": -77.339
  },
  {
    "Date": "2025-05-17",
    "Location": "Forest Junction, WI",
    "Host": "N.E.W K9 Scent Work LLC",
    "TrialTypes": "L1C, NW1, NW2",
    "EventCount": 3,
    "Latitude": 44.2033,
    "Longitude": -88.1556
  },
  {
    "Date": "2025-05-17",
    "Location": "Norton, MA",
    "Host": "Dogs Make Scents",
    "TrialTypes": "NW3, NW1",
    "EventCount": 2,
    "Latitude": 42.0104,
    "Longitude": -71.2314
  },
  {
    "Date": "2025-05-17",
    "Location": "Peru, MA",
    "Host": "Lucky Dog Events",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 42.4124,
    "Longitude": -73.0079
  },
  {
    "Date": "2025-05-17",
    "Location": "Valley Forge, PA",
    "Host": "Sniff Sniff Hooray",
    "TrialTypes": "NW1, ELT",
    "EventCount": 2,
    "Latitude": 40.1308,
    "Longitude": -75.4679
  },
  {
    "Date": "2025-05-23",
    "Location": "La Jolla, CA",
    "Host": "Anita Cheesman and Jessica Koester",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 32.8693,
    "Longitude": -117.2266
  },
  {
    "Date": "2025-05-24",
    "Location": "Altamont , NY",
    "Host": "My Dog Smells LLC",
    "TrialTypes": "ELT, NW2",
    "EventCount": 2,
    "Latitude": 42.7501,
    "Longitude": -74.0457
  },
  {
    "Date": "2025-05-24",
    "Location": "Batavia, NY",
    "Host": "Do Over Dog Training",
    "TrialTypes": "ELT-P, L1I, L3I",
    "EventCount": 3,
    "Latitude": 42.9927,
    "Longitude": -78.2061
  },
  {
    "Date": "2025-05-24",
    "Location": "Lancaster, PA",
    "Host": "Nose-It-All, LLC",
    "TrialTypes": "L3I, NW2, ELT",
    "EventCount": 3,
    "Latitude": 40.046,
    "Longitude": -76.2789
  },
  {
    "Date": "2025-05-24",
    "Location": "Norwich , CT",
    "Host": "For the Love of Dogs NY LLC",
    "TrialTypes": "ELT-S, NW2, NW3, ELT",
    "EventCount": 4,
    "Latitude": 41.5288,
    "Longitude": -72.0548
  },
  {
    "Date": "2025-05-24",
    "Location": "Rockaway, NJ",
    "Host": "Shamrock Pot of Gold K9 Scenter",
    "TrialTypes": "NW3, ELT-S, NW1, ELT-P",
    "EventCount": 4,
    "Latitude": 40.9105,
    "Longitude": -74.5565
  },
  {
    "Date": "2025-05-24",
    "Location": "Waukesha , WI",
    "Host": "Think Pawsitive Dog Training",
    "TrialTypes": "NW1, L1V, L1C",
    "EventCount": 3,
    "Latitude": 43.0285,
    "Longitude": -88.2808
  },
  {
    "Date": "2025-05-24",
    "Location": "Welches, OR",
    "Host": "Trust Your Dog K9 Events",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 45.2902,
    "Longitude": -122.0068
  },
  {
    "Date": "2025-05-29",
    "Location": "Bayfield, CO",
    "Host": "Wag Between Barks",
    "TrialTypes": "ELT-S, ELT, NW3",
    "EventCount": 3,
    "Latitude": 37.2169,
    "Longitude": -107.6367
  },
  {
    "Date": "2025-05-30",
    "Location": "Honesdale, PA",
    "Host": "Your Dog's Place, LLC",
    "TrialTypes": "NW3, ELT, ELT-S, NW2",
    "EventCount": 4,
    "Latitude": 41.5938,
    "Longitude": -75.284
  },
  {
    "Date": "2025-05-30",
    "Location": "Moline, IL",
    "Host": "Fur Better Fur Worse Dog Training",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 41.5256,
    "Longitude": -90.4693
  },
  {
    "Date": "2025-05-31",
    "Location": "Amherst, NY",
    "Host": "Do Over Dog Training",
    "TrialTypes": "NW2, ELT-S, NW1",
    "EventCount": 3,
    "Latitude": 42.9459,
    "Longitude": -78.7965
  },
  {
    "Date": "2025-05-31",
    "Location": "Columbus, MT",
    "Host": "Canine Connection",
    "TrialTypes": "L1V, NW1, NW3",
    "EventCount": 3,
    "Latitude": 45.6499,
    "Longitude": -109.2176
  },
  {
    "Date": "2025-05-31",
    "Location": "Eden Prairie, MN",
    "Host": "The K9 Nose",
    "TrialTypes": "NW1, L1I, L2I",
    "EventCount": 3,
    "Latitude": 44.9046,
    "Longitude": -93.5061
  },
  {
    "Date": "2025-05-31",
    "Location": "Napa, CA",
    "Host": "Napa Valley Dog Training Club",
    "TrialTypes": "NW2, NW1",
    "EventCount": 2,
    "Latitude": 38.4475,
    "Longitude": -122.3618
  },
  {
    "Date": "2025-05-31",
    "Location": "New Wilmington, PA",
    "Host": "Steel City Nosework, LLC",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 41.1513,
    "Longitude": -80.3382
  },
  {
    "Date": "2025-05-31",
    "Location": "North Manchester, IN",
    "Host": "2 Nose You Is 2 Loves You",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 41.0113,
    "Longitude": -85.8077
  },
  {
    "Date": "2025-06-06",
    "Location": "Pueblo, CO",
    "Host": "Mountain Dogs, LLC",
    "TrialTypes": "SMT, ELT",
    "EventCount": 2,
    "Latitude": 38.3011,
    "Longitude": -104.6107
  },
  {
    "Date": "2025-06-06",
    "Location": "Winsted, CT",
    "Host": "Waggin’ Work",
    "TrialTypes": "ELT, NW2, L2C",
    "EventCount": 3,
    "Latitude": 41.9544,
    "Longitude": -73.0883
  },
  {
    "Date": "2025-06-07",
    "Location": "Clancy, MT",
    "Host": "Nosework Breakfast Club",
    "TrialTypes": "NW2, NW3",
    "EventCount": 2,
    "Latitude": 46.4208,
    "Longitude": -112.0257
  },
  {
    "Date": "2025-06-07",
    "Location": "Cumming, GA",
    "Host": "Georgia Nosework",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 34.2207,
    "Longitude": -84.1218
  },
  {
    "Date": "2025-06-07",
    "Location": "Davenport, IA",
    "Host": "Fur Better Fur Worse Dog Training",
    "TrialTypes": "ELT-S, NW2, NW1",
    "EventCount": 3,
    "Latitude": 41.4797,
    "Longitude": -90.6094
  },
  {
    "Date": "2025-06-07",
    "Location": "Enterprise, OR",
    "Host": "Country K9 Nosework, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 45.3774,
    "Longitude": -117.2623
  },
  {
    "Date": "2025-06-07",
    "Location": "Grants Pass, OR",
    "Host": "Nose Work Detectives",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 42.4773,
    "Longitude": -123.3017
  },
  {
    "Date": "2025-06-07",
    "Location": "Meadowbrook, PA",
    "Host": "Patricia Grassey",
    "TrialTypes": "NW1, NW2, ELT-S, ELT",
    "EventCount": 4,
    "Latitude": 40.118,
    "Longitude": -75.0443
  },
  {
    "Date": "2025-06-07",
    "Location": "Palmyra, VA",
    "Host": "Your Dog Knows, LLC",
    "TrialTypes": "NW1, L1I",
    "EventCount": 2,
    "Latitude": 37.8677,
    "Longitude": -78.2637
  },
  {
    "Date": "2025-06-07",
    "Location": "Wrightstown, WI",
    "Host": "N.E.W. K9 Scent Work, LLC",
    "TrialTypes": "ELT-P, L2C, L3I",
    "EventCount": 3,
    "Latitude": 44.3445,
    "Longitude": -88.119
  },
  {
    "Date": "2025-06-13",
    "Location": "Jordan, MN",
    "Host": "St Paul Dog Training Club",
    "TrialTypes": "NW3, ELT-S, L1V, L2E, L3V",
    "EventCount": 5,
    "Latitude": 44.6924,
    "Longitude": -93.5796
  },
  {
    "Date": "2025-06-14",
    "Location": "Cummington, MA",
    "Host": "Lucky Dog Events",
    "TrialTypes": "ELT-P, ELT-S, L2C",
    "EventCount": 3,
    "Latitude": 42.4751,
    "Longitude": -72.8927
  },
  {
    "Date": "2025-06-14",
    "Location": "Danvers, MA",
    "Host": "Everydog, LLC",
    "TrialTypes": "L2I, NW2, NW1",
    "EventCount": 3,
    "Latitude": 42.5587,
    "Longitude": -70.9691
  },
  {
    "Date": "2025-06-14",
    "Location": "Ithaca, NY",
    "Host": "The Brainy Canine",
    "TrialTypes": "ELT-P, ELT",
    "EventCount": 2,
    "Latitude": 42.4189,
    "Longitude": -76.5104
  },
  {
    "Date": "2025-06-14",
    "Location": "Linden, MI",
    "Host": "Every Dog Nosework",
    "TrialTypes": "NW1, ELT-P",
    "EventCount": 2,
    "Latitude": 42.7722,
    "Longitude": -83.7947
  },
  {
    "Date": "2025-06-20",
    "Location": "Greeley, CO",
    "Host": "Beyond Elevation K9",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 40.3749,
    "Longitude": -104.7537
  },
  {
    "Date": "2025-06-20",
    "Location": "San Luis Obispo, CA",
    "Host": "Central Coast Nosework Club of California, Inc.",
    "TrialTypes": "NW2, ELT",
    "EventCount": 2,
    "Latitude": 35.3671,
    "Longitude": -120.3571
  },
  {
    "Date": "2025-06-20",
    "Location": "Upland, CA",
    "Host": "Agile Paws Dog Sports",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 34.0975,
    "Longitude": -117.6915
  },
  {
    "Date": "2025-06-20",
    "Location": "Warwick, NY",
    "Host": "For the Love of Dogs NY LLC",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 41.2856,
    "Longitude": -74.392
  },
  {
    "Date": "2025-06-21",
    "Location": "Fayette, MO",
    "Host": "Columbia Canine Sports Center",
    "TrialTypes": "ELT, NW1, NW2",
    "EventCount": 3,
    "Latitude": 39.1519,
    "Longitude": -92.6931
  },
  {
    "Date": "2025-06-21",
    "Location": "Inver Grove Heights, MN",
    "Host": "Outside The Box Dog Training, LLC",
    "TrialTypes": "L1C, NW2, ELT",
    "EventCount": 3,
    "Latitude": 44.8649,
    "Longitude": -93.0625
  },
  {
    "Date": "2025-06-21",
    "Location": "Jefferson, WI",
    "Host": "Think Pawsitive Dog Training",
    "TrialTypes": "NW2",
    "EventCount": 1,
    "Latitude": 43.0145,
    "Longitude": -88.7385
  },
  {
    "Date": "2025-06-21",
    "Location": "Toledo, OH",
    "Host": "Robin Ford Dog Training, LLC",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 41.6324,
    "Longitude": -83.5043
  },
  {
    "Date": "2025-06-21",
    "Location": "Woodstock, GA",
    "Host": "Georgia Nosework LLC",
    "TrialTypes": "NW3, L2C, NW1",
    "EventCount": 3,
    "Latitude": 34.0728,
    "Longitude": -84.5608
  },
  {
    "Date": "2025-06-25",
    "Location": "Kenai, AK",
    "Host": "Peninsula Dog Obedience Group",
    "TrialTypes": "NW1, NW2, NW3, ELT",
    "EventCount": 4,
    "Latitude": 60.5516,
    "Longitude": -151.2767
  },
  {
    "Date": "2025-06-28",
    "Location": "Delran, NJ",
    "Host": "K9 InScentives",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 39.9704,
    "Longitude": -74.9978
  },
  {
    "Date": "2025-06-28",
    "Location": "Deming, WA",
    "Host": "Nose Work Magic",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 48.8418,
    "Longitude": -122.2101
  },
  {
    "Date": "2025-06-28",
    "Location": "Kenosha, WI",
    "Host": "Loving Paws Dog Training LLC",
    "TrialTypes": "NW3, NW1",
    "EventCount": 2,
    "Latitude": 42.5792,
    "Longitude": -87.7849
  },
  {
    "Date": "2025-06-28",
    "Location": "Somers, CT",
    "Host": "HeavenScent Sniffers",
    "TrialTypes": "ELT, L1V, NW1",
    "EventCount": 3,
    "Latitude": 41.9847,
    "Longitude": -72.487
  },
  {
    "Date": "2025-06-28",
    "Location": "St. Paul, MN",
    "Host": "Bark and Bond LLC",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 44.9159,
    "Longitude": -93.0676
  },
  {
    "Date": "2025-06-28",
    "Location": "Stevenson, WA",
    "Host": "Sharon Smith",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 45.6628,
    "Longitude": -121.8342
  },
  {
    "Date": "2025-07-04",
    "Location": "Huntington, MA",
    "Host": "Shamrock Pot Of Gold K9 Scenter",
    "TrialTypes": "ELT, NW3, L2I, ELT-S",
    "EventCount": 4,
    "Latitude": 42.2408,
    "Longitude": -72.8666
  },
  {
    "Date": "2025-07-05",
    "Location": "Delran, NJ",
    "Host": "Ev-ry Earthdog, LLC",
    "TrialTypes": "NW3, NW1, NW2, ELT",
    "EventCount": 4,
    "Latitude": 40.0148,
    "Longitude": -74.9785
  },
  {
    "Date": "2025-07-11",
    "Location": "Leadville, CO",
    "Host": "Mountain Dogs LLC",
    "TrialTypes": "ELT, NW3, ELT-S, NW2",
    "EventCount": 4,
    "Latitude": 39.2043,
    "Longitude": -106.2489
  },
  {
    "Date": "2025-07-12",
    "Location": "Brainerd, MN",
    "Host": "Nose 2 Tail Dog Training LLC",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 46.3183,
    "Longitude": -94.239
  },
  {
    "Date": "2025-07-12",
    "Location": "Livonia, MI",
    "Host": "Every Dog Nosework",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 42.3322,
    "Longitude": -83.3843
  },
  {
    "Date": "2025-07-18",
    "Location": "Leadville, CO",
    "Host": "Mountain Dogs, LLC",
    "TrialTypes": "NW3, NW1, NW2, L1C, L1I",
    "EventCount": 5,
    "Latitude": 39.2981,
    "Longitude": -106.3131
  },
  {
    "Date": "2025-07-19",
    "Location": "Dunmore, PA",
    "Host": "Your Dog's Place, LLC",
    "TrialTypes": "ELT-S, NW1, L1C",
    "EventCount": 3,
    "Latitude": 41.4428,
    "Longitude": -75.6335
  },
  {
    "Date": "2025-07-19",
    "Location": "Fayette , MO",
    "Host": "Columbia Canine Sports Center",
    "TrialTypes": "NW3, L1C, L1I",
    "EventCount": 3,
    "Latitude": 39.106,
    "Longitude": -92.7217
  },
  {
    "Date": "2025-07-19",
    "Location": "Houlton, WI",
    "Host": "Gretchen Hofheins-Wackerfuss",
    "TrialTypes": "NW1, ELT-S, ELT-P",
    "EventCount": 3,
    "Latitude": 45.0563,
    "Longitude": -92.7887
  },
  {
    "Date": "2025-07-19",
    "Location": "Walpole, MA",
    "Host": "MasterPeace Dog Training",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 42.1784,
    "Longitude": -71.298
  },
  {
    "Date": "2025-07-21",
    "Location": "Montgomery, NY",
    "Host": "For the Love of Dogs NY LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 42.8843,
    "Longitude": -74.406
  },
  {
    "Date": "2025-08-02",
    "Location": "Altamont, IL",
    "Host": "Kudos for Canines, LLC",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 39.0466,
    "Longitude": -88.704
  },
  {
    "Date": "2025-08-02",
    "Location": "Anchorage, AK",
    "Host": "Alaska Dog Sports, LLC",
    "TrialTypes": "NW3, NW2, NW1",
    "EventCount": 3,
    "Latitude": 61.2235,
    "Longitude": -149.9256
  },
  {
    "Date": "2025-08-02",
    "Location": "Bettendorf, IA",
    "Host": "Fur Better Fur Worse Dog Training",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 41.4809,
    "Longitude": -90.4889
  },
  {
    "Date": "2025-08-02",
    "Location": "Jefferson, WI",
    "Host": "Think Pawsitive Dog Training LLC",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 43.0672,
    "Longitude": -88.7477
  },
  {
    "Date": "2025-08-02",
    "Location": "Pillager, MN",
    "Host": "Nose 2 Tail Dog Training, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 46.2953,
    "Longitude": -94.4538
  },
  {
    "Date": "2025-08-02",
    "Location": "Red Lodge, MT",
    "Host": "Canine Connection",
    "TrialTypes": "L1I, ELT-S, ELT",
    "EventCount": 3,
    "Latitude": 45.1919,
    "Longitude": -109.2912
  },
  {
    "Date": "2025-08-02",
    "Location": "Rochester, NY",
    "Host": "Suzan Tessier",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 43.1367,
    "Longitude": -77.5955
  },
  {
    "Date": "2025-08-11",
    "Location": "Cambria, CA",
    "Host": "Gentle Touch Pet Training",
    "TrialTypes": "L1C, L2I, ELT",
    "EventCount": 3,
    "Latitude": 35.5335,
    "Longitude": -121.0948
  },
  {
    "Date": "2025-08-15",
    "Location": "Huntington Beach, CA",
    "Host": "JavaK9s, LLC",
    "TrialTypes": "ELT-P, L1C, L1I",
    "EventCount": 3,
    "Latitude": 33.6397,
    "Longitude": -118.0153
  },
  {
    "Date": "2025-08-16",
    "Location": "Colesville , MD",
    "Host": "Red Huskies",
    "TrialTypes": "L3C, ELT-S, NW2, ELT",
    "EventCount": 4,
    "Latitude": 39.0513,
    "Longitude": -76.9595
  },
  {
    "Date": "2025-08-16",
    "Location": "Mount Kisco, NY",
    "Host": "For the Love of Dogs, LLC",
    "TrialTypes": "L2I, NW2, L1I, NW1",
    "EventCount": 4,
    "Latitude": 41.19,
    "Longitude": -73.6923
  },
  {
    "Date": "2025-08-16",
    "Location": "Reedsport, OR",
    "Host": "Kiddy Christie",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 43.7413,
    "Longitude": -124.1359
  },
  {
    "Date": "2025-08-22",
    "Location": "Chelsea, MI",
    "Host": "Force Free Dale, LLC",
    "TrialTypes": "NW3, NW2, NW1",
    "EventCount": 3,
    "Latitude": 42.351,
    "Longitude": -83.9814
  },
  {
    "Date": "2025-08-23",
    "Location": "Gervais, OR",
    "Host": "Helix Fairweather & Doglandia, LLC",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 45.1035,
    "Longitude": -122.9304
  },
  {
    "Date": "2025-08-23",
    "Location": "Johnstown, NY",
    "Host": "My Dog Smells, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 42.9767,
    "Longitude": -74.4075
  },
  {
    "Date": "2025-08-23",
    "Location": "Tyngsborough, MA",
    "Host": "Spot-On K9 Coaching",
    "TrialTypes": "NW3, L1C, L1I",
    "EventCount": 3,
    "Latitude": 42.644,
    "Longitude": -71.4596
  },
  {
    "Date": "2025-08-29",
    "Location": "Bridger, MT",
    "Host": "Canine Connection",
    "TrialTypes": "NW1, L2I, NW3",
    "EventCount": 3,
    "Latitude": 45.2655,
    "Longitude": -108.9557
  },
  {
    "Date": "2025-08-30",
    "Location": "Eliot, ME",
    "Host": "McLean Pups, LLC",
    "TrialTypes": "L1V, L1E",
    "EventCount": 2,
    "Latitude": 43.1086,
    "Longitude": -70.7324
  },
  {
    "Date": "2025-08-30",
    "Location": "Fort Worth, TX",
    "Host": "North Texas Nosework Club",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 32.801,
    "Longitude": -97.2921
  },
  {
    "Date": "2025-08-30",
    "Location": "Pomfret Center, CT",
    "Host": "HeavenScent Sniffers",
    "TrialTypes": "NW3, NW2, NW1",
    "EventCount": 3,
    "Latitude": 41.8994,
    "Longitude": -72.0024
  },
  {
    "Date": "2025-08-31",
    "Location": "Helena, MT",
    "Host": "Nosework Breakfast Club",
    "TrialTypes": "NW2, NW3",
    "EventCount": 2,
    "Latitude": 46.6096,
    "Longitude": -112.0102
  },
  {
    "Date": "2025-09-05",
    "Location": "Centreville, MD",
    "Host": "Fair Play Labradors",
    "TrialTypes": "ELT, NW3, ELT-S",
    "EventCount": 3,
    "Latitude": 39.0269,
    "Longitude": -76.0595
  },
  {
    "Date": "2025-09-06",
    "Location": "Dunkirk, NY",
    "Host": "Do Over Dog Training",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 42.4693,
    "Longitude": -79.3592
  },
  {
    "Date": "2025-09-06",
    "Location": "Loma Mar, CA",
    "Host": "The Bay Team",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 37.2898,
    "Longitude": -122.2989
  },
  {
    "Date": "2025-09-06",
    "Location": "North Bend, WA",
    "Host": "Northwest K9 Sniffers",
    "TrialTypes": "NW2, L2E, L2C",
    "EventCount": 3,
    "Latitude": 47.4947,
    "Longitude": -121.8056
  },
  {
    "Date": "2025-09-12",
    "Location": "Frederick, MD",
    "Host": "Red Huskies",
    "TrialTypes": "NW3, ELT, ELT-S",
    "EventCount": 3,
    "Latitude": 39.4432,
    "Longitude": -77.44
  },
  {
    "Date": "2025-09-12",
    "Location": "Honey Brook, PA",
    "Host": "Patricia Grassey",
    "TrialTypes": "ELT, ELT-S, NW3",
    "EventCount": 3,
    "Latitude": 40.0588,
    "Longitude": -75.954
  },
  {
    "Date": "2025-09-12",
    "Location": "Lakeville, MN",
    "Host": "St Paul Dog Training Club",
    "TrialTypes": "ELT, NW2, NW1",
    "EventCount": 3,
    "Latitude": 44.661,
    "Longitude": -93.273
  },
  {
    "Date": "2025-09-12",
    "Location": "New Milford, PA",
    "Host": "Your Dog's Place, LLC",
    "TrialTypes": "ELT-P, ELT-S, L2V, L1E",
    "EventCount": 4,
    "Latitude": 41.9181,
    "Longitude": -75.7345
  },
  {
    "Date": "2025-09-13",
    "Location": "Ames, IA",
    "Host": "KBP Dog Training",
    "TrialTypes": "ELT-S, ELT",
    "EventCount": 2,
    "Latitude": 42.0716,
    "Longitude": -93.5777
  },
  {
    "Date": "2025-09-13",
    "Location": "Colebrook, CT",
    "Host": "For the Love of Dogs, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 41.9747,
    "Longitude": -73.063
  },
  {
    "Date": "2025-09-13",
    "Location": "Hermosa, SD",
    "Host": "Two Paws Up Dog Training, LLC",
    "TrialTypes": "NW3, L1C, L1I",
    "EventCount": 3,
    "Latitude": 43.8612,
    "Longitude": -103.1934
  },
  {
    "Date": "2025-09-13",
    "Location": "Jefferson, GA",
    "Host": "Right Choice Dog Training, LLC",
    "TrialTypes": "L2V, NW2, ELT-S, L1C",
    "EventCount": 4,
    "Latitude": 33.0637,
    "Longitude": -82.4286
  },
  {
    "Date": "2025-09-13",
    "Location": "Pittsburgh , PA",
    "Host": "Steel City Nosework, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 40.4128,
    "Longitude": -80.0365
  },
  {
    "Date": "2025-09-15",
    "Location": "Green Lane, PA",
    "Host": "Sniff Sniff Hooray",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 40.2981,
    "Longitude": -75.5057
  },
  {
    "Date": "2025-09-19",
    "Location": "Flint, MI",
    "Host": "Every Dog Nosework",
    "TrialTypes": "NW3, ELT-S, L1C, NW2",
    "EventCount": 4,
    "Latitude": 43.0189,
    "Longitude": -83.6512
  },
  {
    "Date": "2025-09-19",
    "Location": "Fruita, CO",
    "Host": "Mountain Dogs LLC",
    "TrialTypes": "ELT, NW3, NW2",
    "EventCount": 3,
    "Latitude": 39.1772,
    "Longitude": -108.7771
  },
  {
    "Date": "2025-09-20",
    "Location": "Cumming, GA",
    "Host": "Georgia Nosework",
    "TrialTypes": "NW3, ELT-S, L2I, ELT",
    "EventCount": 4,
    "Latitude": 34.2082,
    "Longitude": -84.1149
  },
  {
    "Date": "2025-09-20",
    "Location": "Darlington, MD",
    "Host": "Firezone GS",
    "TrialTypes": "NW3, L1E, NW2",
    "EventCount": 3,
    "Latitude": 39.6926,
    "Longitude": -76.1728
  },
  {
    "Date": "2025-09-20",
    "Location": "Egg Harbor City, NJ",
    "Host": "Rotts-n-Notts Nosework, LLC",
    "TrialTypes": "NW3, L2I, NW2",
    "EventCount": 3,
    "Latitude": 39.5628,
    "Longitude": -74.6016
  },
  {
    "Date": "2025-09-20",
    "Location": "Fishkill, NY",
    "Host": "Top Notch Dogs, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 41.5468,
    "Longitude": -73.8524
  },
  {
    "Date": "2025-09-20",
    "Location": "Mesquite, TX",
    "Host": "All About The Nose",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 32.7419,
    "Longitude": -96.6188
  },
  {
    "Date": "2025-09-20",
    "Location": "Novato, CA",
    "Host": "Marin Humane",
    "TrialTypes": "NW1, NW2, NW3",
    "EventCount": 3,
    "Latitude": 38.1222,
    "Longitude": -122.5321
  },
  {
    "Date": "2025-09-20",
    "Location": "Sunriver, OR",
    "Host": "Canine Discovery Corps",
    "TrialTypes": "NW1, NW2, NW3",
    "EventCount": 3,
    "Latitude": 43.8982,
    "Longitude": -121.4073
  },
  {
    "Date": "2025-09-20",
    "Location": "Tuftonboro, NH",
    "Host": "The Big Sniff, LLC",
    "TrialTypes": "NW2, L2E, L2V",
    "EventCount": 3,
    "Latitude": 43.6789,
    "Longitude": -71.2717
  },
  {
    "Date": "2025-09-20",
    "Location": "White Salmon, WA",
    "Host": "Sharon Smith",
    "TrialTypes": "NW1, NW2, NW3",
    "EventCount": 3,
    "Latitude": 45.7313,
    "Longitude": -121.506
  },
  {
    "Date": "2025-09-26",
    "Location": "Hockessin, DE",
    "Host": "Patricia Grassey",
    "TrialTypes": "L2E, NW2, NW1, L2I, NW3",
    "EventCount": 5,
    "Latitude": 39.8186,
    "Longitude": -75.6539
  },
  {
    "Date": "2025-09-27",
    "Location": "Copake, NY",
    "Host": "Saints2Source, LLC",
    "TrialTypes": "NW3, ELT-P, ELT",
    "EventCount": 3,
    "Latitude": 42.0605,
    "Longitude": -73.5147
  },
  {
    "Date": "2025-09-27",
    "Location": "Florissant, MO",
    "Host": "Happy Dog Concepts, LLC",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 38.8243,
    "Longitude": -90.357
  },
  {
    "Date": "2025-09-27",
    "Location": "Kilmarnock, VA",
    "Host": "Red Huskies",
    "TrialTypes": "ELT, ELT-S, NW2",
    "EventCount": 3,
    "Latitude": 37.7044,
    "Longitude": -76.3386
  },
  {
    "Date": "2025-09-27",
    "Location": "Moultonborough, NH",
    "Host": "Dogs Makes Scents",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 43.7092,
    "Longitude": -71.4123
  },
  {
    "Date": "2025-09-27",
    "Location": "Reedsport, OR",
    "Host": "Wells Creek Dog Training",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 43.6673,
    "Longitude": -124.0552
  },
  {
    "Date": "2025-09-27",
    "Location": "Waynesboro, PA",
    "Host": "Nose-It-All, LLC",
    "TrialTypes": "L3E, NW2, ELT",
    "EventCount": 3,
    "Latitude": 39.8009,
    "Longitude": -77.57
  },
  {
    "Date": "2025-10-03",
    "Location": "Middlebury, CT",
    "Host": "For the Love of Dogs NY LLC",
    "TrialTypes": "NW3, ELT-S, ELT",
    "EventCount": 3,
    "Latitude": 41.5459,
    "Longitude": -73.0948
  },
  {
    "Date": "2025-10-04",
    "Location": "Crosslake, MN",
    "Host": "Nose 2 Tail Dog Training LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 46.6396,
    "Longitude": -94.1263
  },
  {
    "Date": "2025-10-04",
    "Location": "Nashua, NH",
    "Host": "The Big Sniff, LLC",
    "TrialTypes": "ELT, L3I, L2C",
    "EventCount": 3,
    "Latitude": 42.7277,
    "Longitude": -71.5065
  },
  {
    "Date": "2025-10-04",
    "Location": "New Paltz, NY",
    "Host": "Top Notch Dogs, LLC",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 41.7382,
    "Longitude": -74.0716
  },
  {
    "Date": "2025-10-04",
    "Location": "Smithton, PA",
    "Host": "Steel City Nosework, LLC",
    "TrialTypes": "NW2, ELT",
    "EventCount": 2,
    "Latitude": 40.1125,
    "Longitude": -79.7502
  },
  {
    "Date": "2025-10-06",
    "Location": "Monterey, CA",
    "Host": "CalCoastal Dog Owners Group",
    "TrialTypes": "ELT-S, NW1, L2I, NW2",
    "EventCount": 4,
    "Latitude": 36.2611,
    "Longitude": -121.4152
  },
  {
    "Date": "2025-10-10",
    "Location": "West Bend, WI",
    "Host": "Think Pawsitive Dog Training LLC",
    "TrialTypes": "ELT, L2C, L1E",
    "EventCount": 3,
    "Latitude": 43.4203,
    "Longitude": -88.1589
  },
  {
    "Date": "2025-10-11",
    "Location": "Bloomington, MN",
    "Host": "St Paul Dog Training Club",
    "TrialTypes": "ELT-P, ELT-S",
    "EventCount": 2,
    "Latitude": 44.7845,
    "Longitude": -93.2804
  },
  {
    "Date": "2025-10-11",
    "Location": "Colfax, IA",
    "Host": "KBP Dog Training",
    "TrialTypes": "NW1, NW2, NW3",
    "EventCount": 3,
    "Latitude": 41.7107,
    "Longitude": -93.2761
  },
  {
    "Date": "2025-10-11",
    "Location": "Columbus, MT",
    "Host": "Canine Connection",
    "TrialTypes": "L1C, NW2, NW3",
    "EventCount": 3,
    "Latitude": 45.6825,
    "Longitude": -109.2129
  },
  {
    "Date": "2025-10-11",
    "Location": "Durham, NC",
    "Host": "Whole Dog Institute, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 36.0099,
    "Longitude": -78.9007
  },
  {
    "Date": "2025-10-11",
    "Location": "Ferndale, WA",
    "Host": "Nose Work Magic",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 48.8615,
    "Longitude": -122.6207
  },
  {
    "Date": "2025-10-11",
    "Location": "Loveland, CO",
    "Host": "Paws 4 Thought Dog Training, LLC",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 40.447,
    "Longitude": -105.0614
  },
  {
    "Date": "2025-10-11",
    "Location": "New City , NY",
    "Host": "Saints2Source, LLC",
    "TrialTypes": "NW2, NW3, ELT",
    "EventCount": 3,
    "Latitude": 41.1794,
    "Longitude": -73.9748
  },
  {
    "Date": "2025-10-11",
    "Location": "Occidental, CA",
    "Host": "Marin Humane",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 38.3911,
    "Longitude": -122.9
  },
  {
    "Date": "2025-10-11",
    "Location": "Roseburg, OR",
    "Host": "Canine Discovery Corps",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 43.2461,
    "Longitude": -123.3327
  },
  {
    "Date": "2025-10-11",
    "Location": "Sedona, AZ",
    "Host": "Successful Sniffer",
    "TrialTypes": "ELT-P, ELT, NW3",
    "EventCount": 3,
    "Latitude": 34.8918,
    "Longitude": -111.7188
  },
  {
    "Date": "2025-10-17",
    "Location": "Elizabeth, CO",
    "Host": "Beyond Elevation K9",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 39.344,
    "Longitude": -104.6198
  },
  {
    "Date": "2025-10-17",
    "Location": "Lawrenceville, GA",
    "Host": "Chestnut Hill Canine Sports",
    "TrialTypes": "NW3, L2C, NW1",
    "EventCount": 3,
    "Latitude": 33.9958,
    "Longitude": -83.9501
  },
  {
    "Date": "2025-10-17",
    "Location": "Newfoundland, PA",
    "Host": "Your Dog's Place, LLC",
    "TrialTypes": "L1V, ELT-S, L2C, L2E",
    "EventCount": 4,
    "Latitude": 41.3115,
    "Longitude": -75.2715
  },
  {
    "Date": "2025-10-18",
    "Location": "Centralia, WA",
    "Host": "Rachelle Bailey-Austin/About Face K9 Academy & Dorothy Turley/Let's Talk Dogs, LLC",
    "TrialTypes": "L3I, L2C, NW2",
    "EventCount": 3,
    "Latitude": 46.6942,
    "Longitude": -123.0017
  },
  {
    "Date": "2025-10-18",
    "Location": "Delevan, NY",
    "Host": "Do Over Dog Training",
    "TrialTypes": "ELT, ELT-S, NW2",
    "EventCount": 3,
    "Latitude": 42.493,
    "Longitude": -78.5253
  },
  {
    "Date": "2025-10-18",
    "Location": "Lafayette Hill, PA",
    "Host": "Sniff Sniff Hooray",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 40.063,
    "Longitude": -75.2631
  },
  {
    "Date": "2025-10-18",
    "Location": "Milton, NH",
    "Host": "Lucky Dog Events",
    "TrialTypes": "NW2, NW3",
    "EventCount": 2,
    "Latitude": 43.3688,
    "Longitude": -70.9722
  },
  {
    "Date": "2025-10-18",
    "Location": "Nevada City, CA",
    "Host": "Sierra Sniffing Canines",
    "TrialTypes": "L1I, L2I, ELT",
    "EventCount": 3,
    "Latitude": 39.2896,
    "Longitude": -121.0376
  },
  {
    "Date": "2025-10-18",
    "Location": "Terryville, CT",
    "Host": "Willoughby Training",
    "TrialTypes": "NW2, NW1",
    "EventCount": 2,
    "Latitude": 41.6485,
    "Longitude": -73.0484
  },
  {
    "Date": "2025-10-18",
    "Location": "Troy, VA",
    "Host": "Your Dog Knows, LLC",
    "TrialTypes": "ELT, L1V, L2V",
    "EventCount": 3,
    "Latitude": 37.9197,
    "Longitude": -78.2216
  },
  {
    "Date": "2025-10-18",
    "Location": "Watsonville, CA",
    "Host": "CalCoastal Dog Owners Group",
    "TrialTypes": "L3V, L2V, L1V",
    "EventCount": 3,
    "Latitude": 36.8992,
    "Longitude": -121.7162
  },
  {
    "Date": "2025-10-19",
    "Location": "San Martin, CA",
    "Host": "B.L. McMutts",
    "TrialTypes": "L1V, L3V",
    "EventCount": 2,
    "Latitude": 37.0857,
    "Longitude": -121.5726
  },
  {
    "Date": "2025-10-24",
    "Location": "Calhan, CO",
    "Host": "Mountain Dogs, LLC",
    "TrialTypes": "ELT, SMT",
    "EventCount": 2,
    "Latitude": 38.994,
    "Longitude": -104.273
  },
  {
    "Date": "2025-10-24",
    "Location": "Easton, MD",
    "Host": "Fair Play Point Labradors",
    "TrialTypes": "ELT, ELT-S, L2V, L2C, L3C",
    "EventCount": 5,
    "Latitude": 38.7481,
    "Longitude": -76.1063
  },
  {
    "Date": "2025-10-24",
    "Location": "Palmyra, VA",
    "Host": "Paws Plus Training, LLC",
    "TrialTypes": "NW3, NW1, NW2, ELT-P",
    "EventCount": 4,
    "Latitude": 37.8297,
    "Longitude": -78.2942
  },
  {
    "Date": "2025-10-24",
    "Location": "Ypsilanti, MI",
    "Host": "Every Dog Nosework",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 42.2518,
    "Longitude": -83.6318
  },
  {
    "Date": "2025-10-25",
    "Location": "Auburn, WA",
    "Host": "Northwest K9 Sniffers",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 47.2788,
    "Longitude": -122.258
  },
  {
    "Date": "2025-10-25",
    "Location": "Fishkill, NY",
    "Host": "Top Notch Dogs, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 41.5218,
    "Longitude": -73.8816
  },
  {
    "Date": "2025-10-25",
    "Location": "Lyle, WA",
    "Host": "Trust Your Dog K9 Events",
    "TrialTypes": "NW3, L3I, L3C",
    "EventCount": 3,
    "Latitude": 45.6641,
    "Longitude": -121.2357
  },
  {
    "Date": "2025-10-25",
    "Location": "Niantic, IL",
    "Host": "Kudos for Canines, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 39.8692,
    "Longitude": -89.1564
  },
  {
    "Date": "2025-10-25",
    "Location": "Poland Springs, ME",
    "Host": "Virginia Howe",
    "TrialTypes": "NW1, NW3",
    "EventCount": 2,
    "Latitude": 43.9856,
    "Longitude": -70.3147
  },
  {
    "Date": "2025-10-25",
    "Location": "West Friendship, MD",
    "Host": "Red Huskies",
    "TrialTypes": "NW3, NW1, L1C",
    "EventCount": 3,
    "Latitude": 39.3139,
    "Longitude": -76.9909
  },
  {
    "Date": "2025-10-27",
    "Location": "Clayton, GA",
    "Host": "Right Choice Dog Training LLC",
    "TrialTypes": "NW3, ELT, ELT-P",
    "EventCount": 3,
    "Latitude": 33.5686,
    "Longitude": -84.3781
  },
  {
    "Date": "2025-10-27",
    "Location": "Paicines, CA",
    "Host": "Two Nosey Girls",
    "TrialTypes": "NW2, ELT-P",
    "EventCount": 2,
    "Latitude": 36.6839,
    "Longitude": -121.2397
  },
  {
    "Date": "2025-10-31",
    "Location": "Cannon Falls, MN",
    "Host": "St Paul Dog Training Club",
    "TrialTypes": "SMT, NW3",
    "EventCount": 2,
    "Latitude": 44.5378,
    "Longitude": -92.9561
  },
  {
    "Date": "2025-10-31",
    "Location": "Loranger, LA",
    "Host": "Dog Gone Right",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 30.5888,
    "Longitude": -90.4187
  },
  {
    "Date": "2025-10-31",
    "Location": "Meeker, CO",
    "Host": "Mountain Dogs LLC",
    "TrialTypes": "ELT, NW3, NW1, NW2",
    "EventCount": 4,
    "Latitude": 40.0153,
    "Longitude": -107.9257
  },
  {
    "Date": "2025-10-31",
    "Location": "Scotts Mills, OR",
    "Host": "Canine Discovery Corps",
    "TrialTypes": "ELT, NW1, L3C",
    "EventCount": 3,
    "Latitude": 45.0647,
    "Longitude": -122.7158
  },
  {
    "Date": "2025-11-01",
    "Location": "Beloit, WI",
    "Host": "George Carpenter",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 42.4642,
    "Longitude": -89.0777
  },
  {
    "Date": "2025-11-01",
    "Location": "Charlton, MA",
    "Host": "HeavenScent Sniffers",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 42.1808,
    "Longitude": -71.9465
  },
  {
    "Date": "2025-11-01",
    "Location": "Kennebunkport, ME",
    "Host": "Elizabeth Dutton",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 43.4032,
    "Longitude": -70.4653
  },
  {
    "Date": "2025-11-01",
    "Location": "Mill Spring, NC",
    "Host": "Foothills Canine Academy, LLC",
    "TrialTypes": "NW1, NW2, ELT",
    "EventCount": 3,
    "Latitude": 35.2494,
    "Longitude": -82.181
  },
  {
    "Date": "2025-11-01",
    "Location": "Monkton, MD",
    "Host": "Firezone GS",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 39.5962,
    "Longitude": -76.6194
  },
  {
    "Date": "2025-11-01",
    "Location": "Wappingers Falls, NY",
    "Host": "Top Notch Dogs, LLC",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 41.6156,
    "Longitude": -73.9396
  },
  {
    "Date": "2025-11-01",
    "Location": "Woodstock, GA",
    "Host": "Georgia Nosework LLC",
    "TrialTypes": "L1C, L3I, L3C, L1I",
    "EventCount": 4,
    "Latitude": 34.0571,
    "Longitude": -84.5674
  },
  {
    "Date": "2025-11-01",
    "Location": "Yamhill, OR",
    "Host": "Nose Work Detectives",
    "TrialTypes": "ELT-S, L1C",
    "EventCount": 2,
    "Latitude": 45.2281,
    "Longitude": -123.2132
  },
  {
    "Date": "2025-11-05",
    "Location": "Ventura, CA",
    "Host": "Pink Biscuit K9s",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 34.4241,
    "Longitude": -119.0922
  },
  {
    "Date": "2025-11-07",
    "Location": "Montrose, CO",
    "Host": "Mountain Dogs LLC",
    "TrialTypes": "ELT-S, NW2, ELT",
    "EventCount": 3,
    "Latitude": 38.4507,
    "Longitude": -107.924
  },
  {
    "Date": "2025-11-07",
    "Location": "Rancho Cucamonga , CA",
    "Host": "Agile Paws Dog Sports",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 34.0559,
    "Longitude": -117.6157
  },
  {
    "Date": "2025-11-08",
    "Location": "Coburg, OR",
    "Host": "Kiddy Christie",
    "TrialTypes": "NW1, NW2, ELT-S, L1E",
    "EventCount": 4,
    "Latitude": 44.1568,
    "Longitude": -123.0991
  },
  {
    "Date": "2025-11-08",
    "Location": "Elkhorn, WI",
    "Host": "Loving Paws Dog Training LLC",
    "TrialTypes": "ELT, NW2",
    "EventCount": 2,
    "Latitude": 42.6589,
    "Longitude": -88.5437
  },
  {
    "Date": "2025-11-08",
    "Location": "Guerneville, CA",
    "Host": "Jen Huot",
    "TrialTypes": "NW3, ELT, ELT-P",
    "EventCount": 3,
    "Latitude": 38.4912,
    "Longitude": -122.9776
  },
  {
    "Date": "2025-11-08",
    "Location": "Mays Landing, NJ",
    "Host": "Rotts-n-Notts Nosework, LLC",
    "TrialTypes": "NW3, L2E, NW2",
    "EventCount": 3,
    "Latitude": 39.4783,
    "Longitude": -74.7595
  },
  {
    "Date": "2025-11-08",
    "Location": "Pine Grove, PA",
    "Host": "Paws n' Sniff",
    "TrialTypes": "ELT-P, NW3",
    "EventCount": 2,
    "Latitude": 40.5443,
    "Longitude": -76.4116
  },
  {
    "Date": "2025-11-08",
    "Location": "Tucson, AZ",
    "Host": "Patience Unlimited Professional Dog Training",
    "TrialTypes": "NW3, L2C, NW2",
    "EventCount": 3,
    "Latitude": 32.1825,
    "Longitude": -111.007
  },
  {
    "Date": "2025-11-11",
    "Location": "Guerneville, CA",
    "Host": "Jen Huot",
    "TrialTypes": "ELT-P",
    "EventCount": 1,
    "Latitude": 38.4726,
    "Longitude": -123.0185
  },
  {
    "Date": "2025-11-12",
    "Location": "Astoria, OR",
    "Host": "Nose Work Detectives, LLC",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 46.1445,
    "Longitude": -123.8203
  },
  {
    "Date": "2025-11-14",
    "Location": "Elgin, IL",
    "Host": "Common Scents K9",
    "TrialTypes": "L1C, L2C, L1I, L2I, NW1",
    "EventCount": 5,
    "Latitude": 42.0445,
    "Longitude": -88.2774
  },
  {
    "Date": "2025-11-14",
    "Location": "New Freedom, PA",
    "Host": "Firezone GS",
    "TrialTypes": "ELT, NW3, ELT-S, NW2",
    "EventCount": 4,
    "Latitude": 39.7092,
    "Longitude": -76.7344
  },
  {
    "Date": "2025-11-15",
    "Location": "Albuquerque, NM",
    "Host": "New Mexico Canine Scent Work, LLC",
    "TrialTypes": "NW3, L1C, NW2",
    "EventCount": 3,
    "Latitude": 35.1219,
    "Longitude": -106.6946
  },
  {
    "Date": "2025-11-15",
    "Location": "Bonham, TX",
    "Host": "All About The Nose",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 33.559,
    "Longitude": -96.1369
  },
  {
    "Date": "2025-11-15",
    "Location": "Bradenton, FL",
    "Host": "Hoppin' in the Hills",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 27.4996,
    "Longitude": -82.6071
  },
  {
    "Date": "2025-11-15",
    "Location": "Eldred, NY",
    "Host": "Your Dog's Place, LLC",
    "TrialTypes": "L1I, NW2, L3I, L3C",
    "EventCount": 4,
    "Latitude": 41.5356,
    "Longitude": -74.867
  },
  {
    "Date": "2025-11-15",
    "Location": "Marbury, AL",
    "Host": "Kaye Stevenson",
    "TrialTypes": "NW2, NW1, L1E",
    "EventCount": 3,
    "Latitude": 32.7149,
    "Longitude": -86.4347
  },
  {
    "Date": "2025-11-15",
    "Location": "Montgomery, AL",
    "Host": "By A Nose Nosework",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 32.3358,
    "Longitude": -86.3253
  },
  {
    "Date": "2025-11-15",
    "Location": "Welches, OR",
    "Host": "Trust Your Dog K9 Events",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 45.3792,
    "Longitude": -122.0005
  },
  {
    "Date": "2025-11-17",
    "Location": "Hudson, MA",
    "Host": "For the Love of Dogs NY LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 42.4337,
    "Longitude": -71.557
  },
  {
    "Date": "2025-11-21",
    "Location": "Harrington, DE",
    "Host": "Shamrock Pot Of Gold K9 Scenter",
    "TrialTypes": "NW3, ELT-S, NW2, NW1, L1C, ELT",
    "EventCount": 6,
    "Latitude": 38.9223,
    "Longitude": -75.5654
  },
  {
    "Date": "2025-11-21",
    "Location": "San Luis Obispo, CA",
    "Host": "Gentle Touch Pet Training",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 35.318,
    "Longitude": -120.3597
  },
  {
    "Date": "2025-11-22",
    "Location": "Crownsville, MD",
    "Host": "Red Huskies",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 39.0425,
    "Longitude": -76.5654
  },
  {
    "Date": "2025-11-22",
    "Location": "Defuniak Springs, FL",
    "Host": "Linda Culliton",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 30.755,
    "Longitude": -86.1216
  },
  {
    "Date": "2025-11-22",
    "Location": "Delta, CO",
    "Host": "Mountain Dogs, LLC",
    "TrialTypes": "ELT, NW3, ELT-S, NW2",
    "EventCount": 4,
    "Latitude": 38.7884,
    "Longitude": -107.878
  },
  {
    "Date": "2025-11-22",
    "Location": "Foxborough , MA",
    "Host": "MasterPeace Dog Training",
    "TrialTypes": "L3C, NW1, NW2",
    "EventCount": 3,
    "Latitude": 42.101,
    "Longitude": -71.2106
  },
  {
    "Date": "2025-11-22",
    "Location": "Kintnersville, PA",
    "Host": "Paws n' Sniff",
    "TrialTypes": "L1C, NW1, L2I, L2E",
    "EventCount": 4,
    "Latitude": 40.5993,
    "Longitude": -75.2062
  },
  {
    "Date": "2025-11-22",
    "Location": "Marble Falls, TX",
    "Host": "Scent Work Across Texas",
    "TrialTypes": "ELT-S, NW2, NW1, L1I",
    "EventCount": 4,
    "Latitude": 30.5938,
    "Longitude": -98.3081
  },
  {
    "Date": "2025-11-22",
    "Location": "Medford, NJ",
    "Host": "K9 InScentives",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 39.9273,
    "Longitude": -74.8652
  },
  {
    "Date": "2025-11-22",
    "Location": "Norton, MA",
    "Host": "Dogs Make Scents",
    "TrialTypes": "ELT, L1E, L1C",
    "EventCount": 3,
    "Latitude": 42.0162,
    "Longitude": -71.1429
  },
  {
    "Date": "2025-11-22",
    "Location": "Salem Lakes, WI",
    "Host": "Loving Paws Dog Training, LLC",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 42.5702,
    "Longitude": -88.1295
  },
  {
    "Date": "2025-11-22",
    "Location": "Smyrna, TN",
    "Host": "Dogs Have Amazing Noses, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 35.9358,
    "Longitude": -86.5221
  },
  {
    "Date": "2025-11-28",
    "Location": "Dana Point, CA",
    "Host": "JavaK9s, LLC",
    "TrialTypes": "NW3, ELT-S",
    "EventCount": 2,
    "Latitude": 33.4246,
    "Longitude": -117.6932
  },
  {
    "Date": "2025-11-28",
    "Location": "San Jose, CA",
    "Host": "The Bay Team",
    "TrialTypes": "NW2, NW1",
    "EventCount": 2,
    "Latitude": 37.3837,
    "Longitude": -121.8998
  },
  {
    "Date": "2025-11-29",
    "Location": "Alpharetta, GA",
    "Host": "Georgia Nosework",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 34.1077,
    "Longitude": -84.3185
  },
  {
    "Date": "2025-11-29",
    "Location": "Canandaigua, NY",
    "Host": "Savvy Dog Sports",
    "TrialTypes": "ELT, NW2",
    "EventCount": 2,
    "Latitude": 42.8894,
    "Longitude": -77.2983
  },
  {
    "Date": "2025-11-29",
    "Location": "Cottage Grove, MN",
    "Host": "Gretchen Hofheins-Wackerfuss",
    "TrialTypes": "ELT-S, ELT-P",
    "EventCount": 2,
    "Latitude": 44.789,
    "Longitude": -92.9022
  },
  {
    "Date": "2025-11-29",
    "Location": "Lebanon, NJ",
    "Host": "Sirius K9 Solutions",
    "TrialTypes": "NW3, L3I, ELT-S",
    "EventCount": 3,
    "Latitude": 40.6858,
    "Longitude": -74.7937
  },
  {
    "Date": "2025-12-05",
    "Location": "Bowie, MD",
    "Host": "Red Huskies",
    "TrialTypes": "ELT, ELT-P, ELT-S",
    "EventCount": 3,
    "Latitude": 38.9643,
    "Longitude": -76.7423
  },
  {
    "Date": "2025-12-06",
    "Location": "Annapolis, MD",
    "Host": "Chesapeake Search Dogs",
    "TrialTypes": "NW3, NW2, L2C",
    "EventCount": 3,
    "Latitude": 39.0215,
    "Longitude": -76.504
  },
  {
    "Date": "2025-12-06",
    "Location": "Auburn, WA",
    "Host": "Northwest K9 Sniffers",
    "TrialTypes": "L1C, L2C, NW3",
    "EventCount": 3,
    "Latitude": 47.3434,
    "Longitude": -122.1867
  },
  {
    "Date": "2025-12-06",
    "Location": "Centralia, WA",
    "Host": "About Face K9 Academy and Let's Talk Dogs",
    "TrialTypes": "NW3, NW1",
    "EventCount": 2,
    "Latitude": 46.7023,
    "Longitude": -122.9551
  },
  {
    "Date": "2025-12-06",
    "Location": "Fillmore, CA",
    "Host": "Pink Biscuit K9s",
    "TrialTypes": "NW1, NW2, ELT",
    "EventCount": 3,
    "Latitude": 34.3953,
    "Longitude": -118.9155
  },
  {
    "Date": "2025-12-06",
    "Location": "Hoover, AL",
    "Host": "Southeast Scent Work Alliance, LLC",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 33.3974,
    "Longitude": -86.8649
  },
  {
    "Date": "2025-12-06",
    "Location": "Kittanning, PA",
    "Host": "Steel City Nosework, LLC",
    "TrialTypes": "NW3, ELT-S, NW1",
    "EventCount": 3,
    "Latitude": 40.8368,
    "Longitude": -79.5364
  },
  {
    "Date": "2025-12-06",
    "Location": "Newfoundland, PA",
    "Host": "Your Dog's Place, LLC",
    "TrialTypes": "L2I, L3V, NW3",
    "EventCount": 3,
    "Latitude": 41.2928,
    "Longitude": -75.2765
  },
  {
    "Date": "2025-12-07",
    "Location": "Cape Coral, FL",
    "Host": "Your Dog Knows, LLC",
    "TrialTypes": "NW1",
    "EventCount": 1,
    "Latitude": 26.5878,
    "Longitude": -81.985
  },
  {
    "Date": "2025-12-12",
    "Location": "Douglassville, PA",
    "Host": "Patricia Grassey",
    "TrialTypes": "NW3, ELT, ELT-P",
    "EventCount": 3,
    "Latitude": 40.2113,
    "Longitude": -75.7064
  },
  {
    "Date": "2025-12-12",
    "Location": "Pittstown, NJ",
    "Host": "Shamrock Pot Of Gold K9 Scenter",
    "TrialTypes": "NW3, ELT, ELT-S, NW1",
    "EventCount": 4,
    "Latitude": 40.5471,
    "Longitude": -74.9841
  },
  {
    "Date": "2025-12-13",
    "Location": "DeLeon Springs, FL",
    "Host": "River Poodles Training, LLC",
    "TrialTypes": "ELT-P, ELT-S, L2C",
    "EventCount": 3,
    "Latitude": 29.1432,
    "Longitude": -81.3408
  },
  {
    "Date": "2025-12-13",
    "Location": "Easton, MA",
    "Host": "South Coast Scent Dogs",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 42.0645,
    "Longitude": -71.1783
  },
  {
    "Date": "2025-12-13",
    "Location": "Escondido, CA",
    "Host": "Uber Dog and Rewarding Rover LLC",
    "TrialTypes": "NW2",
    "EventCount": 1,
    "Latitude": 33.1469,
    "Longitude": -117.0391
  },
  {
    "Date": "2025-12-13",
    "Location": "Greer, SC",
    "Host": "Trained to Trust LLC",
    "TrialTypes": "ELT, ELT-S, NW2",
    "EventCount": 3,
    "Latitude": 34.9111,
    "Longitude": -82.2097
  },
  {
    "Date": "2025-12-13",
    "Location": "Independence, OR",
    "Host": "Doglandia, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 44.8959,
    "Longitude": -123.1772
  },
  {
    "Date": "2025-12-13",
    "Location": "Westminster, MD",
    "Host": "Red Huskies",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 39.5707,
    "Longitude": -76.9589
  },
  {
    "Date": "2025-12-16",
    "Location": "Duluth, GA",
    "Host": "Right Choice Dog Training LLC",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 33.9685,
    "Longitude": -84.1362
  },
  {
    "Date": "2025-12-20",
    "Location": "Cumming, GA",
    "Host": "Georgia Nosework LLC",
    "TrialTypes": "SMT, ELT",
    "EventCount": 2,
    "Latitude": 34.1971,
    "Longitude": -84.1302
  },
  {
    "Date": "2025-12-20",
    "Location": "Florissant, MO",
    "Host": "Happy Dog Concepts, LLC",
    "TrialTypes": "ELT-P",
    "EventCount": 1,
    "Latitude": 38.8277,
    "Longitude": -90.2799
  },
  {
    "Date": "2025-12-20",
    "Location": "Salem, OR",
    "Host": "Helix Fairweather & Doglandia, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 44.988,
    "Longitude": -123.024
  },
  {
    "Date": "2025-12-20",
    "Location": "Silex, MO",
    "Host": "WestInn Kennels",
    "TrialTypes": "NW1, NW2, NW3",
    "EventCount": 3,
    "Latitude": 39.1166,
    "Longitude": -91.0509
  },
  {
    "Date": "2025-12-20",
    "Location": "Stockton, CA",
    "Host": "Two Nosey Girls",
    "TrialTypes": "NW3, NW1",
    "EventCount": 2,
    "Latitude": 37.9268,
    "Longitude": -121.3062
  },
  {
    "Date": "2025-12-27",
    "Location": "Auburn, AL",
    "Host": "Daphne Melillo",
    "TrialTypes": "NW2, NW1",
    "EventCount": 2,
    "Latitude": 32.5853,
    "Longitude": -85.4564
  },
  {
    "Date": "2025-12-27",
    "Location": "Fort Morgan, CO",
    "Host": "Paws 4 Thought Dog Training, LLC",
    "TrialTypes": "L1V, NW2, NW1, L1I",
    "EventCount": 4,
    "Latitude": 40.202,
    "Longitude": -103.8364
  },
  {
    "Date": "2025-12-27",
    "Location": "New Rochelle, NY",
    "Host": "For the Love of Dogs NY LLC",
    "TrialTypes": "NW3, ELT-S, ELT",
    "EventCount": 3,
    "Latitude": 40.9449,
    "Longitude": -73.7406
  },
  {
    "Date": "2025-12-27",
    "Location": "White Plains, NY",
    "Host": "Saints2Source",
    "TrialTypes": "NW1, NW2, L2E, L2C",
    "EventCount": 4,
    "Latitude": 41.057,
    "Longitude": -73.7368
  },
  {
    "Date": "2025-12-28",
    "Location": "Exton, PA",
    "Host": "Patricia Grassey",
    "TrialTypes": "ELT, ELT-P, NW3",
    "EventCount": 3,
    "Latitude": 40.0303,
    "Longitude": -75.6629
  },
  {
    "Date": "2025-12-28",
    "Location": "Waukesha, WI",
    "Host": "Think Pawsitive Dog Training",
    "TrialTypes": "NW2, NW1",
    "EventCount": 2,
    "Latitude": 43.0989,
    "Longitude": -88.2725
  },
  {
    "Date": "2025-12-29",
    "Location": "Barrington, RI",
    "Host": "Bay State Sniffers",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 41.756,
    "Longitude": -71.2884
  },
  {
    "Date": "2026-01-02",
    "Location": "Emmitsburg , MD",
    "Host": "Red Huskies",
    "TrialTypes": "NW3, ELT, ELT-S",
    "EventCount": 3,
    "Latitude": 39.735,
    "Longitude": -77.3646
  },
  {
    "Date": "2026-01-03",
    "Location": "Bonsall, CA",
    "Host": "Linda Buchanan",
    "TrialTypes": "NW3, ELT-P",
    "EventCount": 2,
    "Latitude": 33.2663,
    "Longitude": -117.2412
  },
  {
    "Date": "2026-01-03",
    "Location": "Green Cove Springs, FL",
    "Host": "Hoppin' in the Hills",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 30.0062,
    "Longitude": -81.7269
  },
  {
    "Date": "2026-01-03",
    "Location": "Maryville, TN",
    "Host": "Rachel Hawkins",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 35.7249,
    "Longitude": -83.9318
  },
  {
    "Date": "2026-01-03",
    "Location": "Montevallo, AL",
    "Host": "Southeast Scent Work Alliance, LLC",
    "TrialTypes": "NW1",
    "EventCount": 1,
    "Latitude": 33.1028,
    "Longitude": -86.8713
  },
  {
    "Date": "2026-01-09",
    "Location": "Hartfield, VA",
    "Host": "Paws Plus Training, LLC",
    "TrialTypes": "NW3, NW1, NW2, ELT",
    "EventCount": 4,
    "Latitude": 37.5198,
    "Longitude": -76.4566
  },
  {
    "Date": "2026-01-09",
    "Location": "Spring City, PA",
    "Host": "Patricia Grassey",
    "TrialTypes": "NW3, NW2, NW1, L2I",
    "EventCount": 4,
    "Latitude": 40.1284,
    "Longitude": -75.587
  },
  {
    "Date": "2026-01-10",
    "Location": "Canton , GA",
    "Host": "Run Spot Jump Dog Training",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 34.2606,
    "Longitude": -84.4683
  },
  {
    "Date": "2026-01-10",
    "Location": "Pflugerville, TX",
    "Host": "Scent Work Across Texas",
    "TrialTypes": "ELT-S, L2C, NW3",
    "EventCount": 3,
    "Latitude": 30.4554,
    "Longitude": -97.6603
  },
  {
    "Date": "2026-01-10",
    "Location": "Valencia, CA",
    "Host": "Pink Biscuit K9s",
    "TrialTypes": "NW1, NW2, ELT",
    "EventCount": 3,
    "Latitude": 34.4548,
    "Longitude": -118.5412
  },
  {
    "Date": "2026-01-16",
    "Location": "Bristol, PA",
    "Host": "Patricia Grassey",
    "TrialTypes": "ELT, ELT-P, ELT-S, NW2",
    "EventCount": 4,
    "Latitude": 40.0821,
    "Longitude": -74.8173
  },
  {
    "Date": "2026-01-17",
    "Location": "Clanton, AL",
    "Host": "By A Nose Nosework",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 32.8852,
    "Longitude": -86.617
  },
  {
    "Date": "2026-01-17",
    "Location": "Melrose, FL",
    "Host": "River Poodles Training, LLC",
    "TrialTypes": "NW1, NW2, L1V, L1E",
    "EventCount": 4,
    "Latitude": 29.7099,
    "Longitude": -82.0947
  },
  {
    "Date": "2026-01-17",
    "Location": "New Rochelle, NY",
    "Host": "For the Love of Dogs NY LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 40.9256,
    "Longitude": -73.8186
  },
  {
    "Date": "2026-01-17",
    "Location": "San Marcos, CA",
    "Host": "Rewarding Rover LLC and Uberdog",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 33.1299,
    "Longitude": -117.188
  },
  {
    "Date": "2026-01-17",
    "Location": "Tecumseh, OK",
    "Host": "The Doggie Spot, LLC",
    "TrialTypes": "ELT, NW3, NW2",
    "EventCount": 3,
    "Latitude": 35.2411,
    "Longitude": -96.9553
  },
  {
    "Date": "2026-01-19",
    "Location": "Redlands, CA",
    "Host": "Agile Paws Dog Sports",
    "TrialTypes": "NW3, ELT-P",
    "EventCount": 2,
    "Latitude": 34.0472,
    "Longitude": -117.2209
  },
  {
    "Date": "2026-01-20",
    "Location": "Murfreesboro, TN",
    "Host": "Dogs Have Amazing Noses, LLC",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 35.8182,
    "Longitude": -86.4358
  },
  {
    "Date": "2026-01-23",
    "Location": "Rome, GA",
    "Host": "Georgia Nosework, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 34.2775,
    "Longitude": -85.2053
  },
  {
    "Date": "2026-01-30",
    "Location": "Greeley, CO",
    "Host": "Beyond Elevation K9 Training",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 40.3926,
    "Longitude": -104.6766
  },
  {
    "Date": "2026-01-31",
    "Location": "Denton, MD",
    "Host": "Fair Play Labradors",
    "TrialTypes": "ELT-S, L2C, NW1, L1E",
    "EventCount": 4,
    "Latitude": 38.903,
    "Longitude": -75.8378
  },
  {
    "Date": "2026-01-31",
    "Location": "Tucson, AZ",
    "Host": "Patience Unlimited Professional Dog Training",
    "TrialTypes": "NW3, ELT-S, L1V",
    "EventCount": 3,
    "Latitude": 32.1987,
    "Longitude": -111.0179
  },
  {
    "Date": "2026-02-07",
    "Location": "Lakewood, NJ",
    "Host": "Rotts n Notts Nosework",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 40.1435,
    "Longitude": -74.2598
  },
  {
    "Date": "2026-02-07",
    "Location": "Murfreesboro, TN",
    "Host": "Dogs Have Amazing Noses, LLC",
    "TrialTypes": "ELT, NW1",
    "EventCount": 2,
    "Latitude": 35.8083,
    "Longitude": -86.388
  },
  {
    "Date": "2026-02-13",
    "Location": "Havre De Grace, MD",
    "Host": "Firezone GS",
    "TrialTypes": "ELT-P, NW3, L3I, NW2",
    "EventCount": 4,
    "Latitude": 39.544,
    "Longitude": -76.1085
  },
  {
    "Date": "2026-02-13",
    "Location": "Vista, CA",
    "Host": "Rewarding Rover LLC and Uberdog",
    "TrialTypes": "ELT, L1C, L2C",
    "EventCount": 3,
    "Latitude": 33.212,
    "Longitude": -117.212
  },
  {
    "Date": "2026-02-14",
    "Location": "Clarkesville, GA",
    "Host": "Right Choice Dog Training, LLC",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 34.6076,
    "Longitude": -83.5527
  },
  {
    "Date": "2026-02-14",
    "Location": "Colesville, MD",
    "Host": "Red Huskies",
    "TrialTypes": "L1C, NW1, ELT-S, ELT",
    "EventCount": 4,
    "Latitude": 39.0398,
    "Longitude": -77.0012
  },
  {
    "Date": "2026-02-14",
    "Location": "Flemington, NJ",
    "Host": "Shamrock Pot Of Gold K9 Scenter",
    "TrialTypes": "ELT, ELT-S, NW1",
    "EventCount": 3,
    "Latitude": 40.5304,
    "Longitude": -74.865
  },
  {
    "Date": "2026-02-14",
    "Location": "Northridge, CA",
    "Host": "SCENTwork.org",
    "TrialTypes": "NW2, ELT",
    "EventCount": 2,
    "Latitude": 34.2577,
    "Longitude": -118.5603
  },
  {
    "Date": "2026-02-14",
    "Location": "Pottsboro, TX",
    "Host": "All About The Nose",
    "TrialTypes": "NW3, NW2, NW1",
    "EventCount": 3,
    "Latitude": 33.7127,
    "Longitude": -96.6236
  },
  {
    "Date": "2026-02-14",
    "Location": "Strafford, PA",
    "Host": "The Sniffing Hound",
    "TrialTypes": "NW3, ELT, ELT-P",
    "EventCount": 3,
    "Latitude": 40.047,
    "Longitude": -75.3729
  },
  {
    "Date": "2026-02-15",
    "Location": "Chino, CA",
    "Host": "Agile Paws Dog Sports",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 34.0188,
    "Longitude": -117.713
  },
  {
    "Date": "2026-02-15",
    "Location": "New Rochelle, NY",
    "Host": "For the Love of Dogs NY LLC",
    "TrialTypes": "ELT-P, NW3, ELT",
    "EventCount": 3,
    "Latitude": 40.9189,
    "Longitude": -73.7432
  },
  {
    "Date": "2026-02-20",
    "Location": "San Rafael/Novato, CA",
    "Host": "Marin Humane",
    "TrialTypes": "L2C, L1I, NW3",
    "EventCount": 3,
    "Latitude": 38.0682,
    "Longitude": -122.3376
  },
  {
    "Date": "2026-02-21",
    "Location": "Clearwater, FL",
    "Host": "Hoppin' in the Hills",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 27.9927,
    "Longitude": -82.7991
  },
  {
    "Date": "2026-02-21",
    "Location": "Veneta, OR",
    "Host": "Wells Creek Dog Training",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 44.0052,
    "Longitude": -123.3936
  },
  {
    "Date": "2026-02-22",
    "Location": "Benson, AZ",
    "Host": "Patience Unlimited Professional Dog Training",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 31.967,
    "Longitude": -110.2667
  },
  {
    "Date": "2026-02-24",
    "Location": "Paso Robles, CA",
    "Host": "Gentle Touch Pet Training",
    "TrialTypes": "ELT-S, L3I",
    "EventCount": 2,
    "Latitude": 35.5962,
    "Longitude": -120.6658
  },
  {
    "Date": "2026-02-28",
    "Location": "Danielsville, GA",
    "Host": "Right Choice Dog Training, LLC",
    "TrialTypes": "NW1, L2I, NW2",
    "EventCount": 3,
    "Latitude": 34.0891,
    "Longitude": -83.2271
  },
  {
    "Date": "2026-02-28",
    "Location": "Keystone Heights, FL",
    "Host": "River Poodles Training, LLC",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 29.8157,
    "Longitude": -81.997
  },
  {
    "Date": "2026-02-28",
    "Location": "Lutherville, MD",
    "Host": "Red Huskies",
    "TrialTypes": "L3C, L1I, NW2",
    "EventCount": 3,
    "Latitude": 39.4205,
    "Longitude": -76.6363
  },
  {
    "Date": "2026-02-28",
    "Location": "Tygh Valley, OR",
    "Host": "Nose Work Detectives, LLC",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 45.2209,
    "Longitude": -121.1878
  },
  {
    "Date": "2026-02-28",
    "Location": "Wilson, NC",
    "Host": "Whole Dog Institute, LLC",
    "TrialTypes": "ELT, ELT-S, NW1",
    "EventCount": 3,
    "Latitude": 35.7389,
    "Longitude": -77.9585
  },
  {
    "Date": "2026-03-06",
    "Location": "Chesterfield, VA",
    "Host": "Paws Plus Training, LLC",
    "TrialTypes": "NW3, NW1, NW2, ELT",
    "EventCount": 4,
    "Latitude": 37.3865,
    "Longitude": -77.5984
  },
  {
    "Date": "2026-03-06",
    "Location": "Elgin, IL",
    "Host": "For Your K9, Inc",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 42.0391,
    "Longitude": -88.2786
  },
  {
    "Date": "2026-03-06",
    "Location": "Westlake Village, CA",
    "Host": "JavaK9s, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 34.1888,
    "Longitude": -118.7566
  },
  {
    "Date": "2026-03-07",
    "Location": "Cumming, GA",
    "Host": "Georgia Nosework LLC",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 34.2485,
    "Longitude": -84.1206
  },
  {
    "Date": "2026-03-07",
    "Location": "Honesdale, PA",
    "Host": "Your Dog's Place, LLC",
    "TrialTypes": "L3C, ELT, ELT-S, L3I",
    "EventCount": 4,
    "Latitude": 41.6153,
    "Longitude": -75.2487
  },
  {
    "Date": "2026-03-07",
    "Location": "Moriarty, NM",
    "Host": "New Mexico Canine Scent Work, LLC",
    "TrialTypes": "NW3, L1I, NW1",
    "EventCount": 3,
    "Latitude": 34.9603,
    "Longitude": -106.0446
  },
  {
    "Date": "2026-03-07",
    "Location": "Warrensburg, IL",
    "Host": "Kudos for Canines",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 39.957,
    "Longitude": -89.0547
  },
  {
    "Date": "2026-03-13",
    "Location": "Centreville , MD",
    "Host": "Fair Play Point Labradors",
    "TrialTypes": "SMT, L2C, L3C",
    "EventCount": 3,
    "Latitude": 39.039,
    "Longitude": -76.1086
  },
  {
    "Date": "2026-03-13",
    "Location": "Colebrook, CT",
    "Host": "For the Love of Dogs NY LLC",
    "TrialTypes": "NW3, SMT",
    "EventCount": 2,
    "Latitude": 41.9801,
    "Longitude": -73.1102
  },
  {
    "Date": "2026-03-13",
    "Location": "Stokesdale, NC",
    "Host": "K9 Nose Adventures, LLC",
    "TrialTypes": "ELT, NW3, NW1, NW2",
    "EventCount": 4,
    "Latitude": 36.2031,
    "Longitude": -79.9847
  },
  {
    "Date": "2026-03-14",
    "Location": "Channahon, IL",
    "Host": "4G & TB",
    "TrialTypes": "NW2, NW1",
    "EventCount": 2,
    "Latitude": 41.4283,
    "Longitude": -88.257
  },
  {
    "Date": "2026-03-14",
    "Location": "Hammond, LA",
    "Host": "Dog Gone Right",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 30.4724,
    "Longitude": -90.496
  },
  {
    "Date": "2026-03-14",
    "Location": "Kent, WA",
    "Host": "K9 Sniffers",
    "TrialTypes": "ELT, L1V, L1E",
    "EventCount": 3,
    "Latitude": 47.4194,
    "Longitude": -122.213
  },
  {
    "Date": "2026-03-14",
    "Location": "Phoenix, AZ",
    "Host": "Release Canine, LLC",
    "TrialTypes": "ELT, NW1",
    "EventCount": 2,
    "Latitude": 33.4469,
    "Longitude": -112.0797
  },
  {
    "Date": "2026-03-14",
    "Location": "Santa Paula, CA",
    "Host": "Pink Biscuit K9s",
    "TrialTypes": "NW3, NW1, ELT-S",
    "EventCount": 3,
    "Latitude": 34.3563,
    "Longitude": -119.047
  },
  {
    "Date": "2026-03-16",
    "Location": "Paso Robles, CA",
    "Host": "Central Coast Nosework Club",
    "TrialTypes": "NW3, ELT-S, L3V",
    "EventCount": 3,
    "Latitude": 35.628,
    "Longitude": -120.6424
  },
  {
    "Date": "2026-03-20",
    "Location": "Shady Hills, FL",
    "Host": "Hoppin' in the Hills",
    "TrialTypes": "L1C, NW1, NW2",
    "EventCount": 3,
    "Latitude": 28.3829,
    "Longitude": -82.579
  },
  {
    "Date": "2026-03-20",
    "Location": "Street, MD",
    "Host": "Firezone GS",
    "TrialTypes": "ELT-P, NW3, ELT-S, NW2",
    "EventCount": 4,
    "Latitude": 39.6627,
    "Longitude": -76.3917
  },
  {
    "Date": "2026-03-21",
    "Location": "Califon, NJ",
    "Host": "Shamrock Pot Of Gold K9 Scenter",
    "TrialTypes": "NW2, ELT-S, ELT",
    "EventCount": 3,
    "Latitude": 40.7364,
    "Longitude": -74.8701
  },
  {
    "Date": "2026-03-21",
    "Location": "Dittmer, MO",
    "Host": "Happy Dog Concepts LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 38.3401,
    "Longitude": -90.6957
  },
  {
    "Date": "2026-03-21",
    "Location": "East Windsor, CT",
    "Host": "Lucky Dog Events",
    "TrialTypes": "ELT, L2C, NW2",
    "EventCount": 3,
    "Latitude": 41.9581,
    "Longitude": -72.5925
  },
  {
    "Date": "2026-03-21",
    "Location": "Elkridge, MD",
    "Host": "Red Huskies",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 39.1788,
    "Longitude": -76.7713
  },
  {
    "Date": "2026-03-21",
    "Location": "Foxboro, MA",
    "Host": "Bay State Sniffers",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 42.0611,
    "Longitude": -71.2536
  },
  {
    "Date": "2026-03-21",
    "Location": "Lawrenceville, GA",
    "Host": "Right Choice Dog Training LLC",
    "TrialTypes": "ELT-P, ELT",
    "EventCount": 2,
    "Latitude": 33.9389,
    "Longitude": -84.0322
  },
  {
    "Date": "2026-03-21",
    "Location": "Oakville, WA",
    "Host": "About Face K9 Academy & Let's Talk Dogs, LLC",
    "TrialTypes": "NW2, ELT",
    "EventCount": 2,
    "Latitude": 46.8603,
    "Longitude": -123.2426
  },
  {
    "Date": "2026-03-21",
    "Location": "Redwood City, CA",
    "Host": "B.L. McMutts LLC",
    "TrialTypes": "ELT-S, L1I, L3C",
    "EventCount": 3,
    "Latitude": 37.4497,
    "Longitude": -122.277
  },
  {
    "Date": "2026-03-21",
    "Location": "Salem Lakes, WI",
    "Host": "Loving Paws Dog Training LLC",
    "TrialTypes": "NW2, L2I, ELT-S",
    "EventCount": 3,
    "Latitude": 42.507,
    "Longitude": -88.1138
  },
  {
    "Date": "2026-03-21",
    "Location": "Winterset, IA",
    "Host": "KBP Dog Training",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 41.315,
    "Longitude": -94.0376
  },
  {
    "Date": "2026-03-23",
    "Location": "Riverside, CA",
    "Host": "Linda Buchanan",
    "TrialTypes": "NW2, NW3",
    "EventCount": 2,
    "Latitude": 33.9665,
    "Longitude": -117.3371
  },
  {
    "Date": "2026-03-27",
    "Location": "Grand Junction, CO",
    "Host": "Mountain Dogs, LLC",
    "TrialTypes": "ELT-P, ELT-S, NW1, NW2",
    "EventCount": 4,
    "Latitude": 39.0906,
    "Longitude": -108.5168
  },
  {
    "Date": "2026-03-27",
    "Location": "Kennett Square, PA",
    "Host": "The Sniffing Hound",
    "TrialTypes": "NW3, ELT, ELT-S, L3I",
    "EventCount": 4,
    "Latitude": 39.8744,
    "Longitude": -75.7019
  },
  {
    "Date": "2026-03-27",
    "Location": "Salem, OR",
    "Host": "Just Nose Work & Doglandia LLC",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 44.9222,
    "Longitude": -123.006
  },
  {
    "Date": "2026-03-27",
    "Location": "Watertown, TN",
    "Host": "Dogs Have Amazing Noses, LLC",
    "TrialTypes": "ELT, NW2",
    "EventCount": 2,
    "Latitude": 36.128,
    "Longitude": -86.0998
  },
  {
    "Date": "2026-03-28",
    "Location": "Batavia, OH",
    "Host": "Clermont County Dog Training Club",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 39.0368,
    "Longitude": -84.1674
  },
  {
    "Date": "2026-03-28",
    "Location": "Colorado Springs, CO",
    "Host": "Beyond Elevation K9",
    "TrialTypes": "NW2, NW3",
    "EventCount": 2,
    "Latitude": 38.7943,
    "Longitude": -104.8218
  },
  {
    "Date": "2026-03-28",
    "Location": "Forks, WA",
    "Host": "Sea Change Canine LLC",
    "TrialTypes": "NW2, NW3",
    "EventCount": 2,
    "Latitude": 47.9189,
    "Longitude": -124.3854
  },
  {
    "Date": "2026-03-29",
    "Location": "Canton, GA",
    "Host": "Run Spot Jump Dog Training",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 34.2199,
    "Longitude": -84.4511
  },
  {
    "Date": "2026-03-30",
    "Location": "Watsonville, CA",
    "Host": "CalCoastal Dog Owners Group",
    "TrialTypes": "NW2, NW1",
    "EventCount": 2,
    "Latitude": 36.8677,
    "Longitude": -121.7633
  },
  {
    "Date": "2026-04-03",
    "Location": "Eagan, MN",
    "Host": "St. Paul Dog Training Center",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 44.8179,
    "Longitude": -93.1487
  },
  {
    "Date": "2026-04-03",
    "Location": "Rochester, NY",
    "Host": "2 Psyched 4 dogs",
    "TrialTypes": "NW3, L1I, L1C",
    "EventCount": 3,
    "Latitude": 43.182,
    "Longitude": -77.6532
  },
  {
    "Date": "2026-04-03",
    "Location": "Warwick, NY",
    "Host": "For the Love of Dogs NY LLC",
    "TrialTypes": "ELT, NW3, NW1",
    "EventCount": 3,
    "Latitude": 41.2652,
    "Longitude": -74.3975
  },
  {
    "Date": "2026-04-04",
    "Location": "Blaine, WA",
    "Host": "The Nosework Magic",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 48.9857,
    "Longitude": -122.7211
  },
  {
    "Date": "2026-04-04",
    "Location": "Burnet, TX",
    "Host": "Scent Work Across Texas",
    "TrialTypes": "L1C, NW2, ELT",
    "EventCount": 3,
    "Latitude": 30.7668,
    "Longitude": -98.185
  },
  {
    "Date": "2026-04-04",
    "Location": "Stayton, OR",
    "Host": "Canine Discovery Corps",
    "TrialTypes": "L2E, L1V, L1E, L2C",
    "EventCount": 4,
    "Latitude": 44.8173,
    "Longitude": -122.8409
  },
  {
    "Date": "2026-04-06",
    "Location": "Sacramento, CA",
    "Host": "Two Nosey Girls",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 38.5474,
    "Longitude": -121.4836
  },
  {
    "Date": "2026-04-08",
    "Location": "Olympia, WA",
    "Host": "Rachelle Bailey-Austin & Dorothy Turley",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 47.078,
    "Longitude": -122.9035
  },
  {
    "Date": "2026-04-10",
    "Location": "Rapid City, SD",
    "Host": "Two Paws Up Dog Training, LLC",
    "TrialTypes": "ELT, NW3, NW1, NW2",
    "EventCount": 4,
    "Latitude": 44.0336,
    "Longitude": -103.2618
  },
  {
    "Date": "2026-04-10",
    "Location": "Somis, CA",
    "Host": "Pink Biscuit K9s",
    "TrialTypes": "NW3, ELT-P, L2C, L2I",
    "EventCount": 4,
    "Latitude": 34.2194,
    "Longitude": -119.0044
  },
  {
    "Date": "2026-04-11",
    "Location": "Bel Air, MD",
    "Host": "Firezone GS",
    "TrialTypes": "ELT-P, ELT-S, NW2",
    "EventCount": 3,
    "Latitude": 39.4911,
    "Longitude": -76.3506
  },
  {
    "Date": "2026-04-11",
    "Location": "Blue Ridge , VA",
    "Host": "Canny K9 Companions, LLC",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 37.4082,
    "Longitude": -79.8439
  },
  {
    "Date": "2026-04-11",
    "Location": "Clinton, WI",
    "Host": "George Carpenter",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 42.5579,
    "Longitude": -88.904
  },
  {
    "Date": "2026-04-11",
    "Location": "Durham, NC",
    "Host": "Dog Fun Forever, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 35.9578,
    "Longitude": -78.9458
  },
  {
    "Date": "2026-04-11",
    "Location": "Genoa, IL",
    "Host": "Common Scents K9",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 42.1127,
    "Longitude": -88.7008
  },
  {
    "Date": "2026-04-11",
    "Location": "Limerick, PA",
    "Host": "Sniff Sniff Hooray",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 40.2668,
    "Longitude": -75.54
  },
  {
    "Date": "2026-04-11",
    "Location": "Rocklin, CA",
    "Host": "Sierra Sniffing Canines",
    "TrialTypes": "NW2, L1E, L2E",
    "EventCount": 3,
    "Latitude": 38.814,
    "Longitude": -121.2271
  },
  {
    "Date": "2026-04-13",
    "Location": "Ellicott City, MD",
    "Host": "Red Huskies",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 39.2926,
    "Longitude": -76.8216
  },
  {
    "Date": "2026-04-17",
    "Location": "Amity, OR",
    "Host": "Doglandia, LLC",
    "TrialTypes": "NW1, NW2, NW3",
    "EventCount": 3,
    "Latitude": 45.1242,
    "Longitude": -123.1583
  },
  {
    "Date": "2026-04-17",
    "Location": "Garrison, NY",
    "Host": "Top Notch Dogs, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 41.4091,
    "Longitude": -73.9471
  },
  {
    "Date": "2026-04-17",
    "Location": "Upland, CA",
    "Host": "Agile Paws Dog Sports",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 34.0579,
    "Longitude": -117.5999
  },
  {
    "Date": "2026-04-18",
    "Location": "Auburn, WA",
    "Host": "Northwest K9 Sniffers",
    "TrialTypes": "L1V, NW1, L1I, NW2",
    "EventCount": 4,
    "Latitude": 47.32,
    "Longitude": -122.214
  },
  {
    "Date": "2026-04-18",
    "Location": "Keystone Heights, FL",
    "Host": "River Poodles Training, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 29.8255,
    "Longitude": -81.9836
  },
  {
    "Date": "2026-04-18",
    "Location": "Laramie, WY",
    "Host": "Mountain Dogs LLC",
    "TrialTypes": "SMT, ELT",
    "EventCount": 2,
    "Latitude": 41.3592,
    "Longitude": -105.5642
  },
  {
    "Date": "2026-04-18",
    "Location": "Pomfret, MD",
    "Host": "Nose-It-All, LLC",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 38.5489,
    "Longitude": -77.061
  },
  {
    "Date": "2026-04-18",
    "Location": "Toledo, OH",
    "Host": "Robin Ford Dog Training LLC",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 41.6589,
    "Longitude": -83.5094
  },
  {
    "Date": "2026-04-18",
    "Location": "Winterset, IA",
    "Host": "KBP Dog Training",
    "TrialTypes": "NW2, NW1",
    "EventCount": 2,
    "Latitude": 41.3584,
    "Longitude": -94.0362
  },
  {
    "Date": "2026-04-18",
    "Location": "Woodstock, IL",
    "Host": "Northwest Obedience Club Inc",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 42.3628,
    "Longitude": -88.4521
  },
  {
    "Date": "2026-04-19",
    "Location": "Glenwood, NY",
    "Host": "Do Over Dog Training",
    "TrialTypes": "L2I, L1E, L2C, ELT-S",
    "EventCount": 4,
    "Latitude": 42.6512,
    "Longitude": -78.6192
  },
  {
    "Date": "2026-04-20",
    "Location": "Amherst, NH",
    "Host": "Lucky Dog Events",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 42.9129,
    "Longitude": -71.6031
  },
  {
    "Date": "2026-04-21",
    "Location": "Stony Point , NY",
    "Host": "Saints2Source, LLC",
    "TrialTypes": "NW3, ELT, ELT-S, NW2",
    "EventCount": 4,
    "Latitude": 41.2048,
    "Longitude": -73.978
  },
  {
    "Date": "2026-04-24",
    "Location": "Asheboro, NC",
    "Host": "K9 Nose Adventures, LLC",
    "TrialTypes": "ELT, NW3, ELT-S, NW2",
    "EventCount": 4,
    "Latitude": 35.702,
    "Longitude": -79.807
  },
  {
    "Date": "2026-04-24",
    "Location": "Easton, MD",
    "Host": "Red Huskies",
    "TrialTypes": "L3C, NW2, L3V, NW1",
    "EventCount": 4,
    "Latitude": 38.725,
    "Longitude": -76.114
  },
  {
    "Date": "2026-04-25",
    "Location": "Canfield, OH",
    "Host": "Nosework Addicts, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 41.0627,
    "Longitude": -80.8015
  },
  {
    "Date": "2026-04-25",
    "Location": "Columbus, MT",
    "Host": "Canine Connection",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 45.6739,
    "Longitude": -109.2177
  },
  {
    "Date": "2026-04-25",
    "Location": "Ellicottville, NY",
    "Host": "Do Over Dog Training",
    "TrialTypes": "ELT, ELT-S, NW1",
    "EventCount": 3,
    "Latitude": 42.3125,
    "Longitude": -78.6427
  },
  {
    "Date": "2026-04-25",
    "Location": "Havre de Grace, MD",
    "Host": "Chesapeake Search Dogs",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 39.5888,
    "Longitude": -76.0777
  },
  {
    "Date": "2026-04-25",
    "Location": "Portland, OR",
    "Host": "Trust Your Dog K9 Events",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 45.4756,
    "Longitude": -122.6727
  },
  {
    "Date": "2026-04-25",
    "Location": "Sharon, MA",
    "Host": "Bay State Sniffers",
    "TrialTypes": "NW3, ELT-P",
    "EventCount": 2,
    "Latitude": 42.1414,
    "Longitude": -71.1348
  },
  {
    "Date": "2026-04-25",
    "Location": "Suring, WI",
    "Host": "Clever Sniffers, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 45.0275,
    "Longitude": -88.3847
  },
  {
    "Date": "2026-04-25",
    "Location": "Traverse City, MI",
    "Host": "Every Dog Nosework",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 44.7876,
    "Longitude": -85.6346
  },
  {
    "Date": "2026-05-01",
    "Location": "Faribault, MN",
    "Host": "St. Paul Dog Training Club",
    "TrialTypes": "NW3, NW1, L2E, L3C",
    "EventCount": 4,
    "Latitude": 43.6745,
    "Longitude": -93.9132
  },
  {
    "Date": "2026-05-01",
    "Location": "Nyack, NY",
    "Host": "Waggin' Work",
    "TrialTypes": "NW2, NW1, ELT-P, ELT-S",
    "EventCount": 4,
    "Latitude": 41.0507,
    "Longitude": -73.8877
  },
  {
    "Date": "2026-05-01",
    "Location": "Turlock, CA",
    "Host": "Two Nosey Girls",
    "TrialTypes": "L1I, L2I, ELT-S, L3I",
    "EventCount": 4,
    "Latitude": 37.4748,
    "Longitude": -120.8778
  },
  {
    "Date": "2026-05-02",
    "Location": "Alexis, IL",
    "Host": "Kudos for Canines, LLC",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 41.0497,
    "Longitude": -90.5976
  },
  {
    "Date": "2026-05-02",
    "Location": "Ashby , MA",
    "Host": "Dogs! Carolyn Barney",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 42.6376,
    "Longitude": -71.7818
  },
  {
    "Date": "2026-05-02",
    "Location": "Hillsdale, NY",
    "Host": "For the Love of Dogs NY LLC",
    "TrialTypes": "ELT-P, ELT",
    "EventCount": 2,
    "Latitude": 42.138,
    "Longitude": -73.5643
  },
  {
    "Date": "2026-05-02",
    "Location": "Santa Paula, CA",
    "Host": "Pink Biscuit K9s",
    "TrialTypes": "NW3, ELT-S, NW2",
    "EventCount": 3,
    "Latitude": 34.3228,
    "Longitude": -119.0567
  },
  {
    "Date": "2026-05-02",
    "Location": "Sedona, AZ",
    "Host": "Successful Sniffer",
    "TrialTypes": "L1I, NW2, NW3",
    "EventCount": 3,
    "Latitude": 34.9176,
    "Longitude": -111.7711
  },
  {
    "Date": "2026-05-02",
    "Location": "Vancouver, WA",
    "Host": "Sniffketeers",
    "TrialTypes": "ELT-S",
    "EventCount": 1,
    "Latitude": 45.5912,
    "Longitude": -122.7237
  },
  {
    "Date": "2026-05-07",
    "Location": "Lancaster, PA",
    "Host": "Red Huskies Nose Work, LLC",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 40.0869,
    "Longitude": -76.3444
  },
  {
    "Date": "2026-05-08",
    "Location": "Grand Island, NY",
    "Host": "Do Over Dog Training",
    "TrialTypes": "SMT, ELT",
    "EventCount": 2,
    "Latitude": 42.9964,
    "Longitude": -78.9764
  },
  {
    "Date": "2026-05-08",
    "Location": "Jarrettsville, MD",
    "Host": "Firezone GS",
    "TrialTypes": "ELT, NW3, ELT-S, L2I",
    "EventCount": 4,
    "Latitude": 39.5567,
    "Longitude": -76.5105
  },
  {
    "Date": "2026-05-08",
    "Location": "Warwick, NY",
    "Host": "Top Notch Dogs, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 41.2975,
    "Longitude": -74.3635
  },
  {
    "Date": "2026-05-08",
    "Location": "Wrightwood, CA",
    "Host": "JavaK9s, LLC",
    "TrialTypes": "NW3, L1C, L1I",
    "EventCount": 3,
    "Latitude": 34.3121,
    "Longitude": -117.6463
  },
  {
    "Date": "2026-05-09",
    "Location": "Brighton, MI",
    "Host": "Every Dog Nosework",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 42.5315,
    "Longitude": -83.7753
  },
  {
    "Date": "2026-05-09",
    "Location": "Charlton, MA",
    "Host": "HeavenScent Sniffers",
    "TrialTypes": "ELT-P, L2C, NW1",
    "EventCount": 3,
    "Latitude": 42.1735,
    "Longitude": -71.9432
  },
  {
    "Date": "2026-05-09",
    "Location": "Egg Harbor City, NJ",
    "Host": "Rotts-n-Notts Nosework LLC",
    "TrialTypes": "L3I, NW1, NW3",
    "EventCount": 3,
    "Latitude": 39.5112,
    "Longitude": -74.6493
  },
  {
    "Date": "2026-05-09",
    "Location": "Livingston, MT",
    "Host": "Trails and Tails Dog School",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 45.7061,
    "Longitude": -110.5519
  },
  {
    "Date": "2026-05-09",
    "Location": "Malvern, IA",
    "Host": "Two Tails Unlimited",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 40.9831,
    "Longitude": -95.5514
  },
  {
    "Date": "2026-05-09",
    "Location": "Poland Springs, ME",
    "Host": "Bare Bones Nosework, LLC",
    "TrialTypes": "L1I, L2I",
    "EventCount": 2,
    "Latitude": 44.0628,
    "Longitude": -70.3421
  },
  {
    "Date": "2026-05-09",
    "Location": "Union Grove, WI",
    "Host": "Loving Paws, LLC",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 42.7167,
    "Longitude": -88.075
  },
  {
    "Date": "2026-05-14",
    "Location": "Frederick, MD",
    "Host": "Red Huskies",
    "TrialTypes": "ELT-P, ELT-S, L2I, L2E, L1E",
    "EventCount": 5,
    "Latitude": 39.4319,
    "Longitude": -77.4299
  },
  {
    "Date": "2026-05-15",
    "Location": "Cannon Falls, MN",
    "Host": "Saint Paul Dog Training Club",
    "TrialTypes": "ELT, ELT-S, L3E, L1I, L1E",
    "EventCount": 5,
    "Latitude": 44.5559,
    "Longitude": -92.9375
  },
  {
    "Date": "2026-05-15",
    "Location": "Phoenix, MD",
    "Host": "Oriole Dog Training Club",
    "TrialTypes": "NW3, L1I, NW1",
    "EventCount": 3,
    "Latitude": 39.4859,
    "Longitude": -76.6143
  },
  {
    "Date": "2026-05-15",
    "Location": "Watsonville, CA",
    "Host": "CalCoastal Dog Owners Group",
    "TrialTypes": "ELT-S, L3C, L1C",
    "EventCount": 3,
    "Latitude": 36.9299,
    "Longitude": -121.716
  },
  {
    "Date": "2026-05-16",
    "Location": "Alexander, NY",
    "Host": "Do Over Dog Training",
    "TrialTypes": "L1V, L2V, L3C, L3I",
    "EventCount": 4,
    "Latitude": 42.9324,
    "Longitude": -78.293
  },
  {
    "Date": "2026-05-16",
    "Location": "Bellingham, WA",
    "Host": "The Nosework Magic",
    "TrialTypes": "NW2, NW1",
    "EventCount": 2,
    "Latitude": 48.7277,
    "Longitude": -122.5192
  },
  {
    "Date": "2026-05-16",
    "Location": "Burien, WA",
    "Host": "Northwest K9 Sniffers",
    "TrialTypes": "NW3, L2V, L2I",
    "EventCount": 3,
    "Latitude": 47.4876,
    "Longitude": -122.311
  },
  {
    "Date": "2026-05-16",
    "Location": "Durham, NC",
    "Host": "Whole Dog Institute, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 35.95,
    "Longitude": -78.9293
  },
  {
    "Date": "2026-05-16",
    "Location": "Kittanning, PA",
    "Host": "Steel City Nosework, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 40.7826,
    "Longitude": -79.5068
  },
  {
    "Date": "2026-05-16",
    "Location": "Monticello , NY",
    "Host": "Saints2Source, LLC",
    "TrialTypes": "NW3, ELT, ELT-S",
    "EventCount": 3,
    "Latitude": 41.6325,
    "Longitude": -74.7045
  },
  {
    "Date": "2026-05-16",
    "Location": "Peru, MA",
    "Host": "Lucky Dog Events",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 42.4269,
    "Longitude": -72.9974
  },
  {
    "Date": "2026-05-16",
    "Location": "Sandwich, IL",
    "Host": "For Your K9, Inc.",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 41.5971,
    "Longitude": -88.6489
  },
  {
    "Date": "2026-05-22",
    "Location": "Anchorage, AK",
    "Host": "Alaska Dog Sports",
    "TrialTypes": "NW1, NW2, NW3",
    "EventCount": 3,
    "Latitude": 61.2402,
    "Longitude": -149.8449
  },
  {
    "Date": "2026-05-22",
    "Location": "Montrose, CO",
    "Host": "Mountain Dogs LLC",
    "TrialTypes": "ELT, NW3, NW2, NW1",
    "EventCount": 4,
    "Latitude": 38.5172,
    "Longitude": -107.8554
  },
  {
    "Date": "2026-05-22",
    "Location": "San Luis Obispo, CA",
    "Host": "Gentle Touch Pet Training",
    "TrialTypes": "NW1, L1I, NW2",
    "EventCount": 3,
    "Latitude": 35.3988,
    "Longitude": -120.3865
  },
  {
    "Date": "2026-05-23",
    "Location": "Alpharetta, GA",
    "Host": "Georgia Nosework, LLC",
    "TrialTypes": "NW3, ELT-S, NW2, ELT",
    "EventCount": 4,
    "Latitude": 34.0868,
    "Longitude": -84.3434
  },
  {
    "Date": "2026-05-23",
    "Location": "Columbus, MT",
    "Host": "Canine Connection",
    "TrialTypes": "L1I, L2I, ELT-S, NW2",
    "EventCount": 4,
    "Latitude": 45.6395,
    "Longitude": -109.2374
  },
  {
    "Date": "2026-05-23",
    "Location": "Emmitsburg, MD",
    "Host": "Red Huskies",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 39.7046,
    "Longitude": -77.342
  },
  {
    "Date": "2026-05-23",
    "Location": "Lancaster, PA",
    "Host": "Nose-It-All, LLC",
    "TrialTypes": "ELT, ELT-P, NW3",
    "EventCount": 3,
    "Latitude": 40.0004,
    "Longitude": -76.2608
  },
  {
    "Date": "2026-05-23",
    "Location": "Murfreesboro, TN",
    "Host": "Dogs Have Amazing Noses, LLC",
    "TrialTypes": "ELT, NW1",
    "EventCount": 2,
    "Latitude": 35.8792,
    "Longitude": -86.356
  },
  {
    "Date": "2026-05-23",
    "Location": "North Manchester, IN",
    "Host": "2 Nose You Is 2 Loves You",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 40.9762,
    "Longitude": -85.8106
  },
  {
    "Date": "2026-05-23",
    "Location": "Rainier, WA",
    "Host": "Let's Talk Dogs, LLC & About Face K9 Academy",
    "TrialTypes": "ELT-S, L1C, NW2",
    "EventCount": 3,
    "Latitude": 46.9407,
    "Longitude": -122.6974
  },
  {
    "Date": "2026-05-23",
    "Location": "Red Feather Lakes, CO",
    "Host": "Beyond Elevation K9 Training LLC",
    "TrialTypes": "NW1, NW2, NW3",
    "EventCount": 3,
    "Latitude": 40.8268,
    "Longitude": -105.6067
  },
  {
    "Date": "2026-05-23",
    "Location": "Rockaway, NJ",
    "Host": "Shamrock Pot Of Gold K9 Scenter",
    "TrialTypes": "NW3, L3E, NW2, ELT-S",
    "EventCount": 4,
    "Latitude": 40.8608,
    "Longitude": -74.5361
  },
  {
    "Date": "2026-05-23",
    "Location": "Sandy, OR",
    "Host": "Trust Your Dog K9 Events",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 45.3505,
    "Longitude": -122.2168
  },
  {
    "Date": "2026-05-25",
    "Location": "Manchester, NH",
    "Host": "For the Love of Dogs NY, LLC",
    "TrialTypes": "NW2, ELT-P, ELT",
    "EventCount": 3,
    "Latitude": 43.0295,
    "Longitude": -71.4326
  },
  {
    "Date": "2026-05-28",
    "Location": "Concord, CA",
    "Host": "The Bay Team",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 37.9999,
    "Longitude": -121.991
  },
  {
    "Date": "2026-05-29",
    "Location": "Grand Junction, CO",
    "Host": "Mountain Dogs LLC",
    "TrialTypes": "NW3, L1V, L1E",
    "EventCount": 3,
    "Latitude": 39.1147,
    "Longitude": -108.6024
  },
  {
    "Date": "2026-05-30",
    "Location": "Amherst, NY",
    "Host": "Do Over Dog Training",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 42.958,
    "Longitude": -78.8344
  },
  {
    "Date": "2026-05-30",
    "Location": "Eden Prairie, MN",
    "Host": "The K9 Nose",
    "TrialTypes": "NW2",
    "EventCount": 1,
    "Latitude": 44.8714,
    "Longitude": -93.4321
  },
  {
    "Date": "2026-05-30",
    "Location": "Spencer, MA",
    "Host": "Lucky Dog Events",
    "TrialTypes": "ELT, L1E, L2C",
    "EventCount": 3,
    "Latitude": 42.2104,
    "Longitude": -72.0045
  },
  {
    "Date": "2026-06-06",
    "Location": "Dunmore, PA",
    "Host": "Your Dog's Place, LLC",
    "TrialTypes": "ELT, ELT-S, L1C",
    "EventCount": 3,
    "Latitude": 41.4074,
    "Longitude": -75.6367
  },
  {
    "Date": "2026-06-06",
    "Location": "Enterprise, OR",
    "Host": "Country K9 Nosework, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 45.3951,
    "Longitude": -117.2519
  },
  {
    "Date": "2026-06-06",
    "Location": "Manheim, PA",
    "Host": "Nose-It-All, LLC",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 40.1563,
    "Longitude": -76.3923
  },
  {
    "Date": "2026-06-06",
    "Location": "Meadowbrook, PA",
    "Host": "Patricia Grassey",
    "TrialTypes": "NW3, NW1, NW2, ELT",
    "EventCount": 4,
    "Latitude": 40.1377,
    "Longitude": -75.0684
  },
  {
    "Date": "2026-06-06",
    "Location": "Rochester, NH",
    "Host": "Pawsitive Image",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 43.3232,
    "Longitude": -70.9918
  },
  {
    "Date": "2026-06-06",
    "Location": "Shawnee, OK",
    "Host": "The Doggie Spot, LLC",
    "TrialTypes": "NW3, NW2, NW1",
    "EventCount": 3,
    "Latitude": 35.307,
    "Longitude": -96.9423
  },
  {
    "Date": "2026-06-06",
    "Location": "Slippery Rock, PA",
    "Host": "Nosework Addicts, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 41.0542,
    "Longitude": -80.012
  },
  {
    "Date": "2026-06-06",
    "Location": "Sparks Glencoe, MD",
    "Host": "Chesapeake Search Dogs",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 39.518,
    "Longitude": -76.6578
  },
  {
    "Date": "2026-06-06",
    "Location": "Wrightstown, WI",
    "Host": "NEWk9Scent Work LLC",
    "TrialTypes": "NW3, NW1, L1I",
    "EventCount": 3,
    "Latitude": 44.282,
    "Longitude": -88.1599
  },
  {
    "Date": "2026-06-12",
    "Location": "Gunnison, CO",
    "Host": "Mountain Dogs LLC",
    "TrialTypes": "ELT, ELT-S, ELT-P",
    "EventCount": 3,
    "Latitude": 38.6086,
    "Longitude": -107.0157
  },
  {
    "Date": "2026-06-12",
    "Location": "New Hope, PA",
    "Host": "Patricia Grassey",
    "TrialTypes": "ELT, ELT-P, ELT-S, L3E",
    "EventCount": 4,
    "Latitude": 40.3471,
    "Longitude": -74.9421
  },
  {
    "Date": "2026-06-13",
    "Location": "East Helena, MT",
    "Host": "Nose Work Breakfast Club",
    "TrialTypes": "NW1, NW2, NW3",
    "EventCount": 3,
    "Latitude": 46.6066,
    "Longitude": -111.867
  },
  {
    "Date": "2026-06-13",
    "Location": "Ithaca, NY",
    "Host": "The Brainy Canine",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 42.4749,
    "Longitude": -76.5048
  },
  {
    "Date": "2026-06-13",
    "Location": "Kenosha, WI",
    "Host": "Loving Paws Dog Training LLC",
    "TrialTypes": "ELT-S, L1I, NW1",
    "EventCount": 3,
    "Latitude": 42.5521,
    "Longitude": -87.8021
  },
  {
    "Date": "2026-06-13",
    "Location": "Linden, MI",
    "Host": "Every Dog Nosework",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 42.8032,
    "Longitude": -83.7565
  },
  {
    "Date": "2026-06-13",
    "Location": "Nazareth/Windgap, PA",
    "Host": "Paws n' Sniff",
    "TrialTypes": "NW2, ELT, NW1, ELT-S, NW3",
    "EventCount": 5,
    "Latitude": 40.7004,
    "Longitude": -75.3489
  },
  {
    "Date": "2026-06-13",
    "Location": "Palmyra, VA",
    "Host": "Your Dog Knows LLC",
    "TrialTypes": "L1I, L2I, L1C, L2C",
    "EventCount": 4,
    "Latitude": 37.9089,
    "Longitude": -78.3107
  },
  {
    "Date": "2026-06-18",
    "Location": "Westminster, MD",
    "Host": "Firezone GS",
    "TrialTypes": "ELT, NW3, ELT-S, NW2",
    "EventCount": 4,
    "Latitude": 39.5711,
    "Longitude": -77.0119
  },
  {
    "Date": "2026-06-19",
    "Location": "Bayfield, CO",
    "Host": "Mountain Dogs, LLC",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 37.2638,
    "Longitude": -107.5514
  },
  {
    "Date": "2026-06-19",
    "Location": "Jordan, MN",
    "Host": "St. Paul Dog Training Club",
    "TrialTypes": "ELT, ELT-P, L1V, L2V",
    "EventCount": 4,
    "Latitude": 44.6741,
    "Longitude": -93.6305
  },
  {
    "Date": "2026-06-19",
    "Location": "New Rochelle, NY",
    "Host": "For the Love of Dogs NY, LLC",
    "TrialTypes": "ELT, NW3, ELT-P",
    "EventCount": 3,
    "Latitude": 40.868,
    "Longitude": -73.7639
  },
  {
    "Date": "2026-06-20",
    "Location": "Cumming, GA",
    "Host": "Georgia Nosework, LLC",
    "TrialTypes": "L2I, L2C, L3C, L1I",
    "EventCount": 4,
    "Latitude": 34.1897,
    "Longitude": -84.1299
  },
  {
    "Date": "2026-06-20",
    "Location": "Danvers, MA",
    "Host": "Everydog, LLC",
    "TrialTypes": "NW1, NW2, NW3",
    "EventCount": 3,
    "Latitude": 42.5748,
    "Longitude": -70.9716
  },
  {
    "Date": "2026-06-20",
    "Location": "Florissant, MO",
    "Host": "Happy Dog Concepts",
    "TrialTypes": "NW1, NW2, NW3",
    "EventCount": 3,
    "Latitude": 38.7636,
    "Longitude": -90.3308
  },
  {
    "Date": "2026-06-20",
    "Location": "Pittsburgh, PA",
    "Host": "Nosework Addicts, LLC",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 40.4718,
    "Longitude": -80.0323
  },
  {
    "Date": "2026-06-20",
    "Location": "Terryville, CT",
    "Host": "Willoughby Training",
    "TrialTypes": "NW3, NW1",
    "EventCount": 2,
    "Latitude": 41.7098,
    "Longitude": -72.995
  },
  {
    "Date": "2026-06-20",
    "Location": "White Salmon, WA",
    "Host": "Sharon Smith",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 45.6777,
    "Longitude": -121.5163
  },
  {
    "Date": "2026-06-26",
    "Location": "Delran, NJ",
    "Host": "K9 InScentives",
    "TrialTypes": "NW1, ELT",
    "EventCount": 2,
    "Latitude": 40.0112,
    "Longitude": -74.9778
  },
  {
    "Date": "2026-06-26",
    "Location": "Loveland, CO",
    "Host": "NoCo Unleashed LLC",
    "TrialTypes": "ELT-S, L2C, L2I, L1C",
    "EventCount": 4,
    "Latitude": 40.4241,
    "Longitude": -105.0313
  },
  {
    "Date": "2026-06-26",
    "Location": "Red Lodge, MT",
    "Host": "Canine Connection",
    "TrialTypes": "ELT, NW3, NW1, NW2",
    "EventCount": 4,
    "Latitude": 45.1831,
    "Longitude": -109.2157
  },
  {
    "Date": "2026-06-27",
    "Location": "Burlington, WI",
    "Host": "Loving Paws Dog Training LLC",
    "TrialTypes": "NW2, ELT",
    "EventCount": 2,
    "Latitude": 42.6582,
    "Longitude": -88.3005
  },
  {
    "Date": "2026-06-27",
    "Location": "De Pere, WI",
    "Host": "NEWk9Scent Work LLC",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 44.4055,
    "Longitude": -88.054
  },
  {
    "Date": "2026-06-27",
    "Location": "Deming, WA",
    "Host": "The Nosework Magic",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 48.8776,
    "Longitude": -122.2828
  },
  {
    "Date": "2026-06-27",
    "Location": "Inver Grove Heights, MN",
    "Host": "Outside the Box Dog Training, LLC",
    "TrialTypes": "NW3, L2C, L2I",
    "EventCount": 3,
    "Latitude": 44.8608,
    "Longitude": -92.9949
  },
  {
    "Date": "2026-06-27",
    "Location": "Lockport, IL",
    "Host": "4G & TB",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 41.6186,
    "Longitude": -88.1073
  },
  {
    "Date": "2026-06-27",
    "Location": "New Wilmington, PA",
    "Host": "Steel City Nosework, LLC",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 41.1676,
    "Longitude": -80.338
  },
  {
    "Date": "2026-06-27",
    "Location": "Salem, OR",
    "Host": "Doglandia, LLC",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 44.9197,
    "Longitude": -123.0632
  },
  {
    "Date": "2026-06-27",
    "Location": "Somers, CT",
    "Host": "HeavenScent Sniffers",
    "TrialTypes": "NW2, L3C, ELT-S",
    "EventCount": 3,
    "Latitude": 41.9886,
    "Longitude": -72.4538
  },
  {
    "Date": "2026-06-30",
    "Location": "Delran, NJ",
    "Host": "Ev-ry Earthdog, LLC",
    "TrialTypes": "NW3, NW1, NW2, ELT-P",
    "EventCount": 4,
    "Latitude": 39.977,
    "Longitude": -74.9777
  },
  {
    "Date": "2026-07-03",
    "Location": "Huntington, MA",
    "Host": "Shamrock Pot Of Gold K9 Scenter",
    "TrialTypes": "NW3, ELT, ELT-S, L2I",
    "EventCount": 4,
    "Latitude": 42.1932,
    "Longitude": -72.9111
  },
  {
    "Date": "2026-07-06",
    "Location": "Montgomery, NY",
    "Host": "For the Love of Dogs NY LLC",
    "TrialTypes": "NW2, ELT",
    "EventCount": 2,
    "Latitude": 42.8724,
    "Longitude": -74.3907
  },
  {
    "Date": "2026-07-10",
    "Location": "Leadville, CO",
    "Host": "Mountain Dogs LLC",
    "TrialTypes": "NW3, NW2, NW1, L2I, L2C",
    "EventCount": 5,
    "Latitude": 39.2819,
    "Longitude": -106.3329
  },
  {
    "Date": "2026-07-10",
    "Location": "Sparks Glencoe, MD",
    "Host": "Firezone GS",
    "TrialTypes": "ELT-P, ELT-S, L3I, ELT",
    "EventCount": 4,
    "Latitude": 39.5537,
    "Longitude": -76.6407
  },
  {
    "Date": "2026-07-11",
    "Location": "Livonia, MI",
    "Host": "Every Dog Nosework",
    "TrialTypes": "ELT-P, NW1",
    "EventCount": 2,
    "Latitude": 42.3572,
    "Longitude": -83.4019
  },
  {
    "Date": "2026-07-13",
    "Location": "Derry, NH",
    "Host": "Lucky Dog Events",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 42.8802,
    "Longitude": -71.3417
  },
  {
    "Date": "2026-07-13",
    "Location": "Florham Park, NJ",
    "Host": "For the Love of Dogs NY LLC",
    "TrialTypes": "L1C, L1I, ELT",
    "EventCount": 3,
    "Latitude": 40.7552,
    "Longitude": -74.392
  },
  {
    "Date": "2026-07-17",
    "Location": "Encinitas, CA",
    "Host": "Rewarding Rover LLC & UberDog/Jessica Koester",
    "TrialTypes": "NW2, NW1, ELT-S",
    "EventCount": 3,
    "Latitude": 33.0664,
    "Longitude": -117.2906
  },
  {
    "Date": "2026-07-17",
    "Location": "Leadville, CO",
    "Host": "Mountain Dogs LLC",
    "TrialTypes": "ELT, NW3, ELT-S, NW2",
    "EventCount": 4,
    "Latitude": 39.2304,
    "Longitude": -106.3104
  },
  {
    "Date": "2026-07-18",
    "Location": "Los Osos, CA",
    "Host": "Central Coast Nosework Club",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 35.2721,
    "Longitude": -120.8107
  },
  {
    "Date": "2026-07-18",
    "Location": "Woodbury, MN",
    "Host": "Gretchen Hofheins-Wackerfuss",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 44.9295,
    "Longitude": -92.9181
  },
  {
    "Date": "2026-07-25",
    "Location": "Elmira, OR",
    "Host": "Kiddy Christie",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 44.1023,
    "Longitude": -123.3666
  },
  {
    "Date": "2026-07-29",
    "Location": "Soldotna, AK",
    "Host": "Peninsula Dog Obedience Group LLC",
    "TrialTypes": "NW1, NW2, NW3, ELT",
    "EventCount": 4,
    "Latitude": 60.5195,
    "Longitude": -151.0461
  },
  {
    "Date": "2026-08-01",
    "Location": "Bettendorf, IA",
    "Host": "Fur Better Fur Worse Dog Training",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 41.5354,
    "Longitude": -90.5369
  },
  {
    "Date": "2026-08-01",
    "Location": "Columbia, MO",
    "Host": "Columbia Canine Sports Center, LLC",
    "TrialTypes": "L1V, L1I, L1C, L2C",
    "EventCount": 4,
    "Latitude": 38.9886,
    "Longitude": -92.297
  },
  {
    "Date": "2026-08-01",
    "Location": "Deming, WA",
    "Host": "The Nosework Magic",
    "TrialTypes": "ELT, NW2",
    "EventCount": 2,
    "Latitude": 48.8045,
    "Longitude": -122.224
  },
  {
    "Date": "2026-08-01",
    "Location": "Jefferson, WI",
    "Host": "K9 Ventures",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 43.061,
    "Longitude": -88.7734
  },
  {
    "Date": "2026-08-01",
    "Location": "Pillager, MN",
    "Host": "Nose 2 Tail Dog Training LLC",
    "TrialTypes": "NW1, NW2, ELT-P",
    "EventCount": 3,
    "Latitude": 46.2932,
    "Longitude": -94.4736
  },
  {
    "Date": "2026-08-07",
    "Location": "Huntington Beach, CA",
    "Host": "JavaK9s, LLC",
    "TrialTypes": "ELT, L2C, L2I",
    "EventCount": 3,
    "Latitude": 33.6386,
    "Longitude": -118.008
  },
  {
    "Date": "2026-08-08",
    "Location": "Altamont, IL",
    "Host": "Kudos for Canines, LLC",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 39.0171,
    "Longitude": -88.7131
  },
  {
    "Date": "2026-08-14",
    "Location": "La Jolla, CA",
    "Host": "Rewarding Rover LLC & UberDog/Jessica Koester",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 32.818,
    "Longitude": -117.2602
  },
  {
    "Date": "2026-08-15",
    "Location": "Greenwich, CT",
    "Host": "For the Love of Dogs NY LLC",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 41.0039,
    "Longitude": -73.6304
  },
  {
    "Date": "2026-08-15",
    "Location": "Monmouth, OR",
    "Host": "Doglandia, LLC",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 44.8191,
    "Longitude": -123.2063
  },
  {
    "Date": "2026-08-21",
    "Location": "Chelsea, MI",
    "Host": "Force Free Dale, LLC",
    "EventLink": "https://forcefreedale.com/events",
    "TrialTypes": "NW3, L1V, L1C, NW2",
    "EventCount": 4,
    "Latitude": 42.2779,
    "Longitude": -83.9772
  },
  {
    "Date": "2026-08-22",
    "Location": "Greenfield, MA",
    "Host": "Lucky Dog Events",
    "EventLink": "https://www.luckydogevents.com/centre-school-greenfield-mass-aug-2223",
    "TrialTypes": "NW3, NW2, NW1",
    "EventCount": 3,
    "Latitude": 42.6324,
    "Longitude": -72.587
  },
  {
    "Date": "2026-08-22",
    "Location": "Johnstown, NY",
    "Host": "My Dog Smells LLC",
    "EventLink": "https://www.mydogsmells.com/nacsw-trials",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 42.9744,
    "Longitude": -74.3376
  },
  {
    "Date": "2026-08-22",
    "Location": "North Bend, WA",
    "Host": "Northwest K9 Sniffers",
    "EventLink": "https://nwk9sniffers.org/aug-2026-north-bend/",
    "TrialTypes": "ELT, L1C, L2I",
    "EventCount": 3,
    "Latitude": 47.5387,
    "Longitude": -121.8325
  },
  {
    "Date": "2026-08-28",
    "Location": "Easton and Lutherville, MD",
    "Host": "Fair Play Labradors",
    "EventLink": "https://www.fairplaylabradors.com/easton-md-august-28-30-2026.html",
    "TrialTypes": "ELT-S, L2E, L2C, NW1, L1I",
    "EventCount": 5,
    "Latitude": 39.4589,
    "Longitude": -76.647
  },
  {
    "Date": "2026-08-28",
    "Location": "Meeker, CO",
    "Host": "Mountain Dogs LLC",
    "EventLink": "https://mountaindogs.org/",
    "TrialTypes": "NW3, NW1, L2C, NW2",
    "EventCount": 4,
    "Latitude": 40.0285,
    "Longitude": -107.8656
  },
  {
    "Date": "2026-08-29",
    "Location": "Dunkirk, NY",
    "Host": "Do Over Dog Training",
    "EventLink": "https://www.dooverdogtraining.com/trials",
    "TrialTypes": "NW1, NW2, ELT-S, L3E",
    "EventCount": 4,
    "Latitude": 42.5145,
    "Longitude": -79.3269
  },
  {
    "Date": "2026-08-31",
    "Location": "Cambria, CA",
    "Host": "Gentle Touch Pet Training",
    "EventLink": "https://www.gentlepets.com/gtpt-events/nacsw%E2%84%A2-elt%2Fl1e%2Fl2e-trials",
    "TrialTypes": "ELT, L1E, L2E",
    "EventCount": 3,
    "Latitude": 35.566,
    "Longitude": -121.1264
  },
  {
    "Date": "2026-09-05",
    "Location": "Luthersville, GA",
    "Host": "Hold The Line K9 LLC",
    "EventLink": "https://www.holdthelinek9nosework.com/",
    "TrialTypes": "L1I, L2I, NW3",
    "EventCount": 3,
    "Latitude": 33.1704,
    "Longitude": -84.7433
  },
  {
    "Date": "2026-09-11",
    "Location": "Richmond, VA",
    "Host": "Paws Plus Training, LLC",
    "EventLink": "https://pawsplustraining.com/",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 37.559,
    "Longitude": -77.4191
  },
  {
    "Date": "2026-09-12",
    "Location": "Clinton, PA",
    "Host": "Nosework Addicts, LLC",
    "EventLink": "https://www.noseworkaddictsllc.com/nacsw-trials",
    "TrialTypes": "NW1, ELT",
    "EventCount": 2,
    "Latitude": 40.5126,
    "Longitude": -80.3524
  },
  {
    "Date": "2026-09-12",
    "Location": "Lafayette Hill, PA",
    "Host": "Sniff Sniff Hooray",
    "EventLink": "https://sniffsniffhooray.com/",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 40.0832,
    "Longitude": -75.257
  },
  {
    "Date": "2026-09-12",
    "Location": "Loma Mar, CA",
    "Host": "The Bay Team",
    "EventLink": "https://www.bayteam.org/",
    "TrialTypes": "NW2, ELT",
    "EventCount": 2,
    "Latitude": 37.2611,
    "Longitude": -122.3176
  },
  {
    "Date": "2026-09-12",
    "Location": "Sharon, MA",
    "Host": "Bay State Sniffers",
    "EventLink": "http://www.baystatesniffers.com/",
    "TrialTypes": "ELT, ELT-S, NW2",
    "EventCount": 3,
    "Latitude": 42.0761,
    "Longitude": -71.1715
  },
  {
    "Date": "2026-09-13",
    "Location": "Colesville, MD",
    "Host": "Red Huskies",
    "EventLink": "https://nosework.redhuskies.com/",
    "TrialTypes": "ELT-S, L3C, NW3",
    "EventCount": 3,
    "Latitude": 39.092,
    "Longitude": -76.9696
  },
  {
    "Date": "2026-09-18",
    "Location": "Flint, MI",
    "Host": "Every Dog Nosework",
    "EventLink": "https://everydognosework.com/trials",
    "TrialTypes": "NW3, NW1, NW2, ELT-P",
    "EventCount": 4,
    "Latitude": 42.9954,
    "Longitude": -83.6577
  },
  {
    "Date": "2026-09-18",
    "Location": "New Milford, PA",
    "Host": "Your Dog's Place, LLC",
    "EventLink": "https://yourdogsplace.com/nacsw-trials/",
    "TrialTypes": "ELT, ELT-S, L1V",
    "EventCount": 3,
    "Latitude": 41.8763,
    "Longitude": -75.7063
  },
  {
    "Date": "2026-09-19",
    "Location": "Ford City, PA",
    "Host": "Steel City Nosework, LLC",
    "EventLink": "https://www.nose-it-all.com/",
    "TrialTypes": "ELT, ELT-S, NW2",
    "EventCount": 3,
    "Latitude": 40.783,
    "Longitude": -79.5727
  },
  {
    "Date": "2026-09-19",
    "Location": "Glen Mills, PA",
    "Host": "Firezone GS",
    "EventLink": "https://www.firezonegiantschnauzers.com/glenmillsschools",
    "TrialTypes": "NW3, ELT-P",
    "EventCount": 2,
    "Latitude": 39.9405,
    "Longitude": -75.5261
  },
  {
    "Date": "2026-09-19",
    "Location": "Palmer, MA",
    "Host": "HeavenScent Sniffers",
    "EventLink": "https://www.heavenscentsniffers.com/",
    "TrialTypes": "NW3, L2V, L1E",
    "EventCount": 3,
    "Latitude": 42.1852,
    "Longitude": -72.2989
  },
  {
    "Date": "2026-09-19",
    "Location": "Stevenson, WA",
    "Host": "Sharon Smith",
    "EventLink": "https://www.sundanceshepherds.com/",
    "TrialTypes": "NW1, NW2, L1V, L1C",
    "EventCount": 4,
    "Latitude": 45.6455,
    "Longitude": -121.9148
  },
  {
    "Date": "2026-09-25",
    "Location": "Frederick, MD",
    "Host": "Red Huskies",
    "EventLink": "https://nosework.redhuskies.com/index.php/events/frederick_fall2026/",
    "TrialTypes": "L3E, ELT-S, ELT-P, ELT",
    "EventCount": 4,
    "Latitude": 39.4197,
    "Longitude": -77.4124
  },
  {
    "Date": "2026-09-26",
    "Location": "Columbus, MT",
    "Host": "Canine Connection",
    "EventLink": "https://canineconnection23.godaddysites.com/2026-trials",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 45.6703,
    "Longitude": -109.282
  },
  {
    "Date": "2026-09-26",
    "Location": "Glenview, IL",
    "Host": "Northwest Obedience Club Inc",
    "EventLink": "https://northwestobedienceclub.org/event/noci-nacsw-elite-trial/",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 42.047,
    "Longitude": -87.8156
  },
  {
    "Date": "2026-09-26",
    "Location": "Kintnersville, PA",
    "Host": "Paws n’ Sniff",
    "EventLink": "http://www.pawsnsniff.com/september-26-27.-2026.html",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 40.5287,
    "Longitude": -75.2132
  },
  {
    "Date": "2026-09-26",
    "Location": "Lawrenceville, GA",
    "Host": "Right Choice Dog Training, LLC",
    "EventLink": "https://www.rightchoicedogtraining.net/eventandvolunteer",
    "TrialTypes": "L1E, NW2, ELT-S, L3I",
    "EventCount": 4,
    "Latitude": 33.9285,
    "Longitude": -83.9397
  },
  {
    "Date": "2026-09-26",
    "Location": "New City, NY",
    "Host": "Saints2Source, LLC",
    "EventLink": "https://www.saints2source.com/",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 41.1739,
    "Longitude": -73.9702
  },
  {
    "Date": "2026-09-26",
    "Location": "Rehoboth, MA",
    "Host": "Dogs Make Scents",
    "EventLink": "https://dogsmakescents.com/",
    "TrialTypes": "NW3, NW1",
    "EventCount": 2,
    "Latitude": 41.8187,
    "Longitude": -71.2019
  },
  {
    "Date": "2026-09-27",
    "Location": "Dover, DE",
    "Host": "Patricia Grassey",
    "EventLink": "https://thesniffinghound.com/about",
    "TrialTypes": "NW3, NW2, ELT",
    "EventCount": 3,
    "Latitude": 39.1596,
    "Longitude": -75.5608
  },
  {
    "Date": "2026-09-28",
    "Location": "Concord, NH",
    "Host": "For the Love of Dogs NY LLC",
    "EventLink": "https://www.fortheloveofdogsny.com/trials-events",
    "TrialTypes": "NW2, NW3",
    "EventCount": 2,
    "Latitude": 43.2146,
    "Longitude": -71.5264
  },
  {
    "Date": "2026-10-03",
    "Location": "Hagerstown, MD",
    "Host": "Firezone GS",
    "EventLink": "https://www.firezonegiantschnauzers.com/nose-work-trials",
    "TrialTypes": "ELT-S, NW3, ELT",
    "EventCount": 3,
    "Latitude": 39.6465,
    "Longitude": -77.7594
  },
  {
    "Date": "2026-10-03",
    "Location": "Hammond, LA",
    "Host": "Dog Gone Right, LLC",
    "EventLink": "https://doggoneright.net/",
    "TrialTypes": "NW1, NW2, L1I, ELT-S",
    "EventCount": 4,
    "Latitude": 30.4936,
    "Longitude": -90.4906
  },
  {
    "Date": "2026-10-03",
    "Location": "Jefferson, OR",
    "Host": "Doglandia, LLC",
    "EventLink": "https://www.cyberdogonline.com/",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 44.5589,
    "Longitude": -121.2339
  },
  {
    "Date": "2026-10-03",
    "Location": "Kilmarnock, VA",
    "Host": "Red Huskies",
    "EventLink": "https://nosework.redhuskies.com/",
    "TrialTypes": "ELT, NW1, ELT-S",
    "EventCount": 3,
    "Latitude": 37.7225,
    "Longitude": -76.3376
  },
  {
    "Date": "2026-10-03",
    "Location": "Nashua, NH",
    "Host": "The Big Sniff, LLC",
    "EventLink": "http://www.thebigsniff.com/",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 42.7165,
    "Longitude": -71.4776
  },
  {
    "Date": "2026-10-03",
    "Location": "New Paltz, NY",
    "Host": "Top Notch Dogs, LLC",
    "EventLink": "https://www.topnotchdogtraining.com/",
    "TrialTypes": "NW1, L2C, ELT",
    "EventCount": 3,
    "Latitude": 41.7388,
    "Longitude": -74.1229
  },
  {
    "Date": "2026-10-03",
    "Location": "Northampton, MA",
    "Host": "Lucky Dog Events",
    "EventLink": "https://www.luckydogevents.com/",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 42.2707,
    "Longitude": -72.6395
  },
  {
    "Date": "2026-10-03",
    "Location": "Seguin, TX",
    "Host": "Scent Work Across Texas",
    "EventLink": "https://scentworkacrosstexas.com/",
    "TrialTypes": "NW1, NW2, NW3",
    "EventCount": 3,
    "Latitude": 29.6175,
    "Longitude": -97.9386
  },
  {
    "Date": "2026-10-03",
    "Location": "Sisters, OR",
    "Host": "Sunriver K9 Genie, LLC",
    "EventLink": "https://k9genie.com/events",
    "TrialTypes": "NW2, ELT",
    "EventCount": 2,
    "Latitude": 44.261,
    "Longitude": -121.5354
  },
  {
    "Date": "2026-10-03",
    "Location": "Waynesboro, PA",
    "Host": "Nose-It-All, LLC",
    "EventLink": "https://www.nose-it-all.com/",
    "TrialTypes": "NW1, NW2, NW3",
    "EventCount": 3,
    "Latitude": 39.7112,
    "Longitude": -77.574
  },
  {
    "Date": "2026-10-09",
    "Location": "Middlebury, CT",
    "Host": "For the Love of Dogs NY LLC",
    "EventLink": "https://www.fortheloveofdogsny.com/trials-events",
    "TrialTypes": "ELT, ELT-P, NW1",
    "EventCount": 3,
    "Latitude": 41.5264,
    "Longitude": -73.0861
  },
  {
    "Date": "2026-10-09",
    "Location": "Pueblo, CO",
    "Host": "Mountain Dogs, LLC",
    "EventLink": "https://mountaindogs.org/",
    "TrialTypes": "SMT, ELT",
    "EventCount": 2,
    "Latitude": 38.2539,
    "Longitude": -104.6343
  },
  {
    "Date": "2026-10-09",
    "Location": "Rock Island, IL",
    "Host": "Fur Better Fur Worse Dog Training",
    "EventLink": "http://www.furbetterfurworse.com/",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 41.4087,
    "Longitude": -90.6052
  },
  {
    "Date": "2026-10-10",
    "Location": "Auburn, WA",
    "Host": "Northwest K9 Sniffers",
    "EventLink": "https://nwk9sniffers.org/",
    "TrialTypes": "ELT-S, L2E, L3I",
    "EventCount": 3,
    "Latitude": 47.2706,
    "Longitude": -122.2329
  },
  {
    "Date": "2026-10-10",
    "Location": "Court Granger, IA",
    "Host": "KBP Dog Training",
    "EventLink": "https://kbpdogtraining.com",
    "TrialTypes": "NW1, NW2, NW3",
    "EventCount": 3,
    "Latitude": 41.7991,
    "Longitude": -93.8153
  },
  {
    "Date": "2026-10-10",
    "Location": "Eagan, MN",
    "Host": "St Paul Dog Training Club",
    "EventLink": "https://spdtc.com/events-at-spdtc/",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 44.809,
    "Longitude": -93.1473
  },
  {
    "Date": "2026-10-10",
    "Location": "Eldred, NY",
    "Host": "Your Dog's Place, LLC",
    "EventLink": "http://www.yourdogsplace.com/",
    "TrialTypes": "ELT-S, L2C, L2I, NW2",
    "EventCount": 4,
    "Latitude": 41.5296,
    "Longitude": -74.8541
  },
  {
    "Date": "2026-10-10",
    "Location": "Helena, MT",
    "Host": "Nose Work Breakfast Club",
    "EventLink": "https://noseworkbreakfastclub.com/our-events/",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 46.6274,
    "Longitude": -112.0792
  },
  {
    "Date": "2026-10-10",
    "Location": "Loveland, CO",
    "Host": "Paws 4 Thought Dog Training, LLC",
    "EventLink": "https://www.p4tnosework.com/premiumloveland",
    "TrialTypes": "NW2, NW1, L1E",
    "EventCount": 3,
    "Latitude": 40.411,
    "Longitude": -105.097
  },
  {
    "Date": "2026-10-10",
    "Location": "Sedona, AZ",
    "Host": "Successful Sniffer",
    "EventLink": "https://www.successfulsniffer.com/trials-and-events",
    "TrialTypes": "ELT, NW1, NW2",
    "EventCount": 3,
    "Latitude": 34.8594,
    "Longitude": -111.7673
  },
  {
    "Date": "2026-10-10",
    "Location": "Troy, VA",
    "Host": "Your Dogs Knows LLC",
    "EventLink": "https://yourdogknows.net/",
    "TrialTypes": "NW1, ELT-S, L1V, L2V",
    "EventCount": 4,
    "Latitude": 37.9692,
    "Longitude": -78.2863
  },
  {
    "Date": "2026-10-10",
    "Location": "Youngwood, PA",
    "Host": "Steel City Nosework, LLC",
    "EventLink": "https://www.nose-it-all.com/",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 40.2077,
    "Longitude": -79.5455
  },
  {
    "Date": "2026-10-12",
    "Location": "Swansea, MA",
    "Host": "Amy Conrad & Heaven Scent Sniffers",
    "EventLink": "https://sniffstreams.smugmug.com/Events",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 41.7796,
    "Longitude": -71.2256
  },
  {
    "Date": "2026-10-16",
    "Location": "Calhan, CO",
    "Host": "Mountain Dogs LLC",
    "EventLink": "https://mountaindogs.org/",
    "TrialTypes": "ELT, NW3, ELT-P",
    "EventCount": 3,
    "Latitude": 39.0072,
    "Longitude": -104.3283
  },
  {
    "Date": "2026-10-16",
    "Location": "Rossville, GA",
    "Host": "Camelot Shepherds, Inc.",
    "EventLink": "https://www.snifferschool.com/events",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 34.9679,
    "Longitude": -85.2715
  },
  {
    "Date": "2026-10-16",
    "Location": "Wilmington, DE",
    "Host": "Patricia Grassey",
    "EventLink": "https://thesniffinghound.com/about",
    "TrialTypes": "NW3, ELT, ELT-P",
    "EventCount": 3,
    "Latitude": 39.7835,
    "Longitude": -75.5778
  },
  {
    "Date": "2026-10-17",
    "Location": "Albuquerque, NM",
    "Host": "New Mexico Canine Scent Work, LLC",
    "EventLink": "https://www.nmcsw.com/events/#oct26",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 35.0791,
    "Longitude": -106.6751
  },
  {
    "Date": "2026-10-17",
    "Location": "Centralia, WA",
    "Host": "Let's Talk Dogs, LLC and About Face K9",
    "EventLink": "https://dorothyturley.com/trials-and-orts/",
    "TrialTypes": "ELT-S, NW2, L3C",
    "EventCount": 3,
    "Latitude": 46.7169,
    "Longitude": -122.976
  },
  {
    "Date": "2026-10-17",
    "Location": "Colebrook, CT",
    "Host": "For the Love of Dogs NY LLC",
    "EventLink": "https://www.fortheloveofdogsny.com/nacsw-trials",
    "TrialTypes": "L1E, ELT-S, NW2, ELT",
    "EventCount": 4,
    "Latitude": 41.9742,
    "Longitude": -73.1061
  },
  {
    "Date": "2026-10-17",
    "Location": "Conroe, TX",
    "Host": "Scent Work Across Texas",
    "EventLink": "https://scentworkacrosstexas.com/",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 30.2826,
    "Longitude": -95.5
  },
  {
    "Date": "2026-10-17",
    "Location": "Delevan, NY",
    "Host": "Do Over Dog Training",
    "EventLink": "https://www.dooverdogtraining.com/trials",
    "TrialTypes": "NW3, L1C, L3V",
    "EventCount": 3,
    "Latitude": 42.5196,
    "Longitude": -78.446
  },
  {
    "Date": "2026-10-17",
    "Location": "Niantic, IL",
    "Host": "Kudos for Canines, LLC",
    "EventLink": "https://kudosforcanines.com/",
    "TrialTypes": "L2C, ELT-S, NW3",
    "EventCount": 3,
    "Latitude": 39.8987,
    "Longitude": -89.1517
  },
  {
    "Date": "2026-10-17",
    "Location": "Staples, MN",
    "Host": "Nose 2 Tail Dog Training LLC",
    "EventLink": "https://nose2tail.net/nacsw-nw3-elite-2/",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 46.3789,
    "Longitude": -94.7568
  },
  {
    "Date": "2026-10-17",
    "Location": "Watsonville, CA",
    "Host": "CalCoastal Dog Owners Group",
    "EventLink": "https://cc-dog.org/",
    "TrialTypes": "L3V, L2V, L1V",
    "EventCount": 3,
    "Latitude": 36.8794,
    "Longitude": -121.7443
  },
  {
    "Date": "2026-10-24",
    "Location": "Cumming, GA",
    "Host": "Georgia Nosework",
    "EventLink": "https://georgianosework.com/",
    "TrialTypes": "NW3, L1C, NW2",
    "EventCount": 3,
    "Latitude": 34.2041,
    "Longitude": -84.171
  },
  {
    "Date": "2026-10-24",
    "Location": "Fishkill, NY",
    "Host": "Top Notch Dogs, LLC",
    "EventLink": "https://www.topnotchdogtraining.com",
    "TrialTypes": "ELT-P, ELT",
    "EventCount": 2,
    "Latitude": 41.544,
    "Longitude": -73.8609
  },
  {
    "Date": "2026-10-24",
    "Location": "Green Bay, WI",
    "Host": "NEWk9Scent Work LLC",
    "EventLink": "https://newk9scentwork.com/nose-work-trials-2",
    "TrialTypes": "NW2, ELT",
    "EventCount": 2,
    "Latitude": 44.544,
    "Longitude": -88.0035
  },
  {
    "Date": "2026-10-24",
    "Location": "Norton, MA",
    "Host": "Dogs Make Scents",
    "EventLink": "https://dogsmakescents.com/events/",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 41.9925,
    "Longitude": -71.2271
  },
  {
    "Date": "2026-10-24",
    "Location": "Penn Yan, NY",
    "Host": "2 Psyched 4 Dogs",
    "EventLink": "https://2psyched4dogs.com/",
    "TrialTypes": "ELT, ELT-S, NW2",
    "EventCount": 3,
    "Latitude": 42.6134,
    "Longitude": -77.008
  },
  {
    "Date": "2026-10-24",
    "Location": "Reedsport, OR",
    "Host": "Wells Creek Dog Training",
    "EventLink": "https://wellscreekdogtraining.com/",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 43.7398,
    "Longitude": -124.0791
  },
  {
    "Date": "2026-10-26",
    "Location": "Stockton, CA",
    "Host": "Two Nosey Girls",
    "EventLink": "https://twonoseygirls.com/",
    "TrialTypes": "L3E, L2E",
    "EventCount": 2,
    "Latitude": 37.9812,
    "Longitude": -121.2783
  },
  {
    "Date": "2026-10-30",
    "Location": "Cameron Park, CA",
    "Host": "Sierra Sniffing Canines, Inc",
    "EventLink": "https://sierrasniffingcanines.org/",
    "TrialTypes": "NW1, NW3",
    "EventCount": 2,
    "Latitude": 38.6413,
    "Longitude": -120.9861
  },
  {
    "Date": "2026-10-30",
    "Location": "Harrington, DE",
    "Host": "Shamrock Pot Of Gold K9 Scenter",
    "EventLink": "https://shamrockpotofgoldk9scenter.com/",
    "TrialTypes": "ELT-S, L2E, NW3, ELT, NW1",
    "EventCount": 5,
    "Latitude": 38.9624,
    "Longitude": -75.5975
  },
  {
    "Date": "2026-10-30",
    "Location": "Honey Brook, PA",
    "Host": "Patricia Grassey",
    "EventLink": "https://thesniffinghound.com/about",
    "TrialTypes": "NW1, L1C, L2C, NW2, L3I, L3C",
    "EventCount": 6,
    "Latitude": 40.0793,
    "Longitude": -75.8716
  },
  {
    "Date": "2026-10-30",
    "Location": "Lakeville, MN",
    "Host": "St Paul Dog Training Club",
    "EventLink": "https://spdtc.com/events-at-spdtc/",
    "TrialTypes": "ELT-P, NW2, ELT-S, L1C",
    "EventCount": 4,
    "Latitude": 44.617,
    "Longitude": -93.2238
  },
  {
    "Date": "2026-10-30",
    "Location": "Lawrenceville, GA",
    "Host": "Chestnut Hill Canine Sports",
    "EventLink": "http://chestnuthillcaninesports.com/lawrenceville-2025/",
    "TrialTypes": "NW3, NW1, L2I",
    "EventCount": 3,
    "Latitude": 33.9285,
    "Longitude": -84.0241
  },
  {
    "Date": "2026-10-30",
    "Location": "Montrose, CO",
    "Host": "Mountain Dogs LLC",
    "EventLink": "https://mountaindogs.org/",
    "TrialTypes": "ELT, ELT-S, NW2",
    "EventCount": 3,
    "Latitude": 38.4496,
    "Longitude": -107.8971
  },
  {
    "Date": "2026-10-30",
    "Location": "York, PA",
    "Host": "Firezone GS",
    "EventLink": "https://www.firezonegiantschnauzers.com/nose-work-trials",
    "TrialTypes": "ELT, NW3, ELT-P",
    "EventCount": 3,
    "Latitude": 39.9735,
    "Longitude": -76.7522
  },
  {
    "Date": "2026-10-31",
    "Location": "Beloit, WI",
    "Host": "George Carpenter",
    "EventLink": "https://gscarpenter.wixsite.com/scwnw/trials",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 42.5065,
    "Longitude": -89.0641
  },
  {
    "Date": "2026-10-31",
    "Location": "Bonham, TX",
    "Host": "All About The Nose",
    "EventLink": "https://www.allaboutthenose.com/",
    "TrialTypes": "NW2, NW1",
    "EventCount": 2,
    "Latitude": 33.5729,
    "Longitude": -96.1675
  },
  {
    "Date": "2026-10-31",
    "Location": "Franklin, GA",
    "Host": "Hold The Line K9 LLC",
    "EventLink": "https://www.holdthelinek9nosework.com/",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 34.328,
    "Longitude": -83.2053
  },
  {
    "Date": "2026-10-31",
    "Location": "Kennebunkport, ME",
    "Host": "Elizabeth Dutton",
    "EventLink": "https://ehdutton.wordpress.com/",
    "TrialTypes": "NW3, ELT-P",
    "EventCount": 2,
    "Latitude": 43.4017,
    "Longitude": -70.4323
  },
  {
    "Date": "2026-10-31",
    "Location": "Plant City, FL",
    "Host": "Hoppin’ in the Hills",
    "EventLink": "https://hoppininthehillscom.wordpress.com",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 28.015,
    "Longitude": -82.0922
  },
  {
    "Date": "2026-10-31",
    "Location": "Sturgis, SD",
    "Host": "Two Paws Up Dog Training, LLC",
    "EventLink": "https://www.twopawsupdogtrainingllc.com/",
    "TrialTypes": "L1V, L2E, L2V, L1E",
    "EventCount": 4,
    "Latitude": 44.3929,
    "Longitude": -103.484
  },
  {
    "Date": "2026-10-31",
    "Location": "White Plains, NY",
    "Host": "Saints2Source, LLC",
    "EventLink": "https://www.saints2source.com/copy-of-new-city-ny-oct-2025",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 41.0135,
    "Longitude": -73.8079
  },
  {
    "Date": "2026-10-31",
    "Location": "Yamhill, OR",
    "Host": "Nose Work Detectives, LLC",
    "EventLink": "https://noseworkdetectives.com/",
    "TrialTypes": "ELT-P",
    "EventCount": 1,
    "Latitude": 45.239,
    "Longitude": -123.2518
  },
  {
    "Date": "2026-11-01",
    "Location": "San Martin, CA",
    "Host": "B.L. McMutts LLC",
    "EventLink": "https://blmcmutts.com/events/nacsw-element-specialty-trial-nov26",
    "TrialTypes": "L1V, L2V",
    "EventCount": 2,
    "Latitude": 37.0515,
    "Longitude": -121.5474
  },
  {
    "Date": "2026-11-03",
    "Location": "Ventura, CA",
    "Host": "Pink Biscuit K9s",
    "EventLink": "https://www.pinkbiscuitk9s.com/arnaz-25-premium.html",
    "TrialTypes": "NW1, NW2, ELT-P",
    "EventCount": 3,
    "Latitude": 34.4837,
    "Longitude": -119.0773
  },
  {
    "Date": "2026-11-06",
    "Location": "Rome, GA",
    "Host": "Georgia Nosework, LLC",
    "EventLink": "https://georgianosework.com/events/",
    "TrialTypes": "NW3, ELT, NW1, NW2",
    "EventCount": 4,
    "Latitude": 34.2596,
    "Longitude": -85.127
  },
  {
    "Date": "2026-11-07",
    "Location": "Bonner Springs, KS",
    "Host": "Brookside Pet Concierge",
    "EventLink": "https://bksdogtraining.com/",
    "TrialTypes": "NW1, NW2, NW3",
    "EventCount": 3,
    "Latitude": 39.0402,
    "Longitude": -94.8639
  },
  {
    "Date": "2026-11-07",
    "Location": "Colorado Springs, CO",
    "Host": "Beyond Elevation K9",
    "EventLink": "https://www.beyondelevationk9.com/",
    "TrialTypes": "NW3, NW2, NW1",
    "EventCount": 3,
    "Latitude": 38.8121,
    "Longitude": -104.7789
  },
  {
    "Date": "2026-11-07",
    "Location": "Geneva, IL",
    "Host": "For Your K9, Inc",
    "EventLink": "http://www.foryourk9.com/",
    "TrialTypes": "ELT, NW1, NW2",
    "EventCount": 3,
    "Latitude": 41.9076,
    "Longitude": -88.2679
  },
  {
    "Date": "2026-11-07",
    "Location": "Guerneville , CA",
    "Host": "Jen Huot",
    "EventLink": "https://k9noseworkacademy.com/",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 38.5514,
    "Longitude": -122.9689
  },
  {
    "Date": "2026-11-07",
    "Location": "Las Vegas, NV",
    "Host": "imPETus Animal Training",
    "EventLink": "https://www.impetusanimaltraining.com/",
    "TrialTypes": "NW3, NW1, L1C",
    "EventCount": 3,
    "Latitude": 36.197,
    "Longitude": -115.1228
  },
  {
    "Date": "2026-11-07",
    "Location": "Mays Landing, NJ",
    "Host": "Rotts-n-Notts Nosework LLC",
    "EventLink": "https://www.rottsnnottsnosework.com/",
    "TrialTypes": "L1C, NW2, L1E, NW1",
    "EventCount": 4,
    "Latitude": 39.4209,
    "Longitude": -74.6786
  },
  {
    "Date": "2026-11-07",
    "Location": "Wappingers Falls, NY",
    "Host": "Top Notch Dogs, LLC",
    "EventLink": "https://www.topnotchdogtraining.com/",
    "TrialTypes": "L3C, NW2, ELT",
    "EventCount": 3,
    "Latitude": 41.554,
    "Longitude": -73.8768
  },
  {
    "Date": "2026-11-07",
    "Location": "Woodward, IA",
    "Host": "KBP Dog Training",
    "EventLink": "https://kbpdogtraining.com/",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 41.9011,
    "Longitude": -93.9129
  },
  {
    "Date": "2026-11-09",
    "Location": "West Berlin, NJ",
    "Host": "For the Love of Dogs NY LLC",
    "EventLink": "https://www.fortheloveofdogsny.com/trials-events",
    "TrialTypes": "L1V, L2E, NW3, ELT-P",
    "EventCount": 4,
    "Latitude": 39.8478,
    "Longitude": -74.904
  },
  {
    "Date": "2026-11-11",
    "Location": "Petaluma, CA",
    "Host": "Marin Humane",
    "EventLink": "https://marinhumane.org/oh-behave/nose-work-events/",
    "TrialTypes": "ELT-P, ELT",
    "EventCount": 2,
    "Latitude": 38.2196,
    "Longitude": -122.5868
  },
  {
    "Date": "2026-11-13",
    "Location": "Chula Vista, CA",
    "Host": "Rewarding Rover LLC, Uberdog, & Claire Brocato",
    "EventLink": "https://www.rewardingrover.com/",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 32.6831,
    "Longitude": -117.1003
  },
  {
    "Date": "2026-11-13",
    "Location": "Gilbertsville, PA",
    "Host": "Sniff Sniff Hooray",
    "EventLink": "https://sniffsniffhooray.com/events",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 40.3407,
    "Longitude": -75.5998
  },
  {
    "Date": "2026-11-13",
    "Location": "Ypsilanti, MI",
    "Host": "Every Dog Nosework",
    "EventLink": "https://everydognosework.com/trials",
    "TrialTypes": "ELT, ELT-S, L2C",
    "EventCount": 3,
    "Latitude": 42.1964,
    "Longitude": -83.6406
  },
  {
    "Date": "2026-11-14",
    "Location": "Greer, SC",
    "Host": "Trained to Trust, LLC",
    "EventLink": "http://www.k9trainedtotrust.com/",
    "TrialTypes": "NW3, L2V, NW1",
    "EventCount": 3,
    "Latitude": 34.9727,
    "Longitude": -82.2591
  },
  {
    "Date": "2026-11-14",
    "Location": "Montgomery, AL",
    "Host": "By A Nose Nosework",
    "EventLink": "https://www.byanosenosework.com/event-details/montgomery-al-elt-nw3",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 32.3376,
    "Longitude": -86.3486
  },
  {
    "Date": "2026-11-14",
    "Location": "Waymart, PA",
    "Host": "Your Dog's Place, LLC",
    "EventLink": "http://www.yourdogsplace.com/",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 41.6113,
    "Longitude": -75.3601
  },
  {
    "Date": "2026-11-16",
    "Location": "Ellicott City, MD",
    "Host": "Red Huskies",
    "EventLink": "https://nosework.redhuskies.com/",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 39.2629,
    "Longitude": -76.8531
  },
  {
    "Date": "2026-11-16",
    "Location": "Hartford, CT",
    "Host": "For the Love of Dogs NY LLC",
    "EventLink": "https://www.fortheloveofdogsny.com/trials-events",
    "TrialTypes": "ELT, ELT-S, L1I",
    "EventCount": 3,
    "Latitude": 41.8129,
    "Longitude": -72.6685
  },
  {
    "Date": "2026-11-17",
    "Location": "Monkton, MD",
    "Host": "Firezone GS",
    "EventLink": "https://firezonegiantschnauzers.com/nose-work-trials",
    "TrialTypes": "NW1, NW2, NW3",
    "EventCount": 3,
    "Latitude": 39.5355,
    "Longitude": -76.6223
  },
  {
    "Date": "2026-11-20",
    "Location": "Boring, OR",
    "Host": "Trust Your Dog K9 Events",
    "EventLink": "https://trustyourdogk9events.com/",
    "TrialTypes": "NW2, NW3, ELT",
    "EventCount": 3,
    "Latitude": 45.4652,
    "Longitude": -122.357
  },
  {
    "Date": "2026-11-20",
    "Location": "Centreville, MD",
    "Host": "Fair Play Point Labradors",
    "EventLink": "https://www.fairplaylabradors.com/",
    "TrialTypes": "SMT, ELT-S, L1I",
    "EventCount": 3,
    "Latitude": 39.0191,
    "Longitude": -76.0795
  },
  {
    "Date": "2026-11-20",
    "Location": "Denver, PA",
    "Host": "Patricia Grassey",
    "EventLink": "https://thesniffinghound.com/",
    "TrialTypes": "ELT, ELT-P, ELT-S, L3V",
    "EventCount": 4,
    "Latitude": 40.2534,
    "Longitude": -76.1652
  },
  {
    "Date": "2026-11-20",
    "Location": "Kintnersville , PA",
    "Host": "Paws n' Sniff",
    "EventLink": "http://www.pawsnsniff.com/",
    "TrialTypes": "NW1, L1C, L1E, L1I",
    "EventCount": 4,
    "Latitude": 40.5506,
    "Longitude": -75.1576
  },
  {
    "Date": "2026-11-20",
    "Location": "Lompoc, CA",
    "Host": "Gentle Touch Pet Training",
    "EventLink": "https://www.gentlepets.com/gtpt-events/nacsw%E2%84%A2-nw3",
    "TrialTypes": "NW3, NW2, NW1",
    "EventCount": 3,
    "Latitude": 34.6745,
    "Longitude": -120.4572
  },
  {
    "Date": "2026-11-20",
    "Location": "Loranger, LA",
    "Host": "Dog Gone Right",
    "EventLink": "http://www.doggoneright.net/",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 30.6687,
    "Longitude": -90.4051
  },
  {
    "Date": "2026-11-21",
    "Location": "Centralia, WA",
    "Host": "Let's Talk Dogs, LLC & About Face K9 Academy",
    "EventLink": "https://www.aboutfacek9academy.com/",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 46.7347,
    "Longitude": -122.998
  },
  {
    "Date": "2026-11-21",
    "Location": "DeLeon Springs, FL",
    "Host": "River Poodles Training, LLC",
    "EventLink": "https://riverpoodlestraining.com/",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 29.0933,
    "Longitude": -81.3592
  },
  {
    "Date": "2026-11-21",
    "Location": "Delta, CO",
    "Host": "Mountain Dogs LLC",
    "EventLink": "https://mountaindogs.org/",
    "TrialTypes": "ELT, NW3, NW1, NW2",
    "EventCount": 4,
    "Latitude": 38.8707,
    "Longitude": -107.8685
  },
  {
    "Date": "2026-11-21",
    "Location": "Fork Union, VA",
    "Host": "Your Dog Knows, LLC",
    "EventLink": "https://yourdogknows.net/",
    "TrialTypes": "ELT, L2I, NW2",
    "EventCount": 3,
    "Latitude": 37.8124,
    "Longitude": -78.2956
  },
  {
    "Date": "2026-11-21",
    "Location": "Marble Falls, TX",
    "Host": "Scent Work Across Texas",
    "EventLink": "https://scentworkacrosstexas.com/",
    "TrialTypes": "ELT-S, L2I, NW3",
    "EventCount": 3,
    "Latitude": 30.6252,
    "Longitude": -98.2293
  },
  {
    "Date": "2026-11-21",
    "Location": "Ontario, CA",
    "Host": "Agile Paws Dog Sports",
    "EventLink": "https://agilepawsdogsports.com/",
    "TrialTypes": "NW1, L3C, L3I",
    "EventCount": 3,
    "Latitude": 34.0494,
    "Longitude": -117.6926
  },
  {
    "Date": "2026-11-21",
    "Location": "Smyrna, TN",
    "Host": "Dogs Have Amazing Noses, LLC",
    "EventLink": "https://dogshaveamazingnoses.com/events/",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 35.9821,
    "Longitude": -86.5529
  },
  {
    "Date": "2026-11-22",
    "Location": "Wilbraham, MA",
    "Host": "Heaven Scent Sniffers",
    "EventLink": "https://www.heavenscentsniffers.com/",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 42.0901,
    "Longitude": -72.4434
  },
  {
    "Date": "2026-11-27",
    "Location": "Elizabeth, CO",
    "Host": "Beyond Elevation K9",
    "EventLink": "https://www.beyondelevationk9.com/",
    "TrialTypes": "NW2, NW3",
    "EventCount": 2,
    "Latitude": 39.3868,
    "Longitude": -104.6065
  },
  {
    "Date": "2026-11-27",
    "Location": "Long Beach, CA",
    "Host": "JavaK9s, LLC",
    "EventLink": "http://www.javak9s.com/",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 33.8061,
    "Longitude": -118.2249
  },
  {
    "Date": "2026-11-27",
    "Location": "San Jose, CA",
    "Host": "The Bay Team",
    "EventLink": "https://www.bayteam.org/",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 37.3299,
    "Longitude": -121.8979
  },
  {
    "Date": "2026-11-28",
    "Location": "Cottage Grove, MN",
    "Host": "Gretchen Hofheins-Wackerfuss",
    "EventLink": "https://www.sniffingminpin.com/",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 44.7957,
    "Longitude": -92.952
  },
  {
    "Date": "2026-11-28",
    "Location": "Cumming, GA",
    "Host": "Georgia Nosework",
    "EventLink": "https://georgianosework.com/",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 34.2121,
    "Longitude": -84.1716
  },
  {
    "Date": "2026-11-28",
    "Location": "Lebanon, NJ",
    "Host": "Sirius K9 Solutions",
    "EventLink": "http://www.siriusk9solutions.net/NoseWork.html",
    "TrialTypes": "NW2, ELT-P",
    "EventCount": 2,
    "Latitude": 40.6829,
    "Longitude": -74.8612
  },
  {
    "Date": "2026-11-28",
    "Location": "Mifflinburg, PA",
    "Host": "Paws-itively Obedient Dog Training School",
    "EventLink": "https://pawsitivelyobedient.net/",
    "TrialTypes": "NW3, NW1",
    "EventCount": 2,
    "Latitude": 40.8698,
    "Longitude": -77.0584
  },
  {
    "Date": "2026-11-28",
    "Location": "Silex, MO",
    "Host": "WestInn Kennels",
    "EventLink": "https://westinnkennels.wixsite.com/silex",
    "TrialTypes": "NW1, NW2, NW3",
    "EventCount": 3,
    "Latitude": 39.1631,
    "Longitude": -91.057
  },
  {
    "Date": "2026-11-28",
    "Location": "Vancouver, WA",
    "Host": "Sniffketeers",
    "EventLink": "https://noseworktrial.blogspot.com/",
    "TrialTypes": "NW1, L1E, L1I, L3V",
    "EventCount": 4,
    "Latitude": 45.6237,
    "Longitude": -122.6289
  },
  {
    "Date": "2026-11-29",
    "Location": "Gettysburg, PA",
    "Host": "Firezone GS",
    "EventLink": "https://www.firezonegiantschnauzers.com/nose-work-trials",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 39.8329,
    "Longitude": -77.2229
  },
  {
    "Date": "2026-12-05",
    "Location": "Centralia, WA",
    "Host": "Let's Talk Dogs, LLC and About Face K9",
    "EventLink": "http://www.dorothyturley.com/",
    "TrialTypes": "ELT, NW1, L2I",
    "EventCount": 3,
    "Latitude": 46.6955,
    "Longitude": -123.0007
  },
  {
    "Date": "2026-12-05",
    "Location": "Charlton, MA",
    "Host": "HeavenScent Sniffers",
    "EventLink": "https://www.heavenscentsniffers.com/",
    "TrialTypes": "NW2, NW1",
    "EventCount": 2,
    "Latitude": 42.1057,
    "Longitude": -71.9262
  },
  {
    "Date": "2026-12-05",
    "Location": "Fillmore, CA",
    "Host": "Pink Biscuit K9s",
    "EventLink": "https://www.pinkbiscuitk9s.com/",
    "TrialTypes": "NW3, ELT-S, L2C",
    "EventCount": 3,
    "Latitude": 34.381,
    "Longitude": -118.8792
  },
  {
    "Date": "2026-12-05",
    "Location": "Fredonia, WI",
    "Host": "On Point Elite Dog Sports, LLC",
    "EventLink": "https://www.opedogsports.com/nacsw-trials",
    "TrialTypes": "NW1, NW2, NW3",
    "EventCount": 3,
    "Latitude": 43.4345,
    "Longitude": -87.9318
  },
  {
    "Date": "2026-12-05",
    "Location": "Hoover, AL",
    "Host": "Southeast Scent Work Alliance, LLC (SSWA)",
    "EventLink": "https://www.southeastscent.com/nw3-elite-hoover-al/",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 33.3005,
    "Longitude": -86.8469
  },
  {
    "Date": "2026-12-05",
    "Location": "Hubertus, WI",
    "Host": "Loving Paws Dog Training, LLC",
    "EventLink": "https://www.lovingpawsllc.com/premium-elt-elt",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 43.2397,
    "Longitude": -88.2087
  },
  {
    "Date": "2026-12-05",
    "Location": "Newfoundland, PA",
    "Host": "Your Dog's Place, LLC",
    "EventLink": "http://www.yourdogsplace.com/",
    "TrialTypes": "L2V, L3C, NW3",
    "EventCount": 3,
    "Latitude": 41.3021,
    "Longitude": -75.3472
  },
  {
    "Date": "2026-12-05",
    "Location": "Tucson, AZ",
    "Host": "Patience Unlimited Dog Training",
    "EventLink": "http://www.patienceunlimited.com/nacsw.html",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 32.2701,
    "Longitude": -110.9682
  },
  {
    "Date": "2026-12-07",
    "Location": "Stockton, CA",
    "Host": "Two Nosey Girls",
    "EventLink": "https://www.twonoseygirls.com/events.html",
    "TrialTypes": "ELT, ELT-S, L3I",
    "EventCount": 3,
    "Latitude": 38.0061,
    "Longitude": -121.3171
  },
  {
    "Date": "2026-12-11",
    "Location": "Douglassville, PA",
    "Host": "Patricia Grassey",
    "EventLink": "https://www.thesniffinghound.com/",
    "TrialTypes": "NW3, NW2, ELT",
    "EventCount": 3,
    "Latitude": 40.2461,
    "Longitude": -75.7401
  },
  {
    "Date": "2026-12-12",
    "Location": "Obetz, OH",
    "Host": "CleverDogs",
    "EventLink": "https://cleverdogsohio.com/",
    "TrialTypes": "NW1, NW2, ELT",
    "EventCount": 3,
    "Latitude": 39.9031,
    "Longitude": -83.0005
  },
  {
    "Date": "2026-12-12",
    "Location": "Sauget, IL",
    "Host": "Happy Dog Concepts, LLC",
    "EventLink": "https://happydogconcepts.com/events",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 38.5567,
    "Longitude": -90.1442
  },
  {
    "Date": "2026-12-13",
    "Location": "McMinnville, OR",
    "Host": "Carol Forsberg and Doglandia, LLC",
    "EventLink": "https://www.justnosework.com",
    "TrialTypes": "NW2, ELT",
    "EventCount": 2,
    "Latitude": 45.1799,
    "Longitude": -123.1707
  },
  {
    "Date": "2026-12-15",
    "Location": "Redlands, CA",
    "Host": "Agile Paws Dog Sports",
    "EventLink": "https://agilepawsdogsports.com/",
    "TrialTypes": "ELT, ELT-S",
    "EventCount": 2,
    "Latitude": 34.0415,
    "Longitude": -117.2141
  },
  {
    "Date": "2026-12-18",
    "Location": "Pittstown, NJ",
    "Host": "Shamrock Pot Of Gold K9 Scenter",
    "EventLink": "https://shamrockpotofgoldk9scenter.com/",
    "TrialTypes": "ELT-S, L1C, NW3, ELT",
    "EventCount": 4,
    "Latitude": 40.5485,
    "Longitude": -75.0085
  },
  {
    "Date": "2026-12-19",
    "Location": "Alpharetta, GA",
    "Host": "Georgia Nosework",
    "EventLink": "https://georgianosework.com/",
    "TrialTypes": "SMT, ELT-P",
    "EventCount": 2,
    "Latitude": 34.0496,
    "Longitude": -84.2834
  },
  {
    "Date": "2026-12-19",
    "Location": "Imperial Beach, CA",
    "Host": "Rewarding Rover LLC/Uber dog/Claire Brocato",
    "EventLink": "https://www.rewardingrover.com/",
    "TrialTypes": "ELT, L1E, NW1",
    "EventCount": 3,
    "Latitude": 32.5712,
    "Longitude": -117.1443
  },
  {
    "Date": "2026-12-27",
    "Location": "Hartsdale, NY",
    "Host": "For the Love of Dogs NY LLC",
    "EventLink": "https://www.fortheloveofdogsny.com/trials-events",
    "TrialTypes": "NW3, ELT, L3I, L2I",
    "EventCount": 4,
    "Latitude": 40.9989,
    "Longitude": -73.7971
  },
  {
    "Date": "2026-12-28",
    "Location": "Phoenix, AZ",
    "Host": "Release Canine LLC",
    "EventLink": "https://www.releasecanine.com/nacsw",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 33.4397,
    "Longitude": -112.1182
  },
  {
    "Date": "2026-12-28",
    "Location": "Tyngsborough, MA",
    "Host": "Spot On K9 Coaching",
    "EventLink": "https://www.sniffalertfinish.com/",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 42.6277,
    "Longitude": -71.4116
  },
  {
    "Date": "2026-12-29",
    "Location": "Duluth, GA",
    "Host": "Right Choice Dog Training, LLC",
    "EventLink": "https://www.rightchoicedogtraining.net/eventandvolunteer",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 33.9765,
    "Longitude": -84.1715
  },
  {
    "Date": "2026-12-31",
    "Location": "Corvallis, OR",
    "Host": "PNW Sniffers",
    "EventLink": "https://pnwsniffers.com/nye-trial",
    "TrialTypes": "NW3, NW1",
    "EventCount": 2,
    "Latitude": 44.5667,
    "Longitude": -123.2736
  },
  {
    "Date": "2027-01-01",
    "Location": "Santa Rosa, CA",
    "Host": "Marin Humane",
    "EventLink": "https://marinhumane.org/oh-behave/nose-work-events/",
    "TrialTypes": "NW3, SMT",
    "EventCount": 2,
    "Latitude": 38.4254,
    "Longitude": -122.7228
  },
  {
    "Date": "2027-01-02",
    "Location": "Bonsall, CA",
    "Host": "Linda Buchanan",
    "EventLink": "https://www.k9slovetosearch.com/",
    "TrialTypes": "ELT, NW2",
    "EventCount": 2,
    "Latitude": 33.2556,
    "Longitude": -117.2492
  },
  {
    "Date": "2027-01-03",
    "Location": "Bee Cave, TX",
    "Host": "Scent Work Across Texas",
    "EventLink": "https://scentworkacrosstexas.com/",
    "TrialTypes": "ELT-S, NW1, NW3",
    "EventCount": 3,
    "Latitude": 30.3082,
    "Longitude": -97.9598
  },
  {
    "Date": "2027-01-09",
    "Location": "Bellingham, WA",
    "Host": "The Nosework Magic",
    "EventLink": "https://www.noseworkmagic.com/",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 48.7982,
    "Longitude": -122.4477
  },
  {
    "Date": "2027-01-09",
    "Location": "Cape Coral, FL",
    "Host": "Your Dog Knows LLC",
    "EventLink": "https://yourdogknows.net/",
    "TrialTypes": "NW1, NW2, L1I, L1C",
    "EventCount": 4,
    "Latitude": 26.6062,
    "Longitude": -81.9378
  },
  {
    "Date": "2027-01-09",
    "Location": "Novato, CA",
    "Host": "Marin Humane",
    "EventLink": "https://marinhumane.org/oh-behave/nose-work-events/",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 38.059,
    "Longitude": -122.5773
  },
  {
    "Date": "2027-01-15",
    "Location": "Upland, CA",
    "Host": "Agile Paws Dog Sports",
    "EventLink": "https://agilepawsdogsports.com/",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 34.1264,
    "Longitude": -117.6318
  },
  {
    "Date": "2027-01-16",
    "Location": "Melrose, FL",
    "Host": "River Poodles Training, LLC",
    "EventLink": "https://riverpoodlestraining.com/",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 29.6906,
    "Longitude": -82.078
  },
  {
    "Date": "2027-01-19",
    "Location": "Murfreesboro, TN",
    "Host": "Dogs Have Amazing Noses, LLC",
    "EventLink": "https://dogshaveamazingnoses.com/events/",
    "TrialTypes": "ELT, ELT-S",
    "EventCount": 2,
    "Latitude": 35.8903,
    "Longitude": -86.4184
  },
  {
    "Date": "2027-01-23",
    "Location": "Montgomery, TX",
    "Host": "Nosy Dogs Houston",
    "EventLink": "http://www.nosydogshouston.com/",
    "TrialTypes": "NW1, L1C, L1V, NW2",
    "EventCount": 4,
    "Latitude": 30.3319,
    "Longitude": -95.4871
  },
  {
    "Date": "2027-01-23",
    "Location": "Valencia, CA",
    "Host": "Pink Biscuit K9s",
    "EventLink": "https://www.pinkbiscuitk9s.com/",
    "TrialTypes": "ELT-P, L2I, L3C",
    "EventCount": 3,
    "Latitude": 34.4101,
    "Longitude": -118.6073
  },
  {
    "Date": "2027-01-30",
    "Location": "Durham, NC",
    "Host": "Whole Dog Institute",
    "EventLink": "https://wholedoginstitute.com/",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 36.0034,
    "Longitude": -78.9275
  },
  {
    "Date": "2027-01-30",
    "Location": "Las Vegas, NV",
    "Host": "imPETus Animal Training",
    "EventLink": "http://impetusanimaltraining.com/",
    "TrialTypes": "NW3, L1I, NW2",
    "EventCount": 3,
    "Latitude": 36.2056,
    "Longitude": -115.1773
  },
  {
    "Date": "2027-01-30",
    "Location": "Petaluma, CA",
    "Host": "Seaside Sniffers",
    "EventLink": "https://www.seasidesniffers.com/",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 38.2123,
    "Longitude": -122.5982
  },
  {
    "Date": "2027-01-30",
    "Location": "San Marcos, CA",
    "Host": "Rewarding Rover LLC, Uberdog, & Claire Brocato",
    "EventLink": "https://www.rewardingrover.com/",
    "TrialTypes": "ELT-S, NW3",
    "EventCount": 2,
    "Latitude": 33.1785,
    "Longitude": -117.1939
  },
  {
    "Date": "2027-01-30",
    "Location": "Seguin, TX",
    "Host": "Sniff Happens",
    "EventLink": "https://www.sniffhappenstx.com/Jan-NACSW-Trial",
    "TrialTypes": "L2C, ELT-S, NW3",
    "EventCount": 3,
    "Latitude": 29.523,
    "Longitude": -97.9754
  },
  {
    "Date": "2027-02-06",
    "Location": "Murfreesboro, TN",
    "Host": "Dogs Have Amazing Noses, LLC",
    "EventLink": "https://dogshaveamazingnoses.com/events/",
    "TrialTypes": "NW2",
    "EventCount": 1,
    "Latitude": 35.8572,
    "Longitude": -86.389
  },
  {
    "Date": "2027-02-13",
    "Location": "Modesto, CA",
    "Host": "Two Nosey Girls",
    "EventLink": "https://twonoseygirls.com/",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 37.609,
    "Longitude": -121.0392
  },
  {
    "Date": "2027-02-21",
    "Location": "Benson, AZ",
    "Host": "Patience Unlimited Dog Training",
    "EventLink": "http://www.patienceunlimited.com/",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 31.9388,
    "Longitude": -110.2453
  },
  {
    "Date": "2027-02-22",
    "Location": "Paso Robles, CA",
    "Host": "Gentle Touch Pet Training",
    "EventLink": "https://www.gentlepets.com/gtpt-events/nacsw%E2%84%A2-elt%2Felt-trials",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 35.5773,
    "Longitude": -120.6446
  },
  {
    "Date": "2027-02-26",
    "Location": "Westlake Village, CA",
    "Host": "JavaK9s, LLC",
    "EventLink": "http://www.javak9s.com/",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 34.1784,
    "Longitude": -118.7602
  },
  {
    "Date": "2027-03-06",
    "Location": "Keystone Heights, FL",
    "Host": "River Poodles Training, LLC",
    "EventLink": "https://riverpoodlestraining.com/",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 29.7717,
    "Longitude": -82.0571
  },
  {
    "Date": "2027-03-08",
    "Location": "Glendora, CA",
    "Host": "Agile Paws Dog Sports",
    "EventLink": "https://agilepawsdogsports.com/",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 34.1442,
    "Longitude": -117.8546
  },
  {
    "Date": "2027-03-13",
    "Location": "Rome, GA",
    "Host": "Southeast Scent Work Alliance, LLC (SSWA)",
    "EventLink": "https://southeastscent.com/events",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 34.2378,
    "Longitude": -85.1799
  },
  {
    "Date": "2027-03-15",
    "Location": "Riverside, CA",
    "Host": "Linda Buchanan",
    "EventLink": "https://www.k9slovetosearch.com/",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 34.0246,
    "Longitude": -117.3796
  },
  {
    "Date": "2027-03-20",
    "Location": "Foxboro, MA",
    "Host": "Bay State Sniffers",
    "EventLink": "http://www.baystatesniffers.com/",
    "TrialTypes": "NW1, L1C, L1I",
    "EventCount": 3,
    "Latitude": 42.139,
    "Longitude": -71.2902
  },
  {
    "Date": "2027-03-20",
    "Location": "Redwood City, CA",
    "Host": "B. L. McMutts, LLC",
    "EventLink": "https://blmcmutts.com/",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 37.4688,
    "Longitude": -122.2544
  },
  {
    "Date": "2027-03-20",
    "Location": "Selma, TX",
    "Host": "Sniff Happens",
    "EventLink": "https://www.sniffhappenstx.com/",
    "TrialTypes": "L1E, ELT-S, ELT",
    "EventCount": 3,
    "Latitude": 29.6162,
    "Longitude": -98.2669
  },
  {
    "Date": "2027-03-26",
    "Location": "Albuquerque, NM",
    "Host": "New Mexico Canine Scent Work, LLC",
    "EventLink": "https://www.nmcsw.com/",
    "TrialTypes": "ELT, NW3, L2I, NW1",
    "EventCount": 4,
    "Latitude": 35.0973,
    "Longitude": -106.6939
  },
  {
    "Date": "2027-04-03",
    "Location": "Keystone Heights, FL",
    "Host": "River Poodles Training, LLC",
    "EventLink": "https://riverpoodlestraining.com/",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 29.756,
    "Longitude": -82.0686
  },
  {
    "Date": "2027-04-03",
    "Location": "Wilmot, WI",
    "Host": "SuperDog Industries LLC",
    "EventLink": "https://www.superdogevents.com/",
    "TrialTypes": "NW2, ELT",
    "EventCount": 2,
    "Latitude": 42.514,
    "Longitude": -88.2171
  },
  {
    "Date": "2027-04-05",
    "Location": "Chester, NY",
    "Host": "For the Love of Dogs NY LLC",
    "EventLink": "https://www.fortheloveofdogsny.com/trials-events",
    "TrialTypes": "NW2, ELT",
    "EventCount": 2,
    "Latitude": 41.3323,
    "Longitude": -74.2679
  },
  {
    "Date": "2027-04-07",
    "Location": "Olympia, WA",
    "Host": "Let's Talk Dogs, LLC & About Face K9 Academy",
    "EventLink": "https://www.aboutfacek9academy.com",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 47.0691,
    "Longitude": -122.8841
  },
  {
    "Date": "2027-04-10",
    "Location": "Northfield, OH",
    "Host": "Nosework Addicts, LLC",
    "EventLink": "https://www.noseworkaddictsllc.com/",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 41.3181,
    "Longitude": -81.556
  },
  {
    "Date": "2027-04-16",
    "Location": "Upland, CA",
    "Host": "Agile Paws Dog Sports",
    "EventLink": "https://agilepawsdogsports.com/",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 34.0661,
    "Longitude": -117.6392
  },
  {
    "Date": "2027-04-17",
    "Location": "Glenwood, NY",
    "Host": "Do Over Dog Training",
    "EventLink": "https://www.dooverdogtraining.com/trials",
    "TrialTypes": "NW1, L2E, L1C, L1I",
    "EventCount": 4,
    "Latitude": 42.6523,
    "Longitude": -78.6198
  },
  {
    "Date": "2027-04-24",
    "Location": "Ellicottville, NY",
    "Host": "Do Over Dog Training",
    "EventLink": "https://www.dooverdogtraining.com/trials",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 42.2752,
    "Longitude": -78.6328
  },
  {
    "Date": "2027-05-22",
    "Location": "Amherst, NY",
    "Host": "Do Over Dog Training",
    "EventLink": "https://www.dooverdogtraining.com/trials",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 42.9923,
    "Longitude": -78.814
  },
  {
    "Date": "2027-06-12",
    "Location": "Portland, OR",
    "Host": "Trust Your Dog K9 Events",
    "EventLink": "https://trustyourdogk9events.com/",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 45.4922,
    "Longitude": -122.6697
  },
  {
    "Date": "2027-06-19",
    "Location": "Spring Grove, IL",
    "Host": "SuperDog Industries LLC",
    "EventLink": "https://www.superdogevents.com/",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 42.4288,
    "Longitude": -88.2016
  },
  {
    "Date": "2027-06-29",
    "Location": "Delran, NJ",
    "Host": "Ev-ry earthdog LLC",
    "EventLink": "https://ev-ryearthdog.com/",
    "TrialTypes": "ELT-P, NW2, NW1, L1I, L2C",
    "EventCount": 5,
    "Latitude": 40.0279,
    "Longitude": -75.0046
  },
  {
    "Date": "2027-09-25",
    "Location": "Eastlake, OH",
    "Host": "Nosework Addicts, LLC",
    "EventLink": "https://www.noseworkaddictsllc.com/",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 41.6444,
    "Longitude": -81.4654
  }
]
;
