const DATA_UPDATED = "August 10, 2026";
const TRIALS_DATA = 
[
  {
    "Date": "2024-08-10",
    "Location": "Fort Wayne, IN",
    "Host": "2 Nose You Is 2 Loves You",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 41.0544,
    "Longitude": -85.1842
  },
  {
    "Date": "2024-08-16",
    "Location": "Elgin, IL",
    "Host": "For Your K9",
    "TrialTypes": "NW3, NW1",
    "EventCount": 2,
    "Latitude": 42.0789,
    "Longitude": -88.2725
  },
  {
    "Date": "2024-08-16",
    "Location": "Huntington Beach, CA",
    "Host": "JavaK9s",
    "TrialTypes": "ELT-S, L1C, L1I",
    "EventCount": 3,
    "Latitude": 33.6461,
    "Longitude": -118.0158
  },
  {
    "Date": "2024-08-17",
    "Location": "Trappe, PA",
    "Host": "Sniff Sniff Hooray, LLC",
    "TrialTypes": "L1I, L1C, L2I, L2C",
    "EventCount": 4,
    "Latitude": 40.2421,
    "Longitude": -75.4547
  },
  {
    "Date": "2024-08-24",
    "Location": "Johnstown, NY",
    "Host": "My Dog Smells LLC",
    "TrialTypes": "ELT, NW1",
    "EventCount": 2,
    "Latitude": 42.9963,
    "Longitude": -74.337
  },
  {
    "Date": "2024-08-29",
    "Location": "White Plains, NY",
    "Host": "For the Love of Dogs NY, LLC",
    "TrialTypes": "NW3, NW2, NW1",
    "EventCount": 3,
    "Latitude": 40.9933,
    "Longitude": -73.7228
  },
  {
    "Date": "2024-08-31",
    "Location": "Dunkirk, NY",
    "Host": "Do Over Dog Training",
    "TrialTypes": "ELT-S, NW2, NW1",
    "EventCount": 3,
    "Latitude": 42.4583,
    "Longitude": -79.3105
  },
  {
    "Date": "2024-08-31",
    "Location": "Jefferson, OH",
    "Host": "Barns And Noses, LLC",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 40.364,
    "Longitude": -80.7791
  },
  {
    "Date": "2024-08-31",
    "Location": "Lafayette, IN",
    "Host": "Outside The Box Dog Training",
    "TrialTypes": "NW3, NW1",
    "EventCount": 2,
    "Latitude": 40.3805,
    "Longitude": -86.9338
  },
  {
    "Date": "2024-09-01",
    "Location": "Colesville, MD",
    "Host": "Red Huskies",
    "TrialTypes": "ELT-P, ELT-S, L3C",
    "EventCount": 3,
    "Latitude": 39.0822,
    "Longitude": -76.9773
  },
  {
    "Date": "2024-09-01",
    "Location": "Watertown, WI",
    "Host": "Think Pawsitive Dog Training",
    "TrialTypes": "NW2, NW1",
    "EventCount": 2,
    "Latitude": 43.1754,
    "Longitude": -88.768
  },
  {
    "Date": "2024-09-07",
    "Location": "Ames, IA",
    "Host": "KBP Dog Training",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 42.0221,
    "Longitude": -93.6412
  },
  {
    "Date": "2024-09-07",
    "Location": "Bloomington, MN",
    "Host": "St Paul Dog Training Club",
    "TrialTypes": "ELT-S, NW2",
    "EventCount": 2,
    "Latitude": 44.8807,
    "Longitude": -93.2744
  },
  {
    "Date": "2024-09-07",
    "Location": "Manchester, NH",
    "Host": "The Big Sniff, LLC",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 42.9546,
    "Longitude": -71.4192
  },
  {
    "Date": "2024-09-07",
    "Location": "Scotts Mills, OR",
    "Host": "Canine Discovery Corps",
    "TrialTypes": "NW1, L1E, NW2",
    "EventCount": 3,
    "Latitude": 45.0262,
    "Longitude": -122.6498
  },
  {
    "Date": "2024-09-13",
    "Location": "Flint, MI",
    "Host": "Every Dog Nosework",
    "TrialTypes": "NW3, ELT, ELT-P",
    "EventCount": 3,
    "Latitude": 42.9956,
    "Longitude": -83.6944
  },
  {
    "Date": "2024-09-13",
    "Location": "Frederick, MD",
    "Host": "Red Huskies",
    "TrialTypes": "ELT-P, NW3",
    "EventCount": 2,
    "Latitude": 39.4422,
    "Longitude": -77.3934
  },
  {
    "Date": "2024-09-13",
    "Location": "New Milford, PA",
    "Host": "Your Dog's Place, LLC",
    "TrialTypes": "ELT, NW3, ELT-S, NW2",
    "EventCount": 4,
    "Latitude": 41.8579,
    "Longitude": -75.7148
  },
  {
    "Date": "2024-09-14",
    "Location": "Carlisle, PA",
    "Host": "Nose-It-All, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 40.1664,
    "Longitude": -77.1919
  },
  {
    "Date": "2024-09-14",
    "Location": "Helena, MT",
    "Host": "Nosework Breakfast Club",
    "TrialTypes": "NW3, NW1, L1E",
    "EventCount": 3,
    "Latitude": 46.5476,
    "Longitude": -112.0326
  },
  {
    "Date": "2024-09-14",
    "Location": "Loma Mar, CA",
    "Host": "The Bay Team",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 37.2184,
    "Longitude": -122.2769
  },
  {
    "Date": "2024-09-14",
    "Location": "Loveland, CO",
    "Host": "Paws 4 Thought Dog Training, LLC",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 40.3952,
    "Longitude": -105.0573
  },
  {
    "Date": "2024-09-20",
    "Location": "Easton, MD",
    "Host": "Fair Play Labradors",
    "TrialTypes": "ELT-P, NW2, L1V",
    "EventCount": 3,
    "Latitude": 38.7522,
    "Longitude": -76.0565
  },
  {
    "Date": "2024-09-21",
    "Location": "North Bend, WA",
    "Host": "Northwest K9 Sniffers",
    "TrialTypes": "NW3, L1C, L1I",
    "EventCount": 3,
    "Latitude": 47.4767,
    "Longitude": -121.8106
  },
  {
    "Date": "2024-09-21",
    "Location": "Tuftonboro, NH",
    "Host": "The Big Sniff, LLC",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 43.7066,
    "Longitude": -71.2758
  },
  {
    "Date": "2024-09-21",
    "Location": "White Salmon, WA",
    "Host": "Sharon Smith",
    "TrialTypes": "NW1, ELT-S, NW3",
    "EventCount": 3,
    "Latitude": 45.6936,
    "Longitude": -121.5171
  },
  {
    "Date": "2024-09-27",
    "Location": "Estes Park, CO",
    "Host": "Mountain Dogs, LLC",
    "TrialTypes": "SMT, ELT-S",
    "EventCount": 2,
    "Latitude": 40.389,
    "Longitude": -105.5206
  },
  {
    "Date": "2024-09-27",
    "Location": "Richmond, VA",
    "Host": "Paws Plus Training, LLC",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 37.541,
    "Longitude": -77.431
  },
  {
    "Date": "2024-09-27",
    "Location": "Turlock, CA",
    "Host": "Two Nosey Girls",
    "TrialTypes": "L2I, L3I, ELT-S",
    "EventCount": 3,
    "Latitude": 37.5154,
    "Longitude": -120.8692
  },
  {
    "Date": "2024-09-28",
    "Location": "Grandview, TX",
    "Host": "North Texas Nosework Club",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 32.2349,
    "Longitude": -97.1565
  },
  {
    "Date": "2024-09-28",
    "Location": "Pittsburgh, PA",
    "Host": "Steel City Nosework, LLC",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 40.4836,
    "Longitude": -79.992
  },
  {
    "Date": "2024-09-28",
    "Location": "Reedsport, OR",
    "Host": "Wells Creek Dog Training",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 43.7197,
    "Longitude": -124.082
  },
  {
    "Date": "2024-09-28",
    "Location": "Waynesboro, PA",
    "Host": "Nose-It-All, LLC",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 39.7288,
    "Longitude": -77.6127
  },
  {
    "Date": "2024-09-29",
    "Location": "Glenwood, NY",
    "Host": "Do Over Dog Training",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 42.6637,
    "Longitude": -78.6915
  },
  {
    "Date": "2024-10-03",
    "Location": "New Rochelle, NY",
    "Host": "For the Love of Dogs NY LLC",
    "TrialTypes": "NW3, NW2, NW1",
    "EventCount": 3,
    "Latitude": 40.9153,
    "Longitude": -73.805
  },
  {
    "Date": "2024-10-04",
    "Location": "Golden, CO",
    "Host": "K9 Nosin’ Around, Inc.",
    "TrialTypes": "NW2, NW1",
    "EventCount": 2,
    "Latitude": 39.7162,
    "Longitude": -105.2043
  },
  {
    "Date": "2024-10-04",
    "Location": "Mechanicsburg, PA",
    "Host": "Nose-It-All, LLC",
    "TrialTypes": "SMT, L3V, L2V",
    "EventCount": 3,
    "Latitude": 40.2561,
    "Longitude": -76.9892
  },
  {
    "Date": "2024-10-05",
    "Location": "Centralia, WA",
    "Host": "About Face K9 Academy & Let's Talk Dogs, LLC",
    "TrialTypes": "ELT-S, NW1",
    "EventCount": 2,
    "Latitude": 46.6769,
    "Longitude": -122.9626
  },
  {
    "Date": "2024-10-05",
    "Location": "Copake, NY",
    "Host": "Saints2Source, LLC",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 42.1212,
    "Longitude": -73.5263
  },
  {
    "Date": "2024-10-05",
    "Location": "Crosslake, MN",
    "Host": "Nose 2 Tail Dog Training, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 46.6935,
    "Longitude": -94.163
  },
  {
    "Date": "2024-10-05",
    "Location": "Nashua, NH",
    "Host": "The Big Sniff, LLC",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 42.7822,
    "Longitude": -71.4341
  },
  {
    "Date": "2024-10-05",
    "Location": "New Paltz, NY",
    "Host": "Pat Tetrault and Dominique Manpel",
    "TrialTypes": "NW2, ELT-S, L2I",
    "EventCount": 3,
    "Latitude": 41.7366,
    "Longitude": -74.0361
  },
  {
    "Date": "2024-10-05",
    "Location": "Sandwich, IL",
    "Host": "For Your K9",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 41.6598,
    "Longitude": -88.5787
  },
  {
    "Date": "2024-10-05",
    "Location": "Troy, VA",
    "Host": "Your Dog Knows, LLC",
    "TrialTypes": "L1I, L2I, L1C, L2C",
    "EventCount": 4,
    "Latitude": 37.9609,
    "Longitude": -78.2697
  },
  {
    "Date": "2024-10-05",
    "Location": "West Bend, WI",
    "Host": "Think Pawsitive Dog Training",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 43.4152,
    "Longitude": -88.1346
  },
  {
    "Date": "2024-10-07",
    "Location": "Monterey, CA",
    "Host": "CalCoastal Dog Owners Group",
    "TrialTypes": "L1I, NW2, L2I",
    "EventCount": 3,
    "Latitude": 36.2534,
    "Longitude": -121.3671
  },
  {
    "Date": "2024-10-11",
    "Location": "South Haven, MN",
    "Host": "St Paul Dog Training Club",
    "TrialTypes": "SMT, ELT",
    "EventCount": 2,
    "Latitude": 45.3247,
    "Longitude": -94.1908
  },
  {
    "Date": "2024-10-11",
    "Location": "Walbridge, OH",
    "Host": "Robin Ford Dog Training",
    "TrialTypes": "NW1, NW2, NW3",
    "EventCount": 3,
    "Latitude": 41.6161,
    "Longitude": -83.4721
  },
  {
    "Date": "2024-10-12",
    "Location": "Homer Glen, IL",
    "Host": "Paws for Scent",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 41.574,
    "Longitude": -87.8905
  },
  {
    "Date": "2024-10-12",
    "Location": "Lafayette Hill, PA",
    "Host": "Sniff Sniff Hooray",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 40.0647,
    "Longitude": -75.2543
  },
  {
    "Date": "2024-10-12",
    "Location": "Sedona, AZ",
    "Host": "Release Canine LLC",
    "TrialTypes": "ELT, ELT-S, NW2, NW1",
    "EventCount": 4,
    "Latitude": 34.8658,
    "Longitude": -111.8057
  },
  {
    "Date": "2024-10-18",
    "Location": "Calhan, CO",
    "Host": "Mountain Dogs, LLC",
    "TrialTypes": "ELT, NW3, ELT-S",
    "EventCount": 3,
    "Latitude": 39.0616,
    "Longitude": -104.2496
  },
  {
    "Date": "2024-10-18",
    "Location": "Loganville, GA",
    "Host": "Canine Country Academy, LLC",
    "TrialTypes": "NW2, NW1",
    "EventCount": 2,
    "Latitude": 33.8831,
    "Longitude": -83.9227
  },
  {
    "Date": "2024-10-18",
    "Location": "Newfoundland, PA",
    "Host": "Your Dog's Place, LLC",
    "TrialTypes": "L1V, ELT-S, L2C, L1E",
    "EventCount": 4,
    "Latitude": 41.2787,
    "Longitude": -75.3132
  },
  {
    "Date": "2024-10-18",
    "Location": "Rossville, GA",
    "Host": "Camelot Shepherds, Inc",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 35.0149,
    "Longitude": -85.2758
  },
  {
    "Date": "2024-10-19",
    "Location": "Ferndale, WA",
    "Host": "Nose Work Magic",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 48.8102,
    "Longitude": -122.5686
  },
  {
    "Date": "2024-10-19",
    "Location": "Griffith, IN",
    "Host": "Outside the Box, LLC",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 41.5701,
    "Longitude": -87.4213
  },
  {
    "Date": "2024-10-19",
    "Location": "Kilmarnock, VA",
    "Host": "Red Huskies",
    "TrialTypes": "NW3, L1E, NW2",
    "EventCount": 3,
    "Latitude": 37.7359,
    "Longitude": -76.3939
  },
  {
    "Date": "2024-10-19",
    "Location": "Kingston, IL",
    "Host": "Common Scents K9",
    "TrialTypes": "NW1, L2C, NW2",
    "EventCount": 3,
    "Latitude": 42.1084,
    "Longitude": -88.7389
  },
  {
    "Date": "2024-10-19",
    "Location": "Lakeville, MN",
    "Host": "St Paul Dog Training Club",
    "TrialTypes": "ELT-P, NW1, L1E",
    "EventCount": 3,
    "Latitude": 44.6794,
    "Longitude": -93.2332
  },
  {
    "Date": "2024-10-19",
    "Location": "Round Rock, TX",
    "Host": "Heng Ten K9 Training",
    "TrialTypes": "NW3, ELT-S, NW1",
    "EventCount": 3,
    "Latitude": 30.4929,
    "Longitude": -97.6851
  },
  {
    "Date": "2024-10-19",
    "Location": "Yamhill, OR",
    "Host": "Nose Work Detectives, LLC",
    "TrialTypes": "L1C, L1V, L2C, L2V",
    "EventCount": 4,
    "Latitude": 45.192,
    "Longitude": -123.199
  },
  {
    "Date": "2024-10-22",
    "Location": "Astoria, OR",
    "Host": "Nosework Detectives, LLC",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 46.2368,
    "Longitude": -123.8311
  },
  {
    "Date": "2024-10-25",
    "Location": "Fishkill, NY",
    "Host": "Pat Tetrault and Dominique Manpel",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 41.5558,
    "Longitude": -73.9446
  },
  {
    "Date": "2024-10-25",
    "Location": "Palmyra, VA",
    "Host": "Paws Plus Training, LLC",
    "TrialTypes": "ELT, NW3, ELT-S, NW1",
    "EventCount": 4,
    "Latitude": 37.8448,
    "Longitude": -78.2902
  },
  {
    "Date": "2024-10-26",
    "Location": "Columbia City, IN",
    "Host": "2 Nose You Is 2 Loves You",
    "TrialTypes": "ELT, NW1",
    "EventCount": 2,
    "Latitude": 41.1401,
    "Longitude": -85.4637
  },
  {
    "Date": "2024-10-26",
    "Location": "Columbus, MT",
    "Host": "Nikki Markle of Canine Connection",
    "TrialTypes": "ELT-S, NW2, ELT",
    "EventCount": 3,
    "Latitude": 45.6844,
    "Longitude": -109.2474
  },
  {
    "Date": "2024-10-26",
    "Location": "Grand Junction, CO",
    "Host": "Mountain Dogs, LLC",
    "TrialTypes": "NW2, NW1",
    "EventCount": 2,
    "Latitude": 39.0977,
    "Longitude": -108.5474
  },
  {
    "Date": "2024-10-26",
    "Location": "Hammond, LA",
    "Host": "Dog Gone Right, LLC",
    "TrialTypes": "ELT-S, NW1, NW3",
    "EventCount": 3,
    "Latitude": 30.5491,
    "Longitude": -90.488
  },
  {
    "Date": "2024-10-26",
    "Location": "Medford, NJ",
    "Host": "K9 InScentives",
    "TrialTypes": "NW3, NW1",
    "EventCount": 2,
    "Latitude": 39.8787,
    "Longitude": -74.7901
  },
  {
    "Date": "2024-10-26",
    "Location": "Poland Springs, ME",
    "Host": "Virginia Howe",
    "TrialTypes": "NW1, NW3",
    "EventCount": 2,
    "Latitude": 44.0112,
    "Longitude": -70.3778
  },
  {
    "Date": "2024-10-26",
    "Location": "Suring, WI",
    "Host": "Clever Sniffers, LLC",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 44.9712,
    "Longitude": -88.4119
  },
  {
    "Date": "2024-10-26",
    "Location": "Welches, OR",
    "Host": "Trust Your Dog K9 Events",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 45.3,
    "Longitude": -121.9964
  },
  {
    "Date": "2024-10-26",
    "Location": "West Friendship, MD",
    "Host": "Red Huskies",
    "TrialTypes": "NW3, L2C, NW2",
    "EventCount": 3,
    "Latitude": 39.2966,
    "Longitude": -76.9095
  },
  {
    "Date": "2024-10-26",
    "Location": "Winterset, IA",
    "Host": "KBP Dog Training",
    "TrialTypes": "NW2, NW1",
    "EventCount": 2,
    "Latitude": 41.3369,
    "Longitude": -94.0178
  },
  {
    "Date": "2024-10-27",
    "Location": "San Martin, CA",
    "Host": "B. L. McMutts",
    "TrialTypes": "L1V, L2V",
    "EventCount": 2,
    "Latitude": 37.0462,
    "Longitude": -121.5672
  },
  {
    "Date": "2024-11-01",
    "Location": "Denton, MD",
    "Host": "Fair Play Labradors",
    "TrialTypes": "NW3, ELT-S, L1C, NW2, L1E",
    "EventCount": 5,
    "Latitude": 38.8754,
    "Longitude": -75.8667
  },
  {
    "Date": "2024-11-01",
    "Location": "Guerneville, CA",
    "Host": "Jen Huot",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 38.493,
    "Longitude": -123.0148
  },
  {
    "Date": "2024-11-01",
    "Location": "Red Feather Lakes, CO",
    "Host": "Beyond Elevation K9 Training",
    "TrialTypes": "NW1, NW2, NW3",
    "EventCount": 3,
    "Latitude": 40.8233,
    "Longitude": -105.5561
  },
  {
    "Date": "2024-11-02",
    "Location": "Callaway, VA",
    "Host": "Canny K9 Companions LLC",
    "TrialTypes": "ELT-S, NW1, NW2",
    "EventCount": 3,
    "Latitude": 36.9735,
    "Longitude": -80.0074
  },
  {
    "Date": "2024-11-02",
    "Location": "Greenview, IL",
    "Host": "Capitol Canine Dog Sports",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 40.0473,
    "Longitude": -89.6952
  },
  {
    "Date": "2024-11-02",
    "Location": "Kennebunkport, ME",
    "Host": "Elizabeth Dutton",
    "TrialTypes": "ELT-P, NW3",
    "EventCount": 2,
    "Latitude": 43.3625,
    "Longitude": -70.5261
  },
  {
    "Date": "2024-11-02",
    "Location": "Mays Landing, NJ",
    "Host": "Rotts-n-Notts Nosework, LLC",
    "TrialTypes": "NW3, L1E, NW1",
    "EventCount": 3,
    "Latitude": 39.4351,
    "Longitude": -74.7375
  },
  {
    "Date": "2024-11-02",
    "Location": "Mill Spring, NC",
    "Host": "Foothills Canine Academy, LLC",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 35.2913,
    "Longitude": -82.1205
  },
  {
    "Date": "2024-11-02",
    "Location": "Shawnee, OK",
    "Host": "The Doggie Spot, LLC",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 35.3622,
    "Longitude": -96.9162
  },
  {
    "Date": "2024-11-02",
    "Location": "Valencia, CA",
    "Host": "Pink Biscuit K9s",
    "TrialTypes": "L1I, L2I, L1C, L2C",
    "EventCount": 4,
    "Latitude": 34.3675,
    "Longitude": -118.538
  },
  {
    "Date": "2024-11-02",
    "Location": "Wappingers Falls, NY",
    "Host": "Top Notch Dogs, LLC",
    "TrialTypes": "ELT-P, ELT-S, L1C",
    "EventCount": 3,
    "Latitude": 41.5488,
    "Longitude": -73.8902
  },
  {
    "Date": "2024-11-03",
    "Location": "McMinnville, OR",
    "Host": "Doglandia LLC and Carol Forsberg",
    "TrialTypes": "NW2, ELT",
    "EventCount": 2,
    "Latitude": 45.2253,
    "Longitude": -123.1739
  },
  {
    "Date": "2024-11-04",
    "Location": "Duluth, GA",
    "Host": "Right Choice Dog Training, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 34.0291,
    "Longitude": -84.1446
  },
  {
    "Date": "2024-11-08",
    "Location": "Montrose, CO",
    "Host": "Mountain Dogs, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 38.5115,
    "Longitude": -107.9262
  },
  {
    "Date": "2024-11-09",
    "Location": "Canoga Park, CA",
    "Host": "Pink Biscuit K9s",
    "TrialTypes": "NW1, L3I, L3C",
    "EventCount": 3,
    "Latitude": 34.2338,
    "Longitude": -118.6021
  },
  {
    "Date": "2024-11-09",
    "Location": "Eldred, NY",
    "Host": "Pocono Nose Work",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 41.4859,
    "Longitude": -74.8665
  },
  {
    "Date": "2024-11-09",
    "Location": "Escondido, CA",
    "Host": "Anita Cheesman and Jessica Koester",
    "TrialTypes": "NW1, L1C",
    "EventCount": 2,
    "Latitude": 33.1584,
    "Longitude": -117.1143
  },
  {
    "Date": "2024-11-09",
    "Location": "Huntsville, AL",
    "Host": "Sniffers Anonymous",
    "TrialTypes": "NW3, L1I, L1C",
    "EventCount": 3,
    "Latitude": 34.7498,
    "Longitude": -86.5812
  },
  {
    "Date": "2024-11-09",
    "Location": "Milton, NH",
    "Host": "Lucky Dog Events",
    "TrialTypes": "NW3, L2I, NW2",
    "EventCount": 3,
    "Latitude": 43.3796,
    "Longitude": -71.017
  },
  {
    "Date": "2024-11-09",
    "Location": "Moline, IL",
    "Host": "Fur Better Fur Worse, LLC",
    "TrialTypes": "ELT-S",
    "EventCount": 1,
    "Latitude": 41.5267,
    "Longitude": -90.4897
  },
  {
    "Date": "2024-11-09",
    "Location": "New Rochelle, NY",
    "Host": "For the Love of Dogs NY, LLC",
    "TrialTypes": "L3I, L3C, NW1, NW2",
    "EventCount": 4,
    "Latitude": 40.9581,
    "Longitude": -73.8014
  },
  {
    "Date": "2024-11-09",
    "Location": "Schaumburg, IL",
    "Host": "Northwest Obedience Club Inc.",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 42.0187,
    "Longitude": -88.1024
  },
  {
    "Date": "2024-11-10",
    "Location": "Odessa, FL",
    "Host": "Hoppin’ in the Hills",
    "TrialTypes": "NW2, NW1",
    "EventCount": 2,
    "Latitude": 28.1924,
    "Longitude": -82.5216
  },
  {
    "Date": "2024-11-11",
    "Location": "Escondido, CA",
    "Host": "Anita Cheesman and Jessica Koester",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 33.0892,
    "Longitude": -117.0456
  },
  {
    "Date": "2024-11-11",
    "Location": "Paso Robles, CA",
    "Host": "Gentle Touch Pet Training",
    "TrialTypes": "L1C, L2I, ELT-S, NW2",
    "EventCount": 4,
    "Latitude": 35.6031,
    "Longitude": -120.7037
  },
  {
    "Date": "2024-11-15",
    "Location": "Harrington, DE",
    "Host": "Shamrock Pot of Gold K9 Scenter",
    "TrialTypes": "NW3, ELT, ELT-P, NW2, NW1",
    "EventCount": 5,
    "Latitude": 38.9493,
    "Longitude": -75.5862
  },
  {
    "Date": "2024-11-15",
    "Location": "Rancho Cucamonga, CA",
    "Host": "Agile Paws Dog Sports",
    "TrialTypes": "L2C, NW2, NW1, L1C",
    "EventCount": 4,
    "Latitude": 34.0758,
    "Longitude": -117.6019
  },
  {
    "Date": "2024-11-16",
    "Location": "Auburn, WA",
    "Host": "Northwest K9 Sniffers",
    "TrialTypes": "ELT-S, NW1, L2C, L2I",
    "EventCount": 4,
    "Latitude": 47.3086,
    "Longitude": -122.2724
  },
  {
    "Date": "2024-11-16",
    "Location": "Foxborough, MA",
    "Host": "MasterPeace Dog Training",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 42.0976,
    "Longitude": -71.2607
  },
  {
    "Date": "2024-11-16",
    "Location": "Marble Falls, TX",
    "Host": "Scent Work Across Texas",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 30.5849,
    "Longitude": -98.314
  },
  {
    "Date": "2024-11-16",
    "Location": "Nevada City, CA",
    "Host": "Sierra Sniffing Canines",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 39.3045,
    "Longitude": -120.989
  },
  {
    "Date": "2024-11-16",
    "Location": "Tucson, AZ",
    "Host": "Patience Unlimited Professional Dog Training",
    "TrialTypes": "NW1, L1C, L1E, L1I",
    "EventCount": 4,
    "Latitude": 32.2008,
    "Longitude": -110.9966
  },
  {
    "Date": "2024-11-16",
    "Location": "Yanceyville, NC",
    "Host": "Whole Dog Institute, LLC",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 36.4197,
    "Longitude": -79.3657
  },
  {
    "Date": "2024-11-23",
    "Location": "Coburg, OR",
    "Host": "Kiddie Christie",
    "TrialTypes": "L1E, NW1, NW3",
    "EventCount": 3,
    "Latitude": 44.1713,
    "Longitude": -123.0892
  },
  {
    "Date": "2024-11-23",
    "Location": "Delta, CO",
    "Host": "Mountain Dogs, LLC",
    "TrialTypes": "NW3, ELT-P",
    "EventCount": 2,
    "Latitude": 38.8284,
    "Longitude": -107.8828
  },
  {
    "Date": "2024-11-23",
    "Location": "Fork Union, VA",
    "Host": "Your Dog Knows LLC",
    "TrialTypes": "NW3, L3I, L1V",
    "EventCount": 3,
    "Latitude": 37.7196,
    "Longitude": -78.235
  },
  {
    "Date": "2024-11-23",
    "Location": "Kintnersville, PA",
    "Host": "Paws n' Sniff",
    "TrialTypes": "NW1, L1E, ELT-S",
    "EventCount": 3,
    "Latitude": 40.5475,
    "Longitude": -75.2244
  },
  {
    "Date": "2024-11-23",
    "Location": "Saltsburg, PA",
    "Host": "Steel City Nosework, LLC",
    "TrialTypes": "NW3, ELT-S, NW1",
    "EventCount": 3,
    "Latitude": 40.4867,
    "Longitude": -79.4072
  },
  {
    "Date": "2024-11-23",
    "Location": "Smyrna, TN",
    "Host": "Dogs Have Amazing Noses LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 35.9366,
    "Longitude": -86.4838
  },
  {
    "Date": "2024-11-29",
    "Location": "Capo Beach/Dana Point, CA",
    "Host": "JavaK9s",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 33.4717,
    "Longitude": -117.6452
  },
  {
    "Date": "2024-11-29",
    "Location": "Foxborough, MA",
    "Host": "Tracey Costa",
    "TrialTypes": "ELT, L1C, NW2",
    "EventCount": 3,
    "Latitude": 42.0811,
    "Longitude": -71.2818
  },
  {
    "Date": "2024-11-30",
    "Location": "Cottage Grove, MN",
    "Host": "Gretchen Hofheins-Wackerfuss",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 44.8115,
    "Longitude": -92.9136
  },
  {
    "Date": "2024-11-30",
    "Location": "Cumming, GA",
    "Host": "Georgia Nosework",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 34.209,
    "Longitude": -84.1084
  },
  {
    "Date": "2024-11-30",
    "Location": "Green Bay, WI",
    "Host": "NEWK9 Scent Work LLC",
    "TrialTypes": "ELT-S, ELT",
    "EventCount": 2,
    "Latitude": 44.5267,
    "Longitude": -88.0219
  },
  {
    "Date": "2024-11-30",
    "Location": "Lebanon, NJ",
    "Host": "Sirius K-9 Solutions",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 40.6035,
    "Longitude": -74.8709
  },
  {
    "Date": "2024-11-30",
    "Location": "Los Osos, CA",
    "Host": "Central Coast Nosework Club, Inc.",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 35.3443,
    "Longitude": -120.8236
  },
  {
    "Date": "2024-11-30",
    "Location": "Plant City, FL",
    "Host": "Hoppin’ in the Hills",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 27.9857,
    "Longitude": -82.155
  },
  {
    "Date": "2024-11-30",
    "Location": "Worcester, PA",
    "Host": "Sniff Sniff Hooray",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 40.201,
    "Longitude": -75.3068
  },
  {
    "Date": "2024-12-06",
    "Location": "Salem, WI",
    "Host": "Loving Paws Dog Training LLC",
    "TrialTypes": "ELT-P, ELT",
    "EventCount": 2,
    "Latitude": 42.5377,
    "Longitude": -88.0935
  },
  {
    "Date": "2024-12-06",
    "Location": "Ypsilanti, MI",
    "Host": "Every Dog Nosework",
    "TrialTypes": "ELT-P, ELT-S, L2C",
    "EventCount": 3,
    "Latitude": 42.2865,
    "Longitude": -83.6236
  },
  {
    "Date": "2024-12-07",
    "Location": "Batavia, NY",
    "Host": "Do Over Dog Training",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 42.9612,
    "Longitude": -78.218
  },
  {
    "Date": "2024-12-07",
    "Location": "Bowie, MD",
    "Host": "Red Huskies",
    "TrialTypes": "ELT-P, ELT-S",
    "EventCount": 2,
    "Latitude": 38.9644,
    "Longitude": -76.6929
  },
  {
    "Date": "2024-12-07",
    "Location": "Centralia, WA",
    "Host": "Let's Talk Dogs, LLC and About Face K9 Academy",
    "TrialTypes": "ELT-P, NW2",
    "EventCount": 2,
    "Latitude": 46.7057,
    "Longitude": -122.9898
  },
  {
    "Date": "2024-12-07",
    "Location": "Chester Springs, PA",
    "Host": "Patricia Grassey",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 40.0774,
    "Longitude": -75.6159
  },
  {
    "Date": "2024-12-07",
    "Location": "DeLeon Springs, FL",
    "Host": "River Poodles Training, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 29.1093,
    "Longitude": -81.3441
  },
  {
    "Date": "2024-12-07",
    "Location": "Fillmore, CA",
    "Host": "Pink Biscuit K9s",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 34.3498,
    "Longitude": -118.89
  },
  {
    "Date": "2024-12-07",
    "Location": "Newfoundland, PA",
    "Host": "Your Dog's Place, LLC",
    "TrialTypes": "L3V, NW2, ELT",
    "EventCount": 3,
    "Latitude": 41.2916,
    "Longitude": -75.2821
  },
  {
    "Date": "2024-12-07",
    "Location": "Owenton, KY",
    "Host": "Clermont County Dog Training Club",
    "TrialTypes": "ELT, NW1, NW2",
    "EventCount": 3,
    "Latitude": 38.5771,
    "Longitude": -84.8122
  },
  {
    "Date": "2024-12-09",
    "Location": "Stockton, CA",
    "Host": "Two Nosey Girls",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 37.9094,
    "Longitude": -121.2423
  },
  {
    "Date": "2024-12-13",
    "Location": "Pittstown, NJ",
    "Host": "Shamrock Pot of Gold K9 Scenter",
    "TrialTypes": "NW3, ELT-P, ELT-S, NW1",
    "EventCount": 4,
    "Latitude": 40.5417,
    "Longitude": -74.9914
  },
  {
    "Date": "2024-12-14",
    "Location": "Ontario, CA",
    "Host": "Agile Paws Dog Sports",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 34.1085,
    "Longitude": -117.6579
  },
  {
    "Date": "2024-12-21",
    "Location": "Cedar Park, TX",
    "Host": "Scent Work Across Texas",
    "TrialTypes": "L1V, L2I, NW1, L2E",
    "EventCount": 4,
    "Latitude": 30.5444,
    "Longitude": -97.8252
  },
  {
    "Date": "2024-12-21",
    "Location": "Jefferson, GA",
    "Host": "Right Choice Dog Training, LLC",
    "TrialTypes": "NW2, NW1",
    "EventCount": 2,
    "Latitude": 33.0422,
    "Longitude": -82.432
  },
  {
    "Date": "2024-12-21",
    "Location": "Marriottsville, MD",
    "Host": "Red Huskies",
    "TrialTypes": "ELT-P, ELT-S, NW1",
    "EventCount": 3,
    "Latitude": 39.3482,
    "Longitude": -76.8802
  },
  {
    "Date": "2024-12-27",
    "Location": "Crownsville, MD",
    "Host": "Red Huskies",
    "TrialTypes": "ELT-P, ELT, ELT-S",
    "EventCount": 3,
    "Latitude": 38.9873,
    "Longitude": -76.636
  },
  {
    "Date": "2024-12-28",
    "Location": "Bellingham, WA",
    "Host": "Nose Work Magic",
    "TrialTypes": "L1V, ELT-S, ELT",
    "EventCount": 3,
    "Latitude": 48.7995,
    "Longitude": -122.5266
  },
  {
    "Date": "2024-12-28",
    "Location": "Cumming, GA",
    "Host": "Georgia Nosework",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 34.2416,
    "Longitude": -84.1375
  },
  {
    "Date": "2024-12-28",
    "Location": "Salem, OR",
    "Host": "Helix Fairweather & Doglandia, LLC",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 44.9089,
    "Longitude": -123.002
  },
  {
    "Date": "2024-12-28",
    "Location": "Williamsburg, VA",
    "Host": "Blockade Runners Flyball",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 37.296,
    "Longitude": -76.6969
  },
  {
    "Date": "2024-12-29",
    "Location": "Waukesha, WI",
    "Host": "Think Pawsitive Dog Training",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 43.0995,
    "Longitude": -88.2815
  },
  {
    "Date": "2024-12-31",
    "Location": "Strasburg, IL",
    "Host": "Kudos for Canines, LLC",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 39.3976,
    "Longitude": -88.5964
  },
  {
    "Date": "2025-01-03",
    "Location": "Brockport, NY",
    "Host": "Savvy Dog Sports",
    "TrialTypes": "ELT-S, ELT",
    "EventCount": 2,
    "Latitude": 43.2022,
    "Longitude": -77.9159
  },
  {
    "Date": "2025-01-03",
    "Location": "Emmitsburg, MD",
    "Host": "Red Huskies",
    "TrialTypes": "ELT, ELT-P, ELT-S",
    "EventCount": 3,
    "Latitude": 39.6763,
    "Longitude": -77.3623
  },
  {
    "Date": "2025-01-04",
    "Location": "Bonsall, CA",
    "Host": "Linda Buchanan",
    "TrialTypes": "NW2, ELT",
    "EventCount": 2,
    "Latitude": 33.3186,
    "Longitude": -117.1797
  },
  {
    "Date": "2025-01-09",
    "Location": "Centreville, MD",
    "Host": "Fair Play Labradors",
    "TrialTypes": "ELT-S, NW3, ELT, ELT-P",
    "EventCount": 4,
    "Latitude": 39.0065,
    "Longitude": -76.0814
  },
  {
    "Date": "2025-01-10",
    "Location": "Hartfield, VA",
    "Host": "Paws Plus Training, LLC",
    "TrialTypes": "NW3, NW1, NW2, ELT",
    "EventCount": 4,
    "Latitude": 37.5597,
    "Longitude": -76.4553
  },
  {
    "Date": "2025-01-11",
    "Location": "Greensboro, NC",
    "Host": "Dog Fun Forever, LLC",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 36.1076,
    "Longitude": -79.7809
  },
  {
    "Date": "2025-01-11",
    "Location": "Lithia, FL",
    "Host": "Hoppin’ in the Hills",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 27.882,
    "Longitude": -82.2231
  },
  {
    "Date": "2025-01-13",
    "Location": "Oakdale, CA",
    "Host": "Two Nosey Girls",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 37.7488,
    "Longitude": -120.846
  },
  {
    "Date": "2025-01-18",
    "Location": "Clanton, AL",
    "Host": "Daphne Melillo",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 32.8833,
    "Longitude": -86.6276
  },
  {
    "Date": "2025-01-18",
    "Location": "Elmira, OR",
    "Host": "Wells Creek Dog Training",
    "TrialTypes": "ELT, L1I, NW2",
    "EventCount": 3,
    "Latitude": 44.115,
    "Longitude": -123.3873
  },
  {
    "Date": "2025-01-18",
    "Location": "Flemington, NJ",
    "Host": "Shamrock Pot of Gold K9 Scenter",
    "TrialTypes": "ELT-S, L2I, L2C, ELT",
    "EventCount": 4,
    "Latitude": 40.5323,
    "Longitude": -74.8486
  },
  {
    "Date": "2025-01-18",
    "Location": "Marble Falls, TX",
    "Host": "Heng Ten K9 Training",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 30.5691,
    "Longitude": -98.2693
  },
  {
    "Date": "2025-01-18",
    "Location": "Melrose, FL",
    "Host": "River Poodles Training, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 29.733,
    "Longitude": -82.0826
  },
  {
    "Date": "2025-01-18",
    "Location": "New Rochelle, NY",
    "Host": "For the Love of Dogs NY, LLC",
    "TrialTypes": "NW2, NW3, ELT-S",
    "EventCount": 3,
    "Latitude": 40.9232,
    "Longitude": -73.8145
  },
  {
    "Date": "2025-01-18",
    "Location": "Redlands, CA",
    "Host": "Agile Paws Dog Sports",
    "TrialTypes": "ELT, NW2",
    "EventCount": 2,
    "Latitude": 34.077,
    "Longitude": -117.1919
  },
  {
    "Date": "2025-01-18",
    "Location": "Sheridan, OR",
    "Host": "Helix Fairweather & Doglandia, LLC",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 45.1446,
    "Longitude": -123.3621
  },
  {
    "Date": "2025-01-25",
    "Location": "Danielsville, GA",
    "Host": "Right Choice Dog Training, LLC",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 34.1137,
    "Longitude": -83.1768
  },
  {
    "Date": "2025-01-25",
    "Location": "Tecumseh, OK",
    "Host": "The Doggie Spot, LLC",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 35.2764,
    "Longitude": -96.9049
  },
  {
    "Date": "2025-01-31",
    "Location": "Vista, CA",
    "Host": "Anita Cheesman and Jessica Koester",
    "TrialTypes": "ELT-S, NW3",
    "EventCount": 2,
    "Latitude": 33.1694,
    "Longitude": -117.2888
  },
  {
    "Date": "2025-02-01",
    "Location": "Northridge, CA",
    "Host": "Scentwork.org",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 34.2096,
    "Longitude": -118.5284
  },
  {
    "Date": "2025-02-08",
    "Location": "Murfreesboro, TN",
    "Host": "Dogs Have Amazing Noses LLC",
    "TrialTypes": "ELT, NW1",
    "EventCount": 2,
    "Latitude": 35.8257,
    "Longitude": -86.3515
  },
  {
    "Date": "2025-02-08",
    "Location": "Veneta, OR",
    "Host": "Kiddy Christie",
    "TrialTypes": "NW3, L1C, NW1",
    "EventCount": 3,
    "Latitude": 44.0505,
    "Longitude": -123.3383
  },
  {
    "Date": "2025-02-14",
    "Location": "Honey Brook, PA",
    "Host": "The Sniffing Hound",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 40.0696,
    "Longitude": -75.8853
  },
  {
    "Date": "2025-02-15",
    "Location": "Bellingham, WA",
    "Host": "Nose Work Magic",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 48.763,
    "Longitude": -122.5223
  },
  {
    "Date": "2025-02-15",
    "Location": "Flemington, NJ",
    "Host": "Shamrock Pot of Gold K9 Scenter",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 40.4883,
    "Longitude": -74.9051
  },
  {
    "Date": "2025-02-15",
    "Location": "Lakewood, NJ",
    "Host": "Rotts-n-Notts Nosework, LLC",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 40.0479,
    "Longitude": -74.257
  },
  {
    "Date": "2025-02-15",
    "Location": "Lutherville-Timonium, MD",
    "Host": "Red Huskies",
    "TrialTypes": "L3C, L1I, NW2",
    "EventCount": 3,
    "Latitude": 39.4252,
    "Longitude": -76.6577
  },
  {
    "Date": "2025-02-15",
    "Location": "Medford, NJ",
    "Host": "Sniff Sniff Hooray",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 39.8906,
    "Longitude": -74.7787
  },
  {
    "Date": "2025-02-15",
    "Location": "Modesto, CA",
    "Host": "Two Nosey Girls",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 37.6651,
    "Longitude": -120.9853
  },
  {
    "Date": "2025-02-15",
    "Location": "White Plains, NY",
    "Host": "For the Love of Dogs NY, LLC",
    "TrialTypes": "L1I, L2C, NW3",
    "EventCount": 3,
    "Latitude": 41.0296,
    "Longitude": -73.8115
  },
  {
    "Date": "2025-02-15",
    "Location": "Wilson, NC",
    "Host": "Whole Dog Institute, LLC",
    "TrialTypes": "NW3, NW1",
    "EventCount": 2,
    "Latitude": 35.6684,
    "Longitude": -77.9044
  },
  {
    "Date": "2025-02-16",
    "Location": "Chino, CA",
    "Host": "Agile Paws Dog Sports",
    "TrialTypes": "ELT, ELT-S",
    "EventCount": 2,
    "Latitude": 34.0153,
    "Longitude": -117.7041
  },
  {
    "Date": "2025-02-22",
    "Location": "Albuquerque, NM",
    "Host": "The Can Do K9, LLC",
    "TrialTypes": "NW1, NW3",
    "EventCount": 2,
    "Latitude": 35.1184,
    "Longitude": -106.6489
  },
  {
    "Date": "2025-02-23",
    "Location": "Benson, AZ",
    "Host": "Patience Unlimited Professional Dog Training",
    "TrialTypes": "NW3, ELT-S, NW2",
    "EventCount": 3,
    "Latitude": 31.974,
    "Longitude": -110.2667
  },
  {
    "Date": "2025-02-24",
    "Location": "Paso Robles, CA",
    "Host": "Gentle Touch Pet Training",
    "TrialTypes": "NW3, L2V, L1V",
    "EventCount": 3,
    "Latitude": 35.6527,
    "Longitude": -120.7137
  },
  {
    "Date": "2025-02-28",
    "Location": "San Rafael, CA",
    "Host": "Marin Humane",
    "TrialTypes": "L1C, ELT-S, ELT",
    "EventCount": 3,
    "Latitude": 37.9755,
    "Longitude": -122.546
  },
  {
    "Date": "2025-03-01",
    "Location": "Augusta, NJ",
    "Host": "Shamrock Pot of Gold K9 Scenter",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 41.1264,
    "Longitude": -74.7002
  },
  {
    "Date": "2025-03-01",
    "Location": "Keystone Heights, FL",
    "Host": "River Poodles Training, LLC",
    "TrialTypes": "NW3, ELT-P",
    "EventCount": 2,
    "Latitude": 29.7652,
    "Longitude": -82.0346
  },
  {
    "Date": "2025-03-01",
    "Location": "Oakville, WA",
    "Host": "About Face K9 Academy and Let's Talk Dogs, LLC",
    "TrialTypes": "NW3, L1C, L1I",
    "EventCount": 3,
    "Latitude": 46.7922,
    "Longitude": -123.2039
  },
  {
    "Date": "2025-03-01",
    "Location": "Pomfret, MD",
    "Host": "Nose-It-All, LLC",
    "TrialTypes": "ELT-S, NW2, ELT",
    "EventCount": 3,
    "Latitude": 38.6121,
    "Longitude": -76.9899
  },
  {
    "Date": "2025-03-01",
    "Location": "Santa Paula, CA",
    "Host": "Pink Biscuit K9s",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 34.3139,
    "Longitude": -119.0966
  },
  {
    "Date": "2025-03-01",
    "Location": "Youngwood, PA",
    "Host": "Steel City Nosework, LLC",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 40.2601,
    "Longitude": -79.5596
  },
  {
    "Date": "2025-03-02",
    "Location": "Shawnee, OK",
    "Host": "The Doggie Spot, LLC",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 35.3016,
    "Longitude": -96.9171
  },
  {
    "Date": "2025-03-07",
    "Location": "Elgin, IL",
    "Host": "For Your K9",
    "TrialTypes": "L1C, L2C, L1I, L2I",
    "EventCount": 4,
    "Latitude": 42.0242,
    "Longitude": -88.2556
  },
  {
    "Date": "2025-03-07",
    "Location": "Spring City, PA",
    "Host": "Patricia Grassey",
    "TrialTypes": "NW3, ELT, ELT-S, L1I",
    "EventCount": 4,
    "Latitude": 40.1703,
    "Longitude": -75.5748
  },
  {
    "Date": "2025-03-07",
    "Location": "Stokesdale , NC",
    "Host": "K9 Nose Adventures, LLC",
    "TrialTypes": "ELT, NW1, NW2, L1C, L1I",
    "EventCount": 5,
    "Latitude": 36.1983,
    "Longitude": -79.9836
  },
  {
    "Date": "2025-03-08",
    "Location": "Farmville, VA",
    "Host": "Paws Plus Training, LLC",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 37.3357,
    "Longitude": -78.3792
  },
  {
    "Date": "2025-03-08",
    "Location": "Fort Collins, CO",
    "Host": "Beyond Elevation K9 Training",
    "TrialTypes": "NW1, NW2, ELT",
    "EventCount": 3,
    "Latitude": 40.5553,
    "Longitude": -105.1247
  },
  {
    "Date": "2025-03-08",
    "Location": "Foxboro , MA",
    "Host": "Bay State Sniffers",
    "TrialTypes": "L1C, ELT-S, L1I, L3I",
    "EventCount": 4,
    "Latitude": 42.1251,
    "Longitude": -71.2174
  },
  {
    "Date": "2025-03-08",
    "Location": "Rome, GA",
    "Host": "Southeast Scent Work Alliance, LLC",
    "TrialTypes": "ELT, NW2",
    "EventCount": 2,
    "Latitude": 34.2478,
    "Longitude": -85.1632
  },
  {
    "Date": "2025-03-08",
    "Location": "Winterset, IA",
    "Host": "KBP Dog Training",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 41.2933,
    "Longitude": -94.0627
  },
  {
    "Date": "2025-03-10",
    "Location": "Riverside, CA",
    "Host": "Linda Buchanan",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 33.9901,
    "Longitude": -117.3656
  },
  {
    "Date": "2025-03-14",
    "Location": "Phoenix, AZ",
    "Host": "Successful Sniffer",
    "TrialTypes": "NW3, ELT-S, NW1, NW2",
    "EventCount": 4,
    "Latitude": 33.4265,
    "Longitude": -112.1096
  },
  {
    "Date": "2025-03-14",
    "Location": "Phoenix, MD",
    "Host": "Oriole Dog Training Club",
    "TrialTypes": "NW3, L2I, NW2",
    "EventCount": 3,
    "Latitude": 39.4912,
    "Longitude": -76.6375
  },
  {
    "Date": "2025-03-15",
    "Location": "Blaine, WA",
    "Host": "Nose Work Magic",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 49.0056,
    "Longitude": -122.7816
  },
  {
    "Date": "2025-03-15",
    "Location": "Califon (formerly Pomona), NY",
    "Host": "Shamrock Pot of Gold K9 Scenter",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 41.1465,
    "Longitude": -74.0877
  },
  {
    "Date": "2025-03-15",
    "Location": "Gainesville, GA",
    "Host": "Georgia Nosework",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 34.3088,
    "Longitude": -83.8504
  },
  {
    "Date": "2025-03-15",
    "Location": "Kent, WA",
    "Host": "Northwest K9 Sniffers",
    "TrialTypes": "ELT-S, L1V, L1I",
    "EventCount": 3,
    "Latitude": 47.3843,
    "Longitude": -122.2489
  },
  {
    "Date": "2025-03-15",
    "Location": "Pflugerville, TX",
    "Host": "Scent Work Across Texas",
    "TrialTypes": "ELT-S, NW2, L1C, L1E",
    "EventCount": 4,
    "Latitude": 30.4547,
    "Longitude": -97.576
  },
  {
    "Date": "2025-03-15",
    "Location": "Thaxton, VA",
    "Host": "Canny K9 Companions LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 37.3199,
    "Longitude": -79.5755
  },
  {
    "Date": "2025-03-15",
    "Location": "Westminster, MD",
    "Host": "Red Huskies",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 39.5569,
    "Longitude": -76.9479
  },
  {
    "Date": "2025-03-17",
    "Location": "Corralitos, CA",
    "Host": "CalCoastal Dog Owners Group",
    "TrialTypes": "NW2, NW1",
    "EventCount": 2,
    "Latitude": 36.9956,
    "Longitude": -121.8363
  },
  {
    "Date": "2025-03-22",
    "Location": "Johnstown, NY",
    "Host": "My Dog Smells LLC",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 43.0281,
    "Longitude": -74.3602
  },
  {
    "Date": "2025-03-22",
    "Location": "Salem, WI",
    "Host": "Loving Paws Dog Training LLC",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 42.5874,
    "Longitude": -88.0896
  },
  {
    "Date": "2025-03-22",
    "Location": "Shelbyville, TN",
    "Host": "Dogs Have Amazing Noses LLC",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 35.4903,
    "Longitude": -86.4539
  },
  {
    "Date": "2025-03-22",
    "Location": "Tampa, FL",
    "Host": "Hoppin’ in the Hills",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 27.9282,
    "Longitude": -82.4396
  },
  {
    "Date": "2025-03-22",
    "Location": "Wakefield, VA",
    "Host": "Paws Plus Training, LLC",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 36.9517,
    "Longitude": -77.0327
  },
  {
    "Date": "2025-03-23",
    "Location": "Rapid City, SD",
    "Host": "Two Paws Up Dog Training, LLC",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 44.0698,
    "Longitude": -103.2447
  },
  {
    "Date": "2025-03-23",
    "Location": "Upland, CA",
    "Host": "Agile Paws Dog Sports",
    "TrialTypes": "NW1",
    "EventCount": 1,
    "Latitude": 34.0958,
    "Longitude": -117.6933
  },
  {
    "Date": "2025-03-28",
    "Location": "Dobbs Ferry, NY",
    "Host": "For the Love of Dogs NY, LLC",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 41.039,
    "Longitude": -73.8962
  },
  {
    "Date": "2025-03-28",
    "Location": "Flint, MI",
    "Host": "Every Dog Nosework",
    "TrialTypes": "SMT, ELT-S, L2I",
    "EventCount": 3,
    "Latitude": 43.0544,
    "Longitude": -83.6873
  },
  {
    "Date": "2025-03-28",
    "Location": "Frederick, MD",
    "Host": "Red Huskies",
    "TrialTypes": "ELT-P, ELT, ELT-S",
    "EventCount": 3,
    "Latitude": 39.3738,
    "Longitude": -77.3888
  },
  {
    "Date": "2025-03-28",
    "Location": "Grand Junction, CO",
    "Host": "Mountain Dogs, LLC",
    "TrialTypes": "NW3, L1C, NW2, NW1",
    "EventCount": 4,
    "Latitude": 39.0623,
    "Longitude": -108.5925
  },
  {
    "Date": "2025-03-28",
    "Location": "Salem, OR",
    "Host": "Kristina Leipzig, Doglandia LLC and Carol Forsberg",
    "TrialTypes": "ELT-S",
    "EventCount": 1,
    "Latitude": 44.9308,
    "Longitude": -123.0383
  },
  {
    "Date": "2025-03-28",
    "Location": "Shady Hills, FL",
    "Host": "Hoppin’ in the Hills",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 28.3388,
    "Longitude": -82.5748
  },
  {
    "Date": "2025-03-29",
    "Location": "Clinton, WI",
    "Host": "George and Shannon Carpenter",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 42.5524,
    "Longitude": -88.8547
  },
  {
    "Date": "2025-03-29",
    "Location": "Gilbertsville, PA",
    "Host": "Sniff Sniff Hooray",
    "TrialTypes": "NW2, ELT",
    "EventCount": 2,
    "Latitude": 40.3422,
    "Longitude": -75.5705
  },
  {
    "Date": "2025-03-29",
    "Location": "Goleta, CA",
    "Host": "All Fur Fun",
    "TrialTypes": "ELT-P, NW3",
    "EventCount": 2,
    "Latitude": 34.4208,
    "Longitude": -119.8048
  },
  {
    "Date": "2025-03-29",
    "Location": "Kennett Square, PA",
    "Host": "The Sniffing Hound",
    "TrialTypes": "ELT-S, ELT",
    "EventCount": 2,
    "Latitude": 39.853,
    "Longitude": -75.7288
  },
  {
    "Date": "2025-03-29",
    "Location": "LeRoy, IL",
    "Host": "Kudos for Canines",
    "TrialTypes": "NW3, L1C, L2I",
    "EventCount": 3,
    "Latitude": 42.4106,
    "Longitude": -88.787
  },
  {
    "Date": "2025-03-29",
    "Location": "Olathe, KS",
    "Host": "Brookside Pet Training Studio for Dogs",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 38.8661,
    "Longitude": -94.8481
  },
  {
    "Date": "2025-03-30",
    "Location": "East Windsor, CT",
    "Host": "Lucky Dog Events",
    "TrialTypes": "L2V, NW2, NW3",
    "EventCount": 3,
    "Latitude": 41.919,
    "Longitude": -72.6625
  },
  {
    "Date": "2025-04-03",
    "Location": "Alpharetta, GA",
    "Host": "Georgia Nosework",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 34.0282,
    "Longitude": -84.2917
  },
  {
    "Date": "2025-04-04",
    "Location": "Easton, MD",
    "Host": "Fair Play Labradors",
    "TrialTypes": "SMT, L1V, L2V",
    "EventCount": 3,
    "Latitude": 38.7943,
    "Longitude": -76.0851
  },
  {
    "Date": "2025-04-05",
    "Location": "Genoa, IL",
    "Host": "Common Scents K9 Scent Work Club of Elgin",
    "TrialTypes": "NW2, NW3",
    "EventCount": 2,
    "Latitude": 42.1291,
    "Longitude": -88.7417
  },
  {
    "Date": "2025-04-05",
    "Location": "Kittanning, PA",
    "Host": "Steel City Nosework, LLC",
    "TrialTypes": "ELT, ELT-S, NW2",
    "EventCount": 3,
    "Latitude": 40.789,
    "Longitude": -79.5152
  },
  {
    "Date": "2025-04-05",
    "Location": "Maple Falls, WA",
    "Host": "Nose Work Magic",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 48.9582,
    "Longitude": -122.129
  },
  {
    "Date": "2025-04-05",
    "Location": "North Java, NY",
    "Host": "Do Over Dog Training",
    "TrialTypes": "ELT-S, L2C, L3E",
    "EventCount": 3,
    "Latitude": 42.6432,
    "Longitude": -78.3097
  },
  {
    "Date": "2025-04-05",
    "Location": "Somis, CA",
    "Host": "Pink Biscuit K9s",
    "TrialTypes": "ELT, ELT-S, NW1",
    "EventCount": 3,
    "Latitude": 34.2591,
    "Longitude": -118.9481
  },
  {
    "Date": "2025-04-05",
    "Location": "Tucson, AZ",
    "Host": "Patience Unlimited Professional Dog Training",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 32.1762,
    "Longitude": -110.995
  },
  {
    "Date": "2025-04-05",
    "Location": "Woodstock, IL",
    "Host": "Northwest Obedience Club Inc.",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 42.3037,
    "Longitude": -88.4365
  },
  {
    "Date": "2025-04-11",
    "Location": "Sequim, WA",
    "Host": "Sarah Becker, Sea Change Canine LLC & Carol Forsberg",
    "TrialTypes": "NW2, NW1",
    "EventCount": 2,
    "Latitude": 48.0473,
    "Longitude": -123.145
  },
  {
    "Date": "2025-04-12",
    "Location": "Auburn, WA",
    "Host": "Northwest K9 Sniffers",
    "TrialTypes": "NW3, L1C, L1E",
    "EventCount": 3,
    "Latitude": 47.3005,
    "Longitude": -122.2208
  },
  {
    "Date": "2025-04-12",
    "Location": "Boone, IA",
    "Host": "KBP Dog Training",
    "TrialTypes": "ELT, NW2",
    "EventCount": 2,
    "Latitude": 42.0029,
    "Longitude": -93.9541
  },
  {
    "Date": "2025-04-12",
    "Location": "Burton, OH",
    "Host": "Barns And Noses, LLC",
    "TrialTypes": "ELT, ELT-S, NW1",
    "EventCount": 3,
    "Latitude": 41.4966,
    "Longitude": -81.1479
  },
  {
    "Date": "2025-04-12",
    "Location": "Carlisle, PA",
    "Host": "Nose-It-All, LLC",
    "TrialTypes": "ELT, ELT-S, L2E",
    "EventCount": 3,
    "Latitude": 40.1567,
    "Longitude": -77.2362
  },
  {
    "Date": "2025-04-12",
    "Location": "Laramie, WY",
    "Host": "Mountain Dogs, LLC",
    "TrialTypes": "ELT, NW3, ELT-S, NW2",
    "EventCount": 4,
    "Latitude": 41.291,
    "Longitude": -105.5512
  },
  {
    "Date": "2025-04-12",
    "Location": "Michigan City, IN",
    "Host": "Indiana Scentwork",
    "TrialTypes": "NW3, ELT-S, NW2",
    "EventCount": 3,
    "Latitude": 41.7376,
    "Longitude": -86.8471
  },
  {
    "Date": "2025-04-12",
    "Location": "Peekskill, NY",
    "Host": "For the Love of Dogs NY, LLC",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 41.2721,
    "Longitude": -73.8973
  },
  {
    "Date": "2025-04-12",
    "Location": "Rhinebeck, NY",
    "Host": "Top Notch Dogs, LLC",
    "TrialTypes": "ELT, L1C, L2C",
    "EventCount": 3,
    "Latitude": 41.9613,
    "Longitude": -73.9145
  },
  {
    "Date": "2025-04-12",
    "Location": "Starke, FL",
    "Host": "River Poodles Training, LLC",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 29.9762,
    "Longitude": -82.0738
  },
  {
    "Date": "2025-04-14",
    "Location": "Sacramento, CA",
    "Host": "Two Nosey Girls",
    "TrialTypes": "L2E, L3E, ELT",
    "EventCount": 3,
    "Latitude": 38.5721,
    "Longitude": -121.5277
  },
  {
    "Date": "2025-04-18",
    "Location": "Asheboro, NC",
    "Host": "K9 Nose Adventures, LLC",
    "TrialTypes": "ELT, NW3, ELT-S, L2C",
    "EventCount": 4,
    "Latitude": 35.6789,
    "Longitude": -79.8185
  },
  {
    "Date": "2025-04-18",
    "Location": "Grand Junction, CO",
    "Host": "Mountain Dogs, LLC",
    "TrialTypes": "ELT, NW3, NW1, NW2",
    "EventCount": 4,
    "Latitude": 39.1134,
    "Longitude": -108.6007
  },
  {
    "Date": "2025-04-18",
    "Location": "Palmer, MA",
    "Host": "HeavenScent Sniffers",
    "TrialTypes": "ELT, NW3, NW1, NW2",
    "EventCount": 4,
    "Latitude": 42.1994,
    "Longitude": -72.3652
  },
  {
    "Date": "2025-04-18",
    "Location": "Rochester, NY",
    "Host": "Tami Sullivan",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 43.1688,
    "Longitude": -77.6165
  },
  {
    "Date": "2025-04-19",
    "Location": "Brooksville, FL",
    "Host": "Hoppin’ in the Hills",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 28.5965,
    "Longitude": -82.3707
  },
  {
    "Date": "2025-04-19",
    "Location": "Kunkletown, PA",
    "Host": "Paws n' Sniff",
    "TrialTypes": "NW3, L3E, ELT-S",
    "EventCount": 3,
    "Latitude": 40.8015,
    "Longitude": -75.4837
  },
  {
    "Date": "2025-04-24",
    "Location": "Concord, CA",
    "Host": "The Bay Team",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 37.9722,
    "Longitude": -122.034
  },
  {
    "Date": "2025-04-25",
    "Location": "Eagan, MN",
    "Host": "St Paul Dog Training Club",
    "TrialTypes": "ELT, NW2, L2C, L3I",
    "EventCount": 4,
    "Latitude": 44.7983,
    "Longitude": -93.1328
  },
  {
    "Date": "2025-04-26",
    "Location": "Decatur, GA",
    "Host": "Right Choice Dog Training LLC",
    "TrialTypes": "L1C, L1I",
    "EventCount": 2,
    "Latitude": 30.8732,
    "Longitude": -84.5384
  },
  {
    "Date": "2025-04-26",
    "Location": "Ellicottville, NY",
    "Host": "Do Over Dog Training",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 42.3247,
    "Longitude": -78.6353
  },
  {
    "Date": "2025-04-26",
    "Location": "FT. Pierce, FL",
    "Host": "Obedience Training Club of Palm Beach County",
    "TrialTypes": "L1E, NW2, NW1, L1C",
    "EventCount": 4,
    "Latitude": 27.4489,
    "Longitude": -80.3652
  },
  {
    "Date": "2025-04-26",
    "Location": "Greenfield, WI",
    "Host": "Loving Paws Dog Training LLC",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 42.9437,
    "Longitude": -87.9957
  },
  {
    "Date": "2025-04-26",
    "Location": "Hammond, LA",
    "Host": "Dog Gone Right",
    "TrialTypes": "NW1, NW2, ELT",
    "EventCount": 3,
    "Latitude": 30.5289,
    "Longitude": -90.4942
  },
  {
    "Date": "2025-04-26",
    "Location": "Kingston, NH",
    "Host": "The Big Sniff, LLC",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 42.9115,
    "Longitude": -71.084
  },
  {
    "Date": "2025-04-26",
    "Location": "Lyons, OR",
    "Host": "Canine Discovery Corps",
    "TrialTypes": "L1I, L2C, ELT",
    "EventCount": 3,
    "Latitude": 44.7653,
    "Longitude": -122.6455
  },
  {
    "Date": "2025-04-26",
    "Location": "Newtown, PA",
    "Host": "K9 Nosen Around, LLC",
    "TrialTypes": "L1V, NW1, L2V, NW2",
    "EventCount": 4,
    "Latitude": 40.2014,
    "Longitude": -74.9611
  },
  {
    "Date": "2025-04-26",
    "Location": "Northampton, MA",
    "Host": "Lucky Dog Events",
    "TrialTypes": "ELT-P, ELT-S, L1C",
    "EventCount": 3,
    "Latitude": 42.2818,
    "Longitude": -72.6736
  },
  {
    "Date": "2025-04-26",
    "Location": "Ocoee, TN",
    "Host": "Camelot Shepherds, Inc.",
    "TrialTypes": "NW1, NW2, NW3",
    "EventCount": 3,
    "Latitude": 35.1134,
    "Longitude": -84.7476
  },
  {
    "Date": "2025-04-26",
    "Location": "Red Feather Lakes, CO",
    "Host": "Beyond Elevation K9",
    "TrialTypes": "NW2, NW3",
    "EventCount": 2,
    "Latitude": 40.8417,
    "Longitude": -105.5943
  },
  {
    "Date": "2025-04-26",
    "Location": "Traverse City, MI",
    "Host": "Every Dog Nosework",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 44.7632,
    "Longitude": -85.5847
  },
  {
    "Date": "2025-04-26",
    "Location": "West Friendship, MD",
    "Host": "Red Huskies",
    "TrialTypes": "NW1, NW2, L2V, L1C",
    "EventCount": 4,
    "Latitude": 39.3467,
    "Longitude": -76.9177
  },
  {
    "Date": "2025-05-01",
    "Location": "Gainesville, GA",
    "Host": "Georgia Nosework",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 34.3163,
    "Longitude": -83.8597
  },
  {
    "Date": "2025-05-02",
    "Location": "Faribault, MN",
    "Host": "St Paul Dog Training Club",
    "TrialTypes": "NW3, ELT-P, L3E, L3C",
    "EventCount": 4,
    "Latitude": 43.7091,
    "Longitude": -93.9348
  },
  {
    "Date": "2025-05-02",
    "Location": "Nyack, NY",
    "Host": "Waggin Work",
    "TrialTypes": "NW3, ELT, ELT-S",
    "EventCount": 3,
    "Latitude": 41.067,
    "Longitude": -73.896
  },
  {
    "Date": "2025-05-03",
    "Location": "Alexis, IL",
    "Host": "Kudos for Canines",
    "TrialTypes": "NW3, ELT-P",
    "EventCount": 2,
    "Latitude": 41.084,
    "Longitude": -90.5551
  },
  {
    "Date": "2025-05-03",
    "Location": "Ashby, MA",
    "Host": "Carolyn Barney dba Dogs!",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 42.6947,
    "Longitude": -71.8446
  },
  {
    "Date": "2025-05-03",
    "Location": "Columbus, MT",
    "Host": "Canine Connection",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 45.6681,
    "Longitude": -109.2998
  },
  {
    "Date": "2025-05-03",
    "Location": "Gray Court, SC",
    "Host": "Foothills Canine Academy, LLC",
    "TrialTypes": "L1V, NW1, NW3",
    "EventCount": 3,
    "Latitude": 34.5791,
    "Longitude": -82.1133
  },
  {
    "Date": "2025-05-03",
    "Location": "Redwood City, CA",
    "Host": "B. L. McMutts",
    "TrialTypes": "NW2, NW1",
    "EventCount": 2,
    "Latitude": 37.5163,
    "Longitude": -122.2429
  },
  {
    "Date": "2025-05-03",
    "Location": "Sandy, OR",
    "Host": "Trust Your Dog K9 Events",
    "TrialTypes": "NW2, NW3",
    "EventCount": 2,
    "Latitude": 45.4443,
    "Longitude": -122.2601
  },
  {
    "Date": "2025-05-03",
    "Location": "Santa Paula, CA",
    "Host": "Pink Biscuit K9s",
    "TrialTypes": "ELT, NW1, NW2",
    "EventCount": 3,
    "Latitude": 34.3322,
    "Longitude": -119.0306
  },
  {
    "Date": "2025-05-03",
    "Location": "White Salmon, WA",
    "Host": "Trisha Thompson and Sharon Smith",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 45.7722,
    "Longitude": -121.4916
  },
  {
    "Date": "2025-05-09",
    "Location": "South Sterling, PA",
    "Host": "Paws n' Sniff",
    "TrialTypes": "L3C, L2I, NW2",
    "EventCount": 3,
    "Latitude": 41.2241,
    "Longitude": -75.3676
  },
  {
    "Date": "2025-05-09",
    "Location": "Warwick, NY",
    "Host": "Top Notch Dogs, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 41.2658,
    "Longitude": -74.3782
  },
  {
    "Date": "2025-05-10",
    "Location": "Alexander, NY",
    "Host": "Do Over Dog Training",
    "TrialTypes": "L1V, L2I, L1C, L3V",
    "EventCount": 4,
    "Latitude": 42.87,
    "Longitude": -78.2729
  },
  {
    "Date": "2025-05-10",
    "Location": "Denton, MD",
    "Host": "Fair Play Labradors",
    "TrialTypes": "NW3, ELT-S",
    "EventCount": 2,
    "Latitude": 38.847,
    "Longitude": -75.8178
  },
  {
    "Date": "2025-05-10",
    "Location": "Poland Springs, ME",
    "Host": "Virginia Howe",
    "TrialTypes": "NW2, ELT",
    "EventCount": 2,
    "Latitude": 44.0132,
    "Longitude": -70.3476
  },
  {
    "Date": "2025-05-10",
    "Location": "Rainier, WA",
    "Host": "Rachelle Bailey-Austin/About Face K9 Academy & Dorothy Turley/Let's Talk Dogs, LLC",
    "TrialTypes": "ELT-S, NW2",
    "EventCount": 2,
    "Latitude": 46.9117,
    "Longitude": -122.6402
  },
  {
    "Date": "2025-05-10",
    "Location": "Santa Barbara, CA",
    "Host": "All Fur Fun",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 34.4563,
    "Longitude": -119.7334
  },
  {
    "Date": "2025-05-13",
    "Location": "Paso Robles, CA",
    "Host": "Gentle Touch Pet Training",
    "TrialTypes": "L2C, NW1",
    "EventCount": 2,
    "Latitude": 35.58,
    "Longitude": -120.7289
  },
  {
    "Date": "2025-05-16",
    "Location": "Montrose, CO",
    "Host": "Mountain Dogs, LLC",
    "TrialTypes": "ELT, NW3, ELT-P",
    "EventCount": 3,
    "Latitude": 38.4314,
    "Longitude": -107.8865
  },
  {
    "Date": "2025-05-16",
    "Location": "Watsonville, CA",
    "Host": "CalCoastal Dog Owners Group",
    "TrialTypes": "ELT-S, L2C, L3C",
    "EventCount": 3,
    "Latitude": 36.8822,
    "Longitude": -121.7702
  },
  {
    "Date": "2025-05-17",
    "Location": "Burien, WA",
    "Host": "Northwest K9 Sniffers",
    "TrialTypes": "ELT, L2V, L2I",
    "EventCount": 3,
    "Latitude": 47.502,
    "Longitude": -122.3813
  },
  {
    "Date": "2025-05-17",
    "Location": "Cobleskill, NY",
    "Host": "The Brainy Canine",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 42.7242,
    "Longitude": -74.4466
  },
  {
    "Date": "2025-05-17",
    "Location": "Emmitsburg, MD",
    "Host": "Red Huskies",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 39.6585,
    "Longitude": -77.2773
  },
  {
    "Date": "2025-05-17",
    "Location": "Forest Junction, WI",
    "Host": "N.E.W K9 Scent Work LLC",
    "TrialTypes": "L1C, NW1, NW2",
    "EventCount": 3,
    "Latitude": 44.2284,
    "Longitude": -88.1205
  },
  {
    "Date": "2025-05-17",
    "Location": "Norton, MA",
    "Host": "Dogs Make Scents",
    "TrialTypes": "NW3, NW1",
    "EventCount": 2,
    "Latitude": 41.9721,
    "Longitude": -71.1711
  },
  {
    "Date": "2025-05-17",
    "Location": "Peru, MA",
    "Host": "Lucky Dog Events",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 42.4576,
    "Longitude": -73.0202
  },
  {
    "Date": "2025-05-17",
    "Location": "Valley Forge, PA",
    "Host": "Sniff Sniff Hooray",
    "TrialTypes": "NW1, ELT",
    "EventCount": 2,
    "Latitude": 40.0955,
    "Longitude": -75.4582
  },
  {
    "Date": "2025-05-23",
    "Location": "La Jolla, CA",
    "Host": "Anita Cheesman and Jessica Koester",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 32.8318,
    "Longitude": -117.2773
  },
  {
    "Date": "2025-05-24",
    "Location": "Altamont , NY",
    "Host": "My Dog Smells LLC",
    "TrialTypes": "ELT, NW2",
    "EventCount": 2,
    "Latitude": 42.6915,
    "Longitude": -74.0096
  },
  {
    "Date": "2025-05-24",
    "Location": "Batavia, NY",
    "Host": "Do Over Dog Training",
    "TrialTypes": "ELT-P, L1I, L3I",
    "EventCount": 3,
    "Latitude": 43.0001,
    "Longitude": -78.166
  },
  {
    "Date": "2025-05-24",
    "Location": "Lancaster, PA",
    "Host": "Nose-It-All, LLC",
    "TrialTypes": "L3I, NW2, ELT",
    "EventCount": 3,
    "Latitude": 40.064,
    "Longitude": -76.3394
  },
  {
    "Date": "2025-05-24",
    "Location": "Norwich , CT",
    "Host": "For the Love of Dogs NY LLC",
    "TrialTypes": "ELT-S, NW2, NW3, ELT",
    "EventCount": 4,
    "Latitude": 41.5514,
    "Longitude": -72.0582
  },
  {
    "Date": "2025-05-24",
    "Location": "Rockaway, NJ",
    "Host": "Shamrock Pot of Gold K9 Scenter",
    "TrialTypes": "NW3, ELT-S, NW1, ELT-P",
    "EventCount": 4,
    "Latitude": 40.8601,
    "Longitude": -74.55
  },
  {
    "Date": "2025-05-24",
    "Location": "Waukesha , WI",
    "Host": "Think Pawsitive Dog Training",
    "TrialTypes": "NW1, L1V, L1C",
    "EventCount": 3,
    "Latitude": 43.112,
    "Longitude": -88.3069
  },
  {
    "Date": "2025-05-24",
    "Location": "Welches, OR",
    "Host": "Trust Your Dog K9 Events",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 45.3075,
    "Longitude": -121.9747
  },
  {
    "Date": "2025-05-29",
    "Location": "Bayfield, CO",
    "Host": "Wag Between Barks",
    "TrialTypes": "ELT-S, ELT, NW3",
    "EventCount": 3,
    "Latitude": 37.2752,
    "Longitude": -107.5609
  },
  {
    "Date": "2025-05-30",
    "Location": "Honesdale, PA",
    "Host": "Your Dog's Place, LLC",
    "TrialTypes": "NW3, ELT, ELT-S, NW2",
    "EventCount": 4,
    "Latitude": 41.5405,
    "Longitude": -75.226
  },
  {
    "Date": "2025-05-30",
    "Location": "Moline, IL",
    "Host": "Fur Better Fur Worse Dog Training",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 41.4886,
    "Longitude": -90.4736
  },
  {
    "Date": "2025-05-31",
    "Location": "Amherst, NY",
    "Host": "Do Over Dog Training",
    "TrialTypes": "NW2, ELT-S, NW1",
    "EventCount": 3,
    "Latitude": 42.9983,
    "Longitude": -78.7944
  },
  {
    "Date": "2025-05-31",
    "Location": "Columbus, MT",
    "Host": "Canine Connection",
    "TrialTypes": "L1V, NW1, NW3",
    "EventCount": 3,
    "Latitude": 45.5871,
    "Longitude": -109.2701
  },
  {
    "Date": "2025-05-31",
    "Location": "Eden Prairie, MN",
    "Host": "The K9 Nose",
    "TrialTypes": "NW1, L1I, L2I",
    "EventCount": 3,
    "Latitude": 44.8568,
    "Longitude": -93.52
  },
  {
    "Date": "2025-05-31",
    "Location": "Napa, CA",
    "Host": "Napa Valley Dog Training Club",
    "TrialTypes": "NW2, NW1",
    "EventCount": 2,
    "Latitude": 38.4548,
    "Longitude": -122.3086
  },
  {
    "Date": "2025-05-31",
    "Location": "New Wilmington, PA",
    "Host": "Steel City Nosework, LLC",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 41.1538,
    "Longitude": -80.326
  },
  {
    "Date": "2025-05-31",
    "Location": "North Manchester, IN",
    "Host": "2 Nose You Is 2 Loves You",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 40.9877,
    "Longitude": -85.7504
  },
  {
    "Date": "2025-06-06",
    "Location": "Pueblo, CO",
    "Host": "Mountain Dogs, LLC",
    "TrialTypes": "SMT, ELT",
    "EventCount": 2,
    "Latitude": 38.2784,
    "Longitude": -104.6358
  },
  {
    "Date": "2025-06-06",
    "Location": "Winsted, CT",
    "Host": "Waggin’ Work",
    "TrialTypes": "ELT, NW2, L2C",
    "EventCount": 3,
    "Latitude": 41.9061,
    "Longitude": -73.0316
  },
  {
    "Date": "2025-06-07",
    "Location": "Clancy, MT",
    "Host": "Nosework Breakfast Club",
    "TrialTypes": "NW2, NW3",
    "EventCount": 2,
    "Latitude": 46.501,
    "Longitude": -112.0123
  },
  {
    "Date": "2025-06-07",
    "Location": "Cumming, GA",
    "Host": "Georgia Nosework",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 34.1589,
    "Longitude": -84.1134
  },
  {
    "Date": "2025-06-07",
    "Location": "Davenport, IA",
    "Host": "Fur Better Fur Worse Dog Training",
    "TrialTypes": "ELT-S, NW2, NW1",
    "EventCount": 3,
    "Latitude": 41.4861,
    "Longitude": -90.5482
  },
  {
    "Date": "2025-06-07",
    "Location": "Enterprise, OR",
    "Host": "Country K9 Nosework, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 45.4324,
    "Longitude": -117.2721
  },
  {
    "Date": "2025-06-07",
    "Location": "Grants Pass, OR",
    "Host": "Nose Work Detectives",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 42.46,
    "Longitude": -123.2953
  },
  {
    "Date": "2025-06-07",
    "Location": "Meadowbrook, PA",
    "Host": "Patricia Grassey",
    "TrialTypes": "NW1, NW2, ELT-S, ELT",
    "EventCount": 4,
    "Latitude": 40.0918,
    "Longitude": -75.1327
  },
  {
    "Date": "2025-06-07",
    "Location": "Palmyra, VA",
    "Host": "Your Dog Knows, LLC",
    "TrialTypes": "NW1, L1I",
    "EventCount": 2,
    "Latitude": 37.8958,
    "Longitude": -78.239
  },
  {
    "Date": "2025-06-07",
    "Location": "Wrightstown, WI",
    "Host": "N.E.W. K9 Scent Work, LLC",
    "TrialTypes": "ELT-P, L2C, L3I",
    "EventCount": 3,
    "Latitude": 44.2927,
    "Longitude": -88.186
  },
  {
    "Date": "2025-06-13",
    "Location": "Jordan, MN",
    "Host": "St Paul Dog Training Club",
    "TrialTypes": "NW3, ELT-S, L1V, L2E, L3V",
    "EventCount": 5,
    "Latitude": 44.6378,
    "Longitude": -93.6434
  },
  {
    "Date": "2025-06-14",
    "Location": "Cummington, MA",
    "Host": "Lucky Dog Events",
    "TrialTypes": "ELT-P, ELT-S, L2C",
    "EventCount": 3,
    "Latitude": 42.4305,
    "Longitude": -72.8652
  },
  {
    "Date": "2025-06-14",
    "Location": "Danvers, MA",
    "Host": "Everydog, LLC",
    "TrialTypes": "L2I, NW2, NW1",
    "EventCount": 3,
    "Latitude": 42.5317,
    "Longitude": -70.9854
  },
  {
    "Date": "2025-06-14",
    "Location": "Ithaca, NY",
    "Host": "The Brainy Canine",
    "TrialTypes": "ELT-P, ELT",
    "EventCount": 2,
    "Latitude": 42.4289,
    "Longitude": -76.5462
  },
  {
    "Date": "2025-06-14",
    "Location": "Linden, MI",
    "Host": "Every Dog Nosework",
    "TrialTypes": "NW1, ELT-P",
    "EventCount": 2,
    "Latitude": 42.8111,
    "Longitude": -83.7885
  },
  {
    "Date": "2025-06-20",
    "Location": "Greeley, CO",
    "Host": "Beyond Elevation K9",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 40.4204,
    "Longitude": -104.6764
  },
  {
    "Date": "2025-06-20",
    "Location": "San Luis Obispo, CA",
    "Host": "Central Coast Nosework Club of California, Inc.",
    "TrialTypes": "NW2, ELT",
    "EventCount": 2,
    "Latitude": 35.358,
    "Longitude": -120.39
  },
  {
    "Date": "2025-06-20",
    "Location": "Upland, CA",
    "Host": "Agile Paws Dog Sports",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 34.1158,
    "Longitude": -117.6056
  },
  {
    "Date": "2025-06-20",
    "Location": "Warwick, NY",
    "Host": "For the Love of Dogs NY LLC",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 41.2416,
    "Longitude": -74.36
  },
  {
    "Date": "2025-06-21",
    "Location": "Fayette, MO",
    "Host": "Columbia Canine Sports Center",
    "TrialTypes": "ELT, NW1, NW2",
    "EventCount": 3,
    "Latitude": 39.1896,
    "Longitude": -92.6603
  },
  {
    "Date": "2025-06-21",
    "Location": "Inver Grove Heights, MN",
    "Host": "Outside The Box Dog Training, LLC",
    "TrialTypes": "L1C, NW2, ELT",
    "EventCount": 3,
    "Latitude": 44.8564,
    "Longitude": -92.997
  },
  {
    "Date": "2025-06-21",
    "Location": "Jefferson, WI",
    "Host": "Think Pawsitive Dog Training",
    "TrialTypes": "NW2",
    "EventCount": 1,
    "Latitude": 42.9725,
    "Longitude": -88.7773
  },
  {
    "Date": "2025-06-21",
    "Location": "Toledo, OH",
    "Host": "Robin Ford Dog Training, LLC",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 41.6879,
    "Longitude": -83.5839
  },
  {
    "Date": "2025-06-21",
    "Location": "Woodstock, GA",
    "Host": "Georgia Nosework LLC",
    "TrialTypes": "NW3, L2C, NW1",
    "EventCount": 3,
    "Latitude": 34.0939,
    "Longitude": -84.5687
  },
  {
    "Date": "2025-06-25",
    "Location": "Kenai, AK",
    "Host": "Peninsula Dog Obedience Group",
    "TrialTypes": "NW1, NW2, NW3, ELT",
    "EventCount": 4,
    "Latitude": 60.5855,
    "Longitude": -151.2967
  },
  {
    "Date": "2025-06-28",
    "Location": "Delran, NJ",
    "Host": "K9 InScentives",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 39.994,
    "Longitude": -74.964
  },
  {
    "Date": "2025-06-28",
    "Location": "Deming, WA",
    "Host": "Nose Work Magic",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 48.8812,
    "Longitude": -122.231
  },
  {
    "Date": "2025-06-28",
    "Location": "Kenosha, WI",
    "Host": "Loving Paws Dog Training LLC",
    "TrialTypes": "NW3, NW1",
    "EventCount": 2,
    "Latitude": 42.5763,
    "Longitude": -87.7989
  },
  {
    "Date": "2025-06-28",
    "Location": "Somers, CT",
    "Host": "HeavenScent Sniffers",
    "TrialTypes": "ELT, L1V, NW1",
    "EventCount": 3,
    "Latitude": 41.961,
    "Longitude": -72.4898
  },
  {
    "Date": "2025-06-28",
    "Location": "St. Paul, MN",
    "Host": "Bark and Bond LLC",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 44.9814,
    "Longitude": -93.1051
  },
  {
    "Date": "2025-06-28",
    "Location": "Stevenson, WA",
    "Host": "Sharon Smith",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 45.71,
    "Longitude": -121.8684
  },
  {
    "Date": "2025-07-04",
    "Location": "Huntington, MA",
    "Host": "Shamrock Pot Of Gold K9 Scenter",
    "TrialTypes": "ELT, NW3, L2I, ELT-S",
    "EventCount": 4,
    "Latitude": 42.2185,
    "Longitude": -72.8811
  },
  {
    "Date": "2025-07-05",
    "Location": "Delran, NJ",
    "Host": "Ev-ry Earthdog, LLC",
    "TrialTypes": "NW3, NW1, NW2, ELT",
    "EventCount": 4,
    "Latitude": 39.9929,
    "Longitude": -74.9996
  },
  {
    "Date": "2025-07-11",
    "Location": "Leadville, CO",
    "Host": "Mountain Dogs LLC",
    "TrialTypes": "ELT, NW3, ELT-S, NW2",
    "EventCount": 4,
    "Latitude": 39.2363,
    "Longitude": -106.3101
  },
  {
    "Date": "2025-07-12",
    "Location": "Brainerd, MN",
    "Host": "Nose 2 Tail Dog Training LLC",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 46.3598,
    "Longitude": -94.1578
  },
  {
    "Date": "2025-07-12",
    "Location": "Livonia, MI",
    "Host": "Every Dog Nosework",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 42.3657,
    "Longitude": -83.3104
  },
  {
    "Date": "2025-07-18",
    "Location": "Leadville, CO",
    "Host": "Mountain Dogs, LLC",
    "TrialTypes": "NW3, NW1, NW2, L1C, L1I",
    "EventCount": 5,
    "Latitude": 39.2873,
    "Longitude": -106.2886
  },
  {
    "Date": "2025-07-19",
    "Location": "Dunmore, PA",
    "Host": "Your Dog's Place, LLC",
    "TrialTypes": "ELT-S, NW1, L1C",
    "EventCount": 3,
    "Latitude": 41.4134,
    "Longitude": -75.6088
  },
  {
    "Date": "2025-07-19",
    "Location": "Fayette , MO",
    "Host": "Columbia Canine Sports Center",
    "TrialTypes": "NW3, L1C, L1I",
    "EventCount": 3,
    "Latitude": 39.1837,
    "Longitude": -92.69
  },
  {
    "Date": "2025-07-19",
    "Location": "Houlton, WI",
    "Host": "Gretchen Hofheins-Wackerfuss",
    "TrialTypes": "NW1, ELT-S, ELT-P",
    "EventCount": 3,
    "Latitude": 45.0609,
    "Longitude": -92.8149
  },
  {
    "Date": "2025-07-19",
    "Location": "Walpole, MA",
    "Host": "MasterPeace Dog Training",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 42.1291,
    "Longitude": -71.2285
  },
  {
    "Date": "2025-07-21",
    "Location": "Montgomery, NY",
    "Host": "For the Love of Dogs NY LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 42.8725,
    "Longitude": -74.3666
  },
  {
    "Date": "2025-08-02",
    "Location": "Altamont, IL",
    "Host": "Kudos for Canines, LLC",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 39.0828,
    "Longitude": -88.775
  },
  {
    "Date": "2025-08-02",
    "Location": "Anchorage, AK",
    "Host": "Alaska Dog Sports, LLC",
    "TrialTypes": "NW3, NW2, NW1",
    "EventCount": 3,
    "Latitude": 61.2523,
    "Longitude": -149.8812
  },
  {
    "Date": "2025-08-02",
    "Location": "Bettendorf, IA",
    "Host": "Fur Better Fur Worse Dog Training",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 41.5054,
    "Longitude": -90.5347
  },
  {
    "Date": "2025-08-02",
    "Location": "Jefferson, WI",
    "Host": "Think Pawsitive Dog Training LLC",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 43.0721,
    "Longitude": -88.7839
  },
  {
    "Date": "2025-08-02",
    "Location": "Pillager, MN",
    "Host": "Nose 2 Tail Dog Training, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 46.3689,
    "Longitude": -94.4733
  },
  {
    "Date": "2025-08-02",
    "Location": "Red Lodge, MT",
    "Host": "Canine Connection",
    "TrialTypes": "L1I, ELT-S, ELT",
    "EventCount": 3,
    "Latitude": 45.2296,
    "Longitude": -109.1972
  },
  {
    "Date": "2025-08-02",
    "Location": "Rochester, NY",
    "Host": "Suzan Tessier",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 43.1622,
    "Longitude": -77.6531
  },
  {
    "Date": "2025-08-11",
    "Location": "Cambria, CA",
    "Host": "Gentle Touch Pet Training",
    "TrialTypes": "L1C, L2I, ELT",
    "EventCount": 3,
    "Latitude": 35.526,
    "Longitude": -121.122
  },
  {
    "Date": "2025-08-15",
    "Location": "Huntington Beach, CA",
    "Host": "JavaK9s, LLC",
    "TrialTypes": "ELT-P, L1C, L1I",
    "EventCount": 3,
    "Latitude": 33.7045,
    "Longitude": -118.0423
  },
  {
    "Date": "2025-08-16",
    "Location": "Colesville , MD",
    "Host": "Red Huskies",
    "TrialTypes": "L3C, ELT-S, NW2, ELT",
    "EventCount": 4,
    "Latitude": 39.0722,
    "Longitude": -77.0283
  },
  {
    "Date": "2025-08-16",
    "Location": "Mount Kisco, NY",
    "Host": "For the Love of Dogs, LLC",
    "TrialTypes": "L2I, NW2, L1I, NW1",
    "EventCount": 4,
    "Latitude": 41.1695,
    "Longitude": -73.7759
  },
  {
    "Date": "2025-08-16",
    "Location": "Reedsport, OR",
    "Host": "Kiddy Christie",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 43.6534,
    "Longitude": -124.1134
  },
  {
    "Date": "2025-08-22",
    "Location": "Chelsea, MI",
    "Host": "Force Free Dale, LLC",
    "TrialTypes": "NW3, NW2, NW1",
    "EventCount": 3,
    "Latitude": 42.3509,
    "Longitude": -84.0572
  },
  {
    "Date": "2025-08-23",
    "Location": "Gervais, OR",
    "Host": "Helix Fairweather & Doglandia, LLC",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 45.1136,
    "Longitude": -122.8953
  },
  {
    "Date": "2025-08-23",
    "Location": "Johnstown, NY",
    "Host": "My Dog Smells, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 42.975,
    "Longitude": -74.3658
  },
  {
    "Date": "2025-08-23",
    "Location": "Tyngsborough, MA",
    "Host": "Spot-On K9 Coaching",
    "TrialTypes": "NW3, L1C, L1I",
    "EventCount": 3,
    "Latitude": 42.6774,
    "Longitude": -71.4574
  },
  {
    "Date": "2025-08-29",
    "Location": "Bridger, MT",
    "Host": "Canine Connection",
    "TrialTypes": "NW1, L2I, NW3",
    "EventCount": 3,
    "Latitude": 45.3083,
    "Longitude": -108.9604
  },
  {
    "Date": "2025-08-30",
    "Location": "Eliot, ME",
    "Host": "McLean Pups, LLC",
    "TrialTypes": "L1V, L1E",
    "EventCount": 2,
    "Latitude": 43.0826,
    "Longitude": -70.825
  },
  {
    "Date": "2025-08-30",
    "Location": "Fort Worth, TX",
    "Host": "North Texas Nosework Club",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 32.7543,
    "Longitude": -97.3389
  },
  {
    "Date": "2025-08-30",
    "Location": "Pomfret Center, CT",
    "Host": "HeavenScent Sniffers",
    "TrialTypes": "NW3, NW2, NW1",
    "EventCount": 3,
    "Latitude": 41.8869,
    "Longitude": -71.9303
  },
  {
    "Date": "2025-08-31",
    "Location": "Helena, MT",
    "Host": "Nosework Breakfast Club",
    "TrialTypes": "NW2, NW3",
    "EventCount": 2,
    "Latitude": 46.6395,
    "Longitude": -112.0188
  },
  {
    "Date": "2025-09-05",
    "Location": "Centreville, MD",
    "Host": "Fair Play Labradors",
    "TrialTypes": "ELT, NW3, ELT-S",
    "EventCount": 3,
    "Latitude": 39.056,
    "Longitude": -76.0786
  },
  {
    "Date": "2025-09-06",
    "Location": "Dunkirk, NY",
    "Host": "Do Over Dog Training",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 42.4584,
    "Longitude": -79.3489
  },
  {
    "Date": "2025-09-06",
    "Location": "Loma Mar, CA",
    "Host": "The Bay Team",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 37.2676,
    "Longitude": -122.2603
  },
  {
    "Date": "2025-09-06",
    "Location": "North Bend, WA",
    "Host": "Northwest K9 Sniffers",
    "TrialTypes": "NW2, L2E, L2C",
    "EventCount": 3,
    "Latitude": 47.4506,
    "Longitude": -121.7676
  },
  {
    "Date": "2025-09-12",
    "Location": "Frederick, MD",
    "Host": "Red Huskies",
    "TrialTypes": "NW3, ELT, ELT-S",
    "EventCount": 3,
    "Latitude": 39.4221,
    "Longitude": -77.3839
  },
  {
    "Date": "2025-09-12",
    "Location": "Honey Brook, PA",
    "Host": "Patricia Grassey",
    "TrialTypes": "ELT, ELT-S, NW3",
    "EventCount": 3,
    "Latitude": 40.0534,
    "Longitude": -75.9
  },
  {
    "Date": "2025-09-12",
    "Location": "Lakeville, MN",
    "Host": "St Paul Dog Training Club",
    "TrialTypes": "ELT, NW2, NW1",
    "EventCount": 3,
    "Latitude": 44.6049,
    "Longitude": -93.2918
  },
  {
    "Date": "2025-09-12",
    "Location": "New Milford, PA",
    "Host": "Your Dog's Place, LLC",
    "TrialTypes": "ELT-P, ELT-S, L2V, L1E",
    "EventCount": 4,
    "Latitude": 41.8963,
    "Longitude": -75.7148
  },
  {
    "Date": "2025-09-13",
    "Location": "Ames, IA",
    "Host": "KBP Dog Training",
    "TrialTypes": "ELT-S, ELT",
    "EventCount": 2,
    "Latitude": 42.0419,
    "Longitude": -93.6613
  },
  {
    "Date": "2025-09-13",
    "Location": "Colebrook, CT",
    "Host": "For the Love of Dogs, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 41.9722,
    "Longitude": -73.0852
  },
  {
    "Date": "2025-09-13",
    "Location": "Hermosa, SD",
    "Host": "Two Paws Up Dog Training, LLC",
    "TrialTypes": "NW3, L1C, L1I",
    "EventCount": 3,
    "Latitude": 43.8815,
    "Longitude": -103.2001
  },
  {
    "Date": "2025-09-13",
    "Location": "Jefferson, GA",
    "Host": "Right Choice Dog Training, LLC",
    "TrialTypes": "L2V, NW2, ELT-S, L1C",
    "EventCount": 4,
    "Latitude": 33.0135,
    "Longitude": -82.478
  },
  {
    "Date": "2025-09-13",
    "Location": "Pittsburgh , PA",
    "Host": "Steel City Nosework, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 40.4742,
    "Longitude": -79.9876
  },
  {
    "Date": "2025-09-15",
    "Location": "Green Lane, PA",
    "Host": "Sniff Sniff Hooray",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 40.3242,
    "Longitude": -75.4714
  },
  {
    "Date": "2025-09-19",
    "Location": "Flint, MI",
    "Host": "Every Dog Nosework",
    "TrialTypes": "NW3, ELT-S, L1C, NW2",
    "EventCount": 4,
    "Latitude": 43.0373,
    "Longitude": -83.6943
  },
  {
    "Date": "2025-09-19",
    "Location": "Fruita, CO",
    "Host": "Mountain Dogs LLC",
    "TrialTypes": "ELT, NW3, NW2",
    "EventCount": 3,
    "Latitude": 39.1992,
    "Longitude": -108.7048
  },
  {
    "Date": "2025-09-20",
    "Location": "Cumming, GA",
    "Host": "Georgia Nosework",
    "TrialTypes": "NW3, ELT-S, L2I, ELT",
    "EventCount": 4,
    "Latitude": 34.2056,
    "Longitude": -84.1498
  },
  {
    "Date": "2025-09-20",
    "Location": "Darlington, MD",
    "Host": "Firezone GS",
    "TrialTypes": "NW3, L1E, NW2",
    "EventCount": 3,
    "Latitude": 39.6137,
    "Longitude": -76.1796
  },
  {
    "Date": "2025-09-20",
    "Location": "Egg Harbor City, NJ",
    "Host": "Rotts-n-Notts Nosework, LLC",
    "TrialTypes": "NW3, L2I, NW2",
    "EventCount": 3,
    "Latitude": 39.4803,
    "Longitude": -74.6877
  },
  {
    "Date": "2025-09-20",
    "Location": "Fishkill, NY",
    "Host": "Top Notch Dogs, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 41.5172,
    "Longitude": -73.9081
  },
  {
    "Date": "2025-09-20",
    "Location": "Mesquite, TX",
    "Host": "All About The Nose",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 32.756,
    "Longitude": -96.5756
  },
  {
    "Date": "2025-09-20",
    "Location": "Novato, CA",
    "Host": "Marin Humane",
    "TrialTypes": "NW1, NW2, NW3",
    "EventCount": 3,
    "Latitude": 38.0659,
    "Longitude": -122.5397
  },
  {
    "Date": "2025-09-20",
    "Location": "Sunriver, OR",
    "Host": "Canine Discovery Corps",
    "TrialTypes": "NW1, NW2, NW3",
    "EventCount": 3,
    "Latitude": 43.865,
    "Longitude": -121.4407
  },
  {
    "Date": "2025-09-20",
    "Location": "Tuftonboro, NH",
    "Host": "The Big Sniff, LLC",
    "TrialTypes": "NW2, L2E, L2V",
    "EventCount": 3,
    "Latitude": 43.6754,
    "Longitude": -71.331
  },
  {
    "Date": "2025-09-20",
    "Location": "White Salmon, WA",
    "Host": "Sharon Smith",
    "TrialTypes": "NW1, NW2, NW3",
    "EventCount": 3,
    "Latitude": 45.7676,
    "Longitude": -121.5134
  },
  {
    "Date": "2025-09-26",
    "Location": "Hockessin, DE",
    "Host": "Patricia Grassey",
    "TrialTypes": "L2E, NW2, NW1, L2I, NW3",
    "EventCount": 5,
    "Latitude": 39.8296,
    "Longitude": -75.7066
  },
  {
    "Date": "2025-09-27",
    "Location": "Copake, NY",
    "Host": "Saints2Source, LLC",
    "TrialTypes": "NW3, ELT-P, ELT",
    "EventCount": 3,
    "Latitude": 42.1513,
    "Longitude": -73.5136
  },
  {
    "Date": "2025-09-27",
    "Location": "Florissant, MO",
    "Host": "Happy Dog Concepts, LLC",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 38.7726,
    "Longitude": -90.314
  },
  {
    "Date": "2025-09-27",
    "Location": "Kilmarnock, VA",
    "Host": "Red Huskies",
    "TrialTypes": "ELT, ELT-S, NW2",
    "EventCount": 3,
    "Latitude": 37.6855,
    "Longitude": -76.3554
  },
  {
    "Date": "2025-09-27",
    "Location": "Moultonborough, NH",
    "Host": "Dogs Makes Scents",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 43.7947,
    "Longitude": -71.3798
  },
  {
    "Date": "2025-09-27",
    "Location": "Reedsport, OR",
    "Host": "Wells Creek Dog Training",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 43.7029,
    "Longitude": -124.0599
  },
  {
    "Date": "2025-09-27",
    "Location": "Waynesboro, PA",
    "Host": "Nose-It-All, LLC",
    "TrialTypes": "L3E, NW2, ELT",
    "EventCount": 3,
    "Latitude": 39.7973,
    "Longitude": -77.5365
  },
  {
    "Date": "2025-10-03",
    "Location": "Middlebury, CT",
    "Host": "For the Love of Dogs NY LLC",
    "TrialTypes": "NW3, ELT-S, ELT",
    "EventCount": 3,
    "Latitude": 41.538,
    "Longitude": -73.1681
  },
  {
    "Date": "2025-10-04",
    "Location": "Crosslake, MN",
    "Host": "Nose 2 Tail Dog Training LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 46.709,
    "Longitude": -94.1414
  },
  {
    "Date": "2025-10-04",
    "Location": "Nashua, NH",
    "Host": "The Big Sniff, LLC",
    "TrialTypes": "ELT, L3I, L2C",
    "EventCount": 3,
    "Latitude": 42.7387,
    "Longitude": -71.4897
  },
  {
    "Date": "2025-10-04",
    "Location": "New Paltz, NY",
    "Host": "Top Notch Dogs, LLC",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 41.7579,
    "Longitude": -74.0762
  },
  {
    "Date": "2025-10-04",
    "Location": "Smithton, PA",
    "Host": "Steel City Nosework, LLC",
    "TrialTypes": "NW2, ELT",
    "EventCount": 2,
    "Latitude": 40.1825,
    "Longitude": -79.7168
  },
  {
    "Date": "2025-10-06",
    "Location": "Monterey, CA",
    "Host": "CalCoastal Dog Owners Group",
    "TrialTypes": "ELT-S, NW1, L2I, NW2",
    "EventCount": 4,
    "Latitude": 36.2202,
    "Longitude": -121.3922
  },
  {
    "Date": "2025-10-10",
    "Location": "West Bend, WI",
    "Host": "Think Pawsitive Dog Training LLC",
    "TrialTypes": "ELT, L2C, L1E",
    "EventCount": 3,
    "Latitude": 43.4678,
    "Longitude": -88.1653
  },
  {
    "Date": "2025-10-11",
    "Location": "Bloomington, MN",
    "Host": "St Paul Dog Training Club",
    "TrialTypes": "ELT-P, ELT-S",
    "EventCount": 2,
    "Latitude": 44.8532,
    "Longitude": -93.3297
  },
  {
    "Date": "2025-10-11",
    "Location": "Colfax, IA",
    "Host": "KBP Dog Training",
    "TrialTypes": "NW1, NW2, NW3",
    "EventCount": 3,
    "Latitude": 41.6785,
    "Longitude": -93.2086
  },
  {
    "Date": "2025-10-11",
    "Location": "Columbus, MT",
    "Host": "Canine Connection",
    "TrialTypes": "L1C, NW2, NW3",
    "EventCount": 3,
    "Latitude": 45.6834,
    "Longitude": -109.208
  },
  {
    "Date": "2025-10-11",
    "Location": "Durham, NC",
    "Host": "Whole Dog Institute, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 36.0104,
    "Longitude": -78.9446
  },
  {
    "Date": "2025-10-11",
    "Location": "Ferndale, WA",
    "Host": "Nose Work Magic",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 48.8899,
    "Longitude": -122.6143
  },
  {
    "Date": "2025-10-11",
    "Location": "Loveland, CO",
    "Host": "Paws 4 Thought Dog Training, LLC",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 40.4414,
    "Longitude": -105.0562
  },
  {
    "Date": "2025-10-11",
    "Location": "New City , NY",
    "Host": "Saints2Source, LLC",
    "TrialTypes": "NW2, NW3, ELT",
    "EventCount": 3,
    "Latitude": 41.1432,
    "Longitude": -74.0395
  },
  {
    "Date": "2025-10-11",
    "Location": "Occidental, CA",
    "Host": "Marin Humane",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 38.4116,
    "Longitude": -122.8837
  },
  {
    "Date": "2025-10-11",
    "Location": "Roseburg, OR",
    "Host": "Canine Discovery Corps",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 43.2264,
    "Longitude": -123.3545
  },
  {
    "Date": "2025-10-11",
    "Location": "Sedona, AZ",
    "Host": "Successful Sniffer",
    "TrialTypes": "ELT-P, ELT, NW3",
    "EventCount": 3,
    "Latitude": 34.8531,
    "Longitude": -111.7344
  },
  {
    "Date": "2025-10-17",
    "Location": "Elizabeth, CO",
    "Host": "Beyond Elevation K9",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 39.3871,
    "Longitude": -104.6096
  },
  {
    "Date": "2025-10-17",
    "Location": "Lawrenceville, GA",
    "Host": "Chestnut Hill Canine Sports",
    "TrialTypes": "NW3, L2C, NW1",
    "EventCount": 3,
    "Latitude": 33.9306,
    "Longitude": -83.9712
  },
  {
    "Date": "2025-10-17",
    "Location": "Newfoundland, PA",
    "Host": "Your Dog's Place, LLC",
    "TrialTypes": "L1V, ELT-S, L2C, L2E",
    "EventCount": 4,
    "Latitude": 41.2966,
    "Longitude": -75.3198
  },
  {
    "Date": "2025-10-18",
    "Location": "Centralia, WA",
    "Host": "Rachelle Bailey-Austin/About Face K9 Academy & Dorothy Turley/Let's Talk Dogs, LLC",
    "TrialTypes": "L3I, L2C, NW2",
    "EventCount": 3,
    "Latitude": 46.7116,
    "Longitude": -122.971
  },
  {
    "Date": "2025-10-18",
    "Location": "Delevan, NY",
    "Host": "Do Over Dog Training",
    "TrialTypes": "ELT, ELT-S, NW2",
    "EventCount": 3,
    "Latitude": 42.4599,
    "Longitude": -78.4649
  },
  {
    "Date": "2025-10-18",
    "Location": "Lafayette Hill, PA",
    "Host": "Sniff Sniff Hooray",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 40.1344,
    "Longitude": -75.2179
  },
  {
    "Date": "2025-10-18",
    "Location": "Milton, NH",
    "Host": "Lucky Dog Events",
    "TrialTypes": "NW2, NW3",
    "EventCount": 2,
    "Latitude": 43.3641,
    "Longitude": -71.03
  },
  {
    "Date": "2025-10-18",
    "Location": "Nevada City, CA",
    "Host": "Sierra Sniffing Canines",
    "TrialTypes": "L1I, L2I, ELT",
    "EventCount": 3,
    "Latitude": 39.2404,
    "Longitude": -120.9902
  },
  {
    "Date": "2025-10-18",
    "Location": "Terryville, CT",
    "Host": "Willoughby Training",
    "TrialTypes": "NW2, NW1",
    "EventCount": 2,
    "Latitude": 41.7082,
    "Longitude": -72.9597
  },
  {
    "Date": "2025-10-18",
    "Location": "Troy, VA",
    "Host": "Your Dog Knows, LLC",
    "TrialTypes": "ELT, L1V, L2V",
    "EventCount": 3,
    "Latitude": 37.9906,
    "Longitude": -78.1992
  },
  {
    "Date": "2025-10-18",
    "Location": "Watsonville, CA",
    "Host": "CalCoastal Dog Owners Group",
    "TrialTypes": "L3V, L2V, L1V",
    "EventCount": 3,
    "Latitude": 36.9335,
    "Longitude": -121.7066
  },
  {
    "Date": "2025-10-19",
    "Location": "San Martin, CA",
    "Host": "B.L. McMutts",
    "TrialTypes": "L1V, L3V",
    "EventCount": 2,
    "Latitude": 37.1122,
    "Longitude": -121.6136
  },
  {
    "Date": "2025-10-24",
    "Location": "Calhan, CO",
    "Host": "Mountain Dogs, LLC",
    "TrialTypes": "ELT, SMT",
    "EventCount": 2,
    "Latitude": 39.0701,
    "Longitude": -104.3214
  },
  {
    "Date": "2025-10-24",
    "Location": "Easton, MD",
    "Host": "Fair Play Point Labradors",
    "TrialTypes": "ELT, ELT-S, L2V, L2C, L3C",
    "EventCount": 5,
    "Latitude": 38.8106,
    "Longitude": -76.0964
  },
  {
    "Date": "2025-10-24",
    "Location": "Palmyra, VA",
    "Host": "Paws Plus Training, LLC",
    "TrialTypes": "NW3, NW1, NW2, ELT-P",
    "EventCount": 4,
    "Latitude": 37.8586,
    "Longitude": -78.2596
  },
  {
    "Date": "2025-10-24",
    "Location": "Ypsilanti, MI",
    "Host": "Every Dog Nosework",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 42.2681,
    "Longitude": -83.5619
  },
  {
    "Date": "2025-10-25",
    "Location": "Auburn, WA",
    "Host": "Northwest K9 Sniffers",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 47.3305,
    "Longitude": -122.2507
  },
  {
    "Date": "2025-10-25",
    "Location": "Fishkill, NY",
    "Host": "Top Notch Dogs, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 41.5704,
    "Longitude": -73.9423
  },
  {
    "Date": "2025-10-25",
    "Location": "Lyle, WA",
    "Host": "Trust Your Dog K9 Events",
    "TrialTypes": "NW3, L3I, L3C",
    "EventCount": 3,
    "Latitude": 45.7329,
    "Longitude": -121.3287
  },
  {
    "Date": "2025-10-25",
    "Location": "Niantic, IL",
    "Host": "Kudos for Canines, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 39.833,
    "Longitude": -89.1669
  },
  {
    "Date": "2025-10-25",
    "Location": "Poland Springs, ME",
    "Host": "Virginia Howe",
    "TrialTypes": "NW1, NW3",
    "EventCount": 2,
    "Latitude": 44.0343,
    "Longitude": -70.3367
  },
  {
    "Date": "2025-10-25",
    "Location": "West Friendship, MD",
    "Host": "Red Huskies",
    "TrialTypes": "NW3, NW1, L1C",
    "EventCount": 3,
    "Latitude": 39.2656,
    "Longitude": -76.9447
  },
  {
    "Date": "2025-10-27",
    "Location": "Clayton, GA",
    "Host": "Right Choice Dog Training LLC",
    "TrialTypes": "NW3, ELT, ELT-P",
    "EventCount": 3,
    "Latitude": 33.4765,
    "Longitude": -84.3359
  },
  {
    "Date": "2025-10-27",
    "Location": "Paicines, CA",
    "Host": "Two Nosey Girls",
    "TrialTypes": "NW2, ELT-P",
    "EventCount": 2,
    "Latitude": 36.7113,
    "Longitude": -121.2504
  },
  {
    "Date": "2025-10-31",
    "Location": "Cannon Falls, MN",
    "Host": "St Paul Dog Training Club",
    "TrialTypes": "SMT, NW3",
    "EventCount": 2,
    "Latitude": 44.4618,
    "Longitude": -92.8877
  },
  {
    "Date": "2025-10-31",
    "Location": "Loranger, LA",
    "Host": "Dog Gone Right",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 30.6766,
    "Longitude": -90.4251
  },
  {
    "Date": "2025-10-31",
    "Location": "Meeker, CO",
    "Host": "Mountain Dogs LLC",
    "TrialTypes": "ELT, NW3, NW1, NW2",
    "EventCount": 4,
    "Latitude": 40.0339,
    "Longitude": -107.9628
  },
  {
    "Date": "2025-10-31",
    "Location": "Scotts Mills, OR",
    "Host": "Canine Discovery Corps",
    "TrialTypes": "ELT, NW1, L3C",
    "EventCount": 3,
    "Latitude": 45.038,
    "Longitude": -122.6592
  },
  {
    "Date": "2025-11-01",
    "Location": "Beloit, WI",
    "Host": "George Carpenter",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 42.5178,
    "Longitude": -89.0762
  },
  {
    "Date": "2025-11-01",
    "Location": "Charlton, MA",
    "Host": "HeavenScent Sniffers",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 42.1648,
    "Longitude": -72.0128
  },
  {
    "Date": "2025-11-01",
    "Location": "Kennebunkport, ME",
    "Host": "Elizabeth Dutton",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 43.3302,
    "Longitude": -70.4545
  },
  {
    "Date": "2025-11-01",
    "Location": "Mill Spring, NC",
    "Host": "Foothills Canine Academy, LLC",
    "TrialTypes": "NW1, NW2, ELT",
    "EventCount": 3,
    "Latitude": 35.278,
    "Longitude": -82.1625
  },
  {
    "Date": "2025-11-01",
    "Location": "Monkton, MD",
    "Host": "Firezone GS",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 39.5332,
    "Longitude": -76.611
  },
  {
    "Date": "2025-11-01",
    "Location": "Wappingers Falls, NY",
    "Host": "Top Notch Dogs, LLC",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 41.5605,
    "Longitude": -73.9371
  },
  {
    "Date": "2025-11-01",
    "Location": "Woodstock, GA",
    "Host": "Georgia Nosework LLC",
    "TrialTypes": "L1C, L3I, L3C, L1I",
    "EventCount": 4,
    "Latitude": 34.0842,
    "Longitude": -84.5556
  },
  {
    "Date": "2025-11-01",
    "Location": "Yamhill, OR",
    "Host": "Nose Work Detectives",
    "TrialTypes": "ELT-S, L1C",
    "EventCount": 2,
    "Latitude": 45.2511,
    "Longitude": -123.1716
  },
  {
    "Date": "2025-11-05",
    "Location": "Ventura, CA",
    "Host": "Pink Biscuit K9s",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 34.4404,
    "Longitude": -119.0429
  },
  {
    "Date": "2025-11-07",
    "Location": "Montrose, CO",
    "Host": "Mountain Dogs LLC",
    "TrialTypes": "ELT-S, NW2, ELT",
    "EventCount": 3,
    "Latitude": 38.5015,
    "Longitude": -107.8852
  },
  {
    "Date": "2025-11-07",
    "Location": "Rancho Cucamonga , CA",
    "Host": "Agile Paws Dog Sports",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 34.1489,
    "Longitude": -117.5535
  },
  {
    "Date": "2025-11-08",
    "Location": "Coburg, OR",
    "Host": "Kiddy Christie",
    "TrialTypes": "NW1, NW2, ELT-S, L1E",
    "EventCount": 4,
    "Latitude": 44.108,
    "Longitude": -123.0627
  },
  {
    "Date": "2025-11-08",
    "Location": "Elkhorn, WI",
    "Host": "Loving Paws Dog Training LLC",
    "TrialTypes": "ELT, NW2",
    "EventCount": 2,
    "Latitude": 42.6381,
    "Longitude": -88.4943
  },
  {
    "Date": "2025-11-08",
    "Location": "Guerneville, CA",
    "Host": "Jen Huot",
    "TrialTypes": "NW3, ELT, ELT-P",
    "EventCount": 3,
    "Latitude": 38.5392,
    "Longitude": -123.0185
  },
  {
    "Date": "2025-11-08",
    "Location": "Mays Landing, NJ",
    "Host": "Rotts-n-Notts Nosework, LLC",
    "TrialTypes": "NW3, L2E, NW2",
    "EventCount": 3,
    "Latitude": 39.4255,
    "Longitude": -74.6835
  },
  {
    "Date": "2025-11-08",
    "Location": "Pine Grove, PA",
    "Host": "Paws n' Sniff",
    "TrialTypes": "ELT-P, NW3",
    "EventCount": 2,
    "Latitude": 40.5044,
    "Longitude": -76.4012
  },
  {
    "Date": "2025-11-08",
    "Location": "Tucson, AZ",
    "Host": "Patience Unlimited Professional Dog Training",
    "TrialTypes": "NW3, L2C, NW2",
    "EventCount": 3,
    "Latitude": 32.241,
    "Longitude": -110.9631
  },
  {
    "Date": "2025-11-11",
    "Location": "Guerneville, CA",
    "Host": "Jen Huot",
    "TrialTypes": "ELT-P",
    "EventCount": 1,
    "Latitude": 38.5212,
    "Longitude": -122.9963
  },
  {
    "Date": "2025-11-12",
    "Location": "Astoria, OR",
    "Host": "Nose Work Detectives, LLC",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 46.159,
    "Longitude": -123.8234
  },
  {
    "Date": "2025-11-14",
    "Location": "Elgin, IL",
    "Host": "Common Scents K9",
    "TrialTypes": "L1C, L2C, L1I, L2I, NW1",
    "EventCount": 5,
    "Latitude": 42.086,
    "Longitude": -88.244
  },
  {
    "Date": "2025-11-14",
    "Location": "New Freedom, PA",
    "Host": "Firezone GS",
    "TrialTypes": "ELT, NW3, ELT-S, NW2",
    "EventCount": 4,
    "Latitude": 39.7128,
    "Longitude": -76.7284
  },
  {
    "Date": "2025-11-15",
    "Location": "Albuquerque, NM",
    "Host": "New Mexico Canine Scent Work, LLC",
    "TrialTypes": "NW3, L1C, NW2",
    "EventCount": 3,
    "Latitude": 35.0629,
    "Longitude": -106.6438
  },
  {
    "Date": "2025-11-15",
    "Location": "Bonham, TX",
    "Host": "All About The Nose",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 33.6014,
    "Longitude": -96.1475
  },
  {
    "Date": "2025-11-15",
    "Location": "Bradenton, FL",
    "Host": "Hoppin' in the Hills",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 27.4497,
    "Longitude": -82.5371
  },
  {
    "Date": "2025-11-15",
    "Location": "Eldred, NY",
    "Host": "Your Dog's Place, LLC",
    "TrialTypes": "L1I, NW2, L3I, L3C",
    "EventCount": 4,
    "Latitude": 41.5473,
    "Longitude": -74.8994
  },
  {
    "Date": "2025-11-15",
    "Location": "Marbury, AL",
    "Host": "Kaye Stevenson",
    "TrialTypes": "NW2, NW1, L1E",
    "EventCount": 3,
    "Latitude": 32.6699,
    "Longitude": -86.4405
  },
  {
    "Date": "2025-11-15",
    "Location": "Montgomery, AL",
    "Host": "By A Nose Nosework",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 32.408,
    "Longitude": -86.3061
  },
  {
    "Date": "2025-11-15",
    "Location": "Welches, OR",
    "Host": "Trust Your Dog K9 Events",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 45.3538,
    "Longitude": -122.0003
  },
  {
    "Date": "2025-11-17",
    "Location": "Hudson, MA",
    "Host": "For the Love of Dogs NY LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 42.4187,
    "Longitude": -71.5389
  },
  {
    "Date": "2025-11-21",
    "Location": "Harrington, DE",
    "Host": "Shamrock Pot Of Gold K9 Scenter",
    "TrialTypes": "NW3, ELT-S, NW2, NW1, L1C, ELT",
    "EventCount": 6,
    "Latitude": 38.926,
    "Longitude": -75.533
  },
  {
    "Date": "2025-11-21",
    "Location": "San Luis Obispo, CA",
    "Host": "Gentle Touch Pet Training",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 35.321,
    "Longitude": -120.4221
  },
  {
    "Date": "2025-11-22",
    "Location": "Crownsville, MD",
    "Host": "Red Huskies",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 39.0605,
    "Longitude": -76.5878
  },
  {
    "Date": "2025-11-22",
    "Location": "Defuniak Springs, FL",
    "Host": "Linda Culliton",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 30.7045,
    "Longitude": -86.0731
  },
  {
    "Date": "2025-11-22",
    "Location": "Delta, CO",
    "Host": "Mountain Dogs, LLC",
    "TrialTypes": "ELT, NW3, ELT-S, NW2",
    "EventCount": 4,
    "Latitude": 38.859,
    "Longitude": -107.849
  },
  {
    "Date": "2025-11-22",
    "Location": "Foxborough , MA",
    "Host": "MasterPeace Dog Training",
    "TrialTypes": "L3C, NW1, NW2",
    "EventCount": 3,
    "Latitude": 42.0866,
    "Longitude": -71.2432
  },
  {
    "Date": "2025-11-22",
    "Location": "Kintnersville, PA",
    "Host": "Paws n' Sniff",
    "TrialTypes": "L1C, NW1, L2I, L2E",
    "EventCount": 4,
    "Latitude": 40.5367,
    "Longitude": -75.1502
  },
  {
    "Date": "2025-11-22",
    "Location": "Marble Falls, TX",
    "Host": "Scent Work Across Texas",
    "TrialTypes": "ELT-S, NW2, NW1, L1I",
    "EventCount": 4,
    "Latitude": 30.6179,
    "Longitude": -98.3066
  },
  {
    "Date": "2025-11-22",
    "Location": "Medford, NJ",
    "Host": "K9 InScentives",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 39.8605,
    "Longitude": -74.8119
  },
  {
    "Date": "2025-11-22",
    "Location": "Norton, MA",
    "Host": "Dogs Make Scents",
    "TrialTypes": "ELT, L1E, L1C",
    "EventCount": 3,
    "Latitude": 41.97,
    "Longitude": -71.1716
  },
  {
    "Date": "2025-11-22",
    "Location": "Salem Lakes, WI",
    "Host": "Loving Paws Dog Training, LLC",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 42.5692,
    "Longitude": -88.1008
  },
  {
    "Date": "2025-11-22",
    "Location": "Smyrna, TN",
    "Host": "Dogs Have Amazing Noses, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 35.9413,
    "Longitude": -86.5305
  },
  {
    "Date": "2025-11-28",
    "Location": "Dana Point, CA",
    "Host": "JavaK9s, LLC",
    "TrialTypes": "NW3, ELT-S",
    "EventCount": 2,
    "Latitude": 33.4285,
    "Longitude": -117.6513
  },
  {
    "Date": "2025-11-28",
    "Location": "San Jose, CA",
    "Host": "The Bay Team",
    "TrialTypes": "NW2, NW1",
    "EventCount": 2,
    "Latitude": 37.2891,
    "Longitude": -121.8813
  },
  {
    "Date": "2025-11-29",
    "Location": "Alpharetta, GA",
    "Host": "Georgia Nosework",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 34.088,
    "Longitude": -84.2687
  },
  {
    "Date": "2025-11-29",
    "Location": "Canandaigua, NY",
    "Host": "Savvy Dog Sports",
    "TrialTypes": "ELT, NW2",
    "EventCount": 2,
    "Latitude": 42.8803,
    "Longitude": -77.3513
  },
  {
    "Date": "2025-11-29",
    "Location": "Cottage Grove, MN",
    "Host": "Gretchen Hofheins-Wackerfuss",
    "TrialTypes": "ELT-S, ELT-P",
    "EventCount": 2,
    "Latitude": 44.8132,
    "Longitude": -92.9364
  },
  {
    "Date": "2025-11-29",
    "Location": "Lebanon, NJ",
    "Host": "Sirius K9 Solutions",
    "TrialTypes": "NW3, L3I, ELT-S",
    "EventCount": 3,
    "Latitude": 40.6756,
    "Longitude": -74.8836
  },
  {
    "Date": "2025-12-05",
    "Location": "Bowie, MD",
    "Host": "Red Huskies",
    "TrialTypes": "ELT, ELT-P, ELT-S",
    "EventCount": 3,
    "Latitude": 38.9227,
    "Longitude": -76.7395
  },
  {
    "Date": "2025-12-06",
    "Location": "Annapolis, MD",
    "Host": "Chesapeake Search Dogs",
    "TrialTypes": "NW3, NW2, L2C",
    "EventCount": 3,
    "Latitude": 38.9615,
    "Longitude": -76.448
  },
  {
    "Date": "2025-12-06",
    "Location": "Auburn, WA",
    "Host": "Northwest K9 Sniffers",
    "TrialTypes": "L1C, L2C, NW3",
    "EventCount": 3,
    "Latitude": 47.3266,
    "Longitude": -122.262
  },
  {
    "Date": "2025-12-06",
    "Location": "Centralia, WA",
    "Host": "About Face K9 Academy and Let's Talk Dogs",
    "TrialTypes": "NW3, NW1",
    "EventCount": 2,
    "Latitude": 46.7178,
    "Longitude": -122.9222
  },
  {
    "Date": "2025-12-06",
    "Location": "Fillmore, CA",
    "Host": "Pink Biscuit K9s",
    "TrialTypes": "NW1, NW2, ELT",
    "EventCount": 3,
    "Latitude": 34.3549,
    "Longitude": -118.9001
  },
  {
    "Date": "2025-12-06",
    "Location": "Hoover, AL",
    "Host": "Southeast Scent Work Alliance, LLC",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 33.3109,
    "Longitude": -86.8038
  },
  {
    "Date": "2025-12-06",
    "Location": "Kittanning, PA",
    "Host": "Steel City Nosework, LLC",
    "TrialTypes": "NW3, ELT-S, NW1",
    "EventCount": 3,
    "Latitude": 40.8462,
    "Longitude": -79.5061
  },
  {
    "Date": "2025-12-06",
    "Location": "Newfoundland, PA",
    "Host": "Your Dog's Place, LLC",
    "TrialTypes": "L2I, L3V, NW3",
    "EventCount": 3,
    "Latitude": 41.3067,
    "Longitude": -75.2858
  },
  {
    "Date": "2025-12-07",
    "Location": "Cape Coral, FL",
    "Host": "Your Dog Knows, LLC",
    "TrialTypes": "NW1",
    "EventCount": 1,
    "Latitude": 26.5282,
    "Longitude": -81.9665
  },
  {
    "Date": "2025-12-12",
    "Location": "Douglassville, PA",
    "Host": "Patricia Grassey",
    "TrialTypes": "NW3, ELT, ELT-P",
    "EventCount": 3,
    "Latitude": 40.2495,
    "Longitude": -75.7617
  },
  {
    "Date": "2025-12-12",
    "Location": "Pittstown, NJ",
    "Host": "Shamrock Pot Of Gold K9 Scenter",
    "TrialTypes": "NW3, ELT, ELT-S, NW1",
    "EventCount": 4,
    "Latitude": 40.5385,
    "Longitude": -74.9443
  },
  {
    "Date": "2025-12-13",
    "Location": "DeLeon Springs, FL",
    "Host": "River Poodles Training, LLC",
    "TrialTypes": "ELT-P, ELT-S, L2C",
    "EventCount": 3,
    "Latitude": 29.1436,
    "Longitude": -81.3565
  },
  {
    "Date": "2025-12-13",
    "Location": "Easton, MA",
    "Host": "South Coast Scent Dogs",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 42.0226,
    "Longitude": -71.137
  },
  {
    "Date": "2025-12-13",
    "Location": "Escondido, CA",
    "Host": "Uber Dog and Rewarding Rover LLC",
    "TrialTypes": "NW2",
    "EventCount": 1,
    "Latitude": 33.1693,
    "Longitude": -117.0918
  },
  {
    "Date": "2025-12-13",
    "Location": "Greer, SC",
    "Host": "Trained to Trust LLC",
    "TrialTypes": "ELT, ELT-S, NW2",
    "EventCount": 3,
    "Latitude": 34.9279,
    "Longitude": -82.2569
  },
  {
    "Date": "2025-12-13",
    "Location": "Independence, OR",
    "Host": "Doglandia, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 44.8673,
    "Longitude": -123.1934
  },
  {
    "Date": "2025-12-13",
    "Location": "Westminster, MD",
    "Host": "Red Huskies",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 39.5712,
    "Longitude": -76.9787
  },
  {
    "Date": "2025-12-16",
    "Location": "Duluth, GA",
    "Host": "Right Choice Dog Training LLC",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 34.0189,
    "Longitude": -84.1335
  },
  {
    "Date": "2025-12-20",
    "Location": "Cumming, GA",
    "Host": "Georgia Nosework LLC",
    "TrialTypes": "SMT, ELT",
    "EventCount": 2,
    "Latitude": 34.1655,
    "Longitude": -84.1834
  },
  {
    "Date": "2025-12-20",
    "Location": "Florissant, MO",
    "Host": "Happy Dog Concepts, LLC",
    "TrialTypes": "ELT-P",
    "EventCount": 1,
    "Latitude": 38.8415,
    "Longitude": -90.3258
  },
  {
    "Date": "2025-12-20",
    "Location": "Salem, OR",
    "Host": "Helix Fairweather & Doglandia, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 44.9805,
    "Longitude": -123.069
  },
  {
    "Date": "2025-12-20",
    "Location": "Silex, MO",
    "Host": "WestInn Kennels",
    "TrialTypes": "NW1, NW2, NW3",
    "EventCount": 3,
    "Latitude": 39.1521,
    "Longitude": -91.012
  },
  {
    "Date": "2025-12-20",
    "Location": "Stockton, CA",
    "Host": "Two Nosey Girls",
    "TrialTypes": "NW3, NW1",
    "EventCount": 2,
    "Latitude": 37.9577,
    "Longitude": -121.3098
  },
  {
    "Date": "2025-12-27",
    "Location": "Auburn, AL",
    "Host": "Daphne Melillo",
    "TrialTypes": "NW2, NW1",
    "EventCount": 2,
    "Latitude": 32.6524,
    "Longitude": -85.4847
  },
  {
    "Date": "2025-12-27",
    "Location": "Fort Morgan, CO",
    "Host": "Paws 4 Thought Dog Training, LLC",
    "TrialTypes": "L1V, NW2, NW1, L1I",
    "EventCount": 4,
    "Latitude": 40.2498,
    "Longitude": -103.7514
  },
  {
    "Date": "2025-12-27",
    "Location": "New Rochelle, NY",
    "Host": "For the Love of Dogs NY LLC",
    "TrialTypes": "NW3, ELT-S, ELT",
    "EventCount": 3,
    "Latitude": 40.9224,
    "Longitude": -73.781
  },
  {
    "Date": "2025-12-27",
    "Location": "White Plains, NY",
    "Host": "Saints2Source",
    "TrialTypes": "NW1, NW2, L2E, L2C",
    "EventCount": 4,
    "Latitude": 41.0209,
    "Longitude": -73.747
  },
  {
    "Date": "2025-12-28",
    "Location": "Exton, PA",
    "Host": "Patricia Grassey",
    "TrialTypes": "ELT, ELT-P, NW3",
    "EventCount": 3,
    "Latitude": 40.0617,
    "Longitude": -75.629
  },
  {
    "Date": "2025-12-28",
    "Location": "Waukesha, WI",
    "Host": "Think Pawsitive Dog Training",
    "TrialTypes": "NW2, NW1",
    "EventCount": 2,
    "Latitude": 43.0905,
    "Longitude": -88.3475
  },
  {
    "Date": "2025-12-29",
    "Location": "Barrington, RI",
    "Host": "Bay State Sniffers",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 41.7213,
    "Longitude": -71.3465
  },
  {
    "Date": "2026-01-02",
    "Location": "Emmitsburg , MD",
    "Host": "Red Huskies",
    "TrialTypes": "NW3, ELT, ELT-S",
    "EventCount": 3,
    "Latitude": 39.7326,
    "Longitude": -77.2912
  },
  {
    "Date": "2026-01-03",
    "Location": "Bonsall, CA",
    "Host": "Linda Buchanan",
    "TrialTypes": "NW3, ELT-P",
    "EventCount": 2,
    "Latitude": 33.2785,
    "Longitude": -117.1713
  },
  {
    "Date": "2026-01-03",
    "Location": "Green Cove Springs, FL",
    "Host": "Hoppin' in the Hills",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 29.9579,
    "Longitude": -81.6857
  },
  {
    "Date": "2026-01-03",
    "Location": "Maryville, TN",
    "Host": "Rachel Hawkins",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 35.7273,
    "Longitude": -83.9364
  },
  {
    "Date": "2026-01-03",
    "Location": "Montevallo, AL",
    "Host": "Southeast Scent Work Alliance, LLC",
    "TrialTypes": "NW1",
    "EventCount": 1,
    "Latitude": 33.079,
    "Longitude": -86.8838
  },
  {
    "Date": "2026-01-09",
    "Location": "Hartfield, VA",
    "Host": "Paws Plus Training, LLC",
    "TrialTypes": "NW3, NW1, NW2, ELT",
    "EventCount": 4,
    "Latitude": 37.5718,
    "Longitude": -76.4875
  },
  {
    "Date": "2026-01-09",
    "Location": "Spring City, PA",
    "Host": "Patricia Grassey",
    "TrialTypes": "NW3, NW2, NW1, L2I",
    "EventCount": 4,
    "Latitude": 40.1988,
    "Longitude": -75.5711
  },
  {
    "Date": "2026-01-10",
    "Location": "Canton , GA",
    "Host": "Run Spot Jump Dog Training",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 34.21,
    "Longitude": -84.5066
  },
  {
    "Date": "2026-01-10",
    "Location": "Pflugerville, TX",
    "Host": "Scent Work Across Texas",
    "TrialTypes": "ELT-S, L2C, NW3",
    "EventCount": 3,
    "Latitude": 30.4331,
    "Longitude": -97.6554
  },
  {
    "Date": "2026-01-10",
    "Location": "Valencia, CA",
    "Host": "Pink Biscuit K9s",
    "TrialTypes": "NW1, NW2, ELT",
    "EventCount": 3,
    "Latitude": 34.4531,
    "Longitude": -118.606
  },
  {
    "Date": "2026-01-16",
    "Location": "Bristol, PA",
    "Host": "Patricia Grassey",
    "TrialTypes": "ELT, ELT-P, ELT-S, NW2",
    "EventCount": 4,
    "Latitude": 40.1558,
    "Longitude": -74.8431
  },
  {
    "Date": "2026-01-17",
    "Location": "Clanton, AL",
    "Host": "By A Nose Nosework",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 32.8839,
    "Longitude": -86.6666
  },
  {
    "Date": "2026-01-17",
    "Location": "Melrose, FL",
    "Host": "River Poodles Training, LLC",
    "TrialTypes": "NW1, NW2, L1V, L1E",
    "EventCount": 4,
    "Latitude": 29.6878,
    "Longitude": -82.0933
  },
  {
    "Date": "2026-01-17",
    "Location": "New Rochelle, NY",
    "Host": "For the Love of Dogs NY LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 40.9231,
    "Longitude": -73.8004
  },
  {
    "Date": "2026-01-17",
    "Location": "San Marcos, CA",
    "Host": "Rewarding Rover LLC and Uberdog",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 33.1186,
    "Longitude": -117.2204
  },
  {
    "Date": "2026-01-17",
    "Location": "Tecumseh, OK",
    "Host": "The Doggie Spot, LLC",
    "TrialTypes": "ELT, NW3, NW2",
    "EventCount": 3,
    "Latitude": 35.2175,
    "Longitude": -96.8872
  },
  {
    "Date": "2026-01-19",
    "Location": "Redlands, CA",
    "Host": "Agile Paws Dog Sports",
    "TrialTypes": "NW3, ELT-P",
    "EventCount": 2,
    "Latitude": 34.0145,
    "Longitude": -117.1665
  },
  {
    "Date": "2026-01-20",
    "Location": "Murfreesboro, TN",
    "Host": "Dogs Have Amazing Noses, LLC",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 35.8673,
    "Longitude": -86.389
  },
  {
    "Date": "2026-01-23",
    "Location": "Rome, GA",
    "Host": "Georgia Nosework, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 34.2754,
    "Longitude": -85.1547
  },
  {
    "Date": "2026-01-30",
    "Location": "Greeley, CO",
    "Host": "Beyond Elevation K9 Training",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 40.4516,
    "Longitude": -104.7137
  },
  {
    "Date": "2026-01-31",
    "Location": "Denton, MD",
    "Host": "Fair Play Labradors",
    "TrialTypes": "ELT-S, L2C, NW1, L1E",
    "EventCount": 4,
    "Latitude": 38.8452,
    "Longitude": -75.8348
  },
  {
    "Date": "2026-01-31",
    "Location": "Tucson, AZ",
    "Host": "Patience Unlimited Professional Dog Training",
    "TrialTypes": "NW3, ELT-S, L1V",
    "EventCount": 3,
    "Latitude": 32.2487,
    "Longitude": -111.0093
  },
  {
    "Date": "2026-02-07",
    "Location": "Lakewood, NJ",
    "Host": "Rotts n Notts Nosework",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 40.076,
    "Longitude": -74.177
  },
  {
    "Date": "2026-02-07",
    "Location": "Murfreesboro, TN",
    "Host": "Dogs Have Amazing Noses, LLC",
    "TrialTypes": "ELT, NW1",
    "EventCount": 2,
    "Latitude": 35.8508,
    "Longitude": -86.3795
  },
  {
    "Date": "2026-02-13",
    "Location": "Havre De Grace, MD",
    "Host": "Firezone GS",
    "TrialTypes": "ELT-P, NW3, L3I, NW2",
    "EventCount": 4,
    "Latitude": 39.5664,
    "Longitude": -76.0543
  },
  {
    "Date": "2026-02-13",
    "Location": "Vista, CA",
    "Host": "Rewarding Rover LLC and Uberdog",
    "TrialTypes": "ELT, L1C, L2C",
    "EventCount": 3,
    "Latitude": 33.2429,
    "Longitude": -117.2009
  },
  {
    "Date": "2026-02-14",
    "Location": "Clarkesville, GA",
    "Host": "Right Choice Dog Training, LLC",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 34.5893,
    "Longitude": -83.5467
  },
  {
    "Date": "2026-02-14",
    "Location": "Colesville, MD",
    "Host": "Red Huskies",
    "TrialTypes": "L1C, NW1, ELT-S, ELT",
    "EventCount": 4,
    "Latitude": 39.0671,
    "Longitude": -77.0428
  },
  {
    "Date": "2026-02-14",
    "Location": "Flemington, NJ",
    "Host": "Shamrock Pot Of Gold K9 Scenter",
    "TrialTypes": "ELT, ELT-S, NW1",
    "EventCount": 3,
    "Latitude": 40.5072,
    "Longitude": -74.8736
  },
  {
    "Date": "2026-02-14",
    "Location": "Northridge, CA",
    "Host": "SCENTwork.org",
    "TrialTypes": "NW2, ELT",
    "EventCount": 2,
    "Latitude": 34.2479,
    "Longitude": -118.5732
  },
  {
    "Date": "2026-02-14",
    "Location": "Pottsboro, TX",
    "Host": "All About The Nose",
    "TrialTypes": "NW3, NW2, NW1",
    "EventCount": 3,
    "Latitude": 33.8077,
    "Longitude": -96.6249
  },
  {
    "Date": "2026-02-14",
    "Location": "Strafford, PA",
    "Host": "The Sniffing Hound",
    "TrialTypes": "NW3, ELT, ELT-P",
    "EventCount": 3,
    "Latitude": 40.0401,
    "Longitude": -75.4248
  },
  {
    "Date": "2026-02-15",
    "Location": "Chino, CA",
    "Host": "Agile Paws Dog Sports",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 34.0554,
    "Longitude": -117.7297
  },
  {
    "Date": "2026-02-15",
    "Location": "New Rochelle, NY",
    "Host": "For the Love of Dogs NY LLC",
    "TrialTypes": "ELT-P, NW3, ELT",
    "EventCount": 3,
    "Latitude": 40.8638,
    "Longitude": -73.8111
  },
  {
    "Date": "2026-02-20",
    "Location": "San Rafael/Novato, CA",
    "Host": "Marin Humane",
    "TrialTypes": "L2C, L1I, NW3",
    "EventCount": 3,
    "Latitude": 38.0301,
    "Longitude": -122.4244
  },
  {
    "Date": "2026-02-21",
    "Location": "Clearwater, FL",
    "Host": "Hoppin' in the Hills",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 28.0102,
    "Longitude": -82.7643
  },
  {
    "Date": "2026-02-21",
    "Location": "Veneta, OR",
    "Host": "Wells Creek Dog Training",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 44.0426,
    "Longitude": -123.3493
  },
  {
    "Date": "2026-02-22",
    "Location": "Benson, AZ",
    "Host": "Patience Unlimited Professional Dog Training",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 31.93,
    "Longitude": -110.2899
  },
  {
    "Date": "2026-02-24",
    "Location": "Paso Robles, CA",
    "Host": "Gentle Touch Pet Training",
    "TrialTypes": "ELT-S, L3I",
    "EventCount": 2,
    "Latitude": 35.6322,
    "Longitude": -120.6743
  },
  {
    "Date": "2026-02-28",
    "Location": "Danielsville, GA",
    "Host": "Right Choice Dog Training, LLC",
    "TrialTypes": "NW1, L2I, NW2",
    "EventCount": 3,
    "Latitude": 34.0862,
    "Longitude": -83.2284
  },
  {
    "Date": "2026-02-28",
    "Location": "Keystone Heights, FL",
    "Host": "River Poodles Training, LLC",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 29.8085,
    "Longitude": -82.0635
  },
  {
    "Date": "2026-02-28",
    "Location": "Lutherville, MD",
    "Host": "Red Huskies",
    "TrialTypes": "L3C, L1I, NW2",
    "EventCount": 3,
    "Latitude": 39.3946,
    "Longitude": -76.6178
  },
  {
    "Date": "2026-02-28",
    "Location": "Tygh Valley, OR",
    "Host": "Nose Work Detectives, LLC",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 45.2918,
    "Longitude": -121.1279
  },
  {
    "Date": "2026-02-28",
    "Location": "Wilson, NC",
    "Host": "Whole Dog Institute, LLC",
    "TrialTypes": "ELT, ELT-S, NW1",
    "EventCount": 3,
    "Latitude": 35.6812,
    "Longitude": -77.9012
  },
  {
    "Date": "2026-03-06",
    "Location": "Chesterfield, VA",
    "Host": "Paws Plus Training, LLC",
    "TrialTypes": "NW3, NW1, NW2, ELT",
    "EventCount": 4,
    "Latitude": 37.3731,
    "Longitude": -77.582
  },
  {
    "Date": "2026-03-06",
    "Location": "Elgin, IL",
    "Host": "For Your K9, Inc",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 42.0689,
    "Longitude": -88.2518
  },
  {
    "Date": "2026-03-06",
    "Location": "Westlake Village, CA",
    "Host": "JavaK9s, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 34.1464,
    "Longitude": -118.7608
  },
  {
    "Date": "2026-03-07",
    "Location": "Cumming, GA",
    "Host": "Georgia Nosework LLC",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 34.2495,
    "Longitude": -84.1567
  },
  {
    "Date": "2026-03-07",
    "Location": "Honesdale, PA",
    "Host": "Your Dog's Place, LLC",
    "TrialTypes": "L3C, ELT, ELT-S, L3I",
    "EventCount": 4,
    "Latitude": 41.543,
    "Longitude": -75.2444
  },
  {
    "Date": "2026-03-07",
    "Location": "Moriarty, NM",
    "Host": "New Mexico Canine Scent Work, LLC",
    "TrialTypes": "NW3, L1I, NW1",
    "EventCount": 3,
    "Latitude": 35.0096,
    "Longitude": -106.074
  },
  {
    "Date": "2026-03-07",
    "Location": "Warrensburg, IL",
    "Host": "Kudos for Canines",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 39.9195,
    "Longitude": -89.103
  },
  {
    "Date": "2026-03-13",
    "Location": "Centreville , MD",
    "Host": "Fair Play Point Labradors",
    "TrialTypes": "SMT, L2C, L3C",
    "EventCount": 3,
    "Latitude": 39.05,
    "Longitude": -76.0401
  },
  {
    "Date": "2026-03-13",
    "Location": "Colebrook, CT",
    "Host": "For the Love of Dogs NY LLC",
    "TrialTypes": "NW3, SMT",
    "EventCount": 2,
    "Latitude": 41.9964,
    "Longitude": -73.092
  },
  {
    "Date": "2026-03-13",
    "Location": "Stokesdale, NC",
    "Host": "K9 Nose Adventures, LLC",
    "TrialTypes": "ELT, NW3, NW1, NW2",
    "EventCount": 4,
    "Latitude": 36.2616,
    "Longitude": -79.9732
  },
  {
    "Date": "2026-03-14",
    "Location": "Channahon, IL",
    "Host": "4G & TB",
    "TrialTypes": "NW2, NW1",
    "EventCount": 2,
    "Latitude": 41.4218,
    "Longitude": -88.2703
  },
  {
    "Date": "2026-03-14",
    "Location": "Hammond, LA",
    "Host": "Dog Gone Right",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 30.4964,
    "Longitude": -90.4381
  },
  {
    "Date": "2026-03-14",
    "Location": "Kent, WA",
    "Host": "K9 Sniffers",
    "TrialTypes": "ELT, L1V, L1E",
    "EventCount": 3,
    "Latitude": 47.4147,
    "Longitude": -122.1945
  },
  {
    "Date": "2026-03-14",
    "Location": "Phoenix, AZ",
    "Host": "Release Canine, LLC",
    "TrialTypes": "ELT, NW1",
    "EventCount": 2,
    "Latitude": 33.406,
    "Longitude": -112.0481
  },
  {
    "Date": "2026-03-14",
    "Location": "Santa Paula, CA",
    "Host": "Pink Biscuit K9s",
    "TrialTypes": "NW3, NW1, ELT-S",
    "EventCount": 3,
    "Latitude": 34.3796,
    "Longitude": -119.1031
  },
  {
    "Date": "2026-03-16",
    "Location": "Paso Robles, CA",
    "Host": "Central Coast Nosework Club",
    "TrialTypes": "NW3, ELT-S, L3V",
    "EventCount": 3,
    "Latitude": 35.6131,
    "Longitude": -120.7324
  },
  {
    "Date": "2026-03-20",
    "Location": "Shady Hills, FL",
    "Host": "Hoppin' in the Hills",
    "TrialTypes": "L1C, NW1, NW2",
    "EventCount": 3,
    "Latitude": 28.3862,
    "Longitude": -82.5442
  },
  {
    "Date": "2026-03-20",
    "Location": "Street, MD",
    "Host": "Firezone GS",
    "TrialTypes": "ELT-P, NW3, ELT-S, NW2",
    "EventCount": 4,
    "Latitude": 39.6826,
    "Longitude": -76.3482
  },
  {
    "Date": "2026-03-21",
    "Location": "Califon, NJ",
    "Host": "Shamrock Pot Of Gold K9 Scenter",
    "TrialTypes": "NW2, ELT-S, ELT",
    "EventCount": 3,
    "Latitude": 40.7426,
    "Longitude": -74.8108
  },
  {
    "Date": "2026-03-21",
    "Location": "Dittmer, MO",
    "Host": "Happy Dog Concepts LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 38.3372,
    "Longitude": -90.7154
  },
  {
    "Date": "2026-03-21",
    "Location": "East Windsor, CT",
    "Host": "Lucky Dog Events",
    "TrialTypes": "ELT, L2C, NW2",
    "EventCount": 3,
    "Latitude": 41.9335,
    "Longitude": -72.6115
  },
  {
    "Date": "2026-03-21",
    "Location": "Elkridge, MD",
    "Host": "Red Huskies",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 39.2095,
    "Longitude": -76.7856
  },
  {
    "Date": "2026-03-21",
    "Location": "Foxboro, MA",
    "Host": "Bay State Sniffers",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 42.0568,
    "Longitude": -71.2348
  },
  {
    "Date": "2026-03-21",
    "Location": "Lawrenceville, GA",
    "Host": "Right Choice Dog Training LLC",
    "TrialTypes": "ELT-P, ELT",
    "EventCount": 2,
    "Latitude": 33.9685,
    "Longitude": -84.0365
  },
  {
    "Date": "2026-03-21",
    "Location": "Oakville, WA",
    "Host": "About Face K9 Academy & Let's Talk Dogs, LLC",
    "TrialTypes": "NW2, ELT",
    "EventCount": 2,
    "Latitude": 46.8729,
    "Longitude": -123.2677
  },
  {
    "Date": "2026-03-21",
    "Location": "Redwood City, CA",
    "Host": "B.L. McMutts LLC",
    "TrialTypes": "ELT-S, L1I, L3C",
    "EventCount": 3,
    "Latitude": 37.5027,
    "Longitude": -122.2391
  },
  {
    "Date": "2026-03-21",
    "Location": "Salem Lakes, WI",
    "Host": "Loving Paws Dog Training LLC",
    "TrialTypes": "NW2, L2I, ELT-S",
    "EventCount": 3,
    "Latitude": 42.4881,
    "Longitude": -88.1364
  },
  {
    "Date": "2026-03-21",
    "Location": "Winterset, IA",
    "Host": "KBP Dog Training",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 41.3032,
    "Longitude": -94.0285
  },
  {
    "Date": "2026-03-23",
    "Location": "Riverside, CA",
    "Host": "Linda Buchanan",
    "TrialTypes": "NW2, NW3",
    "EventCount": 2,
    "Latitude": 34.0322,
    "Longitude": -117.3543
  },
  {
    "Date": "2026-03-27",
    "Location": "Grand Junction, CO",
    "Host": "Mountain Dogs, LLC",
    "TrialTypes": "ELT-P, ELT-S, NW1, NW2",
    "EventCount": 4,
    "Latitude": 39.0224,
    "Longitude": -108.5888
  },
  {
    "Date": "2026-03-27",
    "Location": "Kennett Square, PA",
    "Host": "The Sniffing Hound",
    "TrialTypes": "NW3, ELT, ELT-S, L3I",
    "EventCount": 4,
    "Latitude": 39.8021,
    "Longitude": -75.7387
  },
  {
    "Date": "2026-03-27",
    "Location": "Salem, OR",
    "Host": "Just Nose Work & Doglandia LLC",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 44.9246,
    "Longitude": -123.0158
  },
  {
    "Date": "2026-03-27",
    "Location": "Watertown, TN",
    "Host": "Dogs Have Amazing Noses, LLC",
    "TrialTypes": "ELT, NW2",
    "EventCount": 2,
    "Latitude": 36.1009,
    "Longitude": -86.1072
  },
  {
    "Date": "2026-03-28",
    "Location": "Batavia, OH",
    "Host": "Clermont County Dog Training Club",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 39.0593,
    "Longitude": -84.2178
  },
  {
    "Date": "2026-03-28",
    "Location": "Colorado Springs, CO",
    "Host": "Beyond Elevation K9",
    "TrialTypes": "NW2, NW3",
    "EventCount": 2,
    "Latitude": 38.7857,
    "Longitude": -104.8387
  },
  {
    "Date": "2026-03-28",
    "Location": "Forks, WA",
    "Host": "Sea Change Canine LLC",
    "TrialTypes": "NW2, NW3",
    "EventCount": 2,
    "Latitude": 47.9266,
    "Longitude": -124.34
  },
  {
    "Date": "2026-03-29",
    "Location": "Canton, GA",
    "Host": "Run Spot Jump Dog Training",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 34.2291,
    "Longitude": -84.464
  },
  {
    "Date": "2026-03-30",
    "Location": "Watsonville, CA",
    "Host": "CalCoastal Dog Owners Group",
    "TrialTypes": "NW2, NW1",
    "EventCount": 2,
    "Latitude": 36.9009,
    "Longitude": -121.7643
  },
  {
    "Date": "2026-04-03",
    "Location": "Eagan, MN",
    "Host": "St. Paul Dog Training Center",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 44.7789,
    "Longitude": -93.2084
  },
  {
    "Date": "2026-04-03",
    "Location": "Rochester, NY",
    "Host": "2 Psyched 4 dogs",
    "TrialTypes": "NW3, L1I, L1C",
    "EventCount": 3,
    "Latitude": 43.158,
    "Longitude": -77.5718
  },
  {
    "Date": "2026-04-03",
    "Location": "Warwick, NY",
    "Host": "For the Love of Dogs NY LLC",
    "TrialTypes": "ELT, NW3, NW1",
    "EventCount": 3,
    "Latitude": 41.2329,
    "Longitude": -74.3327
  },
  {
    "Date": "2026-04-04",
    "Location": "Blaine, WA",
    "Host": "The Nosework Magic",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 48.9641,
    "Longitude": -122.7558
  },
  {
    "Date": "2026-04-04",
    "Location": "Burnet, TX",
    "Host": "Scent Work Across Texas",
    "TrialTypes": "L1C, NW2, ELT",
    "EventCount": 3,
    "Latitude": 30.7354,
    "Longitude": -98.1486
  },
  {
    "Date": "2026-04-04",
    "Location": "Stayton, OR",
    "Host": "Canine Discovery Corps",
    "TrialTypes": "L2E, L1V, L1E, L2C",
    "EventCount": 4,
    "Latitude": 44.7845,
    "Longitude": -122.7865
  },
  {
    "Date": "2026-04-06",
    "Location": "Sacramento, CA",
    "Host": "Two Nosey Girls",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 38.6273,
    "Longitude": -121.5369
  },
  {
    "Date": "2026-04-08",
    "Location": "Olympia, WA",
    "Host": "Rachelle Bailey-Austin & Dorothy Turley",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 47.0848,
    "Longitude": -122.9064
  },
  {
    "Date": "2026-04-10",
    "Location": "Rapid City, SD",
    "Host": "Two Paws Up Dog Training, LLC",
    "TrialTypes": "ELT, NW3, NW1, NW2",
    "EventCount": 4,
    "Latitude": 44.0971,
    "Longitude": -103.2636
  },
  {
    "Date": "2026-04-10",
    "Location": "Somis, CA",
    "Host": "Pink Biscuit K9s",
    "TrialTypes": "NW3, ELT-P, L2C, L2I",
    "EventCount": 4,
    "Latitude": 34.2731,
    "Longitude": -118.9749
  },
  {
    "Date": "2026-04-11",
    "Location": "Bel Air, MD",
    "Host": "Firezone GS",
    "TrialTypes": "ELT-P, ELT-S, NW2",
    "EventCount": 3,
    "Latitude": 39.5522,
    "Longitude": -76.3382
  },
  {
    "Date": "2026-04-11",
    "Location": "Blue Ridge , VA",
    "Host": "Canny K9 Companions, LLC",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 37.3735,
    "Longitude": -79.8647
  },
  {
    "Date": "2026-04-11",
    "Location": "Clinton, WI",
    "Host": "George Carpenter",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 42.5856,
    "Longitude": -88.8229
  },
  {
    "Date": "2026-04-11",
    "Location": "Durham, NC",
    "Host": "Dog Fun Forever, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 35.9689,
    "Longitude": -78.862
  },
  {
    "Date": "2026-04-11",
    "Location": "Genoa, IL",
    "Host": "Common Scents K9",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 42.1126,
    "Longitude": -88.706
  },
  {
    "Date": "2026-04-11",
    "Location": "Limerick, PA",
    "Host": "Sniff Sniff Hooray",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 40.2673,
    "Longitude": -75.4935
  },
  {
    "Date": "2026-04-11",
    "Location": "Rocklin, CA",
    "Host": "Sierra Sniffing Canines",
    "TrialTypes": "NW2, L1E, L2E",
    "EventCount": 3,
    "Latitude": 38.7917,
    "Longitude": -121.284
  },
  {
    "Date": "2026-04-13",
    "Location": "Ellicott City, MD",
    "Host": "Red Huskies",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 39.2861,
    "Longitude": -76.8114
  },
  {
    "Date": "2026-04-17",
    "Location": "Amity, OR",
    "Host": "Doglandia, LLC",
    "TrialTypes": "NW1, NW2, NW3",
    "EventCount": 3,
    "Latitude": 45.1132,
    "Longitude": -123.2471
  },
  {
    "Date": "2026-04-17",
    "Location": "Garrison, NY",
    "Host": "Top Notch Dogs, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 41.3605,
    "Longitude": -73.9297
  },
  {
    "Date": "2026-04-17",
    "Location": "Upland, CA",
    "Host": "Agile Paws Dog Sports",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 34.068,
    "Longitude": -117.644
  },
  {
    "Date": "2026-04-18",
    "Location": "Auburn, WA",
    "Host": "Northwest K9 Sniffers",
    "TrialTypes": "L1V, NW1, L1I, NW2",
    "EventCount": 4,
    "Latitude": 47.2697,
    "Longitude": -122.277
  },
  {
    "Date": "2026-04-18",
    "Location": "Keystone Heights, FL",
    "Host": "River Poodles Training, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 29.7829,
    "Longitude": -82.0172
  },
  {
    "Date": "2026-04-18",
    "Location": "Laramie, WY",
    "Host": "Mountain Dogs LLC",
    "TrialTypes": "SMT, ELT",
    "EventCount": 2,
    "Latitude": 41.3496,
    "Longitude": -105.5688
  },
  {
    "Date": "2026-04-18",
    "Location": "Pomfret, MD",
    "Host": "Nose-It-All, LLC",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 38.6078,
    "Longitude": -77.0702
  },
  {
    "Date": "2026-04-18",
    "Location": "Toledo, OH",
    "Host": "Robin Ford Dog Training LLC",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 41.6113,
    "Longitude": -83.5336
  },
  {
    "Date": "2026-04-18",
    "Location": "Winterset, IA",
    "Host": "KBP Dog Training",
    "TrialTypes": "NW2, NW1",
    "EventCount": 2,
    "Latitude": 41.3648,
    "Longitude": -94.0561
  },
  {
    "Date": "2026-04-18",
    "Location": "Woodstock, IL",
    "Host": "Northwest Obedience Club Inc",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 42.2981,
    "Longitude": -88.4048
  },
  {
    "Date": "2026-04-19",
    "Location": "Glenwood, NY",
    "Host": "Do Over Dog Training",
    "TrialTypes": "L2I, L1E, L2C, ELT-S",
    "EventCount": 4,
    "Latitude": 42.6607,
    "Longitude": -78.611
  },
  {
    "Date": "2026-04-20",
    "Location": "Amherst, NH",
    "Host": "Lucky Dog Events",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 42.8362,
    "Longitude": -71.6331
  },
  {
    "Date": "2026-04-21",
    "Location": "Stony Point , NY",
    "Host": "Saints2Source, LLC",
    "TrialTypes": "NW3, ELT, ELT-S, NW2",
    "EventCount": 4,
    "Latitude": 41.226,
    "Longitude": -73.9977
  },
  {
    "Date": "2026-04-24",
    "Location": "Asheboro, NC",
    "Host": "K9 Nose Adventures, LLC",
    "TrialTypes": "ELT, NW3, ELT-S, NW2",
    "EventCount": 4,
    "Latitude": 35.7077,
    "Longitude": -79.8473
  },
  {
    "Date": "2026-04-24",
    "Location": "Easton, MD",
    "Host": "Red Huskies",
    "TrialTypes": "L3C, NW2, L3V, NW1",
    "EventCount": 4,
    "Latitude": 38.8061,
    "Longitude": -76.0729
  },
  {
    "Date": "2026-04-25",
    "Location": "Canfield, OH",
    "Host": "Nosework Addicts, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 41.0203,
    "Longitude": -80.8071
  },
  {
    "Date": "2026-04-25",
    "Location": "Columbus, MT",
    "Host": "Canine Connection",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 45.6064,
    "Longitude": -109.2948
  },
  {
    "Date": "2026-04-25",
    "Location": "Ellicottville, NY",
    "Host": "Do Over Dog Training",
    "TrialTypes": "ELT, ELT-S, NW1",
    "EventCount": 3,
    "Latitude": 42.2889,
    "Longitude": -78.6707
  },
  {
    "Date": "2026-04-25",
    "Location": "Havre de Grace, MD",
    "Host": "Chesapeake Search Dogs",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 39.5549,
    "Longitude": -76.124
  },
  {
    "Date": "2026-04-25",
    "Location": "Portland, OR",
    "Host": "Trust Your Dog K9 Events",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 45.5611,
    "Longitude": -122.6506
  },
  {
    "Date": "2026-04-25",
    "Location": "Sharon, MA",
    "Host": "Bay State Sniffers",
    "TrialTypes": "NW3, ELT-P",
    "EventCount": 2,
    "Latitude": 42.1683,
    "Longitude": -71.1333
  },
  {
    "Date": "2026-04-25",
    "Location": "Suring, WI",
    "Host": "Clever Sniffers, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 45.0019,
    "Longitude": -88.3534
  },
  {
    "Date": "2026-04-25",
    "Location": "Traverse City, MI",
    "Host": "Every Dog Nosework",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 44.7446,
    "Longitude": -85.5906
  },
  {
    "Date": "2026-05-01",
    "Location": "Faribault, MN",
    "Host": "St. Paul Dog Training Club",
    "TrialTypes": "NW3, NW1, L2E, L3C",
    "EventCount": 4,
    "Latitude": 43.6251,
    "Longitude": -93.9656
  },
  {
    "Date": "2026-05-01",
    "Location": "Nyack, NY",
    "Host": "Waggin' Work",
    "TrialTypes": "NW2, NW1, ELT-P, ELT-S",
    "EventCount": 4,
    "Latitude": 41.0792,
    "Longitude": -73.9308
  },
  {
    "Date": "2026-05-01",
    "Location": "Turlock, CA",
    "Host": "Two Nosey Girls",
    "TrialTypes": "L1I, L2I, ELT-S, L3I",
    "EventCount": 4,
    "Latitude": 37.5288,
    "Longitude": -120.8315
  },
  {
    "Date": "2026-05-02",
    "Location": "Alexis, IL",
    "Host": "Kudos for Canines, LLC",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 41.0475,
    "Longitude": -90.5515
  },
  {
    "Date": "2026-05-02",
    "Location": "Ashby , MA",
    "Host": "Dogs! Carolyn Barney",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 42.6584,
    "Longitude": -71.7801
  },
  {
    "Date": "2026-05-02",
    "Location": "Hillsdale, NY",
    "Host": "For the Love of Dogs NY LLC",
    "TrialTypes": "ELT-P, ELT",
    "EventCount": 2,
    "Latitude": 42.2061,
    "Longitude": -73.5499
  },
  {
    "Date": "2026-05-02",
    "Location": "Santa Paula, CA",
    "Host": "Pink Biscuit K9s",
    "TrialTypes": "NW3, ELT-S, NW2",
    "EventCount": 3,
    "Latitude": 34.39,
    "Longitude": -119.0113
  },
  {
    "Date": "2026-05-02",
    "Location": "Sedona, AZ",
    "Host": "Successful Sniffer",
    "TrialTypes": "L1I, NW2, NW3",
    "EventCount": 3,
    "Latitude": 34.8805,
    "Longitude": -111.7561
  },
  {
    "Date": "2026-05-02",
    "Location": "Vancouver, WA",
    "Host": "Sniffketeers",
    "TrialTypes": "ELT-S",
    "EventCount": 1,
    "Latitude": 45.6152,
    "Longitude": -122.6871
  },
  {
    "Date": "2026-05-07",
    "Location": "Lancaster, PA",
    "Host": "Red Huskies Nose Work, LLC",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 40.0043,
    "Longitude": -76.3007
  },
  {
    "Date": "2026-05-08",
    "Location": "Grand Island, NY",
    "Host": "Do Over Dog Training",
    "TrialTypes": "SMT, ELT",
    "EventCount": 2,
    "Latitude": 43.0384,
    "Longitude": -78.918
  },
  {
    "Date": "2026-05-08",
    "Location": "Jarrettsville, MD",
    "Host": "Firezone GS",
    "TrialTypes": "ELT, NW3, ELT-S, L2I",
    "EventCount": 4,
    "Latitude": 39.6205,
    "Longitude": -76.5027
  },
  {
    "Date": "2026-05-08",
    "Location": "Warwick, NY",
    "Host": "Top Notch Dogs, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 41.2676,
    "Longitude": -74.389
  },
  {
    "Date": "2026-05-08",
    "Location": "Wrightwood, CA",
    "Host": "JavaK9s, LLC",
    "TrialTypes": "NW3, L1C, L1I",
    "EventCount": 3,
    "Latitude": 34.351,
    "Longitude": -117.6321
  },
  {
    "Date": "2026-05-09",
    "Location": "Brighton, MI",
    "Host": "Every Dog Nosework",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 42.5176,
    "Longitude": -83.8142
  },
  {
    "Date": "2026-05-09",
    "Location": "Charlton, MA",
    "Host": "HeavenScent Sniffers",
    "TrialTypes": "ELT-P, L2C, NW1",
    "EventCount": 3,
    "Latitude": 42.1746,
    "Longitude": -72.0033
  },
  {
    "Date": "2026-05-09",
    "Location": "Egg Harbor City, NJ",
    "Host": "Rotts-n-Notts Nosework LLC",
    "TrialTypes": "L3I, NW1, NW3",
    "EventCount": 3,
    "Latitude": 39.5779,
    "Longitude": -74.6311
  },
  {
    "Date": "2026-05-09",
    "Location": "Livingston, MT",
    "Host": "Trails and Tails Dog School",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 45.6262,
    "Longitude": -110.569
  },
  {
    "Date": "2026-05-09",
    "Location": "Malvern, IA",
    "Host": "Two Tails Unlimited",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 41.0173,
    "Longitude": -95.6346
  },
  {
    "Date": "2026-05-09",
    "Location": "Poland Springs, ME",
    "Host": "Bare Bones Nosework, LLC",
    "TrialTypes": "L1I, L2I",
    "EventCount": 2,
    "Latitude": 44.0364,
    "Longitude": -70.3555
  },
  {
    "Date": "2026-05-09",
    "Location": "Union Grove, WI",
    "Host": "Loving Paws, LLC",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 42.684,
    "Longitude": -88.0859
  },
  {
    "Date": "2026-05-14",
    "Location": "Frederick, MD",
    "Host": "Red Huskies",
    "TrialTypes": "ELT-P, ELT-S, L2I, L2E, L1E",
    "EventCount": 5,
    "Latitude": 39.4433,
    "Longitude": -77.4393
  },
  {
    "Date": "2026-05-15",
    "Location": "Cannon Falls, MN",
    "Host": "Saint Paul Dog Training Club",
    "TrialTypes": "ELT, ELT-S, L3E, L1I, L1E",
    "EventCount": 5,
    "Latitude": 44.4727,
    "Longitude": -92.8928
  },
  {
    "Date": "2026-05-15",
    "Location": "Phoenix, MD",
    "Host": "Oriole Dog Training Club",
    "TrialTypes": "NW3, L1I, NW1",
    "EventCount": 3,
    "Latitude": 39.4953,
    "Longitude": -76.6159
  },
  {
    "Date": "2026-05-15",
    "Location": "Watsonville, CA",
    "Host": "CalCoastal Dog Owners Group",
    "TrialTypes": "ELT-S, L3C, L1C",
    "EventCount": 3,
    "Latitude": 36.9041,
    "Longitude": -121.728
  },
  {
    "Date": "2026-05-16",
    "Location": "Alexander, NY",
    "Host": "Do Over Dog Training",
    "TrialTypes": "L1V, L2V, L3C, L3I",
    "EventCount": 4,
    "Latitude": 42.8836,
    "Longitude": -78.2143
  },
  {
    "Date": "2026-05-16",
    "Location": "Bellingham, WA",
    "Host": "The Nosework Magic",
    "TrialTypes": "NW2, NW1",
    "EventCount": 2,
    "Latitude": 48.7995,
    "Longitude": -122.4959
  },
  {
    "Date": "2026-05-16",
    "Location": "Burien, WA",
    "Host": "Northwest K9 Sniffers",
    "TrialTypes": "NW3, L2V, L2I",
    "EventCount": 3,
    "Latitude": 47.4467,
    "Longitude": -122.3099
  },
  {
    "Date": "2026-05-16",
    "Location": "Durham, NC",
    "Host": "Whole Dog Institute, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 35.9632,
    "Longitude": -78.9297
  },
  {
    "Date": "2026-05-16",
    "Location": "Kittanning, PA",
    "Host": "Steel City Nosework, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 40.7875,
    "Longitude": -79.5218
  },
  {
    "Date": "2026-05-16",
    "Location": "Monticello , NY",
    "Host": "Saints2Source, LLC",
    "TrialTypes": "NW3, ELT, ELT-S",
    "EventCount": 3,
    "Latitude": 41.6577,
    "Longitude": -74.7089
  },
  {
    "Date": "2026-05-16",
    "Location": "Peru, MA",
    "Host": "Lucky Dog Events",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 42.46,
    "Longitude": -73.083
  },
  {
    "Date": "2026-05-16",
    "Location": "Sandwich, IL",
    "Host": "For Your K9, Inc.",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 41.6869,
    "Longitude": -88.6066
  },
  {
    "Date": "2026-05-22",
    "Location": "Anchorage, AK",
    "Host": "Alaska Dog Sports",
    "TrialTypes": "NW1, NW2, NW3",
    "EventCount": 3,
    "Latitude": 61.2365,
    "Longitude": -149.8452
  },
  {
    "Date": "2026-05-22",
    "Location": "Montrose, CO",
    "Host": "Mountain Dogs LLC",
    "TrialTypes": "ELT, NW3, NW2, NW1",
    "EventCount": 4,
    "Latitude": 38.4826,
    "Longitude": -107.904
  },
  {
    "Date": "2026-05-22",
    "Location": "San Luis Obispo, CA",
    "Host": "Gentle Touch Pet Training",
    "TrialTypes": "NW1, L1I, NW2",
    "EventCount": 3,
    "Latitude": 35.3206,
    "Longitude": -120.3371
  },
  {
    "Date": "2026-05-23",
    "Location": "Alpharetta, GA",
    "Host": "Georgia Nosework, LLC",
    "TrialTypes": "NW3, ELT-S, NW2, ELT",
    "EventCount": 4,
    "Latitude": 34.0731,
    "Longitude": -84.2967
  },
  {
    "Date": "2026-05-23",
    "Location": "Columbus, MT",
    "Host": "Canine Connection",
    "TrialTypes": "L1I, L2I, ELT-S, NW2",
    "EventCount": 4,
    "Latitude": 45.5981,
    "Longitude": -109.2589
  },
  {
    "Date": "2026-05-23",
    "Location": "Emmitsburg, MD",
    "Host": "Red Huskies",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 39.71,
    "Longitude": -77.3427
  },
  {
    "Date": "2026-05-23",
    "Location": "Lancaster, PA",
    "Host": "Nose-It-All, LLC",
    "TrialTypes": "ELT, ELT-P, NW3",
    "EventCount": 3,
    "Latitude": 40.0746,
    "Longitude": -76.3449
  },
  {
    "Date": "2026-05-23",
    "Location": "Murfreesboro, TN",
    "Host": "Dogs Have Amazing Noses, LLC",
    "TrialTypes": "ELT, NW1",
    "EventCount": 2,
    "Latitude": 35.8952,
    "Longitude": -86.3722
  },
  {
    "Date": "2026-05-23",
    "Location": "North Manchester, IN",
    "Host": "2 Nose You Is 2 Loves You",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 40.9858,
    "Longitude": -85.7587
  },
  {
    "Date": "2026-05-23",
    "Location": "Rainier, WA",
    "Host": "Let's Talk Dogs, LLC & About Face K9 Academy",
    "TrialTypes": "ELT-S, L1C, NW2",
    "EventCount": 3,
    "Latitude": 46.8607,
    "Longitude": -122.6848
  },
  {
    "Date": "2026-05-23",
    "Location": "Red Feather Lakes, CO",
    "Host": "Beyond Elevation K9 Training LLC",
    "TrialTypes": "NW1, NW2, NW3",
    "EventCount": 3,
    "Latitude": 40.8418,
    "Longitude": -105.5492
  },
  {
    "Date": "2026-05-23",
    "Location": "Rockaway, NJ",
    "Host": "Shamrock Pot Of Gold K9 Scenter",
    "TrialTypes": "NW3, L3E, NW2, ELT-S",
    "EventCount": 4,
    "Latitude": 40.9017,
    "Longitude": -74.518
  },
  {
    "Date": "2026-05-23",
    "Location": "Sandy, OR",
    "Host": "Trust Your Dog K9 Events",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 45.4429,
    "Longitude": -122.2432
  },
  {
    "Date": "2026-05-25",
    "Location": "Manchester, NH",
    "Host": "For the Love of Dogs NY, LLC",
    "TrialTypes": "NW2, ELT-P, ELT",
    "EventCount": 3,
    "Latitude": 43.0297,
    "Longitude": -71.4329
  },
  {
    "Date": "2026-05-28",
    "Location": "Concord, CA",
    "Host": "The Bay Team",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 37.9537,
    "Longitude": -122.0602
  },
  {
    "Date": "2026-05-29",
    "Location": "Grand Junction, CO",
    "Host": "Mountain Dogs LLC",
    "TrialTypes": "NW3, L1V, L1E",
    "EventCount": 3,
    "Latitude": 39.0291,
    "Longitude": -108.5358
  },
  {
    "Date": "2026-05-30",
    "Location": "Amherst, NY",
    "Host": "Do Over Dog Training",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 42.9325,
    "Longitude": -78.7721
  },
  {
    "Date": "2026-05-30",
    "Location": "Eden Prairie, MN",
    "Host": "The K9 Nose",
    "TrialTypes": "NW2",
    "EventCount": 1,
    "Latitude": 44.8994,
    "Longitude": -93.4409
  },
  {
    "Date": "2026-05-30",
    "Location": "Spencer, MA",
    "Host": "Lucky Dog Events",
    "TrialTypes": "ELT, L1E, L2C",
    "EventCount": 3,
    "Latitude": 42.2073,
    "Longitude": -71.9558
  },
  {
    "Date": "2026-06-06",
    "Location": "Dunmore, PA",
    "Host": "Your Dog's Place, LLC",
    "TrialTypes": "ELT, ELT-S, L1C",
    "EventCount": 3,
    "Latitude": 41.4228,
    "Longitude": -75.662
  },
  {
    "Date": "2026-06-06",
    "Location": "Enterprise, OR",
    "Host": "Country K9 Nosework, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 45.4035,
    "Longitude": -117.2964
  },
  {
    "Date": "2026-06-06",
    "Location": "Manheim, PA",
    "Host": "Nose-It-All, LLC",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 40.2105,
    "Longitude": -76.4266
  },
  {
    "Date": "2026-06-06",
    "Location": "Meadowbrook, PA",
    "Host": "Patricia Grassey",
    "TrialTypes": "NW3, NW1, NW2, ELT",
    "EventCount": 4,
    "Latitude": 40.0825,
    "Longitude": -75.1328
  },
  {
    "Date": "2026-06-06",
    "Location": "Rochester, NH",
    "Host": "Pawsitive Image",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 43.2758,
    "Longitude": -70.9335
  },
  {
    "Date": "2026-06-06",
    "Location": "Shawnee, OK",
    "Host": "The Doggie Spot, LLC",
    "TrialTypes": "NW3, NW2, NW1",
    "EventCount": 3,
    "Latitude": 35.2958,
    "Longitude": -96.9257
  },
  {
    "Date": "2026-06-06",
    "Location": "Slippery Rock, PA",
    "Host": "Nosework Addicts, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 41.0946,
    "Longitude": -80.0582
  },
  {
    "Date": "2026-06-06",
    "Location": "Sparks Glencoe, MD",
    "Host": "Chesapeake Search Dogs",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 39.4987,
    "Longitude": -76.6619
  },
  {
    "Date": "2026-06-06",
    "Location": "Wrightstown, WI",
    "Host": "NEWk9Scent Work LLC",
    "TrialTypes": "NW3, NW1, L1I",
    "EventCount": 3,
    "Latitude": 44.3082,
    "Longitude": -88.2027
  },
  {
    "Date": "2026-06-12",
    "Location": "Gunnison, CO",
    "Host": "Mountain Dogs LLC",
    "TrialTypes": "ELT, ELT-S, ELT-P",
    "EventCount": 3,
    "Latitude": 38.6195,
    "Longitude": -107.0152
  },
  {
    "Date": "2026-06-12",
    "Location": "New Hope, PA",
    "Host": "Patricia Grassey",
    "TrialTypes": "ELT, ELT-P, ELT-S, L3E",
    "EventCount": 4,
    "Latitude": 40.3962,
    "Longitude": -74.9246
  },
  {
    "Date": "2026-06-13",
    "Location": "East Helena, MT",
    "Host": "Nose Work Breakfast Club",
    "TrialTypes": "NW1, NW2, NW3",
    "EventCount": 3,
    "Latitude": 46.6323,
    "Longitude": -111.8755
  },
  {
    "Date": "2026-06-13",
    "Location": "Ithaca, NY",
    "Host": "The Brainy Canine",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 42.3971,
    "Longitude": -76.5944
  },
  {
    "Date": "2026-06-13",
    "Location": "Kenosha, WI",
    "Host": "Loving Paws Dog Training LLC",
    "TrialTypes": "ELT-S, L1I, NW1",
    "EventCount": 3,
    "Latitude": 42.5523,
    "Longitude": -87.7952
  },
  {
    "Date": "2026-06-13",
    "Location": "Linden, MI",
    "Host": "Every Dog Nosework",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 42.8146,
    "Longitude": -83.8205
  },
  {
    "Date": "2026-06-13",
    "Location": "Nazareth/Windgap, PA",
    "Host": "Paws n' Sniff",
    "TrialTypes": "NW2, ELT, NW1, ELT-S, NW3",
    "EventCount": 5,
    "Latitude": 40.7231,
    "Longitude": -75.2636
  },
  {
    "Date": "2026-06-13",
    "Location": "Palmyra, VA",
    "Host": "Your Dog Knows LLC",
    "TrialTypes": "L1I, L2I, L1C, L2C",
    "EventCount": 4,
    "Latitude": 37.8414,
    "Longitude": -78.2684
  },
  {
    "Date": "2026-06-18",
    "Location": "Westminster, MD",
    "Host": "Firezone GS",
    "TrialTypes": "ELT, NW3, ELT-S, NW2",
    "EventCount": 4,
    "Latitude": 39.5576,
    "Longitude": -77.0036
  },
  {
    "Date": "2026-06-19",
    "Location": "Bayfield, CO",
    "Host": "Mountain Dogs, LLC",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 37.2465,
    "Longitude": -107.5913
  },
  {
    "Date": "2026-06-19",
    "Location": "Jordan, MN",
    "Host": "St. Paul Dog Training Club",
    "TrialTypes": "ELT, ELT-P, L1V, L2V",
    "EventCount": 4,
    "Latitude": 44.671,
    "Longitude": -93.6759
  },
  {
    "Date": "2026-06-19",
    "Location": "New Rochelle, NY",
    "Host": "For the Love of Dogs NY, LLC",
    "TrialTypes": "ELT, NW3, ELT-P",
    "EventCount": 3,
    "Latitude": 40.9456,
    "Longitude": -73.8221
  },
  {
    "Date": "2026-06-20",
    "Location": "Cumming, GA",
    "Host": "Georgia Nosework, LLC",
    "TrialTypes": "L2I, L2C, L3C, L1I",
    "EventCount": 4,
    "Latitude": 34.2154,
    "Longitude": -84.1694
  },
  {
    "Date": "2026-06-20",
    "Location": "Danvers, MA",
    "Host": "Everydog, LLC",
    "TrialTypes": "NW1, NW2, NW3",
    "EventCount": 3,
    "Latitude": 42.5393,
    "Longitude": -70.8955
  },
  {
    "Date": "2026-06-20",
    "Location": "Florissant, MO",
    "Host": "Happy Dog Concepts",
    "TrialTypes": "NW1, NW2, NW3",
    "EventCount": 3,
    "Latitude": 38.7784,
    "Longitude": -90.2767
  },
  {
    "Date": "2026-06-20",
    "Location": "Pittsburgh, PA",
    "Host": "Nosework Addicts, LLC",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 40.402,
    "Longitude": -80.0513
  },
  {
    "Date": "2026-06-20",
    "Location": "Terryville, CT",
    "Host": "Willoughby Training",
    "TrialTypes": "NW3, NW1",
    "EventCount": 2,
    "Latitude": 41.7225,
    "Longitude": -73.0315
  },
  {
    "Date": "2026-06-20",
    "Location": "White Salmon, WA",
    "Host": "Sharon Smith",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 45.6784,
    "Longitude": -121.4826
  },
  {
    "Date": "2026-06-26",
    "Location": "Delran, NJ",
    "Host": "K9 InScentives",
    "TrialTypes": "NW1, ELT",
    "EventCount": 2,
    "Latitude": 39.9759,
    "Longitude": -74.9801
  },
  {
    "Date": "2026-06-26",
    "Location": "Loveland, CO",
    "Host": "NoCo Unleashed LLC",
    "TrialTypes": "ELT-S, L2C, L2I, L1C",
    "EventCount": 4,
    "Latitude": 40.4141,
    "Longitude": -105.0823
  },
  {
    "Date": "2026-06-26",
    "Location": "Red Lodge, MT",
    "Host": "Canine Connection",
    "TrialTypes": "ELT, NW3, NW1, NW2",
    "EventCount": 4,
    "Latitude": 45.209,
    "Longitude": -109.2226
  },
  {
    "Date": "2026-06-27",
    "Location": "Burlington, WI",
    "Host": "Loving Paws Dog Training LLC",
    "TrialTypes": "NW2, ELT",
    "EventCount": 2,
    "Latitude": 42.7164,
    "Longitude": -88.3146
  },
  {
    "Date": "2026-06-27",
    "Location": "De Pere, WI",
    "Host": "NEWk9Scent Work LLC",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 44.4676,
    "Longitude": -88.0394
  },
  {
    "Date": "2026-06-27",
    "Location": "Deming, WA",
    "Host": "The Nosework Magic",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 48.8672,
    "Longitude": -122.2468
  },
  {
    "Date": "2026-06-27",
    "Location": "Inver Grove Heights, MN",
    "Host": "Outside the Box Dog Training, LLC",
    "TrialTypes": "NW3, L2C, L2I",
    "EventCount": 3,
    "Latitude": 44.8942,
    "Longitude": -93.0602
  },
  {
    "Date": "2026-06-27",
    "Location": "Lockport, IL",
    "Host": "4G & TB",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 41.6006,
    "Longitude": -88.0953
  },
  {
    "Date": "2026-06-27",
    "Location": "New Wilmington, PA",
    "Host": "Steel City Nosework, LLC",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 41.1191,
    "Longitude": -80.3321
  },
  {
    "Date": "2026-06-27",
    "Location": "Salem, OR",
    "Host": "Doglandia, LLC",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 44.9821,
    "Longitude": -123.0704
  },
  {
    "Date": "2026-06-27",
    "Location": "Somers, CT",
    "Host": "HeavenScent Sniffers",
    "TrialTypes": "NW2, L3C, ELT-S",
    "EventCount": 3,
    "Latitude": 41.9488,
    "Longitude": -72.4665
  },
  {
    "Date": "2026-06-30",
    "Location": "Delran, NJ",
    "Host": "Ev-ry Earthdog, LLC",
    "TrialTypes": "NW3, NW1, NW2, ELT-P",
    "EventCount": 4,
    "Latitude": 40.0574,
    "Longitude": -74.977
  },
  {
    "Date": "2026-07-03",
    "Location": "Huntington, MA",
    "Host": "Shamrock Pot Of Gold K9 Scenter",
    "TrialTypes": "NW3, ELT, ELT-S, L2I",
    "EventCount": 4,
    "Latitude": 42.2496,
    "Longitude": -72.8522
  },
  {
    "Date": "2026-07-06",
    "Location": "Montgomery, NY",
    "Host": "For the Love of Dogs NY LLC",
    "TrialTypes": "NW2, ELT",
    "EventCount": 2,
    "Latitude": 42.858,
    "Longitude": -74.4595
  },
  {
    "Date": "2026-07-10",
    "Location": "Leadville, CO",
    "Host": "Mountain Dogs LLC",
    "TrialTypes": "NW3, NW2, NW1, L2I, L2C",
    "EventCount": 5,
    "Latitude": 39.2921,
    "Longitude": -106.3158
  },
  {
    "Date": "2026-07-10",
    "Location": "Sparks Glencoe, MD",
    "Host": "Firezone GS",
    "TrialTypes": "ELT-P, ELT-S, L3I, ELT",
    "EventCount": 4,
    "Latitude": 39.479,
    "Longitude": -76.6755
  },
  {
    "Date": "2026-07-11",
    "Location": "Livonia, MI",
    "Host": "Every Dog Nosework",
    "TrialTypes": "ELT-P, NW1",
    "EventCount": 2,
    "Latitude": 42.3272,
    "Longitude": -83.3648
  },
  {
    "Date": "2026-07-13",
    "Location": "Derry, NH",
    "Host": "Lucky Dog Events",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 42.8808,
    "Longitude": -71.3265
  },
  {
    "Date": "2026-07-13",
    "Location": "Florham Park, NJ",
    "Host": "For the Love of Dogs NY LLC",
    "TrialTypes": "L1C, L1I, ELT",
    "EventCount": 3,
    "Latitude": 40.838,
    "Longitude": -74.4278
  },
  {
    "Date": "2026-07-17",
    "Location": "Encinitas, CA",
    "Host": "Rewarding Rover LLC & UberDog/Jessica Koester",
    "TrialTypes": "NW2, NW1, ELT-S",
    "EventCount": 3,
    "Latitude": 33.0235,
    "Longitude": -117.2466
  },
  {
    "Date": "2026-07-17",
    "Location": "Leadville, CO",
    "Host": "Mountain Dogs LLC",
    "TrialTypes": "ELT, NW3, ELT-S, NW2",
    "EventCount": 4,
    "Latitude": 39.2465,
    "Longitude": -106.2986
  },
  {
    "Date": "2026-07-18",
    "Location": "Los Osos, CA",
    "Host": "Central Coast Nosework Club",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 35.2693,
    "Longitude": -120.8645
  },
  {
    "Date": "2026-07-18",
    "Location": "Woodbury, MN",
    "Host": "Gretchen Hofheins-Wackerfuss",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 44.8835,
    "Longitude": -92.9134
  },
  {
    "Date": "2026-07-25",
    "Location": "Elmira, OR",
    "Host": "Kiddy Christie",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 44.0198,
    "Longitude": -123.3565
  },
  {
    "Date": "2026-07-29",
    "Location": "Soldotna, AK",
    "Host": "Peninsula Dog Obedience Group LLC",
    "TrialTypes": "NW1, NW2, NW3, ELT",
    "EventCount": 4,
    "Latitude": 60.4523,
    "Longitude": -151.0664
  },
  {
    "Date": "2026-08-01",
    "Location": "Bettendorf, IA",
    "Host": "Fur Better Fur Worse Dog Training",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 41.5686,
    "Longitude": -90.5038
  },
  {
    "Date": "2026-08-01",
    "Location": "Columbia, MO",
    "Host": "Columbia Canine Sports Center, LLC",
    "TrialTypes": "L1V, L1I, L1C, L2C",
    "EventCount": 4,
    "Latitude": 38.9926,
    "Longitude": -92.3387
  },
  {
    "Date": "2026-08-01",
    "Location": "Deming, WA",
    "Host": "The Nosework Magic",
    "TrialTypes": "ELT, NW2",
    "EventCount": 2,
    "Latitude": 48.8479,
    "Longitude": -122.2561
  },
  {
    "Date": "2026-08-01",
    "Location": "Jefferson, WI",
    "Host": "K9 Ventures",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 43.0585,
    "Longitude": -88.816
  },
  {
    "Date": "2026-08-01",
    "Location": "Pillager, MN",
    "Host": "Nose 2 Tail Dog Training LLC",
    "TrialTypes": "NW1, NW2, ELT-P",
    "EventCount": 3,
    "Latitude": 46.3367,
    "Longitude": -94.4701
  },
  {
    "Date": "2026-08-07",
    "Location": "Huntington Beach, CA",
    "Host": "JavaK9s, LLC",
    "TrialTypes": "ELT, L2C, L2I",
    "EventCount": 3,
    "Latitude": 33.718,
    "Longitude": -118.0354
  },
  {
    "Date": "2026-08-08",
    "Location": "Altamont, IL",
    "Host": "Kudos for Canines, LLC",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 39.0561,
    "Longitude": -88.7643
  },
  {
    "Date": "2026-08-14",
    "Location": "La Jolla, CA",
    "Host": "Rewarding Rover LLC & UberDog/Jessica Koester",
    "EventLink": "https://rewardingrover.blogspot.com/",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 32.8215,
    "Longitude": -117.3049
  },
  {
    "Date": "2026-08-15",
    "Location": "Greenwich, CT",
    "Host": "For the Love of Dogs NY LLC",
    "EventLink": "https://www.fortheloveofdogsny.com/nacsw-trials",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 41.0086,
    "Longitude": -73.6098
  },
  {
    "Date": "2026-08-15",
    "Location": "Monmouth, OR",
    "Host": "Doglandia, LLC",
    "EventLink": "https://www.cyberdogonline.com/index.php?option=com_content&view=article&id=146&Itemid=327",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 44.8394,
    "Longitude": -123.1948
  },
  {
    "Date": "2026-08-21",
    "Location": "Chelsea, MI",
    "Host": "Force Free Dale, LLC",
    "EventLink": "https://forcefreedale.com/events",
    "TrialTypes": "NW3, L1V, L1C, NW2",
    "EventCount": 4,
    "Latitude": 42.278,
    "Longitude": -84.0123
  },
  {
    "Date": "2026-08-22",
    "Location": "Greenfield, MA",
    "Host": "Lucky Dog Events",
    "EventLink": "https://www.luckydogevents.com/centre-school-greenfield-mass-aug-2223",
    "TrialTypes": "NW3, NW2, NW1",
    "EventCount": 3,
    "Latitude": 42.5724,
    "Longitude": -72.5802
  },
  {
    "Date": "2026-08-22",
    "Location": "Johnstown, NY",
    "Host": "My Dog Smells LLC",
    "EventLink": "https://www.mydogsmells.com/nacsw-trials",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 43.032,
    "Longitude": -74.4182
  },
  {
    "Date": "2026-08-22",
    "Location": "North Bend, WA",
    "Host": "Northwest K9 Sniffers",
    "EventLink": "https://nwk9sniffers.org/aug-2026-north-bend/",
    "TrialTypes": "ELT, L1C, L2I",
    "EventCount": 3,
    "Latitude": 47.4555,
    "Longitude": -121.7589
  },
  {
    "Date": "2026-08-28",
    "Location": "Easton and Lutherville, MD",
    "Host": "Fair Play Labradors",
    "EventLink": "https://www.fairplaylabradors.com/easton-md-august-28-30-2026.html",
    "TrialTypes": "ELT-S, L2E, L2C, NW1, L1I",
    "EventCount": 5,
    "Latitude": 39.4615,
    "Longitude": -76.5938
  },
  {
    "Date": "2026-08-28",
    "Location": "Meeker, CO",
    "Host": "Mountain Dogs LLC",
    "EventLink": "https://mountaindogs.org/",
    "TrialTypes": "NW3, NW1, L2C, NW2",
    "EventCount": 4,
    "Latitude": 40.0169,
    "Longitude": -107.8932
  },
  {
    "Date": "2026-08-29",
    "Location": "Dunkirk, NY",
    "Host": "Do Over Dog Training",
    "EventLink": "https://www.dooverdogtraining.com/trials",
    "TrialTypes": "NW1, NW2, ELT-S, L3E",
    "EventCount": 4,
    "Latitude": 42.4357,
    "Longitude": -79.3582
  },
  {
    "Date": "2026-08-31",
    "Location": "Cambria, CA",
    "Host": "Gentle Touch Pet Training",
    "EventLink": "https://www.gentlepets.com/gtpt-events/nacsw%E2%84%A2-elt%2Fl1e%2Fl2e-trials",
    "TrialTypes": "ELT, L1E, L2E",
    "EventCount": 3,
    "Latitude": 35.5473,
    "Longitude": -121.0822
  },
  {
    "Date": "2026-09-05",
    "Location": "Luthersville, GA",
    "Host": "Hold The Line K9 LLC",
    "EventLink": "https://www.holdthelinek9nosework.com/",
    "TrialTypes": "L1I, L2I, NW3",
    "EventCount": 3,
    "Latitude": 33.2388,
    "Longitude": -84.7403
  },
  {
    "Date": "2026-09-11",
    "Location": "Richmond, VA",
    "Host": "Paws Plus Training, LLC",
    "EventLink": "https://pawsplustraining.com/",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 37.5129,
    "Longitude": -77.4731
  },
  {
    "Date": "2026-09-12",
    "Location": "Clinton, PA",
    "Host": "Nosework Addicts, LLC",
    "EventLink": "https://www.noseworkaddictsllc.com/nacsw-trials",
    "TrialTypes": "NW1, ELT",
    "EventCount": 2,
    "Latitude": 40.5732,
    "Longitude": -80.3064
  },
  {
    "Date": "2026-09-12",
    "Location": "Lafayette Hill, PA",
    "Host": "Sniff Sniff Hooray",
    "EventLink": "https://sniffsniffhooray.com/",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 40.0471,
    "Longitude": -75.2661
  },
  {
    "Date": "2026-09-12",
    "Location": "Loma Mar, CA",
    "Host": "The Bay Team",
    "EventLink": "https://www.bayteam.org/",
    "TrialTypes": "NW2, ELT",
    "EventCount": 2,
    "Latitude": 37.2887,
    "Longitude": -122.3484
  },
  {
    "Date": "2026-09-12",
    "Location": "Sharon, MA",
    "Host": "Bay State Sniffers",
    "EventLink": "http://www.baystatesniffers.com/",
    "TrialTypes": "ELT, ELT-S, NW2",
    "EventCount": 3,
    "Latitude": 42.1078,
    "Longitude": -71.2152
  },
  {
    "Date": "2026-09-13",
    "Location": "Colesville, MD",
    "Host": "Red Huskies",
    "EventLink": "https://nosework.redhuskies.com/",
    "TrialTypes": "ELT-S, L3C, NW3",
    "EventCount": 3,
    "Latitude": 39.1177,
    "Longitude": -77.0217
  },
  {
    "Date": "2026-09-18",
    "Location": "Flint, MI",
    "Host": "Every Dog Nosework",
    "EventLink": "https://everydognosework.com/trials",
    "TrialTypes": "NW3, NW1, NW2, ELT-P",
    "EventCount": 4,
    "Latitude": 43.0657,
    "Longitude": -83.6445
  },
  {
    "Date": "2026-09-18",
    "Location": "New Milford, PA",
    "Host": "Your Dog's Place, LLC",
    "EventLink": "https://yourdogsplace.com/nacsw-trials/",
    "TrialTypes": "ELT, ELT-S, L1V",
    "EventCount": 3,
    "Latitude": 41.8298,
    "Longitude": -75.7068
  },
  {
    "Date": "2026-09-19",
    "Location": "Ford City, PA",
    "Host": "Steel City Nosework, LLC",
    "EventLink": "https://www.nose-it-all.com/",
    "TrialTypes": "ELT, ELT-S, NW2",
    "EventCount": 3,
    "Latitude": 40.7937,
    "Longitude": -79.5076
  },
  {
    "Date": "2026-09-19",
    "Location": "Glen Mills, PA",
    "Host": "Firezone GS",
    "EventLink": "https://www.firezonegiantschnauzers.com/glenmillsschools",
    "TrialTypes": "NW3, ELT-P",
    "EventCount": 2,
    "Latitude": 39.8977,
    "Longitude": -75.5284
  },
  {
    "Date": "2026-09-19",
    "Location": "Kilmarnock, VA",
    "Host": "Red Huskies",
    "EventLink": "https://nosework.redhuskies.com/",
    "TrialTypes": "ELT, NW1, ELT-S",
    "EventCount": 3,
    "Latitude": 37.6863,
    "Longitude": -76.3319
  },
  {
    "Date": "2026-09-19",
    "Location": "Palmer, MA",
    "Host": "HeavenScent Sniffers",
    "EventLink": "https://www.heavenscentsniffers.com/",
    "TrialTypes": "NW3, L2V, L1E",
    "EventCount": 3,
    "Latitude": 42.1915,
    "Longitude": -72.3281
  },
  {
    "Date": "2026-09-19",
    "Location": "Stevenson, WA",
    "Host": "Sharon Smith",
    "EventLink": "https://www.sundanceshepherds.com/",
    "TrialTypes": "NW1, NW2, L1V, L1C",
    "EventCount": 4,
    "Latitude": 45.6627,
    "Longitude": -121.9118
  },
  {
    "Date": "2026-09-25",
    "Location": "Frederick, MD",
    "Host": "Red Huskies",
    "EventLink": "https://nosework.redhuskies.com/index.php/events/frederick_fall2026/",
    "TrialTypes": "L3E, ELT-S, ELT-P, ELT",
    "EventCount": 4,
    "Latitude": 39.3667,
    "Longitude": -77.3973
  },
  {
    "Date": "2026-09-26",
    "Location": "Columbus, MT",
    "Host": "Canine Connection",
    "EventLink": "https://canineconnection23.godaddysites.com/2026-trials",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 45.5887,
    "Longitude": -109.2282
  },
  {
    "Date": "2026-09-26",
    "Location": "Glenview, IL",
    "Host": "Northwest Obedience Club Inc",
    "EventLink": "https://northwestobedienceclub.org/event/noci-nacsw-elite-trial/",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 42.1115,
    "Longitude": -87.7694
  },
  {
    "Date": "2026-09-26",
    "Location": "Kintnersville, PA",
    "Host": "Paws n’ Sniff",
    "EventLink": "http://www.pawsnsniff.com/september-26-27.-2026.html",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 40.5445,
    "Longitude": -75.1399
  },
  {
    "Date": "2026-09-26",
    "Location": "Lawrenceville, GA",
    "Host": "Right Choice Dog Training, LLC",
    "EventLink": "https://www.rightchoicedogtraining.net/eventandvolunteer",
    "TrialTypes": "L1E, NW2, ELT-S, L3I",
    "EventCount": 4,
    "Latitude": 33.9617,
    "Longitude": -83.9673
  },
  {
    "Date": "2026-09-26",
    "Location": "New City, NY",
    "Host": "Saints2Source, LLC",
    "EventLink": "https://www.saints2source.com/",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 41.1049,
    "Longitude": -73.9592
  },
  {
    "Date": "2026-09-26",
    "Location": "Rehoboth, MA",
    "Host": "Dogs Make Scents",
    "EventLink": "https://dogsmakescents.com/",
    "TrialTypes": "NW3, NW1",
    "EventCount": 2,
    "Latitude": 41.8477,
    "Longitude": -71.2926
  },
  {
    "Date": "2026-09-27",
    "Location": "Dover, DE",
    "Host": "Patricia Grassey",
    "EventLink": "https://thesniffinghound.com/about",
    "TrialTypes": "NW3, NW2, ELT",
    "EventCount": 3,
    "Latitude": 39.177,
    "Longitude": -75.5724
  },
  {
    "Date": "2026-09-28",
    "Location": "Concord, NH",
    "Host": "For the Love of Dogs NY LLC",
    "EventLink": "https://www.fortheloveofdogsny.com/trials-events",
    "TrialTypes": "NW2, NW3",
    "EventCount": 2,
    "Latitude": 43.2202,
    "Longitude": -71.4936
  },
  {
    "Date": "2026-10-03",
    "Location": "Hagerstown, MD",
    "Host": "Firezone GS",
    "EventLink": "https://www.firezonegiantschnauzers.com/nose-work-trials",
    "TrialTypes": "ELT-S, NW3, ELT",
    "EventCount": 3,
    "Latitude": 39.6124,
    "Longitude": -77.7015
  },
  {
    "Date": "2026-10-03",
    "Location": "Hammond, LA",
    "Host": "Dog Gone Right, LLC",
    "EventLink": "https://doggoneright.net/",
    "TrialTypes": "NW1, NW2, L1I, ELT-S",
    "EventCount": 4,
    "Latitude": 30.5493,
    "Longitude": -90.4674
  },
  {
    "Date": "2026-10-03",
    "Location": "Jefferson, OR",
    "Host": "Doglandia, LLC",
    "EventLink": "https://www.cyberdogonline.com/",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 44.6163,
    "Longitude": -121.2963
  },
  {
    "Date": "2026-10-03",
    "Location": "Nashua, NH",
    "Host": "The Big Sniff, LLC",
    "EventLink": "http://www.thebigsniff.com/",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 42.719,
    "Longitude": -71.4415
  },
  {
    "Date": "2026-10-03",
    "Location": "New Paltz, NY",
    "Host": "Top Notch Dogs, LLC",
    "EventLink": "https://www.topnotchdogtraining.com/",
    "TrialTypes": "NW1, L2C, ELT",
    "EventCount": 3,
    "Latitude": 41.7919,
    "Longitude": -74.1319
  },
  {
    "Date": "2026-10-03",
    "Location": "Northampton, MA",
    "Host": "Lucky Dog Events",
    "EventLink": "https://www.luckydogevents.com/",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 42.3641,
    "Longitude": -72.6527
  },
  {
    "Date": "2026-10-03",
    "Location": "Seguin, TX",
    "Host": "Scent Work Across Texas",
    "EventLink": "https://scentworkacrosstexas.com/",
    "TrialTypes": "NW1, NW2, NW3",
    "EventCount": 3,
    "Latitude": 29.5607,
    "Longitude": -97.9539
  },
  {
    "Date": "2026-10-03",
    "Location": "Sisters, OR",
    "Host": "Sunriver K9 Genie, LLC",
    "EventLink": "https://k9genie.com/events",
    "TrialTypes": "NW2, ELT",
    "EventCount": 2,
    "Latitude": 44.3343,
    "Longitude": -121.58
  },
  {
    "Date": "2026-10-03",
    "Location": "Waynesboro, PA",
    "Host": "Nose-It-All, LLC",
    "EventLink": "https://www.nose-it-all.com/",
    "TrialTypes": "NW1, NW2, NW3",
    "EventCount": 3,
    "Latitude": 39.8003,
    "Longitude": -77.5943
  },
  {
    "Date": "2026-10-09",
    "Location": "Middlebury, CT",
    "Host": "For the Love of Dogs NY LLC",
    "EventLink": "https://www.fortheloveofdogsny.com/trials-events",
    "TrialTypes": "ELT, ELT-P, NW1",
    "EventCount": 3,
    "Latitude": 41.5134,
    "Longitude": -73.1588
  },
  {
    "Date": "2026-10-09",
    "Location": "Pueblo, CO",
    "Host": "Mountain Dogs, LLC",
    "EventLink": "https://mountaindogs.org/",
    "TrialTypes": "SMT, ELT",
    "EventCount": 2,
    "Latitude": 38.2708,
    "Longitude": -104.6156
  },
  {
    "Date": "2026-10-09",
    "Location": "Rock Island, IL",
    "Host": "Fur Better Fur Worse Dog Training",
    "EventLink": "http://www.furbetterfurworse.com/",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 41.3946,
    "Longitude": -90.5849
  },
  {
    "Date": "2026-10-10",
    "Location": "Auburn, WA",
    "Host": "Northwest K9 Sniffers",
    "EventLink": "https://nwk9sniffers.org/",
    "TrialTypes": "ELT-S, L2E, L3I",
    "EventCount": 3,
    "Latitude": 47.2648,
    "Longitude": -122.1901
  },
  {
    "Date": "2026-10-10",
    "Location": "Court Granger, IA",
    "Host": "KBP Dog Training",
    "EventLink": "https://kbpdogtraining.com",
    "TrialTypes": "NW1, NW2, NW3",
    "EventCount": 3,
    "Latitude": 41.8062,
    "Longitude": -93.8027
  },
  {
    "Date": "2026-10-10",
    "Location": "Eagan, MN",
    "Host": "St Paul Dog Training Club",
    "EventLink": "https://spdtc.com/events-at-spdtc/",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 44.8059,
    "Longitude": -93.1903
  },
  {
    "Date": "2026-10-10",
    "Location": "Eldred, NY",
    "Host": "Your Dog's Place, LLC",
    "EventLink": "http://www.yourdogsplace.com/",
    "TrialTypes": "ELT-S, L2C, L2I, NW2",
    "EventCount": 4,
    "Latitude": 41.55,
    "Longitude": -74.8902
  },
  {
    "Date": "2026-10-10",
    "Location": "Helena, MT",
    "Host": "Nose Work Breakfast Club",
    "EventLink": "https://noseworkbreakfastclub.com/our-events/",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 46.5847,
    "Longitude": -112.027
  },
  {
    "Date": "2026-10-10",
    "Location": "Loveland, CO",
    "Host": "Paws 4 Thought Dog Training, LLC",
    "EventLink": "https://www.p4tnosework.com/premiumloveland",
    "TrialTypes": "NW2, NW1, L1E",
    "EventCount": 3,
    "Latitude": 40.4149,
    "Longitude": -105.1076
  },
  {
    "Date": "2026-10-10",
    "Location": "Sedona, AZ",
    "Host": "Successful Sniffer",
    "EventLink": "https://www.successfulsniffer.com/trials-and-events",
    "TrialTypes": "ELT, NW1, NW2",
    "EventCount": 3,
    "Latitude": 34.9105,
    "Longitude": -111.7141
  },
  {
    "Date": "2026-10-10",
    "Location": "Troy, VA",
    "Host": "Your Dogs Knows LLC",
    "EventLink": "https://yourdogknows.net/",
    "TrialTypes": "NW1, ELT-S, L1V, L2V",
    "EventCount": 4,
    "Latitude": 37.9602,
    "Longitude": -78.2034
  },
  {
    "Date": "2026-10-10",
    "Location": "Youngwood, PA",
    "Host": "Steel City Nosework, LLC",
    "EventLink": "https://www.nose-it-all.com/",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 40.243,
    "Longitude": -79.6045
  },
  {
    "Date": "2026-10-12",
    "Location": "Swansea, MA",
    "Host": "Amy Conrad & Heaven Scent Sniffers",
    "EventLink": "https://sniffstreams.smugmug.com/Events",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 41.7009,
    "Longitude": -71.2253
  },
  {
    "Date": "2026-10-16",
    "Location": "Calhan, CO",
    "Host": "Mountain Dogs LLC",
    "EventLink": "https://mountaindogs.org/",
    "TrialTypes": "ELT, NW3, ELT-P",
    "EventCount": 3,
    "Latitude": 39.0555,
    "Longitude": -104.2482
  },
  {
    "Date": "2026-10-16",
    "Location": "Rossville, GA",
    "Host": "Camelot Shepherds, Inc.",
    "EventLink": "https://www.snifferschool.com/events",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 34.9694,
    "Longitude": -85.2437
  },
  {
    "Date": "2026-10-16",
    "Location": "Wilmington, DE",
    "Host": "Patricia Grassey",
    "EventLink": "https://thesniffinghound.com/about",
    "TrialTypes": "NW3, ELT, ELT-P",
    "EventCount": 3,
    "Latitude": 39.7107,
    "Longitude": -75.5883
  },
  {
    "Date": "2026-10-17",
    "Location": "Albuquerque, NM",
    "Host": "New Mexico Canine Scent Work, LLC",
    "EventLink": "https://www.nmcsw.com/events/#oct26",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 35.1148,
    "Longitude": -106.652
  },
  {
    "Date": "2026-10-17",
    "Location": "Centralia, WA",
    "Host": "Let's Talk Dogs, LLC and About Face K9",
    "EventLink": "http://www.dorothyturley.com/",
    "TrialTypes": "ELT-S, NW2, L3C",
    "EventCount": 3,
    "Latitude": 46.6751,
    "Longitude": -122.9537
  },
  {
    "Date": "2026-10-17",
    "Location": "Colebrook, CT",
    "Host": "For the Love of Dogs NY LLC",
    "EventLink": "https://www.fortheloveofdogsny.com/nacsw-trials",
    "TrialTypes": "L1E, ELT-S, NW2, ELT",
    "EventCount": 4,
    "Latitude": 41.9926,
    "Longitude": -73.0697
  },
  {
    "Date": "2026-10-17",
    "Location": "Conroe, TX",
    "Host": "Scent Work Across Texas",
    "EventLink": "https://scentworkacrosstexas.com/",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 30.3501,
    "Longitude": -95.4813
  },
  {
    "Date": "2026-10-17",
    "Location": "Delevan, NY",
    "Host": "Do Over Dog Training",
    "EventLink": "https://www.dooverdogtraining.com/trials",
    "TrialTypes": "NW3, L1C, L3V",
    "EventCount": 3,
    "Latitude": 42.501,
    "Longitude": -78.4985
  },
  {
    "Date": "2026-10-17",
    "Location": "Niantic, IL",
    "Host": "Kudos for Canines, LLC",
    "EventLink": "https://kudosforcanines.com/",
    "TrialTypes": "L2C, ELT-S, NW3",
    "EventCount": 3,
    "Latitude": 39.8534,
    "Longitude": -89.2085
  },
  {
    "Date": "2026-10-17",
    "Location": "Staples, MN",
    "Host": "Nose 2 Tail Dog Training LLC",
    "EventLink": "https://nose2tail.net/",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 46.3296,
    "Longitude": -94.7523
  },
  {
    "Date": "2026-10-17",
    "Location": "Watsonville, CA",
    "Host": "CalCoastal Dog Owners Group",
    "EventLink": "https://cc-dog.org/",
    "TrialTypes": "L3V, L2V, L1V",
    "EventCount": 3,
    "Latitude": 36.9007,
    "Longitude": -121.777
  },
  {
    "Date": "2026-10-19",
    "Location": "Ellicott City, MD",
    "Host": "Red Huskies",
    "EventLink": "https://nosework.redhuskies.com/",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 39.3005,
    "Longitude": -76.8244
  },
  {
    "Date": "2026-10-24",
    "Location": "Cumming, GA",
    "Host": "Georgia Nosework",
    "EventLink": "https://georgianosework.com/",
    "TrialTypes": "NW3, L1C, NW2",
    "EventCount": 3,
    "Latitude": 34.19,
    "Longitude": -84.1441
  },
  {
    "Date": "2026-10-24",
    "Location": "Fishkill, NY",
    "Host": "Top Notch Dogs, LLC",
    "EventLink": "https://www.topnotchdogtraining.com",
    "TrialTypes": "ELT-P, ELT",
    "EventCount": 2,
    "Latitude": 41.5526,
    "Longitude": -73.9465
  },
  {
    "Date": "2026-10-24",
    "Location": "Green Bay, WI",
    "Host": "NEWk9Scent Work LLC",
    "EventLink": "http://newk9scentwork.com/",
    "TrialTypes": "NW2, ELT",
    "EventCount": 2,
    "Latitude": 44.4742,
    "Longitude": -88.0129
  },
  {
    "Date": "2026-10-24",
    "Location": "Norton, MA",
    "Host": "Dogs Make Scents",
    "EventLink": "https://dogsmakescents.com/events/",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 41.9379,
    "Longitude": -71.1631
  },
  {
    "Date": "2026-10-24",
    "Location": "Penn Yan, NY",
    "Host": "2 Psyched 4 Dogs",
    "EventLink": "https://2psyched4dogs.com/",
    "TrialTypes": "ELT-S, NW2, ELT",
    "EventCount": 3,
    "Latitude": 42.6557,
    "Longitude": -77.0182
  },
  {
    "Date": "2026-10-24",
    "Location": "Reedsport, OR",
    "Host": "Wells Creek Dog Training",
    "EventLink": "https://wellscreekdogtraining.com/",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 43.7466,
    "Longitude": -124.0784
  },
  {
    "Date": "2026-10-26",
    "Location": "Stockton, CA",
    "Host": "Two Nosey Girls",
    "EventLink": "https://twonoseygirls.com/",
    "TrialTypes": "L3E, L2E",
    "EventCount": 2,
    "Latitude": 37.951,
    "Longitude": -121.2543
  },
  {
    "Date": "2026-10-30",
    "Location": "Cameron Park, CA",
    "Host": "Sierra Sniffing Canines, Inc",
    "EventLink": "https://sierrasniffingcanines.org/",
    "TrialTypes": "NW1, NW3",
    "EventCount": 2,
    "Latitude": 38.6734,
    "Longitude": -120.9694
  },
  {
    "Date": "2026-10-30",
    "Location": "Harrington, DE",
    "Host": "Shamrock Pot Of Gold K9 Scenter",
    "EventLink": "https://shamrockpotofgoldk9scenter.com/",
    "TrialTypes": "ELT-S, L2E, NW3, ELT, NW1",
    "EventCount": 5,
    "Latitude": 38.9526,
    "Longitude": -75.6128
  },
  {
    "Date": "2026-10-30",
    "Location": "Honey Brook, PA",
    "Host": "Patricia Grassey",
    "EventLink": "https://thesniffinghound.com/",
    "TrialTypes": "NW1, L1C, NW2, L2C, L3I, L3C",
    "EventCount": 6,
    "Latitude": 40.1401,
    "Longitude": -75.8709
  },
  {
    "Date": "2026-10-30",
    "Location": "Lakeville, MN",
    "Host": "St Paul Dog Training Club",
    "EventLink": "https://spdtc.com/events-at-spdtc/",
    "TrialTypes": "ELT-P, NW2, ELT-S, L1C",
    "EventCount": 4,
    "Latitude": 44.6697,
    "Longitude": -93.2521
  },
  {
    "Date": "2026-10-30",
    "Location": "Lawrenceville, GA",
    "Host": "Chestnut Hill Canine Sports",
    "EventLink": "http://chestnuthillcaninesports.com/lawrenceville-2025/",
    "TrialTypes": "NW3, NW1, L2I",
    "EventCount": 3,
    "Latitude": 33.9164,
    "Longitude": -84.0312
  },
  {
    "Date": "2026-10-30",
    "Location": "Montrose, CO",
    "Host": "Mountain Dogs LLC",
    "EventLink": "https://mountaindogs.org/",
    "TrialTypes": "ELT, ELT-S, NW2",
    "EventCount": 3,
    "Latitude": 38.5188,
    "Longitude": -107.8861
  },
  {
    "Date": "2026-10-30",
    "Location": "York, PA",
    "Host": "Firezone GS",
    "EventLink": "https://www.firezonegiantschnauzers.com/nose-work-trials",
    "TrialTypes": "ELT, NW3, ELT-P",
    "EventCount": 3,
    "Latitude": 39.9146,
    "Longitude": -76.7184
  },
  {
    "Date": "2026-10-31",
    "Location": "Beloit, WI",
    "Host": "George Carpenter",
    "EventLink": "https://gscarpenter.wixsite.com/scwnw/trials",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 42.4921,
    "Longitude": -89.0598
  },
  {
    "Date": "2026-10-31",
    "Location": "Bonham, TX",
    "Host": "All About The Nose",
    "EventLink": "https://www.allaboutthenose.com/",
    "TrialTypes": "NW2, NW1",
    "EventCount": 2,
    "Latitude": 33.5308,
    "Longitude": -96.1647
  },
  {
    "Date": "2026-10-31",
    "Location": "Franklin, GA",
    "Host": "Hold The Line K9 LLC",
    "EventLink": "https://www.holdthelinek9nosework.com/",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 34.4029,
    "Longitude": -83.2091
  },
  {
    "Date": "2026-10-31",
    "Location": "Kennebunkport, ME",
    "Host": "Elizabeth Dutton",
    "EventLink": "https://ehdutton.wordpress.com/",
    "TrialTypes": "NW3, ELT-P",
    "EventCount": 2,
    "Latitude": 43.3355,
    "Longitude": -70.4616
  },
  {
    "Date": "2026-10-31",
    "Location": "Plant City, FL",
    "Host": "Hoppin’ in the Hills",
    "EventLink": "https://hoppininthehillscom.wordpress.com",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 28.0634,
    "Longitude": -82.0806
  },
  {
    "Date": "2026-10-31",
    "Location": "Sturgis, SD",
    "Host": "Two Paws Up Dog Training, LLC",
    "EventLink": "https://www.twopawsupdogtrainingllc.com/",
    "TrialTypes": "L1V, L2E, L2V, L1E",
    "EventCount": 4,
    "Latitude": 44.4367,
    "Longitude": -103.49
  },
  {
    "Date": "2026-10-31",
    "Location": "White Plains, NY",
    "Host": "Saints2Source, LLC",
    "EventLink": "https://www.saints2source.com/",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 41.0837,
    "Longitude": -73.7544
  },
  {
    "Date": "2026-10-31",
    "Location": "Yamhill, OR",
    "Host": "Nose Work Detectives, LLC",
    "EventLink": "https://noseworkdetectives.com/",
    "TrialTypes": "ELT-P",
    "EventCount": 1,
    "Latitude": 45.2593,
    "Longitude": -123.2179
  },
  {
    "Date": "2026-11-01",
    "Location": "San Martin, CA",
    "Host": "B.L. McMutts LLC",
    "EventLink": "https://blmcmutts.com/events/nacsw-element-specialty-trial-nov26",
    "TrialTypes": "L1V, L2V",
    "EventCount": 2,
    "Latitude": 37.0787,
    "Longitude": -121.5686
  },
  {
    "Date": "2026-11-03",
    "Location": "Ventura, CA",
    "Host": "Pink Biscuit K9s",
    "EventLink": "https://www.pinkbiscuitk9s.com/",
    "TrialTypes": "NW1, NW2, ELT-P",
    "EventCount": 3,
    "Latitude": 34.4858,
    "Longitude": -119.0937
  },
  {
    "Date": "2026-11-06",
    "Location": "Rome, GA",
    "Host": "Georgia Nosework, LLC",
    "EventLink": "https://georgianosework.com/events/",
    "TrialTypes": "NW3, ELT, NW1, NW2",
    "EventCount": 4,
    "Latitude": 34.2207,
    "Longitude": -85.1471
  },
  {
    "Date": "2026-11-07",
    "Location": "Bonner Springs, KS",
    "Host": "Brookside Pet Concierge",
    "EventLink": "https://bksdogtraining.com/",
    "TrialTypes": "NW1, NW2, NW3",
    "EventCount": 3,
    "Latitude": 39.0276,
    "Longitude": -94.8715
  },
  {
    "Date": "2026-11-07",
    "Location": "Colorado Springs, CO",
    "Host": "Beyond Elevation K9",
    "EventLink": "https://www.beyondelevationk9.com/",
    "TrialTypes": "NW3, NW2, NW1",
    "EventCount": 3,
    "Latitude": 38.8031,
    "Longitude": -104.8236
  },
  {
    "Date": "2026-11-07",
    "Location": "Geneva, IL",
    "Host": "For Your K9, Inc",
    "EventLink": "http://www.foryourk9.com/",
    "TrialTypes": "ELT, NW1, NW2",
    "EventCount": 3,
    "Latitude": 41.871,
    "Longitude": -88.2989
  },
  {
    "Date": "2026-11-07",
    "Location": "Las Vegas, NV",
    "Host": "imPETus Animal Training",
    "EventLink": "https://www.impetusanimaltraining.com/",
    "TrialTypes": "NW3, NW1, L1C",
    "EventCount": 3,
    "Latitude": 36.1623,
    "Longitude": -115.1243
  },
  {
    "Date": "2026-11-07",
    "Location": "Mays Landing, NJ",
    "Host": "Rotts-n-Notts Nosework LLC",
    "EventLink": "https://www.rottsnnottsnosework.com/",
    "TrialTypes": "L1C, NW2, L1E, NW1",
    "EventCount": 4,
    "Latitude": 39.4506,
    "Longitude": -74.7176
  },
  {
    "Date": "2026-11-07",
    "Location": "Wappingers Falls, NY",
    "Host": "Top Notch Dogs, LLC",
    "EventLink": "https://www.topnotchdogtraining.com/",
    "TrialTypes": "L3C, NW2, ELT",
    "EventCount": 3,
    "Latitude": 41.5816,
    "Longitude": -73.9349
  },
  {
    "Date": "2026-11-07",
    "Location": "Woodward, IA",
    "Host": "KBP Dog Training",
    "EventLink": "https://kbpdogtraining.com/",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 41.8424,
    "Longitude": -93.9461
  },
  {
    "Date": "2026-11-09",
    "Location": "West Berlin, NJ",
    "Host": "For the Love of Dogs NY LLC",
    "EventLink": "https://www.fortheloveofdogsny.com/trials-events",
    "TrialTypes": "L1V, L2E, NW3, ELT-P",
    "EventCount": 4,
    "Latitude": 39.8281,
    "Longitude": -74.9553
  },
  {
    "Date": "2026-11-11",
    "Location": "Petaluma, CA",
    "Host": "Marin Humane",
    "EventLink": "https://marinhumane.org/oh-behave/nose-work-events/",
    "TrialTypes": "ELT-P, ELT",
    "EventCount": 2,
    "Latitude": 38.2168,
    "Longitude": -122.6721
  },
  {
    "Date": "2026-11-13",
    "Location": "Chula Vista, CA",
    "Host": "Rewarding Rover LLC, Uberdog, & Claire Brocato",
    "EventLink": "https://www.rewardingrover.com/",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 32.6873,
    "Longitude": -117.1333
  },
  {
    "Date": "2026-11-13",
    "Location": "Gilbertsville, PA",
    "Host": "Sniff Sniff Hooray",
    "EventLink": "https://sniffsniffhooray.com/events",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 40.3517,
    "Longitude": -75.6512
  },
  {
    "Date": "2026-11-13",
    "Location": "Ypsilanti, MI",
    "Host": "Every Dog Nosework",
    "EventLink": "https://everydognosework.com/trials",
    "TrialTypes": "ELT, ELT-S, L2C",
    "EventCount": 3,
    "Latitude": 42.2486,
    "Longitude": -83.6087
  },
  {
    "Date": "2026-11-14",
    "Location": "Greer, SC",
    "Host": "Trained to Trust, LLC",
    "EventLink": "http://www.k9trainedtotrust.com/",
    "TrialTypes": "NW3, L2V, NW1",
    "EventCount": 3,
    "Latitude": 34.9211,
    "Longitude": -82.2481
  },
  {
    "Date": "2026-11-14",
    "Location": "Montgomery, AL",
    "Host": "By A Nose Nosework",
    "EventLink": "https://www.byanosenosework.com/",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 32.3524,
    "Longitude": -86.3421
  },
  {
    "Date": "2026-11-14",
    "Location": "Waymart, PA",
    "Host": "Your Dog's Place, LLC",
    "EventLink": "http://www.yourdogsplace.com/",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 41.5973,
    "Longitude": -75.4478
  },
  {
    "Date": "2026-11-16",
    "Location": "Hartford, CT",
    "Host": "For the Love of Dogs NY LLC",
    "EventLink": "https://www.fortheloveofdogsny.com/trials-events",
    "TrialTypes": "ELT, ELT-S, L1I",
    "EventCount": 3,
    "Latitude": 41.7389,
    "Longitude": -72.7354
  },
  {
    "Date": "2026-11-20",
    "Location": "Boring, OR",
    "Host": "Trust Your Dog K9 Events",
    "EventLink": "https://trustyourdogk9events.com/",
    "TrialTypes": "NW2, NW3, ELT",
    "EventCount": 3,
    "Latitude": 45.4061,
    "Longitude": -122.3704
  },
  {
    "Date": "2026-11-20",
    "Location": "Centreville, MD",
    "Host": "Fair Play Point Labradors",
    "EventLink": "https://www.fairplaylabradors.com/",
    "TrialTypes": "SMT, ELT-S, L1I",
    "EventCount": 3,
    "Latitude": 39.0214,
    "Longitude": -76.0821
  },
  {
    "Date": "2026-11-20",
    "Location": "Denver, PA",
    "Host": "Patricia Grassey",
    "EventLink": "https://thesniffinghound.com/",
    "TrialTypes": "ELT, ELT-P, ELT-S, L3V",
    "EventCount": 4,
    "Latitude": 40.279,
    "Longitude": -76.1635
  },
  {
    "Date": "2026-11-20",
    "Location": "Lompoc, CA",
    "Host": "Gentle Touch Pet Training",
    "EventLink": "https://www.gentlepets.com/gtpt-events/nacsw%E2%84%A2-nw3",
    "TrialTypes": "NW3, NW2, NW1",
    "EventCount": 3,
    "Latitude": 34.6678,
    "Longitude": -120.4189
  },
  {
    "Date": "2026-11-20",
    "Location": "Loranger, LA",
    "Host": "Dog Gone Right",
    "EventLink": "http://www.doggoneright.net/",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 30.643,
    "Longitude": -90.3523
  },
  {
    "Date": "2026-11-21",
    "Location": "Centralia, WA",
    "Host": "Let's Talk Dogs, LLC & About Face K9 Academy",
    "EventLink": "https://www.aboutfacek9academy.com/",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 46.7688,
    "Longitude": -122.9754
  },
  {
    "Date": "2026-11-21",
    "Location": "DeLeon Springs, FL",
    "Host": "River Poodles Training, LLC",
    "EventLink": "https://riverpoodlestraining.com/",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 29.1792,
    "Longitude": -81.3084
  },
  {
    "Date": "2026-11-21",
    "Location": "Delta, CO",
    "Host": "Mountain Dogs LLC",
    "EventLink": "https://mountaindogs.org/",
    "TrialTypes": "ELT, NW3, NW1, NW2",
    "EventCount": 4,
    "Latitude": 38.7971,
    "Longitude": -107.8157
  },
  {
    "Date": "2026-11-21",
    "Location": "Marble Falls, TX",
    "Host": "Scent Work Across Texas",
    "EventLink": "https://scentworkacrosstexas.com/",
    "TrialTypes": "ELT-S, L2I, NW3",
    "EventCount": 3,
    "Latitude": 30.6137,
    "Longitude": -98.2694
  },
  {
    "Date": "2026-11-21",
    "Location": "Ontario, CA",
    "Host": "Agile Paws Dog Sports",
    "EventLink": "https://agilepawsdogsports.com/",
    "TrialTypes": "NW1, L3C, L3I",
    "EventCount": 3,
    "Latitude": 34.0499,
    "Longitude": -117.6584
  },
  {
    "Date": "2026-11-21",
    "Location": "Smyrna, TN",
    "Host": "Dogs Have Amazing Noses, LLC",
    "EventLink": "https://dogshaveamazingnoses.com/events/",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 35.9391,
    "Longitude": -86.492
  },
  {
    "Date": "2026-11-22",
    "Location": "Wilbraham, MA",
    "Host": "Heaven Scent Sniffers",
    "EventLink": "https://www.heavenscentsniffers.com/",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 42.1304,
    "Longitude": -72.4299
  },
  {
    "Date": "2026-11-27",
    "Location": "Elizabeth, CO",
    "Host": "Beyond Elevation K9",
    "EventLink": "https://www.beyondelevationk9.com/",
    "TrialTypes": "NW2, NW3",
    "EventCount": 2,
    "Latitude": 39.3261,
    "Longitude": -104.5711
  },
  {
    "Date": "2026-11-27",
    "Location": "Long Beach, CA",
    "Host": "JavaK9s, LLC",
    "EventLink": "http://www.javak9s.com/",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 33.7764,
    "Longitude": -118.2188
  },
  {
    "Date": "2026-11-27",
    "Location": "San Jose, CA",
    "Host": "The Bay Team",
    "EventLink": "https://www.bayteam.org/",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 37.3547,
    "Longitude": -121.8592
  },
  {
    "Date": "2026-11-28",
    "Location": "Cottage Grove, MN",
    "Host": "Gretchen Hofheins-Wackerfuss",
    "EventLink": "https://www.sniffingminpin.com/",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 44.8769,
    "Longitude": -92.9114
  },
  {
    "Date": "2026-11-28",
    "Location": "Fort Morgan, CO",
    "Host": "Paws 4 Thought Dog Training, LLC",
    "EventLink": "http://p4tnosework.com/",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 40.2723,
    "Longitude": -103.8171
  },
  {
    "Date": "2026-11-28",
    "Location": "Lebanon, NJ",
    "Host": "Sirius K9 Solutions",
    "EventLink": "http://www.siriusk9solutions.net/NoseWork.html",
    "TrialTypes": "NW2, ELT-P",
    "EventCount": 2,
    "Latitude": 40.6676,
    "Longitude": -74.8305
  },
  {
    "Date": "2026-11-28",
    "Location": "Mifflinburg, PA",
    "Host": "Paws-itively Obedient Dog Training School",
    "EventLink": "https://pawsitivelyobedient.net/",
    "TrialTypes": "NW3, NW1",
    "EventCount": 2,
    "Latitude": 40.9095,
    "Longitude": -77.0087
  },
  {
    "Date": "2026-11-28",
    "Location": "Silex, MO",
    "Host": "WestInn Kennels",
    "EventLink": "https://westinnkennels.wixsite.com/silex",
    "TrialTypes": "NW1, NW2, NW3",
    "EventCount": 3,
    "Latitude": 39.1544,
    "Longitude": -91.0657
  },
  {
    "Date": "2026-11-28",
    "Location": "Vancouver, WA",
    "Host": "Sniffketeers",
    "EventLink": "https://noseworktrial.blogspot.com/",
    "TrialTypes": "NW1, L1E, L1I, L3V",
    "EventCount": 4,
    "Latitude": 45.6262,
    "Longitude": -122.6568
  },
  {
    "Date": "2026-11-29",
    "Location": "Gettysburg, PA",
    "Host": "Firezone GS",
    "EventLink": "https://www.firezonegiantschnauzers.com/nose-work-trials",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 39.8593,
    "Longitude": -77.1998
  },
  {
    "Date": "2026-12-05",
    "Location": "Centralia, WA",
    "Host": "Let's Talk Dogs, LLC and About Face K9",
    "EventLink": "http://www.dorothyturley.com/",
    "TrialTypes": "ELT, NW1, L2I",
    "EventCount": 3,
    "Latitude": 46.6974,
    "Longitude": -122.9555
  },
  {
    "Date": "2026-12-05",
    "Location": "Fillmore, CA",
    "Host": "Pink Biscuit K9s",
    "EventLink": "https://www.pinkbiscuitk9s.com/",
    "TrialTypes": "NW3, ELT-S, L2C",
    "EventCount": 3,
    "Latitude": 34.4136,
    "Longitude": -118.8636
  },
  {
    "Date": "2026-12-05",
    "Location": "Fredonia, WI",
    "Host": "On Point Elite Dog Sports, LLC",
    "EventLink": "https://www.opedogsports.com/nacsw-trials",
    "TrialTypes": "NW1, NW2, NW3",
    "EventCount": 3,
    "Latitude": 43.498,
    "Longitude": -87.9201
  },
  {
    "Date": "2026-12-05",
    "Location": "Hoover, AL",
    "Host": "Southeast Scent Work Alliance, LLC (SSWA)",
    "EventLink": "https://www.southeastscent.com/nw3-elite-hoover-al/",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 33.3489,
    "Longitude": -86.8958
  },
  {
    "Date": "2026-12-05",
    "Location": "Hubertus, WI",
    "Host": "Loving Paws Dog Training, LLC",
    "EventLink": "https://www.lovingpawsllc.com/",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 43.2841,
    "Longitude": -88.1736
  },
  {
    "Date": "2026-12-05",
    "Location": "Newfoundland, PA",
    "Host": "Your Dog's Place, LLC",
    "EventLink": "http://www.yourdogsplace.com/",
    "TrialTypes": "L2V, L3C, NW3",
    "EventCount": 3,
    "Latitude": 41.3321,
    "Longitude": -75.3284
  },
  {
    "Date": "2026-12-05",
    "Location": "Tucson, AZ",
    "Host": "Patience Unlimited Dog Training",
    "EventLink": "http://www.patienceunlimited.com/nacsw.html",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 32.2683,
    "Longitude": -110.9667
  },
  {
    "Date": "2026-12-07",
    "Location": "Stockton, CA",
    "Host": "Two Nosey Girls",
    "EventLink": "https://www.twonoseygirls.com/events.html",
    "TrialTypes": "ELT, ELT-S, L3I",
    "EventCount": 3,
    "Latitude": 37.916,
    "Longitude": -121.3169
  },
  {
    "Date": "2026-12-11",
    "Location": "Douglassville, PA",
    "Host": "Patricia Grassey",
    "EventLink": "https://www.thesniffinghound.com/",
    "TrialTypes": "NW3, NW2, ELT",
    "EventCount": 3,
    "Latitude": 40.2452,
    "Longitude": -75.7695
  },
  {
    "Date": "2026-12-12",
    "Location": "Sauget, IL",
    "Host": "Happy Dog Concepts, LLC",
    "EventLink": "https://happydogconcepts.com/events",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 38.5779,
    "Longitude": -90.1503
  },
  {
    "Date": "2026-12-13",
    "Location": "McMinnville, OR",
    "Host": "Carol Forsberg and Doglandia, LLC",
    "EventLink": "https://www.justnosework.com",
    "TrialTypes": "NW2, ELT",
    "EventCount": 2,
    "Latitude": 45.2165,
    "Longitude": -123.1563
  },
  {
    "Date": "2026-12-15",
    "Location": "Redlands, CA",
    "Host": "Agile Paws Dog Sports",
    "EventLink": "https://agilepawsdogsports.com/",
    "TrialTypes": "ELT, ELT-S",
    "EventCount": 2,
    "Latitude": 34.0717,
    "Longitude": -117.2239
  },
  {
    "Date": "2026-12-18",
    "Location": "Pittstown, NJ",
    "Host": "Shamrock Pot Of Gold K9 Scenter",
    "EventLink": "https://shamrockpotofgoldk9scenter.com/",
    "TrialTypes": "ELT-S, L1C, NW3, ELT",
    "EventCount": 4,
    "Latitude": 40.6269,
    "Longitude": -74.9413
  },
  {
    "Date": "2026-12-19",
    "Location": "Alpharetta, GA",
    "Host": "Georgia Nosework",
    "EventLink": "https://georgianosework.com/",
    "TrialTypes": "SMT, ELT-P",
    "EventCount": 2,
    "Latitude": 34.0443,
    "Longitude": -84.3331
  },
  {
    "Date": "2026-12-19",
    "Location": "Imperial Beach, CA",
    "Host": "Rewarding Rover LLC/Uber dog/Claire Brocato",
    "EventLink": "https://www.rewardingrover.com/",
    "TrialTypes": "ELT, L1E, NW1",
    "EventCount": 3,
    "Latitude": 32.5874,
    "Longitude": -117.1468
  },
  {
    "Date": "2026-12-28",
    "Location": "Phoenix, AZ",
    "Host": "Release Canine LLC",
    "EventLink": "https://www.releasecanine.com/nacsw",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 33.4646,
    "Longitude": -112.0667
  },
  {
    "Date": "2026-12-28",
    "Location": "Tyngsborough, MA",
    "Host": "Spot On K9 Coaching",
    "EventLink": "https://www.sniffalertfinish.com/",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 42.6323,
    "Longitude": -71.3859
  },
  {
    "Date": "2026-12-31",
    "Location": "Corvallis, OR",
    "Host": "PNW Sniffers",
    "EventLink": "https://pnwsniffers.com/",
    "TrialTypes": "NW3, NW1",
    "EventCount": 2,
    "Latitude": 44.5161,
    "Longitude": -123.2292
  },
  {
    "Date": "2027-01-02",
    "Location": "Bonsall, CA",
    "Host": "Linda Buchanan",
    "EventLink": "https://www.k9slovetosearch.com/",
    "TrialTypes": "ELT, NW2",
    "EventCount": 2,
    "Latitude": 33.3222,
    "Longitude": -117.202
  },
  {
    "Date": "2027-01-03",
    "Location": "Bee Cave, TX",
    "Host": "Scent Work Across Texas",
    "EventLink": "https://scentworkacrosstexas.com/",
    "TrialTypes": "ELT-S, NW1, NW3",
    "EventCount": 3,
    "Latitude": 30.341,
    "Longitude": -97.9084
  },
  {
    "Date": "2027-01-09",
    "Location": "Bellingham, WA",
    "Host": "The Nosework Magic",
    "EventLink": "https://www.noseworkmagic.com/",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 48.7695,
    "Longitude": -122.4984
  },
  {
    "Date": "2027-01-09",
    "Location": "Novato, CA",
    "Host": "Marin Humane",
    "EventLink": "https://marinhumane.org/oh-behave/nose-work-events/",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 38.1247,
    "Longitude": -122.5951
  },
  {
    "Date": "2027-01-15",
    "Location": "Upland, CA",
    "Host": "Agile Paws Dog Sports",
    "EventLink": "https://agilepawsdogsports.com/",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 34.1047,
    "Longitude": -117.6102
  },
  {
    "Date": "2027-01-16",
    "Location": "Melrose, FL",
    "Host": "River Poodles Training, LLC",
    "EventLink": "https://riverpoodlestraining.com/",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 29.7118,
    "Longitude": -82.059
  },
  {
    "Date": "2027-01-23",
    "Location": "Montgomery, TX",
    "Host": "Nosy Dogs Houston",
    "EventLink": "http://www.nosydogshouston.com/",
    "TrialTypes": "NW1, L1C, L1V, NW2",
    "EventCount": 4,
    "Latitude": 30.3059,
    "Longitude": -95.536
  },
  {
    "Date": "2027-01-23",
    "Location": "Valencia, CA",
    "Host": "Pink Biscuit K9s",
    "EventLink": "https://www.pinkbiscuitk9s.com/",
    "TrialTypes": "ELT-P, L2I, L3C",
    "EventCount": 3,
    "Latitude": 34.3993,
    "Longitude": -118.549
  },
  {
    "Date": "2027-01-30",
    "Location": "Las Vegas, NV",
    "Host": "imPETus Animal Training",
    "EventLink": "http://impetusanimaltraining.com/",
    "TrialTypes": "NW3, L1I, NW2",
    "EventCount": 3,
    "Latitude": 36.1788,
    "Longitude": -115.137
  },
  {
    "Date": "2027-01-30",
    "Location": "Petaluma, CA",
    "Host": "Seaside Sniffers",
    "EventLink": "https://www.seasidesniffers.com/",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 38.2615,
    "Longitude": -122.6
  },
  {
    "Date": "2027-01-30",
    "Location": "San Marcos, CA",
    "Host": "Rewarding Rover LLC, Uberdog, & Claire Brocato",
    "EventLink": "https://www.rewardingrover.com/",
    "TrialTypes": "ELT-S, NW3",
    "EventCount": 2,
    "Latitude": 33.1673,
    "Longitude": -117.1537
  },
  {
    "Date": "2027-01-30",
    "Location": "Seguin, TX",
    "Host": "Sniff Happens",
    "EventLink": "https://www.sniffhappenstx.com/Jan-NACSW-Trial",
    "TrialTypes": "L2C, ELT-S, NW3",
    "EventCount": 3,
    "Latitude": 29.5811,
    "Longitude": -97.9728
  },
  {
    "Date": "2027-01-31",
    "Location": "Durham, NC",
    "Host": "Whole Dog Institute",
    "EventLink": "https://wholedoginstitute.com/",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 35.9618,
    "Longitude": -78.8541
  },
  {
    "Date": "2027-02-13",
    "Location": "Modesto, CA",
    "Host": "Two Nosey Girls",
    "EventLink": "https://twonoseygirls.com/",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 37.6667,
    "Longitude": -121.0111
  },
  {
    "Date": "2027-02-21",
    "Location": "Benson, AZ",
    "Host": "Patience Unlimited Dog Training",
    "EventLink": "http://www.patienceunlimited.com/",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 31.9562,
    "Longitude": -110.2611
  },
  {
    "Date": "2027-02-22",
    "Location": "Paso Robles, CA",
    "Host": "Gentle Touch Pet Training",
    "EventLink": "http://gentlepets.com/",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 35.6414,
    "Longitude": -120.6505
  },
  {
    "Date": "2027-02-26",
    "Location": "Westlake Village, CA",
    "Host": "JavaK9s, LLC",
    "EventLink": "http://www.javak9s.com/",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 34.1663,
    "Longitude": -118.8412
  },
  {
    "Date": "2027-03-06",
    "Location": "Keystone Heights, FL",
    "Host": "River Poodles Training, LLC",
    "EventLink": "https://riverpoodlestraining.com/",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 29.7518,
    "Longitude": -82.0099
  },
  {
    "Date": "2027-03-08",
    "Location": "Glendora, CA",
    "Host": "Agile Paws Dog Sports",
    "EventLink": "https://agilepawsdogsports.com/",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 34.1772,
    "Longitude": -117.865
  },
  {
    "Date": "2027-03-13",
    "Location": "Rome, GA",
    "Host": "Southeast Scent Work Alliance, LLC (SSWA)",
    "EventLink": "https://southeastscent.com/events",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 34.2219,
    "Longitude": -85.1392
  },
  {
    "Date": "2027-03-15",
    "Location": "Riverside, CA",
    "Host": "Linda Buchanan",
    "EventLink": "https://www.k9slovetosearch.com/",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 33.9611,
    "Longitude": -117.328
  },
  {
    "Date": "2027-03-20",
    "Location": "Redwood City, CA",
    "Host": "B. L. McMutts, LLC",
    "EventLink": "https://blmcmutts.com/",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 37.4945,
    "Longitude": -122.2791
  },
  {
    "Date": "2027-03-20",
    "Location": "Selma, TX",
    "Host": "Sniff Happens",
    "EventLink": "https://www.sniffhappenstx.com/",
    "TrialTypes": "L1E, ELT-S, ELT",
    "EventCount": 3,
    "Latitude": 29.611,
    "Longitude": -98.3392
  },
  {
    "Date": "2027-03-26",
    "Location": "Albuquerque, NM",
    "Host": "New Mexico Canine Scent Work, LLC",
    "EventLink": "https://www.nmcsw.com/",
    "TrialTypes": "ELT, NW3, L2I, NW1",
    "EventCount": 4,
    "Latitude": 35.1055,
    "Longitude": -106.6916
  },
  {
    "Date": "2027-04-03",
    "Location": "Keystone Heights, FL",
    "Host": "River Poodles Training, LLC",
    "EventLink": "https://riverpoodlestraining.com/",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 29.8346,
    "Longitude": -82.0401
  },
  {
    "Date": "2027-04-05",
    "Location": "Chester, NY",
    "Host": "For the Love of Dogs NY LLC",
    "EventLink": "https://www.fortheloveofdogsny.com/trials-events",
    "TrialTypes": "NW2, ELT",
    "EventCount": 2,
    "Latitude": 41.3975,
    "Longitude": -74.2891
  },
  {
    "Date": "2027-04-07",
    "Location": "Olympia, WA",
    "Host": "Let's Talk Dogs, LLC & About Face K9 Academy",
    "EventLink": "https://www.aboutfacek9academy.com",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 47.0214,
    "Longitude": -122.8933
  },
  {
    "Date": "2027-04-16",
    "Location": "Upland, CA",
    "Host": "Agile Paws Dog Sports",
    "EventLink": "https://agilepawsdogsports.com/",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 34.0954,
    "Longitude": -117.6768
  },
  {
    "Date": "2027-04-17",
    "Location": "Glenwood, NY",
    "Host": "Do Over Dog Training",
    "EventLink": "https://www.dooverdogtraining.com/trials",
    "TrialTypes": "NW1, L2E, L1C, L1I",
    "EventCount": 4,
    "Latitude": 42.6001,
    "Longitude": -78.6387
  },
  {
    "Date": "2027-04-24",
    "Location": "Ellicottville, NY",
    "Host": "Do Over Dog Training",
    "EventLink": "https://www.dooverdogtraining.com/trials",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 42.2711,
    "Longitude": -78.6799
  },
  {
    "Date": "2027-05-22",
    "Location": "Amherst, NY",
    "Host": "Do Over Dog Training",
    "EventLink": "https://www.dooverdogtraining.com/trials",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 43.0147,
    "Longitude": -78.8473
  },
  {
    "Date": "2027-06-12",
    "Location": "Portland, OR",
    "Host": "Trust Your Dog K9 Events",
    "EventLink": "https://trustyourdogk9events.com/",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 45.537,
    "Longitude": -122.6948
  }
]
;
