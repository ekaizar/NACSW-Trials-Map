const DATA_UPDATED = "September 21, 2026";
const TRIALS_DATA = 
[
  {
    "Date": "2024-09-21",
    "Location": "North Bend, WA",
    "Host": "Northwest K9 Sniffers",
    "TrialTypes": "NW3, L1C, L1I",
    "EventCount": 3,
    "Latitude": 47.4729,
    "Longitude": -121.7961
  },
  {
    "Date": "2024-09-21",
    "Location": "Tuftonboro, NH",
    "Host": "The Big Sniff, LLC",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 43.7201,
    "Longitude": -71.3478
  },
  {
    "Date": "2024-09-21",
    "Location": "White Salmon, WA",
    "Host": "Sharon Smith",
    "TrialTypes": "NW1, ELT-S, NW3",
    "EventCount": 3,
    "Latitude": 45.7003,
    "Longitude": -121.5065
  },
  {
    "Date": "2024-09-27",
    "Location": "Estes Park, CO",
    "Host": "Mountain Dogs, LLC",
    "TrialTypes": "SMT, ELT-S",
    "EventCount": 2,
    "Latitude": 40.3923,
    "Longitude": -105.5309
  },
  {
    "Date": "2024-09-27",
    "Location": "Richmond, VA",
    "Host": "Paws Plus Training, LLC",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 37.5261,
    "Longitude": -77.3985
  },
  {
    "Date": "2024-09-27",
    "Location": "Turlock, CA",
    "Host": "Two Nosey Girls",
    "TrialTypes": "L2I, L3I, ELT-S",
    "EventCount": 3,
    "Latitude": 37.4934,
    "Longitude": -120.8799
  },
  {
    "Date": "2024-09-28",
    "Location": "Grandview, TX",
    "Host": "North Texas Nosework Club",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 32.2234,
    "Longitude": -97.2187
  },
  {
    "Date": "2024-09-28",
    "Location": "Pittsburgh, PA",
    "Host": "Steel City Nosework, LLC",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 40.4046,
    "Longitude": -80.0389
  },
  {
    "Date": "2024-09-28",
    "Location": "Reedsport, OR",
    "Host": "Wells Creek Dog Training",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 43.6571,
    "Longitude": -124.0944
  },
  {
    "Date": "2024-09-28",
    "Location": "Waynesboro, PA",
    "Host": "Nose-It-All, LLC",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 39.7465,
    "Longitude": -77.5715
  },
  {
    "Date": "2024-09-29",
    "Location": "Glenwood, NY",
    "Host": "Do Over Dog Training",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 42.5783,
    "Longitude": -78.6433
  },
  {
    "Date": "2024-10-03",
    "Location": "New Rochelle, NY",
    "Host": "For the Love of Dogs NY LLC",
    "TrialTypes": "NW3, NW2, NW1",
    "EventCount": 3,
    "Latitude": 40.926,
    "Longitude": -73.8024
  },
  {
    "Date": "2024-10-04",
    "Location": "Golden, CO",
    "Host": "K9 Nosin’ Around, Inc.",
    "TrialTypes": "NW2, NW1",
    "EventCount": 2,
    "Latitude": 39.7626,
    "Longitude": -105.2534
  },
  {
    "Date": "2024-10-04",
    "Location": "Mechanicsburg, PA",
    "Host": "Nose-It-All, LLC",
    "TrialTypes": "SMT, L3V, L2V",
    "EventCount": 3,
    "Latitude": 40.1946,
    "Longitude": -76.9762
  },
  {
    "Date": "2024-10-05",
    "Location": "Centralia, WA",
    "Host": "About Face K9 Academy & Let's Talk Dogs, LLC",
    "TrialTypes": "ELT-S, NW1",
    "EventCount": 2,
    "Latitude": 46.7006,
    "Longitude": -122.925
  },
  {
    "Date": "2024-10-05",
    "Location": "Copake, NY",
    "Host": "Saints2Source, LLC",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 42.0914,
    "Longitude": -73.5109
  },
  {
    "Date": "2024-10-05",
    "Location": "Crosslake, MN",
    "Host": "Nose 2 Tail Dog Training, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 46.6319,
    "Longitude": -94.1385
  },
  {
    "Date": "2024-10-05",
    "Location": "Nashua, NH",
    "Host": "The Big Sniff, LLC",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 42.7742,
    "Longitude": -71.4847
  },
  {
    "Date": "2024-10-05",
    "Location": "New Paltz, NY",
    "Host": "Pat Tetrault and Dominique Manpel",
    "TrialTypes": "NW2, ELT-S, L2I",
    "EventCount": 3,
    "Latitude": 41.7221,
    "Longitude": -74.1167
  },
  {
    "Date": "2024-10-05",
    "Location": "Sandwich, IL",
    "Host": "For Your K9",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 41.692,
    "Longitude": -88.5938
  },
  {
    "Date": "2024-10-05",
    "Location": "Troy, VA",
    "Host": "Your Dog Knows, LLC",
    "TrialTypes": "L1I, L2I, L1C, L2C",
    "EventCount": 4,
    "Latitude": 37.9873,
    "Longitude": -78.2524
  },
  {
    "Date": "2024-10-05",
    "Location": "West Bend, WI",
    "Host": "Think Pawsitive Dog Training",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 43.4177,
    "Longitude": -88.1966
  },
  {
    "Date": "2024-10-07",
    "Location": "Monterey, CA",
    "Host": "CalCoastal Dog Owners Group",
    "TrialTypes": "L1I, NW2, L2I",
    "EventCount": 3,
    "Latitude": 36.1998,
    "Longitude": -121.4331
  },
  {
    "Date": "2024-10-11",
    "Location": "South Haven, MN",
    "Host": "St Paul Dog Training Club",
    "TrialTypes": "SMT, ELT",
    "EventCount": 2,
    "Latitude": 45.2803,
    "Longitude": -94.2457
  },
  {
    "Date": "2024-10-11",
    "Location": "Walbridge, OH",
    "Host": "Robin Ford Dog Training",
    "TrialTypes": "NW1, NW2, NW3",
    "EventCount": 3,
    "Latitude": 41.5653,
    "Longitude": -83.5376
  },
  {
    "Date": "2024-10-12",
    "Location": "Homer Glen, IL",
    "Host": "Paws for Scent",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 41.6385,
    "Longitude": -87.9589
  },
  {
    "Date": "2024-10-12",
    "Location": "Lafayette Hill, PA",
    "Host": "Sniff Sniff Hooray",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 40.0635,
    "Longitude": -75.2744
  },
  {
    "Date": "2024-10-12",
    "Location": "Sedona, AZ",
    "Host": "Release Canine LLC",
    "TrialTypes": "ELT, ELT-S, NW2, NW1",
    "EventCount": 4,
    "Latitude": 34.8797,
    "Longitude": -111.765
  },
  {
    "Date": "2024-10-18",
    "Location": "Calhan, CO",
    "Host": "Mountain Dogs, LLC",
    "TrialTypes": "ELT, NW3, ELT-S",
    "EventCount": 3,
    "Latitude": 39.0468,
    "Longitude": -104.3189
  },
  {
    "Date": "2024-10-18",
    "Location": "Loganville, GA",
    "Host": "Canine Country Academy, LLC",
    "TrialTypes": "NW2, NW1",
    "EventCount": 2,
    "Latitude": 33.8264,
    "Longitude": -83.9408
  },
  {
    "Date": "2024-10-18",
    "Location": "Newfoundland, PA",
    "Host": "Your Dog's Place, LLC",
    "TrialTypes": "L1V, ELT-S, L2C, L1E",
    "EventCount": 4,
    "Latitude": 41.2791,
    "Longitude": -75.3598
  },
  {
    "Date": "2024-10-18",
    "Location": "Rossville, GA",
    "Host": "Camelot Shepherds, Inc",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 34.9818,
    "Longitude": -85.2724
  },
  {
    "Date": "2024-10-19",
    "Location": "Ferndale, WA",
    "Host": "Nose Work Magic",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 48.8911,
    "Longitude": -122.6189
  },
  {
    "Date": "2024-10-19",
    "Location": "Griffith, IN",
    "Host": "Outside the Box, LLC",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 41.5198,
    "Longitude": -87.4649
  },
  {
    "Date": "2024-10-19",
    "Location": "Kilmarnock, VA",
    "Host": "Red Huskies",
    "TrialTypes": "NW3, L1E, NW2",
    "EventCount": 3,
    "Latitude": 37.6746,
    "Longitude": -76.4272
  },
  {
    "Date": "2024-10-19",
    "Location": "Kingston, IL",
    "Host": "Common Scents K9",
    "TrialTypes": "NW1, L2C, NW2",
    "EventCount": 3,
    "Latitude": 42.133,
    "Longitude": -88.7735
  },
  {
    "Date": "2024-10-19",
    "Location": "Lakeville, MN",
    "Host": "St Paul Dog Training Club",
    "TrialTypes": "ELT-P, NW1, L1E",
    "EventCount": 3,
    "Latitude": 44.6101,
    "Longitude": -93.2504
  },
  {
    "Date": "2024-10-19",
    "Location": "Round Rock, TX",
    "Host": "Heng Ten K9 Training",
    "TrialTypes": "NW3, ELT-S, NW1",
    "EventCount": 3,
    "Latitude": 30.5354,
    "Longitude": -97.6377
  },
  {
    "Date": "2024-10-19",
    "Location": "Yamhill, OR",
    "Host": "Nose Work Detectives, LLC",
    "TrialTypes": "L1C, L1V, L2C, L2V",
    "EventCount": 4,
    "Latitude": 45.1856,
    "Longitude": -123.2506
  },
  {
    "Date": "2024-10-22",
    "Location": "Astoria, OR",
    "Host": "Nosework Detectives, LLC",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 46.1969,
    "Longitude": -123.869
  },
  {
    "Date": "2024-10-25",
    "Location": "Fishkill, NY",
    "Host": "Pat Tetrault and Dominique Manpel",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 41.5153,
    "Longitude": -73.8789
  },
  {
    "Date": "2024-10-25",
    "Location": "Palmyra, VA",
    "Host": "Paws Plus Training, LLC",
    "TrialTypes": "ELT, NW3, ELT-S, NW1",
    "EventCount": 4,
    "Latitude": 37.8827,
    "Longitude": -78.2554
  },
  {
    "Date": "2024-10-26",
    "Location": "Columbia City, IN",
    "Host": "2 Nose You Is 2 Loves You",
    "TrialTypes": "ELT, NW1",
    "EventCount": 2,
    "Latitude": 41.1419,
    "Longitude": -85.5125
  },
  {
    "Date": "2024-10-26",
    "Location": "Columbus, MT",
    "Host": "Nikki Markle of Canine Connection",
    "TrialTypes": "ELT-S, NW2, ELT",
    "EventCount": 3,
    "Latitude": 45.6306,
    "Longitude": -109.2629
  },
  {
    "Date": "2024-10-26",
    "Location": "Grand Junction, CO",
    "Host": "Mountain Dogs, LLC",
    "TrialTypes": "NW2, NW1",
    "EventCount": 2,
    "Latitude": 39.0259,
    "Longitude": -108.5975
  },
  {
    "Date": "2024-10-26",
    "Location": "Hammond, LA",
    "Host": "Dog Gone Right, LLC",
    "TrialTypes": "ELT-S, NW1, NW3",
    "EventCount": 3,
    "Latitude": 30.5507,
    "Longitude": -90.4467
  },
  {
    "Date": "2024-10-26",
    "Location": "Medford, NJ",
    "Host": "K9 InScentives",
    "TrialTypes": "NW3, NW1",
    "EventCount": 2,
    "Latitude": 39.8557,
    "Longitude": -74.8539
  },
  {
    "Date": "2024-10-26",
    "Location": "Poland Springs, ME",
    "Host": "Virginia Howe",
    "TrialTypes": "NW1, NW3",
    "EventCount": 2,
    "Latitude": 44.013,
    "Longitude": -70.3154
  },
  {
    "Date": "2024-10-26",
    "Location": "Suring, WI",
    "Host": "Clever Sniffers, LLC",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 44.981,
    "Longitude": -88.3968
  },
  {
    "Date": "2024-10-26",
    "Location": "Welches, OR",
    "Host": "Trust Your Dog K9 Events",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 45.3501,
    "Longitude": -121.9786
  },
  {
    "Date": "2024-10-26",
    "Location": "West Friendship, MD",
    "Host": "Red Huskies",
    "TrialTypes": "NW3, L2C, NW2",
    "EventCount": 3,
    "Latitude": 39.2951,
    "Longitude": -76.9712
  },
  {
    "Date": "2024-10-26",
    "Location": "Winterset, IA",
    "Host": "KBP Dog Training",
    "TrialTypes": "NW2, NW1",
    "EventCount": 2,
    "Latitude": 41.3606,
    "Longitude": -93.9816
  },
  {
    "Date": "2024-10-27",
    "Location": "San Martin, CA",
    "Host": "B. L. McMutts",
    "TrialTypes": "L1V, L2V",
    "EventCount": 2,
    "Latitude": 37.0908,
    "Longitude": -121.568
  },
  {
    "Date": "2024-11-01",
    "Location": "Denton, MD",
    "Host": "Fair Play Labradors",
    "TrialTypes": "NW3, ELT-S, L1C, NW2, L1E",
    "EventCount": 5,
    "Latitude": 38.8941,
    "Longitude": -75.7914
  },
  {
    "Date": "2024-11-01",
    "Location": "Guerneville, CA",
    "Host": "Jen Huot",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 38.4725,
    "Longitude": -122.9489
  },
  {
    "Date": "2024-11-01",
    "Location": "Red Feather Lakes, CO",
    "Host": "Beyond Elevation K9 Training",
    "TrialTypes": "NW1, NW2, NW3",
    "EventCount": 3,
    "Latitude": 40.8518,
    "Longitude": -105.5488
  },
  {
    "Date": "2024-11-02",
    "Location": "Callaway, VA",
    "Host": "Canny K9 Companions LLC",
    "TrialTypes": "ELT-S, NW1, NW2",
    "EventCount": 3,
    "Latitude": 37.0551,
    "Longitude": -80.0058
  },
  {
    "Date": "2024-11-02",
    "Location": "Greenview, IL",
    "Host": "Capitol Canine Dog Sports",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 40.1286,
    "Longitude": -89.7464
  },
  {
    "Date": "2024-11-02",
    "Location": "Kennebunkport, ME",
    "Host": "Elizabeth Dutton",
    "TrialTypes": "ELT-P, NW3",
    "EventCount": 2,
    "Latitude": 43.3719,
    "Longitude": -70.5046
  },
  {
    "Date": "2024-11-02",
    "Location": "Mays Landing, NJ",
    "Host": "Rotts-n-Notts Nosework, LLC",
    "TrialTypes": "NW3, L1E, NW1",
    "EventCount": 3,
    "Latitude": 39.4261,
    "Longitude": -74.7735
  },
  {
    "Date": "2024-11-02",
    "Location": "Mill Spring, NC",
    "Host": "Foothills Canine Academy, LLC",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 35.3058,
    "Longitude": -82.172
  },
  {
    "Date": "2024-11-02",
    "Location": "Shawnee, OK",
    "Host": "The Doggie Spot, LLC",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 35.3023,
    "Longitude": -96.8984
  },
  {
    "Date": "2024-11-02",
    "Location": "Valencia, CA",
    "Host": "Pink Biscuit K9s",
    "TrialTypes": "L1I, L2I, L1C, L2C",
    "EventCount": 4,
    "Latitude": 34.3888,
    "Longitude": -118.5361
  },
  {
    "Date": "2024-11-02",
    "Location": "Wappingers Falls, NY",
    "Host": "Top Notch Dogs, LLC",
    "TrialTypes": "ELT-P, ELT-S, L1C",
    "EventCount": 3,
    "Latitude": 41.6106,
    "Longitude": -73.9132
  },
  {
    "Date": "2024-11-03",
    "Location": "McMinnville, OR",
    "Host": "Doglandia LLC and Carol Forsberg",
    "TrialTypes": "NW2, ELT",
    "EventCount": 2,
    "Latitude": 45.2017,
    "Longitude": -123.1908
  },
  {
    "Date": "2024-11-04",
    "Location": "Duluth, GA",
    "Host": "Right Choice Dog Training, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 33.9822,
    "Longitude": -84.1914
  },
  {
    "Date": "2024-11-08",
    "Location": "Montrose, CO",
    "Host": "Mountain Dogs, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 38.4624,
    "Longitude": -107.8471
  },
  {
    "Date": "2024-11-09",
    "Location": "Canoga Park, CA",
    "Host": "Pink Biscuit K9s",
    "TrialTypes": "NW1, L3I, L3C",
    "EventCount": 3,
    "Latitude": 34.2196,
    "Longitude": -118.5838
  },
  {
    "Date": "2024-11-09",
    "Location": "Eldred, NY",
    "Host": "Pocono Nose Work",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 41.5512,
    "Longitude": -74.8728
  },
  {
    "Date": "2024-11-09",
    "Location": "Escondido, CA",
    "Host": "Anita Cheesman and Jessica Koester",
    "TrialTypes": "NW1, L1C",
    "EventCount": 2,
    "Latitude": 33.1363,
    "Longitude": -117.0523
  },
  {
    "Date": "2024-11-09",
    "Location": "Huntsville, AL",
    "Host": "Sniffers Anonymous",
    "TrialTypes": "NW3, L1I, L1C",
    "EventCount": 3,
    "Latitude": 34.7428,
    "Longitude": -86.6113
  },
  {
    "Date": "2024-11-09",
    "Location": "Milton, NH",
    "Host": "Lucky Dog Events",
    "TrialTypes": "NW3, L2I, NW2",
    "EventCount": 3,
    "Latitude": 43.4097,
    "Longitude": -70.9936
  },
  {
    "Date": "2024-11-09",
    "Location": "Moline, IL",
    "Host": "Fur Better Fur Worse, LLC",
    "TrialTypes": "ELT-S",
    "EventCount": 1,
    "Latitude": 41.5455,
    "Longitude": -90.5218
  },
  {
    "Date": "2024-11-09",
    "Location": "New Rochelle, NY",
    "Host": "For the Love of Dogs NY, LLC",
    "TrialTypes": "L3I, L3C, NW1, NW2",
    "EventCount": 4,
    "Latitude": 40.919,
    "Longitude": -73.7745
  },
  {
    "Date": "2024-11-09",
    "Location": "Schaumburg, IL",
    "Host": "Northwest Obedience Club Inc.",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 42.043,
    "Longitude": -88.0461
  },
  {
    "Date": "2024-11-10",
    "Location": "Odessa, FL",
    "Host": "Hoppin’ in the Hills",
    "TrialTypes": "NW2, NW1",
    "EventCount": 2,
    "Latitude": 28.172,
    "Longitude": -82.5709
  },
  {
    "Date": "2024-11-11",
    "Location": "Escondido, CA",
    "Host": "Anita Cheesman and Jessica Koester",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 33.1578,
    "Longitude": -117.0504
  },
  {
    "Date": "2024-11-11",
    "Location": "Paso Robles, CA",
    "Host": "Gentle Touch Pet Training",
    "TrialTypes": "L1C, L2I, ELT-S, NW2",
    "EventCount": 4,
    "Latitude": 35.5997,
    "Longitude": -120.6622
  },
  {
    "Date": "2024-11-15",
    "Location": "Harrington, DE",
    "Host": "Shamrock Pot of Gold K9 Scenter",
    "TrialTypes": "NW3, ELT, ELT-P, NW2, NW1",
    "EventCount": 5,
    "Latitude": 38.939,
    "Longitude": -75.574
  },
  {
    "Date": "2024-11-15",
    "Location": "Rancho Cucamonga, CA",
    "Host": "Agile Paws Dog Sports",
    "TrialTypes": "L2C, NW2, NW1, L1C",
    "EventCount": 4,
    "Latitude": 34.0822,
    "Longitude": -117.5334
  },
  {
    "Date": "2024-11-16",
    "Location": "Auburn, WA",
    "Host": "Northwest K9 Sniffers",
    "TrialTypes": "ELT-S, NW1, L2C, L2I",
    "EventCount": 4,
    "Latitude": 47.3533,
    "Longitude": -122.2193
  },
  {
    "Date": "2024-11-16",
    "Location": "Foxborough, MA",
    "Host": "MasterPeace Dog Training",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 42.0293,
    "Longitude": -71.2418
  },
  {
    "Date": "2024-11-16",
    "Location": "Marble Falls, TX",
    "Host": "Scent Work Across Texas",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 30.6105,
    "Longitude": -98.2583
  },
  {
    "Date": "2024-11-16",
    "Location": "Nevada City, CA",
    "Host": "Sierra Sniffing Canines",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 39.2466,
    "Longitude": -121.0218
  },
  {
    "Date": "2024-11-16",
    "Location": "Tucson, AZ",
    "Host": "Patience Unlimited Professional Dog Training",
    "TrialTypes": "NW1, L1C, L1E, L1I",
    "EventCount": 4,
    "Latitude": 32.2715,
    "Longitude": -110.9827
  },
  {
    "Date": "2024-11-16",
    "Location": "Yanceyville, NC",
    "Host": "Whole Dog Institute, LLC",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 36.3732,
    "Longitude": -79.3319
  },
  {
    "Date": "2024-11-23",
    "Location": "Coburg, OR",
    "Host": "Kiddie Christie",
    "TrialTypes": "L1E, NW1, NW3",
    "EventCount": 3,
    "Latitude": 44.1269,
    "Longitude": -123.1063
  },
  {
    "Date": "2024-11-23",
    "Location": "Delta, CO",
    "Host": "Mountain Dogs, LLC",
    "TrialTypes": "NW3, ELT-P",
    "EventCount": 2,
    "Latitude": 38.8846,
    "Longitude": -107.903
  },
  {
    "Date": "2024-11-23",
    "Location": "Fork Union, VA",
    "Host": "Your Dog Knows LLC",
    "TrialTypes": "NW3, L3I, L1V",
    "EventCount": 3,
    "Latitude": 37.7345,
    "Longitude": -78.3075
  },
  {
    "Date": "2024-11-23",
    "Location": "Kintnersville, PA",
    "Host": "Paws n' Sniff",
    "TrialTypes": "NW1, L1E, ELT-S",
    "EventCount": 3,
    "Latitude": 40.5824,
    "Longitude": -75.2142
  },
  {
    "Date": "2024-11-23",
    "Location": "Saltsburg, PA",
    "Host": "Steel City Nosework, LLC",
    "TrialTypes": "NW3, ELT-S, NW1",
    "EventCount": 3,
    "Latitude": 40.4913,
    "Longitude": -79.4642
  },
  {
    "Date": "2024-11-23",
    "Location": "Smyrna, TN",
    "Host": "Dogs Have Amazing Noses LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 36.0216,
    "Longitude": -86.5318
  },
  {
    "Date": "2024-11-29",
    "Location": "Capo Beach/Dana Point, CA",
    "Host": "JavaK9s",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 33.4781,
    "Longitude": -117.6842
  },
  {
    "Date": "2024-11-29",
    "Location": "Foxborough, MA",
    "Host": "Tracey Costa",
    "TrialTypes": "ELT, L1C, NW2",
    "EventCount": 3,
    "Latitude": 42.0481,
    "Longitude": -71.2505
  },
  {
    "Date": "2024-11-30",
    "Location": "Cottage Grove, MN",
    "Host": "Gretchen Hofheins-Wackerfuss",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 44.7962,
    "Longitude": -92.9477
  },
  {
    "Date": "2024-11-30",
    "Location": "Cumming, GA",
    "Host": "Georgia Nosework",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 34.2181,
    "Longitude": -84.1458
  },
  {
    "Date": "2024-11-30",
    "Location": "Green Bay, WI",
    "Host": "NEWK9 Scent Work LLC",
    "TrialTypes": "ELT-S, ELT",
    "EventCount": 2,
    "Latitude": 44.5484,
    "Longitude": -87.9639
  },
  {
    "Date": "2024-11-30",
    "Location": "Lebanon, NJ",
    "Host": "Sirius K-9 Solutions",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 40.608,
    "Longitude": -74.8334
  },
  {
    "Date": "2024-11-30",
    "Location": "Los Osos, CA",
    "Host": "Central Coast Nosework Club, Inc.",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 35.3167,
    "Longitude": -120.8523
  },
  {
    "Date": "2024-11-30",
    "Location": "Plant City, FL",
    "Host": "Hoppin’ in the Hills",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 27.9763,
    "Longitude": -82.0875
  },
  {
    "Date": "2024-11-30",
    "Location": "Worcester, PA",
    "Host": "Sniff Sniff Hooray",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 40.2446,
    "Longitude": -75.3795
  },
  {
    "Date": "2024-12-06",
    "Location": "Salem, WI",
    "Host": "Loving Paws Dog Training LLC",
    "TrialTypes": "ELT-P, ELT",
    "EventCount": 2,
    "Latitude": 42.5683,
    "Longitude": -88.0743
  },
  {
    "Date": "2024-12-06",
    "Location": "Ypsilanti, MI",
    "Host": "Every Dog Nosework",
    "TrialTypes": "ELT-P, ELT-S, L2C",
    "EventCount": 3,
    "Latitude": 42.2353,
    "Longitude": -83.5835
  },
  {
    "Date": "2024-12-07",
    "Location": "Batavia, NY",
    "Host": "Do Over Dog Training",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 43.0021,
    "Longitude": -78.2041
  },
  {
    "Date": "2024-12-07",
    "Location": "Bowie, MD",
    "Host": "Red Huskies",
    "TrialTypes": "ELT-P, ELT-S",
    "EventCount": 2,
    "Latitude": 38.9374,
    "Longitude": -76.7548
  },
  {
    "Date": "2024-12-07",
    "Location": "Centralia, WA",
    "Host": "Let's Talk Dogs, LLC and About Face K9 Academy",
    "TrialTypes": "ELT-P, NW2",
    "EventCount": 2,
    "Latitude": 46.7674,
    "Longitude": -122.9402
  },
  {
    "Date": "2024-12-07",
    "Location": "Chester Springs, PA",
    "Host": "Patricia Grassey",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 40.0955,
    "Longitude": -75.5785
  },
  {
    "Date": "2024-12-07",
    "Location": "DeLeon Springs, FL",
    "Host": "River Poodles Training, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 29.1082,
    "Longitude": -81.3868
  },
  {
    "Date": "2024-12-07",
    "Location": "Fillmore, CA",
    "Host": "Pink Biscuit K9s",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 34.4452,
    "Longitude": -118.9371
  },
  {
    "Date": "2024-12-07",
    "Location": "Newfoundland, PA",
    "Host": "Your Dog's Place, LLC",
    "TrialTypes": "L3V, NW2, ELT",
    "EventCount": 3,
    "Latitude": 41.3358,
    "Longitude": -75.317
  },
  {
    "Date": "2024-12-07",
    "Location": "Owenton, KY",
    "Host": "Clermont County Dog Training Club",
    "TrialTypes": "ELT, NW1, NW2",
    "EventCount": 3,
    "Latitude": 38.5694,
    "Longitude": -84.7996
  },
  {
    "Date": "2024-12-09",
    "Location": "Stockton, CA",
    "Host": "Two Nosey Girls",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 37.9372,
    "Longitude": -121.287
  },
  {
    "Date": "2024-12-13",
    "Location": "Pittstown, NJ",
    "Host": "Shamrock Pot of Gold K9 Scenter",
    "TrialTypes": "NW3, ELT-P, ELT-S, NW1",
    "EventCount": 4,
    "Latitude": 40.6065,
    "Longitude": -74.9422
  },
  {
    "Date": "2024-12-14",
    "Location": "Ontario, CA",
    "Host": "Agile Paws Dog Sports",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 34.0709,
    "Longitude": -117.6489
  },
  {
    "Date": "2024-12-21",
    "Location": "Cedar Park, TX",
    "Host": "Scent Work Across Texas",
    "TrialTypes": "L1V, L2I, NW1, L2E",
    "EventCount": 4,
    "Latitude": 30.484,
    "Longitude": -97.8428
  },
  {
    "Date": "2024-12-21",
    "Location": "Jefferson, GA",
    "Host": "Right Choice Dog Training, LLC",
    "TrialTypes": "NW2, NW1",
    "EventCount": 2,
    "Latitude": 33.0227,
    "Longitude": -82.4476
  },
  {
    "Date": "2024-12-21",
    "Location": "Marriottsville, MD",
    "Host": "Red Huskies",
    "TrialTypes": "ELT-P, ELT-S, NW1",
    "EventCount": 3,
    "Latitude": 39.3538,
    "Longitude": -76.9087
  },
  {
    "Date": "2024-12-27",
    "Location": "Crownsville, MD",
    "Host": "Red Huskies",
    "TrialTypes": "ELT-P, ELT, ELT-S",
    "EventCount": 3,
    "Latitude": 39.0619,
    "Longitude": -76.5653
  },
  {
    "Date": "2024-12-28",
    "Location": "Bellingham, WA",
    "Host": "Nose Work Magic",
    "TrialTypes": "L1V, ELT-S, ELT",
    "EventCount": 3,
    "Latitude": 48.7429,
    "Longitude": -122.4412
  },
  {
    "Date": "2024-12-28",
    "Location": "Cumming, GA",
    "Host": "Georgia Nosework",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 34.2325,
    "Longitude": -84.1121
  },
  {
    "Date": "2024-12-28",
    "Location": "Salem, OR",
    "Host": "Helix Fairweather & Doglandia, LLC",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 44.9007,
    "Longitude": -122.9957
  },
  {
    "Date": "2024-12-28",
    "Location": "Williamsburg, VA",
    "Host": "Blockade Runners Flyball",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 37.2368,
    "Longitude": -76.744
  },
  {
    "Date": "2024-12-29",
    "Location": "Waukesha, WI",
    "Host": "Think Pawsitive Dog Training",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 43.0461,
    "Longitude": -88.3078
  },
  {
    "Date": "2024-12-31",
    "Location": "Strasburg, IL",
    "Host": "Kudos for Canines, LLC",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 39.3221,
    "Longitude": -88.6441
  },
  {
    "Date": "2025-01-03",
    "Location": "Brockport, NY",
    "Host": "Savvy Dog Sports",
    "TrialTypes": "ELT-S, ELT",
    "EventCount": 2,
    "Latitude": 43.2342,
    "Longitude": -77.946
  },
  {
    "Date": "2025-01-03",
    "Location": "Emmitsburg, MD",
    "Host": "Red Huskies",
    "TrialTypes": "ELT, ELT-P, ELT-S",
    "EventCount": 3,
    "Latitude": 39.6624,
    "Longitude": -77.2832
  },
  {
    "Date": "2025-01-04",
    "Location": "Bonsall, CA",
    "Host": "Linda Buchanan",
    "TrialTypes": "NW2, ELT",
    "EventCount": 2,
    "Latitude": 33.3102,
    "Longitude": -117.2178
  },
  {
    "Date": "2025-01-09",
    "Location": "Centreville, MD",
    "Host": "Fair Play Labradors",
    "TrialTypes": "ELT-S, NW3, ELT, ELT-P",
    "EventCount": 4,
    "Latitude": 39.09,
    "Longitude": -76.0355
  },
  {
    "Date": "2025-01-10",
    "Location": "Hartfield, VA",
    "Host": "Paws Plus Training, LLC",
    "TrialTypes": "NW3, NW1, NW2, ELT",
    "EventCount": 4,
    "Latitude": 37.552,
    "Longitude": -76.4255
  },
  {
    "Date": "2025-01-11",
    "Location": "Greensboro, NC",
    "Host": "Dog Fun Forever, LLC",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 36.0303,
    "Longitude": -79.8308
  },
  {
    "Date": "2025-01-11",
    "Location": "Lithia, FL",
    "Host": "Hoppin’ in the Hills",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 27.9057,
    "Longitude": -82.2447
  },
  {
    "Date": "2025-01-13",
    "Location": "Oakdale, CA",
    "Host": "Two Nosey Girls",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 37.7265,
    "Longitude": -120.8074
  },
  {
    "Date": "2025-01-18",
    "Location": "Clanton, AL",
    "Host": "Daphne Melillo",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 32.8508,
    "Longitude": -86.5915
  },
  {
    "Date": "2025-01-18",
    "Location": "Elmira, OR",
    "Host": "Wells Creek Dog Training",
    "TrialTypes": "ELT, L1I, NW2",
    "EventCount": 3,
    "Latitude": 44.0923,
    "Longitude": -123.3218
  },
  {
    "Date": "2025-01-18",
    "Location": "Flemington, NJ",
    "Host": "Shamrock Pot of Gold K9 Scenter",
    "TrialTypes": "ELT-S, L2I, L2C, ELT",
    "EventCount": 4,
    "Latitude": 40.5354,
    "Longitude": -74.8155
  },
  {
    "Date": "2025-01-18",
    "Location": "Marble Falls, TX",
    "Host": "Heng Ten K9 Training",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 30.5498,
    "Longitude": -98.3128
  },
  {
    "Date": "2025-01-18",
    "Location": "Melrose, FL",
    "Host": "River Poodles Training, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 29.6792,
    "Longitude": -82.0977
  },
  {
    "Date": "2025-01-18",
    "Location": "New Rochelle, NY",
    "Host": "For the Love of Dogs NY, LLC",
    "TrialTypes": "NW2, NW3, ELT-S",
    "EventCount": 3,
    "Latitude": 40.9344,
    "Longitude": -73.8253
  },
  {
    "Date": "2025-01-18",
    "Location": "Redlands, CA",
    "Host": "Agile Paws Dog Sports",
    "TrialTypes": "ELT, NW2",
    "EventCount": 2,
    "Latitude": 34.0324,
    "Longitude": -117.1644
  },
  {
    "Date": "2025-01-18",
    "Location": "Sheridan, OR",
    "Host": "Helix Fairweather & Doglandia, LLC",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 45.0634,
    "Longitude": -123.3636
  },
  {
    "Date": "2025-01-25",
    "Location": "Danielsville, GA",
    "Host": "Right Choice Dog Training, LLC",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 34.0822,
    "Longitude": -83.224
  },
  {
    "Date": "2025-01-25",
    "Location": "Tecumseh, OK",
    "Host": "The Doggie Spot, LLC",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 35.2371,
    "Longitude": -96.9347
  },
  {
    "Date": "2025-01-31",
    "Location": "Vista, CA",
    "Host": "Anita Cheesman and Jessica Koester",
    "TrialTypes": "ELT-S, NW3",
    "EventCount": 2,
    "Latitude": 33.2348,
    "Longitude": -117.2312
  },
  {
    "Date": "2025-02-01",
    "Location": "Northridge, CA",
    "Host": "Scentwork.org",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 34.2316,
    "Longitude": -118.5735
  },
  {
    "Date": "2025-02-08",
    "Location": "Murfreesboro, TN",
    "Host": "Dogs Have Amazing Noses LLC",
    "TrialTypes": "ELT, NW1",
    "EventCount": 2,
    "Latitude": 35.8848,
    "Longitude": -86.3467
  },
  {
    "Date": "2025-02-08",
    "Location": "Veneta, OR",
    "Host": "Kiddy Christie",
    "TrialTypes": "NW3, L1C, NW1",
    "EventCount": 3,
    "Latitude": 44.0858,
    "Longitude": -123.3705
  },
  {
    "Date": "2025-02-14",
    "Location": "Honey Brook, PA",
    "Host": "The Sniffing Hound",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 40.0857,
    "Longitude": -75.881
  },
  {
    "Date": "2025-02-15",
    "Location": "Bellingham, WA",
    "Host": "Nose Work Magic",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 48.7409,
    "Longitude": -122.4645
  },
  {
    "Date": "2025-02-15",
    "Location": "Flemington, NJ",
    "Host": "Shamrock Pot of Gold K9 Scenter",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 40.4875,
    "Longitude": -74.8838
  },
  {
    "Date": "2025-02-15",
    "Location": "Lakewood, NJ",
    "Host": "Rotts-n-Notts Nosework, LLC",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 40.1353,
    "Longitude": -74.1949
  },
  {
    "Date": "2025-02-15",
    "Location": "Lutherville-Timonium, MD",
    "Host": "Red Huskies",
    "TrialTypes": "L3C, L1I, NW2",
    "EventCount": 3,
    "Latitude": 39.3795,
    "Longitude": -76.664
  },
  {
    "Date": "2025-02-15",
    "Location": "Medford, NJ",
    "Host": "Sniff Sniff Hooray",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 39.8569,
    "Longitude": -74.8187
  },
  {
    "Date": "2025-02-15",
    "Location": "Modesto, CA",
    "Host": "Two Nosey Girls",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 37.5991,
    "Longitude": -120.9654
  },
  {
    "Date": "2025-02-15",
    "Location": "White Plains, NY",
    "Host": "For the Love of Dogs NY, LLC",
    "TrialTypes": "L1I, L2C, NW3",
    "EventCount": 3,
    "Latitude": 41.0782,
    "Longitude": -73.713
  },
  {
    "Date": "2025-02-15",
    "Location": "Wilson, NC",
    "Host": "Whole Dog Institute, LLC",
    "TrialTypes": "NW3, NW1",
    "EventCount": 2,
    "Latitude": 35.733,
    "Longitude": -77.9019
  },
  {
    "Date": "2025-02-16",
    "Location": "Chino, CA",
    "Host": "Agile Paws Dog Sports",
    "TrialTypes": "ELT, ELT-S",
    "EventCount": 2,
    "Latitude": 34.0578,
    "Longitude": -117.6657
  },
  {
    "Date": "2025-02-22",
    "Location": "Albuquerque, NM",
    "Host": "The Can Do K9, LLC",
    "TrialTypes": "NW1, NW3",
    "EventCount": 2,
    "Latitude": 35.0819,
    "Longitude": -106.698
  },
  {
    "Date": "2025-02-23",
    "Location": "Benson, AZ",
    "Host": "Patience Unlimited Professional Dog Training",
    "TrialTypes": "NW3, ELT-S, NW2",
    "EventCount": 3,
    "Latitude": 31.9643,
    "Longitude": -110.3072
  },
  {
    "Date": "2025-02-24",
    "Location": "Paso Robles, CA",
    "Host": "Gentle Touch Pet Training",
    "TrialTypes": "NW3, L2V, L1V",
    "EventCount": 3,
    "Latitude": 35.6044,
    "Longitude": -120.7101
  },
  {
    "Date": "2025-02-28",
    "Location": "San Rafael, CA",
    "Host": "Marin Humane",
    "TrialTypes": "L1C, ELT-S, ELT",
    "EventCount": 3,
    "Latitude": 37.9612,
    "Longitude": -122.5337
  },
  {
    "Date": "2025-03-01",
    "Location": "Augusta, NJ",
    "Host": "Shamrock Pot of Gold K9 Scenter",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 41.1486,
    "Longitude": -74.7364
  },
  {
    "Date": "2025-03-01",
    "Location": "Keystone Heights, FL",
    "Host": "River Poodles Training, LLC",
    "TrialTypes": "NW3, ELT-P",
    "EventCount": 2,
    "Latitude": 29.8074,
    "Longitude": -82.0138
  },
  {
    "Date": "2025-03-01",
    "Location": "Oakville, WA",
    "Host": "About Face K9 Academy and Let's Talk Dogs, LLC",
    "TrialTypes": "NW3, L1C, L1I",
    "EventCount": 3,
    "Latitude": 46.8598,
    "Longitude": -123.2229
  },
  {
    "Date": "2025-03-01",
    "Location": "Pomfret, MD",
    "Host": "Nose-It-All, LLC",
    "TrialTypes": "ELT-S, NW2, ELT",
    "EventCount": 3,
    "Latitude": 38.6132,
    "Longitude": -77.0409
  },
  {
    "Date": "2025-03-01",
    "Location": "Santa Paula, CA",
    "Host": "Pink Biscuit K9s",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 34.3803,
    "Longitude": -119.0452
  },
  {
    "Date": "2025-03-01",
    "Location": "Youngwood, PA",
    "Host": "Steel City Nosework, LLC",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 40.2893,
    "Longitude": -79.5867
  },
  {
    "Date": "2025-03-02",
    "Location": "Shawnee, OK",
    "Host": "The Doggie Spot, LLC",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 35.3248,
    "Longitude": -96.9565
  },
  {
    "Date": "2025-03-07",
    "Location": "Elgin, IL",
    "Host": "For Your K9",
    "TrialTypes": "L1C, L2C, L1I, L2I",
    "EventCount": 4,
    "Latitude": 42.0474,
    "Longitude": -88.3241
  },
  {
    "Date": "2025-03-07",
    "Location": "Spring City, PA",
    "Host": "Patricia Grassey",
    "TrialTypes": "NW3, ELT, ELT-S, L1I",
    "EventCount": 4,
    "Latitude": 40.1983,
    "Longitude": -75.5963
  },
  {
    "Date": "2025-03-07",
    "Location": "Stokesdale , NC",
    "Host": "K9 Nose Adventures, LLC",
    "TrialTypes": "ELT, NW1, NW2, L1C, L1I",
    "EventCount": 5,
    "Latitude": 36.2606,
    "Longitude": -79.9754
  },
  {
    "Date": "2025-03-08",
    "Location": "Farmville, VA",
    "Host": "Paws Plus Training, LLC",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 37.2538,
    "Longitude": -78.4398
  },
  {
    "Date": "2025-03-08",
    "Location": "Fort Collins, CO",
    "Host": "Beyond Elevation K9 Training",
    "TrialTypes": "NW1, NW2, ELT",
    "EventCount": 3,
    "Latitude": 40.6096,
    "Longitude": -105.0329
  },
  {
    "Date": "2025-03-08",
    "Location": "Foxboro , MA",
    "Host": "Bay State Sniffers",
    "TrialTypes": "L1C, ELT-S, L1I, L3I",
    "EventCount": 4,
    "Latitude": 42.0766,
    "Longitude": -71.3041
  },
  {
    "Date": "2025-03-08",
    "Location": "Rome, GA",
    "Host": "Southeast Scent Work Alliance, LLC",
    "TrialTypes": "ELT, NW2",
    "EventCount": 2,
    "Latitude": 34.3041,
    "Longitude": -85.142
  },
  {
    "Date": "2025-03-08",
    "Location": "Winterset, IA",
    "Host": "KBP Dog Training",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 41.3252,
    "Longitude": -94.002
  },
  {
    "Date": "2025-03-10",
    "Location": "Riverside, CA",
    "Host": "Linda Buchanan",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 33.9327,
    "Longitude": -117.4195
  },
  {
    "Date": "2025-03-14",
    "Location": "Phoenix, AZ",
    "Host": "Successful Sniffer",
    "TrialTypes": "NW3, ELT-S, NW1, NW2",
    "EventCount": 4,
    "Latitude": 33.4908,
    "Longitude": -112.0495
  },
  {
    "Date": "2025-03-14",
    "Location": "Phoenix, MD",
    "Host": "Oriole Dog Training Club",
    "TrialTypes": "NW3, L2I, NW2",
    "EventCount": 3,
    "Latitude": 39.4671,
    "Longitude": -76.5801
  },
  {
    "Date": "2025-03-15",
    "Location": "Blaine, WA",
    "Host": "Nose Work Magic",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 48.9775,
    "Longitude": -122.7827
  },
  {
    "Date": "2025-03-15",
    "Location": "Califon (formerly Pomona), NY",
    "Host": "Shamrock Pot of Gold K9 Scenter",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 41.2014,
    "Longitude": -74.0602
  },
  {
    "Date": "2025-03-15",
    "Location": "Gainesville, GA",
    "Host": "Georgia Nosework",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 34.3374,
    "Longitude": -83.7995
  },
  {
    "Date": "2025-03-15",
    "Location": "Kent, WA",
    "Host": "Northwest K9 Sniffers",
    "TrialTypes": "ELT-S, L1V, L1I",
    "EventCount": 3,
    "Latitude": 47.3364,
    "Longitude": -122.2339
  },
  {
    "Date": "2025-03-15",
    "Location": "Pflugerville, TX",
    "Host": "Scent Work Across Texas",
    "TrialTypes": "ELT-S, NW2, L1C, L1E",
    "EventCount": 4,
    "Latitude": 30.4542,
    "Longitude": -97.6314
  },
  {
    "Date": "2025-03-15",
    "Location": "Thaxton, VA",
    "Host": "Canny K9 Companions LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 37.3675,
    "Longitude": -79.5712
  },
  {
    "Date": "2025-03-15",
    "Location": "Westminster, MD",
    "Host": "Red Huskies",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 39.5514,
    "Longitude": -77.0387
  },
  {
    "Date": "2025-03-17",
    "Location": "Corralitos, CA",
    "Host": "CalCoastal Dog Owners Group",
    "TrialTypes": "NW2, NW1",
    "EventCount": 2,
    "Latitude": 36.9623,
    "Longitude": -121.8174
  },
  {
    "Date": "2025-03-22",
    "Location": "Johnstown, NY",
    "Host": "My Dog Smells LLC",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 43.0219,
    "Longitude": -74.3422
  },
  {
    "Date": "2025-03-22",
    "Location": "Salem, WI",
    "Host": "Loving Paws Dog Training LLC",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 42.6001,
    "Longitude": -88.0972
  },
  {
    "Date": "2025-03-22",
    "Location": "Shelbyville, TN",
    "Host": "Dogs Have Amazing Noses LLC",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 35.4578,
    "Longitude": -86.4623
  },
  {
    "Date": "2025-03-22",
    "Location": "Tampa, FL",
    "Host": "Hoppin’ in the Hills",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 27.957,
    "Longitude": -82.4446
  },
  {
    "Date": "2025-03-22",
    "Location": "Wakefield, VA",
    "Host": "Paws Plus Training, LLC",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 36.9419,
    "Longitude": -76.9767
  },
  {
    "Date": "2025-03-23",
    "Location": "Rapid City, SD",
    "Host": "Two Paws Up Dog Training, LLC",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 44.0392,
    "Longitude": -103.2437
  },
  {
    "Date": "2025-03-23",
    "Location": "Upland, CA",
    "Host": "Agile Paws Dog Sports",
    "TrialTypes": "NW1",
    "EventCount": 1,
    "Latitude": 34.052,
    "Longitude": -117.6131
  },
  {
    "Date": "2025-03-28",
    "Location": "Dobbs Ferry, NY",
    "Host": "For the Love of Dogs NY, LLC",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 41.0319,
    "Longitude": -73.8737
  },
  {
    "Date": "2025-03-28",
    "Location": "Flint, MI",
    "Host": "Every Dog Nosework",
    "TrialTypes": "SMT, ELT-S, L2I",
    "EventCount": 3,
    "Latitude": 43.047,
    "Longitude": -83.6858
  },
  {
    "Date": "2025-03-28",
    "Location": "Frederick, MD",
    "Host": "Red Huskies",
    "TrialTypes": "ELT-P, ELT, ELT-S",
    "EventCount": 3,
    "Latitude": 39.4005,
    "Longitude": -77.4505
  },
  {
    "Date": "2025-03-28",
    "Location": "Grand Junction, CO",
    "Host": "Mountain Dogs, LLC",
    "TrialTypes": "NW3, L1C, NW2, NW1",
    "EventCount": 4,
    "Latitude": 39.1088,
    "Longitude": -108.5259
  },
  {
    "Date": "2025-03-28",
    "Location": "Salem, OR",
    "Host": "Kristina Leipzig, Doglandia LLC and Carol Forsberg",
    "TrialTypes": "ELT-S",
    "EventCount": 1,
    "Latitude": 44.9592,
    "Longitude": -123.0592
  },
  {
    "Date": "2025-03-28",
    "Location": "Shady Hills, FL",
    "Host": "Hoppin’ in the Hills",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 28.3552,
    "Longitude": -82.542
  },
  {
    "Date": "2025-03-29",
    "Location": "Clinton, WI",
    "Host": "George and Shannon Carpenter",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 42.5473,
    "Longitude": -88.8734
  },
  {
    "Date": "2025-03-29",
    "Location": "Gilbertsville, PA",
    "Host": "Sniff Sniff Hooray",
    "TrialTypes": "NW2, ELT",
    "EventCount": 2,
    "Latitude": 40.3323,
    "Longitude": -75.6321
  },
  {
    "Date": "2025-03-29",
    "Location": "Goleta, CA",
    "Host": "All Fur Fun",
    "TrialTypes": "ELT-P, NW3",
    "EventCount": 2,
    "Latitude": 34.4364,
    "Longitude": -119.8513
  },
  {
    "Date": "2025-03-29",
    "Location": "Kennett Square, PA",
    "Host": "The Sniffing Hound",
    "TrialTypes": "ELT-S, ELT",
    "EventCount": 2,
    "Latitude": 39.8069,
    "Longitude": -75.7069
  },
  {
    "Date": "2025-03-29",
    "Location": "LeRoy, IL",
    "Host": "Kudos for Canines",
    "TrialTypes": "NW3, L1C, L2I",
    "EventCount": 3,
    "Latitude": 42.485,
    "Longitude": -88.803
  },
  {
    "Date": "2025-03-29",
    "Location": "Olathe, KS",
    "Host": "Brookside Pet Training Studio for Dogs",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 38.86,
    "Longitude": -94.8367
  },
  {
    "Date": "2025-03-30",
    "Location": "East Windsor, CT",
    "Host": "Lucky Dog Events",
    "TrialTypes": "L2V, NW2, NW3",
    "EventCount": 3,
    "Latitude": 41.9136,
    "Longitude": -72.5758
  },
  {
    "Date": "2025-04-03",
    "Location": "Alpharetta, GA",
    "Host": "Georgia Nosework",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 34.0398,
    "Longitude": -84.262
  },
  {
    "Date": "2025-04-04",
    "Location": "Easton, MD",
    "Host": "Fair Play Labradors",
    "TrialTypes": "SMT, L1V, L2V",
    "EventCount": 3,
    "Latitude": 38.7417,
    "Longitude": -76.0622
  },
  {
    "Date": "2025-04-05",
    "Location": "Genoa, IL",
    "Host": "Common Scents K9 Scent Work Club of Elgin",
    "TrialTypes": "NW2, NW3",
    "EventCount": 2,
    "Latitude": 42.1425,
    "Longitude": -88.6591
  },
  {
    "Date": "2025-04-05",
    "Location": "Kittanning, PA",
    "Host": "Steel City Nosework, LLC",
    "TrialTypes": "ELT, ELT-S, NW2",
    "EventCount": 3,
    "Latitude": 40.8042,
    "Longitude": -79.5303
  },
  {
    "Date": "2025-04-05",
    "Location": "Maple Falls, WA",
    "Host": "Nose Work Magic",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 48.901,
    "Longitude": -122.1356
  },
  {
    "Date": "2025-04-05",
    "Location": "North Java, NY",
    "Host": "Do Over Dog Training",
    "TrialTypes": "ELT-S, L2C, L3E",
    "EventCount": 3,
    "Latitude": 42.7304,
    "Longitude": -78.3308
  },
  {
    "Date": "2025-04-05",
    "Location": "Somis, CA",
    "Host": "Pink Biscuit K9s",
    "TrialTypes": "ELT, ELT-S, NW1",
    "EventCount": 3,
    "Latitude": 34.2843,
    "Longitude": -118.9937
  },
  {
    "Date": "2025-04-05",
    "Location": "Tucson, AZ",
    "Host": "Patience Unlimited Professional Dog Training",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 32.2034,
    "Longitude": -110.9638
  },
  {
    "Date": "2025-04-05",
    "Location": "Woodstock, IL",
    "Host": "Northwest Obedience Club Inc.",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 42.2817,
    "Longitude": -88.4533
  },
  {
    "Date": "2025-04-11",
    "Location": "Sequim, WA",
    "Host": "Sarah Becker, Sea Change Canine LLC & Carol Forsberg",
    "TrialTypes": "NW2, NW1",
    "EventCount": 2,
    "Latitude": 48.1031,
    "Longitude": -123.1016
  },
  {
    "Date": "2025-04-12",
    "Location": "Auburn, WA",
    "Host": "Northwest K9 Sniffers",
    "TrialTypes": "NW3, L1C, L1E",
    "EventCount": 3,
    "Latitude": 47.3547,
    "Longitude": -122.2248
  },
  {
    "Date": "2025-04-12",
    "Location": "Boone, IA",
    "Host": "KBP Dog Training",
    "TrialTypes": "ELT, NW2",
    "EventCount": 2,
    "Latitude": 42.023,
    "Longitude": -93.9666
  },
  {
    "Date": "2025-04-12",
    "Location": "Burton, OH",
    "Host": "Barns And Noses, LLC",
    "TrialTypes": "ELT, ELT-S, NW1",
    "EventCount": 3,
    "Latitude": 41.4366,
    "Longitude": -81.115
  },
  {
    "Date": "2025-04-12",
    "Location": "Carlisle, PA",
    "Host": "Nose-It-All, LLC",
    "TrialTypes": "ELT, ELT-S, L2E",
    "EventCount": 3,
    "Latitude": 40.1591,
    "Longitude": -77.1733
  },
  {
    "Date": "2025-04-12",
    "Location": "Laramie, WY",
    "Host": "Mountain Dogs, LLC",
    "TrialTypes": "ELT, NW3, ELT-S, NW2",
    "EventCount": 4,
    "Latitude": 41.3099,
    "Longitude": -105.6257
  },
  {
    "Date": "2025-04-12",
    "Location": "Michigan City, IN",
    "Host": "Indiana Scentwork",
    "TrialTypes": "NW3, ELT-S, NW2",
    "EventCount": 3,
    "Latitude": 41.7047,
    "Longitude": -86.9288
  },
  {
    "Date": "2025-04-12",
    "Location": "Peekskill, NY",
    "Host": "For the Love of Dogs NY, LLC",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 41.2492,
    "Longitude": -73.9475
  },
  {
    "Date": "2025-04-12",
    "Location": "Rhinebeck, NY",
    "Host": "Top Notch Dogs, LLC",
    "TrialTypes": "ELT, L1C, L2C",
    "EventCount": 3,
    "Latitude": 41.9673,
    "Longitude": -73.9382
  },
  {
    "Date": "2025-04-12",
    "Location": "Starke, FL",
    "Host": "River Poodles Training, LLC",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 29.8995,
    "Longitude": -82.1376
  },
  {
    "Date": "2025-04-14",
    "Location": "Sacramento, CA",
    "Host": "Two Nosey Girls",
    "TrialTypes": "L2E, L3E, ELT",
    "EventCount": 3,
    "Latitude": 38.5845,
    "Longitude": -121.5076
  },
  {
    "Date": "2025-04-18",
    "Location": "Asheboro, NC",
    "Host": "K9 Nose Adventures, LLC",
    "TrialTypes": "ELT, NW3, ELT-S, L2C",
    "EventCount": 4,
    "Latitude": 35.6978,
    "Longitude": -79.8317
  },
  {
    "Date": "2025-04-18",
    "Location": "Grand Junction, CO",
    "Host": "Mountain Dogs, LLC",
    "TrialTypes": "ELT, NW3, NW1, NW2",
    "EventCount": 4,
    "Latitude": 39.0635,
    "Longitude": -108.56
  },
  {
    "Date": "2025-04-18",
    "Location": "Palmer, MA",
    "Host": "HeavenScent Sniffers",
    "TrialTypes": "ELT, NW3, NW1, NW2",
    "EventCount": 4,
    "Latitude": 42.1495,
    "Longitude": -72.3735
  },
  {
    "Date": "2025-04-18",
    "Location": "Rochester, NY",
    "Host": "Tami Sullivan",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 43.1114,
    "Longitude": -77.6096
  },
  {
    "Date": "2025-04-19",
    "Location": "Brooksville, FL",
    "Host": "Hoppin’ in the Hills",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 28.5632,
    "Longitude": -82.3791
  },
  {
    "Date": "2025-04-19",
    "Location": "Kunkletown, PA",
    "Host": "Paws n' Sniff",
    "TrialTypes": "NW3, L3E, ELT-S",
    "EventCount": 3,
    "Latitude": 40.8864,
    "Longitude": -75.4824
  },
  {
    "Date": "2025-04-24",
    "Location": "Concord, CA",
    "Host": "The Bay Team",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 37.9672,
    "Longitude": -122.0628
  },
  {
    "Date": "2025-04-25",
    "Location": "Eagan, MN",
    "Host": "St Paul Dog Training Club",
    "TrialTypes": "ELT, NW2, L2C, L3I",
    "EventCount": 4,
    "Latitude": 44.7862,
    "Longitude": -93.2115
  },
  {
    "Date": "2025-04-26",
    "Location": "Decatur, GA",
    "Host": "Right Choice Dog Training LLC",
    "TrialTypes": "L1C, L1I",
    "EventCount": 2,
    "Latitude": 30.8786,
    "Longitude": -84.5362
  },
  {
    "Date": "2025-04-26",
    "Location": "Ellicottville, NY",
    "Host": "Do Over Dog Training",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 42.2393,
    "Longitude": -78.6406
  },
  {
    "Date": "2025-04-26",
    "Location": "FT. Pierce, FL",
    "Host": "Obedience Training Club of Palm Beach County",
    "TrialTypes": "L1E, NW2, NW1, L1C",
    "EventCount": 4,
    "Latitude": 27.4535,
    "Longitude": -80.281
  },
  {
    "Date": "2025-04-26",
    "Location": "Greenfield, WI",
    "Host": "Loving Paws Dog Training LLC",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 42.9373,
    "Longitude": -88.0199
  },
  {
    "Date": "2025-04-26",
    "Location": "Hammond, LA",
    "Host": "Dog Gone Right",
    "TrialTypes": "NW1, NW2, ELT",
    "EventCount": 3,
    "Latitude": 30.5261,
    "Longitude": -90.4218
  },
  {
    "Date": "2025-04-26",
    "Location": "Kingston, NH",
    "Host": "The Big Sniff, LLC",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 42.9335,
    "Longitude": -71.0299
  },
  {
    "Date": "2025-04-26",
    "Location": "Lyons, OR",
    "Host": "Canine Discovery Corps",
    "TrialTypes": "L1I, L2C, ELT",
    "EventCount": 3,
    "Latitude": 44.779,
    "Longitude": -122.5768
  },
  {
    "Date": "2025-04-26",
    "Location": "Newtown, PA",
    "Host": "K9 Nosen Around, LLC",
    "TrialTypes": "L1V, NW1, L2V, NW2",
    "EventCount": 4,
    "Latitude": 40.2515,
    "Longitude": -74.8947
  },
  {
    "Date": "2025-04-26",
    "Location": "Northampton, MA",
    "Host": "Lucky Dog Events",
    "TrialTypes": "ELT-P, ELT-S, L1C",
    "EventCount": 3,
    "Latitude": 42.3647,
    "Longitude": -72.6753
  },
  {
    "Date": "2025-04-26",
    "Location": "Ocoee, TN",
    "Host": "Camelot Shepherds, Inc.",
    "TrialTypes": "NW1, NW2, NW3",
    "EventCount": 3,
    "Latitude": 35.1227,
    "Longitude": -84.7395
  },
  {
    "Date": "2025-04-26",
    "Location": "Red Feather Lakes, CO",
    "Host": "Beyond Elevation K9",
    "TrialTypes": "NW2, NW3",
    "EventCount": 2,
    "Latitude": 40.7887,
    "Longitude": -105.5324
  },
  {
    "Date": "2025-04-26",
    "Location": "Traverse City, MI",
    "Host": "Every Dog Nosework",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 44.7545,
    "Longitude": -85.608
  },
  {
    "Date": "2025-04-26",
    "Location": "West Friendship, MD",
    "Host": "Red Huskies",
    "TrialTypes": "NW1, NW2, L2V, L1C",
    "EventCount": 4,
    "Latitude": 39.2993,
    "Longitude": -76.9264
  },
  {
    "Date": "2025-05-01",
    "Location": "Gainesville, GA",
    "Host": "Georgia Nosework",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 34.2582,
    "Longitude": -83.8141
  },
  {
    "Date": "2025-05-02",
    "Location": "Faribault, MN",
    "Host": "St Paul Dog Training Club",
    "TrialTypes": "NW3, ELT-P, L3E, L3C",
    "EventCount": 4,
    "Latitude": 43.6955,
    "Longitude": -93.9232
  },
  {
    "Date": "2025-05-02",
    "Location": "Nyack, NY",
    "Host": "Waggin Work",
    "TrialTypes": "NW3, ELT, ELT-S",
    "EventCount": 3,
    "Latitude": 41.0799,
    "Longitude": -73.9345
  },
  {
    "Date": "2025-05-03",
    "Location": "Alexis, IL",
    "Host": "Kudos for Canines",
    "TrialTypes": "NW3, ELT-P",
    "EventCount": 2,
    "Latitude": 41.1126,
    "Longitude": -90.575
  },
  {
    "Date": "2025-05-03",
    "Location": "Ashby, MA",
    "Host": "Carolyn Barney dba Dogs!",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 42.6536,
    "Longitude": -71.7763
  },
  {
    "Date": "2025-05-03",
    "Location": "Columbus, MT",
    "Host": "Canine Connection",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 45.5953,
    "Longitude": -109.2565
  },
  {
    "Date": "2025-05-03",
    "Location": "Gray Court, SC",
    "Host": "Foothills Canine Academy, LLC",
    "TrialTypes": "L1V, NW1, NW3",
    "EventCount": 3,
    "Latitude": 34.6221,
    "Longitude": -82.0845
  },
  {
    "Date": "2025-05-03",
    "Location": "Redwood City, CA",
    "Host": "B. L. McMutts",
    "TrialTypes": "NW2, NW1",
    "EventCount": 2,
    "Latitude": 37.4989,
    "Longitude": -122.2603
  },
  {
    "Date": "2025-05-03",
    "Location": "Sandy, OR",
    "Host": "Trust Your Dog K9 Events",
    "TrialTypes": "NW2, NW3",
    "EventCount": 2,
    "Latitude": 45.3738,
    "Longitude": -122.2642
  },
  {
    "Date": "2025-05-03",
    "Location": "Santa Paula, CA",
    "Host": "Pink Biscuit K9s",
    "TrialTypes": "ELT, NW1, NW2",
    "EventCount": 3,
    "Latitude": 34.3271,
    "Longitude": -119.0692
  },
  {
    "Date": "2025-05-03",
    "Location": "White Salmon, WA",
    "Host": "Trisha Thompson and Sharon Smith",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 45.7152,
    "Longitude": -121.4622
  },
  {
    "Date": "2025-05-09",
    "Location": "South Sterling, PA",
    "Host": "Paws n' Sniff",
    "TrialTypes": "L3C, L2I, NW2",
    "EventCount": 3,
    "Latitude": 41.2809,
    "Longitude": -75.3099
  },
  {
    "Date": "2025-05-09",
    "Location": "Warwick, NY",
    "Host": "Top Notch Dogs, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 41.2862,
    "Longitude": -74.3895
  },
  {
    "Date": "2025-05-10",
    "Location": "Alexander, NY",
    "Host": "Do Over Dog Training",
    "TrialTypes": "L1V, L2I, L1C, L3V",
    "EventCount": 4,
    "Latitude": 42.8803,
    "Longitude": -78.2122
  },
  {
    "Date": "2025-05-10",
    "Location": "Denton, MD",
    "Host": "Fair Play Labradors",
    "TrialTypes": "NW3, ELT-S",
    "EventCount": 2,
    "Latitude": 38.8448,
    "Longitude": -75.8723
  },
  {
    "Date": "2025-05-10",
    "Location": "Poland Springs, ME",
    "Host": "Virginia Howe",
    "TrialTypes": "NW2, ELT",
    "EventCount": 2,
    "Latitude": 43.9791,
    "Longitude": -70.3586
  },
  {
    "Date": "2025-05-10",
    "Location": "Rainier, WA",
    "Host": "Rachelle Bailey-Austin/About Face K9 Academy & Dorothy Turley/Let's Talk Dogs, LLC",
    "TrialTypes": "ELT-S, NW2",
    "EventCount": 2,
    "Latitude": 46.8535,
    "Longitude": -122.7175
  },
  {
    "Date": "2025-05-10",
    "Location": "Santa Barbara, CA",
    "Host": "All Fur Fun",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 34.4658,
    "Longitude": -119.6763
  },
  {
    "Date": "2025-05-13",
    "Location": "Paso Robles, CA",
    "Host": "Gentle Touch Pet Training",
    "TrialTypes": "L2C, NW1",
    "EventCount": 2,
    "Latitude": 35.6439,
    "Longitude": -120.7289
  },
  {
    "Date": "2025-05-16",
    "Location": "Montrose, CO",
    "Host": "Mountain Dogs, LLC",
    "TrialTypes": "ELT, NW3, ELT-P",
    "EventCount": 3,
    "Latitude": 38.4699,
    "Longitude": -107.8432
  },
  {
    "Date": "2025-05-16",
    "Location": "Watsonville, CA",
    "Host": "CalCoastal Dog Owners Group",
    "TrialTypes": "ELT-S, L2C, L3C",
    "EventCount": 3,
    "Latitude": 36.8895,
    "Longitude": -121.7652
  },
  {
    "Date": "2025-05-17",
    "Location": "Burien, WA",
    "Host": "Northwest K9 Sniffers",
    "TrialTypes": "ELT, L2V, L2I",
    "EventCount": 3,
    "Latitude": 47.4581,
    "Longitude": -122.3145
  },
  {
    "Date": "2025-05-17",
    "Location": "Cobleskill, NY",
    "Host": "The Brainy Canine",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 42.6651,
    "Longitude": -74.5305
  },
  {
    "Date": "2025-05-17",
    "Location": "Emmitsburg, MD",
    "Host": "Red Huskies",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 39.7259,
    "Longitude": -77.3354
  },
  {
    "Date": "2025-05-17",
    "Location": "Forest Junction, WI",
    "Host": "N.E.W K9 Scent Work LLC",
    "TrialTypes": "L1C, NW1, NW2",
    "EventCount": 3,
    "Latitude": 44.2165,
    "Longitude": -88.1426
  },
  {
    "Date": "2025-05-17",
    "Location": "Norton, MA",
    "Host": "Dogs Make Scents",
    "TrialTypes": "NW3, NW1",
    "EventCount": 2,
    "Latitude": 41.9538,
    "Longitude": -71.2367
  },
  {
    "Date": "2025-05-17",
    "Location": "Peru, MA",
    "Host": "Lucky Dog Events",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 42.4286,
    "Longitude": -73.0776
  },
  {
    "Date": "2025-05-17",
    "Location": "Valley Forge, PA",
    "Host": "Sniff Sniff Hooray",
    "TrialTypes": "NW1, ELT",
    "EventCount": 2,
    "Latitude": 40.0745,
    "Longitude": -75.5184
  },
  {
    "Date": "2025-05-23",
    "Location": "La Jolla, CA",
    "Host": "Anita Cheesman and Jessica Koester",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 32.879,
    "Longitude": -117.249
  },
  {
    "Date": "2025-05-24",
    "Location": "Altamont , NY",
    "Host": "My Dog Smells LLC",
    "TrialTypes": "ELT, NW2",
    "EventCount": 2,
    "Latitude": 42.6992,
    "Longitude": -74.0228
  },
  {
    "Date": "2025-05-24",
    "Location": "Batavia, NY",
    "Host": "Do Over Dog Training",
    "TrialTypes": "ELT-P, L1I, L3I",
    "EventCount": 3,
    "Latitude": 42.9871,
    "Longitude": -78.2208
  },
  {
    "Date": "2025-05-24",
    "Location": "Lancaster, PA",
    "Host": "Nose-It-All, LLC",
    "TrialTypes": "L3I, NW2, ELT",
    "EventCount": 3,
    "Latitude": 40.0561,
    "Longitude": -76.3056
  },
  {
    "Date": "2025-05-24",
    "Location": "Norwich , CT",
    "Host": "For the Love of Dogs NY LLC",
    "TrialTypes": "ELT-S, NW2, NW3, ELT",
    "EventCount": 4,
    "Latitude": 41.5485,
    "Longitude": -72.1005
  },
  {
    "Date": "2025-05-24",
    "Location": "Rockaway, NJ",
    "Host": "Shamrock Pot of Gold K9 Scenter",
    "TrialTypes": "NW3, ELT-S, NW1, ELT-P",
    "EventCount": 4,
    "Latitude": 40.9327,
    "Longitude": -74.5176
  },
  {
    "Date": "2025-05-24",
    "Location": "Waukesha , WI",
    "Host": "Think Pawsitive Dog Training",
    "TrialTypes": "NW1, L1V, L1C",
    "EventCount": 3,
    "Latitude": 43.1093,
    "Longitude": -88.3014
  },
  {
    "Date": "2025-05-24",
    "Location": "Welches, OR",
    "Host": "Trust Your Dog K9 Events",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 45.3786,
    "Longitude": -121.983
  },
  {
    "Date": "2025-05-29",
    "Location": "Bayfield, CO",
    "Host": "Wag Between Barks",
    "TrialTypes": "ELT-S, ELT, NW3",
    "EventCount": 3,
    "Latitude": 37.1827,
    "Longitude": -107.5495
  },
  {
    "Date": "2025-05-30",
    "Location": "Honesdale, PA",
    "Host": "Your Dog's Place, LLC",
    "TrialTypes": "NW3, ELT, ELT-S, NW2",
    "EventCount": 4,
    "Latitude": 41.6066,
    "Longitude": -75.2446
  },
  {
    "Date": "2025-05-30",
    "Location": "Moline, IL",
    "Host": "Fur Better Fur Worse Dog Training",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 41.5475,
    "Longitude": -90.4873
  },
  {
    "Date": "2025-05-31",
    "Location": "Amherst, NY",
    "Host": "Do Over Dog Training",
    "TrialTypes": "NW2, ELT-S, NW1",
    "EventCount": 3,
    "Latitude": 42.9616,
    "Longitude": -78.7552
  },
  {
    "Date": "2025-05-31",
    "Location": "Columbus, MT",
    "Host": "Canine Connection",
    "TrialTypes": "L1V, NW1, NW3",
    "EventCount": 3,
    "Latitude": 45.6198,
    "Longitude": -109.2167
  },
  {
    "Date": "2025-05-31",
    "Location": "Eden Prairie, MN",
    "Host": "The K9 Nose",
    "TrialTypes": "NW1, L1I, L2I",
    "EventCount": 3,
    "Latitude": 44.8142,
    "Longitude": -93.4275
  },
  {
    "Date": "2025-05-31",
    "Location": "Napa, CA",
    "Host": "Napa Valley Dog Training Club",
    "TrialTypes": "NW2, NW1",
    "EventCount": 2,
    "Latitude": 38.454,
    "Longitude": -122.3083
  },
  {
    "Date": "2025-05-31",
    "Location": "New Wilmington, PA",
    "Host": "Steel City Nosework, LLC",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 41.0988,
    "Longitude": -80.3795
  },
  {
    "Date": "2025-05-31",
    "Location": "North Manchester, IN",
    "Host": "2 Nose You Is 2 Loves You",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 41.0122,
    "Longitude": -85.8057
  },
  {
    "Date": "2025-06-06",
    "Location": "Pueblo, CO",
    "Host": "Mountain Dogs, LLC",
    "TrialTypes": "SMT, ELT",
    "EventCount": 2,
    "Latitude": 38.2566,
    "Longitude": -104.61
  },
  {
    "Date": "2025-06-06",
    "Location": "Winsted, CT",
    "Host": "Waggin’ Work",
    "TrialTypes": "ELT, NW2, L2C",
    "EventCount": 3,
    "Latitude": 41.9538,
    "Longitude": -73.0517
  },
  {
    "Date": "2025-06-07",
    "Location": "Clancy, MT",
    "Host": "Nosework Breakfast Club",
    "TrialTypes": "NW2, NW3",
    "EventCount": 2,
    "Latitude": 46.4458,
    "Longitude": -111.9808
  },
  {
    "Date": "2025-06-07",
    "Location": "Cumming, GA",
    "Host": "Georgia Nosework",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 34.2271,
    "Longitude": -84.1677
  },
  {
    "Date": "2025-06-07",
    "Location": "Davenport, IA",
    "Host": "Fur Better Fur Worse Dog Training",
    "TrialTypes": "ELT-S, NW2, NW1",
    "EventCount": 3,
    "Latitude": 41.4999,
    "Longitude": -90.6238
  },
  {
    "Date": "2025-06-07",
    "Location": "Enterprise, OR",
    "Host": "Country K9 Nosework, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 45.459,
    "Longitude": -117.2462
  },
  {
    "Date": "2025-06-07",
    "Location": "Grants Pass, OR",
    "Host": "Nose Work Detectives",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 42.442,
    "Longitude": -123.3143
  },
  {
    "Date": "2025-06-07",
    "Location": "Meadowbrook, PA",
    "Host": "Patricia Grassey",
    "TrialTypes": "NW1, NW2, ELT-S, ELT",
    "EventCount": 4,
    "Latitude": 40.0897,
    "Longitude": -75.0931
  },
  {
    "Date": "2025-06-07",
    "Location": "Palmyra, VA",
    "Host": "Your Dog Knows, LLC",
    "TrialTypes": "NW1, L1I",
    "EventCount": 2,
    "Latitude": 37.8386,
    "Longitude": -78.3012
  },
  {
    "Date": "2025-06-07",
    "Location": "Wrightstown, WI",
    "Host": "N.E.W. K9 Scent Work, LLC",
    "TrialTypes": "ELT-P, L2C, L3I",
    "EventCount": 3,
    "Latitude": 44.3105,
    "Longitude": -88.1457
  },
  {
    "Date": "2025-06-13",
    "Location": "Jordan, MN",
    "Host": "St Paul Dog Training Club",
    "TrialTypes": "NW3, ELT-S, L1V, L2E, L3V",
    "EventCount": 5,
    "Latitude": 44.6179,
    "Longitude": -93.6724
  },
  {
    "Date": "2025-06-14",
    "Location": "Cummington, MA",
    "Host": "Lucky Dog Events",
    "TrialTypes": "ELT-P, ELT-S, L2C",
    "EventCount": 3,
    "Latitude": 42.42,
    "Longitude": -72.8958
  },
  {
    "Date": "2025-06-14",
    "Location": "Danvers, MA",
    "Host": "Everydog, LLC",
    "TrialTypes": "L2I, NW2, NW1",
    "EventCount": 3,
    "Latitude": 42.5528,
    "Longitude": -70.9795
  },
  {
    "Date": "2025-06-14",
    "Location": "Ithaca, NY",
    "Host": "The Brainy Canine",
    "TrialTypes": "ELT-P, ELT",
    "EventCount": 2,
    "Latitude": 42.4666,
    "Longitude": -76.5818
  },
  {
    "Date": "2025-06-14",
    "Location": "Linden, MI",
    "Host": "Every Dog Nosework",
    "TrialTypes": "NW1, ELT-P",
    "EventCount": 2,
    "Latitude": 42.7906,
    "Longitude": -83.7945
  },
  {
    "Date": "2025-06-20",
    "Location": "Greeley, CO",
    "Host": "Beyond Elevation K9",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 40.4556,
    "Longitude": -104.6966
  },
  {
    "Date": "2025-06-20",
    "Location": "San Luis Obispo, CA",
    "Host": "Central Coast Nosework Club of California, Inc.",
    "TrialTypes": "NW2, ELT",
    "EventCount": 2,
    "Latitude": 35.3134,
    "Longitude": -120.4156
  },
  {
    "Date": "2025-06-20",
    "Location": "Upland, CA",
    "Host": "Agile Paws Dog Sports",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 34.1458,
    "Longitude": -117.6954
  },
  {
    "Date": "2025-06-20",
    "Location": "Warwick, NY",
    "Host": "For the Love of Dogs NY LLC",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 41.3007,
    "Longitude": -74.371
  },
  {
    "Date": "2025-06-21",
    "Location": "Fayette, MO",
    "Host": "Columbia Canine Sports Center",
    "TrialTypes": "ELT, NW1, NW2",
    "EventCount": 3,
    "Latitude": 39.1223,
    "Longitude": -92.6895
  },
  {
    "Date": "2025-06-21",
    "Location": "Inver Grove Heights, MN",
    "Host": "Outside The Box Dog Training, LLC",
    "TrialTypes": "L1C, NW2, ELT",
    "EventCount": 3,
    "Latitude": 44.826,
    "Longitude": -93.0152
  },
  {
    "Date": "2025-06-21",
    "Location": "Jefferson, WI",
    "Host": "Think Pawsitive Dog Training",
    "TrialTypes": "NW2",
    "EventCount": 1,
    "Latitude": 42.9863,
    "Longitude": -88.7791
  },
  {
    "Date": "2025-06-21",
    "Location": "Toledo, OH",
    "Host": "Robin Ford Dog Training, LLC",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 41.6742,
    "Longitude": -83.5501
  },
  {
    "Date": "2025-06-21",
    "Location": "Woodstock, GA",
    "Host": "Georgia Nosework LLC",
    "TrialTypes": "NW3, L2C, NW1",
    "EventCount": 3,
    "Latitude": 34.1094,
    "Longitude": -84.4946
  },
  {
    "Date": "2025-06-25",
    "Location": "Kenai, AK",
    "Host": "Peninsula Dog Obedience Group",
    "TrialTypes": "NW1, NW2, NW3, ELT",
    "EventCount": 4,
    "Latitude": 60.5741,
    "Longitude": -151.2883
  },
  {
    "Date": "2025-06-28",
    "Location": "Delran, NJ",
    "Host": "K9 InScentives",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 40.0401,
    "Longitude": -74.924
  },
  {
    "Date": "2025-06-28",
    "Location": "Deming, WA",
    "Host": "Nose Work Magic",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 48.8868,
    "Longitude": -122.2554
  },
  {
    "Date": "2025-06-28",
    "Location": "Kenosha, WI",
    "Host": "Loving Paws Dog Training LLC",
    "TrialTypes": "NW3, NW1",
    "EventCount": 2,
    "Latitude": 42.5839,
    "Longitude": -87.7981
  },
  {
    "Date": "2025-06-28",
    "Location": "Somers, CT",
    "Host": "HeavenScent Sniffers",
    "TrialTypes": "ELT, L1V, NW1",
    "EventCount": 3,
    "Latitude": 41.9532,
    "Longitude": -72.4464
  },
  {
    "Date": "2025-06-28",
    "Location": "St. Paul, MN",
    "Host": "Bark and Bond LLC",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 44.9428,
    "Longitude": -93.0577
  },
  {
    "Date": "2025-06-28",
    "Location": "Stevenson, WA",
    "Host": "Sharon Smith",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 45.6452,
    "Longitude": -121.8572
  },
  {
    "Date": "2025-07-04",
    "Location": "Huntington, MA",
    "Host": "Shamrock Pot Of Gold K9 Scenter",
    "TrialTypes": "ELT, NW3, L2I, ELT-S",
    "EventCount": 4,
    "Latitude": 42.2518,
    "Longitude": -72.8664
  },
  {
    "Date": "2025-07-05",
    "Location": "Delran, NJ",
    "Host": "Ev-ry Earthdog, LLC",
    "TrialTypes": "NW3, NW1, NW2, ELT",
    "EventCount": 4,
    "Latitude": 39.9882,
    "Longitude": -75.0049
  },
  {
    "Date": "2025-07-11",
    "Location": "Leadville, CO",
    "Host": "Mountain Dogs LLC",
    "TrialTypes": "ELT, NW3, ELT-S, NW2",
    "EventCount": 4,
    "Latitude": 39.2389,
    "Longitude": -106.2899
  },
  {
    "Date": "2025-07-12",
    "Location": "Brainerd, MN",
    "Host": "Nose 2 Tail Dog Training LLC",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 46.3149,
    "Longitude": -94.1904
  },
  {
    "Date": "2025-07-12",
    "Location": "Livonia, MI",
    "Host": "Every Dog Nosework",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 42.3715,
    "Longitude": -83.3047
  },
  {
    "Date": "2025-07-18",
    "Location": "Leadville, CO",
    "Host": "Mountain Dogs, LLC",
    "TrialTypes": "NW3, NW1, NW2, L1C, L1I",
    "EventCount": 5,
    "Latitude": 39.2698,
    "Longitude": -106.2734
  },
  {
    "Date": "2025-07-19",
    "Location": "Dunmore, PA",
    "Host": "Your Dog's Place, LLC",
    "TrialTypes": "ELT-S, NW1, L1C",
    "EventCount": 3,
    "Latitude": 41.4308,
    "Longitude": -75.6354
  },
  {
    "Date": "2025-07-19",
    "Location": "Fayette , MO",
    "Host": "Columbia Canine Sports Center",
    "TrialTypes": "NW3, L1C, L1I",
    "EventCount": 3,
    "Latitude": 39.1771,
    "Longitude": -92.7066
  },
  {
    "Date": "2025-07-19",
    "Location": "Houlton, WI",
    "Host": "Gretchen Hofheins-Wackerfuss",
    "TrialTypes": "NW1, ELT-S, ELT-P",
    "EventCount": 3,
    "Latitude": 45.0828,
    "Longitude": -92.8324
  },
  {
    "Date": "2025-07-19",
    "Location": "Walpole, MA",
    "Host": "MasterPeace Dog Training",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 42.1191,
    "Longitude": -71.3026
  },
  {
    "Date": "2025-07-21",
    "Location": "Montgomery, NY",
    "Host": "For the Love of Dogs NY LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 42.9103,
    "Longitude": -74.4233
  },
  {
    "Date": "2025-08-02",
    "Location": "Altamont, IL",
    "Host": "Kudos for Canines, LLC",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 39.0459,
    "Longitude": -88.7408
  },
  {
    "Date": "2025-08-02",
    "Location": "Anchorage, AK",
    "Host": "Alaska Dog Sports, LLC",
    "TrialTypes": "NW3, NW2, NW1",
    "EventCount": 3,
    "Latitude": 61.2269,
    "Longitude": -149.9329
  },
  {
    "Date": "2025-08-02",
    "Location": "Bettendorf, IA",
    "Host": "Fur Better Fur Worse Dog Training",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 41.492,
    "Longitude": -90.5135
  },
  {
    "Date": "2025-08-02",
    "Location": "Jefferson, WI",
    "Host": "Think Pawsitive Dog Training LLC",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 43.041,
    "Longitude": -88.7449
  },
  {
    "Date": "2025-08-02",
    "Location": "Pillager, MN",
    "Host": "Nose 2 Tail Dog Training, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 46.336,
    "Longitude": -94.4613
  },
  {
    "Date": "2025-08-02",
    "Location": "Red Lodge, MT",
    "Host": "Canine Connection",
    "TrialTypes": "L1I, ELT-S, ELT",
    "EventCount": 3,
    "Latitude": 45.2116,
    "Longitude": -109.2647
  },
  {
    "Date": "2025-08-02",
    "Location": "Rochester, NY",
    "Host": "Suzan Tessier",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 43.1318,
    "Longitude": -77.5759
  },
  {
    "Date": "2025-08-11",
    "Location": "Cambria, CA",
    "Host": "Gentle Touch Pet Training",
    "TrialTypes": "L1C, L2I, ELT",
    "EventCount": 3,
    "Latitude": 35.5961,
    "Longitude": -121.0647
  },
  {
    "Date": "2025-08-15",
    "Location": "Huntington Beach, CA",
    "Host": "JavaK9s, LLC",
    "TrialTypes": "ELT-P, L1C, L1I",
    "EventCount": 3,
    "Latitude": 33.6903,
    "Longitude": -117.9595
  },
  {
    "Date": "2025-08-16",
    "Location": "Colesville , MD",
    "Host": "Red Huskies",
    "TrialTypes": "L3C, ELT-S, NW2, ELT",
    "EventCount": 4,
    "Latitude": 39.0709,
    "Longitude": -76.9854
  },
  {
    "Date": "2025-08-16",
    "Location": "Mount Kisco, NY",
    "Host": "For the Love of Dogs, LLC",
    "TrialTypes": "L2I, NW2, L1I, NW1",
    "EventCount": 4,
    "Latitude": 41.1654,
    "Longitude": -73.6932
  },
  {
    "Date": "2025-08-16",
    "Location": "Reedsport, OR",
    "Host": "Kiddy Christie",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 43.752,
    "Longitude": -124.118
  },
  {
    "Date": "2025-08-22",
    "Location": "Chelsea, MI",
    "Host": "Force Free Dale, LLC",
    "TrialTypes": "NW3, NW2, NW1",
    "EventCount": 3,
    "Latitude": 42.3639,
    "Longitude": -84.0022
  },
  {
    "Date": "2025-08-23",
    "Location": "Gervais, OR",
    "Host": "Helix Fairweather & Doglandia, LLC",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 45.0632,
    "Longitude": -122.8762
  },
  {
    "Date": "2025-08-23",
    "Location": "Johnstown, NY",
    "Host": "My Dog Smells, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 43.0243,
    "Longitude": -74.3731
  },
  {
    "Date": "2025-08-23",
    "Location": "Tyngsborough, MA",
    "Host": "Spot-On K9 Coaching",
    "TrialTypes": "NW3, L1C, L1I",
    "EventCount": 3,
    "Latitude": 42.6606,
    "Longitude": -71.3825
  },
  {
    "Date": "2025-08-29",
    "Location": "Bridger, MT",
    "Host": "Canine Connection",
    "TrialTypes": "NW1, L2I, NW3",
    "EventCount": 3,
    "Latitude": 45.2897,
    "Longitude": -108.9147
  },
  {
    "Date": "2025-08-30",
    "Location": "Eliot, ME",
    "Host": "McLean Pups, LLC",
    "TrialTypes": "L1V, L1E",
    "EventCount": 2,
    "Latitude": 43.1351,
    "Longitude": -70.808
  },
  {
    "Date": "2025-08-30",
    "Location": "Fort Worth, TX",
    "Host": "North Texas Nosework Club",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 32.7929,
    "Longitude": -97.2946
  },
  {
    "Date": "2025-08-30",
    "Location": "Pomfret Center, CT",
    "Host": "HeavenScent Sniffers",
    "TrialTypes": "NW3, NW2, NW1",
    "EventCount": 3,
    "Latitude": 41.8958,
    "Longitude": -71.9519
  },
  {
    "Date": "2025-08-31",
    "Location": "Helena, MT",
    "Host": "Nosework Breakfast Club",
    "TrialTypes": "NW2, NW3",
    "EventCount": 2,
    "Latitude": 46.5924,
    "Longitude": -112.0766
  },
  {
    "Date": "2025-09-05",
    "Location": "Centreville, MD",
    "Host": "Fair Play Labradors",
    "TrialTypes": "ELT, NW3, ELT-S",
    "EventCount": 3,
    "Latitude": 39.0919,
    "Longitude": -76.0503
  },
  {
    "Date": "2025-09-06",
    "Location": "Dunkirk, NY",
    "Host": "Do Over Dog Training",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 42.4526,
    "Longitude": -79.3405
  },
  {
    "Date": "2025-09-06",
    "Location": "Loma Mar, CA",
    "Host": "The Bay Team",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 37.2787,
    "Longitude": -122.3073
  },
  {
    "Date": "2025-09-06",
    "Location": "North Bend, WA",
    "Host": "Northwest K9 Sniffers",
    "TrialTypes": "NW2, L2E, L2C",
    "EventCount": 3,
    "Latitude": 47.5222,
    "Longitude": -121.7603
  },
  {
    "Date": "2025-09-12",
    "Location": "Frederick, MD",
    "Host": "Red Huskies",
    "TrialTypes": "NW3, ELT, ELT-S",
    "EventCount": 3,
    "Latitude": 39.3806,
    "Longitude": -77.3946
  },
  {
    "Date": "2025-09-12",
    "Location": "Honey Brook, PA",
    "Host": "Patricia Grassey",
    "TrialTypes": "ELT, ELT-S, NW3",
    "EventCount": 3,
    "Latitude": 40.054,
    "Longitude": -75.9013
  },
  {
    "Date": "2025-09-12",
    "Location": "Lakeville, MN",
    "Host": "St Paul Dog Training Club",
    "TrialTypes": "ELT, NW2, NW1",
    "EventCount": 3,
    "Latitude": 44.6136,
    "Longitude": -93.2462
  },
  {
    "Date": "2025-09-12",
    "Location": "New Milford, PA",
    "Host": "Your Dog's Place, LLC",
    "TrialTypes": "ELT-P, ELT-S, L2V, L1E",
    "EventCount": 4,
    "Latitude": 41.8926,
    "Longitude": -75.7095
  },
  {
    "Date": "2025-09-13",
    "Location": "Ames, IA",
    "Host": "KBP Dog Training",
    "TrialTypes": "ELT-S, ELT",
    "EventCount": 2,
    "Latitude": 42.0074,
    "Longitude": -93.6045
  },
  {
    "Date": "2025-09-13",
    "Location": "Colebrook, CT",
    "Host": "For the Love of Dogs, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 41.9935,
    "Longitude": -73.1194
  },
  {
    "Date": "2025-09-13",
    "Location": "Hermosa, SD",
    "Host": "Two Paws Up Dog Training, LLC",
    "TrialTypes": "NW3, L1C, L1I",
    "EventCount": 3,
    "Latitude": 43.8246,
    "Longitude": -103.2313
  },
  {
    "Date": "2025-09-13",
    "Location": "Jefferson, GA",
    "Host": "Right Choice Dog Training, LLC",
    "TrialTypes": "L2V, NW2, ELT-S, L1C",
    "EventCount": 4,
    "Latitude": 33.0348,
    "Longitude": -82.4233
  },
  {
    "Date": "2025-09-13",
    "Location": "Pittsburgh , PA",
    "Host": "Steel City Nosework, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 40.3995,
    "Longitude": -79.9976
  },
  {
    "Date": "2025-09-15",
    "Location": "Green Lane, PA",
    "Host": "Sniff Sniff Hooray",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 40.3126,
    "Longitude": -75.4269
  },
  {
    "Date": "2025-09-19",
    "Location": "Flint, MI",
    "Host": "Every Dog Nosework",
    "TrialTypes": "NW3, ELT-S, L1C, NW2",
    "EventCount": 4,
    "Latitude": 43.0145,
    "Longitude": -83.6795
  },
  {
    "Date": "2025-09-19",
    "Location": "Fruita, CO",
    "Host": "Mountain Dogs LLC",
    "TrialTypes": "ELT, NW3, NW2",
    "EventCount": 3,
    "Latitude": 39.1157,
    "Longitude": -108.7803
  },
  {
    "Date": "2025-09-20",
    "Location": "Cumming, GA",
    "Host": "Georgia Nosework",
    "TrialTypes": "NW3, ELT-S, L2I, ELT",
    "EventCount": 4,
    "Latitude": 34.1871,
    "Longitude": -84.1144
  },
  {
    "Date": "2025-09-20",
    "Location": "Darlington, MD",
    "Host": "Firezone GS",
    "TrialTypes": "NW3, L1E, NW2",
    "EventCount": 3,
    "Latitude": 39.6226,
    "Longitude": -76.1623
  },
  {
    "Date": "2025-09-20",
    "Location": "Egg Harbor City, NJ",
    "Host": "Rotts-n-Notts Nosework, LLC",
    "TrialTypes": "NW3, L2I, NW2",
    "EventCount": 3,
    "Latitude": 39.5115,
    "Longitude": -74.6014
  },
  {
    "Date": "2025-09-20",
    "Location": "Fishkill, NY",
    "Host": "Top Notch Dogs, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 41.5842,
    "Longitude": -73.9378
  },
  {
    "Date": "2025-09-20",
    "Location": "Mesquite, TX",
    "Host": "All About The Nose",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 32.7912,
    "Longitude": -96.5805
  },
  {
    "Date": "2025-09-20",
    "Location": "Novato, CA",
    "Host": "Marin Humane",
    "TrialTypes": "NW1, NW2, NW3",
    "EventCount": 3,
    "Latitude": 38.1177,
    "Longitude": -122.5622
  },
  {
    "Date": "2025-09-20",
    "Location": "Sunriver, OR",
    "Host": "Canine Discovery Corps",
    "TrialTypes": "NW1, NW2, NW3",
    "EventCount": 3,
    "Latitude": 43.9128,
    "Longitude": -121.4417
  },
  {
    "Date": "2025-09-20",
    "Location": "Tuftonboro, NH",
    "Host": "The Big Sniff, LLC",
    "TrialTypes": "NW2, L2E, L2V",
    "EventCount": 3,
    "Latitude": 43.6438,
    "Longitude": -71.3217
  },
  {
    "Date": "2025-09-20",
    "Location": "White Salmon, WA",
    "Host": "Sharon Smith",
    "TrialTypes": "NW1, NW2, NW3",
    "EventCount": 3,
    "Latitude": 45.7619,
    "Longitude": -121.4638
  },
  {
    "Date": "2025-09-26",
    "Location": "Hockessin, DE",
    "Host": "Patricia Grassey",
    "TrialTypes": "L2E, NW2, NW1, L2I, NW3",
    "EventCount": 5,
    "Latitude": 39.7584,
    "Longitude": -75.6635
  },
  {
    "Date": "2025-09-27",
    "Location": "Copake, NY",
    "Host": "Saints2Source, LLC",
    "TrialTypes": "NW3, ELT-P, ELT",
    "EventCount": 3,
    "Latitude": 42.1198,
    "Longitude": -73.5323
  },
  {
    "Date": "2025-09-27",
    "Location": "Florissant, MO",
    "Host": "Happy Dog Concepts, LLC",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 38.8045,
    "Longitude": -90.2861
  },
  {
    "Date": "2025-09-27",
    "Location": "Kilmarnock, VA",
    "Host": "Red Huskies",
    "TrialTypes": "ELT, ELT-S, NW2",
    "EventCount": 3,
    "Latitude": 37.7407,
    "Longitude": -76.3556
  },
  {
    "Date": "2025-09-27",
    "Location": "Moultonborough, NH",
    "Host": "Dogs Makes Scents",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 43.7153,
    "Longitude": -71.4114
  },
  {
    "Date": "2025-09-27",
    "Location": "Reedsport, OR",
    "Host": "Wells Creek Dog Training",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 43.7138,
    "Longitude": -124.0681
  },
  {
    "Date": "2025-09-27",
    "Location": "Waynesboro, PA",
    "Host": "Nose-It-All, LLC",
    "TrialTypes": "L3E, NW2, ELT",
    "EventCount": 3,
    "Latitude": 39.7429,
    "Longitude": -77.5847
  },
  {
    "Date": "2025-10-03",
    "Location": "Middlebury, CT",
    "Host": "For the Love of Dogs NY LLC",
    "TrialTypes": "NW3, ELT-S, ELT",
    "EventCount": 3,
    "Latitude": 41.4957,
    "Longitude": -73.139
  },
  {
    "Date": "2025-10-04",
    "Location": "Crosslake, MN",
    "Host": "Nose 2 Tail Dog Training LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 46.6305,
    "Longitude": -94.1429
  },
  {
    "Date": "2025-10-04",
    "Location": "Nashua, NH",
    "Host": "The Big Sniff, LLC",
    "TrialTypes": "ELT, L3I, L2C",
    "EventCount": 3,
    "Latitude": 42.754,
    "Longitude": -71.4491
  },
  {
    "Date": "2025-10-04",
    "Location": "New Paltz, NY",
    "Host": "Top Notch Dogs, LLC",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 41.7569,
    "Longitude": -74.0952
  },
  {
    "Date": "2025-10-04",
    "Location": "Smithton, PA",
    "Host": "Steel City Nosework, LLC",
    "TrialTypes": "NW2, ELT",
    "EventCount": 2,
    "Latitude": 40.1689,
    "Longitude": -79.7567
  },
  {
    "Date": "2025-10-06",
    "Location": "Monterey, CA",
    "Host": "CalCoastal Dog Owners Group",
    "TrialTypes": "ELT-S, NW1, L2I, NW2",
    "EventCount": 4,
    "Latitude": 36.192,
    "Longitude": -121.4023
  },
  {
    "Date": "2025-10-10",
    "Location": "West Bend, WI",
    "Host": "Think Pawsitive Dog Training LLC",
    "TrialTypes": "ELT, L2C, L1E",
    "EventCount": 3,
    "Latitude": 43.46,
    "Longitude": -88.1357
  },
  {
    "Date": "2025-10-11",
    "Location": "Bloomington, MN",
    "Host": "St Paul Dog Training Club",
    "TrialTypes": "ELT-P, ELT-S",
    "EventCount": 2,
    "Latitude": 44.8115,
    "Longitude": -93.3054
  },
  {
    "Date": "2025-10-11",
    "Location": "Colfax, IA",
    "Host": "KBP Dog Training",
    "TrialTypes": "NW1, NW2, NW3",
    "EventCount": 3,
    "Latitude": 41.6524,
    "Longitude": -93.2915
  },
  {
    "Date": "2025-10-11",
    "Location": "Columbus, MT",
    "Host": "Canine Connection",
    "TrialTypes": "L1C, NW2, NW3",
    "EventCount": 3,
    "Latitude": 45.5889,
    "Longitude": -109.3002
  },
  {
    "Date": "2025-10-11",
    "Location": "Durham, NC",
    "Host": "Whole Dog Institute, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 35.9808,
    "Longitude": -78.9151
  },
  {
    "Date": "2025-10-11",
    "Location": "Ferndale, WA",
    "Host": "Nose Work Magic",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 48.8535,
    "Longitude": -122.6363
  },
  {
    "Date": "2025-10-11",
    "Location": "Loveland, CO",
    "Host": "Paws 4 Thought Dog Training, LLC",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 40.3576,
    "Longitude": -105.0266
  },
  {
    "Date": "2025-10-11",
    "Location": "New City , NY",
    "Host": "Saints2Source, LLC",
    "TrialTypes": "NW2, NW3, ELT",
    "EventCount": 3,
    "Latitude": 41.1998,
    "Longitude": -73.9938
  },
  {
    "Date": "2025-10-11",
    "Location": "Occidental, CA",
    "Host": "Marin Humane",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 38.3513,
    "Longitude": -122.912
  },
  {
    "Date": "2025-10-11",
    "Location": "Roseburg, OR",
    "Host": "Canine Discovery Corps",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 43.192,
    "Longitude": -123.3008
  },
  {
    "Date": "2025-10-11",
    "Location": "Sedona, AZ",
    "Host": "Successful Sniffer",
    "TrialTypes": "ELT-P, ELT, NW3",
    "EventCount": 3,
    "Latitude": 34.8718,
    "Longitude": -111.737
  },
  {
    "Date": "2025-10-17",
    "Location": "Elizabeth, CO",
    "Host": "Beyond Elevation K9",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 39.3644,
    "Longitude": -104.6476
  },
  {
    "Date": "2025-10-17",
    "Location": "Lawrenceville, GA",
    "Host": "Chestnut Hill Canine Sports",
    "TrialTypes": "NW3, L2C, NW1",
    "EventCount": 3,
    "Latitude": 33.9846,
    "Longitude": -83.9474
  },
  {
    "Date": "2025-10-17",
    "Location": "Newfoundland, PA",
    "Host": "Your Dog's Place, LLC",
    "TrialTypes": "L1V, ELT-S, L2C, L2E",
    "EventCount": 4,
    "Latitude": 41.3333,
    "Longitude": -75.366
  },
  {
    "Date": "2025-10-18",
    "Location": "Centralia, WA",
    "Host": "Rachelle Bailey-Austin/About Face K9 Academy & Dorothy Turley/Let's Talk Dogs, LLC",
    "TrialTypes": "L3I, L2C, NW2",
    "EventCount": 3,
    "Latitude": 46.7632,
    "Longitude": -122.9206
  },
  {
    "Date": "2025-10-18",
    "Location": "Delevan, NY",
    "Host": "Do Over Dog Training",
    "TrialTypes": "ELT, ELT-S, NW2",
    "EventCount": 3,
    "Latitude": 42.4533,
    "Longitude": -78.4751
  },
  {
    "Date": "2025-10-18",
    "Location": "Lafayette Hill, PA",
    "Host": "Sniff Sniff Hooray",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 40.1281,
    "Longitude": -75.2732
  },
  {
    "Date": "2025-10-18",
    "Location": "Milton, NH",
    "Host": "Lucky Dog Events",
    "TrialTypes": "NW2, NW3",
    "EventCount": 2,
    "Latitude": 43.3631,
    "Longitude": -71.0299
  },
  {
    "Date": "2025-10-18",
    "Location": "Nevada City, CA",
    "Host": "Sierra Sniffing Canines",
    "TrialTypes": "L1I, L2I, ELT",
    "EventCount": 3,
    "Latitude": 39.2652,
    "Longitude": -120.9688
  },
  {
    "Date": "2025-10-18",
    "Location": "Terryville, CT",
    "Host": "Willoughby Training",
    "TrialTypes": "NW2, NW1",
    "EventCount": 2,
    "Latitude": 41.7029,
    "Longitude": -73.0103
  },
  {
    "Date": "2025-10-18",
    "Location": "Troy, VA",
    "Host": "Your Dog Knows, LLC",
    "TrialTypes": "ELT, L1V, L2V",
    "EventCount": 3,
    "Latitude": 37.9995,
    "Longitude": -78.2547
  },
  {
    "Date": "2025-10-18",
    "Location": "Watsonville, CA",
    "Host": "CalCoastal Dog Owners Group",
    "TrialTypes": "L3V, L2V, L1V",
    "EventCount": 3,
    "Latitude": 36.933,
    "Longitude": -121.7191
  },
  {
    "Date": "2025-10-19",
    "Location": "San Martin, CA",
    "Host": "B.L. McMutts",
    "TrialTypes": "L1V, L3V",
    "EventCount": 2,
    "Latitude": 37.0528,
    "Longitude": -121.5922
  },
  {
    "Date": "2025-10-24",
    "Location": "Calhan, CO",
    "Host": "Mountain Dogs, LLC",
    "TrialTypes": "ELT, SMT",
    "EventCount": 2,
    "Latitude": 39.0161,
    "Longitude": -104.3323
  },
  {
    "Date": "2025-10-24",
    "Location": "Easton, MD",
    "Host": "Fair Play Point Labradors",
    "TrialTypes": "ELT, ELT-S, L2V, L2C, L3C",
    "EventCount": 5,
    "Latitude": 38.7526,
    "Longitude": -76.0911
  },
  {
    "Date": "2025-10-24",
    "Location": "Palmyra, VA",
    "Host": "Paws Plus Training, LLC",
    "TrialTypes": "NW3, NW1, NW2, ELT-P",
    "EventCount": 4,
    "Latitude": 37.893,
    "Longitude": -78.2531
  },
  {
    "Date": "2025-10-24",
    "Location": "Ypsilanti, MI",
    "Host": "Every Dog Nosework",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 42.2169,
    "Longitude": -83.6444
  },
  {
    "Date": "2025-10-25",
    "Location": "Auburn, WA",
    "Host": "Northwest K9 Sniffers",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 47.2744,
    "Longitude": -122.2068
  },
  {
    "Date": "2025-10-25",
    "Location": "Fishkill, NY",
    "Host": "Top Notch Dogs, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 41.5085,
    "Longitude": -73.8577
  },
  {
    "Date": "2025-10-25",
    "Location": "Lyle, WA",
    "Host": "Trust Your Dog K9 Events",
    "TrialTypes": "NW3, L3I, L3C",
    "EventCount": 3,
    "Latitude": 45.6646,
    "Longitude": -121.2455
  },
  {
    "Date": "2025-10-25",
    "Location": "Niantic, IL",
    "Host": "Kudos for Canines, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 39.8559,
    "Longitude": -89.1379
  },
  {
    "Date": "2025-10-25",
    "Location": "Poland Springs, ME",
    "Host": "Virginia Howe",
    "TrialTypes": "NW1, NW3",
    "EventCount": 2,
    "Latitude": 44.0482,
    "Longitude": -70.3402
  },
  {
    "Date": "2025-10-25",
    "Location": "West Friendship, MD",
    "Host": "Red Huskies",
    "TrialTypes": "NW3, NW1, L1C",
    "EventCount": 3,
    "Latitude": 39.2553,
    "Longitude": -76.9781
  },
  {
    "Date": "2025-10-27",
    "Location": "Clayton, GA",
    "Host": "Right Choice Dog Training LLC",
    "TrialTypes": "NW3, ELT, ELT-P",
    "EventCount": 3,
    "Latitude": 33.5212,
    "Longitude": -84.3955
  },
  {
    "Date": "2025-10-27",
    "Location": "Paicines, CA",
    "Host": "Two Nosey Girls",
    "TrialTypes": "NW2, ELT-P",
    "EventCount": 2,
    "Latitude": 36.7007,
    "Longitude": -121.2745
  },
  {
    "Date": "2025-10-31",
    "Location": "Cannon Falls, MN",
    "Host": "St Paul Dog Training Club",
    "TrialTypes": "SMT, NW3",
    "EventCount": 2,
    "Latitude": 44.5401,
    "Longitude": -92.9535
  },
  {
    "Date": "2025-10-31",
    "Location": "Loranger, LA",
    "Host": "Dog Gone Right",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 30.6794,
    "Longitude": -90.4312
  },
  {
    "Date": "2025-10-31",
    "Location": "Meeker, CO",
    "Host": "Mountain Dogs LLC",
    "TrialTypes": "ELT, NW3, NW1, NW2",
    "EventCount": 4,
    "Latitude": 40.0117,
    "Longitude": -107.9544
  },
  {
    "Date": "2025-10-31",
    "Location": "Scotts Mills, OR",
    "Host": "Canine Discovery Corps",
    "TrialTypes": "ELT, NW1, L3C",
    "EventCount": 3,
    "Latitude": 44.9939,
    "Longitude": -122.6374
  },
  {
    "Date": "2025-11-01",
    "Location": "Beloit, WI",
    "Host": "George Carpenter",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 42.5032,
    "Longitude": -89.0008
  },
  {
    "Date": "2025-11-01",
    "Location": "Charlton, MA",
    "Host": "HeavenScent Sniffers",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 42.1135,
    "Longitude": -71.9338
  },
  {
    "Date": "2025-11-01",
    "Location": "Kennebunkport, ME",
    "Host": "Elizabeth Dutton",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 43.3625,
    "Longitude": -70.4312
  },
  {
    "Date": "2025-11-01",
    "Location": "Mill Spring, NC",
    "Host": "Foothills Canine Academy, LLC",
    "TrialTypes": "NW1, NW2, ELT",
    "EventCount": 3,
    "Latitude": 35.3037,
    "Longitude": -82.1204
  },
  {
    "Date": "2025-11-01",
    "Location": "Monkton, MD",
    "Host": "Firezone GS",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 39.5606,
    "Longitude": -76.5816
  },
  {
    "Date": "2025-11-01",
    "Location": "Wappingers Falls, NY",
    "Host": "Top Notch Dogs, LLC",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 41.5747,
    "Longitude": -73.8894
  },
  {
    "Date": "2025-11-01",
    "Location": "Woodstock, GA",
    "Host": "Georgia Nosework LLC",
    "TrialTypes": "L1C, L3I, L3C, L1I",
    "EventCount": 4,
    "Latitude": 34.1047,
    "Longitude": -84.4841
  },
  {
    "Date": "2025-11-01",
    "Location": "Yamhill, OR",
    "Host": "Nose Work Detectives",
    "TrialTypes": "ELT-S, L1C",
    "EventCount": 2,
    "Latitude": 45.2465,
    "Longitude": -123.1986
  },
  {
    "Date": "2025-11-05",
    "Location": "Ventura, CA",
    "Host": "Pink Biscuit K9s",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 34.4456,
    "Longitude": -119.0756
  },
  {
    "Date": "2025-11-07",
    "Location": "Montrose, CO",
    "Host": "Mountain Dogs LLC",
    "TrialTypes": "ELT-S, NW2, ELT",
    "EventCount": 3,
    "Latitude": 38.4885,
    "Longitude": -107.8945
  },
  {
    "Date": "2025-11-07",
    "Location": "Rancho Cucamonga , CA",
    "Host": "Agile Paws Dog Sports",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 34.1136,
    "Longitude": -117.5539
  },
  {
    "Date": "2025-11-08",
    "Location": "Coburg, OR",
    "Host": "Kiddy Christie",
    "TrialTypes": "NW1, NW2, ELT-S, L1E",
    "EventCount": 4,
    "Latitude": 44.1358,
    "Longitude": -123.0955
  },
  {
    "Date": "2025-11-08",
    "Location": "Elkhorn, WI",
    "Host": "Loving Paws Dog Training LLC",
    "TrialTypes": "ELT, NW2",
    "EventCount": 2,
    "Latitude": 42.6564,
    "Longitude": -88.5088
  },
  {
    "Date": "2025-11-08",
    "Location": "Guerneville, CA",
    "Host": "Jen Huot",
    "TrialTypes": "NW3, ELT, ELT-P",
    "EventCount": 3,
    "Latitude": 38.4844,
    "Longitude": -122.9589
  },
  {
    "Date": "2025-11-08",
    "Location": "Mays Landing, NJ",
    "Host": "Rotts-n-Notts Nosework, LLC",
    "TrialTypes": "NW3, L2E, NW2",
    "EventCount": 3,
    "Latitude": 39.4671,
    "Longitude": -74.7454
  },
  {
    "Date": "2025-11-08",
    "Location": "Pine Grove, PA",
    "Host": "Paws n' Sniff",
    "TrialTypes": "ELT-P, NW3",
    "EventCount": 2,
    "Latitude": 40.5493,
    "Longitude": -76.3503
  },
  {
    "Date": "2025-11-08",
    "Location": "Tucson, AZ",
    "Host": "Patience Unlimited Professional Dog Training",
    "TrialTypes": "NW3, L2C, NW2",
    "EventCount": 3,
    "Latitude": 32.2513,
    "Longitude": -110.9632
  },
  {
    "Date": "2025-11-11",
    "Location": "Guerneville, CA",
    "Host": "Jen Huot",
    "TrialTypes": "ELT-P",
    "EventCount": 1,
    "Latitude": 38.5087,
    "Longitude": -123.0095
  },
  {
    "Date": "2025-11-12",
    "Location": "Astoria, OR",
    "Host": "Nose Work Detectives, LLC",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 46.1398,
    "Longitude": -123.8117
  },
  {
    "Date": "2025-11-14",
    "Location": "Elgin, IL",
    "Host": "Common Scents K9",
    "TrialTypes": "L1C, L2C, L1I, L2I, NW1",
    "EventCount": 5,
    "Latitude": 42.0746,
    "Longitude": -88.2566
  },
  {
    "Date": "2025-11-14",
    "Location": "New Freedom, PA",
    "Host": "Firezone GS",
    "TrialTypes": "ELT, NW3, ELT-S, NW2",
    "EventCount": 4,
    "Latitude": 39.7766,
    "Longitude": -76.7089
  },
  {
    "Date": "2025-11-15",
    "Location": "Albuquerque, NM",
    "Host": "New Mexico Canine Scent Work, LLC",
    "TrialTypes": "NW3, L1C, NW2",
    "EventCount": 3,
    "Latitude": 35.1159,
    "Longitude": -106.6319
  },
  {
    "Date": "2025-11-15",
    "Location": "Bonham, TX",
    "Host": "All About The Nose",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 33.5283,
    "Longitude": -96.1934
  },
  {
    "Date": "2025-11-15",
    "Location": "Bradenton, FL",
    "Host": "Hoppin' in the Hills",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 27.4812,
    "Longitude": -82.5369
  },
  {
    "Date": "2025-11-15",
    "Location": "Eldred, NY",
    "Host": "Your Dog's Place, LLC",
    "TrialTypes": "L1I, NW2, L3I, L3C",
    "EventCount": 4,
    "Latitude": 41.5432,
    "Longitude": -74.9125
  },
  {
    "Date": "2025-11-15",
    "Location": "Marbury, AL",
    "Host": "Kaye Stevenson",
    "TrialTypes": "NW2, NW1, L1E",
    "EventCount": 3,
    "Latitude": 32.6801,
    "Longitude": -86.4277
  },
  {
    "Date": "2025-11-15",
    "Location": "Montgomery, AL",
    "Host": "By A Nose Nosework",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 32.3429,
    "Longitude": -86.2988
  },
  {
    "Date": "2025-11-15",
    "Location": "Welches, OR",
    "Host": "Trust Your Dog K9 Events",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 45.3526,
    "Longitude": -122.0114
  },
  {
    "Date": "2025-11-17",
    "Location": "Hudson, MA",
    "Host": "For the Love of Dogs NY LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 42.4064,
    "Longitude": -71.5688
  },
  {
    "Date": "2025-11-21",
    "Location": "Harrington, DE",
    "Host": "Shamrock Pot Of Gold K9 Scenter",
    "TrialTypes": "NW3, ELT-S, NW2, NW1, L1C, ELT",
    "EventCount": 6,
    "Latitude": 38.9085,
    "Longitude": -75.5805
  },
  {
    "Date": "2025-11-21",
    "Location": "San Luis Obispo, CA",
    "Host": "Gentle Touch Pet Training",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 35.312,
    "Longitude": -120.3273
  },
  {
    "Date": "2025-11-22",
    "Location": "Crownsville, MD",
    "Host": "Red Huskies",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 39.0638,
    "Longitude": -76.6002
  },
  {
    "Date": "2025-11-22",
    "Location": "Defuniak Springs, FL",
    "Host": "Linda Culliton",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 30.6972,
    "Longitude": -86.0829
  },
  {
    "Date": "2025-11-22",
    "Location": "Delta, CO",
    "Host": "Mountain Dogs, LLC",
    "TrialTypes": "ELT, NW3, ELT-S, NW2",
    "EventCount": 4,
    "Latitude": 38.8037,
    "Longitude": -107.827
  },
  {
    "Date": "2025-11-22",
    "Location": "Foxborough , MA",
    "Host": "MasterPeace Dog Training",
    "TrialTypes": "L3C, NW1, NW2",
    "EventCount": 3,
    "Latitude": 42.0196,
    "Longitude": -71.2914
  },
  {
    "Date": "2025-11-22",
    "Location": "Kintnersville, PA",
    "Host": "Paws n' Sniff",
    "TrialTypes": "L1C, NW1, L2I, L2E",
    "EventCount": 4,
    "Latitude": 40.5998,
    "Longitude": -75.2043
  },
  {
    "Date": "2025-11-22",
    "Location": "Marble Falls, TX",
    "Host": "Scent Work Across Texas",
    "TrialTypes": "ELT-S, NW2, NW1, L1I",
    "EventCount": 4,
    "Latitude": 30.5991,
    "Longitude": -98.2392
  },
  {
    "Date": "2025-11-22",
    "Location": "Medford, NJ",
    "Host": "K9 InScentives",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 39.8941,
    "Longitude": -74.8558
  },
  {
    "Date": "2025-11-22",
    "Location": "Norton, MA",
    "Host": "Dogs Make Scents",
    "TrialTypes": "ELT, L1E, L1C",
    "EventCount": 3,
    "Latitude": 41.927,
    "Longitude": -71.211
  },
  {
    "Date": "2025-11-22",
    "Location": "Salem Lakes, WI",
    "Host": "Loving Paws Dog Training, LLC",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 42.5749,
    "Longitude": -88.1249
  },
  {
    "Date": "2025-11-22",
    "Location": "Smyrna, TN",
    "Host": "Dogs Have Amazing Noses, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 35.9405,
    "Longitude": -86.5232
  },
  {
    "Date": "2025-11-28",
    "Location": "Dana Point, CA",
    "Host": "JavaK9s, LLC",
    "TrialTypes": "NW3, ELT-S",
    "EventCount": 2,
    "Latitude": 33.457,
    "Longitude": -117.7217
  },
  {
    "Date": "2025-11-28",
    "Location": "San Jose, CA",
    "Host": "The Bay Team",
    "TrialTypes": "NW2, NW1",
    "EventCount": 2,
    "Latitude": 37.384,
    "Longitude": -121.8726
  },
  {
    "Date": "2025-11-29",
    "Location": "Alpharetta, GA",
    "Host": "Georgia Nosework",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 34.0805,
    "Longitude": -84.335
  },
  {
    "Date": "2025-11-29",
    "Location": "Canandaigua, NY",
    "Host": "Savvy Dog Sports",
    "TrialTypes": "ELT, NW2",
    "EventCount": 2,
    "Latitude": 42.809,
    "Longitude": -77.303
  },
  {
    "Date": "2025-11-29",
    "Location": "Cottage Grove, MN",
    "Host": "Gretchen Hofheins-Wackerfuss",
    "TrialTypes": "ELT-S, ELT-P",
    "EventCount": 2,
    "Latitude": 44.7797,
    "Longitude": -92.9787
  },
  {
    "Date": "2025-11-29",
    "Location": "Lebanon, NJ",
    "Host": "Sirius K9 Solutions",
    "TrialTypes": "NW3, L3I, ELT-S",
    "EventCount": 3,
    "Latitude": 40.6871,
    "Longitude": -74.8021
  },
  {
    "Date": "2025-12-05",
    "Location": "Bowie, MD",
    "Host": "Red Huskies",
    "TrialTypes": "ELT, ELT-P, ELT-S",
    "EventCount": 3,
    "Latitude": 38.9306,
    "Longitude": -76.6807
  },
  {
    "Date": "2025-12-06",
    "Location": "Annapolis, MD",
    "Host": "Chesapeake Search Dogs",
    "TrialTypes": "NW3, NW2, L2C",
    "EventCount": 3,
    "Latitude": 38.975,
    "Longitude": -76.499
  },
  {
    "Date": "2025-12-06",
    "Location": "Auburn, WA",
    "Host": "Northwest K9 Sniffers",
    "TrialTypes": "L1C, L2C, NW3",
    "EventCount": 3,
    "Latitude": 47.2923,
    "Longitude": -122.2427
  },
  {
    "Date": "2025-12-06",
    "Location": "Centralia, WA",
    "Host": "About Face K9 Academy and Let's Talk Dogs",
    "TrialTypes": "NW3, NW1",
    "EventCount": 2,
    "Latitude": 46.7063,
    "Longitude": -122.9406
  },
  {
    "Date": "2025-12-06",
    "Location": "Fillmore, CA",
    "Host": "Pink Biscuit K9s",
    "TrialTypes": "NW1, NW2, ELT",
    "EventCount": 3,
    "Latitude": 34.4281,
    "Longitude": -118.9223
  },
  {
    "Date": "2025-12-06",
    "Location": "Hoover, AL",
    "Host": "Southeast Scent Work Alliance, LLC",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 33.3707,
    "Longitude": -86.8584
  },
  {
    "Date": "2025-12-06",
    "Location": "Kittanning, PA",
    "Host": "Steel City Nosework, LLC",
    "TrialTypes": "NW3, ELT-S, NW1",
    "EventCount": 3,
    "Latitude": 40.847,
    "Longitude": -79.4956
  },
  {
    "Date": "2025-12-06",
    "Location": "Newfoundland, PA",
    "Host": "Your Dog's Place, LLC",
    "TrialTypes": "L2I, L3V, NW3",
    "EventCount": 3,
    "Latitude": 41.3196,
    "Longitude": -75.3675
  },
  {
    "Date": "2025-12-07",
    "Location": "Cape Coral, FL",
    "Host": "Your Dog Knows, LLC",
    "TrialTypes": "NW1",
    "EventCount": 1,
    "Latitude": 26.5171,
    "Longitude": -81.8951
  },
  {
    "Date": "2025-12-12",
    "Location": "Douglassville, PA",
    "Host": "Patricia Grassey",
    "TrialTypes": "NW3, ELT, ELT-P",
    "EventCount": 3,
    "Latitude": 40.3059,
    "Longitude": -75.7116
  },
  {
    "Date": "2025-12-12",
    "Location": "Pittstown, NJ",
    "Host": "Shamrock Pot Of Gold K9 Scenter",
    "TrialTypes": "NW3, ELT, ELT-S, NW1",
    "EventCount": 4,
    "Latitude": 40.5591,
    "Longitude": -74.9489
  },
  {
    "Date": "2025-12-13",
    "Location": "DeLeon Springs, FL",
    "Host": "River Poodles Training, LLC",
    "TrialTypes": "ELT-P, ELT-S, L2C",
    "EventCount": 3,
    "Latitude": 29.0892,
    "Longitude": -81.3207
  },
  {
    "Date": "2025-12-13",
    "Location": "Easton, MA",
    "Host": "South Coast Scent Dogs",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 41.9902,
    "Longitude": -71.1649
  },
  {
    "Date": "2025-12-13",
    "Location": "Escondido, CA",
    "Host": "Uber Dog and Rewarding Rover LLC",
    "TrialTypes": "NW2",
    "EventCount": 1,
    "Latitude": 33.1229,
    "Longitude": -117.1123
  },
  {
    "Date": "2025-12-13",
    "Location": "Greer, SC",
    "Host": "Trained to Trust LLC",
    "TrialTypes": "ELT, ELT-S, NW2",
    "EventCount": 3,
    "Latitude": 34.9862,
    "Longitude": -82.2698
  },
  {
    "Date": "2025-12-13",
    "Location": "Independence, OR",
    "Host": "Doglandia, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 44.8103,
    "Longitude": -123.1828
  },
  {
    "Date": "2025-12-13",
    "Location": "Westminster, MD",
    "Host": "Red Huskies",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 39.5799,
    "Longitude": -76.9699
  },
  {
    "Date": "2025-12-16",
    "Location": "Duluth, GA",
    "Host": "Right Choice Dog Training LLC",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 34.0182,
    "Longitude": -84.1458
  },
  {
    "Date": "2025-12-20",
    "Location": "Cumming, GA",
    "Host": "Georgia Nosework LLC",
    "TrialTypes": "SMT, ELT",
    "EventCount": 2,
    "Latitude": 34.1814,
    "Longitude": -84.1642
  },
  {
    "Date": "2025-12-20",
    "Location": "Florissant, MO",
    "Host": "Happy Dog Concepts, LLC",
    "TrialTypes": "ELT-P",
    "EventCount": 1,
    "Latitude": 38.788,
    "Longitude": -90.3559
  },
  {
    "Date": "2025-12-20",
    "Location": "Salem, OR",
    "Host": "Helix Fairweather & Doglandia, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 44.9218,
    "Longitude": -122.9997
  },
  {
    "Date": "2025-12-20",
    "Location": "Silex, MO",
    "Host": "WestInn Kennels",
    "TrialTypes": "NW1, NW2, NW3",
    "EventCount": 3,
    "Latitude": 39.0973,
    "Longitude": -91.0375
  },
  {
    "Date": "2025-12-20",
    "Location": "Stockton, CA",
    "Host": "Two Nosey Girls",
    "TrialTypes": "NW3, NW1",
    "EventCount": 2,
    "Latitude": 37.9733,
    "Longitude": -121.293
  },
  {
    "Date": "2025-12-27",
    "Location": "Auburn, AL",
    "Host": "Daphne Melillo",
    "TrialTypes": "NW2, NW1",
    "EventCount": 2,
    "Latitude": 32.5796,
    "Longitude": -85.5084
  },
  {
    "Date": "2025-12-27",
    "Location": "Fort Morgan, CO",
    "Host": "Paws 4 Thought Dog Training, LLC",
    "TrialTypes": "L1V, NW2, NW1, L1I",
    "EventCount": 4,
    "Latitude": 40.2147,
    "Longitude": -103.8288
  },
  {
    "Date": "2025-12-27",
    "Location": "New Rochelle, NY",
    "Host": "For the Love of Dogs NY LLC",
    "TrialTypes": "NW3, ELT-S, ELT",
    "EventCount": 3,
    "Latitude": 40.8898,
    "Longitude": -73.8041
  },
  {
    "Date": "2025-12-27",
    "Location": "White Plains, NY",
    "Host": "Saints2Source",
    "TrialTypes": "NW1, NW2, L2E, L2C",
    "EventCount": 4,
    "Latitude": 40.988,
    "Longitude": -73.7546
  },
  {
    "Date": "2025-12-28",
    "Location": "Exton, PA",
    "Host": "Patricia Grassey",
    "TrialTypes": "ELT, ELT-P, NW3",
    "EventCount": 3,
    "Latitude": 39.9801,
    "Longitude": -75.6007
  },
  {
    "Date": "2025-12-28",
    "Location": "Waukesha, WI",
    "Host": "Think Pawsitive Dog Training",
    "TrialTypes": "NW2, NW1",
    "EventCount": 2,
    "Latitude": 43.0136,
    "Longitude": -88.3159
  },
  {
    "Date": "2025-12-29",
    "Location": "Barrington, RI",
    "Host": "Bay State Sniffers",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 41.77,
    "Longitude": -71.3024
  },
  {
    "Date": "2026-01-02",
    "Location": "Emmitsburg , MD",
    "Host": "Red Huskies",
    "TrialTypes": "NW3, ELT, ELT-S",
    "EventCount": 3,
    "Latitude": 39.7248,
    "Longitude": -77.293
  },
  {
    "Date": "2026-01-03",
    "Location": "Bonsall, CA",
    "Host": "Linda Buchanan",
    "TrialTypes": "NW3, ELT-P",
    "EventCount": 2,
    "Latitude": 33.2516,
    "Longitude": -117.2309
  },
  {
    "Date": "2026-01-03",
    "Location": "Green Cove Springs, FL",
    "Host": "Hoppin' in the Hills",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 29.9895,
    "Longitude": -81.7102
  },
  {
    "Date": "2026-01-03",
    "Location": "Maryville, TN",
    "Host": "Rachel Hawkins",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 35.7709,
    "Longitude": -83.9968
  },
  {
    "Date": "2026-01-03",
    "Location": "Montevallo, AL",
    "Host": "Southeast Scent Work Alliance, LLC",
    "TrialTypes": "NW1",
    "EventCount": 1,
    "Latitude": 33.1029,
    "Longitude": -86.8652
  },
  {
    "Date": "2026-01-09",
    "Location": "Hartfield, VA",
    "Host": "Paws Plus Training, LLC",
    "TrialTypes": "NW3, NW1, NW2, ELT",
    "EventCount": 4,
    "Latitude": 37.5602,
    "Longitude": -76.4547
  },
  {
    "Date": "2026-01-09",
    "Location": "Spring City, PA",
    "Host": "Patricia Grassey",
    "TrialTypes": "NW3, NW2, NW1, L2I",
    "EventCount": 4,
    "Latitude": 40.1394,
    "Longitude": -75.5082
  },
  {
    "Date": "2026-01-10",
    "Location": "Canton , GA",
    "Host": "Run Spot Jump Dog Training",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 34.2023,
    "Longitude": -84.4856
  },
  {
    "Date": "2026-01-10",
    "Location": "Pflugerville, TX",
    "Host": "Scent Work Across Texas",
    "TrialTypes": "ELT-S, L2C, NW3",
    "EventCount": 3,
    "Latitude": 30.4412,
    "Longitude": -97.5815
  },
  {
    "Date": "2026-01-10",
    "Location": "Valencia, CA",
    "Host": "Pink Biscuit K9s",
    "TrialTypes": "NW1, NW2, ELT",
    "EventCount": 3,
    "Latitude": 34.4533,
    "Longitude": -118.5847
  },
  {
    "Date": "2026-01-16",
    "Location": "Bristol, PA",
    "Host": "Patricia Grassey",
    "TrialTypes": "ELT, ELT-P, ELT-S, NW2",
    "EventCount": 4,
    "Latitude": 40.112,
    "Longitude": -74.8783
  },
  {
    "Date": "2026-01-17",
    "Location": "Clanton, AL",
    "Host": "By A Nose Nosework",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 32.7927,
    "Longitude": -86.6264
  },
  {
    "Date": "2026-01-17",
    "Location": "Melrose, FL",
    "Host": "River Poodles Training, LLC",
    "TrialTypes": "NW1, NW2, L1V, L1E",
    "EventCount": 4,
    "Latitude": 29.7029,
    "Longitude": -82.0743
  },
  {
    "Date": "2026-01-17",
    "Location": "New Rochelle, NY",
    "Host": "For the Love of Dogs NY LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 40.8996,
    "Longitude": -73.735
  },
  {
    "Date": "2026-01-17",
    "Location": "San Marcos, CA",
    "Host": "Rewarding Rover LLC and Uberdog",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 33.0912,
    "Longitude": -117.133
  },
  {
    "Date": "2026-01-17",
    "Location": "Tecumseh, OK",
    "Host": "The Doggie Spot, LLC",
    "TrialTypes": "ELT, NW3, NW2",
    "EventCount": 3,
    "Latitude": 35.2823,
    "Longitude": -96.9423
  },
  {
    "Date": "2026-01-19",
    "Location": "Redlands, CA",
    "Host": "Agile Paws Dog Sports",
    "TrialTypes": "NW3, ELT-P",
    "EventCount": 2,
    "Latitude": 34.0138,
    "Longitude": -117.1791
  },
  {
    "Date": "2026-01-20",
    "Location": "Murfreesboro, TN",
    "Host": "Dogs Have Amazing Noses, LLC",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 35.8423,
    "Longitude": -86.4307
  },
  {
    "Date": "2026-01-23",
    "Location": "Rome, GA",
    "Host": "Georgia Nosework, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 34.2378,
    "Longitude": -85.1606
  },
  {
    "Date": "2026-01-30",
    "Location": "Greeley, CO",
    "Host": "Beyond Elevation K9 Training",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 40.4201,
    "Longitude": -104.6891
  },
  {
    "Date": "2026-01-31",
    "Location": "Denton, MD",
    "Host": "Fair Play Labradors",
    "TrialTypes": "ELT-S, L2C, NW1, L1E",
    "EventCount": 4,
    "Latitude": 38.8953,
    "Longitude": -75.8082
  },
  {
    "Date": "2026-01-31",
    "Location": "Tucson, AZ",
    "Host": "Patience Unlimited Professional Dog Training",
    "TrialTypes": "NW3, ELT-S, L1V",
    "EventCount": 3,
    "Latitude": 32.2213,
    "Longitude": -110.9996
  },
  {
    "Date": "2026-02-07",
    "Location": "Lakewood, NJ",
    "Host": "Rotts n Notts Nosework",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 40.0804,
    "Longitude": -74.1835
  },
  {
    "Date": "2026-02-07",
    "Location": "Murfreesboro, TN",
    "Host": "Dogs Have Amazing Noses, LLC",
    "TrialTypes": "ELT, NW1",
    "EventCount": 2,
    "Latitude": 35.8089,
    "Longitude": -86.3424
  },
  {
    "Date": "2026-02-13",
    "Location": "Havre De Grace, MD",
    "Host": "Firezone GS",
    "TrialTypes": "ELT-P, NW3, L3I, NW2",
    "EventCount": 4,
    "Latitude": 39.5634,
    "Longitude": -76.0809
  },
  {
    "Date": "2026-02-13",
    "Location": "Vista, CA",
    "Host": "Rewarding Rover LLC and Uberdog",
    "TrialTypes": "ELT, L1C, L2C",
    "EventCount": 3,
    "Latitude": 33.2497,
    "Longitude": -117.2491
  },
  {
    "Date": "2026-02-14",
    "Location": "Clarkesville, GA",
    "Host": "Right Choice Dog Training, LLC",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 34.632,
    "Longitude": -83.4961
  },
  {
    "Date": "2026-02-14",
    "Location": "Colesville, MD",
    "Host": "Red Huskies",
    "TrialTypes": "L1C, NW1, ELT-S, ELT",
    "EventCount": 4,
    "Latitude": 39.0597,
    "Longitude": -77.0466
  },
  {
    "Date": "2026-02-14",
    "Location": "Flemington, NJ",
    "Host": "Shamrock Pot Of Gold K9 Scenter",
    "TrialTypes": "ELT, ELT-S, NW1",
    "EventCount": 3,
    "Latitude": 40.518,
    "Longitude": -74.9085
  },
  {
    "Date": "2026-02-14",
    "Location": "Northridge, CA",
    "Host": "SCENTwork.org",
    "TrialTypes": "NW2, ELT",
    "EventCount": 2,
    "Latitude": 34.239,
    "Longitude": -118.499
  },
  {
    "Date": "2026-02-14",
    "Location": "Pottsboro, TX",
    "Host": "All About The Nose",
    "TrialTypes": "NW3, NW2, NW1",
    "EventCount": 3,
    "Latitude": 33.7307,
    "Longitude": -96.6353
  },
  {
    "Date": "2026-02-14",
    "Location": "Strafford, PA",
    "Host": "The Sniffing Hound",
    "TrialTypes": "NW3, ELT, ELT-P",
    "EventCount": 3,
    "Latitude": 40.012,
    "Longitude": -75.4051
  },
  {
    "Date": "2026-02-15",
    "Location": "Chino, CA",
    "Host": "Agile Paws Dog Sports",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 34.0169,
    "Longitude": -117.6969
  },
  {
    "Date": "2026-02-15",
    "Location": "New Rochelle, NY",
    "Host": "For the Love of Dogs NY LLC",
    "TrialTypes": "ELT-P, NW3, ELT",
    "EventCount": 3,
    "Latitude": 40.942,
    "Longitude": -73.8158
  },
  {
    "Date": "2026-02-20",
    "Location": "San Rafael/Novato, CA",
    "Host": "Marin Humane",
    "TrialTypes": "L2C, L1I, NW3",
    "EventCount": 3,
    "Latitude": 38.0691,
    "Longitude": -122.4059
  },
  {
    "Date": "2026-02-21",
    "Location": "Clearwater, FL",
    "Host": "Hoppin' in the Hills",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 27.9348,
    "Longitude": -82.786
  },
  {
    "Date": "2026-02-21",
    "Location": "Veneta, OR",
    "Host": "Wells Creek Dog Training",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 44.0681,
    "Longitude": -123.3895
  },
  {
    "Date": "2026-02-22",
    "Location": "Benson, AZ",
    "Host": "Patience Unlimited Professional Dog Training",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 32.0002,
    "Longitude": -110.2706
  },
  {
    "Date": "2026-02-24",
    "Location": "Paso Robles, CA",
    "Host": "Gentle Touch Pet Training",
    "TrialTypes": "ELT-S, L3I",
    "EventCount": 2,
    "Latitude": 35.6614,
    "Longitude": -120.6434
  },
  {
    "Date": "2026-02-28",
    "Location": "Danielsville, GA",
    "Host": "Right Choice Dog Training, LLC",
    "TrialTypes": "NW1, L2I, NW2",
    "EventCount": 3,
    "Latitude": 34.1312,
    "Longitude": -83.2395
  },
  {
    "Date": "2026-02-28",
    "Location": "Keystone Heights, FL",
    "Host": "River Poodles Training, LLC",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 29.746,
    "Longitude": -82.0703
  },
  {
    "Date": "2026-02-28",
    "Location": "Lutherville, MD",
    "Host": "Red Huskies",
    "TrialTypes": "L3C, L1I, NW2",
    "EventCount": 3,
    "Latitude": 39.4485,
    "Longitude": -76.6449
  },
  {
    "Date": "2026-02-28",
    "Location": "Tygh Valley, OR",
    "Host": "Nose Work Detectives, LLC",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 45.2052,
    "Longitude": -121.1706
  },
  {
    "Date": "2026-02-28",
    "Location": "Wilson, NC",
    "Host": "Whole Dog Institute, LLC",
    "TrialTypes": "ELT, ELT-S, NW1",
    "EventCount": 3,
    "Latitude": 35.6993,
    "Longitude": -77.8834
  },
  {
    "Date": "2026-03-06",
    "Location": "Chesterfield, VA",
    "Host": "Paws Plus Training, LLC",
    "TrialTypes": "NW3, NW1, NW2, ELT",
    "EventCount": 4,
    "Latitude": 37.431,
    "Longitude": -77.5386
  },
  {
    "Date": "2026-03-06",
    "Location": "Elgin, IL",
    "Host": "For Your K9, Inc",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 42.0805,
    "Longitude": -88.3111
  },
  {
    "Date": "2026-03-06",
    "Location": "Westlake Village, CA",
    "Host": "JavaK9s, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 34.1367,
    "Longitude": -118.823
  },
  {
    "Date": "2026-03-07",
    "Location": "Cumming, GA",
    "Host": "Georgia Nosework LLC",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 34.2265,
    "Longitude": -84.1069
  },
  {
    "Date": "2026-03-07",
    "Location": "Honesdale, PA",
    "Host": "Your Dog's Place, LLC",
    "TrialTypes": "L3C, ELT, ELT-S, L3I",
    "EventCount": 4,
    "Latitude": 41.6173,
    "Longitude": -75.242
  },
  {
    "Date": "2026-03-07",
    "Location": "Moriarty, NM",
    "Host": "New Mexico Canine Scent Work, LLC",
    "TrialTypes": "NW3, L1I, NW1",
    "EventCount": 3,
    "Latitude": 34.9581,
    "Longitude": -106.0267
  },
  {
    "Date": "2026-03-07",
    "Location": "Warrensburg, IL",
    "Host": "Kudos for Canines",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 39.9541,
    "Longitude": -89.0168
  },
  {
    "Date": "2026-03-13",
    "Location": "Centreville , MD",
    "Host": "Fair Play Point Labradors",
    "TrialTypes": "SMT, L2C, L3C",
    "EventCount": 3,
    "Latitude": 39.0534,
    "Longitude": -76.105
  },
  {
    "Date": "2026-03-13",
    "Location": "Colebrook, CT",
    "Host": "For the Love of Dogs NY LLC",
    "TrialTypes": "NW3, SMT",
    "EventCount": 2,
    "Latitude": 41.9922,
    "Longitude": -73.0665
  },
  {
    "Date": "2026-03-13",
    "Location": "Stokesdale, NC",
    "Host": "K9 Nose Adventures, LLC",
    "TrialTypes": "ELT, NW3, NW1, NW2",
    "EventCount": 4,
    "Latitude": 36.2694,
    "Longitude": -79.9969
  },
  {
    "Date": "2026-03-14",
    "Location": "Channahon, IL",
    "Host": "4G & TB",
    "TrialTypes": "NW2, NW1",
    "EventCount": 2,
    "Latitude": 41.3807,
    "Longitude": -88.1983
  },
  {
    "Date": "2026-03-14",
    "Location": "Hammond, LA",
    "Host": "Dog Gone Right",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 30.4585,
    "Longitude": -90.4494
  },
  {
    "Date": "2026-03-14",
    "Location": "Kent, WA",
    "Host": "K9 Sniffers",
    "TrialTypes": "ELT, L1V, L1E",
    "EventCount": 3,
    "Latitude": 47.3463,
    "Longitude": -122.1875
  },
  {
    "Date": "2026-03-14",
    "Location": "Phoenix, AZ",
    "Host": "Release Canine, LLC",
    "TrialTypes": "ELT, NW1",
    "EventCount": 2,
    "Latitude": 33.4142,
    "Longitude": -112.0618
  },
  {
    "Date": "2026-03-14",
    "Location": "Santa Paula, CA",
    "Host": "Pink Biscuit K9s",
    "TrialTypes": "NW3, NW1, ELT-S",
    "EventCount": 3,
    "Latitude": 34.3451,
    "Longitude": -119.0233
  },
  {
    "Date": "2026-03-16",
    "Location": "Paso Robles, CA",
    "Host": "Central Coast Nosework Club",
    "TrialTypes": "NW3, ELT-S, L3V",
    "EventCount": 3,
    "Latitude": 35.6411,
    "Longitude": -120.7019
  },
  {
    "Date": "2026-03-20",
    "Location": "Shady Hills, FL",
    "Host": "Hoppin' in the Hills",
    "TrialTypes": "L1C, NW1, NW2",
    "EventCount": 3,
    "Latitude": 28.3881,
    "Longitude": -82.5251
  },
  {
    "Date": "2026-03-20",
    "Location": "Street, MD",
    "Host": "Firezone GS",
    "TrialTypes": "ELT-P, NW3, ELT-S, NW2",
    "EventCount": 4,
    "Latitude": 39.6719,
    "Longitude": -76.3847
  },
  {
    "Date": "2026-03-21",
    "Location": "Califon, NJ",
    "Host": "Shamrock Pot Of Gold K9 Scenter",
    "TrialTypes": "NW2, ELT-S, ELT",
    "EventCount": 3,
    "Latitude": 40.7468,
    "Longitude": -74.817
  },
  {
    "Date": "2026-03-21",
    "Location": "Dittmer, MO",
    "Host": "Happy Dog Concepts LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 38.3694,
    "Longitude": -90.7011
  },
  {
    "Date": "2026-03-21",
    "Location": "East Windsor, CT",
    "Host": "Lucky Dog Events",
    "TrialTypes": "ELT, L2C, NW2",
    "EventCount": 3,
    "Latitude": 41.8748,
    "Longitude": -72.6605
  },
  {
    "Date": "2026-03-21",
    "Location": "Elkridge, MD",
    "Host": "Red Huskies",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 39.2016,
    "Longitude": -76.7029
  },
  {
    "Date": "2026-03-21",
    "Location": "Foxboro, MA",
    "Host": "Bay State Sniffers",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 42.0554,
    "Longitude": -71.2132
  },
  {
    "Date": "2026-03-21",
    "Location": "Lawrenceville, GA",
    "Host": "Right Choice Dog Training LLC",
    "TrialTypes": "ELT-P, ELT",
    "EventCount": 2,
    "Latitude": 33.9903,
    "Longitude": -83.9779
  },
  {
    "Date": "2026-03-21",
    "Location": "Oakville, WA",
    "Host": "About Face K9 Academy & Let's Talk Dogs, LLC",
    "TrialTypes": "NW2, ELT",
    "EventCount": 2,
    "Latitude": 46.8181,
    "Longitude": -123.189
  },
  {
    "Date": "2026-03-21",
    "Location": "Redwood City, CA",
    "Host": "B.L. McMutts LLC",
    "TrialTypes": "ELT-S, L1I, L3C",
    "EventCount": 3,
    "Latitude": 37.5361,
    "Longitude": -122.239
  },
  {
    "Date": "2026-03-21",
    "Location": "Salem Lakes, WI",
    "Host": "Loving Paws Dog Training LLC",
    "TrialTypes": "NW2, L2I, ELT-S",
    "EventCount": 3,
    "Latitude": 42.5304,
    "Longitude": -88.1385
  },
  {
    "Date": "2026-03-21",
    "Location": "Winterset, IA",
    "Host": "KBP Dog Training",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 41.3603,
    "Longitude": -94.0274
  },
  {
    "Date": "2026-03-23",
    "Location": "Riverside, CA",
    "Host": "Linda Buchanan",
    "TrialTypes": "NW2, NW3",
    "EventCount": 2,
    "Latitude": 33.9389,
    "Longitude": -117.4028
  },
  {
    "Date": "2026-03-27",
    "Location": "Grand Junction, CO",
    "Host": "Mountain Dogs, LLC",
    "TrialTypes": "ELT-P, ELT-S, NW1, NW2",
    "EventCount": 4,
    "Latitude": 39.0288,
    "Longitude": -108.5955
  },
  {
    "Date": "2026-03-27",
    "Location": "Kennett Square, PA",
    "Host": "The Sniffing Hound",
    "TrialTypes": "NW3, ELT, ELT-S, L3I",
    "EventCount": 4,
    "Latitude": 39.798,
    "Longitude": -75.7259
  },
  {
    "Date": "2026-03-27",
    "Location": "Salem, OR",
    "Host": "Just Nose Work & Doglandia LLC",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 44.9032,
    "Longitude": -123.0588
  },
  {
    "Date": "2026-03-27",
    "Location": "Watertown, TN",
    "Host": "Dogs Have Amazing Noses, LLC",
    "TrialTypes": "ELT, NW2",
    "EventCount": 2,
    "Latitude": 36.0801,
    "Longitude": -86.1052
  },
  {
    "Date": "2026-03-28",
    "Location": "Batavia, OH",
    "Host": "Clermont County Dog Training Club",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 39.0993,
    "Longitude": -84.2065
  },
  {
    "Date": "2026-03-28",
    "Location": "Colorado Springs, CO",
    "Host": "Beyond Elevation K9",
    "TrialTypes": "NW2, NW3",
    "EventCount": 2,
    "Latitude": 38.807,
    "Longitude": -104.8238
  },
  {
    "Date": "2026-03-28",
    "Location": "Forks, WA",
    "Host": "Sea Change Canine LLC",
    "TrialTypes": "NW2, NW3",
    "EventCount": 2,
    "Latitude": 47.9367,
    "Longitude": -124.4237
  },
  {
    "Date": "2026-03-29",
    "Location": "Canton, GA",
    "Host": "Run Spot Jump Dog Training",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 34.2724,
    "Longitude": -84.4733
  },
  {
    "Date": "2026-03-30",
    "Location": "Watsonville, CA",
    "Host": "CalCoastal Dog Owners Group",
    "TrialTypes": "NW2, NW1",
    "EventCount": 2,
    "Latitude": 36.9133,
    "Longitude": -121.7951
  },
  {
    "Date": "2026-04-03",
    "Location": "Eagan, MN",
    "Host": "St. Paul Dog Training Center",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 44.8473,
    "Longitude": -93.1739
  },
  {
    "Date": "2026-04-03",
    "Location": "Rochester, NY",
    "Host": "2 Psyched 4 dogs",
    "TrialTypes": "NW3, L1I, L1C",
    "EventCount": 3,
    "Latitude": 43.1355,
    "Longitude": -77.5659
  },
  {
    "Date": "2026-04-03",
    "Location": "Warwick, NY",
    "Host": "For the Love of Dogs NY LLC",
    "TrialTypes": "ELT, NW3, NW1",
    "EventCount": 3,
    "Latitude": 41.2593,
    "Longitude": -74.3438
  },
  {
    "Date": "2026-04-04",
    "Location": "Blaine, WA",
    "Host": "The Nosework Magic",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 48.9659,
    "Longitude": -122.7146
  },
  {
    "Date": "2026-04-04",
    "Location": "Burnet, TX",
    "Host": "Scent Work Across Texas",
    "TrialTypes": "L1C, NW2, ELT",
    "EventCount": 3,
    "Latitude": 30.768,
    "Longitude": -98.1754
  },
  {
    "Date": "2026-04-04",
    "Location": "Stayton, OR",
    "Host": "Canine Discovery Corps",
    "TrialTypes": "L2E, L1V, L1E, L2C",
    "EventCount": 4,
    "Latitude": 44.7536,
    "Longitude": -122.7757
  },
  {
    "Date": "2026-04-06",
    "Location": "Sacramento, CA",
    "Host": "Two Nosey Girls",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 38.5546,
    "Longitude": -121.4779
  },
  {
    "Date": "2026-04-08",
    "Location": "Olympia, WA",
    "Host": "Rachelle Bailey-Austin & Dorothy Turley",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 47.0365,
    "Longitude": -122.9322
  },
  {
    "Date": "2026-04-10",
    "Location": "Rapid City, SD",
    "Host": "Two Paws Up Dog Training, LLC",
    "TrialTypes": "ELT, NW3, NW1, NW2",
    "EventCount": 4,
    "Latitude": 44.0632,
    "Longitude": -103.2573
  },
  {
    "Date": "2026-04-10",
    "Location": "Somis, CA",
    "Host": "Pink Biscuit K9s",
    "TrialTypes": "NW3, ELT-P, L2C, L2I",
    "EventCount": 4,
    "Latitude": 34.2419,
    "Longitude": -118.993
  },
  {
    "Date": "2026-04-11",
    "Location": "Bel Air, MD",
    "Host": "Firezone GS",
    "TrialTypes": "ELT-P, ELT-S, NW2",
    "EventCount": 3,
    "Latitude": 39.5366,
    "Longitude": -76.3448
  },
  {
    "Date": "2026-04-11",
    "Location": "Blue Ridge , VA",
    "Host": "Canny K9 Companions, LLC",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 37.3806,
    "Longitude": -79.7926
  },
  {
    "Date": "2026-04-11",
    "Location": "Clinton, WI",
    "Host": "George Carpenter",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 42.5561,
    "Longitude": -88.887
  },
  {
    "Date": "2026-04-11",
    "Location": "Durham, NC",
    "Host": "Dog Fun Forever, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 35.9787,
    "Longitude": -78.9282
  },
  {
    "Date": "2026-04-11",
    "Location": "Genoa, IL",
    "Host": "Common Scents K9",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 42.0637,
    "Longitude": -88.7137
  },
  {
    "Date": "2026-04-11",
    "Location": "Limerick, PA",
    "Host": "Sniff Sniff Hooray",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 40.2517,
    "Longitude": -75.5454
  },
  {
    "Date": "2026-04-11",
    "Location": "Rocklin, CA",
    "Host": "Sierra Sniffing Canines",
    "TrialTypes": "NW2, L1E, L2E",
    "EventCount": 3,
    "Latitude": 38.7986,
    "Longitude": -121.1913
  },
  {
    "Date": "2026-04-13",
    "Location": "Ellicott City, MD",
    "Host": "Red Huskies",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 39.32,
    "Longitude": -76.8215
  },
  {
    "Date": "2026-04-17",
    "Location": "Amity, OR",
    "Host": "Doglandia, LLC",
    "TrialTypes": "NW1, NW2, NW3",
    "EventCount": 3,
    "Latitude": 45.1146,
    "Longitude": -123.1908
  },
  {
    "Date": "2026-04-17",
    "Location": "Garrison, NY",
    "Host": "Top Notch Dogs, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 41.3503,
    "Longitude": -73.9491
  },
  {
    "Date": "2026-04-17",
    "Location": "Upland, CA",
    "Host": "Agile Paws Dog Sports",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 34.0777,
    "Longitude": -117.6035
  },
  {
    "Date": "2026-04-18",
    "Location": "Auburn, WA",
    "Host": "Northwest K9 Sniffers",
    "TrialTypes": "L1V, NW1, L1I, NW2",
    "EventCount": 4,
    "Latitude": 47.2693,
    "Longitude": -122.2723
  },
  {
    "Date": "2026-04-18",
    "Location": "Keystone Heights, FL",
    "Host": "River Poodles Training, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 29.8104,
    "Longitude": -82.0641
  },
  {
    "Date": "2026-04-18",
    "Location": "Laramie, WY",
    "Host": "Mountain Dogs LLC",
    "TrialTypes": "SMT, ELT",
    "EventCount": 2,
    "Latitude": 41.2828,
    "Longitude": -105.6212
  },
  {
    "Date": "2026-04-18",
    "Location": "Pomfret, MD",
    "Host": "Nose-It-All, LLC",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 38.5872,
    "Longitude": -77.0255
  },
  {
    "Date": "2026-04-18",
    "Location": "Toledo, OH",
    "Host": "Robin Ford Dog Training LLC",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 41.6985,
    "Longitude": -83.5818
  },
  {
    "Date": "2026-04-18",
    "Location": "Winterset, IA",
    "Host": "KBP Dog Training",
    "TrialTypes": "NW2, NW1",
    "EventCount": 2,
    "Latitude": 41.3528,
    "Longitude": -94.0155
  },
  {
    "Date": "2026-04-18",
    "Location": "Woodstock, IL",
    "Host": "Northwest Obedience Club Inc",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 42.3101,
    "Longitude": -88.4419
  },
  {
    "Date": "2026-04-19",
    "Location": "Glenwood, NY",
    "Host": "Do Over Dog Training",
    "TrialTypes": "L2I, L1E, L2C, ELT-S",
    "EventCount": 4,
    "Latitude": 42.5779,
    "Longitude": -78.6727
  },
  {
    "Date": "2026-04-20",
    "Location": "Amherst, NH",
    "Host": "Lucky Dog Events",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 42.8649,
    "Longitude": -71.5791
  },
  {
    "Date": "2026-04-21",
    "Location": "Stony Point , NY",
    "Host": "Saints2Source, LLC",
    "TrialTypes": "NW3, ELT, ELT-S, NW2",
    "EventCount": 4,
    "Latitude": 41.2504,
    "Longitude": -73.9875
  },
  {
    "Date": "2026-04-24",
    "Location": "Asheboro, NC",
    "Host": "K9 Nose Adventures, LLC",
    "TrialTypes": "ELT, NW3, ELT-S, NW2",
    "EventCount": 4,
    "Latitude": 35.6859,
    "Longitude": -79.775
  },
  {
    "Date": "2026-04-24",
    "Location": "Easton, MD",
    "Host": "Red Huskies",
    "TrialTypes": "L3C, NW2, L3V, NW1",
    "EventCount": 4,
    "Latitude": 38.7309,
    "Longitude": -76.0338
  },
  {
    "Date": "2026-04-25",
    "Location": "Canfield, OH",
    "Host": "Nosework Addicts, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 41.0647,
    "Longitude": -80.7228
  },
  {
    "Date": "2026-04-25",
    "Location": "Columbus, MT",
    "Host": "Canine Connection",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 45.6021,
    "Longitude": -109.2974
  },
  {
    "Date": "2026-04-25",
    "Location": "Ellicottville, NY",
    "Host": "Do Over Dog Training",
    "TrialTypes": "ELT, ELT-S, NW1",
    "EventCount": 3,
    "Latitude": 42.2886,
    "Longitude": -78.6985
  },
  {
    "Date": "2026-04-25",
    "Location": "Havre de Grace, MD",
    "Host": "Chesapeake Search Dogs",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 39.5051,
    "Longitude": -76.0915
  },
  {
    "Date": "2026-04-25",
    "Location": "Portland, OR",
    "Host": "Trust Your Dog K9 Events",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 45.5506,
    "Longitude": -122.6454
  },
  {
    "Date": "2026-04-25",
    "Location": "Sharon, MA",
    "Host": "Bay State Sniffers",
    "TrialTypes": "NW3, ELT-P",
    "EventCount": 2,
    "Latitude": 42.1019,
    "Longitude": -71.2102
  },
  {
    "Date": "2026-04-25",
    "Location": "Suring, WI",
    "Host": "Clever Sniffers, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 44.9978,
    "Longitude": -88.3359
  },
  {
    "Date": "2026-04-25",
    "Location": "Traverse City, MI",
    "Host": "Every Dog Nosework",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 44.7857,
    "Longitude": -85.5926
  },
  {
    "Date": "2026-05-01",
    "Location": "Faribault, MN",
    "Host": "St. Paul Dog Training Club",
    "TrialTypes": "NW3, NW1, L2E, L3C",
    "EventCount": 4,
    "Latitude": 43.6608,
    "Longitude": -93.928
  },
  {
    "Date": "2026-05-01",
    "Location": "Nyack, NY",
    "Host": "Waggin' Work",
    "TrialTypes": "NW2, NW1, ELT-P, ELT-S",
    "EventCount": 4,
    "Latitude": 41.1151,
    "Longitude": -73.949
  },
  {
    "Date": "2026-05-01",
    "Location": "Turlock, CA",
    "Host": "Two Nosey Girls",
    "TrialTypes": "L1I, L2I, ELT-S, L3I",
    "EventCount": 4,
    "Latitude": 37.4756,
    "Longitude": -120.839
  },
  {
    "Date": "2026-05-02",
    "Location": "Alexis, IL",
    "Host": "Kudos for Canines, LLC",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 41.0933,
    "Longitude": -90.5448
  },
  {
    "Date": "2026-05-02",
    "Location": "Ashby , MA",
    "Host": "Dogs! Carolyn Barney",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 42.72,
    "Longitude": -71.8104
  },
  {
    "Date": "2026-05-02",
    "Location": "Hillsdale, NY",
    "Host": "For the Love of Dogs NY LLC",
    "TrialTypes": "ELT-P, ELT",
    "EventCount": 2,
    "Latitude": 42.1398,
    "Longitude": -73.5445
  },
  {
    "Date": "2026-05-02",
    "Location": "Santa Paula, CA",
    "Host": "Pink Biscuit K9s",
    "TrialTypes": "NW3, ELT-S, NW2",
    "EventCount": 3,
    "Latitude": 34.3073,
    "Longitude": -119.0168
  },
  {
    "Date": "2026-05-02",
    "Location": "Sedona, AZ",
    "Host": "Successful Sniffer",
    "TrialTypes": "L1I, NW2, NW3",
    "EventCount": 3,
    "Latitude": 34.9063,
    "Longitude": -111.7338
  },
  {
    "Date": "2026-05-02",
    "Location": "Vancouver, WA",
    "Host": "Sniffketeers",
    "TrialTypes": "ELT-S",
    "EventCount": 1,
    "Latitude": 45.6545,
    "Longitude": -122.6765
  },
  {
    "Date": "2026-05-07",
    "Location": "Lancaster, PA",
    "Host": "Red Huskies Nose Work, LLC",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 39.9954,
    "Longitude": -76.2674
  },
  {
    "Date": "2026-05-08",
    "Location": "Grand Island, NY",
    "Host": "Do Over Dog Training",
    "TrialTypes": "SMT, ELT",
    "EventCount": 2,
    "Latitude": 43.0304,
    "Longitude": -78.9354
  },
  {
    "Date": "2026-05-08",
    "Location": "Jarrettsville, MD",
    "Host": "Firezone GS",
    "TrialTypes": "ELT, NW3, ELT-S, L2I",
    "EventCount": 4,
    "Latitude": 39.6375,
    "Longitude": -76.4606
  },
  {
    "Date": "2026-05-08",
    "Location": "Warwick, NY",
    "Host": "Top Notch Dogs, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 41.2265,
    "Longitude": -74.408
  },
  {
    "Date": "2026-05-08",
    "Location": "Wrightwood, CA",
    "Host": "JavaK9s, LLC",
    "TrialTypes": "NW3, L1C, L1I",
    "EventCount": 3,
    "Latitude": 34.3133,
    "Longitude": -117.6617
  },
  {
    "Date": "2026-05-09",
    "Location": "Brighton, MI",
    "Host": "Every Dog Nosework",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 42.4947,
    "Longitude": -83.7655
  },
  {
    "Date": "2026-05-09",
    "Location": "Charlton, MA",
    "Host": "HeavenScent Sniffers",
    "TrialTypes": "ELT-P, L2C, NW1",
    "EventCount": 3,
    "Latitude": 42.1649,
    "Longitude": -71.9223
  },
  {
    "Date": "2026-05-09",
    "Location": "Egg Harbor City, NJ",
    "Host": "Rotts-n-Notts Nosework LLC",
    "TrialTypes": "L3I, NW1, NW3",
    "EventCount": 3,
    "Latitude": 39.5727,
    "Longitude": -74.6939
  },
  {
    "Date": "2026-05-09",
    "Location": "Livingston, MT",
    "Host": "Trails and Tails Dog School",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 45.6668,
    "Longitude": -110.575
  },
  {
    "Date": "2026-05-09",
    "Location": "Malvern, IA",
    "Host": "Two Tails Unlimited",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 41.027,
    "Longitude": -95.5958
  },
  {
    "Date": "2026-05-09",
    "Location": "Poland Springs, ME",
    "Host": "Bare Bones Nosework, LLC",
    "TrialTypes": "L1I, L2I",
    "EventCount": 2,
    "Latitude": 43.9803,
    "Longitude": -70.394
  },
  {
    "Date": "2026-05-09",
    "Location": "Union Grove, WI",
    "Host": "Loving Paws, LLC",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 42.6934,
    "Longitude": -88.0313
  },
  {
    "Date": "2026-05-14",
    "Location": "Frederick, MD",
    "Host": "Red Huskies",
    "TrialTypes": "ELT-P, ELT-S, L2I, L2E, L1E",
    "EventCount": 5,
    "Latitude": 39.4296,
    "Longitude": -77.4069
  },
  {
    "Date": "2026-05-15",
    "Location": "Cannon Falls, MN",
    "Host": "Saint Paul Dog Training Club",
    "TrialTypes": "ELT, ELT-S, L3E, L1I, L1E",
    "EventCount": 5,
    "Latitude": 44.5116,
    "Longitude": -92.9136
  },
  {
    "Date": "2026-05-15",
    "Location": "Phoenix, MD",
    "Host": "Oriole Dog Training Club",
    "TrialTypes": "NW3, L1I, NW1",
    "EventCount": 3,
    "Latitude": 39.5056,
    "Longitude": -76.6316
  },
  {
    "Date": "2026-05-15",
    "Location": "Watsonville, CA",
    "Host": "CalCoastal Dog Owners Group",
    "TrialTypes": "ELT-S, L3C, L1C",
    "EventCount": 3,
    "Latitude": 36.9009,
    "Longitude": -121.716
  },
  {
    "Date": "2026-05-16",
    "Location": "Alexander, NY",
    "Host": "Do Over Dog Training",
    "TrialTypes": "L1V, L2V, L3C, L3I",
    "EventCount": 4,
    "Latitude": 42.905,
    "Longitude": -78.2324
  },
  {
    "Date": "2026-05-16",
    "Location": "Bellingham, WA",
    "Host": "The Nosework Magic",
    "TrialTypes": "NW2, NW1",
    "EventCount": 2,
    "Latitude": 48.7892,
    "Longitude": -122.4825
  },
  {
    "Date": "2026-05-16",
    "Location": "Burien, WA",
    "Host": "Northwest K9 Sniffers",
    "TrialTypes": "NW3, L2V, L2I",
    "EventCount": 3,
    "Latitude": 47.4332,
    "Longitude": -122.3537
  },
  {
    "Date": "2026-05-16",
    "Location": "Durham, NC",
    "Host": "Whole Dog Institute, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 35.9602,
    "Longitude": -78.9514
  },
  {
    "Date": "2026-05-16",
    "Location": "Kittanning, PA",
    "Host": "Steel City Nosework, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 40.7835,
    "Longitude": -79.5369
  },
  {
    "Date": "2026-05-16",
    "Location": "Monticello , NY",
    "Host": "Saints2Source, LLC",
    "TrialTypes": "NW3, ELT, ELT-S",
    "EventCount": 3,
    "Latitude": 41.6881,
    "Longitude": -74.7017
  },
  {
    "Date": "2026-05-16",
    "Location": "Peru, MA",
    "Host": "Lucky Dog Events",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 42.4831,
    "Longitude": -73.0837
  },
  {
    "Date": "2026-05-16",
    "Location": "Sandwich, IL",
    "Host": "For Your K9, Inc.",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 41.6512,
    "Longitude": -88.5992
  },
  {
    "Date": "2026-05-22",
    "Location": "Anchorage, AK",
    "Host": "Alaska Dog Sports",
    "TrialTypes": "NW1, NW2, NW3",
    "EventCount": 3,
    "Latitude": 61.2263,
    "Longitude": -149.9107
  },
  {
    "Date": "2026-05-22",
    "Location": "Montrose, CO",
    "Host": "Mountain Dogs LLC",
    "TrialTypes": "ELT, NW3, NW2, NW1",
    "EventCount": 4,
    "Latitude": 38.4646,
    "Longitude": -107.9021
  },
  {
    "Date": "2026-05-22",
    "Location": "San Luis Obispo, CA",
    "Host": "Gentle Touch Pet Training",
    "TrialTypes": "NW1, L1I, NW2",
    "EventCount": 3,
    "Latitude": 35.3138,
    "Longitude": -120.4066
  },
  {
    "Date": "2026-05-23",
    "Location": "Alpharetta, GA",
    "Host": "Georgia Nosework, LLC",
    "TrialTypes": "NW3, ELT-S, NW2, ELT",
    "EventCount": 4,
    "Latitude": 34.1086,
    "Longitude": -84.3177
  },
  {
    "Date": "2026-05-23",
    "Location": "Columbus, MT",
    "Host": "Canine Connection",
    "TrialTypes": "L1I, L2I, ELT-S, NW2",
    "EventCount": 4,
    "Latitude": 45.6721,
    "Longitude": -109.2186
  },
  {
    "Date": "2026-05-23",
    "Location": "Emmitsburg, MD",
    "Host": "Red Huskies",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 39.6582,
    "Longitude": -77.2987
  },
  {
    "Date": "2026-05-23",
    "Location": "Lancaster, PA",
    "Host": "Nose-It-All, LLC",
    "TrialTypes": "ELT, ELT-P, NW3",
    "EventCount": 3,
    "Latitude": 40.0351,
    "Longitude": -76.3477
  },
  {
    "Date": "2026-05-23",
    "Location": "Murfreesboro, TN",
    "Host": "Dogs Have Amazing Noses, LLC",
    "TrialTypes": "ELT, NW1",
    "EventCount": 2,
    "Latitude": 35.8903,
    "Longitude": -86.4021
  },
  {
    "Date": "2026-05-23",
    "Location": "North Manchester, IN",
    "Host": "2 Nose You Is 2 Loves You",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 41.0329,
    "Longitude": -85.7829
  },
  {
    "Date": "2026-05-23",
    "Location": "Rainier, WA",
    "Host": "Let's Talk Dogs, LLC & About Face K9 Academy",
    "TrialTypes": "ELT-S, L1C, NW2",
    "EventCount": 3,
    "Latitude": 46.9055,
    "Longitude": -122.7173
  },
  {
    "Date": "2026-05-23",
    "Location": "Red Feather Lakes, CO",
    "Host": "Beyond Elevation K9 Training LLC",
    "TrialTypes": "NW1, NW2, NW3",
    "EventCount": 3,
    "Latitude": 40.7837,
    "Longitude": -105.5373
  },
  {
    "Date": "2026-05-23",
    "Location": "Rockaway, NJ",
    "Host": "Shamrock Pot Of Gold K9 Scenter",
    "TrialTypes": "NW3, L3E, NW2, ELT-S",
    "EventCount": 4,
    "Latitude": 40.9505,
    "Longitude": -74.477
  },
  {
    "Date": "2026-05-23",
    "Location": "Sandy, OR",
    "Host": "Trust Your Dog K9 Events",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 45.3997,
    "Longitude": -122.2495
  },
  {
    "Date": "2026-05-25",
    "Location": "Manchester, NH",
    "Host": "For the Love of Dogs NY, LLC",
    "TrialTypes": "NW2, ELT-P, ELT",
    "EventCount": 3,
    "Latitude": 42.9984,
    "Longitude": -71.449
  },
  {
    "Date": "2026-05-28",
    "Location": "Concord, CA",
    "Host": "The Bay Team",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 37.9575,
    "Longitude": -122.0208
  },
  {
    "Date": "2026-05-29",
    "Location": "Grand Junction, CO",
    "Host": "Mountain Dogs LLC",
    "TrialTypes": "NW3, L1V, L1E",
    "EventCount": 3,
    "Latitude": 39.0405,
    "Longitude": -108.5554
  },
  {
    "Date": "2026-05-30",
    "Location": "Amherst, NY",
    "Host": "Do Over Dog Training",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 42.9302,
    "Longitude": -78.7737
  },
  {
    "Date": "2026-05-30",
    "Location": "Eden Prairie, MN",
    "Host": "The K9 Nose",
    "TrialTypes": "NW2",
    "EventCount": 1,
    "Latitude": 44.8601,
    "Longitude": -93.5077
  },
  {
    "Date": "2026-05-30",
    "Location": "Spencer, MA",
    "Host": "Lucky Dog Events",
    "TrialTypes": "ELT, L1E, L2C",
    "EventCount": 3,
    "Latitude": 42.2402,
    "Longitude": -72.0131
  },
  {
    "Date": "2026-06-05",
    "Location": "Enterprise, OR",
    "Host": "Country K9 Nosework, LLC",
    "TrialTypes": "NW2, ELT-P",
    "EventCount": 2,
    "Latitude": 45.4598,
    "Longitude": -117.3245
  },
  {
    "Date": "2026-06-06",
    "Location": "Dunmore, PA",
    "Host": "Your Dog's Place, LLC",
    "TrialTypes": "ELT, ELT-S, L1C",
    "EventCount": 3,
    "Latitude": 41.409,
    "Longitude": -75.6331
  },
  {
    "Date": "2026-06-06",
    "Location": "Enterprise, OR",
    "Host": "Country K9 Nosework, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 45.3785,
    "Longitude": -117.2618
  },
  {
    "Date": "2026-06-06",
    "Location": "Manheim, PA",
    "Host": "Nose-It-All, LLC",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 40.1946,
    "Longitude": -76.3504
  },
  {
    "Date": "2026-06-06",
    "Location": "Meadowbrook, PA",
    "Host": "Patricia Grassey",
    "TrialTypes": "NW3, NW1, NW2, ELT",
    "EventCount": 4,
    "Latitude": 40.0699,
    "Longitude": -75.0747
  },
  {
    "Date": "2026-06-06",
    "Location": "Rochester, NH",
    "Host": "Pawsitive Image",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 43.3061,
    "Longitude": -70.9608
  },
  {
    "Date": "2026-06-06",
    "Location": "Shawnee, OK",
    "Host": "The Doggie Spot, LLC",
    "TrialTypes": "NW3, NW2, NW1",
    "EventCount": 3,
    "Latitude": 35.3197,
    "Longitude": -96.9503
  },
  {
    "Date": "2026-06-06",
    "Location": "Slippery Rock, PA",
    "Host": "Nosework Addicts, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 41.094,
    "Longitude": -80.0096
  },
  {
    "Date": "2026-06-06",
    "Location": "Sparks Glencoe, MD",
    "Host": "Chesapeake Search Dogs",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 39.5315,
    "Longitude": -76.6855
  },
  {
    "Date": "2026-06-06",
    "Location": "Wrightstown, WI",
    "Host": "NEWk9Scent Work LLC",
    "TrialTypes": "NW3, NW1, L1I",
    "EventCount": 3,
    "Latitude": 44.3468,
    "Longitude": -88.1714
  },
  {
    "Date": "2026-06-12",
    "Location": "Gunnison, CO",
    "Host": "Mountain Dogs LLC",
    "TrialTypes": "ELT, ELT-S, ELT-P",
    "EventCount": 3,
    "Latitude": 38.6726,
    "Longitude": -107.0815
  },
  {
    "Date": "2026-06-12",
    "Location": "New Hope, PA",
    "Host": "Patricia Grassey",
    "TrialTypes": "ELT, ELT-P, ELT-S, L3E",
    "EventCount": 4,
    "Latitude": 40.3356,
    "Longitude": -74.9363
  },
  {
    "Date": "2026-06-13",
    "Location": "East Helena, MT",
    "Host": "Nose Work Breakfast Club",
    "TrialTypes": "NW1, NW2, NW3",
    "EventCount": 3,
    "Latitude": 46.5827,
    "Longitude": -111.9562
  },
  {
    "Date": "2026-06-13",
    "Location": "Ithaca, NY",
    "Host": "The Brainy Canine",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 42.4615,
    "Longitude": -76.5681
  },
  {
    "Date": "2026-06-13",
    "Location": "Kenosha, WI",
    "Host": "Loving Paws Dog Training LLC",
    "TrialTypes": "ELT-S, L1I, NW1",
    "EventCount": 3,
    "Latitude": 42.5833,
    "Longitude": -87.8686
  },
  {
    "Date": "2026-06-13",
    "Location": "Linden, MI",
    "Host": "Every Dog Nosework",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 42.8574,
    "Longitude": -83.8058
  },
  {
    "Date": "2026-06-13",
    "Location": "Nazareth/Windgap, PA",
    "Host": "Paws n' Sniff",
    "TrialTypes": "NW2, ELT, NW1, ELT-S, NW3",
    "EventCount": 5,
    "Latitude": 40.7149,
    "Longitude": -75.2731
  },
  {
    "Date": "2026-06-13",
    "Location": "Palmyra, VA",
    "Host": "Your Dog Knows LLC",
    "TrialTypes": "L1I, L2I, L1C, L2C",
    "EventCount": 4,
    "Latitude": 37.8978,
    "Longitude": -78.2915
  },
  {
    "Date": "2026-06-18",
    "Location": "Westminster, MD",
    "Host": "Firezone GS",
    "TrialTypes": "ELT, NW3, ELT-S, NW2",
    "EventCount": 4,
    "Latitude": 39.5561,
    "Longitude": -76.9677
  },
  {
    "Date": "2026-06-19",
    "Location": "Bayfield, CO",
    "Host": "Mountain Dogs, LLC",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 37.2194,
    "Longitude": -107.6361
  },
  {
    "Date": "2026-06-19",
    "Location": "Jordan, MN",
    "Host": "St. Paul Dog Training Club",
    "TrialTypes": "ELT, ELT-P, L1V, L2V",
    "EventCount": 4,
    "Latitude": 44.659,
    "Longitude": -93.6174
  },
  {
    "Date": "2026-06-19",
    "Location": "New Rochelle, NY",
    "Host": "For the Love of Dogs NY, LLC",
    "TrialTypes": "ELT, NW3, ELT-P",
    "EventCount": 3,
    "Latitude": 40.9275,
    "Longitude": -73.762
  },
  {
    "Date": "2026-06-20",
    "Location": "Cumming, GA",
    "Host": "Georgia Nosework, LLC",
    "TrialTypes": "L2I, L2C, L3C, L1I",
    "EventCount": 4,
    "Latitude": 34.2455,
    "Longitude": -84.0975
  },
  {
    "Date": "2026-06-20",
    "Location": "Danvers, MA",
    "Host": "Everydog, LLC",
    "TrialTypes": "NW1, NW2, NW3",
    "EventCount": 3,
    "Latitude": 42.5571,
    "Longitude": -70.9036
  },
  {
    "Date": "2026-06-20",
    "Location": "Florissant, MO",
    "Host": "Happy Dog Concepts",
    "TrialTypes": "NW1, NW2, NW3",
    "EventCount": 3,
    "Latitude": 38.7663,
    "Longitude": -90.299
  },
  {
    "Date": "2026-06-20",
    "Location": "Pittsburgh, PA",
    "Host": "Nosework Addicts, LLC",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 40.4149,
    "Longitude": -80.0144
  },
  {
    "Date": "2026-06-20",
    "Location": "Terryville, CT",
    "Host": "Willoughby Training",
    "TrialTypes": "NW3, NW1",
    "EventCount": 2,
    "Latitude": 41.6384,
    "Longitude": -73.0204
  },
  {
    "Date": "2026-06-20",
    "Location": "White Salmon, WA",
    "Host": "Sharon Smith",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 45.7227,
    "Longitude": -121.4527
  },
  {
    "Date": "2026-06-26",
    "Location": "Delran, NJ",
    "Host": "K9 InScentives",
    "TrialTypes": "NW1, ELT",
    "EventCount": 2,
    "Latitude": 40.0033,
    "Longitude": -74.9207
  },
  {
    "Date": "2026-06-26",
    "Location": "Loveland, CO",
    "Host": "NoCo Unleashed LLC",
    "TrialTypes": "ELT-S, L2C, L2I, L1C",
    "EventCount": 4,
    "Latitude": 40.4273,
    "Longitude": -105.0795
  },
  {
    "Date": "2026-06-26",
    "Location": "Red Lodge, MT",
    "Host": "Canine Connection",
    "TrialTypes": "ELT, NW3, NW1, NW2",
    "EventCount": 4,
    "Latitude": 45.2357,
    "Longitude": -109.256
  },
  {
    "Date": "2026-06-27",
    "Location": "Burlington, WI",
    "Host": "Loving Paws Dog Training LLC",
    "TrialTypes": "NW2, ELT",
    "EventCount": 2,
    "Latitude": 42.6809,
    "Longitude": -88.237
  },
  {
    "Date": "2026-06-27",
    "Location": "De Pere, WI",
    "Host": "NEWk9Scent Work LLC",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 44.4354,
    "Longitude": -88.0751
  },
  {
    "Date": "2026-06-27",
    "Location": "Deming, WA",
    "Host": "The Nosework Magic",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 48.8386,
    "Longitude": -122.259
  },
  {
    "Date": "2026-06-27",
    "Location": "Inver Grove Heights, MN",
    "Host": "Outside the Box Dog Training, LLC",
    "TrialTypes": "NW3, L2C, L2I",
    "EventCount": 3,
    "Latitude": 44.88,
    "Longitude": -92.9998
  },
  {
    "Date": "2026-06-27",
    "Location": "Lockport, IL",
    "Host": "4G & TB",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 41.6072,
    "Longitude": -88.0469
  },
  {
    "Date": "2026-06-27",
    "Location": "New Wilmington, PA",
    "Host": "Steel City Nosework, LLC",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 41.0862,
    "Longitude": -80.3211
  },
  {
    "Date": "2026-06-27",
    "Location": "Salem, OR",
    "Host": "Doglandia, LLC",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 44.9365,
    "Longitude": -123.0024
  },
  {
    "Date": "2026-06-27",
    "Location": "Somers, CT",
    "Host": "HeavenScent Sniffers",
    "TrialTypes": "NW2, L3C, ELT-S",
    "EventCount": 3,
    "Latitude": 42.0225,
    "Longitude": -72.4919
  },
  {
    "Date": "2026-06-30",
    "Location": "Delran, NJ",
    "Host": "Ev-ry Earthdog, LLC",
    "TrialTypes": "NW3, NW1, NW2, ELT-P",
    "EventCount": 4,
    "Latitude": 40.0587,
    "Longitude": -74.9991
  },
  {
    "Date": "2026-07-03",
    "Location": "Huntington, MA",
    "Host": "Shamrock Pot Of Gold K9 Scenter",
    "TrialTypes": "NW3, ELT, ELT-S, L2I",
    "EventCount": 4,
    "Latitude": 42.2779,
    "Longitude": -72.8789
  },
  {
    "Date": "2026-07-06",
    "Location": "Montgomery, NY",
    "Host": "For the Love of Dogs NY LLC",
    "TrialTypes": "NW2, ELT",
    "EventCount": 2,
    "Latitude": 42.9386,
    "Longitude": -74.3653
  },
  {
    "Date": "2026-07-10",
    "Location": "Leadville, CO",
    "Host": "Mountain Dogs LLC",
    "TrialTypes": "NW3, NW2, NW1, L2I, L2C",
    "EventCount": 5,
    "Latitude": 39.283,
    "Longitude": -106.295
  },
  {
    "Date": "2026-07-10",
    "Location": "Sparks Glencoe, MD",
    "Host": "Firezone GS",
    "TrialTypes": "ELT-P, ELT-S, L3I, ELT",
    "EventCount": 4,
    "Latitude": 39.5504,
    "Longitude": -76.6825
  },
  {
    "Date": "2026-07-11",
    "Location": "Livonia, MI",
    "Host": "Every Dog Nosework",
    "TrialTypes": "ELT-P, NW1",
    "EventCount": 2,
    "Latitude": 42.3714,
    "Longitude": -83.3421
  },
  {
    "Date": "2026-07-13",
    "Location": "Derry, NH",
    "Host": "Lucky Dog Events",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 42.8577,
    "Longitude": -71.3189
  },
  {
    "Date": "2026-07-13",
    "Location": "Florham Park, NJ",
    "Host": "For the Love of Dogs NY LLC",
    "TrialTypes": "L1C, L1I, ELT",
    "EventCount": 3,
    "Latitude": 40.827,
    "Longitude": -74.3638
  },
  {
    "Date": "2026-07-17",
    "Location": "Encinitas, CA",
    "Host": "Rewarding Rover LLC & UberDog/Jessica Koester",
    "TrialTypes": "NW2, NW1, ELT-S",
    "EventCount": 3,
    "Latitude": 33.0154,
    "Longitude": -117.3047
  },
  {
    "Date": "2026-07-17",
    "Location": "Leadville, CO",
    "Host": "Mountain Dogs LLC",
    "TrialTypes": "ELT, NW3, ELT-S, NW2",
    "EventCount": 4,
    "Latitude": 39.2546,
    "Longitude": -106.2757
  },
  {
    "Date": "2026-07-18",
    "Location": "Los Osos, CA",
    "Host": "Central Coast Nosework Club",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 35.3445,
    "Longitude": -120.8364
  },
  {
    "Date": "2026-07-18",
    "Location": "Woodbury, MN",
    "Host": "Gretchen Hofheins-Wackerfuss",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 44.8892,
    "Longitude": -92.9735
  },
  {
    "Date": "2026-07-25",
    "Location": "Elmira, OR",
    "Host": "Kiddy Christie",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 44.0586,
    "Longitude": -123.3283
  },
  {
    "Date": "2026-07-29",
    "Location": "Soldotna, AK",
    "Host": "Peninsula Dog Obedience Group LLC",
    "TrialTypes": "NW1, NW2, NW3, ELT",
    "EventCount": 4,
    "Latitude": 60.4406,
    "Longitude": -151.0166
  },
  {
    "Date": "2026-08-01",
    "Location": "Bettendorf, IA",
    "Host": "Fur Better Fur Worse Dog Training",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 41.4885,
    "Longitude": -90.5152
  },
  {
    "Date": "2026-08-01",
    "Location": "Columbia, MO",
    "Host": "Columbia Canine Sports Center, LLC",
    "TrialTypes": "L1V, L1I, L1C, L2C",
    "EventCount": 4,
    "Latitude": 38.9291,
    "Longitude": -92.3208
  },
  {
    "Date": "2026-08-01",
    "Location": "Deming, WA",
    "Host": "The Nosework Magic",
    "TrialTypes": "ELT, NW2",
    "EventCount": 2,
    "Latitude": 48.801,
    "Longitude": -122.1982
  },
  {
    "Date": "2026-08-01",
    "Location": "Jefferson, WI",
    "Host": "K9 Ventures",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 42.9977,
    "Longitude": -88.8059
  },
  {
    "Date": "2026-08-01",
    "Location": "Pillager, MN",
    "Host": "Nose 2 Tail Dog Training LLC",
    "TrialTypes": "NW1, NW2, ELT-P",
    "EventCount": 3,
    "Latitude": 46.367,
    "Longitude": -94.4751
  },
  {
    "Date": "2026-08-07",
    "Location": "Huntington Beach, CA",
    "Host": "JavaK9s, LLC",
    "TrialTypes": "ELT, L2C, L2I",
    "EventCount": 3,
    "Latitude": 33.6687,
    "Longitude": -118.0291
  },
  {
    "Date": "2026-08-08",
    "Location": "Altamont, IL",
    "Host": "Kudos for Canines, LLC",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 39.0504,
    "Longitude": -88.748
  },
  {
    "Date": "2026-08-14",
    "Location": "La Jolla, CA",
    "Host": "Rewarding Rover LLC & UberDog/Jessica Koester",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 32.808,
    "Longitude": -117.2874
  },
  {
    "Date": "2026-08-15",
    "Location": "Greenwich, CT",
    "Host": "For the Love of Dogs NY LLC",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 41.0047,
    "Longitude": -73.6056
  },
  {
    "Date": "2026-08-15",
    "Location": "Monmouth, OR",
    "Host": "Doglandia, LLC",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 44.897,
    "Longitude": -123.2429
  },
  {
    "Date": "2026-08-21",
    "Location": "Chelsea, MI",
    "Host": "Force Free Dale, LLC",
    "TrialTypes": "NW3, L1V, L1C, NW2",
    "EventCount": 4,
    "Latitude": 42.3505,
    "Longitude": -84.0462
  },
  {
    "Date": "2026-08-22",
    "Location": "Greenfield, MA",
    "Host": "Lucky Dog Events",
    "TrialTypes": "NW3, NW2, NW1",
    "EventCount": 3,
    "Latitude": 42.5929,
    "Longitude": -72.5931
  },
  {
    "Date": "2026-08-22",
    "Location": "Johnstown, NY",
    "Host": "My Dog Smells LLC",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 43.0435,
    "Longitude": -74.3326
  },
  {
    "Date": "2026-08-22",
    "Location": "North Bend, WA",
    "Host": "Northwest K9 Sniffers",
    "TrialTypes": "ELT, L1C, L2I",
    "EventCount": 3,
    "Latitude": 47.4833,
    "Longitude": -121.813
  },
  {
    "Date": "2026-08-28",
    "Location": "Easton and Lutherville, MD",
    "Host": "Fair Play Labradors",
    "TrialTypes": "ELT-S, L2E, L2C, NW1, L1I",
    "EventCount": 5,
    "Latitude": 39.4242,
    "Longitude": -76.5867
  },
  {
    "Date": "2026-08-28",
    "Location": "Meeker, CO",
    "Host": "Mountain Dogs LLC",
    "TrialTypes": "NW3, NW1, L2C, NW2",
    "EventCount": 4,
    "Latitude": 40.0441,
    "Longitude": -107.9619
  },
  {
    "Date": "2026-08-29",
    "Location": "Dunkirk, NY",
    "Host": "Do Over Dog Training",
    "TrialTypes": "NW1, NW2, ELT-S, L3E",
    "EventCount": 4,
    "Latitude": 42.4923,
    "Longitude": -79.2979
  },
  {
    "Date": "2026-08-31",
    "Location": "Cambria, CA",
    "Host": "Gentle Touch Pet Training",
    "TrialTypes": "ELT, L1E, L2E",
    "EventCount": 3,
    "Latitude": 35.5485,
    "Longitude": -121.1365
  },
  {
    "Date": "2026-09-05",
    "Location": "Luthersville, GA",
    "Host": "Hold The Line K9 LLC",
    "TrialTypes": "L1I, L2I, NW3",
    "EventCount": 3,
    "Latitude": 33.1727,
    "Longitude": -84.755
  },
  {
    "Date": "2026-09-11",
    "Location": "Richmond, VA",
    "Host": "Paws Plus Training, LLC",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 37.5507,
    "Longitude": -77.4737
  },
  {
    "Date": "2026-09-12",
    "Location": "Clinton, PA",
    "Host": "Nosework Addicts, LLC",
    "TrialTypes": "NW1, ELT",
    "EventCount": 2,
    "Latitude": 40.5629,
    "Longitude": -80.2992
  },
  {
    "Date": "2026-09-12",
    "Location": "Lafayette Hill, PA",
    "Host": "Sniff Sniff Hooray",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 40.1085,
    "Longitude": -75.2918
  },
  {
    "Date": "2026-09-12",
    "Location": "Loma Mar, CA",
    "Host": "The Bay Team",
    "TrialTypes": "NW2, ELT",
    "EventCount": 2,
    "Latitude": 37.3125,
    "Longitude": -122.254
  },
  {
    "Date": "2026-09-12",
    "Location": "Sharon, MA",
    "Host": "Bay State Sniffers",
    "TrialTypes": "ELT, ELT-S, NW2",
    "EventCount": 3,
    "Latitude": 42.0849,
    "Longitude": -71.1907
  },
  {
    "Date": "2026-09-13",
    "Location": "Colesville, MD",
    "Host": "Red Huskies",
    "TrialTypes": "ELT-S, L3C, NW3",
    "EventCount": 3,
    "Latitude": 39.0591,
    "Longitude": -76.953
  },
  {
    "Date": "2026-09-18",
    "Location": "Flint, MI",
    "Host": "Every Dog Nosework",
    "TrialTypes": "NW3, NW1, NW2, ELT-P",
    "EventCount": 4,
    "Latitude": 43.058,
    "Longitude": -83.7234
  },
  {
    "Date": "2026-09-18",
    "Location": "New Milford, PA",
    "Host": "Your Dog's Place, LLC",
    "TrialTypes": "ELT, ELT-S, L1V",
    "EventCount": 3,
    "Latitude": 41.8899,
    "Longitude": -75.75
  },
  {
    "Date": "2026-09-19",
    "Location": "Ford City, PA",
    "Host": "Steel City Nosework, LLC",
    "TrialTypes": "ELT, ELT-S, NW2",
    "EventCount": 3,
    "Latitude": 40.7394,
    "Longitude": -79.5622
  },
  {
    "Date": "2026-09-19",
    "Location": "Glen Mills, PA",
    "Host": "Firezone GS",
    "TrialTypes": "NW3, ELT-P",
    "EventCount": 2,
    "Latitude": 39.8781,
    "Longitude": -75.4414
  },
  {
    "Date": "2026-09-19",
    "Location": "Palmer, MA",
    "Host": "HeavenScent Sniffers",
    "TrialTypes": "NW3, L2V, L1E",
    "EventCount": 3,
    "Latitude": 42.1815,
    "Longitude": -72.3404
  },
  {
    "Date": "2026-09-19",
    "Location": "Stevenson, WA",
    "Host": "Sharon Smith",
    "TrialTypes": "NW1, NW2, L1V, L1C",
    "EventCount": 4,
    "Latitude": 45.7259,
    "Longitude": -121.863
  },
  {
    "Date": "2026-09-25",
    "Location": "Frederick, MD",
    "Host": "Red Huskies",
    "EventLink": "https://nosework.redhuskies.com/index.php/events/frederick_fall2026/",
    "TrialTypes": "L3E, ELT-S, ELT-P, ELT",
    "EventCount": 4,
    "Latitude": 39.4637,
    "Longitude": -77.3899
  },
  {
    "Date": "2026-09-26",
    "Location": "Columbus, MT",
    "Host": "Canine Connection",
    "EventLink": "https://canineconnection23.godaddysites.com/2026-trials",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 45.6027,
    "Longitude": -109.2628
  },
  {
    "Date": "2026-09-26",
    "Location": "Glenview, IL",
    "Host": "Northwest Obedience Club Inc",
    "EventLink": "https://northwestobedienceclub.org/event/noci-nacsw-elite-trial/",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 42.0501,
    "Longitude": -87.7685
  },
  {
    "Date": "2026-09-26",
    "Location": "Kintnersville, PA",
    "Host": "Paws n’ Sniff",
    "EventLink": "http://www.pawsnsniff.com/september-26-27.-2026.html",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 40.5315,
    "Longitude": -75.1642
  },
  {
    "Date": "2026-09-26",
    "Location": "Lawrenceville, GA",
    "Host": "Right Choice Dog Training, LLC",
    "EventLink": "https://www.rightchoicedogtraining.net/eventandvolunteer",
    "TrialTypes": "L1E, NW2, ELT-S, L3I",
    "EventCount": 4,
    "Latitude": 33.9483,
    "Longitude": -83.9437
  },
  {
    "Date": "2026-09-26",
    "Location": "New City, NY",
    "Host": "Saints2Source, LLC",
    "EventLink": "https://www.saints2source.com/",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 41.17,
    "Longitude": -73.9585
  },
  {
    "Date": "2026-09-26",
    "Location": "Rehoboth, MA",
    "Host": "Dogs Make Scents",
    "EventLink": "https://dogsmakescents.com/",
    "TrialTypes": "NW3, NW1",
    "EventCount": 2,
    "Latitude": 41.8542,
    "Longitude": -71.2866
  },
  {
    "Date": "2026-09-27",
    "Location": "Dover, DE",
    "Host": "Patricia Grassey",
    "EventLink": "https://thesniffinghound.com/about",
    "TrialTypes": "NW3, NW2, ELT",
    "EventCount": 3,
    "Latitude": 39.1358,
    "Longitude": -75.5362
  },
  {
    "Date": "2026-09-28",
    "Location": "Concord, NH",
    "Host": "For the Love of Dogs NY LLC",
    "EventLink": "https://www.fortheloveofdogsny.com/trials-events",
    "TrialTypes": "NW2, NW3",
    "EventCount": 2,
    "Latitude": 43.1924,
    "Longitude": -71.5151
  },
  {
    "Date": "2026-10-03",
    "Location": "Hagerstown, MD",
    "Host": "Firezone GS",
    "EventLink": "https://www.firezonegiantschnauzers.com/nose-work-trials",
    "TrialTypes": "ELT-S, NW3, ELT",
    "EventCount": 3,
    "Latitude": 39.6491,
    "Longitude": -77.6959
  },
  {
    "Date": "2026-10-03",
    "Location": "Hammond, LA",
    "Host": "Dog Gone Right, LLC",
    "EventLink": "https://doggoneright.net/",
    "TrialTypes": "NW1, NW2, L1I, ELT-S",
    "EventCount": 4,
    "Latitude": 30.4756,
    "Longitude": -90.4239
  },
  {
    "Date": "2026-10-03",
    "Location": "Jefferson, OR",
    "Host": "Doglandia, LLC",
    "EventLink": "https://www.cyberdogonline.com/",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 44.6019,
    "Longitude": -121.2565
  },
  {
    "Date": "2026-10-03",
    "Location": "Nashua, NH",
    "Host": "The Big Sniff, LLC",
    "EventLink": "http://www.thebigsniff.com/",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 42.7373,
    "Longitude": -71.5071
  },
  {
    "Date": "2026-10-03",
    "Location": "New Paltz, NY",
    "Host": "Top Notch Dogs, LLC",
    "EventLink": "https://www.topnotchdogtraining.com/",
    "TrialTypes": "NW1, L2C, ELT",
    "EventCount": 3,
    "Latitude": 41.7921,
    "Longitude": -74.0511
  },
  {
    "Date": "2026-10-03",
    "Location": "Northampton, MA",
    "Host": "Lucky Dog Events",
    "EventLink": "https://www.luckydogevents.com/",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 42.3056,
    "Longitude": -72.61
  },
  {
    "Date": "2026-10-03",
    "Location": "Seguin, TX",
    "Host": "Scent Work Across Texas",
    "EventLink": "https://scentworkacrosstexas.com/",
    "TrialTypes": "NW1, NW2, NW3",
    "EventCount": 3,
    "Latitude": 29.5744,
    "Longitude": -97.9922
  },
  {
    "Date": "2026-10-03",
    "Location": "Sisters, OR",
    "Host": "Sunriver K9 Genie, LLC",
    "EventLink": "https://k9genie.com/events",
    "TrialTypes": "NW2, ELT",
    "EventCount": 2,
    "Latitude": 44.296,
    "Longitude": -121.5273
  },
  {
    "Date": "2026-10-03",
    "Location": "Waynesboro, PA",
    "Host": "Nose-It-All, LLC",
    "EventLink": "https://www.nose-it-all.com/",
    "TrialTypes": "NW1, NW2, NW3",
    "EventCount": 3,
    "Latitude": 39.787,
    "Longitude": -77.6251
  },
  {
    "Date": "2026-10-09",
    "Location": "Middlebury, CT",
    "Host": "For the Love of Dogs NY LLC",
    "EventLink": "https://www.fortheloveofdogsny.com/trials-events",
    "TrialTypes": "ELT, ELT-P, NW1",
    "EventCount": 3,
    "Latitude": 41.4958,
    "Longitude": -73.0847
  },
  {
    "Date": "2026-10-09",
    "Location": "Pueblo, CO",
    "Host": "Mountain Dogs, LLC",
    "EventLink": "https://mountaindogs.org/",
    "TrialTypes": "SMT, ELT",
    "EventCount": 2,
    "Latitude": 38.2974,
    "Longitude": -104.6534
  },
  {
    "Date": "2026-10-09",
    "Location": "Rock Island, IL",
    "Host": "Fur Better Fur Worse Dog Training",
    "EventLink": "http://www.furbetterfurworse.com/",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 41.412,
    "Longitude": -90.5532
  },
  {
    "Date": "2026-10-10",
    "Location": "Auburn, WA",
    "Host": "Northwest K9 Sniffers",
    "EventLink": "https://nwk9sniffers.org/",
    "TrialTypes": "ELT-S, L2E, L3I",
    "EventCount": 3,
    "Latitude": 47.2969,
    "Longitude": -122.2396
  },
  {
    "Date": "2026-10-10",
    "Location": "Court Granger, IA",
    "Host": "KBP Dog Training",
    "EventLink": "https://kbpdogtraining.com",
    "TrialTypes": "NW1, NW2, NW3",
    "EventCount": 3,
    "Latitude": 41.7397,
    "Longitude": -93.7843
  },
  {
    "Date": "2026-10-10",
    "Location": "Eagan, MN",
    "Host": "St Paul Dog Training Club",
    "EventLink": "https://spdtc.com/events-at-spdtc/",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 44.7811,
    "Longitude": -93.1921
  },
  {
    "Date": "2026-10-10",
    "Location": "Eldred, NY",
    "Host": "Your Dog's Place, LLC",
    "EventLink": "http://www.yourdogsplace.com/",
    "TrialTypes": "L2C, NW2, ELT-S, L1C",
    "EventCount": 4,
    "Latitude": 41.5692,
    "Longitude": -74.8449
  },
  {
    "Date": "2026-10-10",
    "Location": "Helena, MT",
    "Host": "Nose Work Breakfast Club",
    "EventLink": "https://noseworkbreakfastclub.com/our-events/",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 46.592,
    "Longitude": -112.0038
  },
  {
    "Date": "2026-10-10",
    "Location": "Loveland, CO",
    "Host": "Paws 4 Thought Dog Training, LLC",
    "EventLink": "https://www.p4tnosework.com/premiumloveland",
    "TrialTypes": "NW2, NW1, L1E",
    "EventCount": 3,
    "Latitude": 40.3731,
    "Longitude": -105.0565
  },
  {
    "Date": "2026-10-10",
    "Location": "Sedona, AZ",
    "Host": "Successful Sniffer",
    "EventLink": "https://www.successfulsniffer.com/trials-and-events",
    "TrialTypes": "ELT, NW1, NW2",
    "EventCount": 3,
    "Latitude": 34.8251,
    "Longitude": -111.745
  },
  {
    "Date": "2026-10-10",
    "Location": "Troy, VA",
    "Host": "Your Dogs Knows LLC",
    "EventLink": "https://yourdogknows.net/",
    "TrialTypes": "NW1, ELT-S, L1V, L2V",
    "EventCount": 4,
    "Latitude": 37.9359,
    "Longitude": -78.2679
  },
  {
    "Date": "2026-10-10",
    "Location": "Youngwood, PA",
    "Host": "Steel City Nosework, LLC",
    "EventLink": "https://www.nose-it-all.com/",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 40.2685,
    "Longitude": -79.5749
  },
  {
    "Date": "2026-10-12",
    "Location": "Swansea, MA",
    "Host": "Amy Conrad & Heaven Scent Sniffers",
    "EventLink": "https://sniffstreams.smugmug.com/Events",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 41.7846,
    "Longitude": -71.1972
  },
  {
    "Date": "2026-10-16",
    "Location": "Calhan, CO",
    "Host": "Mountain Dogs LLC",
    "EventLink": "https://mountaindogs.org/",
    "TrialTypes": "ELT, NW3, ELT-P",
    "EventCount": 3,
    "Latitude": 39.0797,
    "Longitude": -104.3153
  },
  {
    "Date": "2026-10-16",
    "Location": "Rossville, GA",
    "Host": "Camelot Shepherds, Inc.",
    "EventLink": "https://www.snifferschool.com/events",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 35.0194,
    "Longitude": -85.2484
  },
  {
    "Date": "2026-10-16",
    "Location": "Wilmington, DE",
    "Host": "Patricia Grassey",
    "EventLink": "https://thesniffinghound.com/about",
    "TrialTypes": "NW3, ELT, ELT-P",
    "EventCount": 3,
    "Latitude": 39.7805,
    "Longitude": -75.5315
  },
  {
    "Date": "2026-10-17",
    "Location": "Albuquerque, NM",
    "Host": "New Mexico Canine Scent Work, LLC",
    "EventLink": "https://www.nmcsw.com/events/#oct26",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 35.0778,
    "Longitude": -106.6022
  },
  {
    "Date": "2026-10-17",
    "Location": "Centralia, WA",
    "Host": "Let's Talk Dogs, LLC and About Face K9",
    "EventLink": "https://dorothyturley.com/trials-and-orts/",
    "TrialTypes": "ELT-S, NW2, L3C",
    "EventCount": 3,
    "Latitude": 46.7609,
    "Longitude": -122.9549
  },
  {
    "Date": "2026-10-17",
    "Location": "Colebrook, CT",
    "Host": "For the Love of Dogs NY LLC",
    "EventLink": "https://www.fortheloveofdogsny.com/nacsw-trials",
    "TrialTypes": "L1E, ELT-S, NW2, ELT",
    "EventCount": 4,
    "Latitude": 41.9518,
    "Longitude": -73.0606
  },
  {
    "Date": "2026-10-17",
    "Location": "Conroe, TX",
    "Host": "Scent Work Across Texas",
    "EventLink": "https://scentworkacrosstexas.com/",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 30.3049,
    "Longitude": -95.4541
  },
  {
    "Date": "2026-10-17",
    "Location": "Delevan, NY",
    "Host": "Do Over Dog Training",
    "EventLink": "https://www.dooverdogtraining.com/trials",
    "TrialTypes": "NW3, L1C, L3V",
    "EventCount": 3,
    "Latitude": 42.5339,
    "Longitude": -78.5145
  },
  {
    "Date": "2026-10-17",
    "Location": "Niantic, IL",
    "Host": "Kudos for Canines, LLC",
    "EventLink": "https://kudosforcanines.com/",
    "TrialTypes": "L2C, ELT-S, NW3",
    "EventCount": 3,
    "Latitude": 39.828,
    "Longitude": -89.1306
  },
  {
    "Date": "2026-10-17",
    "Location": "Staples, MN",
    "Host": "Nose 2 Tail Dog Training LLC",
    "EventLink": "https://nose2tail.net/nacsw-nw3-elite-2/",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 46.329,
    "Longitude": -94.7837
  },
  {
    "Date": "2026-10-17",
    "Location": "Watsonville, CA",
    "Host": "CalCoastal Dog Owners Group",
    "EventLink": "https://cc-dog.org/",
    "TrialTypes": "L3V, L2V, L1V",
    "EventCount": 3,
    "Latitude": 36.8687,
    "Longitude": -121.7198
  },
  {
    "Date": "2026-10-24",
    "Location": "Cumming, GA",
    "Host": "Georgia Nosework",
    "EventLink": "https://georgianosework.com/",
    "TrialTypes": "NW3, L1C, NW2",
    "EventCount": 3,
    "Latitude": 34.1873,
    "Longitude": -84.1807
  },
  {
    "Date": "2026-10-24",
    "Location": "Fishkill, NY",
    "Host": "Top Notch Dogs, LLC",
    "EventLink": "https://www.topnotchdogtraining.com",
    "TrialTypes": "ELT-P, ELT",
    "EventCount": 2,
    "Latitude": 41.5155,
    "Longitude": -73.9456
  },
  {
    "Date": "2026-10-24",
    "Location": "Green Bay, WI",
    "Host": "NEWk9Scent Work LLC",
    "EventLink": "https://newk9scentwork.com/nose-work-trials-2",
    "TrialTypes": "NW2, ELT",
    "EventCount": 2,
    "Latitude": 44.5162,
    "Longitude": -87.9853
  },
  {
    "Date": "2026-10-24",
    "Location": "Kilmarnock, VA",
    "Host": "Red Huskies",
    "EventLink": "https://nosework.redhuskies.com/",
    "TrialTypes": "ELT, NW1, ELT-S",
    "EventCount": 3,
    "Latitude": 37.6882,
    "Longitude": -76.4158
  },
  {
    "Date": "2026-10-24",
    "Location": "Norton, MA",
    "Host": "Dogs Make Scents",
    "EventLink": "https://dogsmakescents.com/events/",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 41.9221,
    "Longitude": -71.1578
  },
  {
    "Date": "2026-10-24",
    "Location": "Penn Yan, NY",
    "Host": "2 Psyched 4 Dogs",
    "EventLink": "https://2psyched4dogs.com/",
    "TrialTypes": "ELT, ELT-S, NW2",
    "EventCount": 3,
    "Latitude": 42.6973,
    "Longitude": -77.0219
  },
  {
    "Date": "2026-10-24",
    "Location": "Reedsport, OR",
    "Host": "Wells Creek Dog Training",
    "EventLink": "https://wellscreekdogtraining.com/",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 43.7099,
    "Longitude": -124.0839
  },
  {
    "Date": "2026-10-26",
    "Location": "Stockton, CA",
    "Host": "Two Nosey Girls",
    "EventLink": "https://twonoseygirls.com/",
    "TrialTypes": "L3E, L2E",
    "EventCount": 2,
    "Latitude": 37.9241,
    "Longitude": -121.3212
  },
  {
    "Date": "2026-10-28",
    "Location": "Aberdeen, MD",
    "Host": "Red Huskies",
    "EventLink": "https://nosework.redhuskies.com/",
    "TrialTypes": "ELT, ELT-P, ELT-S, L2I",
    "EventCount": 4,
    "Latitude": 39.5429,
    "Longitude": -76.1641
  },
  {
    "Date": "2026-10-30",
    "Location": "Cameron Park, CA",
    "Host": "Sierra Sniffing Canines, Inc",
    "EventLink": "https://sierrasniffingcanines.org/",
    "TrialTypes": "NW1, NW3",
    "EventCount": 2,
    "Latitude": 38.7063,
    "Longitude": -120.9529
  },
  {
    "Date": "2026-10-30",
    "Location": "Harrington, DE",
    "Host": "Shamrock Pot Of Gold K9 Scenter",
    "EventLink": "https://shamrockpotofgoldk9scenter.com/",
    "TrialTypes": "NW3, ELT, NW1, ELT-S, L2E",
    "EventCount": 5,
    "Latitude": 38.9399,
    "Longitude": -75.5546
  },
  {
    "Date": "2026-10-30",
    "Location": "Honey Brook, PA",
    "Host": "Patricia Grassey",
    "EventLink": "https://thesniffinghound.com/about",
    "TrialTypes": "NW1, L1C, L2C, NW2, L3I, L3C",
    "EventCount": 6,
    "Latitude": 40.0932,
    "Longitude": -75.8937
  },
  {
    "Date": "2026-10-30",
    "Location": "Lakeville, MN",
    "Host": "St Paul Dog Training Club",
    "EventLink": "https://spdtc.com/events-at-spdtc/",
    "TrialTypes": "ELT-P, NW2, ELT-S, L1C",
    "EventCount": 4,
    "Latitude": 44.6852,
    "Longitude": -93.2615
  },
  {
    "Date": "2026-10-30",
    "Location": "Lawrenceville, GA",
    "Host": "Chestnut Hill Canine Sports",
    "EventLink": "http://chestnuthillcaninesports.com/lawrenceville-2025/",
    "TrialTypes": "NW3, NW1, L2I",
    "EventCount": 3,
    "Latitude": 33.9851,
    "Longitude": -83.9803
  },
  {
    "Date": "2026-10-30",
    "Location": "Montrose, CO",
    "Host": "Mountain Dogs LLC",
    "EventLink": "https://mountaindogs.org/",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 38.5114,
    "Longitude": -107.8744
  },
  {
    "Date": "2026-10-30",
    "Location": "York, PA",
    "Host": "Firezone GS",
    "EventLink": "https://www.firezonegiantschnauzers.com/nose-work-trials",
    "TrialTypes": "ELT, NW3, ELT-P",
    "EventCount": 3,
    "Latitude": 39.9339,
    "Longitude": -76.7361
  },
  {
    "Date": "2026-10-31",
    "Location": "Beloit, WI",
    "Host": "George Carpenter",
    "EventLink": "https://gscarpenter.wixsite.com/scwnw/trials",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 42.4898,
    "Longitude": -89.0021
  },
  {
    "Date": "2026-10-31",
    "Location": "Bonham, TX",
    "Host": "All About The Nose",
    "EventLink": "https://www.allaboutthenose.com/",
    "TrialTypes": "NW2, NW1",
    "EventCount": 2,
    "Latitude": 33.5617,
    "Longitude": -96.1984
  },
  {
    "Date": "2026-10-31",
    "Location": "Franklin, GA",
    "Host": "Hold The Line K9 LLC",
    "EventLink": "https://www.holdthelinek9nosework.com/",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 34.4063,
    "Longitude": -83.2394
  },
  {
    "Date": "2026-10-31",
    "Location": "Kennebunkport, ME",
    "Host": "Elizabeth Dutton",
    "EventLink": "https://ehdutton.wordpress.com/",
    "TrialTypes": "NW3, ELT-P",
    "EventCount": 2,
    "Latitude": 43.3428,
    "Longitude": -70.5165
  },
  {
    "Date": "2026-10-31",
    "Location": "Plant City, FL",
    "Host": "Hoppin’ in the Hills",
    "EventLink": "https://hoppininthehillscom.wordpress.com",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 28.0366,
    "Longitude": -82.1023
  },
  {
    "Date": "2026-10-31",
    "Location": "Sturgis, SD",
    "Host": "Two Paws Up Dog Training, LLC",
    "EventLink": "https://www.twopawsupdogtrainingllc.com/events",
    "TrialTypes": "L2V, L2E, L1V, L1E",
    "EventCount": 4,
    "Latitude": 44.4555,
    "Longitude": -103.5075
  },
  {
    "Date": "2026-10-31",
    "Location": "White Plains, NY",
    "Host": "Saints2Source, LLC",
    "EventLink": "https://www.saints2source.com/copy-of-new-city-ny-oct-2025",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 41.0442,
    "Longitude": -73.741
  },
  {
    "Date": "2026-10-31",
    "Location": "Yamhill, OR",
    "Host": "Nose Work Detectives, LLC",
    "EventLink": "https://noseworkdetectives.com/",
    "TrialTypes": "ELT-P",
    "EventCount": 1,
    "Latitude": 45.262,
    "Longitude": -123.2576
  },
  {
    "Date": "2026-11-01",
    "Location": "San Martin, CA",
    "Host": "B.L. McMutts LLC",
    "EventLink": "https://blmcmutts.com/events/nacsw-element-specialty-trial-nov26",
    "TrialTypes": "L1V, L2V",
    "EventCount": 2,
    "Latitude": 37.0568,
    "Longitude": -121.6393
  },
  {
    "Date": "2026-11-03",
    "Location": "Ventura, CA",
    "Host": "Pink Biscuit K9s",
    "EventLink": "https://www.pinkbiscuitk9s.com/arnaz-25-premium.html",
    "TrialTypes": "NW1, NW2, ELT-P",
    "EventCount": 3,
    "Latitude": 34.4251,
    "Longitude": -119.0762
  },
  {
    "Date": "2026-11-06",
    "Location": "Rome, GA",
    "Host": "Georgia Nosework, LLC",
    "EventLink": "https://georgianosework.com/events/",
    "TrialTypes": "NW3, NW1, NW2, ELT",
    "EventCount": 4,
    "Latitude": 34.2826,
    "Longitude": -85.194
  },
  {
    "Date": "2026-11-07",
    "Location": "Bonner Springs, KS",
    "Host": "Brookside Pet Concierge",
    "EventLink": "https://bksdogtraining.com/",
    "TrialTypes": "NW1, NW2, NW3",
    "EventCount": 3,
    "Latitude": 39.0298,
    "Longitude": -94.9075
  },
  {
    "Date": "2026-11-07",
    "Location": "Colorado Springs, CO",
    "Host": "Beyond Elevation K9",
    "EventLink": "https://www.beyondelevationk9.com/",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 38.8223,
    "Longitude": -104.869
  },
  {
    "Date": "2026-11-07",
    "Location": "Fishkill, NY",
    "Host": "Top Notch Dogs, LLC",
    "EventLink": "https://www.topnotchdogtraining.com/",
    "TrialTypes": "L3C, NW2, ELT",
    "EventCount": 3,
    "Latitude": 41.5335,
    "Longitude": -73.8643
  },
  {
    "Date": "2026-11-07",
    "Location": "Geneva, IL",
    "Host": "For Your K9, Inc",
    "EventLink": "http://www.foryourk9.com/",
    "TrialTypes": "ELT, NW1, NW2",
    "EventCount": 3,
    "Latitude": 41.8681,
    "Longitude": -88.2992
  },
  {
    "Date": "2026-11-07",
    "Location": "Guerneville, CA",
    "Host": "Jen Huot",
    "EventLink": "https://k9noseworkacademy.com/",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 38.5488,
    "Longitude": -122.9282
  },
  {
    "Date": "2026-11-07",
    "Location": "Lancaster, PA",
    "Host": "Red Huskies Nose Work, LLC",
    "EventLink": "https://nosework.redhuskies.com/",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 40.0288,
    "Longitude": -76.3197
  },
  {
    "Date": "2026-11-07",
    "Location": "Las Vegas, NV",
    "Host": "imPETus Animal Training",
    "EventLink": "https://www.impetusanimaltraining.com/",
    "TrialTypes": "NW3, NW1, L1C",
    "EventCount": 3,
    "Latitude": 36.1926,
    "Longitude": -115.1834
  },
  {
    "Date": "2026-11-07",
    "Location": "Mays Landing, NJ",
    "Host": "Rotts-n-Notts Nosework LLC",
    "EventLink": "https://www.rottsnnottsnosework.com/",
    "TrialTypes": "L1C, NW2, L1E, NW1",
    "EventCount": 4,
    "Latitude": 39.5002,
    "Longitude": -74.7438
  },
  {
    "Date": "2026-11-07",
    "Location": "Woodward, IA",
    "Host": "KBP Dog Training",
    "EventLink": "https://kbpdogtraining.com/202611-nw3-elt/",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 41.8082,
    "Longitude": -93.8978
  },
  {
    "Date": "2026-11-09",
    "Location": "West Berlin, NJ",
    "Host": "For the Love of Dogs NY LLC",
    "EventLink": "https://www.fortheloveofdogsny.com/trials-events",
    "TrialTypes": "L2E, L1V, NW3, ELT-P",
    "EventCount": 4,
    "Latitude": 39.8308,
    "Longitude": -74.9254
  },
  {
    "Date": "2026-11-11",
    "Location": "Petaluma, CA",
    "Host": "Marin Humane",
    "EventLink": "https://marinhumane.org/oh-behave/nose-work-events/",
    "TrialTypes": "ELT-P, ELT",
    "EventCount": 2,
    "Latitude": 38.2421,
    "Longitude": -122.6361
  },
  {
    "Date": "2026-11-13",
    "Location": "Chula Vista, CA",
    "Host": "Rewarding Rover LLC, Uberdog, & Claire Brocato",
    "EventLink": "https://www.rewardingrover.com/",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 32.638,
    "Longitude": -117.1261
  },
  {
    "Date": "2026-11-13",
    "Location": "Gilbertsville, PA",
    "Host": "Sniff Sniff Hooray",
    "EventLink": "https://sniffsniffhooray.com/events",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 40.3214,
    "Longitude": -75.5804
  },
  {
    "Date": "2026-11-13",
    "Location": "Ypsilanti, MI",
    "Host": "Every Dog Nosework",
    "EventLink": "https://everydognosework.com/trials",
    "TrialTypes": "ELT, ELT-S, L2C",
    "EventCount": 3,
    "Latitude": 42.2778,
    "Longitude": -83.5999
  },
  {
    "Date": "2026-11-14",
    "Location": "Coburg, OR",
    "Host": "Kiddy Christie",
    "EventLink": "https://wellscreekdogtraining.com/",
    "TrialTypes": "NW1, NW2, L2I, L3C",
    "EventCount": 4,
    "Latitude": 44.1636,
    "Longitude": -123.0862
  },
  {
    "Date": "2026-11-14",
    "Location": "Greer, SC",
    "Host": "Trained to Trust, LLC",
    "EventLink": "https://www.k9trainedtotrust.com/events",
    "TrialTypes": "NW3, L2V, NW1",
    "EventCount": 3,
    "Latitude": 34.9372,
    "Longitude": -82.2277
  },
  {
    "Date": "2026-11-14",
    "Location": "Montgomery, AL",
    "Host": "By A Nose Nosework",
    "EventLink": "https://www.byanosenosework.com/event-details/montgomery-al-elt-nw3",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 32.3701,
    "Longitude": -86.3031
  },
  {
    "Date": "2026-11-14",
    "Location": "Waymart, PA",
    "Host": "Your Dog's Place, LLC",
    "EventLink": "http://www.yourdogsplace.com/",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 41.5366,
    "Longitude": -75.4466
  },
  {
    "Date": "2026-11-16",
    "Location": "Ellicott City, MD",
    "Host": "Red Huskies",
    "EventLink": "https://nosework.redhuskies.com/",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 39.2585,
    "Longitude": -76.8213
  },
  {
    "Date": "2026-11-16",
    "Location": "Hartford, CT",
    "Host": "For the Love of Dogs NY LLC",
    "EventLink": "https://www.fortheloveofdogsny.com/trials-events",
    "TrialTypes": "L1I, ELT-S, ELT",
    "EventCount": 3,
    "Latitude": 41.7347,
    "Longitude": -72.7343
  },
  {
    "Date": "2026-11-18",
    "Location": "Monkton, MD",
    "Host": "Firezone GS",
    "EventLink": "https://firezonegiantschnauzers.com/nose-work-trials",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 39.6268,
    "Longitude": -76.6253
  },
  {
    "Date": "2026-11-20",
    "Location": "Boring, OR",
    "Host": "Trust Your Dog K9 Events",
    "EventLink": "https://trustyourdogk9events.com/",
    "TrialTypes": "NW2, NW3, ELT",
    "EventCount": 3,
    "Latitude": 45.4546,
    "Longitude": -122.3409
  },
  {
    "Date": "2026-11-20",
    "Location": "Centreville, MD",
    "Host": "Fair Play Point Labradors",
    "EventLink": "https://www.fairplaylabradors.com/",
    "TrialTypes": "SMT, ELT-S, L1I",
    "EventCount": 3,
    "Latitude": 39.0911,
    "Longitude": -76.0934
  },
  {
    "Date": "2026-11-20",
    "Location": "Denver, PA",
    "Host": "Patricia Grassey",
    "EventLink": "https://thesniffinghound.com/about",
    "TrialTypes": "ELT, ELT-P, ELT-S, L3V",
    "EventCount": 4,
    "Latitude": 40.1919,
    "Longitude": -76.0932
  },
  {
    "Date": "2026-11-20",
    "Location": "Kintnersville , PA",
    "Host": "Paws n' Sniff",
    "EventLink": "http://www.pawsnsniff.com/",
    "TrialTypes": "NW1, L1C, L1E, L1I",
    "EventCount": 4,
    "Latitude": 40.5228,
    "Longitude": -75.1611
  },
  {
    "Date": "2026-11-20",
    "Location": "Lompoc, CA",
    "Host": "Gentle Touch Pet Training",
    "EventLink": "https://www.gentlepets.com/gtpt-events/nacsw%E2%84%A2-nw3",
    "TrialTypes": "NW3, NW2, NW1",
    "EventCount": 3,
    "Latitude": 34.6295,
    "Longitude": -120.4966
  },
  {
    "Date": "2026-11-20",
    "Location": "Loranger, LA",
    "Host": "Dog Gone Right",
    "EventLink": "http://www.doggoneright.net/",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 30.5868,
    "Longitude": -90.4043
  },
  {
    "Date": "2026-11-21",
    "Location": "Centralia, WA",
    "Host": "Let's Talk Dogs, LLC & About Face K9 Academy",
    "EventLink": "https://www.aboutfacek9academy.com/",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 46.7611,
    "Longitude": -123.0094
  },
  {
    "Date": "2026-11-21",
    "Location": "DeLeon Springs, FL",
    "Host": "River Poodles Training, LLC",
    "EventLink": "https://riverpoodlestraining.com/",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 29.1037,
    "Longitude": -81.3698
  },
  {
    "Date": "2026-11-21",
    "Location": "Delta, CO",
    "Host": "Mountain Dogs LLC",
    "EventLink": "https://mountaindogs.org/",
    "TrialTypes": "ELT, NW3, NW1, NW2",
    "EventCount": 4,
    "Latitude": 38.8448,
    "Longitude": -107.8577
  },
  {
    "Date": "2026-11-21",
    "Location": "Fork Union, VA",
    "Host": "Your Dog Knows, LLC",
    "EventLink": "https://yourdogknows.net/",
    "TrialTypes": "ELT, L2I, NW2",
    "EventCount": 3,
    "Latitude": 37.7343,
    "Longitude": -78.2619
  },
  {
    "Date": "2026-11-21",
    "Location": "Marble Falls, TX",
    "Host": "Scent Work Across Texas",
    "EventLink": "https://scentworkacrosstexas.com/",
    "TrialTypes": "ELT-S, L2I, NW3",
    "EventCount": 3,
    "Latitude": 30.5634,
    "Longitude": -98.2889
  },
  {
    "Date": "2026-11-21",
    "Location": "Ontario, CA",
    "Host": "Agile Paws Dog Sports",
    "EventLink": "https://agilepawsdogsports.com/",
    "TrialTypes": "NW1, L3I, L3C",
    "EventCount": 3,
    "Latitude": 34.0664,
    "Longitude": -117.6769
  },
  {
    "Date": "2026-11-21",
    "Location": "Smyrna, TN",
    "Host": "Dogs Have Amazing Noses, LLC",
    "EventLink": "https://dogshaveamazingnoses.com/events/",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 36.0089,
    "Longitude": -86.4832
  },
  {
    "Date": "2026-11-22",
    "Location": "Wilbraham, MA",
    "Host": "Heaven Scent Sniffers",
    "EventLink": "https://www.heavenscentsniffers.com/",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 42.0728,
    "Longitude": -72.422
  },
  {
    "Date": "2026-11-27",
    "Location": "Elizabeth, CO",
    "Host": "Beyond Elevation K9",
    "EventLink": "https://www.beyondelevationk9.com/",
    "TrialTypes": "NW2, NW3",
    "EventCount": 2,
    "Latitude": 39.3226,
    "Longitude": -104.5869
  },
  {
    "Date": "2026-11-27",
    "Location": "Long Beach, CA",
    "Host": "JavaK9s, LLC",
    "EventLink": "http://www.javak9s.com/",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 33.7916,
    "Longitude": -118.1797
  },
  {
    "Date": "2026-11-27",
    "Location": "San Jose, CA",
    "Host": "The Bay Team",
    "EventLink": "https://www.bayteam.org/",
    "TrialTypes": "NW2, NW1",
    "EventCount": 2,
    "Latitude": 37.3754,
    "Longitude": -121.8578
  },
  {
    "Date": "2026-11-28",
    "Location": "Cottage Grove, MN",
    "Host": "Gretchen Hofheins-Wackerfuss",
    "EventLink": "https://www.sniffingminpin.com/",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 44.8721,
    "Longitude": -92.9103
  },
  {
    "Date": "2026-11-28",
    "Location": "Cumming, GA",
    "Host": "Georgia Nosework",
    "EventLink": "https://georgianosework.com/events/",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 34.1876,
    "Longitude": -84.1138
  },
  {
    "Date": "2026-11-28",
    "Location": "Lebanon, NJ",
    "Host": "Sirius K9 Solutions",
    "EventLink": "http://www.siriusk9solutions.net/NoseWork.html",
    "TrialTypes": "L2I, NW2, ELT-P",
    "EventCount": 3,
    "Latitude": 40.6703,
    "Longitude": -74.79
  },
  {
    "Date": "2026-11-28",
    "Location": "Mifflinburg, PA",
    "Host": "Paws-itively Obedient Dog Training School",
    "EventLink": "https://pawsitivelyobedient.net/",
    "TrialTypes": "NW3, NW1",
    "EventCount": 2,
    "Latitude": 40.9354,
    "Longitude": -77.0945
  },
  {
    "Date": "2026-11-28",
    "Location": "Silex, MO",
    "Host": "WestInn Kennels",
    "EventLink": "https://westinnkennels.wixsite.com/silex",
    "TrialTypes": "NW1, NW2, NW3",
    "EventCount": 3,
    "Latitude": 39.1197,
    "Longitude": -91.0067
  },
  {
    "Date": "2026-11-28",
    "Location": "Vancouver, WA",
    "Host": "Sniffketeers",
    "EventLink": "https://noseworktrial.blogspot.com/",
    "TrialTypes": "NW1, L1E, L1I, L3V",
    "EventCount": 4,
    "Latitude": 45.5854,
    "Longitude": -122.706
  },
  {
    "Date": "2026-11-29",
    "Location": "Gettysburg, PA",
    "Host": "Firezone GS",
    "EventLink": "https://www.firezonegiantschnauzers.com/nose-work-trials",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 39.8201,
    "Longitude": -77.2438
  },
  {
    "Date": "2026-12-05",
    "Location": "Centralia, WA",
    "Host": "Let's Talk Dogs, LLC and About Face K9",
    "EventLink": "http://www.dorothyturley.com/",
    "TrialTypes": "ELT, NW1, L2I",
    "EventCount": 3,
    "Latitude": 46.7181,
    "Longitude": -122.9815
  },
  {
    "Date": "2026-12-05",
    "Location": "Charlton, MA",
    "Host": "HeavenScent Sniffers",
    "EventLink": "https://www.heavenscentsniffers.com/",
    "TrialTypes": "NW2, NW1",
    "EventCount": 2,
    "Latitude": 42.1225,
    "Longitude": -72.0088
  },
  {
    "Date": "2026-12-05",
    "Location": "Fillmore, CA",
    "Host": "Pink Biscuit K9s",
    "EventLink": "https://www.pinkbiscuitk9s.com/",
    "TrialTypes": "NW3, ELT-S, L2C",
    "EventCount": 3,
    "Latitude": 34.3562,
    "Longitude": -118.8837
  },
  {
    "Date": "2026-12-05",
    "Location": "Fredonia, WI",
    "Host": "On Point Elite Dog Sports, LLC",
    "EventLink": "https://www.opedogsports.com/nacsw-trials",
    "TrialTypes": "NW1, NW2, NW3",
    "EventCount": 3,
    "Latitude": 43.4781,
    "Longitude": -87.9472
  },
  {
    "Date": "2026-12-05",
    "Location": "Hoover, AL",
    "Host": "Southeast Scent Work Alliance, LLC (SSWA)",
    "EventLink": "https://www.southeastscent.com/nw3-elite-hoover-al/",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 33.3474,
    "Longitude": -86.8594
  },
  {
    "Date": "2026-12-05",
    "Location": "Hubertus, WI",
    "Host": "Loving Paws Dog Training, LLC",
    "EventLink": "https://www.lovingpawsllc.com/premium-elt-elt",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 43.2173,
    "Longitude": -88.2491
  },
  {
    "Date": "2026-12-05",
    "Location": "Newfoundland, PA",
    "Host": "Your Dog's Place, LLC",
    "EventLink": "http://www.yourdogsplace.com/",
    "TrialTypes": "L2V, L3C, NW3",
    "EventCount": 3,
    "Latitude": 41.3516,
    "Longitude": -75.2766
  },
  {
    "Date": "2026-12-05",
    "Location": "Tecumseh, OK",
    "Host": "The Doggie Spot",
    "EventLink": "https://thedoggiespot.com/",
    "TrialTypes": "NW3, NW1",
    "EventCount": 2,
    "Latitude": 35.2134,
    "Longitude": -96.9696
  },
  {
    "Date": "2026-12-05",
    "Location": "Tucson, AZ",
    "Host": "Patience Unlimited Dog Training",
    "EventLink": "http://www.patienceunlimited.com/nacsw.html",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 32.1956,
    "Longitude": -110.9956
  },
  {
    "Date": "2026-12-05",
    "Location": "West River, MD",
    "Host": "Chesapeake Search Dogs",
    "EventLink": "https://chesapeakesearchdogs.org/",
    "TrialTypes": "ELT-S, L3E, NW1, NW2",
    "EventCount": 4,
    "Latitude": 38.8863,
    "Longitude": -76.6075
  },
  {
    "Date": "2026-12-07",
    "Location": "Stockton, CA",
    "Host": "Two Nosey Girls",
    "EventLink": "https://www.twonoseygirls.com/events.html",
    "TrialTypes": "ELT, ELT-S, L3I",
    "EventCount": 3,
    "Latitude": 38.002,
    "Longitude": -121.2968
  },
  {
    "Date": "2026-12-11",
    "Location": "Douglassville, PA",
    "Host": "Patricia Grassey",
    "EventLink": "https://www.thesniffinghound.com/",
    "TrialTypes": "NW3, NW2, ELT",
    "EventCount": 3,
    "Latitude": 40.2966,
    "Longitude": -75.7581
  },
  {
    "Date": "2026-12-12",
    "Location": "Deckers, CO",
    "Host": "Beyond Elevation K9",
    "EventLink": "https://www.beyondelevationk9.com/",
    "TrialTypes": "NW3, ELT-S, L2E",
    "EventCount": 3,
    "Latitude": 39.2195,
    "Longitude": -105.2241
  },
  {
    "Date": "2026-12-12",
    "Location": "Durham, NC",
    "Host": "Whole Dog Institute, LLC",
    "EventLink": "https://wholedoginstitute.com/",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 35.9579,
    "Longitude": -78.8711
  },
  {
    "Date": "2026-12-12",
    "Location": "Obetz, OH",
    "Host": "CleverDogs",
    "EventLink": "https://cleverdogsohio.com/",
    "TrialTypes": "NW1, NW2, ELT",
    "EventCount": 3,
    "Latitude": 39.887,
    "Longitude": -82.9963
  },
  {
    "Date": "2026-12-12",
    "Location": "Sauget, IL",
    "Host": "Happy Dog Concepts, LLC",
    "EventLink": "https://happydogconcepts.com/events",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 38.5684,
    "Longitude": -90.2124
  },
  {
    "Date": "2026-12-15",
    "Location": "Redlands, CA",
    "Host": "Agile Paws Dog Sports",
    "EventLink": "https://agilepawsdogsports.com/",
    "TrialTypes": "ELT, ELT-S",
    "EventCount": 2,
    "Latitude": 34.0402,
    "Longitude": -117.219
  },
  {
    "Date": "2026-12-18",
    "Location": "Pittstown, NJ",
    "Host": "Shamrock Pot Of Gold K9 Scenter",
    "EventLink": "https://shamrockpotofgoldk9scenter.com/",
    "TrialTypes": "ELT-S, L1C, NW3, ELT",
    "EventCount": 4,
    "Latitude": 40.5922,
    "Longitude": -74.9504
  },
  {
    "Date": "2026-12-19",
    "Location": "Alpharetta, GA",
    "Host": "Georgia Nosework",
    "EventLink": "https://georgianosework.com/",
    "TrialTypes": "SMT, ELT-P",
    "EventCount": 2,
    "Latitude": 34.0862,
    "Longitude": -84.2611
  },
  {
    "Date": "2026-12-19",
    "Location": "Imperial Beach, CA",
    "Host": "Rewarding Rover LLC/Uber dog/Claire Brocato",
    "EventLink": "https://www.rewardingrover.com/",
    "TrialTypes": "ELT, L1E, NW1",
    "EventCount": 3,
    "Latitude": 32.57,
    "Longitude": -117.1305
  },
  {
    "Date": "2026-12-19",
    "Location": "Salem, OR",
    "Host": "Doglandia, LLC",
    "EventLink": "https://www.cyberdogonline.com/",
    "TrialTypes": "NW3, ELT-P",
    "EventCount": 2,
    "Latitude": 44.9113,
    "Longitude": -123.0643
  },
  {
    "Date": "2026-12-27",
    "Location": "Exton, PA",
    "Host": "Patricia Grassey",
    "EventLink": "https://www.thesniffinghound.com/",
    "TrialTypes": "NW3, ELT-P, ELT-S, NW2",
    "EventCount": 4,
    "Latitude": 40.0118,
    "Longitude": -75.6751
  },
  {
    "Date": "2026-12-27",
    "Location": "Hartsdale, NY",
    "Host": "For the Love of Dogs NY LLC",
    "EventLink": "https://www.fortheloveofdogsny.com/trials-events",
    "TrialTypes": "L2I, L3I, NW3, ELT",
    "EventCount": 4,
    "Latitude": 41.0085,
    "Longitude": -73.8304
  },
  {
    "Date": "2026-12-27",
    "Location": "Waukesha, WI",
    "Host": "K9 Ventures",
    "EventLink": "https://www.k9ventureswi.net/",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 43.0932,
    "Longitude": -88.3343
  },
  {
    "Date": "2026-12-28",
    "Location": "Phoenix, AZ",
    "Host": "Release Canine LLC",
    "EventLink": "https://www.releasecanine.com/nacsw",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 33.4911,
    "Longitude": -112.0582
  },
  {
    "Date": "2026-12-28",
    "Location": "Tyngsborough, MA",
    "Host": "Spot On K9 Coaching",
    "EventLink": "https://www.sniffalertfinish.com/",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 42.6581,
    "Longitude": -71.4242
  },
  {
    "Date": "2026-12-29",
    "Location": "Duluth, GA",
    "Host": "Right Choice Dog Training, LLC",
    "EventLink": "https://www.rightchoicedogtraining.net/eventandvolunteer",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 34.0463,
    "Longitude": -84.1317
  },
  {
    "Date": "2026-12-31",
    "Location": "Corvallis, OR",
    "Host": "PNW Sniffers",
    "EventLink": "https://pnwsniffers.com/nye-trial",
    "TrialTypes": "NW3, NW1",
    "EventCount": 2,
    "Latitude": 44.5697,
    "Longitude": -123.292
  },
  {
    "Date": "2027-01-01",
    "Location": "Santa Rosa, CA",
    "Host": "Marin Humane",
    "EventLink": "https://marinhumane.org/oh-behave/nose-work-events/",
    "TrialTypes": "NW3, SMT",
    "EventCount": 2,
    "Latitude": 38.4788,
    "Longitude": -122.6768
  },
  {
    "Date": "2027-01-02",
    "Location": "Bonsall, CA",
    "Host": "Linda Buchanan",
    "EventLink": "https://www.k9slovetosearch.com/",
    "TrialTypes": "ELT, NW2",
    "EventCount": 2,
    "Latitude": 33.249,
    "Longitude": -117.1564
  },
  {
    "Date": "2027-01-03",
    "Location": "Bee Cave, TX",
    "Host": "Scent Work Across Texas",
    "EventLink": "https://scentworkacrosstexas.com/",
    "TrialTypes": "ELT-S, NW1, NW3",
    "EventCount": 3,
    "Latitude": 30.2642,
    "Longitude": -97.9433
  },
  {
    "Date": "2027-01-09",
    "Location": "Bellingham, WA",
    "Host": "The Nosework Magic",
    "EventLink": "https://www.noseworkmagic.com/",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 48.7934,
    "Longitude": -122.4537
  },
  {
    "Date": "2027-01-09",
    "Location": "Cape Coral, FL",
    "Host": "Your Dog Knows LLC",
    "EventLink": "https://yourdogknows.net/",
    "TrialTypes": "NW1, NW2, L1I, L1C",
    "EventCount": 4,
    "Latitude": 26.5722,
    "Longitude": -81.9039
  },
  {
    "Date": "2027-01-09",
    "Location": "Merced, CA",
    "Host": "Two Nosey Girls",
    "EventLink": "https://twonoseygirls.com/",
    "TrialTypes": "ELT-P, ELT",
    "EventCount": 2,
    "Latitude": 37.1862,
    "Longitude": -120.779
  },
  {
    "Date": "2027-01-09",
    "Location": "Novato, CA",
    "Host": "Marin Humane",
    "EventLink": "https://marinhumane.org/oh-behave/nose-work-events/",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 38.0839,
    "Longitude": -122.5881
  },
  {
    "Date": "2027-01-15",
    "Location": "Upland, CA",
    "Host": "Agile Paws Dog Sports",
    "EventLink": "https://agilepawsdogsports.com/",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 34.1345,
    "Longitude": -117.6816
  },
  {
    "Date": "2027-01-16",
    "Location": "Clanton, AL",
    "Host": "Daphne Melillo",
    "EventLink": "https://www.byanosenosework.com/",
    "TrialTypes": "NW2, ELT",
    "EventCount": 2,
    "Latitude": 32.8374,
    "Longitude": -86.6592
  },
  {
    "Date": "2027-01-16",
    "Location": "Melrose, FL",
    "Host": "River Poodles Training, LLC",
    "EventLink": "https://riverpoodlestraining.com/",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 29.7224,
    "Longitude": -82.0764
  },
  {
    "Date": "2027-01-16",
    "Location": "Purchase, NY",
    "Host": "For the Love of Dogs NY LLC",
    "EventLink": "https://www.fortheloveofdogsny.com/",
    "TrialTypes": "SMT, NW3",
    "EventCount": 2,
    "Latitude": 40.997,
    "Longitude": -73.7636
  },
  {
    "Date": "2027-01-19",
    "Location": "Murfreesboro, TN",
    "Host": "Dogs Have Amazing Noses, LLC",
    "EventLink": "https://dogshaveamazingnoses.com/events/",
    "TrialTypes": "ELT, ELT-S",
    "EventCount": 2,
    "Latitude": 35.8591,
    "Longitude": -86.4301
  },
  {
    "Date": "2027-01-23",
    "Location": "Montgomery, TX",
    "Host": "Nosy Dogs Houston",
    "EventLink": "http://www.nosydogshouston.com/",
    "TrialTypes": "NW1, L1C, L1V, NW2",
    "EventCount": 4,
    "Latitude": 30.2746,
    "Longitude": -95.4801
  },
  {
    "Date": "2027-01-23",
    "Location": "Valencia, CA",
    "Host": "Pink Biscuit K9s",
    "EventLink": "https://www.pinkbiscuitk9s.com/",
    "TrialTypes": "ELT-P, L2I, L3C",
    "EventCount": 3,
    "Latitude": 34.3823,
    "Longitude": -118.5166
  },
  {
    "Date": "2027-01-30",
    "Location": "Durham, NC",
    "Host": "Whole Dog Institute",
    "EventLink": "https://wholedoginstitute.com/",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 35.9505,
    "Longitude": -78.8755
  },
  {
    "Date": "2027-01-30",
    "Location": "Las Vegas, NV",
    "Host": "imPETus Animal Training",
    "EventLink": "http://impetusanimaltraining.com/",
    "TrialTypes": "NW3, L1I, NW2",
    "EventCount": 3,
    "Latitude": 36.1311,
    "Longitude": -115.1287
  },
  {
    "Date": "2027-01-30",
    "Location": "Petaluma, CA",
    "Host": "Seaside Sniffers",
    "EventLink": "https://www.seasidesniffers.com/",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 38.2511,
    "Longitude": -122.6574
  },
  {
    "Date": "2027-01-30",
    "Location": "San Marcos, CA",
    "Host": "Rewarding Rover LLC, Uberdog, & Claire Brocato",
    "EventLink": "https://www.rewardingrover.com/",
    "TrialTypes": "ELT-S, NW3",
    "EventCount": 2,
    "Latitude": 33.1546,
    "Longitude": -117.1344
  },
  {
    "Date": "2027-01-30",
    "Location": "Seguin, TX",
    "Host": "Sniff Happens",
    "EventLink": "https://www.sniffhappenstx.com/Jan-NACSW-Trial",
    "TrialTypes": "L2C, ELT-S, NW3",
    "EventCount": 3,
    "Latitude": 29.6185,
    "Longitude": -97.941
  },
  {
    "Date": "2027-02-06",
    "Location": "Murfreesboro, TN",
    "Host": "Dogs Have Amazing Noses, LLC",
    "EventLink": "https://dogshaveamazingnoses.com/events/",
    "TrialTypes": "NW2",
    "EventCount": 1,
    "Latitude": 35.8129,
    "Longitude": -86.4137
  },
  {
    "Date": "2027-02-13",
    "Location": "Modesto, CA",
    "Host": "Two Nosey Girls",
    "EventLink": "https://twonoseygirls.com/",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 37.6137,
    "Longitude": -121.033
  },
  {
    "Date": "2027-02-19",
    "Location": "Vista, CA",
    "Host": "Rewarding Rover LLC, Uber Dog and Claire Brocato",
    "EventLink": "https://www.rewardingrover.com/",
    "TrialTypes": "NW3, ELT-S, NW2",
    "EventCount": 3,
    "Latitude": 33.1549,
    "Longitude": -117.2446
  },
  {
    "Date": "2027-02-20",
    "Location": "Clarkesville, GA",
    "Host": "Right Choice Dog Training, LLC",
    "EventLink": "https://www.rightchoicedogtraining.net/eventandvolunteer",
    "TrialTypes": "L3I, NW2, ELT",
    "EventCount": 3,
    "Latitude": 34.5646,
    "Longitude": -83.5192
  },
  {
    "Date": "2027-02-21",
    "Location": "Benson, AZ",
    "Host": "Patience Unlimited Dog Training",
    "EventLink": "http://www.patienceunlimited.com/",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 31.9904,
    "Longitude": -110.2691
  },
  {
    "Date": "2027-02-22",
    "Location": "Paso Robles, CA",
    "Host": "Gentle Touch Pet Training",
    "EventLink": "https://www.gentlepets.com/gtpt-events/nacsw%E2%84%A2-elt%2Felt-trials",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 35.6762,
    "Longitude": -120.7214
  },
  {
    "Date": "2027-02-26",
    "Location": "McKinney, TX",
    "Host": "All About The Nose",
    "EventLink": "https://www.allaboutthenose.com/",
    "TrialTypes": "L1C, L1I, NW1, NW2",
    "EventCount": 4,
    "Latitude": 33.1873,
    "Longitude": -96.6558
  },
  {
    "Date": "2027-02-26",
    "Location": "Westlake Village, CA",
    "Host": "JavaK9s, LLC",
    "EventLink": "http://www.javak9s.com/",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 34.1084,
    "Longitude": -118.8527
  },
  {
    "Date": "2027-02-27",
    "Location": "Brooksville, FL",
    "Host": "Hoppin’ in the Hills",
    "EventLink": "https://hoppininthehillscom.wordpress.com",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 28.5119,
    "Longitude": -82.3454
  },
  {
    "Date": "2027-03-05",
    "Location": "Glen Mills, PA",
    "Host": "Firezone GS",
    "EventLink": "https://www.firezonegiantschnauzers.com/nose-work-trials",
    "TrialTypes": "SMT, NW3",
    "EventCount": 2,
    "Latitude": 39.9353,
    "Longitude": -75.4618
  },
  {
    "Date": "2027-03-06",
    "Location": "Keystone Heights, FL",
    "Host": "River Poodles Training, LLC",
    "EventLink": "https://riverpoodlestraining.com/",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 29.7869,
    "Longitude": -82.0758
  },
  {
    "Date": "2027-03-08",
    "Location": "Glendora, CA",
    "Host": "Agile Paws Dog Sports",
    "EventLink": "https://agilepawsdogsports.com/",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 34.1792,
    "Longitude": -117.8618
  },
  {
    "Date": "2027-03-13",
    "Location": "Rome, GA",
    "Host": "Southeast Scent Work Alliance, LLC (SSWA)",
    "EventLink": "https://southeastscent.com/events",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 34.245,
    "Longitude": -85.1322
  },
  {
    "Date": "2027-03-15",
    "Location": "Riverside, CA",
    "Host": "Linda Buchanan",
    "EventLink": "https://www.k9slovetosearch.com/",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 34.0117,
    "Longitude": -117.3873
  },
  {
    "Date": "2027-03-20",
    "Location": "Foxboro, MA",
    "Host": "Bay State Sniffers",
    "EventLink": "http://www.baystatesniffers.com/",
    "TrialTypes": "NW1, L1C, L1I",
    "EventCount": 3,
    "Latitude": 42.0612,
    "Longitude": -71.264
  },
  {
    "Date": "2027-03-20",
    "Location": "Redwood City, CA",
    "Host": "B. L. McMutts, LLC",
    "EventLink": "https://blmcmutts.com/",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 37.5024,
    "Longitude": -122.1869
  },
  {
    "Date": "2027-03-20",
    "Location": "Selma, TX",
    "Host": "Sniff Happens",
    "EventLink": "https://www.sniffhappenstx.com/",
    "TrialTypes": "L1E, ELT-S, ELT",
    "EventCount": 3,
    "Latitude": 29.5825,
    "Longitude": -98.346
  },
  {
    "Date": "2027-03-20",
    "Location": "Troy, MO",
    "Host": "Happy Dog Concepts LLC",
    "EventLink": "https://happydogconcepts.com/events",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 38.9785,
    "Longitude": -91.006
  },
  {
    "Date": "2027-03-25",
    "Location": "Rochester, NY",
    "Host": "2 Psyched 4 Dogs",
    "EventLink": "https://2psyched4dogs.com/",
    "TrialTypes": "NW1, NW3, ELT",
    "EventCount": 3,
    "Latitude": 43.1836,
    "Longitude": -77.5683
  },
  {
    "Date": "2027-03-26",
    "Location": "Albuquerque, NM",
    "Host": "New Mexico Canine Scent Work, LLC",
    "EventLink": "https://www.nmcsw.com/",
    "TrialTypes": "ELT, NW3, L2I, NW1",
    "EventCount": 4,
    "Latitude": 35.1273,
    "Longitude": -106.6837
  },
  {
    "Date": "2027-03-27",
    "Location": "Groveport, OH",
    "Host": "CleverDogs",
    "EventLink": "https://cleverdogsohio.com/",
    "TrialTypes": "NW1, NW2, NW3",
    "EventCount": 3,
    "Latitude": 39.8456,
    "Longitude": -82.8826
  },
  {
    "Date": "2027-04-02",
    "Location": "Rapid City, SD",
    "Host": "Two Paws Up Dog Training, LLC",
    "EventLink": "https://www.twopawsupdogtrainingllc.com/",
    "TrialTypes": "ELT, NW3, NW2, NW1",
    "EventCount": 4,
    "Latitude": 44.0722,
    "Longitude": -103.2143
  },
  {
    "Date": "2027-04-03",
    "Location": "Keystone Heights, FL",
    "Host": "River Poodles Training, LLC",
    "EventLink": "https://riverpoodlestraining.com/",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 29.7804,
    "Longitude": -82.0448
  },
  {
    "Date": "2027-04-03",
    "Location": "Wilmot, WI",
    "Host": "SuperDog Industries LLC",
    "EventLink": "https://www.superdogevents.com/",
    "TrialTypes": "NW2, ELT",
    "EventCount": 2,
    "Latitude": 42.4656,
    "Longitude": -88.1332
  },
  {
    "Date": "2027-04-05",
    "Location": "Chester, NY",
    "Host": "For the Love of Dogs NY LLC",
    "EventLink": "https://www.fortheloveofdogsny.com/trials-events",
    "TrialTypes": "NW2, ELT",
    "EventCount": 2,
    "Latitude": 41.3376,
    "Longitude": -74.293
  },
  {
    "Date": "2027-04-07",
    "Location": "Olympia, WA",
    "Host": "Let's Talk Dogs, LLC & About Face K9 Academy",
    "EventLink": "https://www.aboutfacek9academy.com",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 47.0059,
    "Longitude": -122.9157
  },
  {
    "Date": "2027-04-10",
    "Location": "Northfield, OH",
    "Host": "Nosework Addicts, LLC",
    "EventLink": "https://www.noseworkaddictsllc.com/",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 41.3715,
    "Longitude": -81.4954
  },
  {
    "Date": "2027-04-10",
    "Location": "Red Feather Lakes, CO",
    "Host": "Beyond Elevation K9",
    "EventLink": "https://www.beyondelevationk9.com/",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 40.8163,
    "Longitude": -105.6135
  },
  {
    "Date": "2027-04-16",
    "Location": "Upland, CA",
    "Host": "Agile Paws Dog Sports",
    "EventLink": "https://agilepawsdogsports.com/",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 34.0602,
    "Longitude": -117.691
  },
  {
    "Date": "2027-04-17",
    "Location": "Glenwood, NY",
    "Host": "Do Over Dog Training",
    "EventLink": "https://www.dooverdogtraining.com/trials",
    "TrialTypes": "NW1, L2E, L1C, L1I",
    "EventCount": 4,
    "Latitude": 42.6635,
    "Longitude": -78.6702
  },
  {
    "Date": "2027-04-19",
    "Location": "Paso Robles, CA",
    "Host": "Gentle Touch Pet Training",
    "EventLink": "https://www.gentlepets.com/",
    "TrialTypes": "ELT-S, L2C",
    "EventCount": 2,
    "Latitude": 35.6067,
    "Longitude": -120.7224
  },
  {
    "Date": "2027-04-24",
    "Location": "Ellicottville, NY",
    "Host": "Do Over Dog Training",
    "EventLink": "https://www.dooverdogtraining.com/trials",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 42.2765,
    "Longitude": -78.6562
  },
  {
    "Date": "2027-05-01",
    "Location": "Manhattan , MT",
    "Host": "Trails and Tails Dog School",
    "EventLink": "https://www.trailsandtailsdogschool.com/events",
    "TrialTypes": "NW1, NW2, NW3",
    "EventCount": 3,
    "Latitude": 45.8503,
    "Longitude": -111.3358
  },
  {
    "Date": "2027-05-01",
    "Location": "Poland Springs, ME",
    "Host": "Bare Bones Nosework, LLC",
    "EventLink": "https://virginiahowe.com/",
    "TrialTypes": "L2I, L3I",
    "EventCount": 2,
    "Latitude": 44.0536,
    "Longitude": -70.3933
  },
  {
    "Date": "2027-05-01",
    "Location": "Sharon, MA",
    "Host": "Bay State Sniffers",
    "EventLink": "http://www.baystatesniffers.com/",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 42.0876,
    "Longitude": -71.1834
  },
  {
    "Date": "2027-05-08",
    "Location": "Ashby, MA",
    "Host": "Carolyn Barney dba Dogs!",
    "EventLink": "https://carolynbarney.com/",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 42.7039,
    "Longitude": -71.8451
  },
  {
    "Date": "2027-05-22",
    "Location": "Amherst, NY",
    "Host": "Do Over Dog Training",
    "EventLink": "https://www.dooverdogtraining.com/trials",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 42.9692,
    "Longitude": -78.8159
  },
  {
    "Date": "2027-06-12",
    "Location": "Portland, OR",
    "Host": "Trust Your Dog K9 Events",
    "EventLink": "https://trustyourdogk9events.com/",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 45.5511,
    "Longitude": -122.7234
  },
  {
    "Date": "2027-06-19",
    "Location": "Spring Grove, IL",
    "Host": "SuperDog Industries LLC",
    "EventLink": "https://www.superdogevents.com/",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 42.4867,
    "Longitude": -88.2483
  },
  {
    "Date": "2027-06-26",
    "Location": "Delran, NY",
    "Host": "K9 InScentives",
    "EventLink": "https://www.k9inscentives.com/",
    "TrialTypes": "NW3, NW1, L2I",
    "EventCount": 3,
    "Latitude": 40.0348,
    "Longitude": -74.9096
  },
  {
    "Date": "2027-06-29",
    "Location": "Delran, NJ",
    "Host": "Ev-ry earthdog LLC",
    "EventLink": "https://ev-ryearthdog.com/",
    "TrialTypes": "ELT-P, NW2, NW1, L1I, L2C",
    "EventCount": 5,
    "Latitude": 39.9725,
    "Longitude": -74.9938
  },
  {
    "Date": "2027-09-25",
    "Location": "Eastlake, OH",
    "Host": "Nosework Addicts, LLC",
    "EventLink": "https://www.noseworkaddictsllc.com/",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 41.6533,
    "Longitude": -81.4617
  }
]
;
