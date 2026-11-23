const DATA_UPDATED = "November 23, 2026";
const TRIALS_DATA = 
[
  {
    "Date": "2024-11-23",
    "Location": "Coburg, OR",
    "Host": "Kiddie Christie",
    "TrialTypes": "L1E, NW1, NW3",
    "EventCount": 3,
    "Latitude": 44.1253,
    "Longitude": -123.0945
  },
  {
    "Date": "2024-11-23",
    "Location": "Delta, CO",
    "Host": "Mountain Dogs, LLC",
    "TrialTypes": "NW3, ELT-P",
    "EventCount": 2,
    "Latitude": 38.8718,
    "Longitude": -107.8893
  },
  {
    "Date": "2024-11-23",
    "Location": "Fork Union, VA",
    "Host": "Your Dog Knows LLC",
    "TrialTypes": "NW3, L3I, L1V",
    "EventCount": 3,
    "Latitude": 37.7491,
    "Longitude": -78.3077
  },
  {
    "Date": "2024-11-23",
    "Location": "Kintnersville, PA",
    "Host": "Paws n' Sniff",
    "TrialTypes": "NW1, L1E, ELT-S",
    "EventCount": 3,
    "Latitude": 40.5596,
    "Longitude": -75.1541
  },
  {
    "Date": "2024-11-23",
    "Location": "Saltsburg, PA",
    "Host": "Steel City Nosework, LLC",
    "TrialTypes": "NW3, ELT-S, NW1",
    "EventCount": 3,
    "Latitude": 40.4442,
    "Longitude": -79.467
  },
  {
    "Date": "2024-11-23",
    "Location": "Smyrna, TN",
    "Host": "Dogs Have Amazing Noses LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 36.0321,
    "Longitude": -86.4908
  },
  {
    "Date": "2024-11-29",
    "Location": "Capo Beach/Dana Point, CA",
    "Host": "JavaK9s",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 33.4472,
    "Longitude": -117.6591
  },
  {
    "Date": "2024-11-29",
    "Location": "Foxborough, MA",
    "Host": "Tracey Costa",
    "TrialTypes": "ELT, L1C, NW2",
    "EventCount": 3,
    "Latitude": 42.0728,
    "Longitude": -71.232
  },
  {
    "Date": "2024-11-30",
    "Location": "Cottage Grove, MN",
    "Host": "Gretchen Hofheins-Wackerfuss",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 44.7794,
    "Longitude": -92.9158
  },
  {
    "Date": "2024-11-30",
    "Location": "Cumming, GA",
    "Host": "Georgia Nosework",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 34.2123,
    "Longitude": -84.1419
  },
  {
    "Date": "2024-11-30",
    "Location": "Green Bay, WI",
    "Host": "NEWK9 Scent Work LLC",
    "TrialTypes": "ELT-S, ELT",
    "EventCount": 2,
    "Latitude": 44.4983,
    "Longitude": -87.9898
  },
  {
    "Date": "2024-11-30",
    "Location": "Lebanon, NJ",
    "Host": "Sirius K-9 Solutions",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 40.6007,
    "Longitude": -74.8072
  },
  {
    "Date": "2024-11-30",
    "Location": "Los Osos, CA",
    "Host": "Central Coast Nosework Club, Inc.",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 35.3108,
    "Longitude": -120.8533
  },
  {
    "Date": "2024-11-30",
    "Location": "Plant City, FL",
    "Host": "Hoppin’ in the Hills",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 27.9857,
    "Longitude": -82.1597
  },
  {
    "Date": "2024-11-30",
    "Location": "Worcester, PA",
    "Host": "Sniff Sniff Hooray",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 40.1621,
    "Longitude": -75.3933
  },
  {
    "Date": "2024-12-06",
    "Location": "Salem, WI",
    "Host": "Loving Paws Dog Training LLC",
    "TrialTypes": "ELT-P, ELT",
    "EventCount": 2,
    "Latitude": 42.5891,
    "Longitude": -88.1517
  },
  {
    "Date": "2024-12-06",
    "Location": "Ypsilanti, MI",
    "Host": "Every Dog Nosework",
    "TrialTypes": "ELT-P, ELT-S, L2C",
    "EventCount": 3,
    "Latitude": 42.2622,
    "Longitude": -83.5907
  },
  {
    "Date": "2024-12-07",
    "Location": "Batavia, NY",
    "Host": "Do Over Dog Training",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 42.9997,
    "Longitude": -78.2225
  },
  {
    "Date": "2024-12-07",
    "Location": "Bowie, MD",
    "Host": "Red Huskies",
    "TrialTypes": "ELT-P, ELT-S",
    "EventCount": 2,
    "Latitude": 38.9381,
    "Longitude": -76.7693
  },
  {
    "Date": "2024-12-07",
    "Location": "Centralia, WA",
    "Host": "Let's Talk Dogs, LLC and About Face K9 Academy",
    "TrialTypes": "ELT-P, NW2",
    "EventCount": 2,
    "Latitude": 46.7639,
    "Longitude": -122.9838
  },
  {
    "Date": "2024-12-07",
    "Location": "Chester Springs, PA",
    "Host": "Patricia Grassey",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 40.1297,
    "Longitude": -75.5766
  },
  {
    "Date": "2024-12-07",
    "Location": "DeLeon Springs, FL",
    "Host": "River Poodles Training, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 29.1266,
    "Longitude": -81.3787
  },
  {
    "Date": "2024-12-07",
    "Location": "Fillmore, CA",
    "Host": "Pink Biscuit K9s",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 34.3942,
    "Longitude": -118.9107
  },
  {
    "Date": "2024-12-07",
    "Location": "Newfoundland, PA",
    "Host": "Your Dog's Place, LLC",
    "TrialTypes": "L3V, NW2, ELT",
    "EventCount": 3,
    "Latitude": 41.3544,
    "Longitude": -75.2832
  },
  {
    "Date": "2024-12-07",
    "Location": "Owenton, KY",
    "Host": "Clermont County Dog Training Club",
    "TrialTypes": "ELT, NW1, NW2",
    "EventCount": 3,
    "Latitude": 38.4967,
    "Longitude": -84.7933
  },
  {
    "Date": "2024-12-09",
    "Location": "Stockton, CA",
    "Host": "Two Nosey Girls",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 37.9926,
    "Longitude": -121.3268
  },
  {
    "Date": "2024-12-13",
    "Location": "Pittstown, NJ",
    "Host": "Shamrock Pot of Gold K9 Scenter",
    "TrialTypes": "NW3, ELT-P, ELT-S, NW1",
    "EventCount": 4,
    "Latitude": 40.5891,
    "Longitude": -74.9213
  },
  {
    "Date": "2024-12-14",
    "Location": "Ontario, CA",
    "Host": "Agile Paws Dog Sports",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 34.0703,
    "Longitude": -117.696
  },
  {
    "Date": "2024-12-21",
    "Location": "Cedar Park, TX",
    "Host": "Scent Work Across Texas",
    "TrialTypes": "L1V, L2I, NW1, L2E",
    "EventCount": 4,
    "Latitude": 30.5581,
    "Longitude": -97.7978
  },
  {
    "Date": "2024-12-21",
    "Location": "Jefferson, GA",
    "Host": "Right Choice Dog Training, LLC",
    "TrialTypes": "NW2, NW1",
    "EventCount": 2,
    "Latitude": 33.0396,
    "Longitude": -82.44
  },
  {
    "Date": "2024-12-21",
    "Location": "Marriottsville, MD",
    "Host": "Red Huskies",
    "TrialTypes": "ELT-P, ELT-S, NW1",
    "EventCount": 3,
    "Latitude": 39.3177,
    "Longitude": -76.9
  },
  {
    "Date": "2024-12-27",
    "Location": "Crownsville, MD",
    "Host": "Red Huskies",
    "TrialTypes": "ELT-P, ELT, ELT-S",
    "EventCount": 3,
    "Latitude": 39.0226,
    "Longitude": -76.6373
  },
  {
    "Date": "2024-12-28",
    "Location": "Bellingham, WA",
    "Host": "Nose Work Magic",
    "TrialTypes": "L1V, ELT-S, ELT",
    "EventCount": 3,
    "Latitude": 48.721,
    "Longitude": -122.5046
  },
  {
    "Date": "2024-12-28",
    "Location": "Cumming, GA",
    "Host": "Georgia Nosework",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 34.2179,
    "Longitude": -84.1001
  },
  {
    "Date": "2024-12-28",
    "Location": "Salem, OR",
    "Host": "Helix Fairweather & Doglandia, LLC",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 44.9485,
    "Longitude": -122.9849
  },
  {
    "Date": "2024-12-28",
    "Location": "Williamsburg, VA",
    "Host": "Blockade Runners Flyball",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 37.2703,
    "Longitude": -76.7475
  },
  {
    "Date": "2024-12-29",
    "Location": "Waukesha, WI",
    "Host": "Think Pawsitive Dog Training",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 43.1076,
    "Longitude": -88.3023
  },
  {
    "Date": "2024-12-31",
    "Location": "Strasburg, IL",
    "Host": "Kudos for Canines, LLC",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 39.3317,
    "Longitude": -88.6324
  },
  {
    "Date": "2025-01-03",
    "Location": "Brockport, NY",
    "Host": "Savvy Dog Sports",
    "TrialTypes": "ELT-S, ELT",
    "EventCount": 2,
    "Latitude": 43.2423,
    "Longitude": -77.9103
  },
  {
    "Date": "2025-01-03",
    "Location": "Emmitsburg, MD",
    "Host": "Red Huskies",
    "TrialTypes": "ELT, ELT-P, ELT-S",
    "EventCount": 3,
    "Latitude": 39.6689,
    "Longitude": -77.3416
  },
  {
    "Date": "2025-01-04",
    "Location": "Bonsall, CA",
    "Host": "Linda Buchanan",
    "TrialTypes": "NW2, ELT",
    "EventCount": 2,
    "Latitude": 33.2903,
    "Longitude": -117.1593
  },
  {
    "Date": "2025-01-09",
    "Location": "Centreville, MD",
    "Host": "Fair Play Labradors",
    "TrialTypes": "ELT-S, NW3, ELT, ELT-P",
    "EventCount": 4,
    "Latitude": 39.0804,
    "Longitude": -76.0416
  },
  {
    "Date": "2025-01-10",
    "Location": "Hartfield, VA",
    "Host": "Paws Plus Training, LLC",
    "TrialTypes": "NW3, NW1, NW2, ELT",
    "EventCount": 4,
    "Latitude": 37.5893,
    "Longitude": -76.4684
  },
  {
    "Date": "2025-01-11",
    "Location": "Greensboro, NC",
    "Host": "Dog Fun Forever, LLC",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 36.0999,
    "Longitude": -79.7452
  },
  {
    "Date": "2025-01-11",
    "Location": "Lithia, FL",
    "Host": "Hoppin’ in the Hills",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 27.862,
    "Longitude": -82.1923
  },
  {
    "Date": "2025-01-13",
    "Location": "Oakdale, CA",
    "Host": "Two Nosey Girls",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 37.8062,
    "Longitude": -120.8385
  },
  {
    "Date": "2025-01-18",
    "Location": "Clanton, AL",
    "Host": "Daphne Melillo",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 32.8508,
    "Longitude": -86.6405
  },
  {
    "Date": "2025-01-18",
    "Location": "Elmira, OR",
    "Host": "Wells Creek Dog Training",
    "TrialTypes": "ELT, L1I, NW2",
    "EventCount": 3,
    "Latitude": 44.0675,
    "Longitude": -123.3778
  },
  {
    "Date": "2025-01-18",
    "Location": "Flemington, NJ",
    "Host": "Shamrock Pot of Gold K9 Scenter",
    "TrialTypes": "ELT-S, L2I, L2C, ELT",
    "EventCount": 4,
    "Latitude": 40.5427,
    "Longitude": -74.8296
  },
  {
    "Date": "2025-01-18",
    "Location": "Marble Falls, TX",
    "Host": "Heng Ten K9 Training",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 30.6007,
    "Longitude": -98.3034
  },
  {
    "Date": "2025-01-18",
    "Location": "Melrose, FL",
    "Host": "River Poodles Training, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 29.7224,
    "Longitude": -82.0509
  },
  {
    "Date": "2025-01-18",
    "Location": "New Rochelle, NY",
    "Host": "For the Love of Dogs NY, LLC",
    "TrialTypes": "NW2, NW3, ELT-S",
    "EventCount": 3,
    "Latitude": 40.9253,
    "Longitude": -73.8322
  },
  {
    "Date": "2025-01-18",
    "Location": "Redlands, CA",
    "Host": "Agile Paws Dog Sports",
    "TrialTypes": "ELT, NW2",
    "EventCount": 2,
    "Latitude": 34.038,
    "Longitude": -117.1492
  },
  {
    "Date": "2025-01-18",
    "Location": "Sheridan, OR",
    "Host": "Helix Fairweather & Doglandia, LLC",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 45.1061,
    "Longitude": -123.3986
  },
  {
    "Date": "2025-01-25",
    "Location": "Danielsville, GA",
    "Host": "Right Choice Dog Training, LLC",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 34.107,
    "Longitude": -83.255
  },
  {
    "Date": "2025-01-25",
    "Location": "Tecumseh, OK",
    "Host": "The Doggie Spot, LLC",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 35.2708,
    "Longitude": -96.9772
  },
  {
    "Date": "2025-01-31",
    "Location": "Vista, CA",
    "Host": "Anita Cheesman and Jessica Koester",
    "TrialTypes": "ELT-S, NW3",
    "EventCount": 2,
    "Latitude": 33.165,
    "Longitude": -117.2883
  },
  {
    "Date": "2025-02-01",
    "Location": "Northridge, CA",
    "Host": "Scentwork.org",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 34.2313,
    "Longitude": -118.5396
  },
  {
    "Date": "2025-02-08",
    "Location": "Murfreesboro, TN",
    "Host": "Dogs Have Amazing Noses LLC",
    "TrialTypes": "ELT, NW1",
    "EventCount": 2,
    "Latitude": 35.8336,
    "Longitude": -86.4082
  },
  {
    "Date": "2025-02-08",
    "Location": "Veneta, OR",
    "Host": "Kiddy Christie",
    "TrialTypes": "NW3, L1C, NW1",
    "EventCount": 3,
    "Latitude": 44.0599,
    "Longitude": -123.3791
  },
  {
    "Date": "2025-02-14",
    "Location": "Honey Brook, PA",
    "Host": "The Sniffing Hound",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 40.1224,
    "Longitude": -75.9385
  },
  {
    "Date": "2025-02-15",
    "Location": "Bellingham, WA",
    "Host": "Nose Work Magic",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 48.7722,
    "Longitude": -122.4487
  },
  {
    "Date": "2025-02-15",
    "Location": "Flemington, NJ",
    "Host": "Shamrock Pot of Gold K9 Scenter",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 40.4909,
    "Longitude": -74.862
  },
  {
    "Date": "2025-02-15",
    "Location": "Lakewood, NJ",
    "Host": "Rotts-n-Notts Nosework, LLC",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 40.1101,
    "Longitude": -74.2614
  },
  {
    "Date": "2025-02-15",
    "Location": "Lutherville-Timonium, MD",
    "Host": "Red Huskies",
    "TrialTypes": "L3C, L1I, NW2",
    "EventCount": 3,
    "Latitude": 39.4017,
    "Longitude": -76.605
  },
  {
    "Date": "2025-02-15",
    "Location": "Medford, NJ",
    "Host": "Sniff Sniff Hooray",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 39.9461,
    "Longitude": -74.8144
  },
  {
    "Date": "2025-02-15",
    "Location": "Modesto, CA",
    "Host": "Two Nosey Girls",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 37.661,
    "Longitude": -120.9569
  },
  {
    "Date": "2025-02-15",
    "Location": "White Plains, NY",
    "Host": "For the Love of Dogs NY, LLC",
    "TrialTypes": "L1I, L2C, NW3",
    "EventCount": 3,
    "Latitude": 41.0002,
    "Longitude": -73.7476
  },
  {
    "Date": "2025-02-15",
    "Location": "Wilson, NC",
    "Host": "Whole Dog Institute, LLC",
    "TrialTypes": "NW3, NW1",
    "EventCount": 2,
    "Latitude": 35.753,
    "Longitude": -77.9629
  },
  {
    "Date": "2025-02-16",
    "Location": "Chino, CA",
    "Host": "Agile Paws Dog Sports",
    "TrialTypes": "ELT, ELT-S",
    "EventCount": 2,
    "Latitude": 33.9871,
    "Longitude": -117.643
  },
  {
    "Date": "2025-02-22",
    "Location": "Albuquerque, NM",
    "Host": "The Can Do K9, LLC",
    "TrialTypes": "NW1, NW3",
    "EventCount": 2,
    "Latitude": 35.1257,
    "Longitude": -106.6266
  },
  {
    "Date": "2025-02-23",
    "Location": "Benson, AZ",
    "Host": "Patience Unlimited Professional Dog Training",
    "TrialTypes": "NW3, ELT-S, NW2",
    "EventCount": 3,
    "Latitude": 31.9791,
    "Longitude": -110.2993
  },
  {
    "Date": "2025-02-24",
    "Location": "Paso Robles, CA",
    "Host": "Gentle Touch Pet Training",
    "TrialTypes": "NW3, L2V, L1V",
    "EventCount": 3,
    "Latitude": 35.587,
    "Longitude": -120.6404
  },
  {
    "Date": "2025-02-28",
    "Location": "San Rafael, CA",
    "Host": "Marin Humane",
    "TrialTypes": "L1C, ELT-S, ELT",
    "EventCount": 3,
    "Latitude": 38.0055,
    "Longitude": -122.5508
  },
  {
    "Date": "2025-03-01",
    "Location": "Augusta, NJ",
    "Host": "Shamrock Pot of Gold K9 Scenter",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 41.1584,
    "Longitude": -74.7017
  },
  {
    "Date": "2025-03-01",
    "Location": "Keystone Heights, FL",
    "Host": "River Poodles Training, LLC",
    "TrialTypes": "NW3, ELT-P",
    "EventCount": 2,
    "Latitude": 29.7635,
    "Longitude": -82.0021
  },
  {
    "Date": "2025-03-01",
    "Location": "Oakville, WA",
    "Host": "About Face K9 Academy and Let's Talk Dogs, LLC",
    "TrialTypes": "NW3, L1C, L1I",
    "EventCount": 3,
    "Latitude": 46.8188,
    "Longitude": -123.2022
  },
  {
    "Date": "2025-03-01",
    "Location": "Pomfret, MD",
    "Host": "Nose-It-All, LLC",
    "TrialTypes": "ELT-S, NW2, ELT",
    "EventCount": 3,
    "Latitude": 38.593,
    "Longitude": -77.0393
  },
  {
    "Date": "2025-03-01",
    "Location": "Santa Paula, CA",
    "Host": "Pink Biscuit K9s",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 34.3914,
    "Longitude": -119.0615
  },
  {
    "Date": "2025-03-01",
    "Location": "Youngwood, PA",
    "Host": "Steel City Nosework, LLC",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 40.263,
    "Longitude": -79.5672
  },
  {
    "Date": "2025-03-02",
    "Location": "Shawnee, OK",
    "Host": "The Doggie Spot, LLC",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 35.3729,
    "Longitude": -96.9418
  },
  {
    "Date": "2025-03-07",
    "Location": "Elgin, IL",
    "Host": "For Your K9",
    "TrialTypes": "L1C, L2C, L1I, L2I",
    "EventCount": 4,
    "Latitude": 41.9959,
    "Longitude": -88.246
  },
  {
    "Date": "2025-03-07",
    "Location": "Spring City, PA",
    "Host": "Patricia Grassey",
    "TrialTypes": "NW3, ELT, ELT-S, L1I",
    "EventCount": 4,
    "Latitude": 40.2229,
    "Longitude": -75.5921
  },
  {
    "Date": "2025-03-07",
    "Location": "Stokesdale , NC",
    "Host": "K9 Nose Adventures, LLC",
    "TrialTypes": "ELT, NW1, NW2, L1C, L1I",
    "EventCount": 5,
    "Latitude": 36.2282,
    "Longitude": -79.9567
  },
  {
    "Date": "2025-03-08",
    "Location": "Farmville, VA",
    "Host": "Paws Plus Training, LLC",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 37.2823,
    "Longitude": -78.4361
  },
  {
    "Date": "2025-03-08",
    "Location": "Fort Collins, CO",
    "Host": "Beyond Elevation K9 Training",
    "TrialTypes": "NW1, NW2, ELT",
    "EventCount": 3,
    "Latitude": 40.6289,
    "Longitude": -105.0572
  },
  {
    "Date": "2025-03-08",
    "Location": "Foxboro , MA",
    "Host": "Bay State Sniffers",
    "TrialTypes": "L1C, ELT-S, L1I, L3I",
    "EventCount": 4,
    "Latitude": 42.1085,
    "Longitude": -71.2608
  },
  {
    "Date": "2025-03-08",
    "Location": "Rome, GA",
    "Host": "Southeast Scent Work Alliance, LLC",
    "TrialTypes": "ELT, NW2",
    "EventCount": 2,
    "Latitude": 34.2768,
    "Longitude": -85.2085
  },
  {
    "Date": "2025-03-08",
    "Location": "Winterset, IA",
    "Host": "KBP Dog Training",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 41.3105,
    "Longitude": -94.0527
  },
  {
    "Date": "2025-03-10",
    "Location": "Riverside, CA",
    "Host": "Linda Buchanan",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 33.9877,
    "Longitude": -117.3935
  },
  {
    "Date": "2025-03-14",
    "Location": "Phoenix, AZ",
    "Host": "Successful Sniffer",
    "TrialTypes": "NW3, ELT-S, NW1, NW2",
    "EventCount": 4,
    "Latitude": 33.4321,
    "Longitude": -112.1008
  },
  {
    "Date": "2025-03-14",
    "Location": "Phoenix, MD",
    "Host": "Oriole Dog Training Club",
    "TrialTypes": "NW3, L2I, NW2",
    "EventCount": 3,
    "Latitude": 39.5286,
    "Longitude": -76.607
  },
  {
    "Date": "2025-03-15",
    "Location": "Blaine, WA",
    "Host": "Nose Work Magic",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 49.0206,
    "Longitude": -122.7431
  },
  {
    "Date": "2025-03-15",
    "Location": "Califon (formerly Pomona), NY",
    "Host": "Shamrock Pot of Gold K9 Scenter",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 41.1549,
    "Longitude": -74.0554
  },
  {
    "Date": "2025-03-15",
    "Location": "Gainesville, GA",
    "Host": "Georgia Nosework",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 34.2649,
    "Longitude": -83.7923
  },
  {
    "Date": "2025-03-15",
    "Location": "Kent, WA",
    "Host": "Northwest K9 Sniffers",
    "TrialTypes": "ELT-S, L1V, L1I",
    "EventCount": 3,
    "Latitude": 47.3457,
    "Longitude": -122.2305
  },
  {
    "Date": "2025-03-15",
    "Location": "Pflugerville, TX",
    "Host": "Scent Work Across Texas",
    "TrialTypes": "ELT-S, NW2, L1C, L1E",
    "EventCount": 4,
    "Latitude": 30.4765,
    "Longitude": -97.6659
  },
  {
    "Date": "2025-03-15",
    "Location": "Thaxton, VA",
    "Host": "Canny K9 Companions LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 37.3438,
    "Longitude": -79.6099
  },
  {
    "Date": "2025-03-15",
    "Location": "Westminster, MD",
    "Host": "Red Huskies",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 39.5991,
    "Longitude": -77.0333
  },
  {
    "Date": "2025-03-17",
    "Location": "Corralitos, CA",
    "Host": "CalCoastal Dog Owners Group",
    "TrialTypes": "NW2, NW1",
    "EventCount": 2,
    "Latitude": 36.9744,
    "Longitude": -121.8255
  },
  {
    "Date": "2025-03-22",
    "Location": "Johnstown, NY",
    "Host": "My Dog Smells LLC",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 43.0263,
    "Longitude": -74.4177
  },
  {
    "Date": "2025-03-22",
    "Location": "Salem, WI",
    "Host": "Loving Paws Dog Training LLC",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 42.5235,
    "Longitude": -88.1053
  },
  {
    "Date": "2025-03-22",
    "Location": "Shelbyville, TN",
    "Host": "Dogs Have Amazing Noses LLC",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 35.4656,
    "Longitude": -86.4587
  },
  {
    "Date": "2025-03-22",
    "Location": "Tampa, FL",
    "Host": "Hoppin’ in the Hills",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 27.9634,
    "Longitude": -82.4624
  },
  {
    "Date": "2025-03-22",
    "Location": "Wakefield, VA",
    "Host": "Paws Plus Training, LLC",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 37.0182,
    "Longitude": -76.9744
  },
  {
    "Date": "2025-03-23",
    "Location": "Rapid City, SD",
    "Host": "Two Paws Up Dog Training, LLC",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 44.0329,
    "Longitude": -103.2577
  },
  {
    "Date": "2025-03-23",
    "Location": "Upland, CA",
    "Host": "Agile Paws Dog Sports",
    "TrialTypes": "NW1",
    "EventCount": 1,
    "Latitude": 34.1056,
    "Longitude": -117.6443
  },
  {
    "Date": "2025-03-28",
    "Location": "Dobbs Ferry, NY",
    "Host": "For the Love of Dogs NY, LLC",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 40.9833,
    "Longitude": -73.843
  },
  {
    "Date": "2025-03-28",
    "Location": "Flint, MI",
    "Host": "Every Dog Nosework",
    "TrialTypes": "SMT, ELT-S, L2I",
    "EventCount": 3,
    "Latitude": 43.0367,
    "Longitude": -83.686
  },
  {
    "Date": "2025-03-28",
    "Location": "Frederick, MD",
    "Host": "Red Huskies",
    "TrialTypes": "ELT-P, ELT, ELT-S",
    "EventCount": 3,
    "Latitude": 39.4149,
    "Longitude": -77.3738
  },
  {
    "Date": "2025-03-28",
    "Location": "Grand Junction, CO",
    "Host": "Mountain Dogs, LLC",
    "TrialTypes": "NW3, L1C, NW2, NW1",
    "EventCount": 4,
    "Latitude": 39.0743,
    "Longitude": -108.544
  },
  {
    "Date": "2025-03-28",
    "Location": "Salem, OR",
    "Host": "Kristina Leipzig, Doglandia LLC and Carol Forsberg",
    "TrialTypes": "ELT-S",
    "EventCount": 1,
    "Latitude": 44.9646,
    "Longitude": -123.0487
  },
  {
    "Date": "2025-03-28",
    "Location": "Shady Hills, FL",
    "Host": "Hoppin’ in the Hills",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 28.3773,
    "Longitude": -82.5151
  },
  {
    "Date": "2025-03-29",
    "Location": "Clinton, WI",
    "Host": "George and Shannon Carpenter",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 42.5274,
    "Longitude": -88.8294
  },
  {
    "Date": "2025-03-29",
    "Location": "Gilbertsville, PA",
    "Host": "Sniff Sniff Hooray",
    "TrialTypes": "NW2, ELT",
    "EventCount": 2,
    "Latitude": 40.3006,
    "Longitude": -75.6313
  },
  {
    "Date": "2025-03-29",
    "Location": "Goleta, CA",
    "Host": "All Fur Fun",
    "TrialTypes": "ELT-P, NW3",
    "EventCount": 2,
    "Latitude": 34.4106,
    "Longitude": -119.825
  },
  {
    "Date": "2025-03-29",
    "Location": "Kennett Square, PA",
    "Host": "The Sniffing Hound",
    "TrialTypes": "ELT-S, ELT",
    "EventCount": 2,
    "Latitude": 39.8847,
    "Longitude": -75.7207
  },
  {
    "Date": "2025-03-29",
    "Location": "LeRoy, IL",
    "Host": "Kudos for Canines",
    "TrialTypes": "NW3, L1C, L2I",
    "EventCount": 3,
    "Latitude": 42.4891,
    "Longitude": -88.7777
  },
  {
    "Date": "2025-03-29",
    "Location": "Olathe, KS",
    "Host": "Brookside Pet Training Studio for Dogs",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 38.9298,
    "Longitude": -94.7757
  },
  {
    "Date": "2025-03-30",
    "Location": "East Windsor, CT",
    "Host": "Lucky Dog Events",
    "TrialTypes": "L2V, NW2, NW3",
    "EventCount": 3,
    "Latitude": 41.9077,
    "Longitude": -72.5719
  },
  {
    "Date": "2025-04-03",
    "Location": "Alpharetta, GA",
    "Host": "Georgia Nosework",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 34.0953,
    "Longitude": -84.315
  },
  {
    "Date": "2025-04-04",
    "Location": "Easton, MD",
    "Host": "Fair Play Labradors",
    "TrialTypes": "SMT, L1V, L2V",
    "EventCount": 3,
    "Latitude": 38.8036,
    "Longitude": -76.0764
  },
  {
    "Date": "2025-04-05",
    "Location": "Genoa, IL",
    "Host": "Common Scents K9 Scent Work Club of Elgin",
    "TrialTypes": "NW2, NW3",
    "EventCount": 2,
    "Latitude": 42.145,
    "Longitude": -88.6619
  },
  {
    "Date": "2025-04-05",
    "Location": "Kittanning, PA",
    "Host": "Steel City Nosework, LLC",
    "TrialTypes": "ELT, ELT-S, NW2",
    "EventCount": 3,
    "Latitude": 40.8594,
    "Longitude": -79.5463
  },
  {
    "Date": "2025-04-05",
    "Location": "Maple Falls, WA",
    "Host": "Nose Work Magic",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 48.9627,
    "Longitude": -122.1251
  },
  {
    "Date": "2025-04-05",
    "Location": "North Java, NY",
    "Host": "Do Over Dog Training",
    "TrialTypes": "ELT-S, L2C, L3E",
    "EventCount": 3,
    "Latitude": 42.6934,
    "Longitude": -78.293
  },
  {
    "Date": "2025-04-05",
    "Location": "Somis, CA",
    "Host": "Pink Biscuit K9s",
    "TrialTypes": "ELT, ELT-S, NW1",
    "EventCount": 3,
    "Latitude": 34.2741,
    "Longitude": -118.9658
  },
  {
    "Date": "2025-04-05",
    "Location": "Tucson, AZ",
    "Host": "Patience Unlimited Professional Dog Training",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 32.2248,
    "Longitude": -110.925
  },
  {
    "Date": "2025-04-05",
    "Location": "Woodstock, IL",
    "Host": "Northwest Obedience Club Inc.",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 42.3629,
    "Longitude": -88.4621
  },
  {
    "Date": "2025-04-11",
    "Location": "Sequim, WA",
    "Host": "Sarah Becker, Sea Change Canine LLC & Carol Forsberg",
    "TrialTypes": "NW2, NW1",
    "EventCount": 2,
    "Latitude": 48.0612,
    "Longitude": -123.0733
  },
  {
    "Date": "2025-04-12",
    "Location": "Auburn, WA",
    "Host": "Northwest K9 Sniffers",
    "TrialTypes": "NW3, L1C, L1E",
    "EventCount": 3,
    "Latitude": 47.3018,
    "Longitude": -122.2134
  },
  {
    "Date": "2025-04-12",
    "Location": "Boone, IA",
    "Host": "KBP Dog Training",
    "TrialTypes": "ELT, NW2",
    "EventCount": 2,
    "Latitude": 42.0254,
    "Longitude": -93.9264
  },
  {
    "Date": "2025-04-12",
    "Location": "Burton, OH",
    "Host": "Barns And Noses, LLC",
    "TrialTypes": "ELT, ELT-S, NW1",
    "EventCount": 3,
    "Latitude": 41.4571,
    "Longitude": -81.153
  },
  {
    "Date": "2025-04-12",
    "Location": "Carlisle, PA",
    "Host": "Nose-It-All, LLC",
    "TrialTypes": "ELT, ELT-S, L2E",
    "EventCount": 3,
    "Latitude": 40.2255,
    "Longitude": -77.1588
  },
  {
    "Date": "2025-04-12",
    "Location": "Laramie, WY",
    "Host": "Mountain Dogs, LLC",
    "TrialTypes": "ELT, NW3, ELT-S, NW2",
    "EventCount": 4,
    "Latitude": 41.2764,
    "Longitude": -105.5876
  },
  {
    "Date": "2025-04-12",
    "Location": "Michigan City, IN",
    "Host": "Indiana Scentwork",
    "TrialTypes": "NW3, ELT-S, NW2",
    "EventCount": 3,
    "Latitude": 41.6746,
    "Longitude": -86.9188
  },
  {
    "Date": "2025-04-12",
    "Location": "Peekskill, NY",
    "Host": "For the Love of Dogs NY, LLC",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 41.2763,
    "Longitude": -73.8771
  },
  {
    "Date": "2025-04-12",
    "Location": "Rhinebeck, NY",
    "Host": "Top Notch Dogs, LLC",
    "TrialTypes": "ELT, L1C, L2C",
    "EventCount": 3,
    "Latitude": 41.9159,
    "Longitude": -73.8707
  },
  {
    "Date": "2025-04-12",
    "Location": "Starke, FL",
    "Host": "River Poodles Training, LLC",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 29.9265,
    "Longitude": -82.1246
  },
  {
    "Date": "2025-04-14",
    "Location": "Sacramento, CA",
    "Host": "Two Nosey Girls",
    "TrialTypes": "L2E, L3E, ELT",
    "EventCount": 3,
    "Latitude": 38.585,
    "Longitude": -121.4677
  },
  {
    "Date": "2025-04-18",
    "Location": "Asheboro, NC",
    "Host": "K9 Nose Adventures, LLC",
    "TrialTypes": "ELT, NW3, ELT-S, L2C",
    "EventCount": 4,
    "Latitude": 35.7084,
    "Longitude": -79.8388
  },
  {
    "Date": "2025-04-18",
    "Location": "Grand Junction, CO",
    "Host": "Mountain Dogs, LLC",
    "TrialTypes": "ELT, NW3, NW1, NW2",
    "EventCount": 4,
    "Latitude": 39.0971,
    "Longitude": -108.543
  },
  {
    "Date": "2025-04-18",
    "Location": "Palmer, MA",
    "Host": "HeavenScent Sniffers",
    "TrialTypes": "ELT, NW3, NW1, NW2",
    "EventCount": 4,
    "Latitude": 42.2059,
    "Longitude": -72.3633
  },
  {
    "Date": "2025-04-18",
    "Location": "Rochester, NY",
    "Host": "Tami Sullivan",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 43.1726,
    "Longitude": -77.5869
  },
  {
    "Date": "2025-04-19",
    "Location": "Brooksville, FL",
    "Host": "Hoppin’ in the Hills",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 28.5289,
    "Longitude": -82.3428
  },
  {
    "Date": "2025-04-19",
    "Location": "Kunkletown, PA",
    "Host": "Paws n' Sniff",
    "TrialTypes": "NW3, L3E, ELT-S",
    "EventCount": 3,
    "Latitude": 40.8026,
    "Longitude": -75.4639
  },
  {
    "Date": "2025-04-24",
    "Location": "Concord, CA",
    "Host": "The Bay Team",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 37.9531,
    "Longitude": -122.0259
  },
  {
    "Date": "2025-04-25",
    "Location": "Eagan, MN",
    "Host": "St Paul Dog Training Club",
    "TrialTypes": "ELT, NW2, L2C, L3I",
    "EventCount": 4,
    "Latitude": 44.783,
    "Longitude": -93.1238
  },
  {
    "Date": "2025-04-26",
    "Location": "Decatur, GA",
    "Host": "Right Choice Dog Training LLC",
    "TrialTypes": "L1C, L1I",
    "EventCount": 2,
    "Latitude": 30.8964,
    "Longitude": -84.5658
  },
  {
    "Date": "2025-04-26",
    "Location": "Ellicottville, NY",
    "Host": "Do Over Dog Training",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 42.2564,
    "Longitude": -78.6807
  },
  {
    "Date": "2025-04-26",
    "Location": "FT. Pierce, FL",
    "Host": "Obedience Training Club of Palm Beach County",
    "TrialTypes": "L1E, NW2, NW1, L1C",
    "EventCount": 4,
    "Latitude": 27.4036,
    "Longitude": -80.3659
  },
  {
    "Date": "2025-04-26",
    "Location": "Greenfield, WI",
    "Host": "Loving Paws Dog Training LLC",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 42.9254,
    "Longitude": -87.9479
  },
  {
    "Date": "2025-04-26",
    "Location": "Hammond, LA",
    "Host": "Dog Gone Right",
    "TrialTypes": "NW1, NW2, ELT",
    "EventCount": 3,
    "Latitude": 30.4772,
    "Longitude": -90.5016
  },
  {
    "Date": "2025-04-26",
    "Location": "Kingston, NH",
    "Host": "The Big Sniff, LLC",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 42.9614,
    "Longitude": -71.0693
  },
  {
    "Date": "2025-04-26",
    "Location": "Lyons, OR",
    "Host": "Canine Discovery Corps",
    "TrialTypes": "L1I, L2C, ELT",
    "EventCount": 3,
    "Latitude": 44.7593,
    "Longitude": -122.6187
  },
  {
    "Date": "2025-04-26",
    "Location": "Newtown, PA",
    "Host": "K9 Nosen Around, LLC",
    "TrialTypes": "L1V, NW1, L2V, NW2",
    "EventCount": 4,
    "Latitude": 40.2245,
    "Longitude": -74.9212
  },
  {
    "Date": "2025-04-26",
    "Location": "Northampton, MA",
    "Host": "Lucky Dog Events",
    "TrialTypes": "ELT-P, ELT-S, L1C",
    "EventCount": 3,
    "Latitude": 42.3476,
    "Longitude": -72.6773
  },
  {
    "Date": "2025-04-26",
    "Location": "Ocoee, TN",
    "Host": "Camelot Shepherds, Inc.",
    "TrialTypes": "NW1, NW2, NW3",
    "EventCount": 3,
    "Latitude": 35.1139,
    "Longitude": -84.7175
  },
  {
    "Date": "2025-04-26",
    "Location": "Red Feather Lakes, CO",
    "Host": "Beyond Elevation K9",
    "TrialTypes": "NW2, NW3",
    "EventCount": 2,
    "Latitude": 40.7632,
    "Longitude": -105.5677
  },
  {
    "Date": "2025-04-26",
    "Location": "Traverse City, MI",
    "Host": "Every Dog Nosework",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 44.7393,
    "Longitude": -85.6388
  },
  {
    "Date": "2025-04-26",
    "Location": "West Friendship, MD",
    "Host": "Red Huskies",
    "TrialTypes": "NW1, NW2, L2V, L1C",
    "EventCount": 4,
    "Latitude": 39.2702,
    "Longitude": -76.9655
  },
  {
    "Date": "2025-05-01",
    "Location": "Gainesville, GA",
    "Host": "Georgia Nosework",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 34.3316,
    "Longitude": -83.8384
  },
  {
    "Date": "2025-05-02",
    "Location": "Faribault, MN",
    "Host": "St Paul Dog Training Club",
    "TrialTypes": "NW3, ELT-P, L3E, L3C",
    "EventCount": 4,
    "Latitude": 43.6216,
    "Longitude": -93.9971
  },
  {
    "Date": "2025-05-02",
    "Location": "Nyack, NY",
    "Host": "Waggin Work",
    "TrialTypes": "NW3, ELT, ELT-S",
    "EventCount": 3,
    "Latitude": 41.0416,
    "Longitude": -73.9306
  },
  {
    "Date": "2025-05-03",
    "Location": "Alexis, IL",
    "Host": "Kudos for Canines",
    "TrialTypes": "NW3, ELT-P",
    "EventCount": 2,
    "Latitude": 41.0595,
    "Longitude": -90.527
  },
  {
    "Date": "2025-05-03",
    "Location": "Ashby, MA",
    "Host": "Carolyn Barney dba Dogs!",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 42.6867,
    "Longitude": -71.8464
  },
  {
    "Date": "2025-05-03",
    "Location": "Columbus, MT",
    "Host": "Canine Connection",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 45.6113,
    "Longitude": -109.2813
  },
  {
    "Date": "2025-05-03",
    "Location": "Gray Court, SC",
    "Host": "Foothills Canine Academy, LLC",
    "TrialTypes": "L1V, NW1, NW3",
    "EventCount": 3,
    "Latitude": 34.6251,
    "Longitude": -82.1442
  },
  {
    "Date": "2025-05-03",
    "Location": "Redwood City, CA",
    "Host": "B. L. McMutts",
    "TrialTypes": "NW2, NW1",
    "EventCount": 2,
    "Latitude": 37.5294,
    "Longitude": -122.2572
  },
  {
    "Date": "2025-05-03",
    "Location": "Sandy, OR",
    "Host": "Trust Your Dog K9 Events",
    "TrialTypes": "NW2, NW3",
    "EventCount": 2,
    "Latitude": 45.4211,
    "Longitude": -122.2154
  },
  {
    "Date": "2025-05-03",
    "Location": "Santa Paula, CA",
    "Host": "Pink Biscuit K9s",
    "TrialTypes": "ELT, NW1, NW2",
    "EventCount": 3,
    "Latitude": 34.3468,
    "Longitude": -119.0405
  },
  {
    "Date": "2025-05-03",
    "Location": "White Salmon, WA",
    "Host": "Trisha Thompson and Sharon Smith",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 45.728,
    "Longitude": -121.5221
  },
  {
    "Date": "2025-05-09",
    "Location": "South Sterling, PA",
    "Host": "Paws n' Sniff",
    "TrialTypes": "L3C, L2I, NW2",
    "EventCount": 3,
    "Latitude": 41.279,
    "Longitude": -75.3597
  },
  {
    "Date": "2025-05-09",
    "Location": "Warwick, NY",
    "Host": "Top Notch Dogs, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 41.2212,
    "Longitude": -74.3907
  },
  {
    "Date": "2025-05-10",
    "Location": "Alexander, NY",
    "Host": "Do Over Dog Training",
    "TrialTypes": "L1V, L2I, L1C, L3V",
    "EventCount": 4,
    "Latitude": 42.9078,
    "Longitude": -78.2121
  },
  {
    "Date": "2025-05-10",
    "Location": "Denton, MD",
    "Host": "Fair Play Labradors",
    "TrialTypes": "NW3, ELT-S",
    "EventCount": 2,
    "Latitude": 38.9147,
    "Longitude": -75.8772
  },
  {
    "Date": "2025-05-10",
    "Location": "Poland Springs, ME",
    "Host": "Virginia Howe",
    "TrialTypes": "NW2, ELT",
    "EventCount": 2,
    "Latitude": 44.0639,
    "Longitude": -70.3364
  },
  {
    "Date": "2025-05-10",
    "Location": "Rainier, WA",
    "Host": "Rachelle Bailey-Austin/About Face K9 Academy & Dorothy Turley/Let's Talk Dogs, LLC",
    "TrialTypes": "ELT-S, NW2",
    "EventCount": 2,
    "Latitude": 46.9332,
    "Longitude": -122.6944
  },
  {
    "Date": "2025-05-10",
    "Location": "Santa Barbara, CA",
    "Host": "All Fur Fun",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 34.4205,
    "Longitude": -119.6593
  },
  {
    "Date": "2025-05-13",
    "Location": "Paso Robles, CA",
    "Host": "Gentle Touch Pet Training",
    "TrialTypes": "L2C, NW1",
    "EventCount": 2,
    "Latitude": 35.6023,
    "Longitude": -120.68
  },
  {
    "Date": "2025-05-16",
    "Location": "Montrose, CO",
    "Host": "Mountain Dogs, LLC",
    "TrialTypes": "ELT, NW3, ELT-P",
    "EventCount": 3,
    "Latitude": 38.4328,
    "Longitude": -107.8496
  },
  {
    "Date": "2025-05-16",
    "Location": "Watsonville, CA",
    "Host": "CalCoastal Dog Owners Group",
    "TrialTypes": "ELT-S, L2C, L3C",
    "EventCount": 3,
    "Latitude": 36.9289,
    "Longitude": -121.7581
  },
  {
    "Date": "2025-05-17",
    "Location": "Burien, WA",
    "Host": "Northwest K9 Sniffers",
    "TrialTypes": "ELT, L2V, L2I",
    "EventCount": 3,
    "Latitude": 47.4372,
    "Longitude": -122.3281
  },
  {
    "Date": "2025-05-17",
    "Location": "Cobleskill, NY",
    "Host": "The Brainy Canine",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 42.6462,
    "Longitude": -74.4681
  },
  {
    "Date": "2025-05-17",
    "Location": "Emmitsburg, MD",
    "Host": "Red Huskies",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 39.7523,
    "Longitude": -77.2777
  },
  {
    "Date": "2025-05-17",
    "Location": "Forest Junction, WI",
    "Host": "N.E.W K9 Scent Work LLC",
    "TrialTypes": "L1C, NW1, NW2",
    "EventCount": 3,
    "Latitude": 44.2259,
    "Longitude": -88.1286
  },
  {
    "Date": "2025-05-17",
    "Location": "Norton, MA",
    "Host": "Dogs Make Scents",
    "TrialTypes": "NW3, NW1",
    "EventCount": 2,
    "Latitude": 41.9599,
    "Longitude": -71.1784
  },
  {
    "Date": "2025-05-17",
    "Location": "Peru, MA",
    "Host": "Lucky Dog Events",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 42.4777,
    "Longitude": -73.0674
  },
  {
    "Date": "2025-05-17",
    "Location": "Valley Forge, PA",
    "Host": "Sniff Sniff Hooray",
    "TrialTypes": "NW1, ELT",
    "EventCount": 2,
    "Latitude": 40.1427,
    "Longitude": -75.42
  },
  {
    "Date": "2025-05-23",
    "Location": "La Jolla, CA",
    "Host": "Anita Cheesman and Jessica Koester",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 32.8314,
    "Longitude": -117.3047
  },
  {
    "Date": "2025-05-24",
    "Location": "Altamont , NY",
    "Host": "My Dog Smells LLC",
    "TrialTypes": "ELT, NW2",
    "EventCount": 2,
    "Latitude": 42.6686,
    "Longitude": -74.0212
  },
  {
    "Date": "2025-05-24",
    "Location": "Batavia, NY",
    "Host": "Do Over Dog Training",
    "TrialTypes": "ELT-P, L1I, L3I",
    "EventCount": 3,
    "Latitude": 42.9571,
    "Longitude": -78.1786
  },
  {
    "Date": "2025-05-24",
    "Location": "Lancaster, PA",
    "Host": "Nose-It-All, LLC",
    "TrialTypes": "L3I, NW2, ELT",
    "EventCount": 3,
    "Latitude": 40.0626,
    "Longitude": -76.3205
  },
  {
    "Date": "2025-05-24",
    "Location": "Norwich , CT",
    "Host": "For the Love of Dogs NY LLC",
    "TrialTypes": "ELT-S, NW2, NW3, ELT",
    "EventCount": 4,
    "Latitude": 41.5387,
    "Longitude": -72.0777
  },
  {
    "Date": "2025-05-24",
    "Location": "Rockaway, NJ",
    "Host": "Shamrock Pot of Gold K9 Scenter",
    "TrialTypes": "NW3, ELT-S, NW1, ELT-P",
    "EventCount": 4,
    "Latitude": 40.8626,
    "Longitude": -74.535
  },
  {
    "Date": "2025-05-24",
    "Location": "Waukesha , WI",
    "Host": "Think Pawsitive Dog Training",
    "TrialTypes": "NW1, L1V, L1C",
    "EventCount": 3,
    "Latitude": 43.02,
    "Longitude": -88.2874
  },
  {
    "Date": "2025-05-24",
    "Location": "Welches, OR",
    "Host": "Trust Your Dog K9 Events",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 45.3486,
    "Longitude": -122.0086
  },
  {
    "Date": "2025-05-29",
    "Location": "Bayfield, CO",
    "Host": "Wag Between Barks",
    "TrialTypes": "ELT-S, ELT, NW3",
    "EventCount": 3,
    "Latitude": 37.2168,
    "Longitude": -107.6184
  },
  {
    "Date": "2025-05-30",
    "Location": "Honesdale, PA",
    "Host": "Your Dog's Place, LLC",
    "TrialTypes": "NW3, ELT, ELT-S, NW2",
    "EventCount": 4,
    "Latitude": 41.5819,
    "Longitude": -75.2185
  },
  {
    "Date": "2025-05-30",
    "Location": "Moline, IL",
    "Host": "Fur Better Fur Worse Dog Training",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 41.5197,
    "Longitude": -90.4668
  },
  {
    "Date": "2025-05-31",
    "Location": "Amherst, NY",
    "Host": "Do Over Dog Training",
    "TrialTypes": "NW2, ELT-S, NW1",
    "EventCount": 3,
    "Latitude": 42.942,
    "Longitude": -78.8183
  },
  {
    "Date": "2025-05-31",
    "Location": "Columbus, MT",
    "Host": "Canine Connection",
    "TrialTypes": "L1V, NW1, NW3",
    "EventCount": 3,
    "Latitude": 45.6752,
    "Longitude": -109.2401
  },
  {
    "Date": "2025-05-31",
    "Location": "Eden Prairie, MN",
    "Host": "The K9 Nose",
    "TrialTypes": "NW1, L1I, L2I",
    "EventCount": 3,
    "Latitude": 44.8447,
    "Longitude": -93.5083
  },
  {
    "Date": "2025-05-31",
    "Location": "Napa, CA",
    "Host": "Napa Valley Dog Training Club",
    "TrialTypes": "NW2, NW1",
    "EventCount": 2,
    "Latitude": 38.5209,
    "Longitude": -122.3116
  },
  {
    "Date": "2025-05-31",
    "Location": "New Wilmington, PA",
    "Host": "Steel City Nosework, LLC",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 41.1182,
    "Longitude": -80.3557
  },
  {
    "Date": "2025-05-31",
    "Location": "North Manchester, IN",
    "Host": "2 Nose You Is 2 Loves You",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 41.0098,
    "Longitude": -85.8097
  },
  {
    "Date": "2025-06-06",
    "Location": "Pueblo, CO",
    "Host": "Mountain Dogs, LLC",
    "TrialTypes": "SMT, ELT",
    "EventCount": 2,
    "Latitude": 38.2399,
    "Longitude": -104.6118
  },
  {
    "Date": "2025-06-06",
    "Location": "Winsted, CT",
    "Host": "Waggin’ Work",
    "TrialTypes": "ELT, NW2, L2C",
    "EventCount": 3,
    "Latitude": 41.9198,
    "Longitude": -73.0906
  },
  {
    "Date": "2025-06-07",
    "Location": "Clancy, MT",
    "Host": "Nosework Breakfast Club",
    "TrialTypes": "NW2, NW3",
    "EventCount": 2,
    "Latitude": 46.4578,
    "Longitude": -111.9946
  },
  {
    "Date": "2025-06-07",
    "Location": "Cumming, GA",
    "Host": "Georgia Nosework",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 34.1656,
    "Longitude": -84.1683
  },
  {
    "Date": "2025-06-07",
    "Location": "Davenport, IA",
    "Host": "Fur Better Fur Worse Dog Training",
    "TrialTypes": "ELT-S, NW2, NW1",
    "EventCount": 3,
    "Latitude": 41.5089,
    "Longitude": -90.541
  },
  {
    "Date": "2025-06-07",
    "Location": "Enterprise, OR",
    "Host": "Country K9 Nosework, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 45.4296,
    "Longitude": -117.2773
  },
  {
    "Date": "2025-06-07",
    "Location": "Grants Pass, OR",
    "Host": "Nose Work Detectives",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 42.4682,
    "Longitude": -123.3347
  },
  {
    "Date": "2025-06-07",
    "Location": "Meadowbrook, PA",
    "Host": "Patricia Grassey",
    "TrialTypes": "NW1, NW2, ELT-S, ELT",
    "EventCount": 4,
    "Latitude": 40.1189,
    "Longitude": -75.1242
  },
  {
    "Date": "2025-06-07",
    "Location": "Palmyra, VA",
    "Host": "Your Dog Knows, LLC",
    "TrialTypes": "NW1, L1I",
    "EventCount": 2,
    "Latitude": 37.826,
    "Longitude": -78.3019
  },
  {
    "Date": "2025-06-07",
    "Location": "Wrightstown, WI",
    "Host": "N.E.W. K9 Scent Work, LLC",
    "TrialTypes": "ELT-P, L2C, L3I",
    "EventCount": 3,
    "Latitude": 44.3449,
    "Longitude": -88.1998
  },
  {
    "Date": "2025-06-13",
    "Location": "Jordan, MN",
    "Host": "St Paul Dog Training Club",
    "TrialTypes": "NW3, ELT-S, L1V, L2E, L3V",
    "EventCount": 5,
    "Latitude": 44.6368,
    "Longitude": -93.6299
  },
  {
    "Date": "2025-06-14",
    "Location": "Cummington, MA",
    "Host": "Lucky Dog Events",
    "TrialTypes": "ELT-P, ELT-S, L2C",
    "EventCount": 3,
    "Latitude": 42.4873,
    "Longitude": -72.9052
  },
  {
    "Date": "2025-06-14",
    "Location": "Danvers, MA",
    "Host": "Everydog, LLC",
    "TrialTypes": "L2I, NW2, NW1",
    "EventCount": 3,
    "Latitude": 42.5993,
    "Longitude": -70.9685
  },
  {
    "Date": "2025-06-14",
    "Location": "Ithaca, NY",
    "Host": "The Brainy Canine",
    "TrialTypes": "ELT-P, ELT",
    "EventCount": 2,
    "Latitude": 42.401,
    "Longitude": -76.5846
  },
  {
    "Date": "2025-06-14",
    "Location": "Linden, MI",
    "Host": "Every Dog Nosework",
    "TrialTypes": "NW1, ELT-P",
    "EventCount": 2,
    "Latitude": 42.8113,
    "Longitude": -83.812
  },
  {
    "Date": "2025-06-20",
    "Location": "Greeley, CO",
    "Host": "Beyond Elevation K9",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 40.381,
    "Longitude": -104.7544
  },
  {
    "Date": "2025-06-20",
    "Location": "San Luis Obispo, CA",
    "Host": "Central Coast Nosework Club of California, Inc.",
    "TrialTypes": "NW2, ELT",
    "EventCount": 2,
    "Latitude": 35.3465,
    "Longitude": -120.4203
  },
  {
    "Date": "2025-06-20",
    "Location": "Upland, CA",
    "Host": "Agile Paws Dog Sports",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 34.1353,
    "Longitude": -117.6053
  },
  {
    "Date": "2025-06-20",
    "Location": "Warwick, NY",
    "Host": "For the Love of Dogs NY LLC",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 41.2356,
    "Longitude": -74.3281
  },
  {
    "Date": "2025-06-21",
    "Location": "Fayette, MO",
    "Host": "Columbia Canine Sports Center",
    "TrialTypes": "ELT, NW1, NW2",
    "EventCount": 3,
    "Latitude": 39.1011,
    "Longitude": -92.6426
  },
  {
    "Date": "2025-06-21",
    "Location": "Inver Grove Heights, MN",
    "Host": "Outside The Box Dog Training, LLC",
    "TrialTypes": "L1C, NW2, ELT",
    "EventCount": 3,
    "Latitude": 44.8012,
    "Longitude": -93.028
  },
  {
    "Date": "2025-06-21",
    "Location": "Jefferson, WI",
    "Host": "Think Pawsitive Dog Training",
    "TrialTypes": "NW2",
    "EventCount": 1,
    "Latitude": 43.0428,
    "Longitude": -88.7269
  },
  {
    "Date": "2025-06-21",
    "Location": "Toledo, OH",
    "Host": "Robin Ford Dog Training, LLC",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 41.6844,
    "Longitude": -83.5314
  },
  {
    "Date": "2025-06-21",
    "Location": "Woodstock, GA",
    "Host": "Georgia Nosework LLC",
    "TrialTypes": "NW3, L2C, NW1",
    "EventCount": 3,
    "Latitude": 34.1032,
    "Longitude": -84.4981
  },
  {
    "Date": "2025-06-25",
    "Location": "Kenai, AK",
    "Host": "Peninsula Dog Obedience Group",
    "TrialTypes": "NW1, NW2, NW3, ELT",
    "EventCount": 4,
    "Latitude": 60.5876,
    "Longitude": -151.2827
  },
  {
    "Date": "2025-06-28",
    "Location": "Delran, NJ",
    "Host": "K9 InScentives",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 39.9787,
    "Longitude": -74.9963
  },
  {
    "Date": "2025-06-28",
    "Location": "Deming, WA",
    "Host": "Nose Work Magic",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 48.8486,
    "Longitude": -122.1967
  },
  {
    "Date": "2025-06-28",
    "Location": "Kenosha, WI",
    "Host": "Loving Paws Dog Training LLC",
    "TrialTypes": "NW3, NW1",
    "EventCount": 2,
    "Latitude": 42.6115,
    "Longitude": -87.8273
  },
  {
    "Date": "2025-06-28",
    "Location": "Somers, CT",
    "Host": "HeavenScent Sniffers",
    "TrialTypes": "ELT, L1V, NW1",
    "EventCount": 3,
    "Latitude": 42.0232,
    "Longitude": -72.4037
  },
  {
    "Date": "2025-06-28",
    "Location": "St. Paul, MN",
    "Host": "Bark and Bond LLC",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 44.9522,
    "Longitude": -93.1052
  },
  {
    "Date": "2025-06-28",
    "Location": "Stevenson, WA",
    "Host": "Sharon Smith",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 45.7155,
    "Longitude": -121.9128
  },
  {
    "Date": "2025-07-04",
    "Location": "Huntington, MA",
    "Host": "Shamrock Pot Of Gold K9 Scenter",
    "TrialTypes": "ELT, NW3, L2I, ELT-S",
    "EventCount": 4,
    "Latitude": 42.1899,
    "Longitude": -72.8799
  },
  {
    "Date": "2025-07-05",
    "Location": "Delran, NJ",
    "Host": "Ev-ry Earthdog, LLC",
    "TrialTypes": "NW3, NW1, NW2, ELT",
    "EventCount": 4,
    "Latitude": 40.017,
    "Longitude": -74.9452
  },
  {
    "Date": "2025-07-11",
    "Location": "Leadville, CO",
    "Host": "Mountain Dogs LLC",
    "TrialTypes": "ELT, NW3, ELT-S, NW2",
    "EventCount": 4,
    "Latitude": 39.2637,
    "Longitude": -106.2863
  },
  {
    "Date": "2025-07-12",
    "Location": "Brainerd, MN",
    "Host": "Nose 2 Tail Dog Training LLC",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 46.3637,
    "Longitude": -94.2359
  },
  {
    "Date": "2025-07-12",
    "Location": "Livonia, MI",
    "Host": "Every Dog Nosework",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 42.3671,
    "Longitude": -83.3922
  },
  {
    "Date": "2025-07-18",
    "Location": "Leadville, CO",
    "Host": "Mountain Dogs, LLC",
    "TrialTypes": "NW3, NW1, NW2, L1C, L1I",
    "EventCount": 5,
    "Latitude": 39.2037,
    "Longitude": -106.3216
  },
  {
    "Date": "2025-07-19",
    "Location": "Dunmore, PA",
    "Host": "Your Dog's Place, LLC",
    "TrialTypes": "ELT-S, NW1, L1C",
    "EventCount": 3,
    "Latitude": 41.4366,
    "Longitude": -75.6366
  },
  {
    "Date": "2025-07-19",
    "Location": "Fayette , MO",
    "Host": "Columbia Canine Sports Center",
    "TrialTypes": "NW3, L1C, L1I",
    "EventCount": 3,
    "Latitude": 39.1036,
    "Longitude": -92.7299
  },
  {
    "Date": "2025-07-19",
    "Location": "Houlton, WI",
    "Host": "Gretchen Hofheins-Wackerfuss",
    "TrialTypes": "NW1, ELT-S, ELT-P",
    "EventCount": 3,
    "Latitude": 45.0235,
    "Longitude": -92.7973
  },
  {
    "Date": "2025-07-19",
    "Location": "Walpole, MA",
    "Host": "MasterPeace Dog Training",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 42.1178,
    "Longitude": -71.2043
  },
  {
    "Date": "2025-07-21",
    "Location": "Montgomery, NY",
    "Host": "For the Love of Dogs NY LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 42.8569,
    "Longitude": -74.4517
  },
  {
    "Date": "2025-08-02",
    "Location": "Altamont, IL",
    "Host": "Kudos for Canines, LLC",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 39.0349,
    "Longitude": -88.6984
  },
  {
    "Date": "2025-08-02",
    "Location": "Anchorage, AK",
    "Host": "Alaska Dog Sports, LLC",
    "TrialTypes": "NW3, NW2, NW1",
    "EventCount": 3,
    "Latitude": 61.1791,
    "Longitude": -149.9074
  },
  {
    "Date": "2025-08-02",
    "Location": "Bettendorf, IA",
    "Host": "Fur Better Fur Worse Dog Training",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 41.484,
    "Longitude": -90.4618
  },
  {
    "Date": "2025-08-02",
    "Location": "Jefferson, WI",
    "Host": "Think Pawsitive Dog Training LLC",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 42.9745,
    "Longitude": -88.7654
  },
  {
    "Date": "2025-08-02",
    "Location": "Pillager, MN",
    "Host": "Nose 2 Tail Dog Training, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 46.3266,
    "Longitude": -94.4853
  },
  {
    "Date": "2025-08-02",
    "Location": "Red Lodge, MT",
    "Host": "Canine Connection",
    "TrialTypes": "L1I, ELT-S, ELT",
    "EventCount": 3,
    "Latitude": 45.1996,
    "Longitude": -109.2782
  },
  {
    "Date": "2025-08-02",
    "Location": "Rochester, NY",
    "Host": "Suzan Tessier",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 43.1928,
    "Longitude": -77.5929
  },
  {
    "Date": "2025-08-11",
    "Location": "Cambria, CA",
    "Host": "Gentle Touch Pet Training",
    "TrialTypes": "L1C, L2I, ELT",
    "EventCount": 3,
    "Latitude": 35.5348,
    "Longitude": -121.0591
  },
  {
    "Date": "2025-08-15",
    "Location": "Huntington Beach, CA",
    "Host": "JavaK9s, LLC",
    "TrialTypes": "ELT-P, L1C, L1I",
    "EventCount": 3,
    "Latitude": 33.698,
    "Longitude": -117.9712
  },
  {
    "Date": "2025-08-16",
    "Location": "Colesville , MD",
    "Host": "Red Huskies",
    "TrialTypes": "L3C, ELT-S, NW2, ELT",
    "EventCount": 4,
    "Latitude": 39.0241,
    "Longitude": -77.0439
  },
  {
    "Date": "2025-08-16",
    "Location": "Mount Kisco, NY",
    "Host": "For the Love of Dogs, LLC",
    "TrialTypes": "L2I, NW2, L1I, NW1",
    "EventCount": 4,
    "Latitude": 41.2235,
    "Longitude": -73.749
  },
  {
    "Date": "2025-08-16",
    "Location": "Reedsport, OR",
    "Host": "Kiddy Christie",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 43.7423,
    "Longitude": -124.1132
  },
  {
    "Date": "2025-08-22",
    "Location": "Chelsea, MI",
    "Host": "Force Free Dale, LLC",
    "TrialTypes": "NW3, NW2, NW1",
    "EventCount": 3,
    "Latitude": 42.3534,
    "Longitude": -84.0328
  },
  {
    "Date": "2025-08-23",
    "Location": "Gervais, OR",
    "Host": "Helix Fairweather & Doglandia, LLC",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 45.0787,
    "Longitude": -122.849
  },
  {
    "Date": "2025-08-23",
    "Location": "Johnstown, NY",
    "Host": "My Dog Smells, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 43.0428,
    "Longitude": -74.3954
  },
  {
    "Date": "2025-08-23",
    "Location": "Tyngsborough, MA",
    "Host": "Spot-On K9 Coaching",
    "TrialTypes": "NW3, L1C, L1I",
    "EventCount": 3,
    "Latitude": 42.6776,
    "Longitude": -71.4487
  },
  {
    "Date": "2025-08-29",
    "Location": "Bridger, MT",
    "Host": "Canine Connection",
    "TrialTypes": "NW1, L2I, NW3",
    "EventCount": 3,
    "Latitude": 45.3368,
    "Longitude": -108.9555
  },
  {
    "Date": "2025-08-30",
    "Location": "Eliot, ME",
    "Host": "McLean Pups, LLC",
    "TrialTypes": "L1V, L1E",
    "EventCount": 2,
    "Latitude": 43.1152,
    "Longitude": -70.7764
  },
  {
    "Date": "2025-08-30",
    "Location": "Fort Worth, TX",
    "Host": "North Texas Nosework Club",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 32.7442,
    "Longitude": -97.3739
  },
  {
    "Date": "2025-08-30",
    "Location": "Pomfret Center, CT",
    "Host": "HeavenScent Sniffers",
    "TrialTypes": "NW3, NW2, NW1",
    "EventCount": 3,
    "Latitude": 41.852,
    "Longitude": -71.9365
  },
  {
    "Date": "2025-08-31",
    "Location": "Helena, MT",
    "Host": "Nosework Breakfast Club",
    "TrialTypes": "NW2, NW3",
    "EventCount": 2,
    "Latitude": 46.6258,
    "Longitude": -112.0469
  },
  {
    "Date": "2025-09-05",
    "Location": "Centreville, MD",
    "Host": "Fair Play Labradors",
    "TrialTypes": "ELT, NW3, ELT-S",
    "EventCount": 3,
    "Latitude": 39.019,
    "Longitude": -76.0194
  },
  {
    "Date": "2025-09-06",
    "Location": "Dunkirk, NY",
    "Host": "Do Over Dog Training",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 42.4542,
    "Longitude": -79.3556
  },
  {
    "Date": "2025-09-06",
    "Location": "Loma Mar, CA",
    "Host": "The Bay Team",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 37.2453,
    "Longitude": -122.3243
  },
  {
    "Date": "2025-09-06",
    "Location": "North Bend, WA",
    "Host": "Northwest K9 Sniffers",
    "TrialTypes": "NW2, L2E, L2C",
    "EventCount": 3,
    "Latitude": 47.5248,
    "Longitude": -121.8257
  },
  {
    "Date": "2025-09-12",
    "Location": "Frederick, MD",
    "Host": "Red Huskies",
    "TrialTypes": "NW3, ELT, ELT-S",
    "EventCount": 3,
    "Latitude": 39.4611,
    "Longitude": -77.4338
  },
  {
    "Date": "2025-09-12",
    "Location": "Honey Brook, PA",
    "Host": "Patricia Grassey",
    "TrialTypes": "ELT, ELT-S, NW3",
    "EventCount": 3,
    "Latitude": 40.0971,
    "Longitude": -75.9602
  },
  {
    "Date": "2025-09-12",
    "Location": "Lakeville, MN",
    "Host": "St Paul Dog Training Club",
    "TrialTypes": "ELT, NW2, NW1",
    "EventCount": 3,
    "Latitude": 44.65,
    "Longitude": -93.2278
  },
  {
    "Date": "2025-09-12",
    "Location": "New Milford, PA",
    "Host": "Your Dog's Place, LLC",
    "TrialTypes": "ELT-P, ELT-S, L2V, L1E",
    "EventCount": 4,
    "Latitude": 41.8469,
    "Longitude": -75.7306
  },
  {
    "Date": "2025-09-13",
    "Location": "Ames, IA",
    "Host": "KBP Dog Training",
    "TrialTypes": "ELT-S, ELT",
    "EventCount": 2,
    "Latitude": 42.0144,
    "Longitude": -93.5863
  },
  {
    "Date": "2025-09-13",
    "Location": "Colebrook, CT",
    "Host": "For the Love of Dogs, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 41.9582,
    "Longitude": -73.0677
  },
  {
    "Date": "2025-09-13",
    "Location": "Hermosa, SD",
    "Host": "Two Paws Up Dog Training, LLC",
    "TrialTypes": "NW3, L1C, L1I",
    "EventCount": 3,
    "Latitude": 43.8116,
    "Longitude": -103.2308
  },
  {
    "Date": "2025-09-13",
    "Location": "Jefferson, GA",
    "Host": "Right Choice Dog Training, LLC",
    "TrialTypes": "L2V, NW2, ELT-S, L1C",
    "EventCount": 4,
    "Latitude": 32.9936,
    "Longitude": -82.4782
  },
  {
    "Date": "2025-09-13",
    "Location": "Pittsburgh , PA",
    "Host": "Steel City Nosework, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 40.4602,
    "Longitude": -80.0162
  },
  {
    "Date": "2025-09-15",
    "Location": "Green Lane, PA",
    "Host": "Sniff Sniff Hooray",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 40.3189,
    "Longitude": -75.4807
  },
  {
    "Date": "2025-09-19",
    "Location": "Flint, MI",
    "Host": "Every Dog Nosework",
    "TrialTypes": "NW3, ELT-S, L1C, NW2",
    "EventCount": 4,
    "Latitude": 43.0475,
    "Longitude": -83.6772
  },
  {
    "Date": "2025-09-19",
    "Location": "Fruita, CO",
    "Host": "Mountain Dogs LLC",
    "TrialTypes": "ELT, NW3, NW2",
    "EventCount": 3,
    "Latitude": 39.1135,
    "Longitude": -108.6895
  },
  {
    "Date": "2025-09-20",
    "Location": "Cumming, GA",
    "Host": "Georgia Nosework",
    "TrialTypes": "NW3, ELT-S, L2I, ELT",
    "EventCount": 4,
    "Latitude": 34.2511,
    "Longitude": -84.1485
  },
  {
    "Date": "2025-09-20",
    "Location": "Darlington, MD",
    "Host": "Firezone GS",
    "TrialTypes": "NW3, L1E, NW2",
    "EventCount": 3,
    "Latitude": 39.6655,
    "Longitude": -76.2318
  },
  {
    "Date": "2025-09-20",
    "Location": "Egg Harbor City, NJ",
    "Host": "Rotts-n-Notts Nosework, LLC",
    "TrialTypes": "NW3, L2I, NW2",
    "EventCount": 3,
    "Latitude": 39.5155,
    "Longitude": -74.5987
  },
  {
    "Date": "2025-09-20",
    "Location": "Fishkill, NY",
    "Host": "Top Notch Dogs, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 41.579,
    "Longitude": -73.8915
  },
  {
    "Date": "2025-09-20",
    "Location": "Mesquite, TX",
    "Host": "All About The Nose",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 32.7752,
    "Longitude": -96.5838
  },
  {
    "Date": "2025-09-20",
    "Location": "Novato, CA",
    "Host": "Marin Humane",
    "TrialTypes": "NW1, NW2, NW3",
    "EventCount": 3,
    "Latitude": 38.0898,
    "Longitude": -122.5343
  },
  {
    "Date": "2025-09-20",
    "Location": "Sunriver, OR",
    "Host": "Canine Discovery Corps",
    "TrialTypes": "NW1, NW2, NW3",
    "EventCount": 3,
    "Latitude": 43.9002,
    "Longitude": -121.4227
  },
  {
    "Date": "2025-09-20",
    "Location": "Tuftonboro, NH",
    "Host": "The Big Sniff, LLC",
    "TrialTypes": "NW2, L2E, L2V",
    "EventCount": 3,
    "Latitude": 43.7212,
    "Longitude": -71.3482
  },
  {
    "Date": "2025-09-20",
    "Location": "White Salmon, WA",
    "Host": "Sharon Smith",
    "TrialTypes": "NW1, NW2, NW3",
    "EventCount": 3,
    "Latitude": 45.6841,
    "Longitude": -121.4984
  },
  {
    "Date": "2025-09-26",
    "Location": "Hockessin, DE",
    "Host": "Patricia Grassey",
    "TrialTypes": "L2E, NW2, NW1, L2I, NW3",
    "EventCount": 5,
    "Latitude": 39.7941,
    "Longitude": -75.6943
  },
  {
    "Date": "2025-09-27",
    "Location": "Copake, NY",
    "Host": "Saints2Source, LLC",
    "TrialTypes": "NW3, ELT-P, ELT",
    "EventCount": 3,
    "Latitude": 42.1492,
    "Longitude": -73.5086
  },
  {
    "Date": "2025-09-27",
    "Location": "Florissant, MO",
    "Host": "Happy Dog Concepts, LLC",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 38.836,
    "Longitude": -90.3385
  },
  {
    "Date": "2025-09-27",
    "Location": "Kilmarnock, VA",
    "Host": "Red Huskies",
    "TrialTypes": "ELT, ELT-S, NW2",
    "EventCount": 3,
    "Latitude": 37.6787,
    "Longitude": -76.3618
  },
  {
    "Date": "2025-09-27",
    "Location": "Moultonborough, NH",
    "Host": "Dogs Makes Scents",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 43.7331,
    "Longitude": -71.381
  },
  {
    "Date": "2025-09-27",
    "Location": "Reedsport, OR",
    "Host": "Wells Creek Dog Training",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 43.7377,
    "Longitude": -124.0516
  },
  {
    "Date": "2025-09-27",
    "Location": "Waynesboro, PA",
    "Host": "Nose-It-All, LLC",
    "TrialTypes": "L3E, NW2, ELT",
    "EventCount": 3,
    "Latitude": 39.7287,
    "Longitude": -77.6077
  },
  {
    "Date": "2025-10-03",
    "Location": "Middlebury, CT",
    "Host": "For the Love of Dogs NY LLC",
    "TrialTypes": "NW3, ELT-S, ELT",
    "EventCount": 3,
    "Latitude": 41.5225,
    "Longitude": -73.1596
  },
  {
    "Date": "2025-10-04",
    "Location": "Crosslake, MN",
    "Host": "Nose 2 Tail Dog Training LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 46.6115,
    "Longitude": -94.1587
  },
  {
    "Date": "2025-10-04",
    "Location": "Nashua, NH",
    "Host": "The Big Sniff, LLC",
    "TrialTypes": "ELT, L3I, L2C",
    "EventCount": 3,
    "Latitude": 42.7315,
    "Longitude": -71.5114
  },
  {
    "Date": "2025-10-04",
    "Location": "New Paltz, NY",
    "Host": "Top Notch Dogs, LLC",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 41.7815,
    "Longitude": -74.1043
  },
  {
    "Date": "2025-10-04",
    "Location": "Smithton, PA",
    "Host": "Steel City Nosework, LLC",
    "TrialTypes": "NW2, ELT",
    "EventCount": 2,
    "Latitude": 40.1496,
    "Longitude": -79.7413
  },
  {
    "Date": "2025-10-06",
    "Location": "Monterey, CA",
    "Host": "CalCoastal Dog Owners Group",
    "TrialTypes": "ELT-S, NW1, L2I, NW2",
    "EventCount": 4,
    "Latitude": 36.2106,
    "Longitude": -121.4086
  },
  {
    "Date": "2025-10-10",
    "Location": "West Bend, WI",
    "Host": "Think Pawsitive Dog Training LLC",
    "TrialTypes": "ELT, L2C, L1E",
    "EventCount": 3,
    "Latitude": 43.4236,
    "Longitude": -88.1344
  },
  {
    "Date": "2025-10-11",
    "Location": "Bloomington, MN",
    "Host": "St Paul Dog Training Club",
    "TrialTypes": "ELT-P, ELT-S",
    "EventCount": 2,
    "Latitude": 44.834,
    "Longitude": -93.2811
  },
  {
    "Date": "2025-10-11",
    "Location": "Colfax, IA",
    "Host": "KBP Dog Training",
    "TrialTypes": "NW1, NW2, NW3",
    "EventCount": 3,
    "Latitude": 41.6608,
    "Longitude": -93.2539
  },
  {
    "Date": "2025-10-11",
    "Location": "Columbus, MT",
    "Host": "Canine Connection",
    "TrialTypes": "L1C, NW2, NW3",
    "EventCount": 3,
    "Latitude": 45.6482,
    "Longitude": -109.2961
  },
  {
    "Date": "2025-10-11",
    "Location": "Durham, NC",
    "Host": "Whole Dog Institute, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 35.9617,
    "Longitude": -78.9503
  },
  {
    "Date": "2025-10-11",
    "Location": "Ferndale, WA",
    "Host": "Nose Work Magic",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 48.8688,
    "Longitude": -122.5501
  },
  {
    "Date": "2025-10-11",
    "Location": "Loveland, CO",
    "Host": "Paws 4 Thought Dog Training, LLC",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 40.3605,
    "Longitude": -105.1187
  },
  {
    "Date": "2025-10-11",
    "Location": "New City , NY",
    "Host": "Saints2Source, LLC",
    "TrialTypes": "NW2, NW3, ELT",
    "EventCount": 3,
    "Latitude": 41.1151,
    "Longitude": -73.9792
  },
  {
    "Date": "2025-10-11",
    "Location": "Occidental, CA",
    "Host": "Marin Humane",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 38.3629,
    "Longitude": -122.8865
  },
  {
    "Date": "2025-10-11",
    "Location": "Roseburg, OR",
    "Host": "Canine Discovery Corps",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 43.2249,
    "Longitude": -123.382
  },
  {
    "Date": "2025-10-11",
    "Location": "Sedona, AZ",
    "Host": "Successful Sniffer",
    "TrialTypes": "ELT-P, ELT, NW3",
    "EventCount": 3,
    "Latitude": 34.8935,
    "Longitude": -111.7464
  },
  {
    "Date": "2025-10-17",
    "Location": "Elizabeth, CO",
    "Host": "Beyond Elevation K9",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 39.3787,
    "Longitude": -104.5816
  },
  {
    "Date": "2025-10-17",
    "Location": "Lawrenceville, GA",
    "Host": "Chestnut Hill Canine Sports",
    "TrialTypes": "NW3, L2C, NW1",
    "EventCount": 3,
    "Latitude": 33.9098,
    "Longitude": -83.9495
  },
  {
    "Date": "2025-10-17",
    "Location": "Newfoundland, PA",
    "Host": "Your Dog's Place, LLC",
    "TrialTypes": "L1V, ELT-S, L2C, L2E",
    "EventCount": 4,
    "Latitude": 41.291,
    "Longitude": -75.3519
  },
  {
    "Date": "2025-10-18",
    "Location": "Centralia, WA",
    "Host": "Rachelle Bailey-Austin/About Face K9 Academy & Dorothy Turley/Let's Talk Dogs, LLC",
    "TrialTypes": "L3I, L2C, NW2",
    "EventCount": 3,
    "Latitude": 46.6888,
    "Longitude": -122.9293
  },
  {
    "Date": "2025-10-18",
    "Location": "Delevan, NY",
    "Host": "Do Over Dog Training",
    "TrialTypes": "ELT, ELT-S, NW2",
    "EventCount": 3,
    "Latitude": 42.4927,
    "Longitude": -78.4551
  },
  {
    "Date": "2025-10-18",
    "Location": "Lafayette Hill, PA",
    "Host": "Sniff Sniff Hooray",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 40.107,
    "Longitude": -75.2497
  },
  {
    "Date": "2025-10-18",
    "Location": "Milton, NH",
    "Host": "Lucky Dog Events",
    "TrialTypes": "NW2, NW3",
    "EventCount": 2,
    "Latitude": 43.4565,
    "Longitude": -70.9596
  },
  {
    "Date": "2025-10-18",
    "Location": "Nevada City, CA",
    "Host": "Sierra Sniffing Canines",
    "TrialTypes": "L1I, L2I, ELT",
    "EventCount": 3,
    "Latitude": 39.2878,
    "Longitude": -121.0101
  },
  {
    "Date": "2025-10-18",
    "Location": "Terryville, CT",
    "Host": "Willoughby Training",
    "TrialTypes": "NW2, NW1",
    "EventCount": 2,
    "Latitude": 41.6298,
    "Longitude": -73.0465
  },
  {
    "Date": "2025-10-18",
    "Location": "Troy, VA",
    "Host": "Your Dog Knows, LLC",
    "TrialTypes": "ELT, L1V, L2V",
    "EventCount": 3,
    "Latitude": 37.9853,
    "Longitude": -78.2952
  },
  {
    "Date": "2025-10-18",
    "Location": "Watsonville, CA",
    "Host": "CalCoastal Dog Owners Group",
    "TrialTypes": "L3V, L2V, L1V",
    "EventCount": 3,
    "Latitude": 36.9187,
    "Longitude": -121.7405
  },
  {
    "Date": "2025-10-19",
    "Location": "San Martin, CA",
    "Host": "B.L. McMutts",
    "TrialTypes": "L1V, L3V",
    "EventCount": 2,
    "Latitude": 37.1197,
    "Longitude": -121.5887
  },
  {
    "Date": "2025-10-24",
    "Location": "Calhan, CO",
    "Host": "Mountain Dogs, LLC",
    "TrialTypes": "ELT, SMT",
    "EventCount": 2,
    "Latitude": 39.0574,
    "Longitude": -104.2975
  },
  {
    "Date": "2025-10-24",
    "Location": "Easton, MD",
    "Host": "Fair Play Point Labradors",
    "TrialTypes": "ELT, ELT-S, L2V, L2C, L3C",
    "EventCount": 5,
    "Latitude": 38.7443,
    "Longitude": -76.0641
  },
  {
    "Date": "2025-10-24",
    "Location": "Palmyra, VA",
    "Host": "Paws Plus Training, LLC",
    "TrialTypes": "NW3, NW1, NW2, ELT-P",
    "EventCount": 4,
    "Latitude": 37.8353,
    "Longitude": -78.2612
  },
  {
    "Date": "2025-10-24",
    "Location": "Ypsilanti, MI",
    "Host": "Every Dog Nosework",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 42.2461,
    "Longitude": -83.5836
  },
  {
    "Date": "2025-10-25",
    "Location": "Auburn, WA",
    "Host": "Northwest K9 Sniffers",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 47.263,
    "Longitude": -122.2036
  },
  {
    "Date": "2025-10-25",
    "Location": "Fishkill, NY",
    "Host": "Top Notch Dogs, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 41.565,
    "Longitude": -73.8506
  },
  {
    "Date": "2025-10-25",
    "Location": "Lyle, WA",
    "Host": "Trust Your Dog K9 Events",
    "TrialTypes": "NW3, L3I, L3C",
    "EventCount": 3,
    "Latitude": 45.6988,
    "Longitude": -121.3227
  },
  {
    "Date": "2025-10-25",
    "Location": "Niantic, IL",
    "Host": "Kudos for Canines, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 39.8929,
    "Longitude": -89.1175
  },
  {
    "Date": "2025-10-25",
    "Location": "Poland Springs, ME",
    "Host": "Virginia Howe",
    "TrialTypes": "NW1, NW3",
    "EventCount": 2,
    "Latitude": 43.9983,
    "Longitude": -70.3974
  },
  {
    "Date": "2025-10-25",
    "Location": "West Friendship, MD",
    "Host": "Red Huskies",
    "TrialTypes": "NW3, NW1, L1C",
    "EventCount": 3,
    "Latitude": 39.3255,
    "Longitude": -76.9617
  },
  {
    "Date": "2025-10-27",
    "Location": "Clayton, GA",
    "Host": "Right Choice Dog Training LLC",
    "TrialTypes": "NW3, ELT, ELT-P",
    "EventCount": 3,
    "Latitude": 33.4789,
    "Longitude": -84.4055
  },
  {
    "Date": "2025-10-27",
    "Location": "Paicines, CA",
    "Host": "Two Nosey Girls",
    "TrialTypes": "NW2, ELT-P",
    "EventCount": 2,
    "Latitude": 36.702,
    "Longitude": -121.2359
  },
  {
    "Date": "2025-10-31",
    "Location": "Cannon Falls, MN",
    "Host": "St Paul Dog Training Club",
    "TrialTypes": "SMT, NW3",
    "EventCount": 2,
    "Latitude": 44.5373,
    "Longitude": -92.9553
  },
  {
    "Date": "2025-10-31",
    "Location": "Loranger, LA",
    "Host": "Dog Gone Right",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 30.6371,
    "Longitude": -90.445
  },
  {
    "Date": "2025-10-31",
    "Location": "Meeker, CO",
    "Host": "Mountain Dogs LLC",
    "TrialTypes": "ELT, NW3, NW1, NW2",
    "EventCount": 4,
    "Latitude": 40.0085,
    "Longitude": -107.8956
  },
  {
    "Date": "2025-10-31",
    "Location": "Scotts Mills, OR",
    "Host": "Canine Discovery Corps",
    "TrialTypes": "ELT, NW1, L3C",
    "EventCount": 3,
    "Latitude": 45.0794,
    "Longitude": -122.69
  },
  {
    "Date": "2025-11-01",
    "Location": "Beloit, WI",
    "Host": "George Carpenter",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 42.5235,
    "Longitude": -89.0156
  },
  {
    "Date": "2025-11-01",
    "Location": "Charlton, MA",
    "Host": "HeavenScent Sniffers",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 42.1344,
    "Longitude": -71.921
  },
  {
    "Date": "2025-11-01",
    "Location": "Kennebunkport, ME",
    "Host": "Elizabeth Dutton",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 43.3974,
    "Longitude": -70.461
  },
  {
    "Date": "2025-11-01",
    "Location": "Mill Spring, NC",
    "Host": "Foothills Canine Academy, LLC",
    "TrialTypes": "NW1, NW2, ELT",
    "EventCount": 3,
    "Latitude": 35.2569,
    "Longitude": -82.167
  },
  {
    "Date": "2025-11-01",
    "Location": "Monkton, MD",
    "Host": "Firezone GS",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 39.5376,
    "Longitude": -76.5755
  },
  {
    "Date": "2025-11-01",
    "Location": "Wappingers Falls, NY",
    "Host": "Top Notch Dogs, LLC",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 41.5651,
    "Longitude": -73.9025
  },
  {
    "Date": "2025-11-01",
    "Location": "Woodstock, GA",
    "Host": "Georgia Nosework LLC",
    "TrialTypes": "L1C, L3I, L3C, L1I",
    "EventCount": 4,
    "Latitude": 34.0904,
    "Longitude": -84.5088
  },
  {
    "Date": "2025-11-01",
    "Location": "Yamhill, OR",
    "Host": "Nose Work Detectives",
    "TrialTypes": "ELT-S, L1C",
    "EventCount": 2,
    "Latitude": 45.2004,
    "Longitude": -123.2329
  },
  {
    "Date": "2025-11-05",
    "Location": "Ventura, CA",
    "Host": "Pink Biscuit K9s",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 34.4566,
    "Longitude": -119.0565
  },
  {
    "Date": "2025-11-07",
    "Location": "Montrose, CO",
    "Host": "Mountain Dogs LLC",
    "TrialTypes": "ELT-S, NW2, ELT",
    "EventCount": 3,
    "Latitude": 38.5142,
    "Longitude": -107.8275
  },
  {
    "Date": "2025-11-07",
    "Location": "Rancho Cucamonga , CA",
    "Host": "Agile Paws Dog Sports",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 34.1236,
    "Longitude": -117.6089
  },
  {
    "Date": "2025-11-08",
    "Location": "Coburg, OR",
    "Host": "Kiddy Christie",
    "TrialTypes": "NW1, NW2, ELT-S, L1E",
    "EventCount": 4,
    "Latitude": 44.1096,
    "Longitude": -123.0168
  },
  {
    "Date": "2025-11-08",
    "Location": "Elkhorn, WI",
    "Host": "Loving Paws Dog Training LLC",
    "TrialTypes": "ELT, NW2",
    "EventCount": 2,
    "Latitude": 42.6464,
    "Longitude": -88.5065
  },
  {
    "Date": "2025-11-08",
    "Location": "Guerneville, CA",
    "Host": "Jen Huot",
    "TrialTypes": "NW3, ELT, ELT-P",
    "EventCount": 3,
    "Latitude": 38.5158,
    "Longitude": -122.9773
  },
  {
    "Date": "2025-11-08",
    "Location": "Mays Landing, NJ",
    "Host": "Rotts-n-Notts Nosework, LLC",
    "TrialTypes": "NW3, L2E, NW2",
    "EventCount": 3,
    "Latitude": 39.4565,
    "Longitude": -74.7102
  },
  {
    "Date": "2025-11-08",
    "Location": "Pine Grove, PA",
    "Host": "Paws n' Sniff",
    "TrialTypes": "ELT-P, NW3",
    "EventCount": 2,
    "Latitude": 40.5216,
    "Longitude": -76.3523
  },
  {
    "Date": "2025-11-08",
    "Location": "Tucson, AZ",
    "Host": "Patience Unlimited Professional Dog Training",
    "TrialTypes": "NW3, L2C, NW2",
    "EventCount": 3,
    "Latitude": 32.1777,
    "Longitude": -110.9853
  },
  {
    "Date": "2025-11-11",
    "Location": "Guerneville, CA",
    "Host": "Jen Huot",
    "TrialTypes": "ELT-P",
    "EventCount": 1,
    "Latitude": 38.5265,
    "Longitude": -123.0094
  },
  {
    "Date": "2025-11-12",
    "Location": "Astoria, OR",
    "Host": "Nose Work Detectives, LLC",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 46.2157,
    "Longitude": -123.7941
  },
  {
    "Date": "2025-11-14",
    "Location": "Elgin, IL",
    "Host": "Common Scents K9",
    "TrialTypes": "L1C, L2C, L1I, L2I, NW1",
    "EventCount": 5,
    "Latitude": 42.0205,
    "Longitude": -88.2805
  },
  {
    "Date": "2025-11-14",
    "Location": "New Freedom, PA",
    "Host": "Firezone GS",
    "TrialTypes": "ELT, NW3, ELT-S, NW2",
    "EventCount": 4,
    "Latitude": 39.7432,
    "Longitude": -76.7081
  },
  {
    "Date": "2025-11-15",
    "Location": "Albuquerque, NM",
    "Host": "New Mexico Canine Scent Work, LLC",
    "TrialTypes": "NW3, L1C, NW2",
    "EventCount": 3,
    "Latitude": 35.0541,
    "Longitude": -106.6688
  },
  {
    "Date": "2025-11-15",
    "Location": "Bonham, TX",
    "Host": "All About The Nose",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 33.5389,
    "Longitude": -96.2112
  },
  {
    "Date": "2025-11-15",
    "Location": "Bradenton, FL",
    "Host": "Hoppin' in the Hills",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 27.5379,
    "Longitude": -82.6109
  },
  {
    "Date": "2025-11-15",
    "Location": "Eldred, NY",
    "Host": "Your Dog's Place, LLC",
    "TrialTypes": "L1I, NW2, L3I, L3C",
    "EventCount": 4,
    "Latitude": 41.561,
    "Longitude": -74.8553
  },
  {
    "Date": "2025-11-15",
    "Location": "Marbury, AL",
    "Host": "Kaye Stevenson",
    "TrialTypes": "NW2, NW1, L1E",
    "EventCount": 3,
    "Latitude": 32.6968,
    "Longitude": -86.4976
  },
  {
    "Date": "2025-11-15",
    "Location": "Montgomery, AL",
    "Host": "By A Nose Nosework",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 32.3578,
    "Longitude": -86.2662
  },
  {
    "Date": "2025-11-15",
    "Location": "Welches, OR",
    "Host": "Trust Your Dog K9 Events",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 45.344,
    "Longitude": -121.9328
  },
  {
    "Date": "2025-11-17",
    "Location": "Hudson, MA",
    "Host": "For the Love of Dogs NY LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 42.385,
    "Longitude": -71.5705
  },
  {
    "Date": "2025-11-21",
    "Location": "Harrington, DE",
    "Host": "Shamrock Pot Of Gold K9 Scenter",
    "TrialTypes": "NW3, ELT-S, NW2, NW1, L1C, ELT",
    "EventCount": 6,
    "Latitude": 38.9396,
    "Longitude": -75.6208
  },
  {
    "Date": "2025-11-21",
    "Location": "San Luis Obispo, CA",
    "Host": "Gentle Touch Pet Training",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 35.358,
    "Longitude": -120.3733
  },
  {
    "Date": "2025-11-22",
    "Location": "Crownsville, MD",
    "Host": "Red Huskies",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 38.9722,
    "Longitude": -76.5485
  },
  {
    "Date": "2025-11-22",
    "Location": "Defuniak Springs, FL",
    "Host": "Linda Culliton",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 30.7202,
    "Longitude": -86.1064
  },
  {
    "Date": "2025-11-22",
    "Location": "Delta, CO",
    "Host": "Mountain Dogs, LLC",
    "TrialTypes": "ELT, NW3, ELT-S, NW2",
    "EventCount": 4,
    "Latitude": 38.8316,
    "Longitude": -107.8824
  },
  {
    "Date": "2025-11-22",
    "Location": "Foxborough , MA",
    "Host": "MasterPeace Dog Training",
    "TrialTypes": "L3C, NW1, NW2",
    "EventCount": 3,
    "Latitude": 42.0445,
    "Longitude": -71.2806
  },
  {
    "Date": "2025-11-22",
    "Location": "Kintnersville, PA",
    "Host": "Paws n' Sniff",
    "TrialTypes": "L1C, NW1, L2I, L2E",
    "EventCount": 4,
    "Latitude": 40.5549,
    "Longitude": -75.16
  },
  {
    "Date": "2025-11-22",
    "Location": "Marble Falls, TX",
    "Host": "Scent Work Across Texas",
    "TrialTypes": "ELT-S, NW2, NW1, L1I",
    "EventCount": 4,
    "Latitude": 30.5655,
    "Longitude": -98.2742
  },
  {
    "Date": "2025-11-22",
    "Location": "Medford, NJ",
    "Host": "K9 InScentives",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 39.8848,
    "Longitude": -74.7998
  },
  {
    "Date": "2025-11-22",
    "Location": "Norton, MA",
    "Host": "Dogs Make Scents",
    "TrialTypes": "ELT, L1E, L1C",
    "EventCount": 3,
    "Latitude": 41.9259,
    "Longitude": -71.1449
  },
  {
    "Date": "2025-11-22",
    "Location": "Salem Lakes, WI",
    "Host": "Loving Paws Dog Training, LLC",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 42.5486,
    "Longitude": -88.1041
  },
  {
    "Date": "2025-11-22",
    "Location": "Smyrna, TN",
    "Host": "Dogs Have Amazing Noses, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 35.9995,
    "Longitude": -86.508
  },
  {
    "Date": "2025-11-28",
    "Location": "Dana Point, CA",
    "Host": "JavaK9s, LLC",
    "TrialTypes": "NW3, ELT-S",
    "EventCount": 2,
    "Latitude": 33.5093,
    "Longitude": -117.716
  },
  {
    "Date": "2025-11-28",
    "Location": "San Jose, CA",
    "Host": "The Bay Team",
    "TrialTypes": "NW2, NW1",
    "EventCount": 2,
    "Latitude": 37.382,
    "Longitude": -121.8927
  },
  {
    "Date": "2025-11-29",
    "Location": "Alpharetta, GA",
    "Host": "Georgia Nosework",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 34.0786,
    "Longitude": -84.2506
  },
  {
    "Date": "2025-11-29",
    "Location": "Canandaigua, NY",
    "Host": "Savvy Dog Sports",
    "TrialTypes": "ELT, NW2",
    "EventCount": 2,
    "Latitude": 42.8953,
    "Longitude": -77.2763
  },
  {
    "Date": "2025-11-29",
    "Location": "Cottage Grove, MN",
    "Host": "Gretchen Hofheins-Wackerfuss",
    "TrialTypes": "ELT-S, ELT-P",
    "EventCount": 2,
    "Latitude": 44.7954,
    "Longitude": -92.9401
  },
  {
    "Date": "2025-11-29",
    "Location": "Lebanon, NJ",
    "Host": "Sirius K9 Solutions",
    "TrialTypes": "NW3, L3I, ELT-S",
    "EventCount": 3,
    "Latitude": 40.6473,
    "Longitude": -74.8363
  },
  {
    "Date": "2025-12-05",
    "Location": "Bowie, MD",
    "Host": "Red Huskies",
    "TrialTypes": "ELT, ELT-P, ELT-S",
    "EventCount": 3,
    "Latitude": 38.9241,
    "Longitude": -76.77
  },
  {
    "Date": "2025-12-06",
    "Location": "Annapolis, MD",
    "Host": "Chesapeake Search Dogs",
    "TrialTypes": "NW3, NW2, L2C",
    "EventCount": 3,
    "Latitude": 38.9895,
    "Longitude": -76.5314
  },
  {
    "Date": "2025-12-06",
    "Location": "Auburn, WA",
    "Host": "Northwest K9 Sniffers",
    "TrialTypes": "L1C, L2C, NW3",
    "EventCount": 3,
    "Latitude": 47.3333,
    "Longitude": -122.2152
  },
  {
    "Date": "2025-12-06",
    "Location": "Centralia, WA",
    "Host": "About Face K9 Academy and Let's Talk Dogs",
    "TrialTypes": "NW3, NW1",
    "EventCount": 2,
    "Latitude": 46.6811,
    "Longitude": -122.9977
  },
  {
    "Date": "2025-12-06",
    "Location": "Fillmore, CA",
    "Host": "Pink Biscuit K9s",
    "TrialTypes": "NW1, NW2, ELT",
    "EventCount": 3,
    "Latitude": 34.3856,
    "Longitude": -118.9231
  },
  {
    "Date": "2025-12-06",
    "Location": "Hoover, AL",
    "Host": "Southeast Scent Work Alliance, LLC",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 33.3982,
    "Longitude": -86.839
  },
  {
    "Date": "2025-12-06",
    "Location": "Kittanning, PA",
    "Host": "Steel City Nosework, LLC",
    "TrialTypes": "NW3, ELT-S, NW1",
    "EventCount": 3,
    "Latitude": 40.7953,
    "Longitude": -79.5543
  },
  {
    "Date": "2025-12-06",
    "Location": "Newfoundland, PA",
    "Host": "Your Dog's Place, LLC",
    "TrialTypes": "L2I, L3V, NW3",
    "EventCount": 3,
    "Latitude": 41.2865,
    "Longitude": -75.3066
  },
  {
    "Date": "2025-12-07",
    "Location": "Cape Coral, FL",
    "Host": "Your Dog Knows, LLC",
    "TrialTypes": "NW1",
    "EventCount": 1,
    "Latitude": 26.6123,
    "Longitude": -81.9516
  },
  {
    "Date": "2025-12-12",
    "Location": "Douglassville, PA",
    "Host": "Patricia Grassey",
    "TrialTypes": "NW3, ELT, ELT-P",
    "EventCount": 3,
    "Latitude": 40.3024,
    "Longitude": -75.7532
  },
  {
    "Date": "2025-12-12",
    "Location": "Pittstown, NJ",
    "Host": "Shamrock Pot Of Gold K9 Scenter",
    "TrialTypes": "NW3, ELT, ELT-S, NW1",
    "EventCount": 4,
    "Latitude": 40.5498,
    "Longitude": -74.9691
  },
  {
    "Date": "2025-12-13",
    "Location": "DeLeon Springs, FL",
    "Host": "River Poodles Training, LLC",
    "TrialTypes": "ELT-P, ELT-S, L2C",
    "EventCount": 3,
    "Latitude": 29.1237,
    "Longitude": -81.3683
  },
  {
    "Date": "2025-12-13",
    "Location": "Easton, MA",
    "Host": "South Coast Scent Dogs",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 42.0679,
    "Longitude": -71.1274
  },
  {
    "Date": "2025-12-13",
    "Location": "Escondido, CA",
    "Host": "Uber Dog and Rewarding Rover LLC",
    "TrialTypes": "NW2",
    "EventCount": 1,
    "Latitude": 33.0881,
    "Longitude": -117.0329
  },
  {
    "Date": "2025-12-13",
    "Location": "Greer, SC",
    "Host": "Trained to Trust LLC",
    "TrialTypes": "ELT, ELT-S, NW2",
    "EventCount": 3,
    "Latitude": 34.9841,
    "Longitude": -82.2475
  },
  {
    "Date": "2025-12-13",
    "Location": "Independence, OR",
    "Host": "Doglandia, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 44.8698,
    "Longitude": -123.1476
  },
  {
    "Date": "2025-12-13",
    "Location": "Westminster, MD",
    "Host": "Red Huskies",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 39.5517,
    "Longitude": -77.042
  },
  {
    "Date": "2025-12-16",
    "Location": "Duluth, GA",
    "Host": "Right Choice Dog Training LLC",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 33.9557,
    "Longitude": -84.191
  },
  {
    "Date": "2025-12-20",
    "Location": "Cumming, GA",
    "Host": "Georgia Nosework LLC",
    "TrialTypes": "SMT, ELT",
    "EventCount": 2,
    "Latitude": 34.217,
    "Longitude": -84.1751
  },
  {
    "Date": "2025-12-20",
    "Location": "Florissant, MO",
    "Host": "Happy Dog Concepts, LLC",
    "TrialTypes": "ELT-P",
    "EventCount": 1,
    "Latitude": 38.7545,
    "Longitude": -90.2791
  },
  {
    "Date": "2025-12-20",
    "Location": "Salem, OR",
    "Host": "Helix Fairweather & Doglandia, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 44.9112,
    "Longitude": -123.0199
  },
  {
    "Date": "2025-12-20",
    "Location": "Silex, MO",
    "Host": "WestInn Kennels",
    "TrialTypes": "NW1, NW2, NW3",
    "EventCount": 3,
    "Latitude": 39.1385,
    "Longitude": -91.0923
  },
  {
    "Date": "2025-12-20",
    "Location": "Stockton, CA",
    "Host": "Two Nosey Girls",
    "TrialTypes": "NW3, NW1",
    "EventCount": 2,
    "Latitude": 37.9789,
    "Longitude": -121.2901
  },
  {
    "Date": "2025-12-27",
    "Location": "Auburn, AL",
    "Host": "Daphne Melillo",
    "TrialTypes": "NW2, NW1",
    "EventCount": 2,
    "Latitude": 32.6406,
    "Longitude": -85.4439
  },
  {
    "Date": "2025-12-27",
    "Location": "Fort Morgan, CO",
    "Host": "Paws 4 Thought Dog Training, LLC",
    "TrialTypes": "L1V, NW2, NW1, L1I",
    "EventCount": 4,
    "Latitude": 40.2146,
    "Longitude": -103.7879
  },
  {
    "Date": "2025-12-27",
    "Location": "New Rochelle, NY",
    "Host": "For the Love of Dogs NY LLC",
    "TrialTypes": "NW3, ELT-S, ELT",
    "EventCount": 3,
    "Latitude": 40.8651,
    "Longitude": -73.8149
  },
  {
    "Date": "2025-12-27",
    "Location": "White Plains, NY",
    "Host": "Saints2Source",
    "TrialTypes": "NW1, NW2, L2E, L2C",
    "EventCount": 4,
    "Latitude": 41.0038,
    "Longitude": -73.8047
  },
  {
    "Date": "2025-12-28",
    "Location": "Exton, PA",
    "Host": "Patricia Grassey",
    "TrialTypes": "ELT, ELT-P, NW3",
    "EventCount": 3,
    "Latitude": 40.0427,
    "Longitude": -75.657
  },
  {
    "Date": "2025-12-28",
    "Location": "Waukesha, WI",
    "Host": "Think Pawsitive Dog Training",
    "TrialTypes": "NW2, NW1",
    "EventCount": 2,
    "Latitude": 43.1059,
    "Longitude": -88.2846
  },
  {
    "Date": "2025-12-29",
    "Location": "Barrington, RI",
    "Host": "Bay State Sniffers",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 41.7698,
    "Longitude": -71.3204
  },
  {
    "Date": "2026-01-02",
    "Location": "Emmitsburg , MD",
    "Host": "Red Huskies",
    "TrialTypes": "NW3, ELT, ELT-S",
    "EventCount": 3,
    "Latitude": 39.749,
    "Longitude": -77.3205
  },
  {
    "Date": "2026-01-03",
    "Location": "Bonsall, CA",
    "Host": "Linda Buchanan",
    "TrialTypes": "NW3, ELT-P",
    "EventCount": 2,
    "Latitude": 33.3198,
    "Longitude": -117.1546
  },
  {
    "Date": "2026-01-03",
    "Location": "Green Cove Springs, FL",
    "Host": "Hoppin' in the Hills",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 30.0401,
    "Longitude": -81.6828
  },
  {
    "Date": "2026-01-03",
    "Location": "Maryville, TN",
    "Host": "Rachel Hawkins",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 35.7843,
    "Longitude": -83.9919
  },
  {
    "Date": "2026-01-03",
    "Location": "Montevallo, AL",
    "Host": "Southeast Scent Work Alliance, LLC",
    "TrialTypes": "NW1",
    "EventCount": 1,
    "Latitude": 33.1202,
    "Longitude": -86.8649
  },
  {
    "Date": "2026-01-09",
    "Location": "Hartfield, VA",
    "Host": "Paws Plus Training, LLC",
    "TrialTypes": "NW3, NW1, NW2, ELT",
    "EventCount": 4,
    "Latitude": 37.5607,
    "Longitude": -76.4752
  },
  {
    "Date": "2026-01-09",
    "Location": "Spring City, PA",
    "Host": "Patricia Grassey",
    "TrialTypes": "NW3, NW2, NW1, L2I",
    "EventCount": 4,
    "Latitude": 40.183,
    "Longitude": -75.5302
  },
  {
    "Date": "2026-01-10",
    "Location": "Canton , GA",
    "Host": "Run Spot Jump Dog Training",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 34.238,
    "Longitude": -84.4901
  },
  {
    "Date": "2026-01-10",
    "Location": "Pflugerville, TX",
    "Host": "Scent Work Across Texas",
    "TrialTypes": "ELT-S, L2C, NW3",
    "EventCount": 3,
    "Latitude": 30.403,
    "Longitude": -97.664
  },
  {
    "Date": "2026-01-10",
    "Location": "Valencia, CA",
    "Host": "Pink Biscuit K9s",
    "TrialTypes": "NW1, NW2, ELT",
    "EventCount": 3,
    "Latitude": 34.3763,
    "Longitude": -118.516
  },
  {
    "Date": "2026-01-17",
    "Location": "Clanton, AL",
    "Host": "By A Nose Nosework",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 32.859,
    "Longitude": -86.612
  },
  {
    "Date": "2026-01-17",
    "Location": "Melrose, FL",
    "Host": "River Poodles Training, LLC",
    "TrialTypes": "NW1, NW2, L1V, L1E",
    "EventCount": 4,
    "Latitude": 29.7245,
    "Longitude": -82.0762
  },
  {
    "Date": "2026-01-17",
    "Location": "New Rochelle, NY",
    "Host": "For the Love of Dogs NY LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 40.8898,
    "Longitude": -73.7438
  },
  {
    "Date": "2026-01-17",
    "Location": "San Marcos, CA",
    "Host": "Rewarding Rover LLC and Uberdog",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 33.1278,
    "Longitude": -117.1896
  },
  {
    "Date": "2026-01-17",
    "Location": "Tecumseh, OK",
    "Host": "The Doggie Spot, LLC",
    "TrialTypes": "ELT, NW3, NW2",
    "EventCount": 3,
    "Latitude": 35.3036,
    "Longitude": -96.9098
  },
  {
    "Date": "2026-01-19",
    "Location": "Redlands, CA",
    "Host": "Agile Paws Dog Sports",
    "TrialTypes": "NW3, ELT-P",
    "EventCount": 2,
    "Latitude": 34.061,
    "Longitude": -117.1688
  },
  {
    "Date": "2026-01-20",
    "Location": "Murfreesboro, TN",
    "Host": "Dogs Have Amazing Noses, LLC",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 35.8382,
    "Longitude": -86.378
  },
  {
    "Date": "2026-01-23",
    "Location": "Rome, GA",
    "Host": "Georgia Nosework, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 34.2811,
    "Longitude": -85.1887
  },
  {
    "Date": "2026-01-30",
    "Location": "Greeley, CO",
    "Host": "Beyond Elevation K9 Training",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 40.3857,
    "Longitude": -104.7443
  },
  {
    "Date": "2026-01-31",
    "Location": "Denton, MD",
    "Host": "Fair Play Labradors",
    "TrialTypes": "ELT-S, L2C, NW1, L1E",
    "EventCount": 4,
    "Latitude": 38.9256,
    "Longitude": -75.8181
  },
  {
    "Date": "2026-01-31",
    "Location": "Tucson, AZ",
    "Host": "Patience Unlimited Professional Dog Training",
    "TrialTypes": "NW3, ELT-S, L1V",
    "EventCount": 3,
    "Latitude": 32.208,
    "Longitude": -110.9896
  },
  {
    "Date": "2026-02-07",
    "Location": "Lakewood, NJ",
    "Host": "Rotts n Notts Nosework",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 40.1371,
    "Longitude": -74.2218
  },
  {
    "Date": "2026-02-07",
    "Location": "Murfreesboro, TN",
    "Host": "Dogs Have Amazing Noses, LLC",
    "TrialTypes": "ELT, NW1",
    "EventCount": 2,
    "Latitude": 35.8856,
    "Longitude": -86.3692
  },
  {
    "Date": "2026-02-13",
    "Location": "Havre De Grace, MD",
    "Host": "Firezone GS",
    "TrialTypes": "ELT-P, NW3, L3I, NW2",
    "EventCount": 4,
    "Latitude": 39.5965,
    "Longitude": -76.0805
  },
  {
    "Date": "2026-02-13",
    "Location": "Vista, CA",
    "Host": "Rewarding Rover LLC and Uberdog",
    "TrialTypes": "ELT, L1C, L2C",
    "EventCount": 3,
    "Latitude": 33.1986,
    "Longitude": -117.2168
  },
  {
    "Date": "2026-02-14",
    "Location": "Clarkesville, GA",
    "Host": "Right Choice Dog Training, LLC",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 34.6314,
    "Longitude": -83.515
  },
  {
    "Date": "2026-02-14",
    "Location": "Colesville, MD",
    "Host": "Red Huskies",
    "TrialTypes": "L1C, NW1, ELT-S, ELT",
    "EventCount": 4,
    "Latitude": 39.0684,
    "Longitude": -76.9548
  },
  {
    "Date": "2026-02-14",
    "Location": "Flemington, NJ",
    "Host": "Shamrock Pot Of Gold K9 Scenter",
    "TrialTypes": "ELT, ELT-S, NW1",
    "EventCount": 3,
    "Latitude": 40.5228,
    "Longitude": -74.8236
  },
  {
    "Date": "2026-02-14",
    "Location": "Northridge, CA",
    "Host": "SCENTwork.org",
    "TrialTypes": "NW2, ELT",
    "EventCount": 2,
    "Latitude": 34.2149,
    "Longitude": -118.5591
  },
  {
    "Date": "2026-02-14",
    "Location": "Pottsboro, TX",
    "Host": "All About The Nose",
    "TrialTypes": "NW3, NW2, NW1",
    "EventCount": 3,
    "Latitude": 33.7352,
    "Longitude": -96.6955
  },
  {
    "Date": "2026-02-14",
    "Location": "Strafford, PA",
    "Host": "The Sniffing Hound",
    "TrialTypes": "NW3, ELT, ELT-P",
    "EventCount": 3,
    "Latitude": 40.0155,
    "Longitude": -75.3567
  },
  {
    "Date": "2026-02-15",
    "Location": "Chino, CA",
    "Host": "Agile Paws Dog Sports",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 34.05,
    "Longitude": -117.7067
  },
  {
    "Date": "2026-02-15",
    "Location": "New Rochelle, NY",
    "Host": "For the Love of Dogs NY LLC",
    "TrialTypes": "ELT-P, NW3, ELT",
    "EventCount": 3,
    "Latitude": 40.944,
    "Longitude": -73.8193
  },
  {
    "Date": "2026-02-20",
    "Location": "San Rafael/Novato, CA",
    "Host": "Marin Humane",
    "TrialTypes": "L2C, L1I, NW3",
    "EventCount": 3,
    "Latitude": 38.0865,
    "Longitude": -122.4177
  },
  {
    "Date": "2026-02-21",
    "Location": "Clearwater, FL",
    "Host": "Hoppin' in the Hills",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 27.9585,
    "Longitude": -82.7518
  },
  {
    "Date": "2026-02-21",
    "Location": "Veneta, OR",
    "Host": "Wells Creek Dog Training",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 44.0221,
    "Longitude": -123.3566
  },
  {
    "Date": "2026-02-22",
    "Location": "Benson, AZ",
    "Host": "Patience Unlimited Professional Dog Training",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 31.9566,
    "Longitude": -110.2503
  },
  {
    "Date": "2026-02-24",
    "Location": "Paso Robles, CA",
    "Host": "Gentle Touch Pet Training",
    "TrialTypes": "ELT-S, L3I",
    "EventCount": 2,
    "Latitude": 35.6552,
    "Longitude": -120.7392
  },
  {
    "Date": "2026-02-28",
    "Location": "Danielsville, GA",
    "Host": "Right Choice Dog Training, LLC",
    "TrialTypes": "NW1, L2I, NW2",
    "EventCount": 3,
    "Latitude": 34.1176,
    "Longitude": -83.2223
  },
  {
    "Date": "2026-02-28",
    "Location": "Keystone Heights, FL",
    "Host": "River Poodles Training, LLC",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 29.7597,
    "Longitude": -82.0176
  },
  {
    "Date": "2026-02-28",
    "Location": "Lutherville, MD",
    "Host": "Red Huskies",
    "TrialTypes": "L3C, L1I, NW2",
    "EventCount": 3,
    "Latitude": 39.4479,
    "Longitude": -76.6615
  },
  {
    "Date": "2026-02-28",
    "Location": "Tygh Valley, OR",
    "Host": "Nose Work Detectives, LLC",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 45.212,
    "Longitude": -121.1252
  },
  {
    "Date": "2026-02-28",
    "Location": "Wilson, NC",
    "Host": "Whole Dog Institute, LLC",
    "TrialTypes": "ELT, ELT-S, NW1",
    "EventCount": 3,
    "Latitude": 35.7383,
    "Longitude": -77.8983
  },
  {
    "Date": "2026-03-06",
    "Location": "Chesterfield, VA",
    "Host": "Paws Plus Training, LLC",
    "TrialTypes": "NW3, NW1, NW2, ELT",
    "EventCount": 4,
    "Latitude": 37.4243,
    "Longitude": -77.5998
  },
  {
    "Date": "2026-03-06",
    "Location": "Elgin, IL",
    "Host": "For Your K9, Inc",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 41.9917,
    "Longitude": -88.2985
  },
  {
    "Date": "2026-03-06",
    "Location": "Westlake Village, CA",
    "Host": "JavaK9s, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 34.097,
    "Longitude": -118.8213
  },
  {
    "Date": "2026-03-07",
    "Location": "Cumming, GA",
    "Host": "Georgia Nosework LLC",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 34.2528,
    "Longitude": -84.1416
  },
  {
    "Date": "2026-03-07",
    "Location": "Honesdale, PA",
    "Host": "Your Dog's Place, LLC",
    "TrialTypes": "L3C, ELT, ELT-S, L3I",
    "EventCount": 4,
    "Latitude": 41.5496,
    "Longitude": -75.2116
  },
  {
    "Date": "2026-03-07",
    "Location": "Moriarty, NM",
    "Host": "New Mexico Canine Scent Work, LLC",
    "TrialTypes": "NW3, L1I, NW1",
    "EventCount": 3,
    "Latitude": 35.0061,
    "Longitude": -106.0676
  },
  {
    "Date": "2026-03-07",
    "Location": "Warrensburg, IL",
    "Host": "Kudos for Canines",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 39.9704,
    "Longitude": -89.0939
  },
  {
    "Date": "2026-03-13",
    "Location": "Centreville , MD",
    "Host": "Fair Play Point Labradors",
    "TrialTypes": "SMT, L2C, L3C",
    "EventCount": 3,
    "Latitude": 39.0863,
    "Longitude": -76.1094
  },
  {
    "Date": "2026-03-13",
    "Location": "Colebrook, CT",
    "Host": "For the Love of Dogs NY LLC",
    "TrialTypes": "NW3, SMT",
    "EventCount": 2,
    "Latitude": 42.038,
    "Longitude": -73.1126
  },
  {
    "Date": "2026-03-13",
    "Location": "Stokesdale, NC",
    "Host": "K9 Nose Adventures, LLC",
    "TrialTypes": "ELT, NW3, NW1, NW2",
    "EventCount": 4,
    "Latitude": 36.2061,
    "Longitude": -80.0051
  },
  {
    "Date": "2026-03-14",
    "Location": "Channahon, IL",
    "Host": "4G & TB",
    "TrialTypes": "NW2, NW1",
    "EventCount": 2,
    "Latitude": 41.4201,
    "Longitude": -88.1841
  },
  {
    "Date": "2026-03-14",
    "Location": "Hammond, LA",
    "Host": "Dog Gone Right",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 30.5407,
    "Longitude": -90.4903
  },
  {
    "Date": "2026-03-14",
    "Location": "Kent, WA",
    "Host": "K9 Sniffers",
    "TrialTypes": "ELT, L1V, L1E",
    "EventCount": 3,
    "Latitude": 47.3404,
    "Longitude": -122.2488
  },
  {
    "Date": "2026-03-14",
    "Location": "Phoenix, AZ",
    "Host": "Release Canine, LLC",
    "TrialTypes": "ELT, NW1",
    "EventCount": 2,
    "Latitude": 33.4898,
    "Longitude": -112.0822
  },
  {
    "Date": "2026-03-14",
    "Location": "Santa Paula, CA",
    "Host": "Pink Biscuit K9s",
    "TrialTypes": "NW3, NW1, ELT-S",
    "EventCount": 3,
    "Latitude": 34.3909,
    "Longitude": -119.034
  },
  {
    "Date": "2026-03-16",
    "Location": "Paso Robles, CA",
    "Host": "Central Coast Nosework Club",
    "TrialTypes": "NW3, ELT-S, L3V",
    "EventCount": 3,
    "Latitude": 35.6376,
    "Longitude": -120.6502
  },
  {
    "Date": "2026-03-20",
    "Location": "Shady Hills, FL",
    "Host": "Hoppin' in the Hills",
    "TrialTypes": "L1C, NW1, NW2",
    "EventCount": 3,
    "Latitude": 28.3848,
    "Longitude": -82.5232
  },
  {
    "Date": "2026-03-20",
    "Location": "Street, MD",
    "Host": "Firezone GS",
    "TrialTypes": "ELT-P, NW3, ELT-S, NW2",
    "EventCount": 4,
    "Latitude": 39.6478,
    "Longitude": -76.4274
  },
  {
    "Date": "2026-03-21",
    "Location": "Califon, NJ",
    "Host": "Shamrock Pot Of Gold K9 Scenter",
    "TrialTypes": "NW2, ELT-S, ELT",
    "EventCount": 3,
    "Latitude": 40.7455,
    "Longitude": -74.8109
  },
  {
    "Date": "2026-03-21",
    "Location": "Dittmer, MO",
    "Host": "Happy Dog Concepts LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 38.2995,
    "Longitude": -90.7363
  },
  {
    "Date": "2026-03-21",
    "Location": "East Windsor, CT",
    "Host": "Lucky Dog Events",
    "TrialTypes": "ELT, L2C, NW2",
    "EventCount": 3,
    "Latitude": 41.9196,
    "Longitude": -72.5824
  },
  {
    "Date": "2026-03-21",
    "Location": "Elkridge, MD",
    "Host": "Red Huskies",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 39.2012,
    "Longitude": -76.7271
  },
  {
    "Date": "2026-03-21",
    "Location": "Foxboro, MA",
    "Host": "Bay State Sniffers",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 42.0948,
    "Longitude": -71.2435
  },
  {
    "Date": "2026-03-21",
    "Location": "Lawrenceville, GA",
    "Host": "Right Choice Dog Training LLC",
    "TrialTypes": "ELT-P, ELT",
    "EventCount": 2,
    "Latitude": 34.0006,
    "Longitude": -83.9473
  },
  {
    "Date": "2026-03-21",
    "Location": "Oakville, WA",
    "Host": "About Face K9 Academy & Let's Talk Dogs, LLC",
    "TrialTypes": "NW2, ELT",
    "EventCount": 2,
    "Latitude": 46.8085,
    "Longitude": -123.2268
  },
  {
    "Date": "2026-03-21",
    "Location": "Redwood City, CA",
    "Host": "B.L. McMutts LLC",
    "TrialTypes": "ELT-S, L1I, L3C",
    "EventCount": 3,
    "Latitude": 37.4433,
    "Longitude": -122.1889
  },
  {
    "Date": "2026-03-21",
    "Location": "Salem Lakes, WI",
    "Host": "Loving Paws Dog Training LLC",
    "TrialTypes": "NW2, L2I, ELT-S",
    "EventCount": 3,
    "Latitude": 42.5001,
    "Longitude": -88.1389
  },
  {
    "Date": "2026-03-21",
    "Location": "Winterset, IA",
    "Host": "KBP Dog Training",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 41.2973,
    "Longitude": -94.0303
  },
  {
    "Date": "2026-03-23",
    "Location": "Riverside, CA",
    "Host": "Linda Buchanan",
    "TrialTypes": "NW2, NW3",
    "EventCount": 2,
    "Latitude": 33.9915,
    "Longitude": -117.3403
  },
  {
    "Date": "2026-03-27",
    "Location": "Grand Junction, CO",
    "Host": "Mountain Dogs, LLC",
    "TrialTypes": "ELT-P, ELT-S, NW1, NW2",
    "EventCount": 4,
    "Latitude": 39.0315,
    "Longitude": -108.5842
  },
  {
    "Date": "2026-03-27",
    "Location": "Kennett Square, PA",
    "Host": "The Sniffing Hound",
    "TrialTypes": "NW3, ELT, ELT-S, L3I",
    "EventCount": 4,
    "Latitude": 39.8669,
    "Longitude": -75.7558
  },
  {
    "Date": "2026-03-27",
    "Location": "Salem, OR",
    "Host": "Just Nose Work & Doglandia LLC",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 44.9405,
    "Longitude": -123.0198
  },
  {
    "Date": "2026-03-27",
    "Location": "Watertown, TN",
    "Host": "Dogs Have Amazing Noses, LLC",
    "TrialTypes": "ELT, NW2",
    "EventCount": 2,
    "Latitude": 36.0516,
    "Longitude": -86.1174
  },
  {
    "Date": "2026-03-28",
    "Location": "Batavia, OH",
    "Host": "Clermont County Dog Training Club",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 39.0506,
    "Longitude": -84.2207
  },
  {
    "Date": "2026-03-28",
    "Location": "Colorado Springs, CO",
    "Host": "Beyond Elevation K9",
    "TrialTypes": "NW2, NW3",
    "EventCount": 2,
    "Latitude": 38.8208,
    "Longitude": -104.8075
  },
  {
    "Date": "2026-03-28",
    "Location": "Forks, WA",
    "Host": "Sea Change Canine LLC",
    "TrialTypes": "NW2, NW3",
    "EventCount": 2,
    "Latitude": 47.9791,
    "Longitude": -124.3975
  },
  {
    "Date": "2026-03-29",
    "Location": "Canton, GA",
    "Host": "Run Spot Jump Dog Training",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 34.2654,
    "Longitude": -84.4481
  },
  {
    "Date": "2026-03-30",
    "Location": "Watsonville, CA",
    "Host": "CalCoastal Dog Owners Group",
    "TrialTypes": "NW2, NW1",
    "EventCount": 2,
    "Latitude": 36.902,
    "Longitude": -121.7615
  },
  {
    "Date": "2026-04-03",
    "Location": "Eagan, MN",
    "Host": "St. Paul Dog Training Center",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 44.8233,
    "Longitude": -93.1403
  },
  {
    "Date": "2026-04-03",
    "Location": "Rochester, NY",
    "Host": "2 Psyched 4 dogs",
    "TrialTypes": "NW3, L1I, L1C",
    "EventCount": 3,
    "Latitude": 43.1798,
    "Longitude": -77.6378
  },
  {
    "Date": "2026-04-03",
    "Location": "Warwick, NY",
    "Host": "For the Love of Dogs NY LLC",
    "TrialTypes": "ELT, NW3, NW1",
    "EventCount": 3,
    "Latitude": 41.2137,
    "Longitude": -74.3547
  },
  {
    "Date": "2026-04-04",
    "Location": "Blaine, WA",
    "Host": "The Nosework Magic",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 49.0028,
    "Longitude": -122.7585
  },
  {
    "Date": "2026-04-04",
    "Location": "Burnet, TX",
    "Host": "Scent Work Across Texas",
    "TrialTypes": "L1C, NW2, ELT",
    "EventCount": 3,
    "Latitude": 30.8188,
    "Longitude": -98.1292
  },
  {
    "Date": "2026-04-04",
    "Location": "Stayton, OR",
    "Host": "Canine Discovery Corps",
    "TrialTypes": "L2E, L1V, L1E, L2C",
    "EventCount": 4,
    "Latitude": 44.7744,
    "Longitude": -122.7881
  },
  {
    "Date": "2026-04-06",
    "Location": "Sacramento, CA",
    "Host": "Two Nosey Girls",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 38.5985,
    "Longitude": -121.4732
  },
  {
    "Date": "2026-04-08",
    "Location": "Olympia, WA",
    "Host": "Rachelle Bailey-Austin & Dorothy Turley",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 47.0153,
    "Longitude": -122.9242
  },
  {
    "Date": "2026-04-10",
    "Location": "Rapid City, SD",
    "Host": "Two Paws Up Dog Training, LLC",
    "TrialTypes": "ELT, NW3, NW1, NW2",
    "EventCount": 4,
    "Latitude": 44.0453,
    "Longitude": -103.2701
  },
  {
    "Date": "2026-04-10",
    "Location": "Somis, CA",
    "Host": "Pink Biscuit K9s",
    "TrialTypes": "NW3, ELT-P, L2C, L2I",
    "EventCount": 4,
    "Latitude": 34.2195,
    "Longitude": -119.0134
  },
  {
    "Date": "2026-04-11",
    "Location": "Bel Air, MD",
    "Host": "Firezone GS",
    "TrialTypes": "ELT-P, ELT-S, NW2",
    "EventCount": 3,
    "Latitude": 39.4864,
    "Longitude": -76.3832
  },
  {
    "Date": "2026-04-11",
    "Location": "Blue Ridge , VA",
    "Host": "Canny K9 Companions, LLC",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 37.4061,
    "Longitude": -79.8392
  },
  {
    "Date": "2026-04-11",
    "Location": "Clinton, WI",
    "Host": "George Carpenter",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 42.5829,
    "Longitude": -88.8457
  },
  {
    "Date": "2026-04-11",
    "Location": "Durham, NC",
    "Host": "Dog Fun Forever, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 36.0033,
    "Longitude": -78.8766
  },
  {
    "Date": "2026-04-11",
    "Location": "Genoa, IL",
    "Host": "Common Scents K9",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 42.0724,
    "Longitude": -88.6448
  },
  {
    "Date": "2026-04-11",
    "Location": "Limerick, PA",
    "Host": "Sniff Sniff Hooray",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 40.1918,
    "Longitude": -75.5458
  },
  {
    "Date": "2026-04-11",
    "Location": "Rocklin, CA",
    "Host": "Sierra Sniffing Canines",
    "TrialTypes": "NW2, L1E, L2E",
    "EventCount": 3,
    "Latitude": 38.839,
    "Longitude": -121.2088
  },
  {
    "Date": "2026-04-13",
    "Location": "Ellicott City, MD",
    "Host": "Red Huskies",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 39.2684,
    "Longitude": -76.8761
  },
  {
    "Date": "2026-04-17",
    "Location": "Amity, OR",
    "Host": "Doglandia, LLC",
    "TrialTypes": "NW1, NW2, NW3",
    "EventCount": 3,
    "Latitude": 45.1107,
    "Longitude": -123.231
  },
  {
    "Date": "2026-04-17",
    "Location": "Garrison, NY",
    "Host": "Top Notch Dogs, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 41.4012,
    "Longitude": -73.934
  },
  {
    "Date": "2026-04-17",
    "Location": "Upland, CA",
    "Host": "Agile Paws Dog Sports",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 34.0777,
    "Longitude": -117.6967
  },
  {
    "Date": "2026-04-18",
    "Location": "Auburn, WA",
    "Host": "Northwest K9 Sniffers",
    "TrialTypes": "L1V, NW1, L1I, NW2",
    "EventCount": 4,
    "Latitude": 47.2641,
    "Longitude": -122.2396
  },
  {
    "Date": "2026-04-18",
    "Location": "Keystone Heights, FL",
    "Host": "River Poodles Training, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 29.7726,
    "Longitude": -82.007
  },
  {
    "Date": "2026-04-18",
    "Location": "Laramie, WY",
    "Host": "Mountain Dogs LLC",
    "TrialTypes": "SMT, ELT",
    "EventCount": 2,
    "Latitude": 41.2775,
    "Longitude": -105.6117
  },
  {
    "Date": "2026-04-18",
    "Location": "Pomfret, MD",
    "Host": "Nose-It-All, LLC",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 38.5957,
    "Longitude": -77.0481
  },
  {
    "Date": "2026-04-18",
    "Location": "Toledo, OH",
    "Host": "Robin Ford Dog Training LLC",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 41.6504,
    "Longitude": -83.5722
  },
  {
    "Date": "2026-04-18",
    "Location": "Winterset, IA",
    "Host": "KBP Dog Training",
    "TrialTypes": "NW2, NW1",
    "EventCount": 2,
    "Latitude": 41.3347,
    "Longitude": -94.0515
  },
  {
    "Date": "2026-04-18",
    "Location": "Woodstock, IL",
    "Host": "Northwest Obedience Club Inc",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 42.3324,
    "Longitude": -88.4045
  },
  {
    "Date": "2026-04-19",
    "Location": "Glenwood, NY",
    "Host": "Do Over Dog Training",
    "TrialTypes": "L2I, L1E, L2C, ELT-S",
    "EventCount": 4,
    "Latitude": 42.5788,
    "Longitude": -78.6357
  },
  {
    "Date": "2026-04-20",
    "Location": "Amherst, NH",
    "Host": "Lucky Dog Events",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 42.8429,
    "Longitude": -71.628
  },
  {
    "Date": "2026-04-21",
    "Location": "Stony Point , NY",
    "Host": "Saints2Source, LLC",
    "TrialTypes": "NW3, ELT, ELT-S, NW2",
    "EventCount": 4,
    "Latitude": 41.2044,
    "Longitude": -74.0346
  },
  {
    "Date": "2026-04-24",
    "Location": "Asheboro, NC",
    "Host": "K9 Nose Adventures, LLC",
    "TrialTypes": "ELT, NW3, ELT-S, NW2",
    "EventCount": 4,
    "Latitude": 35.6639,
    "Longitude": -79.8286
  },
  {
    "Date": "2026-04-24",
    "Location": "Easton, MD",
    "Host": "Red Huskies",
    "TrialTypes": "L3C, NW2, L3V, NW1",
    "EventCount": 4,
    "Latitude": 38.7908,
    "Longitude": -76.1159
  },
  {
    "Date": "2026-04-25",
    "Location": "Canfield, OH",
    "Host": "Nosework Addicts, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 41.0168,
    "Longitude": -80.7561
  },
  {
    "Date": "2026-04-25",
    "Location": "Columbus, MT",
    "Host": "Canine Connection",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 45.6678,
    "Longitude": -109.2179
  },
  {
    "Date": "2026-04-25",
    "Location": "Ellicottville, NY",
    "Host": "Do Over Dog Training",
    "TrialTypes": "ELT, ELT-S, NW1",
    "EventCount": 3,
    "Latitude": 42.2429,
    "Longitude": -78.7167
  },
  {
    "Date": "2026-04-25",
    "Location": "Havre de Grace, MD",
    "Host": "Chesapeake Search Dogs",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 39.5967,
    "Longitude": -76.0693
  },
  {
    "Date": "2026-04-25",
    "Location": "Portland, OR",
    "Host": "Trust Your Dog K9 Events",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 45.4821,
    "Longitude": -122.6814
  },
  {
    "Date": "2026-04-25",
    "Location": "Sharon, MA",
    "Host": "Bay State Sniffers",
    "TrialTypes": "NW3, ELT-P",
    "EventCount": 2,
    "Latitude": 42.1508,
    "Longitude": -71.1921
  },
  {
    "Date": "2026-04-25",
    "Location": "Suring, WI",
    "Host": "Clever Sniffers, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 44.9733,
    "Longitude": -88.3911
  },
  {
    "Date": "2026-04-25",
    "Location": "Traverse City, MI",
    "Host": "Every Dog Nosework",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 44.711,
    "Longitude": -85.6155
  },
  {
    "Date": "2026-05-01",
    "Location": "Faribault, MN",
    "Host": "St. Paul Dog Training Club",
    "TrialTypes": "NW3, NW1, L2E, L3C",
    "EventCount": 4,
    "Latitude": 43.7129,
    "Longitude": -93.9866
  },
  {
    "Date": "2026-05-01",
    "Location": "Nyack, NY",
    "Host": "Waggin' Work",
    "TrialTypes": "NW2, NW1, ELT-P, ELT-S",
    "EventCount": 4,
    "Latitude": 41.0905,
    "Longitude": -73.8749
  },
  {
    "Date": "2026-05-01",
    "Location": "Turlock, CA",
    "Host": "Two Nosey Girls",
    "TrialTypes": "L1I, L2I, ELT-S, L3I",
    "EventCount": 4,
    "Latitude": 37.4787,
    "Longitude": -120.806
  },
  {
    "Date": "2026-05-02",
    "Location": "Alexis, IL",
    "Host": "Kudos for Canines, LLC",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 41.0213,
    "Longitude": -90.5219
  },
  {
    "Date": "2026-05-02",
    "Location": "Ashby , MA",
    "Host": "Dogs! Carolyn Barney",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 42.6972,
    "Longitude": -71.8615
  },
  {
    "Date": "2026-05-02",
    "Location": "Hillsdale, NY",
    "Host": "For the Love of Dogs NY LLC",
    "TrialTypes": "ELT-P, ELT",
    "EventCount": 2,
    "Latitude": 42.1708,
    "Longitude": -73.5076
  },
  {
    "Date": "2026-05-02",
    "Location": "Santa Paula, CA",
    "Host": "Pink Biscuit K9s",
    "TrialTypes": "NW3, ELT-S, NW2",
    "EventCount": 3,
    "Latitude": 34.337,
    "Longitude": -119.0719
  },
  {
    "Date": "2026-05-02",
    "Location": "Sedona, AZ",
    "Host": "Successful Sniffer",
    "TrialTypes": "L1I, NW2, NW3",
    "EventCount": 3,
    "Latitude": 34.8429,
    "Longitude": -111.8027
  },
  {
    "Date": "2026-05-02",
    "Location": "Vancouver, WA",
    "Host": "Sniffketeers",
    "TrialTypes": "ELT-S",
    "EventCount": 1,
    "Latitude": 45.6055,
    "Longitude": -122.6707
  },
  {
    "Date": "2026-05-07",
    "Location": "Lancaster, PA",
    "Host": "Red Huskies Nose Work, LLC",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 40.0242,
    "Longitude": -76.261
  },
  {
    "Date": "2026-05-08",
    "Location": "Grand Island, NY",
    "Host": "Do Over Dog Training",
    "TrialTypes": "SMT, ELT",
    "EventCount": 2,
    "Latitude": 43.0091,
    "Longitude": -78.9252
  },
  {
    "Date": "2026-05-08",
    "Location": "Jarrettsville, MD",
    "Host": "Firezone GS",
    "TrialTypes": "ELT, NW3, ELT-S, L2I",
    "EventCount": 4,
    "Latitude": 39.5854,
    "Longitude": -76.4726
  },
  {
    "Date": "2026-05-08",
    "Location": "Warwick, NY",
    "Host": "Top Notch Dogs, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 41.2767,
    "Longitude": -74.3175
  },
  {
    "Date": "2026-05-08",
    "Location": "Wrightwood, CA",
    "Host": "JavaK9s, LLC",
    "TrialTypes": "NW3, L1C, L1I",
    "EventCount": 3,
    "Latitude": 34.3345,
    "Longitude": -117.5935
  },
  {
    "Date": "2026-05-09",
    "Location": "Brighton, MI",
    "Host": "Every Dog Nosework",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 42.5017,
    "Longitude": -83.7531
  },
  {
    "Date": "2026-05-09",
    "Location": "Charlton, MA",
    "Host": "HeavenScent Sniffers",
    "TrialTypes": "ELT-P, L2C, NW1",
    "EventCount": 3,
    "Latitude": 42.1718,
    "Longitude": -71.9903
  },
  {
    "Date": "2026-05-09",
    "Location": "Egg Harbor City, NJ",
    "Host": "Rotts-n-Notts Nosework LLC",
    "TrialTypes": "L3I, NW1, NW3",
    "EventCount": 3,
    "Latitude": 39.5533,
    "Longitude": -74.6708
  },
  {
    "Date": "2026-05-09",
    "Location": "Livingston, MT",
    "Host": "Trails and Tails Dog School",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 45.6836,
    "Longitude": -110.5422
  },
  {
    "Date": "2026-05-09",
    "Location": "Malvern, IA",
    "Host": "Two Tails Unlimited",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 41.0371,
    "Longitude": -95.6288
  },
  {
    "Date": "2026-05-09",
    "Location": "Poland Springs, ME",
    "Host": "Bare Bones Nosework, LLC",
    "TrialTypes": "L1I, L2I",
    "EventCount": 2,
    "Latitude": 43.982,
    "Longitude": -70.3548
  },
  {
    "Date": "2026-05-09",
    "Location": "Union Grove, WI",
    "Host": "Loving Paws, LLC",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 42.6957,
    "Longitude": -88.0788
  },
  {
    "Date": "2026-05-14",
    "Location": "Frederick, MD",
    "Host": "Red Huskies",
    "TrialTypes": "ELT-P, ELT-S, L2I, L2E, L1E",
    "EventCount": 5,
    "Latitude": 39.3882,
    "Longitude": -77.4482
  },
  {
    "Date": "2026-05-15",
    "Location": "Cannon Falls, MN",
    "Host": "Saint Paul Dog Training Club",
    "TrialTypes": "ELT, ELT-S, L3E, L1I, L1E",
    "EventCount": 5,
    "Latitude": 44.4814,
    "Longitude": -92.9383
  },
  {
    "Date": "2026-05-15",
    "Location": "Phoenix, MD",
    "Host": "Oriole Dog Training Club",
    "TrialTypes": "NW3, L1I, NW1",
    "EventCount": 3,
    "Latitude": 39.4705,
    "Longitude": -76.6139
  },
  {
    "Date": "2026-05-15",
    "Location": "Watsonville, CA",
    "Host": "CalCoastal Dog Owners Group",
    "TrialTypes": "ELT-S, L3C, L1C",
    "EventCount": 3,
    "Latitude": 36.8757,
    "Longitude": -121.7079
  },
  {
    "Date": "2026-05-16",
    "Location": "Alexander, NY",
    "Host": "Do Over Dog Training",
    "TrialTypes": "L1V, L2V, L3C, L3I",
    "EventCount": 4,
    "Latitude": 42.8899,
    "Longitude": -78.2104
  },
  {
    "Date": "2026-05-16",
    "Location": "Bellingham, WA",
    "Host": "The Nosework Magic",
    "TrialTypes": "NW2, NW1",
    "EventCount": 2,
    "Latitude": 48.7191,
    "Longitude": -122.4666
  },
  {
    "Date": "2026-05-16",
    "Location": "Burien, WA",
    "Host": "Northwest K9 Sniffers",
    "TrialTypes": "NW3, L2V, L2I",
    "EventCount": 3,
    "Latitude": 47.515,
    "Longitude": -122.3164
  },
  {
    "Date": "2026-05-16",
    "Location": "Durham, NC",
    "Host": "Whole Dog Institute, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 35.9901,
    "Longitude": -78.9345
  },
  {
    "Date": "2026-05-16",
    "Location": "Kittanning, PA",
    "Host": "Steel City Nosework, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 40.7902,
    "Longitude": -79.5493
  },
  {
    "Date": "2026-05-16",
    "Location": "Monticello , NY",
    "Host": "Saints2Source, LLC",
    "TrialTypes": "NW3, ELT, ELT-S",
    "EventCount": 3,
    "Latitude": 41.6383,
    "Longitude": -74.6867
  },
  {
    "Date": "2026-05-16",
    "Location": "Peru, MA",
    "Host": "Lucky Dog Events",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 42.4731,
    "Longitude": -73.051
  },
  {
    "Date": "2026-05-16",
    "Location": "Sandwich, IL",
    "Host": "For Your K9, Inc.",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 41.6251,
    "Longitude": -88.6196
  },
  {
    "Date": "2026-05-22",
    "Location": "Anchorage, AK",
    "Host": "Alaska Dog Sports",
    "TrialTypes": "NW1, NW2, NW3",
    "EventCount": 3,
    "Latitude": 61.2651,
    "Longitude": -149.9
  },
  {
    "Date": "2026-05-22",
    "Location": "Montrose, CO",
    "Host": "Mountain Dogs LLC",
    "TrialTypes": "ELT, NW3, NW2, NW1",
    "EventCount": 4,
    "Latitude": 38.518,
    "Longitude": -107.853
  },
  {
    "Date": "2026-05-22",
    "Location": "San Luis Obispo, CA",
    "Host": "Gentle Touch Pet Training",
    "TrialTypes": "NW1, L1I, NW2",
    "EventCount": 3,
    "Latitude": 35.3822,
    "Longitude": -120.4187
  },
  {
    "Date": "2026-05-23",
    "Location": "Alpharetta, GA",
    "Host": "Georgia Nosework, LLC",
    "TrialTypes": "NW3, ELT-S, NW2, ELT",
    "EventCount": 4,
    "Latitude": 34.082,
    "Longitude": -84.2921
  },
  {
    "Date": "2026-05-23",
    "Location": "Columbus, MT",
    "Host": "Canine Connection",
    "TrialTypes": "L1I, L2I, ELT-S, NW2",
    "EventCount": 4,
    "Latitude": 45.6285,
    "Longitude": -109.2028
  },
  {
    "Date": "2026-05-23",
    "Location": "Emmitsburg, MD",
    "Host": "Red Huskies",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 39.6863,
    "Longitude": -77.3099
  },
  {
    "Date": "2026-05-23",
    "Location": "Lancaster, PA",
    "Host": "Nose-It-All, LLC",
    "TrialTypes": "ELT, ELT-P, NW3",
    "EventCount": 3,
    "Latitude": 40.0621,
    "Longitude": -76.314
  },
  {
    "Date": "2026-05-23",
    "Location": "Murfreesboro, TN",
    "Host": "Dogs Have Amazing Noses, LLC",
    "TrialTypes": "ELT, NW1",
    "EventCount": 2,
    "Latitude": 35.7978,
    "Longitude": -86.3849
  },
  {
    "Date": "2026-05-23",
    "Location": "North Manchester, IN",
    "Host": "2 Nose You Is 2 Loves You",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 40.9649,
    "Longitude": -85.7916
  },
  {
    "Date": "2026-05-23",
    "Location": "Rainier, WA",
    "Host": "Let's Talk Dogs, LLC & About Face K9 Academy",
    "TrialTypes": "ELT-S, L1C, NW2",
    "EventCount": 3,
    "Latitude": 46.8703,
    "Longitude": -122.7381
  },
  {
    "Date": "2026-05-23",
    "Location": "Red Feather Lakes, CO",
    "Host": "Beyond Elevation K9 Training LLC",
    "TrialTypes": "NW1, NW2, NW3",
    "EventCount": 3,
    "Latitude": 40.7761,
    "Longitude": -105.5377
  },
  {
    "Date": "2026-05-23",
    "Location": "Rockaway, NJ",
    "Host": "Shamrock Pot Of Gold K9 Scenter",
    "TrialTypes": "NW3, L3E, NW2, ELT-S",
    "EventCount": 4,
    "Latitude": 40.8862,
    "Longitude": -74.4935
  },
  {
    "Date": "2026-05-23",
    "Location": "Sandy, OR",
    "Host": "Trust Your Dog K9 Events",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 45.3776,
    "Longitude": -122.2571
  },
  {
    "Date": "2026-05-25",
    "Location": "Manchester, NH",
    "Host": "For the Love of Dogs NY, LLC",
    "TrialTypes": "NW2, ELT-P, ELT",
    "EventCount": 3,
    "Latitude": 42.9474,
    "Longitude": -71.4213
  },
  {
    "Date": "2026-05-28",
    "Location": "Concord, CA",
    "Host": "The Bay Team",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 37.9354,
    "Longitude": -122.0008
  },
  {
    "Date": "2026-05-29",
    "Location": "Grand Junction, CO",
    "Host": "Mountain Dogs LLC",
    "TrialTypes": "NW3, L1V, L1E",
    "EventCount": 3,
    "Latitude": 39.0457,
    "Longitude": -108.5221
  },
  {
    "Date": "2026-05-30",
    "Location": "Amherst, NY",
    "Host": "Do Over Dog Training",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 43.028,
    "Longitude": -78.7989
  },
  {
    "Date": "2026-05-30",
    "Location": "Eden Prairie, MN",
    "Host": "The K9 Nose",
    "TrialTypes": "NW2",
    "EventCount": 1,
    "Latitude": 44.8382,
    "Longitude": -93.4929
  },
  {
    "Date": "2026-05-30",
    "Location": "Spencer, MA",
    "Host": "Lucky Dog Events",
    "TrialTypes": "ELT, L1E, L2C",
    "EventCount": 3,
    "Latitude": 42.2313,
    "Longitude": -71.9781
  },
  {
    "Date": "2026-06-06",
    "Location": "Dunmore, PA",
    "Host": "Your Dog's Place, LLC",
    "TrialTypes": "ELT, ELT-S, L1C",
    "EventCount": 3,
    "Latitude": 41.3758,
    "Longitude": -75.6382
  },
  {
    "Date": "2026-06-06",
    "Location": "Enterprise, OR",
    "Host": "Country K9 Nosework, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 45.4648,
    "Longitude": -117.3224
  },
  {
    "Date": "2026-06-06",
    "Location": "Manheim, PA",
    "Host": "Nose-It-All, LLC",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 40.1687,
    "Longitude": -76.4202
  },
  {
    "Date": "2026-06-06",
    "Location": "Meadowbrook, PA",
    "Host": "Patricia Grassey",
    "TrialTypes": "NW3, NW1, NW2, ELT",
    "EventCount": 4,
    "Latitude": 40.0715,
    "Longitude": -75.0894
  },
  {
    "Date": "2026-06-06",
    "Location": "Rochester, NH",
    "Host": "Pawsitive Image",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 43.2633,
    "Longitude": -70.975
  },
  {
    "Date": "2026-06-06",
    "Location": "Shawnee, OK",
    "Host": "The Doggie Spot, LLC",
    "TrialTypes": "NW3, NW2, NW1",
    "EventCount": 3,
    "Latitude": 35.3016,
    "Longitude": -96.9647
  },
  {
    "Date": "2026-06-06",
    "Location": "Slippery Rock, PA",
    "Host": "Nosework Addicts, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 41.0215,
    "Longitude": -80.084
  },
  {
    "Date": "2026-06-06",
    "Location": "Sparks Glencoe, MD",
    "Host": "Chesapeake Search Dogs",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 39.5296,
    "Longitude": -76.692
  },
  {
    "Date": "2026-06-06",
    "Location": "Wrightstown, WI",
    "Host": "NEWk9Scent Work LLC",
    "TrialTypes": "NW3, NW1, L1I",
    "EventCount": 3,
    "Latitude": 44.3608,
    "Longitude": -88.183
  },
  {
    "Date": "2026-06-12",
    "Location": "Gunnison, CO",
    "Host": "Mountain Dogs LLC",
    "TrialTypes": "ELT, ELT-S, ELT-P",
    "EventCount": 3,
    "Latitude": 38.6435,
    "Longitude": -107.0559
  },
  {
    "Date": "2026-06-12",
    "Location": "New Hope, PA",
    "Host": "Patricia Grassey",
    "TrialTypes": "ELT, ELT-P, ELT-S, L3E",
    "EventCount": 4,
    "Latitude": 40.3982,
    "Longitude": -74.9812
  },
  {
    "Date": "2026-06-13",
    "Location": "East Helena, MT",
    "Host": "Nose Work Breakfast Club",
    "TrialTypes": "NW1, NW2, NW3",
    "EventCount": 3,
    "Latitude": 46.5503,
    "Longitude": -111.8679
  },
  {
    "Date": "2026-06-13",
    "Location": "Ithaca, NY",
    "Host": "The Brainy Canine",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 42.4037,
    "Longitude": -76.5832
  },
  {
    "Date": "2026-06-13",
    "Location": "Kenosha, WI",
    "Host": "Loving Paws Dog Training LLC",
    "TrialTypes": "ELT-S, L1I, NW1",
    "EventCount": 3,
    "Latitude": 42.5563,
    "Longitude": -87.8344
  },
  {
    "Date": "2026-06-13",
    "Location": "Linden, MI",
    "Host": "Every Dog Nosework",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 42.8639,
    "Longitude": -83.819
  },
  {
    "Date": "2026-06-13",
    "Location": "Nazareth/Windgap, PA",
    "Host": "Paws n' Sniff",
    "TrialTypes": "NW2, ELT, NW1, ELT-S, NW3",
    "EventCount": 5,
    "Latitude": 40.7811,
    "Longitude": -75.2982
  },
  {
    "Date": "2026-06-13",
    "Location": "Palmyra, VA",
    "Host": "Your Dog Knows LLC",
    "TrialTypes": "L1I, L2I, L1C, L2C",
    "EventCount": 4,
    "Latitude": 37.8231,
    "Longitude": -78.2869
  },
  {
    "Date": "2026-06-18",
    "Location": "Westminster, MD",
    "Host": "Firezone GS",
    "TrialTypes": "ELT, NW3, ELT-S, NW2",
    "EventCount": 4,
    "Latitude": 39.5343,
    "Longitude": -77.034
  },
  {
    "Date": "2026-06-19",
    "Location": "Bayfield, CO",
    "Host": "Mountain Dogs, LLC",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 37.2217,
    "Longitude": -107.6261
  },
  {
    "Date": "2026-06-19",
    "Location": "Jordan, MN",
    "Host": "St. Paul Dog Training Club",
    "TrialTypes": "ELT, ELT-P, L1V, L2V",
    "EventCount": 4,
    "Latitude": 44.7137,
    "Longitude": -93.5857
  },
  {
    "Date": "2026-06-19",
    "Location": "New Rochelle, NY",
    "Host": "For the Love of Dogs NY, LLC",
    "TrialTypes": "ELT, NW3, ELT-P",
    "EventCount": 3,
    "Latitude": 40.9333,
    "Longitude": -73.7589
  },
  {
    "Date": "2026-06-20",
    "Location": "Cumming, GA",
    "Host": "Georgia Nosework, LLC",
    "TrialTypes": "L2I, L2C, L3C, L1I",
    "EventCount": 4,
    "Latitude": 34.1861,
    "Longitude": -84.1663
  },
  {
    "Date": "2026-06-20",
    "Location": "Danvers, MA",
    "Host": "Everydog, LLC",
    "TrialTypes": "NW1, NW2, NW3",
    "EventCount": 3,
    "Latitude": 42.5545,
    "Longitude": -70.9609
  },
  {
    "Date": "2026-06-20",
    "Location": "Florissant, MO",
    "Host": "Happy Dog Concepts",
    "TrialTypes": "NW1, NW2, NW3",
    "EventCount": 3,
    "Latitude": 38.7652,
    "Longitude": -90.3679
  },
  {
    "Date": "2026-06-20",
    "Location": "Pittsburgh, PA",
    "Host": "Nosework Addicts, LLC",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 40.4404,
    "Longitude": -79.9783
  },
  {
    "Date": "2026-06-20",
    "Location": "Terryville, CT",
    "Host": "Willoughby Training",
    "TrialTypes": "NW3, NW1",
    "EventCount": 2,
    "Latitude": 41.6458,
    "Longitude": -73.0319
  },
  {
    "Date": "2026-06-20",
    "Location": "White Salmon, WA",
    "Host": "Sharon Smith",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 45.6778,
    "Longitude": -121.5197
  },
  {
    "Date": "2026-06-26",
    "Location": "Delran, NJ",
    "Host": "K9 InScentives",
    "TrialTypes": "NW1, ELT",
    "EventCount": 2,
    "Latitude": 40.0605,
    "Longitude": -74.9085
  },
  {
    "Date": "2026-06-26",
    "Location": "Loveland, CO",
    "Host": "NoCo Unleashed LLC",
    "TrialTypes": "ELT-S, L2C, L2I, L1C",
    "EventCount": 4,
    "Latitude": 40.3709,
    "Longitude": -105.0764
  },
  {
    "Date": "2026-06-26",
    "Location": "Red Lodge, MT",
    "Host": "Canine Connection",
    "TrialTypes": "ELT, NW3, NW1, NW2",
    "EventCount": 4,
    "Latitude": 45.1428,
    "Longitude": -109.2914
  },
  {
    "Date": "2026-06-27",
    "Location": "Burlington, WI",
    "Host": "Loving Paws Dog Training LLC",
    "TrialTypes": "NW2, ELT",
    "EventCount": 2,
    "Latitude": 42.6312,
    "Longitude": -88.303
  },
  {
    "Date": "2026-06-27",
    "Location": "De Pere, WI",
    "Host": "NEWk9Scent Work LLC",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 44.4919,
    "Longitude": -88.0508
  },
  {
    "Date": "2026-06-27",
    "Location": "Deming, WA",
    "Host": "The Nosework Magic",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 48.7956,
    "Longitude": -122.2726
  },
  {
    "Date": "2026-06-27",
    "Location": "Inver Grove Heights, MN",
    "Host": "Outside the Box Dog Training, LLC",
    "TrialTypes": "NW3, L2C, L2I",
    "EventCount": 3,
    "Latitude": 44.7986,
    "Longitude": -93.0788
  },
  {
    "Date": "2026-06-27",
    "Location": "Lockport, IL",
    "Host": "4G & TB",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 41.6385,
    "Longitude": -88.0455
  },
  {
    "Date": "2026-06-27",
    "Location": "New Wilmington, PA",
    "Host": "Steel City Nosework, LLC",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 41.1082,
    "Longitude": -80.2904
  },
  {
    "Date": "2026-06-27",
    "Location": "Salem, OR",
    "Host": "Doglandia, LLC",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 44.9553,
    "Longitude": -123.0303
  },
  {
    "Date": "2026-06-27",
    "Location": "Somers, CT",
    "Host": "HeavenScent Sniffers",
    "TrialTypes": "NW2, L3C, ELT-S",
    "EventCount": 3,
    "Latitude": 42.0278,
    "Longitude": -72.4145
  },
  {
    "Date": "2026-06-30",
    "Location": "Delran, NJ",
    "Host": "Ev-ry Earthdog, LLC",
    "TrialTypes": "NW3, NW1, NW2, ELT-P",
    "EventCount": 4,
    "Latitude": 40.0423,
    "Longitude": -74.9528
  },
  {
    "Date": "2026-07-03",
    "Location": "Huntington, MA",
    "Host": "Shamrock Pot Of Gold K9 Scenter",
    "TrialTypes": "NW3, ELT, ELT-S, L2I",
    "EventCount": 4,
    "Latitude": 42.2135,
    "Longitude": -72.8445
  },
  {
    "Date": "2026-07-06",
    "Location": "Montgomery, NY",
    "Host": "For the Love of Dogs NY LLC",
    "TrialTypes": "NW2, ELT",
    "EventCount": 2,
    "Latitude": 42.8509,
    "Longitude": -74.3608
  },
  {
    "Date": "2026-07-10",
    "Location": "Leadville, CO",
    "Host": "Mountain Dogs LLC",
    "TrialTypes": "NW3, NW2, NW1, L2I, L2C",
    "EventCount": 5,
    "Latitude": 39.2491,
    "Longitude": -106.2682
  },
  {
    "Date": "2026-07-10",
    "Location": "Sparks Glencoe, MD",
    "Host": "Firezone GS",
    "TrialTypes": "ELT-P, ELT-S, L3I, ELT",
    "EventCount": 4,
    "Latitude": 39.4984,
    "Longitude": -76.6587
  },
  {
    "Date": "2026-07-11",
    "Location": "Livonia, MI",
    "Host": "Every Dog Nosework",
    "TrialTypes": "ELT-P, NW1",
    "EventCount": 2,
    "Latitude": 42.4002,
    "Longitude": -83.3957
  },
  {
    "Date": "2026-07-13",
    "Location": "Derry, NH",
    "Host": "Lucky Dog Events",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 42.8309,
    "Longitude": -71.343
  },
  {
    "Date": "2026-07-13",
    "Location": "Florham Park, NJ",
    "Host": "For the Love of Dogs NY LLC",
    "TrialTypes": "L1C, L1I, ELT",
    "EventCount": 3,
    "Latitude": 40.8182,
    "Longitude": -74.406
  },
  {
    "Date": "2026-07-17",
    "Location": "Encinitas, CA",
    "Host": "Rewarding Rover LLC & UberDog/Jessica Koester",
    "TrialTypes": "NW2, NW1, ELT-S",
    "EventCount": 3,
    "Latitude": 33.0799,
    "Longitude": -117.2552
  },
  {
    "Date": "2026-07-17",
    "Location": "Leadville, CO",
    "Host": "Mountain Dogs LLC",
    "TrialTypes": "ELT, NW3, ELT-S, NW2",
    "EventCount": 4,
    "Latitude": 39.2034,
    "Longitude": -106.262
  },
  {
    "Date": "2026-07-18",
    "Location": "Los Osos, CA",
    "Host": "Central Coast Nosework Club",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 35.2835,
    "Longitude": -120.8102
  },
  {
    "Date": "2026-07-18",
    "Location": "Woodbury, MN",
    "Host": "Gretchen Hofheins-Wackerfuss",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 44.9581,
    "Longitude": -92.9514
  },
  {
    "Date": "2026-07-25",
    "Location": "Elmira, OR",
    "Host": "Kiddy Christie",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 44.0803,
    "Longitude": -123.3404
  },
  {
    "Date": "2026-07-29",
    "Location": "Soldotna, AK",
    "Host": "Peninsula Dog Obedience Group LLC",
    "TrialTypes": "NW1, NW2, NW3, ELT",
    "EventCount": 4,
    "Latitude": 60.486,
    "Longitude": -151.028
  },
  {
    "Date": "2026-08-01",
    "Location": "Bettendorf, IA",
    "Host": "Fur Better Fur Worse Dog Training",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 41.486,
    "Longitude": -90.5485
  },
  {
    "Date": "2026-08-01",
    "Location": "Columbia, MO",
    "Host": "Columbia Canine Sports Center, LLC",
    "TrialTypes": "L1V, L1I, L1C, L2C",
    "EventCount": 4,
    "Latitude": 38.9602,
    "Longitude": -92.3479
  },
  {
    "Date": "2026-08-01",
    "Location": "Deming, WA",
    "Host": "The Nosework Magic",
    "TrialTypes": "ELT, NW2",
    "EventCount": 2,
    "Latitude": 48.8411,
    "Longitude": -122.2363
  },
  {
    "Date": "2026-08-01",
    "Location": "Jefferson, WI",
    "Host": "K9 Ventures",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 43.055,
    "Longitude": -88.7871
  },
  {
    "Date": "2026-08-01",
    "Location": "Pillager, MN",
    "Host": "Nose 2 Tail Dog Training LLC",
    "TrialTypes": "NW1, NW2, ELT-P",
    "EventCount": 3,
    "Latitude": 46.3377,
    "Longitude": -94.5097
  },
  {
    "Date": "2026-08-07",
    "Location": "Huntington Beach, CA",
    "Host": "JavaK9s, LLC",
    "TrialTypes": "ELT, L2C, L2I",
    "EventCount": 3,
    "Latitude": 33.6843,
    "Longitude": -118.0219
  },
  {
    "Date": "2026-08-08",
    "Location": "Altamont, IL",
    "Host": "Kudos for Canines, LLC",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 39.055,
    "Longitude": -88.7881
  },
  {
    "Date": "2026-08-14",
    "Location": "La Jolla, CA",
    "Host": "Rewarding Rover LLC & UberDog/Jessica Koester",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 32.8881,
    "Longitude": -117.3062
  },
  {
    "Date": "2026-08-15",
    "Location": "Greenwich, CT",
    "Host": "For the Love of Dogs NY LLC",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 41.0458,
    "Longitude": -73.6697
  },
  {
    "Date": "2026-08-15",
    "Location": "Monmouth, OR",
    "Host": "Doglandia, LLC",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 44.8111,
    "Longitude": -123.2661
  },
  {
    "Date": "2026-08-21",
    "Location": "Chelsea, MI",
    "Host": "Force Free Dale, LLC",
    "TrialTypes": "NW3, L1V, L1C, NW2",
    "EventCount": 4,
    "Latitude": 42.3337,
    "Longitude": -84.0047
  },
  {
    "Date": "2026-08-22",
    "Location": "Greenfield, MA",
    "Host": "Lucky Dog Events",
    "TrialTypes": "NW3, NW2, NW1",
    "EventCount": 3,
    "Latitude": 42.5915,
    "Longitude": -72.6046
  },
  {
    "Date": "2026-08-22",
    "Location": "Johnstown, NY",
    "Host": "My Dog Smells LLC",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 43.021,
    "Longitude": -74.3542
  },
  {
    "Date": "2026-08-22",
    "Location": "North Bend, WA",
    "Host": "Northwest K9 Sniffers",
    "TrialTypes": "ELT, L1C, L2I",
    "EventCount": 3,
    "Latitude": 47.4894,
    "Longitude": -121.7513
  },
  {
    "Date": "2026-08-28",
    "Location": "Easton and Lutherville, MD",
    "Host": "Fair Play Labradors",
    "TrialTypes": "ELT-S, L2E, L2C, NW1, L1I",
    "EventCount": 5,
    "Latitude": 39.4025,
    "Longitude": -76.6626
  },
  {
    "Date": "2026-08-28",
    "Location": "Meeker, CO",
    "Host": "Mountain Dogs LLC",
    "TrialTypes": "NW3, NW1, L2C, NW2",
    "EventCount": 4,
    "Latitude": 40.0257,
    "Longitude": -107.864
  },
  {
    "Date": "2026-08-29",
    "Location": "Dunkirk, NY",
    "Host": "Do Over Dog Training",
    "TrialTypes": "NW1, NW2, ELT-S, L3E",
    "EventCount": 4,
    "Latitude": 42.4573,
    "Longitude": -79.3718
  },
  {
    "Date": "2026-08-31",
    "Location": "Cambria, CA",
    "Host": "Gentle Touch Pet Training",
    "TrialTypes": "ELT, L1E, L2E",
    "EventCount": 3,
    "Latitude": 35.5516,
    "Longitude": -121.1155
  },
  {
    "Date": "2026-09-05",
    "Location": "Luthersville, GA",
    "Host": "Hold The Line K9 LLC",
    "TrialTypes": "L1I, L2I, NW3",
    "EventCount": 3,
    "Latitude": 33.2106,
    "Longitude": -84.7034
  },
  {
    "Date": "2026-09-11",
    "Location": "Richmond, VA",
    "Host": "Paws Plus Training, LLC",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 37.5053,
    "Longitude": -77.4214
  },
  {
    "Date": "2026-09-12",
    "Location": "Clinton, PA",
    "Host": "Nosework Addicts, LLC",
    "TrialTypes": "NW1, ELT",
    "EventCount": 2,
    "Latitude": 40.5701,
    "Longitude": -80.3388
  },
  {
    "Date": "2026-09-12",
    "Location": "Lafayette Hill, PA",
    "Host": "Sniff Sniff Hooray",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 40.12,
    "Longitude": -75.2275
  },
  {
    "Date": "2026-09-12",
    "Location": "Loma Mar, CA",
    "Host": "The Bay Team",
    "TrialTypes": "NW2, ELT",
    "EventCount": 2,
    "Latitude": 37.2396,
    "Longitude": -122.3308
  },
  {
    "Date": "2026-09-12",
    "Location": "Sharon, MA",
    "Host": "Bay State Sniffers",
    "TrialTypes": "ELT, ELT-S, NW2",
    "EventCount": 3,
    "Latitude": 42.1062,
    "Longitude": -71.1993
  },
  {
    "Date": "2026-09-13",
    "Location": "Colesville, MD",
    "Host": "Red Huskies",
    "TrialTypes": "ELT-S, L3C, NW3",
    "EventCount": 3,
    "Latitude": 39.0252,
    "Longitude": -76.9561
  },
  {
    "Date": "2026-09-18",
    "Location": "Flint, MI",
    "Host": "Every Dog Nosework",
    "TrialTypes": "NW3, NW1, NW2, ELT-P",
    "EventCount": 4,
    "Latitude": 42.9747,
    "Longitude": -83.7087
  },
  {
    "Date": "2026-09-18",
    "Location": "New Milford, PA",
    "Host": "Your Dog's Place, LLC",
    "TrialTypes": "ELT, ELT-S, L1V",
    "EventCount": 3,
    "Latitude": 41.9132,
    "Longitude": -75.7562
  },
  {
    "Date": "2026-09-19",
    "Location": "Ford City, PA",
    "Host": "Steel City Nosework, LLC",
    "TrialTypes": "ELT, ELT-S, NW2",
    "EventCount": 3,
    "Latitude": 40.7489,
    "Longitude": -79.5123
  },
  {
    "Date": "2026-09-19",
    "Location": "Glen Mills, PA",
    "Host": "Firezone GS",
    "TrialTypes": "NW3, ELT-P",
    "EventCount": 2,
    "Latitude": 39.8905,
    "Longitude": -75.5226
  },
  {
    "Date": "2026-09-19",
    "Location": "Palmer, MA",
    "Host": "HeavenScent Sniffers",
    "TrialTypes": "NW3, L2V, L1E",
    "EventCount": 3,
    "Latitude": 42.1916,
    "Longitude": -72.3334
  },
  {
    "Date": "2026-09-19",
    "Location": "Stevenson, WA",
    "Host": "Sharon Smith",
    "TrialTypes": "NW1, NW2, L1V, L1C",
    "EventCount": 4,
    "Latitude": 45.6571,
    "Longitude": -121.9023
  },
  {
    "Date": "2026-09-25",
    "Location": "Frederick, MD",
    "Host": "Red Huskies",
    "TrialTypes": "L3E, ELT-S, ELT-P, ELT",
    "EventCount": 4,
    "Latitude": 39.461,
    "Longitude": -77.4009
  },
  {
    "Date": "2026-09-26",
    "Location": "Columbus, MT",
    "Host": "Canine Connection",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 45.5934,
    "Longitude": -109.2644
  },
  {
    "Date": "2026-09-26",
    "Location": "Glenview, IL",
    "Host": "Northwest Obedience Club Inc",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 42.0644,
    "Longitude": -87.8023
  },
  {
    "Date": "2026-09-26",
    "Location": "Kintnersville, PA",
    "Host": "Paws n’ Sniff",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 40.5699,
    "Longitude": -75.2116
  },
  {
    "Date": "2026-09-26",
    "Location": "Lawrenceville, GA",
    "Host": "Right Choice Dog Training, LLC",
    "TrialTypes": "L1E, NW2, ELT-S, L3I",
    "EventCount": 4,
    "Latitude": 33.9205,
    "Longitude": -83.9918
  },
  {
    "Date": "2026-09-26",
    "Location": "New City, NY",
    "Host": "Saints2Source, LLC",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 41.1421,
    "Longitude": -73.9888
  },
  {
    "Date": "2026-09-26",
    "Location": "Rehoboth, MA",
    "Host": "Dogs Make Scents",
    "TrialTypes": "NW3, NW1",
    "EventCount": 2,
    "Latitude": 41.8036,
    "Longitude": -71.2771
  },
  {
    "Date": "2026-09-27",
    "Location": "Dover, DE",
    "Host": "Patricia Grassey",
    "TrialTypes": "NW3, NW2, ELT",
    "EventCount": 3,
    "Latitude": 39.1282,
    "Longitude": -75.5246
  },
  {
    "Date": "2026-09-28",
    "Location": "Concord, NH",
    "Host": "For the Love of Dogs NY LLC",
    "TrialTypes": "NW2, NW3",
    "EventCount": 2,
    "Latitude": 43.1624,
    "Longitude": -71.5225
  },
  {
    "Date": "2026-10-03",
    "Location": "Hagerstown, MD",
    "Host": "Firezone GS",
    "TrialTypes": "ELT-S, NW3, ELT",
    "EventCount": 3,
    "Latitude": 39.6345,
    "Longitude": -77.688
  },
  {
    "Date": "2026-10-03",
    "Location": "Hammond, LA",
    "Host": "Dog Gone Right, LLC",
    "TrialTypes": "NW1, NW2, L1I, ELT-S",
    "EventCount": 4,
    "Latitude": 30.4604,
    "Longitude": -90.5085
  },
  {
    "Date": "2026-10-03",
    "Location": "Jefferson, OR",
    "Host": "Doglandia, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 44.602,
    "Longitude": -121.1968
  },
  {
    "Date": "2026-10-03",
    "Location": "Nashua, NH",
    "Host": "The Big Sniff, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 42.7263,
    "Longitude": -71.4238
  },
  {
    "Date": "2026-10-03",
    "Location": "New Paltz, NY",
    "Host": "Top Notch Dogs, LLC",
    "TrialTypes": "NW1, L2C, ELT",
    "EventCount": 3,
    "Latitude": 41.7922,
    "Longitude": -74.0589
  },
  {
    "Date": "2026-10-03",
    "Location": "Northampton, MA",
    "Host": "Lucky Dog Events",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 42.325,
    "Longitude": -72.6122
  },
  {
    "Date": "2026-10-03",
    "Location": "Seguin, TX",
    "Host": "Scent Work Across Texas",
    "TrialTypes": "NW1, NW2, NW3",
    "EventCount": 3,
    "Latitude": 29.5938,
    "Longitude": -97.9807
  },
  {
    "Date": "2026-10-03",
    "Location": "Sisters, OR",
    "Host": "Sunriver K9 Genie, LLC",
    "TrialTypes": "NW2, ELT",
    "EventCount": 2,
    "Latitude": 44.3372,
    "Longitude": -121.5255
  },
  {
    "Date": "2026-10-03",
    "Location": "Waynesboro, PA",
    "Host": "Nose-It-All, LLC",
    "TrialTypes": "NW1, NW2, NW3",
    "EventCount": 3,
    "Latitude": 39.7588,
    "Longitude": -77.5859
  },
  {
    "Date": "2026-10-09",
    "Location": "Middlebury, CT",
    "Host": "For the Love of Dogs NY LLC",
    "TrialTypes": "ELT, ELT-P, NW1",
    "EventCount": 3,
    "Latitude": 41.485,
    "Longitude": -73.0788
  },
  {
    "Date": "2026-10-09",
    "Location": "Pueblo, CO",
    "Host": "Mountain Dogs, LLC",
    "TrialTypes": "SMT, ELT",
    "EventCount": 2,
    "Latitude": 38.2766,
    "Longitude": -104.5819
  },
  {
    "Date": "2026-10-09",
    "Location": "Rock Island, IL",
    "Host": "Fur Better Fur Worse Dog Training",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 41.4847,
    "Longitude": -90.5928
  },
  {
    "Date": "2026-10-10",
    "Location": "Auburn, WA",
    "Host": "Northwest K9 Sniffers",
    "TrialTypes": "ELT-S, L2E, L3I",
    "EventCount": 3,
    "Latitude": 47.2848,
    "Longitude": -122.2489
  },
  {
    "Date": "2026-10-10",
    "Location": "Court Granger, IA",
    "Host": "KBP Dog Training",
    "TrialTypes": "NW1, NW2, NW3",
    "EventCount": 3,
    "Latitude": 41.7151,
    "Longitude": -93.8056
  },
  {
    "Date": "2026-10-10",
    "Location": "Eagan, MN",
    "Host": "St Paul Dog Training Club",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 44.8338,
    "Longitude": -93.1314
  },
  {
    "Date": "2026-10-10",
    "Location": "Eldred, NY",
    "Host": "Your Dog's Place, LLC",
    "TrialTypes": "L2C, NW2, ELT-S, L1C",
    "EventCount": 4,
    "Latitude": 41.5625,
    "Longitude": -74.9149
  },
  {
    "Date": "2026-10-10",
    "Location": "Helena, MT",
    "Host": "Nose Work Breakfast Club",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 46.5516,
    "Longitude": -112.0276
  },
  {
    "Date": "2026-10-10",
    "Location": "Loveland, CO",
    "Host": "Paws 4 Thought Dog Training, LLC",
    "TrialTypes": "NW2, NW1, L1E",
    "EventCount": 3,
    "Latitude": 40.4285,
    "Longitude": -105.0666
  },
  {
    "Date": "2026-10-10",
    "Location": "Sedona, AZ",
    "Host": "Successful Sniffer",
    "TrialTypes": "ELT, NW1, NW2",
    "EventCount": 3,
    "Latitude": 34.8224,
    "Longitude": -111.7674
  },
  {
    "Date": "2026-10-10",
    "Location": "Troy, VA",
    "Host": "Your Dogs Knows LLC",
    "TrialTypes": "NW1, ELT-S, L1V, L2V",
    "EventCount": 4,
    "Latitude": 37.9224,
    "Longitude": -78.2555
  },
  {
    "Date": "2026-10-10",
    "Location": "Youngwood, PA",
    "Host": "Steel City Nosework, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 40.2071,
    "Longitude": -79.5652
  },
  {
    "Date": "2026-10-12",
    "Location": "Swansea, MA",
    "Host": "Amy Conrad & Heaven Scent Sniffers",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 41.7015,
    "Longitude": -71.1914
  },
  {
    "Date": "2026-10-16",
    "Location": "Calhan, CO",
    "Host": "Mountain Dogs LLC",
    "TrialTypes": "ELT, NW3, ELT-P",
    "EventCount": 3,
    "Latitude": 38.9875,
    "Longitude": -104.3082
  },
  {
    "Date": "2026-10-16",
    "Location": "Rossville, GA",
    "Host": "Camelot Shepherds, Inc.",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 34.938,
    "Longitude": -85.3242
  },
  {
    "Date": "2026-10-16",
    "Location": "Wilmington, DE",
    "Host": "Patricia Grassey",
    "TrialTypes": "NW3, ELT, ELT-P",
    "EventCount": 3,
    "Latitude": 39.7629,
    "Longitude": -75.5451
  },
  {
    "Date": "2026-10-17",
    "Location": "Albuquerque, NM",
    "Host": "New Mexico Canine Scent Work, LLC",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 35.0906,
    "Longitude": -106.6344
  },
  {
    "Date": "2026-10-17",
    "Location": "Centralia, WA",
    "Host": "Let's Talk Dogs, LLC and About Face K9",
    "TrialTypes": "ELT-S, NW2, L3C",
    "EventCount": 3,
    "Latitude": 46.689,
    "Longitude": -122.9298
  },
  {
    "Date": "2026-10-17",
    "Location": "Colebrook, CT",
    "Host": "For the Love of Dogs NY LLC",
    "TrialTypes": "L1E, ELT-S, NW2, ELT",
    "EventCount": 4,
    "Latitude": 41.9639,
    "Longitude": -73.1219
  },
  {
    "Date": "2026-10-17",
    "Location": "Conroe, TX",
    "Host": "Scent Work Across Texas",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 30.2691,
    "Longitude": -95.4481
  },
  {
    "Date": "2026-10-17",
    "Location": "Delevan, NY",
    "Host": "Do Over Dog Training",
    "TrialTypes": "NW3, L1C, L3V",
    "EventCount": 3,
    "Latitude": 42.4732,
    "Longitude": -78.5014
  },
  {
    "Date": "2026-10-17",
    "Location": "Niantic, IL",
    "Host": "Kudos for Canines, LLC",
    "TrialTypes": "L2C, ELT-S, NW3",
    "EventCount": 3,
    "Latitude": 39.8377,
    "Longitude": -89.2104
  },
  {
    "Date": "2026-10-17",
    "Location": "Staples, MN",
    "Host": "Nose 2 Tail Dog Training LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 46.3177,
    "Longitude": -94.8375
  },
  {
    "Date": "2026-10-17",
    "Location": "Watsonville, CA",
    "Host": "CalCoastal Dog Owners Group",
    "TrialTypes": "L3V, L2V, L1V",
    "EventCount": 3,
    "Latitude": 36.9181,
    "Longitude": -121.7242
  },
  {
    "Date": "2026-10-24",
    "Location": "Cumming, GA",
    "Host": "Georgia Nosework",
    "TrialTypes": "NW3, L1C, NW2",
    "EventCount": 3,
    "Latitude": 34.2479,
    "Longitude": -84.1854
  },
  {
    "Date": "2026-10-24",
    "Location": "Fishkill, NY",
    "Host": "Top Notch Dogs, LLC",
    "TrialTypes": "ELT-P, ELT",
    "EventCount": 2,
    "Latitude": 41.558,
    "Longitude": -73.941
  },
  {
    "Date": "2026-10-24",
    "Location": "Green Bay, WI",
    "Host": "NEWk9Scent Work LLC",
    "TrialTypes": "NW2, ELT",
    "EventCount": 2,
    "Latitude": 44.5154,
    "Longitude": -87.9679
  },
  {
    "Date": "2026-10-24",
    "Location": "Kilmarnock, VA",
    "Host": "Red Huskies",
    "TrialTypes": "NW1, ELT-S, ELT",
    "EventCount": 3,
    "Latitude": 37.6765,
    "Longitude": -76.3742
  },
  {
    "Date": "2026-10-24",
    "Location": "Norton, MA",
    "Host": "Dogs Make Scents",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 41.9326,
    "Longitude": -71.2364
  },
  {
    "Date": "2026-10-24",
    "Location": "Penn Yan, NY",
    "Host": "2 Psyched 4 Dogs",
    "TrialTypes": "ELT, ELT-S, NW2",
    "EventCount": 3,
    "Latitude": 42.6216,
    "Longitude": -77.0262
  },
  {
    "Date": "2026-10-24",
    "Location": "Reedsport, OR",
    "Host": "Wells Creek Dog Training",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 43.696,
    "Longitude": -124.0505
  },
  {
    "Date": "2026-10-26",
    "Location": "Stockton, CA",
    "Host": "Two Nosey Girls",
    "TrialTypes": "L3E, L2E",
    "EventCount": 2,
    "Latitude": 37.9532,
    "Longitude": -121.2516
  },
  {
    "Date": "2026-10-28",
    "Location": "Aberdeen, MD",
    "Host": "Red Huskies",
    "TrialTypes": "ELT, ELT-P, ELT-S, L2I",
    "EventCount": 4,
    "Latitude": 39.5338,
    "Longitude": -76.1962
  },
  {
    "Date": "2026-10-30",
    "Location": "Cameron Park, CA",
    "Host": "Sierra Sniffing Canines, Inc",
    "TrialTypes": "NW3, NW1",
    "EventCount": 2,
    "Latitude": 38.6777,
    "Longitude": -120.9758
  },
  {
    "Date": "2026-10-30",
    "Location": "Harrington, DE",
    "Host": "Shamrock Pot Of Gold K9 Scenter",
    "TrialTypes": "NW3, ELT, NW1, ELT-S, L2E",
    "EventCount": 5,
    "Latitude": 38.9262,
    "Longitude": -75.6243
  },
  {
    "Date": "2026-10-30",
    "Location": "Honey Brook, PA",
    "Host": "Patricia Grassey",
    "TrialTypes": "NW1, L1C, L2C, NW2, L3I, L3C",
    "EventCount": 6,
    "Latitude": 40.0811,
    "Longitude": -75.8937
  },
  {
    "Date": "2026-10-30",
    "Location": "Lakeville, MN",
    "Host": "St Paul Dog Training Club",
    "TrialTypes": "ELT-P, NW2, ELT-S, L1C",
    "EventCount": 4,
    "Latitude": 44.601,
    "Longitude": -93.2681
  },
  {
    "Date": "2026-10-30",
    "Location": "Lawrenceville, GA",
    "Host": "Chestnut Hill Canine Sports",
    "TrialTypes": "NW3, NW1, L2I",
    "EventCount": 3,
    "Latitude": 33.9766,
    "Longitude": -83.988
  },
  {
    "Date": "2026-10-30",
    "Location": "Montrose, CO",
    "Host": "Mountain Dogs LLC",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 38.5161,
    "Longitude": -107.8819
  },
  {
    "Date": "2026-10-30",
    "Location": "York, PA",
    "Host": "Firezone GS",
    "TrialTypes": "ELT, NW3, ELT-P",
    "EventCount": 3,
    "Latitude": 39.9352,
    "Longitude": -76.698
  },
  {
    "Date": "2026-10-31",
    "Location": "Beloit, WI",
    "Host": "George Carpenter",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 42.5391,
    "Longitude": -89.0118
  },
  {
    "Date": "2026-10-31",
    "Location": "Bonham, TX",
    "Host": "All About The Nose",
    "TrialTypes": "NW2, NW1",
    "EventCount": 2,
    "Latitude": 33.5344,
    "Longitude": -96.1617
  },
  {
    "Date": "2026-10-31",
    "Location": "Franklin, GA",
    "Host": "Hold The Line K9 LLC",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 34.3862,
    "Longitude": -83.2144
  },
  {
    "Date": "2026-10-31",
    "Location": "Kennebunkport, ME",
    "Host": "Elizabeth Dutton",
    "TrialTypes": "NW3, ELT-P",
    "EventCount": 2,
    "Latitude": 43.3351,
    "Longitude": -70.4418
  },
  {
    "Date": "2026-10-31",
    "Location": "Sturgis, SD",
    "Host": "Two Paws Up Dog Training, LLC",
    "TrialTypes": "L2V, L2E, L1V, L1E",
    "EventCount": 4,
    "Latitude": 44.3829,
    "Longitude": -103.4796
  },
  {
    "Date": "2026-10-31",
    "Location": "White Plains, NY",
    "Host": "Saints2Source, LLC",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 40.9873,
    "Longitude": -73.7992
  },
  {
    "Date": "2026-10-31",
    "Location": "Yamhill, OR",
    "Host": "Nose Work Detectives, LLC",
    "TrialTypes": "ELT-P",
    "EventCount": 1,
    "Latitude": 45.2497,
    "Longitude": -123.238
  },
  {
    "Date": "2026-11-01",
    "Location": "San Martin, CA",
    "Host": "B.L. McMutts LLC",
    "TrialTypes": "L1V, L2V",
    "EventCount": 2,
    "Latitude": 37.1158,
    "Longitude": -121.5541
  },
  {
    "Date": "2026-11-03",
    "Location": "Ventura, CA",
    "Host": "Pink Biscuit K9s",
    "TrialTypes": "NW1, NW2, ELT-P",
    "EventCount": 3,
    "Latitude": 34.4748,
    "Longitude": -119.1048
  },
  {
    "Date": "2026-11-06",
    "Location": "Rome, GA",
    "Host": "Georgia Nosework, LLC",
    "TrialTypes": "NW3, NW1, NW2, ELT",
    "EventCount": 4,
    "Latitude": 34.3019,
    "Longitude": -85.1471
  },
  {
    "Date": "2026-11-07",
    "Location": "Bonner Springs, KS",
    "Host": "Brookside Pet Concierge",
    "TrialTypes": "NW1, NW2, NW3",
    "EventCount": 3,
    "Latitude": 39.0418,
    "Longitude": -94.8773
  },
  {
    "Date": "2026-11-07",
    "Location": "Colorado Springs, CO",
    "Host": "Beyond Elevation K9",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 38.7903,
    "Longitude": -104.8483
  },
  {
    "Date": "2026-11-07",
    "Location": "Fishkill, NY",
    "Host": "Top Notch Dogs, LLC",
    "TrialTypes": "L3C, NW2, ELT",
    "EventCount": 3,
    "Latitude": 41.5836,
    "Longitude": -73.9035
  },
  {
    "Date": "2026-11-07",
    "Location": "Geneva, IL",
    "Host": "For Your K9, Inc",
    "TrialTypes": "ELT, NW1, NW2",
    "EventCount": 3,
    "Latitude": 41.8696,
    "Longitude": -88.3007
  },
  {
    "Date": "2026-11-07",
    "Location": "Guerneville, CA",
    "Host": "Jen Huot",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 38.5636,
    "Longitude": -123.0254
  },
  {
    "Date": "2026-11-07",
    "Location": "Lancaster, PA",
    "Host": "Red Huskies Nose Work, LLC",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 40.0176,
    "Longitude": -76.3
  },
  {
    "Date": "2026-11-07",
    "Location": "Las Vegas, NV",
    "Host": "imPETus Animal Training",
    "TrialTypes": "NW3, NW1, L1C",
    "EventCount": 3,
    "Latitude": 36.166,
    "Longitude": -115.1902
  },
  {
    "Date": "2026-11-07",
    "Location": "Mays Landing, NJ",
    "Host": "Rotts-n-Notts Nosework LLC",
    "TrialTypes": "L1C, NW2, L1E, NW1",
    "EventCount": 4,
    "Latitude": 39.5014,
    "Longitude": -74.6951
  },
  {
    "Date": "2026-11-07",
    "Location": "Woodward, IA",
    "Host": "KBP Dog Training",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 41.8403,
    "Longitude": -93.8951
  },
  {
    "Date": "2026-11-09",
    "Location": "West Berlin, NJ",
    "Host": "For the Love of Dogs NY LLC",
    "TrialTypes": "L2E, L1V, NW3, ELT-P",
    "EventCount": 4,
    "Latitude": 39.816,
    "Longitude": -74.9407
  },
  {
    "Date": "2026-11-11",
    "Location": "Petaluma, CA",
    "Host": "Marin Humane",
    "TrialTypes": "ELT-P, ELT",
    "EventCount": 2,
    "Latitude": 38.2235,
    "Longitude": -122.613
  },
  {
    "Date": "2026-11-13",
    "Location": "Chula Vista, CA",
    "Host": "Rewarding Rover LLC, Uberdog, & Claire Brocato",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 32.5983,
    "Longitude": -117.1104
  },
  {
    "Date": "2026-11-13",
    "Location": "Gilbertsville, PA",
    "Host": "Sniff Sniff Hooray",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 40.306,
    "Longitude": -75.6036
  },
  {
    "Date": "2026-11-13",
    "Location": "Ypsilanti, MI",
    "Host": "Every Dog Nosework",
    "TrialTypes": "ELT, ELT-S, L2C",
    "EventCount": 3,
    "Latitude": 42.2129,
    "Longitude": -83.6261
  },
  {
    "Date": "2026-11-14",
    "Location": "Coburg, OR",
    "Host": "Kiddy Christie",
    "TrialTypes": "NW1, NW2, L2I, L3C",
    "EventCount": 4,
    "Latitude": 44.0882,
    "Longitude": -123.0599
  },
  {
    "Date": "2026-11-14",
    "Location": "Greer, SC",
    "Host": "Trained to Trust, LLC",
    "TrialTypes": "NW3, L2V, NW1",
    "EventCount": 3,
    "Latitude": 34.8948,
    "Longitude": -82.1885
  },
  {
    "Date": "2026-11-14",
    "Location": "Montgomery, AL",
    "Host": "By A Nose Nosework",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 32.4074,
    "Longitude": -86.3285
  },
  {
    "Date": "2026-11-14",
    "Location": "Waymart, PA",
    "Host": "Your Dog's Place, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 41.6059,
    "Longitude": -75.3827
  },
  {
    "Date": "2026-11-16",
    "Location": "Ellicott City, MD",
    "Host": "Red Huskies",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 39.2699,
    "Longitude": -76.8796
  },
  {
    "Date": "2026-11-16",
    "Location": "Hartford, CT",
    "Host": "For the Love of Dogs NY LLC",
    "TrialTypes": "L1I, ELT-S, ELT",
    "EventCount": 3,
    "Latitude": 41.7578,
    "Longitude": -72.6947
  },
  {
    "Date": "2026-11-18",
    "Location": "Monkton, MD",
    "Host": "Firezone GS",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 39.5311,
    "Longitude": -76.6482
  },
  {
    "Date": "2026-11-20",
    "Location": "Boring, OR",
    "Host": "Trust Your Dog K9 Events",
    "TrialTypes": "NW2, NW3, ELT",
    "EventCount": 3,
    "Latitude": 45.4656,
    "Longitude": -122.3649
  },
  {
    "Date": "2026-11-20",
    "Location": "Centreville, MD",
    "Host": "Fair Play Point Labradors",
    "TrialTypes": "SMT, ELT-S, L1I",
    "EventCount": 3,
    "Latitude": 39.0184,
    "Longitude": -76.0973
  },
  {
    "Date": "2026-11-20",
    "Location": "Denver, PA",
    "Host": "Patricia Grassey",
    "TrialTypes": "ELT, ELT-P, ELT-S, L3V",
    "EventCount": 4,
    "Latitude": 40.2266,
    "Longitude": -76.1635
  },
  {
    "Date": "2026-11-20",
    "Location": "Kintnersville, PA",
    "Host": "Paws n' Sniff",
    "TrialTypes": "NW1, L1C, L1E, L1I",
    "EventCount": 4,
    "Latitude": 40.5291,
    "Longitude": -75.1576
  },
  {
    "Date": "2026-11-20",
    "Location": "Lompoc, CA",
    "Host": "Gentle Touch Pet Training",
    "TrialTypes": "NW3, NW2, NW1",
    "EventCount": 3,
    "Latitude": 34.6052,
    "Longitude": -120.4606
  },
  {
    "Date": "2026-11-20",
    "Location": "Loranger, LA",
    "Host": "Dog Gone Right",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 30.6183,
    "Longitude": -90.3659
  },
  {
    "Date": "2026-11-21",
    "Location": "Centralia, WA",
    "Host": "Let's Talk Dogs, LLC & About Face K9 Academy",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 46.6856,
    "Longitude": -122.9993
  },
  {
    "Date": "2026-11-21",
    "Location": "DeLeon Springs, FL",
    "Host": "River Poodles Training, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 29.1433,
    "Longitude": -81.3389
  },
  {
    "Date": "2026-11-21",
    "Location": "Delta, CO",
    "Host": "Mountain Dogs LLC",
    "EventLink": "https://mountaindogs.org/",
    "TrialTypes": "ELT, NW3, NW1, NW2",
    "EventCount": 4,
    "Latitude": 38.8841,
    "Longitude": -107.8624
  },
  {
    "Date": "2026-11-21",
    "Location": "Fork Union, VA",
    "Host": "Your Dog Knows, LLC",
    "TrialTypes": "ELT, L2I, NW2",
    "EventCount": 3,
    "Latitude": 37.7263,
    "Longitude": -78.3035
  },
  {
    "Date": "2026-11-21",
    "Location": "Marble Falls, TX",
    "Host": "Scent Work Across Texas",
    "TrialTypes": "ELT-S, L2I, NW3",
    "EventCount": 3,
    "Latitude": 30.6174,
    "Longitude": -98.2701
  },
  {
    "Date": "2026-11-21",
    "Location": "Ontario, CA",
    "Host": "Agile Paws Dog Sports",
    "TrialTypes": "NW1, L3I, L3C",
    "EventCount": 3,
    "Latitude": 34.0459,
    "Longitude": -117.6661
  },
  {
    "Date": "2026-11-21",
    "Location": "Smyrna, TN",
    "Host": "Dogs Have Amazing Noses, LLC",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 35.9574,
    "Longitude": -86.5565
  },
  {
    "Date": "2026-11-22",
    "Location": "Wilbraham, MA",
    "Host": "Heaven Scent Sniffers",
    "EventLink": "https://www.heavenscentsniffers.com/",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 42.0817,
    "Longitude": -72.451
  },
  {
    "Date": "2026-11-27",
    "Location": "Elizabeth, CO",
    "Host": "Beyond Elevation K9",
    "EventLink": "https://www.beyondelevationk9.com/",
    "TrialTypes": "NW2, NW3",
    "EventCount": 2,
    "Latitude": 39.3693,
    "Longitude": -104.5624
  },
  {
    "Date": "2026-11-27",
    "Location": "Long Beach, CA",
    "Host": "JavaK9s, LLC",
    "EventLink": "http://www.javak9s.com/",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 33.7339,
    "Longitude": -118.1549
  },
  {
    "Date": "2026-11-27",
    "Location": "San Jose, CA",
    "Host": "The Bay Team",
    "EventLink": "https://www.bayteam.org/",
    "TrialTypes": "NW2, NW1",
    "EventCount": 2,
    "Latitude": 37.349,
    "Longitude": -121.9175
  },
  {
    "Date": "2026-11-28",
    "Location": "Bowie, MD",
    "Host": "Red Huskies Nose Work, LLC",
    "EventLink": "https://nosework.redhuskies.com/",
    "TrialTypes": "ELT-P, L3I, ELT-S, ELT",
    "EventCount": 4,
    "Latitude": 38.9071,
    "Longitude": -76.7303
  },
  {
    "Date": "2026-11-28",
    "Location": "Cottage Grove, MN",
    "Host": "Gretchen Hofheins-Wackerfuss",
    "EventLink": "https://www.sniffingminpin.com/",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 44.8037,
    "Longitude": -92.9725
  },
  {
    "Date": "2026-11-28",
    "Location": "Cumming, GA",
    "Host": "Georgia Nosework",
    "EventLink": "https://georgianosework.com/events/",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 34.2062,
    "Longitude": -84.1823
  },
  {
    "Date": "2026-11-28",
    "Location": "Lebanon, NJ",
    "Host": "Sirius K9 Solutions",
    "EventLink": "http://www.siriusk9solutions.net/NoseWork.html",
    "TrialTypes": "L2I, NW2, ELT-P",
    "EventCount": 3,
    "Latitude": 40.6192,
    "Longitude": -74.8109
  },
  {
    "Date": "2026-11-28",
    "Location": "Mifflinburg, PA",
    "Host": "Paws-itively Obedient Dog Training School",
    "EventLink": "https://pawsitivelyobedient.net/special-events/",
    "TrialTypes": "NW3, NW1",
    "EventCount": 2,
    "Latitude": 40.8895,
    "Longitude": -77.0205
  },
  {
    "Date": "2026-11-28",
    "Location": "Silex, MO",
    "Host": "WestInn Kennels",
    "EventLink": "https://westinnkennels.wixsite.com/silex",
    "TrialTypes": "NW1, NW2, NW3",
    "EventCount": 3,
    "Latitude": 39.1378,
    "Longitude": -91.1005
  },
  {
    "Date": "2026-11-28",
    "Location": "Vancouver, WA",
    "Host": "Sniffketeers",
    "EventLink": "https://noseworktrial.blogspot.com/",
    "TrialTypes": "NW1, L1E, L1I, L3V",
    "EventCount": 4,
    "Latitude": 45.6173,
    "Longitude": -122.7
  },
  {
    "Date": "2026-11-29",
    "Location": "Gettysburg, PA",
    "Host": "Firezone GS",
    "EventLink": "https://www.firezonegiantschnauzers.com/wwiimuseum",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 39.8749,
    "Longitude": -77.2063
  },
  {
    "Date": "2026-12-05",
    "Location": "Centralia, WA",
    "Host": "Let's Talk Dogs, LLC and About Face K9",
    "EventLink": "http://www.dorothyturley.com/",
    "TrialTypes": "ELT, NW1, L2I",
    "EventCount": 3,
    "Latitude": 46.7344,
    "Longitude": -122.9505
  },
  {
    "Date": "2026-12-05",
    "Location": "Charlton, MA",
    "Host": "HeavenScent Sniffers",
    "EventLink": "https://www.heavenscentsniffers.com/",
    "TrialTypes": "NW2, NW1",
    "EventCount": 2,
    "Latitude": 42.1325,
    "Longitude": -71.9703
  },
  {
    "Date": "2026-12-05",
    "Location": "Fillmore, CA",
    "Host": "Pink Biscuit K9s",
    "EventLink": "https://www.pinkbiscuitk9s.com/",
    "TrialTypes": "NW3, ELT-S, L2C",
    "EventCount": 3,
    "Latitude": 34.372,
    "Longitude": -118.9285
  },
  {
    "Date": "2026-12-05",
    "Location": "Fredonia, WI",
    "Host": "On Point Elite Dog Sports, LLC",
    "EventLink": "https://www.opedogsports.com/nacsw-trials",
    "TrialTypes": "NW1, NW2, NW3",
    "EventCount": 3,
    "Latitude": 43.5149,
    "Longitude": -87.9538
  },
  {
    "Date": "2026-12-05",
    "Location": "Hoover, AL",
    "Host": "Southeast Scent Work Alliance, LLC (SSWA)",
    "EventLink": "https://www.southeastscent.com/nw3-elite-hoover-al/",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 33.3396,
    "Longitude": -86.8115
  },
  {
    "Date": "2026-12-05",
    "Location": "Newfoundland, PA",
    "Host": "Your Dog's Place, LLC",
    "EventLink": "https://yourdogsplace.com/nacsw-trials/",
    "TrialTypes": "L2V, ELT-S, NW3",
    "EventCount": 3,
    "Latitude": 41.3522,
    "Longitude": -75.3208
  },
  {
    "Date": "2026-12-05",
    "Location": "Tucson, AZ",
    "Host": "Patience Unlimited Dog Training",
    "EventLink": "http://www.patienceunlimited.com/nacsw.html",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 32.1984,
    "Longitude": -110.9368
  },
  {
    "Date": "2026-12-05",
    "Location": "West River, MD",
    "Host": "Chesapeake Search Dogs",
    "EventLink": "https://chesapeakesearchdogs.org/",
    "TrialTypes": "ELT-S, L3E, NW1, NW2",
    "EventCount": 4,
    "Latitude": 38.8753,
    "Longitude": -76.621
  },
  {
    "Date": "2026-12-06",
    "Location": "Hubertus, WI",
    "Host": "Loving Paws Dog Training, LLC",
    "EventLink": "https://www.lovingpawsllc.com/premium-elt-elt",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 43.2354,
    "Longitude": -88.242
  },
  {
    "Date": "2026-12-07",
    "Location": "Stockton, CA",
    "Host": "Two Nosey Girls",
    "EventLink": "https://www.twonoseygirls.com/events.html",
    "TrialTypes": "ELT, ELT-S, L3I",
    "EventCount": 3,
    "Latitude": 37.9774,
    "Longitude": -121.3109
  },
  {
    "Date": "2026-12-11",
    "Location": "Douglassville, PA",
    "Host": "Patricia Grassey",
    "EventLink": "https://www.thesniffinghound.com/",
    "TrialTypes": "NW3, NW2, ELT",
    "EventCount": 3,
    "Latitude": 40.253,
    "Longitude": -75.6827
  },
  {
    "Date": "2026-12-12",
    "Location": "Deckers, CO",
    "Host": "Beyond Elevation K9",
    "EventLink": "https://www.beyondelevationk9.com/",
    "TrialTypes": "NW3, ELT-S, L2E",
    "EventCount": 3,
    "Latitude": 39.2472,
    "Longitude": -105.1996
  },
  {
    "Date": "2026-12-12",
    "Location": "Durham, NC",
    "Host": "Whole Dog Institute, LLC",
    "EventLink": "https://wholedoginstitute.com/",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 35.969,
    "Longitude": -78.856
  },
  {
    "Date": "2026-12-12",
    "Location": "Obetz, OH",
    "Host": "CleverDogs",
    "EventLink": "https://cleverdogsohio.com/",
    "TrialTypes": "NW1, NW2, ELT",
    "EventCount": 3,
    "Latitude": 39.9192,
    "Longitude": -82.9719
  },
  {
    "Date": "2026-12-12",
    "Location": "Sauget, IL",
    "Host": "Happy Dog Concepts, LLC",
    "EventLink": "https://happydogconcepts.com/events",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 38.5794,
    "Longitude": -90.1333
  },
  {
    "Date": "2026-12-12",
    "Location": "Westminster, MD",
    "Host": "Red Huskies Nose Work, LLC",
    "EventLink": "https://nosework.redhuskies.com/",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 39.5343,
    "Longitude": -76.9605
  },
  {
    "Date": "2026-12-18",
    "Location": "Pittstown, NJ",
    "Host": "Shamrock Pot Of Gold K9 Scenter",
    "EventLink": "https://shamrockpotofgoldk9scenter.com/trials/pittstown-trial-info-12-18-2026-12-20-2026/",
    "TrialTypes": "ELT-S, L1C, NW3, ELT",
    "EventCount": 4,
    "Latitude": 40.5627,
    "Longitude": -74.9993
  },
  {
    "Date": "2026-12-19",
    "Location": "Alpharetta, GA",
    "Host": "Georgia Nosework",
    "EventLink": "https://georgianosework.com/",
    "TrialTypes": "SMT, ELT-P",
    "EventCount": 2,
    "Latitude": 34.0305,
    "Longitude": -84.3198
  },
  {
    "Date": "2026-12-19",
    "Location": "Crownsville, MD",
    "Host": "Red Huskies Nose Work, LLC",
    "EventLink": "https://nosework.redhuskies.com/index.php/events/crownsville_dec2026/",
    "TrialTypes": "NW3, ELT-S, ELT",
    "EventCount": 3,
    "Latitude": 39.0328,
    "Longitude": -76.6336
  },
  {
    "Date": "2026-12-19",
    "Location": "Imperial Beach, CA",
    "Host": "Rewarding Rover LLC/Uber dog/Claire Brocato",
    "EventLink": "https://www.rewardingrover.com/",
    "TrialTypes": "ELT, L1E, NW1",
    "EventCount": 3,
    "Latitude": 32.5394,
    "Longitude": -117.0839
  },
  {
    "Date": "2026-12-19",
    "Location": "Salem, OR",
    "Host": "Doglandia, LLC",
    "EventLink": "https://cyberdogonline.net/?page_id=235",
    "TrialTypes": "NW3, ELT-P",
    "EventCount": 2,
    "Latitude": 44.9471,
    "Longitude": -122.9953
  },
  {
    "Date": "2026-12-27",
    "Location": "Exton, PA",
    "Host": "Patricia Grassey",
    "EventLink": "https://www.thesniffinghound.com/",
    "TrialTypes": "NW3, ELT-P, ELT-S",
    "EventCount": 3,
    "Latitude": 40.044,
    "Longitude": -75.6262
  },
  {
    "Date": "2026-12-27",
    "Location": "Hartsdale, NY",
    "Host": "For the Love of Dogs NY LLC",
    "EventLink": "https://www.fortheloveofdogsny.com/trials-events",
    "TrialTypes": "L2I, L3I, NW3, ELT",
    "EventCount": 4,
    "Latitude": 40.9766,
    "Longitude": -73.8201
  },
  {
    "Date": "2026-12-27",
    "Location": "Waukesha, WI",
    "Host": "K9 Ventures",
    "EventLink": "https://www.k9ventureswi.net/",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 43.0935,
    "Longitude": -88.2888
  },
  {
    "Date": "2026-12-28",
    "Location": "Tyngsborough, MA",
    "Host": "Spot On K9 Coaching",
    "EventLink": "https://www.sniffalertfinish.com/",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 42.6796,
    "Longitude": -71.4208
  },
  {
    "Date": "2026-12-29",
    "Location": "Duluth, GA",
    "Host": "Right Choice Dog Training, LLC",
    "EventLink": "https://www.rightchoicedogtraining.net/eventandvolunteer",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 34.02,
    "Longitude": -84.1172
  },
  {
    "Date": "2026-12-31",
    "Location": "Corvallis, OR",
    "Host": "PNW Sniffers",
    "EventLink": "https://pnwsniffers.com/nye-trial",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 44.5193,
    "Longitude": -123.2162
  },
  {
    "Date": "2027-01-01",
    "Location": "Emmitsburg, MD",
    "Host": "Red Huskies Nose Work, LLC",
    "EventLink": "https://nosework.redhuskies.com/",
    "TrialTypes": "ELT-P, ELT-S, ELT",
    "EventCount": 3,
    "Latitude": 39.7259,
    "Longitude": -77.327
  },
  {
    "Date": "2027-01-01",
    "Location": "Exton, PA",
    "Host": "Patricia Grassey",
    "EventLink": "https://www.thesniffinghound.com/",
    "TrialTypes": "NW2, ELT-P, NW1, ELT-S",
    "EventCount": 4,
    "Latitude": 40.0552,
    "Longitude": -75.6647
  },
  {
    "Date": "2027-01-01",
    "Location": "Santa Rosa, CA",
    "Host": "Marin Humane",
    "EventLink": "https://marinhumane.org/oh-behave/nose-work-events/",
    "TrialTypes": "NW3, SMT",
    "EventCount": 2,
    "Latitude": 38.4558,
    "Longitude": -122.7385
  },
  {
    "Date": "2027-01-02",
    "Location": "Bonsall, CA",
    "Host": "Linda Buchanan",
    "EventLink": "https://www.k9slovetosearch.com/",
    "TrialTypes": "NW2, ELT",
    "EventCount": 2,
    "Latitude": 33.3164,
    "Longitude": -117.223
  },
  {
    "Date": "2027-01-03",
    "Location": "Bee Cave, TX",
    "Host": "Scent Work Across Texas",
    "EventLink": "https://scentworkacrosstexas.com/nacsw-jan-3-4%2C-2027",
    "TrialTypes": "ELT-S, NW1, NW3",
    "EventCount": 3,
    "Latitude": 30.339,
    "Longitude": -97.9266
  },
  {
    "Date": "2027-01-08",
    "Location": "Hartfield, VA",
    "Host": "Paws Plus Training, LLC",
    "EventLink": "https://pawsplustraining.com/",
    "TrialTypes": "ELT, NW1, NW2",
    "EventCount": 3,
    "Latitude": 37.5531,
    "Longitude": -76.4277
  },
  {
    "Date": "2027-01-09",
    "Location": "Bellingham, WA",
    "Host": "The Nosework Magic",
    "EventLink": "https://www.noseworkmagic.com/orts-trials-other-events/nw2nw1-trials",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 48.7419,
    "Longitude": -122.4673
  },
  {
    "Date": "2027-01-09",
    "Location": "Cape Coral, FL",
    "Host": "Your Dog Knows LLC",
    "EventLink": "https://yourdogknows.net/upcoming-events",
    "TrialTypes": "NW1, NW2, L1I, L1C",
    "EventCount": 4,
    "Latitude": 26.5907,
    "Longitude": -81.97
  },
  {
    "Date": "2027-01-09",
    "Location": "Merced, CA",
    "Host": "Two Nosey Girls",
    "EventLink": "https://twonoseygirls.com/",
    "TrialTypes": "ELT-P, ELT",
    "EventCount": 2,
    "Latitude": 37.1204,
    "Longitude": -120.8055
  },
  {
    "Date": "2027-01-09",
    "Location": "Novato, CA",
    "Host": "Marin Humane",
    "EventLink": "https://marinhumane.org/oh-behave/nose-work-events/",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 38.0784,
    "Longitude": -122.5355
  },
  {
    "Date": "2027-01-15",
    "Location": "Greeley, CO",
    "Host": "Beyond Elevation K9 Training",
    "EventLink": "https://www.beyondelevationk9.com/jan-2027-greeley-premium",
    "TrialTypes": "NW1, NW2, L1I, L1C",
    "EventCount": 4,
    "Latitude": 40.4196,
    "Longitude": -104.7315
  },
  {
    "Date": "2027-01-15",
    "Location": "Upland, CA",
    "Host": "Agile Paws Dog Sports",
    "EventLink": "https://agilepawsdogsports.com/trials/january-2027-trial",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 34.0778,
    "Longitude": -117.6172
  },
  {
    "Date": "2027-01-16",
    "Location": "Bristol, PA",
    "Host": "Patricia Grassey",
    "EventLink": "https://thesniffinghound.com/about",
    "TrialTypes": "ELT-S, NW2, ELT, ELT-P",
    "EventCount": 4,
    "Latitude": 40.1426,
    "Longitude": -74.8395
  },
  {
    "Date": "2027-01-16",
    "Location": "Clanton, AL",
    "Host": "Daphne Melillo",
    "EventLink": "https://www.byanosenosework.com/event-details/clanton-al-nw2-elt",
    "TrialTypes": "NW2, ELT",
    "EventCount": 2,
    "Latitude": 32.8413,
    "Longitude": -86.6175
  },
  {
    "Date": "2027-01-16",
    "Location": "Havre De Grace, MD",
    "Host": "Firezone GS",
    "EventLink": "https://www.firezonegiantschnauzers.com/adas-shalom",
    "TrialTypes": "NW3, L2C, NW2, ELT",
    "EventCount": 4,
    "Latitude": 39.5767,
    "Longitude": -76.0504
  },
  {
    "Date": "2027-01-16",
    "Location": "Melrose, FL",
    "Host": "River Poodles Training, LLC",
    "EventLink": "https://riverpoodlestraining.com/",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 29.6724,
    "Longitude": -82.0862
  },
  {
    "Date": "2027-01-16",
    "Location": "Purchase, NY",
    "Host": "For the Love of Dogs NY LLC",
    "EventLink": "https://www.fortheloveofdogsny.com/",
    "TrialTypes": "SMT, NW3",
    "EventCount": 2,
    "Latitude": 41.0169,
    "Longitude": -73.755
  },
  {
    "Date": "2027-01-16",
    "Location": "Tecumseh, OK",
    "Host": "The Doggie Spot, LLC",
    "EventLink": "https://thedoggiespot.com/trials-and-orts/",
    "TrialTypes": "NW1, NW2, ELT",
    "EventCount": 3,
    "Latitude": 35.2262,
    "Longitude": -96.9656
  },
  {
    "Date": "2027-01-19",
    "Location": "Murfreesboro, TN",
    "Host": "Dogs Have Amazing Noses, LLC",
    "EventLink": "https://dogshaveamazingnoses.com/events/",
    "TrialTypes": "ELT, ELT-S",
    "EventCount": 2,
    "Latitude": 35.893,
    "Longitude": -86.371
  },
  {
    "Date": "2027-01-23",
    "Location": "Danielsville, GA",
    "Host": "Right Choice Dog Training, LLC",
    "EventLink": "https://www.rightchoicedogtraining.net/",
    "TrialTypes": "NW3, NW1, L2I",
    "EventCount": 3,
    "Latitude": 34.1156,
    "Longitude": -83.2696
  },
  {
    "Date": "2027-01-23",
    "Location": "Montgomery, TX",
    "Host": "Nosy Dogs Houston",
    "EventLink": "http://www.nosydogshouston.com/",
    "TrialTypes": "NW1, L1C, L1V, NW2",
    "EventCount": 4,
    "Latitude": 30.3343,
    "Longitude": -95.5042
  },
  {
    "Date": "2027-01-23",
    "Location": "Valencia, CA",
    "Host": "Pink Biscuit K9s",
    "EventLink": "https://www.pinkbiscuitk9s.com/",
    "TrialTypes": "ELT-P, L2I, L3C",
    "EventCount": 3,
    "Latitude": 34.4589,
    "Longitude": -118.5631
  },
  {
    "Date": "2027-01-30",
    "Location": "Cumming, GA",
    "Host": "Georgia Nosework LLC",
    "EventLink": "https://georgianosework.com/",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 34.2013,
    "Longitude": -84.1281
  },
  {
    "Date": "2027-01-30",
    "Location": "Durham, NC",
    "Host": "Whole Dog Institute",
    "EventLink": "https://wholedoginstitute.com/wdi_events/old-northern-high-school-durham-nc-smt/",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 36.0137,
    "Longitude": -78.8679
  },
  {
    "Date": "2027-01-30",
    "Location": "Las Vegas, NV",
    "Host": "imPETus Animal Training",
    "EventLink": "http://impetusanimaltraining.com/",
    "TrialTypes": "NW3, L1I, NW2",
    "EventCount": 3,
    "Latitude": 36.1815,
    "Longitude": -115.1984
  },
  {
    "Date": "2027-01-30",
    "Location": "Petaluma, CA",
    "Host": "Seaside Sniffers",
    "EventLink": "https://www.seasidesniffers.com/",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 38.2018,
    "Longitude": -122.6486
  },
  {
    "Date": "2027-01-30",
    "Location": "San Marcos, CA",
    "Host": "Rewarding Rover LLC, Uberdog, & Claire Brocato",
    "EventLink": "https://www.rewardingrover.com/",
    "TrialTypes": "ELT-S, NW3",
    "EventCount": 2,
    "Latitude": 33.153,
    "Longitude": -117.1449
  },
  {
    "Date": "2027-01-30",
    "Location": "Seguin, TX",
    "Host": "Sniff Happens",
    "EventLink": "https://www.sniffhappenstx.com/Jan-NACSW-Trial",
    "TrialTypes": "L2C, ELT-S, NW3",
    "EventCount": 3,
    "Latitude": 29.5819,
    "Longitude": -97.9523
  },
  {
    "Date": "2027-02-06",
    "Location": "Bel Air, MD",
    "Host": "Firezone GS, LLC",
    "EventLink": "https://www.firezonegiantschnauzers.com/nose-work-trials",
    "TrialTypes": "ELT, ELT-S, NW2",
    "EventCount": 3,
    "Latitude": 39.5318,
    "Longitude": -76.336
  },
  {
    "Date": "2027-02-06",
    "Location": "Murfreesboro, TN",
    "Host": "Dogs Have Amazing Noses, LLC",
    "EventLink": "https://dogshaveamazingnoses.com/events/",
    "TrialTypes": "NW2",
    "EventCount": 1,
    "Latitude": 35.8854,
    "Longitude": -86.3762
  },
  {
    "Date": "2027-02-12",
    "Location": "Easton and Lutherville, MD",
    "Host": "Fairplay Labradors",
    "EventLink": "https://www.fairplaylabradors.com/",
    "TrialTypes": "NW3, ELT-S, L3E, NW2, L1I",
    "EventCount": 5,
    "Latitude": 39.4256,
    "Longitude": -76.5794
  },
  {
    "Date": "2027-02-12",
    "Location": "Paso Robles, CA",
    "Host": "Diane Baley DBA All Fur Fun",
    "EventLink": "https://allfurfunk9.com/paso-robles",
    "TrialTypes": "NW3, ELT-S, NW1",
    "EventCount": 3,
    "Latitude": 35.6666,
    "Longitude": -120.7263
  },
  {
    "Date": "2027-02-13",
    "Location": "Auburn, AL",
    "Host": "By A Nose Nosework",
    "EventLink": "http://byanosenosework.com",
    "TrialTypes": "NW2, NW1",
    "EventCount": 2,
    "Latitude": 32.613,
    "Longitude": -85.4365
  },
  {
    "Date": "2027-02-13",
    "Location": "Flemington, NJ",
    "Host": "Shamrock Pot of Gold K9 Scenter",
    "EventLink": "https://shamrockpotofgoldk9scenter.com/",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 40.5326,
    "Longitude": -74.8654
  },
  {
    "Date": "2027-02-13",
    "Location": "Modesto, CA",
    "Host": "Two Nosey Girls",
    "EventLink": "https://twonoseygirls.com/",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 37.65,
    "Longitude": -120.978
  },
  {
    "Date": "2027-02-13",
    "Location": "New Rochelle, NY",
    "Host": "For the Love of Dogs NY LLC",
    "EventLink": "https://www.fortheloveofdogsny.com/nacsw-trials",
    "TrialTypes": "L1I, ELT-S, NW3, ELT",
    "EventCount": 4,
    "Latitude": 40.8902,
    "Longitude": -73.7977
  },
  {
    "Date": "2027-02-13",
    "Location": "Strafford, PA",
    "Host": "The Sniffing Hound",
    "EventLink": "https://www.thesniffinghound.com/",
    "TrialTypes": "ELT, ELT-P, NW2",
    "EventCount": 3,
    "Latitude": 40.0972,
    "Longitude": -75.4442
  },
  {
    "Date": "2027-02-19",
    "Location": "Vista, CA",
    "Host": "Rewarding Rover LLC, Uber Dog and Claire Brocato",
    "EventLink": "https://www.rewardingrover.com/",
    "TrialTypes": "NW3, ELT-S, NW2",
    "EventCount": 3,
    "Latitude": 33.1653,
    "Longitude": -117.2282
  },
  {
    "Date": "2027-02-20",
    "Location": "Clarkesville, GA",
    "Host": "Right Choice Dog Training, LLC",
    "EventLink": "https://www.rightchoicedogtraining.net/eventandvolunteer",
    "TrialTypes": "L3I, NW2, ELT",
    "EventCount": 3,
    "Latitude": 34.5868,
    "Longitude": -83.514
  },
  {
    "Date": "2027-02-21",
    "Location": "Benson, AZ",
    "Host": "Patience Unlimited Dog Training",
    "EventLink": "http://www.patienceunlimited.com/nacsw.html",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 32.0061,
    "Longitude": -110.3421
  },
  {
    "Date": "2027-02-22",
    "Location": "Paso Robles, CA",
    "Host": "Gentle Touch Pet Training",
    "EventLink": "https://www.gentlepets.com/gtpt-events/nacsw%E2%84%A2-elt%2Felt-trials",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 35.6396,
    "Longitude": -120.7069
  },
  {
    "Date": "2027-02-26",
    "Location": "McKinney, TX",
    "Host": "All About The Nose",
    "EventLink": "https://www.allaboutthenose.com/trials#nacsw-l1c-l1i-nw1-nw2",
    "TrialTypes": "L1C, L1I, NW2, NW1",
    "EventCount": 4,
    "Latitude": 33.2299,
    "Longitude": -96.5894
  },
  {
    "Date": "2027-02-26",
    "Location": "Westlake Village, CA",
    "Host": "JavaK9s, LLC",
    "EventLink": "http://www.javak9s.com/",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 34.1674,
    "Longitude": -118.7728
  },
  {
    "Date": "2027-02-27",
    "Location": "Brooksville, FL",
    "Host": "Hoppin’ in the Hills",
    "EventLink": "https://hoppininthehillscom.wordpress.com/trial-february-27-28-2027-nw3-elt-brooksville/",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 28.5101,
    "Longitude": -82.4156
  },
  {
    "Date": "2027-02-27",
    "Location": "Wilson, NC",
    "Host": "Whole Dog Institute, LLC",
    "EventLink": "https://wholedoginstitute.com/",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 35.6649,
    "Longitude": -77.8778
  },
  {
    "Date": "2027-03-05",
    "Location": "Glen Mills, PA",
    "Host": "Firezone GS",
    "EventLink": "https://www.firezonegiantschnauzers.com/nose-work-trials",
    "TrialTypes": "SMT, NW3",
    "EventCount": 2,
    "Latitude": 39.9282,
    "Longitude": -75.461
  },
  {
    "Date": "2027-03-06",
    "Location": "Bristol, CT",
    "Host": "For the Love of Dogs NY LLC",
    "EventLink": "https://www.fortheloveofdogsny.com/trials-events",
    "TrialTypes": "NW3, ELT, ELT-P",
    "EventCount": 3,
    "Latitude": 41.696,
    "Longitude": -72.9729
  },
  {
    "Date": "2027-03-06",
    "Location": "Keystone Heights, FL",
    "Host": "River Poodles Training, LLC",
    "EventLink": "https://riverpoodlestraining.com/",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 29.8236,
    "Longitude": -81.9943
  },
  {
    "Date": "2027-03-06",
    "Location": "Santa Paula, CA",
    "Host": "Pink Biscuit K9s",
    "EventLink": "https://www.pinkbiscuitk9s.com/",
    "TrialTypes": "NW1, NW2, ELT",
    "EventCount": 3,
    "Latitude": 34.3948,
    "Longitude": -119.0183
  },
  {
    "Date": "2027-03-08",
    "Location": "Glendora, CA",
    "Host": "Agile Paws Dog Sports",
    "EventLink": "https://agilepawsdogsports.com/",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 34.0922,
    "Longitude": -117.9015
  },
  {
    "Date": "2027-03-11",
    "Location": "Lakeville, MN",
    "Host": "St. Paul Dog Training Club",
    "EventLink": "https://spdtc.com/events-at-spdtc/",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 44.6327,
    "Longitude": -93.2581
  },
  {
    "Date": "2027-03-12",
    "Location": "Stokesdale, NC",
    "Host": "K9 Nose Adventures, LLC",
    "EventLink": "https://www.doggonefunnc.com/",
    "TrialTypes": "ELT-S, NW1, NW2",
    "EventCount": 3,
    "Latitude": 36.1914,
    "Longitude": -80.0224
  },
  {
    "Date": "2027-03-13",
    "Location": "Boca Raton, FL",
    "Host": "West Palm Dog, LLC",
    "EventLink": "https://palmbeachdogtraining.com/nacsw",
    "TrialTypes": "NW1, NW2, L1C, L1I",
    "EventCount": 4,
    "Latitude": 26.3213,
    "Longitude": -80.0332
  },
  {
    "Date": "2027-03-13",
    "Location": "Boone, IA",
    "Host": "KBP Dog Training",
    "EventLink": "https://kbpdogtraining.com/",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 42.0462,
    "Longitude": -93.9402
  },
  {
    "Date": "2027-03-13",
    "Location": "Hammond, LA",
    "Host": "Dog Gone Right",
    "EventLink": "https://doggoneright.net/k9-nose-work-trialsorts-html/",
    "TrialTypes": "NW1, NW2, NW3",
    "EventCount": 3,
    "Latitude": 30.554,
    "Longitude": -90.4263
  },
  {
    "Date": "2027-03-13",
    "Location": "Kent, WA",
    "Host": "Northwest K9 Sniffers",
    "EventLink": "https://nwk9sniffers.org/",
    "TrialTypes": "ELT-P, L1V, L1I",
    "EventCount": 3,
    "Latitude": 47.3352,
    "Longitude": -122.2223
  },
  {
    "Date": "2027-03-13",
    "Location": "Phoenix, AZ",
    "Host": "Successful Sniffer",
    "EventLink": "https://www.successfulsniffer.com/",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 33.4854,
    "Longitude": -112.0936
  },
  {
    "Date": "2027-03-13",
    "Location": "Urbana, IL",
    "Host": "Kudos for Canines, LLC",
    "EventLink": "https://kudosforcanines.com/",
    "TrialTypes": "NW1, NW3",
    "EventCount": 2,
    "Latitude": 40.0792,
    "Longitude": -88.2304
  },
  {
    "Date": "2027-03-13",
    "Location": "Wilmington, DE",
    "Host": "The Sniffing Hound",
    "EventLink": "https://www.thesniffinghound.com/",
    "TrialTypes": "ELT, ELT-P, NW1, ELT-S",
    "EventCount": 4,
    "Latitude": 39.6977,
    "Longitude": -75.5321
  },
  {
    "Date": "2027-03-15",
    "Location": "Riverside, CA",
    "Host": "Linda Buchanan",
    "EventLink": "https://www.k9slovetosearch.com/",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 33.9901,
    "Longitude": -117.4168
  },
  {
    "Date": "2027-03-19",
    "Location": "Honesdale, PA",
    "Host": "Your Dog’s Place, LLC",
    "EventLink": "http://www.yourdogsplace.com/",
    "TrialTypes": "L2C, ELT-P, ELT-S, L1I",
    "EventCount": 4,
    "Latitude": 41.5758,
    "Longitude": -75.2275
  },
  {
    "Date": "2027-03-19",
    "Location": "Moline, IL",
    "Host": "Fur Better Fur Worse, LLC",
    "EventLink": "https://www.furbetterfurworse.com/",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 41.5059,
    "Longitude": -90.5625
  },
  {
    "Date": "2027-03-19",
    "Location": "Watertown, TN",
    "Host": "Dogs Have Amazing Noses, LLC",
    "EventLink": "https://dogshaveamazingnoses.com/events/",
    "TrialTypes": "ELT, NW1",
    "EventCount": 2,
    "Latitude": 36.1236,
    "Longitude": -86.1576
  },
  {
    "Date": "2027-03-20",
    "Location": "Foxboro, MA",
    "Host": "Bay State Sniffers",
    "EventLink": "http://www.baystatesniffers.com/about.html",
    "TrialTypes": "NW1, L1C, L2I",
    "EventCount": 3,
    "Latitude": 42.0503,
    "Longitude": -71.251
  },
  {
    "Date": "2027-03-20",
    "Location": "Opelika, AL",
    "Host": "By A Nose Nosework",
    "EventLink": "https://www.byanosenosework.com/",
    "TrialTypes": "ELT",
    "EventCount": 1,
    "Latitude": 32.6662,
    "Longitude": -85.4199
  },
  {
    "Date": "2027-03-20",
    "Location": "Redwood City, CA",
    "Host": "B. L. McMutts, LLC",
    "EventLink": "https://blmcmutts.com/events/nacsw-nosework-trial-march2027",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 37.4556,
    "Longitude": -122.1851
  },
  {
    "Date": "2027-03-20",
    "Location": "Selma, TX",
    "Host": "Sniff Happens",
    "EventLink": "https://www.sniffhappenstx.com/march-2027-nacsw-trial",
    "TrialTypes": "L1E, ELT-S, ELT",
    "EventCount": 3,
    "Latitude": 29.5415,
    "Longitude": -98.3246
  },
  {
    "Date": "2027-03-20",
    "Location": "Sheridan, OR",
    "Host": "Doglandia LLC (Kristina Leipzig) and Just Nose Work (Carol Forsberg)",
    "EventLink": "https://www.justnosework.com/",
    "TrialTypes": "ELT, NW2",
    "EventCount": 2,
    "Latitude": 45.0702,
    "Longitude": -123.3994
  },
  {
    "Date": "2027-03-20",
    "Location": "Troy, MO",
    "Host": "Happy Dog Concepts LLC",
    "EventLink": "https://happydogconcepts.com/events",
    "TrialTypes": "ELT, NW3",
    "EventCount": 2,
    "Latitude": 38.9617,
    "Longitude": -90.9397
  },
  {
    "Date": "2027-03-25",
    "Location": "Chester Springs, PA",
    "Host": "The Sniffing Hound",
    "EventLink": "https://www.thesniffinghound.com/",
    "TrialTypes": "NW3, ELT-S, NW2, ELT",
    "EventCount": 4,
    "Latitude": 40.0765,
    "Longitude": -75.6618
  },
  {
    "Date": "2027-03-25",
    "Location": "Rochester, NY",
    "Host": "2 Psyched 4 Dogs",
    "EventLink": "https://2psyched4dogs.com/",
    "TrialTypes": "NW1, NW3, ELT",
    "EventCount": 3,
    "Latitude": 43.1251,
    "Longitude": -77.6174
  },
  {
    "Date": "2027-03-26",
    "Location": "Albuquerque, NM",
    "Host": "New Mexico Canine Scent Work, LLC",
    "EventLink": "https://www.nmcsw.com/",
    "TrialTypes": "ELT, NW3, L2I, NW1",
    "EventCount": 4,
    "Latitude": 35.1036,
    "Longitude": -106.679
  },
  {
    "Date": "2027-03-26",
    "Location": "Bensalem, PA",
    "Host": "For the Love of Dogs NY LLC",
    "EventLink": "https://www.fortheloveofdogsny.com/",
    "TrialTypes": "SMT, NW2",
    "EventCount": 2,
    "Latitude": 40.1135,
    "Longitude": -74.945
  },
  {
    "Date": "2027-03-26",
    "Location": "Grand Junction, CO",
    "Host": "Mountain Dogs, LLC",
    "EventLink": "http://mountaindogs.org",
    "TrialTypes": "ELT, NW3, NW1, NW2",
    "EventCount": 4,
    "Latitude": 39.067,
    "Longitude": -108.5221
  },
  {
    "Date": "2027-03-27",
    "Location": "Elkridge, MD",
    "Host": "Red Huskies Nose Work, LLC",
    "EventLink": "https://nosework.redhuskies.com/",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 39.212,
    "Longitude": -76.6952
  },
  {
    "Date": "2027-03-27",
    "Location": "Groveport, OH",
    "Host": "CleverDogs",
    "EventLink": "https://cleverdogsohio.com/",
    "TrialTypes": "NW1, NW2, NW3",
    "EventCount": 3,
    "Latitude": 39.8721,
    "Longitude": -82.8687
  },
  {
    "Date": "2027-03-27",
    "Location": "Woodstock, GA",
    "Host": "Georgia Nosework, LLC",
    "EventLink": "https://georgianosework.com/",
    "TrialTypes": "NW3, L1C, L1I",
    "EventCount": 3,
    "Latitude": 34.1141,
    "Longitude": -84.5242
  },
  {
    "Date": "2027-04-02",
    "Location": "Forest Lake, MN",
    "Host": "St. Paul Dog Training Club",
    "EventLink": "https://spdtc.com/events-at-spdtc/",
    "TrialTypes": "ELT, NW2, NW1",
    "EventCount": 3,
    "Latitude": 45.2314,
    "Longitude": -92.9911
  },
  {
    "Date": "2027-04-02",
    "Location": "Rapid City, SD",
    "Host": "Two Paws Up Dog Training, LLC",
    "EventLink": "https://www.twopawsupdogtrainingllc.com/",
    "TrialTypes": "ELT, NW3, NW2, NW1",
    "EventCount": 4,
    "Latitude": 44.0625,
    "Longitude": -103.1868
  },
  {
    "Date": "2027-04-03",
    "Location": "Batavia, OH",
    "Host": "Clermont County Dog Training Club",
    "EventLink": "https://www.ccdtc.org/",
    "TrialTypes": "NW1, NW2, L1C, L1I",
    "EventCount": 4,
    "Latitude": 39.0955,
    "Longitude": -84.1791
  },
  {
    "Date": "2027-04-03",
    "Location": "Keystone Heights, FL",
    "Host": "River Poodles Training, LLC",
    "EventLink": "https://riverpoodlestraining.com/n-fl-christian-apr-3-4",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 29.7533,
    "Longitude": -82.0081
  },
  {
    "Date": "2027-04-03",
    "Location": "Pittsburgh, PA",
    "Host": "Steel City Nosework, LLC",
    "EventLink": "https://www.nose-it-all.com/",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 40.4653,
    "Longitude": -80.021
  },
  {
    "Date": "2027-04-03",
    "Location": "Rome, GA",
    "Host": "Southeast Scent Work Alliance, LLC (SSWA)",
    "EventLink": "https://southeastscent.com/events",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 34.271,
    "Longitude": -85.2057
  },
  {
    "Date": "2027-04-03",
    "Location": "Somis, CA",
    "Host": "Pink Biscuit K9s",
    "EventLink": "https://www.pinkbiscuitk9s.com/",
    "TrialTypes": "NW1, NW2, ELT-S, L2I",
    "EventCount": 4,
    "Latitude": 34.2346,
    "Longitude": -118.9948
  },
  {
    "Date": "2027-04-03",
    "Location": "Tucson, AZ",
    "Host": "Patience Unlimited Dog Training",
    "EventLink": "http://www.patienceunlimited.com/",
    "TrialTypes": "NW3, NW1, NW2",
    "EventCount": 3,
    "Latitude": 32.1774,
    "Longitude": -110.9839
  },
  {
    "Date": "2027-04-03",
    "Location": "Wilmot, WI",
    "Host": "SuperDog Industries LLC",
    "EventLink": "https://www.superdogevents.com/",
    "TrialTypes": "NW2, ELT",
    "EventCount": 2,
    "Latitude": 42.5064,
    "Longitude": -88.1856
  },
  {
    "Date": "2027-04-03",
    "Location": "Woodstock, IL",
    "Host": "Northwest Obedience Club, Inc.",
    "EventLink": "https://northwestobedienceclub.org/",
    "TrialTypes": "NW3, ELT-P",
    "EventCount": 2,
    "Latitude": 42.3445,
    "Longitude": -88.4746
  },
  {
    "Date": "2027-04-05",
    "Location": "Chester, NY",
    "Host": "For the Love of Dogs NY LLC",
    "EventLink": "https://www.fortheloveofdogsny.com/trials-events",
    "TrialTypes": "NW2, ELT",
    "EventCount": 2,
    "Latitude": 41.3457,
    "Longitude": -74.2751
  },
  {
    "Date": "2027-04-07",
    "Location": "Olympia, WA",
    "Host": "Let's Talk Dogs, LLC & About Face K9 Academy",
    "EventLink": "https://www.aboutfacek9academy.com",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 47.046,
    "Longitude": -122.9314
  },
  {
    "Date": "2027-04-09",
    "Location": "Shady Hills, FL",
    "Host": "Hoppin’ in the Hills",
    "EventLink": "https://hoppininthehillscom.wordpress.com/",
    "TrialTypes": "NW1, L1C, NW2",
    "EventCount": 3,
    "Latitude": 28.3501,
    "Longitude": -82.5062
  },
  {
    "Date": "2027-04-10",
    "Location": "Northfield, OH",
    "Host": "Nosework Addicts, LLC",
    "EventLink": "https://www.noseworkaddictsllc.com/",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 41.3117,
    "Longitude": -81.5619
  },
  {
    "Date": "2027-04-10",
    "Location": "Red Feather Lakes, CO",
    "Host": "Beyond Elevation K9",
    "EventLink": "https://www.beyondelevationk9.com/",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 40.8549,
    "Longitude": -105.573
  },
  {
    "Date": "2027-04-10",
    "Location": "Winterset, IA",
    "Host": "KBP Dog Training",
    "EventLink": "https://kbpdogtraining.com/",
    "TrialTypes": "NW2, ELT",
    "EventCount": 2,
    "Latitude": 41.3731,
    "Longitude": -94.0146
  },
  {
    "Date": "2027-04-16",
    "Location": "Laramie, WY",
    "Host": "Mountain Dogs, LLC",
    "EventLink": "https://mountaindogs.org/",
    "TrialTypes": "ELT-S, NW2, ELT-P, NW3",
    "EventCount": 4,
    "Latitude": 41.3131,
    "Longitude": -105.6402
  },
  {
    "Date": "2027-04-16",
    "Location": "Upland, CA",
    "Host": "Agile Paws Dog Sports",
    "EventLink": "https://agilepawsdogsports.com/",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 34.1092,
    "Longitude": -117.6456
  },
  {
    "Date": "2027-04-17",
    "Location": "Columbus, MT",
    "Host": "Canine Connection, LLC",
    "EventLink": "https://canineconnection23.godaddysites.com/",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 45.6616,
    "Longitude": -109.2694
  },
  {
    "Date": "2027-04-17",
    "Location": "Glenwood, NY",
    "Host": "Do Over Dog Training",
    "EventLink": "https://www.dooverdogtraining.com/trials",
    "TrialTypes": "ELT-S, L2E, L2C, L1I",
    "EventCount": 4,
    "Latitude": 42.6497,
    "Longitude": -78.6414
  },
  {
    "Date": "2027-04-17",
    "Location": "Kunkletown, PA",
    "Host": "Paws n' Sniff",
    "EventLink": "http://www.pawsnsniff.com/",
    "TrialTypes": "ELT-S, NW2, ELT",
    "EventCount": 3,
    "Latitude": 40.8531,
    "Longitude": -75.4761
  },
  {
    "Date": "2027-04-19",
    "Location": "Paso Robles, CA",
    "Host": "Gentle Touch Pet Training",
    "EventLink": "https://www.gentlepets.com/gtpt-events/nacsw%E2%84%A2-elt-sl2c-trials",
    "TrialTypes": "ELT-S, L2C",
    "EventCount": 2,
    "Latitude": 35.6586,
    "Longitude": -120.7261
  },
  {
    "Date": "2027-04-24",
    "Location": "Elkhorn, WI",
    "Host": "Loving Paws Dog Training, LLC",
    "EventLink": "https://www.lovingpawsllc.com/k9nw-trials",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 42.6231,
    "Longitude": -88.5757
  },
  {
    "Date": "2027-04-24",
    "Location": "Ellicottville, NY",
    "Host": "Do Over Dog Training",
    "EventLink": "https://www.dooverdogtraining.com/trials",
    "TrialTypes": "NW3, NW2",
    "EventCount": 2,
    "Latitude": 42.2999,
    "Longitude": -78.647
  },
  {
    "Date": "2027-04-30",
    "Location": "Nyack, NY",
    "Host": "Waggin' Work",
    "EventLink": "https://www.wagginwork.com/",
    "TrialTypes": "NW3, NW2, L2I, NW1, L2C",
    "EventCount": 5,
    "Latitude": 41.1124,
    "Longitude": -73.9632
  },
  {
    "Date": "2027-05-01",
    "Location": "Blue Ridge, VA",
    "Host": "Canny K9 Companions, LLC",
    "EventLink": "https://www.cannyk9companions.com/",
    "TrialTypes": "NW3, NW2, NW1",
    "EventCount": 3,
    "Latitude": 37.4279,
    "Longitude": -79.8051
  },
  {
    "Date": "2027-05-01",
    "Location": "Ft Morgan, CO",
    "Host": "Paws 4 Thought Dog Training, LLC",
    "EventLink": "https://www.p4tnosework.com/",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 40.2274,
    "Longitude": -103.7892
  },
  {
    "Date": "2027-05-01",
    "Location": "Manhattan , MT",
    "Host": "Trails and Tails Dog School",
    "EventLink": "https://www.trailsandtailsdogschool.com/events",
    "TrialTypes": "NW1, NW2, NW3",
    "EventCount": 3,
    "Latitude": 45.8504,
    "Longitude": -111.367
  },
  {
    "Date": "2027-05-01",
    "Location": "Poland Springs, ME",
    "Host": "Bare Bones Nosework, LLC",
    "EventLink": "https://virginiahowe.com/",
    "TrialTypes": "L2I, L3I",
    "EventCount": 2,
    "Latitude": 44.0091,
    "Longitude": -70.3182
  },
  {
    "Date": "2027-05-01",
    "Location": "Santa Paula, CA",
    "Host": "Pink Biscuit K9s",
    "EventLink": "https://www.pinkbiscuitk9s.com/",
    "TrialTypes": "NW1, NW2, ELT",
    "EventCount": 3,
    "Latitude": 34.373,
    "Longitude": -119.06
  },
  {
    "Date": "2027-05-01",
    "Location": "Sharon, MA",
    "Host": "Bay State Sniffers",
    "EventLink": "http://www.baystatesniffers.com/",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 42.0849,
    "Longitude": -71.1293
  },
  {
    "Date": "2027-05-07",
    "Location": "Beaver Dam, WI",
    "Host": "On Point Elite Dog Sports, LLC",
    "EventLink": "https://www.opedogsports.com/nacsw-trials",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 43.4561,
    "Longitude": -88.8339
  },
  {
    "Date": "2027-05-07",
    "Location": "South Sterling, PA",
    "Host": "Paws n' Sniff",
    "EventLink": "http://www.pawsnsniff.com/",
    "TrialTypes": "ELT-S, NW1, NW3",
    "EventCount": 3,
    "Latitude": 41.2223,
    "Longitude": -75.3004
  },
  {
    "Date": "2027-05-07",
    "Location": "Wrightwood, CA",
    "Host": "JavaK9s, LLC",
    "EventLink": "https://www.javak9s.com/",
    "TrialTypes": "ELT, L1C, L1I",
    "EventCount": 3,
    "Latitude": 34.3822,
    "Longitude": -117.5962
  },
  {
    "Date": "2027-05-08",
    "Location": "Ashby, MA",
    "Host": "Carolyn Barney dba Dogs!",
    "EventLink": "https://carolynbarney.com/",
    "TrialTypes": "NW1, NW2",
    "EventCount": 2,
    "Latitude": 42.6331,
    "Longitude": -71.8029
  },
  {
    "Date": "2027-05-08",
    "Location": "Tecumseh, OK",
    "Host": "The Doggie Spot",
    "EventLink": "https://thedoggiespot.com/",
    "TrialTypes": "NW3, NW1",
    "EventCount": 2,
    "Latitude": 35.2979,
    "Longitude": -96.9485
  },
  {
    "Date": "2027-05-15",
    "Location": "Forest Junction, WI",
    "Host": "NEW K9Scent Work LLC",
    "EventLink": "https://newk9scentwork.com/",
    "TrialTypes": "NW3, NW1, L2C",
    "EventCount": 3,
    "Latitude": 44.2091,
    "Longitude": -88.157
  },
  {
    "Date": "2027-05-15",
    "Location": "Monticello, NY",
    "Host": "Saints2Source, LLC",
    "EventLink": "https://www.saints2source.com/",
    "TrialTypes": "NW2, NW1, L1I, ELT-P",
    "EventCount": 4,
    "Latitude": 41.6494,
    "Longitude": -74.6973
  },
  {
    "Date": "2027-05-15",
    "Location": "Shawnee, OK",
    "Host": "The Doggie Spot, LLC",
    "EventLink": "https://thedoggiespot.com/",
    "TrialTypes": "NW3, NW1",
    "EventCount": 2,
    "Latitude": 35.279,
    "Longitude": -96.9537
  },
  {
    "Date": "2027-05-22",
    "Location": "Malvern, IA",
    "Host": "Two Dowells Inc",
    "EventLink": "https://sites.google.com/view/two-tails-unlimited",
    "TrialTypes": "NW3, NW1, L1C",
    "EventCount": 3,
    "Latitude": 40.9561,
    "Longitude": -95.5905
  },
  {
    "Date": "2027-05-29",
    "Location": "Atherton, CA",
    "Host": "B.L. McMutts LLC",
    "EventLink": "https://blmcmutts.com/events",
    "TrialTypes": "NW3",
    "EventCount": 1,
    "Latitude": 37.4195,
    "Longitude": -122.2003
  },
  {
    "Date": "2027-05-29",
    "Location": "Canandaigua, NY",
    "Host": "Savvy Dog Sports",
    "EventLink": "https://savvydogsports.com/trials/",
    "TrialTypes": "NW3, L2I, NW1",
    "EventCount": 3,
    "Latitude": 42.8016,
    "Longitude": -77.3229
  },
  {
    "Date": "2027-06-05",
    "Location": "Amherst, NY",
    "Host": "Do Over Dog Training",
    "EventLink": "https://www.dooverdogtraining.com/trials",
    "TrialTypes": "ELT-S, L3I, ELT",
    "EventCount": 3,
    "Latitude": 42.9692,
    "Longitude": -78.8145
  },
  {
    "Date": "2027-06-05",
    "Location": "Dunmore, PA",
    "Host": "Your Dog's Place, LLC",
    "EventLink": "http://www.yourdogsplace.com/",
    "TrialTypes": "ELT-P, ELT-S, L3C",
    "EventCount": 3,
    "Latitude": 41.4413,
    "Longitude": -75.6332
  },
  {
    "Date": "2027-06-05",
    "Location": "Enterprise, OR",
    "Host": "Country K9 Nosework, LLC",
    "EventLink": "https://www.countryk9nosework.com/",
    "TrialTypes": "NW2, ELT-P",
    "EventCount": 2,
    "Latitude": 45.4424,
    "Longitude": -117.2756
  },
  {
    "Date": "2027-06-05",
    "Location": "Greenville, PA",
    "Host": "Nosework Addicts, LLC",
    "EventLink": "https://www.noseworkaddictsllc.com/",
    "TrialTypes": "SMT",
    "EventCount": 1,
    "Latitude": 41.4211,
    "Longitude": -80.4108
  },
  {
    "Date": "2027-06-12",
    "Location": "Portland, OR",
    "Host": "Trust Your Dog K9 Events",
    "EventLink": "https://trustyourdogk9events.com/",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 45.5288,
    "Longitude": -122.705
  },
  {
    "Date": "2027-06-19",
    "Location": "Spring Grove, IL",
    "Host": "SuperDog Industries LLC",
    "EventLink": "https://www.superdogevents.com/",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 42.4116,
    "Longitude": -88.212
  },
  {
    "Date": "2027-06-26",
    "Location": "Delran, NJ",
    "Host": "K9 InScentives",
    "EventLink": "https://www.k9inscentives.com/",
    "TrialTypes": "NW3, NW1, L2I",
    "EventCount": 3,
    "Latitude": 39.9948,
    "Longitude": -74.994
  },
  {
    "Date": "2027-06-26",
    "Location": "Inver Grove Heights, MN",
    "Host": "Outside the Box Dog Training, LLC",
    "EventLink": "https://www.outsidetheboxdogtraining.com/events",
    "TrialTypes": "ELT, NW2",
    "EventCount": 2,
    "Latitude": 44.8605,
    "Longitude": -93.0056
  },
  {
    "Date": "2027-06-29",
    "Location": "Delran, NJ",
    "Host": "Ev-ry earthdog LLC",
    "EventLink": "https://ev-ryearthdog.com/",
    "TrialTypes": "ELT-P, NW2, NW1, L1I, L2C",
    "EventCount": 5,
    "Latitude": 39.9668,
    "Longitude": -75.0039
  },
  {
    "Date": "2027-07-02",
    "Location": "Huntington, MA",
    "Host": "Shamrock Pot of Gold K9 Scenter",
    "EventLink": "https://shamrockpotofgoldk9scenter.com/",
    "TrialTypes": "ELT-P, NW3, ELT-S, NW2",
    "EventCount": 4,
    "Latitude": 42.2066,
    "Longitude": -72.8659
  },
  {
    "Date": "2027-08-21",
    "Location": "Deckers, CO",
    "Host": "Mountain Dogs, LLC",
    "EventLink": "http://mountaindogs.org",
    "TrialTypes": "NW3, NW2, NW1, L1C",
    "EventCount": 4,
    "Latitude": 39.2408,
    "Longitude": -105.2193
  },
  {
    "Date": "2027-09-11",
    "Location": "Bantam, CT",
    "Host": "Waggin' Work",
    "EventLink": "https://www.wagginwork.com/",
    "TrialTypes": "NW3, ELT, ELT-P",
    "EventCount": 3,
    "Latitude": 41.7743,
    "Longitude": -73.2115
  },
  {
    "Date": "2027-09-17",
    "Location": "New Milford, PA",
    "Host": "Your Dog's Place, LLC",
    "EventLink": "http://www.yourdogsplace.com/",
    "TrialTypes": "ELT, L2V, ELT-S, EST",
    "EventCount": 4,
    "Latitude": 41.8319,
    "Longitude": -75.6808
  },
  {
    "Date": "2027-09-25",
    "Location": "Eastlake, OH",
    "Host": "Nosework Addicts, LLC",
    "EventLink": "https://www.noseworkaddictsllc.com/",
    "TrialTypes": "ELT, ELT-P",
    "EventCount": 2,
    "Latitude": 41.6645,
    "Longitude": -81.4217
  },
  {
    "Date": "2027-10-29",
    "Location": "Palmyra, VA",
    "Host": "Paws Plus Training, LLC",
    "EventLink": "https://pawsplustraining.com/",
    "TrialTypes": "NW3, NW1, NW2, ELT",
    "EventCount": 4,
    "Latitude": 37.9016,
    "Longitude": -78.282
  },
  {
    "Date": "2027-10-30",
    "Location": "Smithton, PA",
    "Host": "Steel City Nosework, LLC",
    "EventLink": "https://www.nose-it-all.com/",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 40.1657,
    "Longitude": -79.7901
  },
  {
    "Date": "2027-11-27",
    "Location": "Lebanon, NJ",
    "Host": "Sirius K9 Solutions",
    "EventLink": "http://www.siriusk9solutions.net/",
    "TrialTypes": "NW3, ELT",
    "EventCount": 2,
    "Latitude": 40.6882,
    "Longitude": -74.8202
  },
  {
    "Date": "2027-11-27",
    "Location": "New Braunfels, TX",
    "Host": "Sniff Happens",
    "EventLink": "https://www.sniffhappenstx.com/",
    "TrialTypes": "L1E, L2E, NW3",
    "EventCount": 3,
    "Latitude": 29.7348,
    "Longitude": -98.112
  }
]
;
