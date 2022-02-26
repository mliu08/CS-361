#
# OSU CS361: Intro to Software Development 1
# Historical PM2.5 Data
# Winter 2022
#
# Author: Maggie Liu
# Version: 1.0.0
# Description: Provides historical PM2.5 data from the AQICN database on dbnomics for the requested city, as long as that city has data in the database.
#

import pandas
import dbnomics
from geopy import geocoders


# Get location request from a text file. Expects a zip code.
with open('historic_aqi.txt', 'r', encoding='UTF8') as infile:
    location = infile.readline()

# Convert to city name in lowercase. 
locator = geocoders.Nominatim(user_agent="historic_aqi_app")    
city_county = locator.geocode(location, country_codes="us").address             # geocode returns City, County, State, Country. Only want city.
city = city_county.split(",")[0].lower()      

infile.close()

# validate city against the 57 in the database
db_cities = []
with open('cities.txt', 'r', encoding='UTF8') as infile:
    db_cities = infile.read().splitlines()

if city.capitalize() in db_cities:
    fetch_code = 'AQICN/AQI/US.' + city + '.pm25.median'

    # Acquire data and remove unnecessary columns
    df = dbnomics.fetch_series(fetch_code)
    df = df[["original_period", "original_value"]]

    # Convert df to csv and write
    df = pandas.DataFrame.to_csv(df, line_terminator='\r')
else:
    # Write an empty csv if the request was not in the database
    df = ""

with open('pm25py.csv','w') as outfile:
        outfile.write(df)
outfile.close()
