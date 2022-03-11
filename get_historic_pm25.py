#
# OSU CS361: Intro to Software Development 1
# Historical PM2.5 Data
# Winter 2022
#
# Author: Maggie Liu
# Version: 1.1.0
# Description: Provides historical PM2.5 data from the AQICN database on dbnomics for the requested city, as long as that city has data in the 57-city database.
#

import json
import pandas
import dbnomics
from geopy import geocoders


def fetch_data(city: str):
    """ 
    Takes a string city name and fetches the median PM2.5 air quality data associated 
    with that city from AQICN. Writes the data to csv.
    """
    fetch_code = 'AQICN/AQI/US.' + db_cities[city] + '.pm25.median'        
    df = dbnomics.fetch_series(fetch_code)
    df = df[["original_period", "original_value"]]                      # remove unnecessary columns

    # Convert df to csv and write
    df = pandas.DataFrame.to_csv(df, line_terminator='\r')
    with open('pm25py.csv','w') as outfile:
        outfile.write(df)
    outfile.close()


if __name__ == '__main__':
    
    # Get location request from a text file. Could be a zip code or US city.
    with open('historic_aqi.txt', 'r', encoding='UTF8') as infile:
        location = infile.readline()
    infile.close()

    # Convert input to city name
    locator = geocoders.Nominatim(user_agent="historic_aqi_app") 
    search =  locator.geocode(location, country_codes="us")

    # Only continue if the text provided was a real place
    if search is not None:
        city = search.address.split(", ")                                                                   
        with open('cities.json', 'r', encoding='UTF8') as infile:
            db_cities = json.load(infile)
        infile.close()

        # Fetch and write data if the city is in the database  
        if city[0] in db_cities:
            fetch_data(city[0])  
        elif city[1] in db_cities:                                  # First result could be a neighborhood so check first two
            fetch_data(city[1])
        else:
            # Write an an error message to the request file if the request was not in the database
            with open('historic_aqi.txt', 'w') as outfile:
                outfile.write("Location not found.")
            outfile.close()

    else:
        # Write an an error message to the request file if the request was not a city
        with open('historic_aqi.txt', 'w') as outfile:
            outfile.write("Entry not recognized. Please check for typos.")
        outfile.close()
    
