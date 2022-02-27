#
# OSU CS361: Intro to Software Development 1
# Air Quality Index Microservice
# Winter 2022
#
# Author: Maggie Liu
# Version: 1.0.0
# Description: A microservice to return current air quality information from a supplied location string.
#

from geopy import geocoders
import requests


# Get location request from a text file
# Can be a city, postal code, or combination
with open('current_aqi.txt', 'r') as infile:
    location = infile.readline()

# Covert supplied location to lat/long
locator = geocoders.Nominatim(user_agent="aqi_microservice")
city = locator.geocode(location)
latitude = city.latitude
longitude = city.longitude

# Get AQI data from Open Weather Map
open_weather_map_key = "9874f807c9c65eed1d03fcf8fc332a6f"
url = "http://api.openweathermap.org/data/2.5/air_pollution?lat=" + str(latitude) + "&lon=" + str(longitude) + "&appid=" + open_weather_map_key
data = requests.get(url)

# Write data to the input file
with open('current_aqi.txt', 'w') as outfile:
    outfile.write(str(data.json()))
