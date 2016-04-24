#This is for small data sets (less than 300 rows)
# since Google has quota of 2500 requests per day
import csv
from geopy.geocoders import GoogleV3

def geocode(location):
    try:
        geolocator = GoogleV3()
        a = geolocator.geocode(location)
        latitude = a.latitude
        longitude = a.longitude
        # marker = [coordALat, coordALon]
        return [latitude, longitude]
    except ValueError:
        geocode(location)


#enter data set here
directory = "path/to/smalldata.csv"
test = []
with open(directory, 'rb') as csvfile:
     hazmatData = csv.reader(csvfile, delimiter=",")
     count = 0
     for row in hazmatData:
         if count == 0:
            test.append(row)
            print test[count]
            count = count + 1
            continue
         print test[count-1]
         latlong = geocode(row[1])
         row[2]  = latlong[0]
         row[3] = latlong[1]
         test.append(row)
        #  print chr(27) + "[2J"

         count = count + 1

#set output file
FILE = "path/to/output.csv"
with open(, 'a') as csvfile:
    writing = csv.writer(csvfile, delimiter=",")
    for row in test:
        writing.writerow(row)
