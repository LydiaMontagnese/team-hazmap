#For the HAZMAT Incidents Data

import csv
import geocoder #uses MapQuest API

#Ensure data keeps
def foo(location):
    return geocode(location)

def geocode(location):
    try:
        g = geocoder.mapquest(location, key = 'OX8iJIr1ZJbHLyGHd7p6BygUMCBiFutY')
        return g.json
    except:
        foo(location)

# Network cuts out occaisionally, so just the last file that
# was last downloading data from the web.

leftoff = 1               #if the network stops, set to last file number
leftover = 121 - leftoff

for i in range(leftover):
    directory = "HAZMAT/HazmatIncidents" + str(i+leftoff) + ".csv"
    test = []
    print "HAZMAT/HazmatIncidents" + str(i+leftoff) + ".csv"
    with open(directory, 'rb') as csvfile:
         hazmatData = csv.reader(csvfile, delimiter=",")
         count = 0
         for row in hazmatData:
             if count == 0:
                test.append('\n')
                count = count + 1
                continue
             print test[count-1]
             latlong = geocode(row[1])
             row[2] = latlong.get('lat')
             row[3] = latlong.get('lng')
             test.append(row)
            #  print chr(27) + "[2J"

             count = count + 1

    with open('HazmatIncidents_GEO.csv', 'a') as csvfile:
        writing = csv.writer(csvfile, delimiter=",")
        for row in test:
            writing.writerow(row)
