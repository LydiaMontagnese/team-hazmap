"""
    Author: Anastassios Dardas
    Team Hazmat
    Event: Bayes Hackathon Impact - 2016, San Francisco
    Date: 4/23/16
"""
import os, sys, csv, webbrowser, folium, shapefile, json
from json import dumps

# Directories - WARNING: Change when doing another file
pntPath = r'/home/tasos/Desktop/Bayes Hackathon/shapefiles/'
directory = r'/home/tasos/Documents/HazmatIncidents_GEO.csv'
os.chdir(pntPath)

# Function - Shapefile to Json
def shp2GeoJson():
    x = "freight_2012.shp"
    reader = shapefile.Reader(x)
    fields = reader.fields[1:]
    field_names = [field[0] for field in fields]
    buffer = []
    for sr in reader.shapeRecords():
        atr = dict(zip(field_names, sr.record))
        geom = sr.shape.__geo_interface__
        buffer.append(dict(type="Feature",
                            geometry=geom, properties=atr))
    htmlDirect = '/home/tasos/Desktop/Bayes Hackathon/html/'
    jsonFile = htmlDirect + 'crash_points.json'
    geojson = open(jsonFile, "w")
    geojson.write(dumps({"type":"FeatureCollection",
                        "features": buffer}, indent=2)
                        + "\n")
    geojson.close()
    print "Shapefile to GeoJson Complete"

# Function - Write HTML, create Heatmap - WARNING: Change names of files
def hot():
    html = '/home/tasos/Desktop/Bayes Hackathon/html/hazmat_incident.html'
    ff = open(html, 'w')
    header = """<!DOCTYPE html>
<html>
  <head>
    <title>Heat Map of Crash Points - Hazmat</title>
     <meta charset='utf-8'>
     <meta name= 'viewport' content='width=device-width, initial-scale=1.0'/>
     <link rel='stylesheet' href='http://cdn.leafletjs.com/leaflet/v0.7.7/leaflet.css'>
     <script src='http://cdn.leafletjs.com/leaflet/v0.7.7/leaflet.js'>
     </script>
     <script src='http://leaflet.github.io/Leaflet.heat/dist/leaflet-heat.js'>
     </script>
  </head>
  <body>
     <div id='map' style='width: 1300px; height: 900px; position: relative;'
     class='leaflet-container leaflet-touch leaflet-fade-anim' tabindex='0'>
     </div>
     <script>
     var map = L.map('map').setView([36.628, -97.718], 4);
     L.tileLayer('http://{s}.tile.osm.org/{z}/{x}/{y}.png').addTo(map);
     var points = ["""
    ff.write(header)
    xArray = []
    yArray = []
    countArray = []
    with open(directory, "rb") as d:
        reader = csv.DictReader(d, delimiter = ',')
        for rows in reader:
            xArray.append(rows['lat'])
            yArray.append(rows['long'])
            countArray.append(rows['incidents'])
    i = 0
    while i <= len(xArray)-1:
        ff.write('[' + xArray[i] + ',' + yArray[i] + ',' + countArray[i] + '],\n\t\t\t\t')
        i += 1
    ff.write('\t\t\t\t];\n')

    heat = """\tvar heat = L.heatLayer(points, {
        radius: 14,
        opacity: 0.7,
        blur: 20,
    }).addTo(map);"""

    ff.write(heat)
    end = """</script>
   </body>
 </html>"""
    ff.write(end)
    ff.close()
    webbrowser.open_new_tab(html)

shp2GeoJson()
hot()
