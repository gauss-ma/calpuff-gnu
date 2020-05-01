#!/bin/bash
#The URL format for the Static Maps API is:

#https://tile.thunderforest.com/static/{style}/{lon},{lat},{zoom}/{width}x{height}{scale}.{format}?apikey={apikey}

#style	The id for the map style, e.g. cycle or transport
#lon	The longitude of the centre of the map, in decimal degrees
#lat	The latitude of the centre of the map, in decimal degrees
#zoom	The zoom level of the map
#width	The width of the map, in pixels. Maximum width is 2560 pixels.
#height	The height of the map, in pixels. Maximum height is 2560 pixels.
#scale	(optional) The scale modifier, e.g. @2x. When omitted, a scale of 1 is used.
#format	The image format, e.g. .png or .jpg90
#apikey	Your API Key


style=landscape
lat=-34.82  
lon=-58.53  
zoom=14
width=1530
height=1530
format=png
apikey=a18add8b6b504cba83ce881713875934 
wget https://tile.thunderforest.com/static/${style}/${lon},${lat},${zoom}/${width}x${height}.${format}?apikey=${apikey}
