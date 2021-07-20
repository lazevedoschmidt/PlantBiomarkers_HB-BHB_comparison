##############################################################################
#Name: Lauren Azevedo Schmidt
#R version: 3.6.1
#Purpose: Leaflet map of BHB and HB
#date started: 04.13.2020
#############################################################################

require(leaflet)
require(geojsonio)
library(raster)

#get boundaries
usa <- getData("GADM", country="USA", level=2)
#add random data
usa$randomData <- rnorm(n=nrow(usa), 150, 30)

#create a color palette to fill the polygons
pal <- colorQuantile("Oranges", NULL, n = 5)

#create leaflet map
map = leaflet() %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  setView(-98.35, 39.7,
          zoom = 4) %>% 
  addPolygons(data = usa, 
              fillColor= ~pal(randomData),
              fillOpacity = 0.4,
              weight = 2,
              color = "white")
map

#TOPO map of WY
nhd_wms_url <- "https://basemap.nationalmap.gov/arcgis/services/USGSTopo/MapServer/WmsServer"

WY_base <-leaflet() %>% 
  setView(lng = -107.2903, lat = 43.0760, zoom = 7) %>%
  addWMSTiles(nhd_wms_url, layers = "0")%>%
  addMiniMap(zoomLevelOffset = -25)%>%
  addMarkers(lng = -106.5620, lat = 41.8700, label="Hanna Basin")%>%
  addMarkers(lng=-108.0390, lat=44.3800, label="Bighorn Basin")
#zoom of 7 is best for just highlighting WY
WY_base

#Hanna, WY lat long 41.8700° N, 106.5620° W
#Bighorn Basin, WY 44.3800° N, 108.0390° W





