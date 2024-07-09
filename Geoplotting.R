install.packages("tidygeocoder")
install.packages("sf")
install.packages("mapview")
install.packages("leaflet")
install.packages("plotly")

library('tidygeocoder')
library('sf')
library('mapview')
library('leaflet')
library('plotly')
library(readxl)


hotel = read_csv("C:/Users/Open User/Downloads/Hotels In Lagos.csv")
str(hotel)
view(hotel)

Sys.setenv(GOOGLEGEOCODE_API_KEY = "AIzaSyDA_XjEWNXWmiJ############")
geo <- hotel %>%
  geocode(address = Address, method = "google")
View(geo)


hotels_clean <- geo %>%
  filter(!is.na(lat) & !is.na(long))


view(hotels_clean)


str(hotels_clean)

mapview(hotels_clean_sf)
hotels_clean_sf<-hotels_clean %>%
  st_as_sf(
    coords=c("long","lat"),
    crs=4326
  )
colnames(hotels_clean_sf)

hotels_clean_sf %>%
  leaflet() %>%
  addProviderTiles(providers$OpenStreetMap, group = 'OpenStreetMap') %>%
  addProviderTiles(providers$Esri.WorldImagery,group="World Imagery")%>%
  addLayersControl(baseGroups=c('WorldImagery','OpenStreetMap'))%>%
  addMarkers(label = hotels_clean_sf$Hotel_Name,
             clusterOptions = markerClusterOptions(),
             popup=ifelse(hotels_clean_sf$Address!=NA,hotels_clean_sf$Address,#value if true
                          "not sure of the hotel's location"))#val false)










