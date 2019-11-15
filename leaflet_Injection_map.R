#####
#Leaflet shiny package
#####

install.packages("leaflet")
devtools::install_github(("rstudio/leaflet"))
1
Yes
install.packages("shiny")
devtools::install_github("rstudio/shiny")
library(leaflet)
library(shiny)
leaflet(width = 400, height = 200)
map <- leaflet(width = 400, height = 400)
map <- addTiles(map)
map <- setView(map, lng = -123.251,
               lat = 49.263,
               zoom = 6)

map

maptypes <- c("MapQuestOpen.Aerial",
              "Stamen.TerrainBackground",
              "Esri.WorldImagery",
              "OpenStreetMap",
              "Stamen.Watercolor")

map <- leaflet() %>% 
  addProviderTiles(maptypes[1])


UTmap <- leaflet() %>% 
  addProviderTiles("Stamen.TerrainBackground") %>% 
  setView(map, lng = -83.9295,
               lat = 35.9544,
               zoom = 10)

UTweather <- leaflet() %>% 
  addProviderTiles("OpenStreetMap") %>% 
  addProviderTiles("OpenWeatherMap.Rain") %>% 
  setView(lng = -83.9295,
          lat = 35.9544,
          zoom = 10)
UTweather

#Vancouver test map with popups
vanc_map <- leaflet() %>% 
  addProviderTiles("Esri.WorldImagery") %>% 
  #moveable map marker
  addMarkers(lng = -123.251,
             lat = 49.263, 
             popup = "You are here.",
             options = markerOptions(draggable = TRUE, riseOnHover = TRUE)) %>% 
  #fixed map marker as a circle
  addCircleMarkers(lng = -123.261,
                   lat = 49.273, 
                   popup = "You aren't here.",
                   fillColor= "red", opacity = 1,
                   options = markerOptions(draggable = FALSE, title = "Whoops")) %>% 
  setView(lng = -123.251,
          lat = 49.263,
          zoom = 13)
vanc_map

#S3 ponds map
S3_map <- leaflet() %>% 
  addProviderTiles("Esri.WorldImagery") %>% 
  #S3 pond marker
  addMarkers(lng = -84.2730,
             lat = 35.9781,
             popup = "S3 Ponds",
             options = markerOptions(draggable = FALSE, riseOnHover = TRUE)) %>% 
  #injection site marker
  addCircleMarkers(lng = -84.2746,
                  lat = 35.9757,
                  popup = "Injection Well",
                  fillColor = "red", opacity = 1,
                  options = markerOptions(draggable = FALSE)) %>% 
  setView(lng = -84.2741,
          lat = 35.9770,
          zoom = 17) %>% 
  addScaleBar()
S3_map

library(shiny)





