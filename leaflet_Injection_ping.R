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
library(readr)
all_micro_data <- read_csv("~/Dropbox/Andrews MacBook (Hazen MacBook Copy)/Documents/Coursework/GEOL590/
                           misc/micro_ecology_shiny_sim/data/micro_geochem_data_adp_2019.11.08.csv")
S3_map <- leaflet() %>% 
#  addProviderTiles("Esri.WorldImagery") %>% 
  addProviderTiles("Stamen.TonerLite") %>% 
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

#shiny basic non-marker map of Y-12
library(shiny)
library(readr)
app <- shinyApp(
  ui = fluidPage(leafletOutput('S3_map')),
  server = function(input, output) {
    map = leaflet() %>% addTiles() %>% setView(lng = -84.2741,
                                              lat = 35.9770,
                                              zoom = 17) 
    output$S3_map = renderLeaflet(map)
  }
)

if(interactive()) app

######
#This is the Shiny App with well markers and source marker
#can be used to maybe make a reactive map
#####
library(shiny)
library(leaflet)
library(readr)
micro.df <- read_csv("~/Dropbox/Andrews MacBook (Hazen MacBook Copy)/Documents/Coursework/GEOL590/misc/micro_ecology_shiny_sim/data/micro_geochem_data_adp_2019.11.08.csv")
chem.df <- read.csv("~/Dropbox/Andrews Macbook (Hazen MacBook Copy)/Documents/Coursework/GEOL590/misc/micro_ecology_shiny_sim/data/geochem.csv")

app <- shinyApp(
  ui = fluidPage(leafletOutput('S3_map')),
  server = function(input, output) {
    map = leaflet(data = micro.df) %>% 
      addProviderTiles("Stamen") %>% 
  #S3 pond marker
      addMarkers(lng = ~longitude,
             lat = ~latitude,
             popup = ~as.character(pH), 
             label = ~as.character(well_id),
             options = markerOptions(draggable = FALSE, riseOnHover = TRUE)) %>% 
  #injection site marker
      addCircleMarkers(lng = -84.2730,
                       lat = 35.9781,
                       popup = "S3 Pond",
                       fillColor = "red", opacity = 1,
                       options = markerOptions(draggable = FALSE)) %>% 
#setview tells shiny where to focus the center of the map
      setView(lng = -84.2741,
          lat = 35.9770,
          zoom = 17) %>% 
      addScaleBar()
    output$S3_map = renderLeaflet(map)
  }
)


if(interactive()) app


# Show first 20 rows from the `quakes` dataset
leaflet(data = quakes[1:20,]) %>% addTiles() %>%
  addMarkers(~long, ~lat, popup = ~as.character(mag), label = ~as.character(mag))


# test to extract the well location data
test_map <- leaflet(data = micro.df) %>% addProviderTiles(provider = "Stamen.TonerLite") %>% 
  addMarkers(~longitude, ~latitude, popup = ~as.character(pH), label = ~as.character(index)) %>% 
  addCircleMarkers(lng = -84.2730,
                   lat = 35.9781, 
                   label = "Contaminant Source",
                   popup = "S3 Ponds") %>% 
  setView(lng = -84.2741,
          lat = 35.9770,
          zoom = 17) %>% 
  addScaleBar(position = "bottomright", options = scaleBarOptions(metric = FALSE))
test_map


