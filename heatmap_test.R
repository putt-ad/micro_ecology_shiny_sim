#####
#heatmaps with leaflet
#####

#install leaflet extra programs (if using webGLHeatmap)
install.packages('leaflet.extras')
library(leaflet)
#to install leaflet extras you will need to run this
#devtools::install_github('bhaskarvk/leaflet.extras')
library(leaflet.extras)


#test of the addWebGLHeatmap using the quakes database
leaflet(quakes) %>% addProviderTiles(providers$CartoDB.DarkMatter) %>%
  addWebGLHeatmap(lng=~long, lat=~lat, size = 60000)

#using webGLHeatmap 
leaflet(data = micro.df) %>% addProviderTiles(providers$Stamen) %>%  
  #this will pull out the latitude and longitude of micro.df dataframe. size is set to 2 to make the wells viewable, not sure how the heatmap is generated
  addWebGLHeatmap(lng=~longitude, lat=~latitude, size = 2) %>% 
  #adds scalebar
  addScaleBar(position = "bottomright", options = scaleBarOptions(metric = FALSE))

#make heatmap where each well is colored by pH.
leaflet(data = micro.df) %>% addProviderTiles(providers$Stamen) %>% 
  addHeatmap(lng = ~longitude, lat = ~latitude, intensity = ~pH,
             blur = 1, max = 2, radius = 3) %>% 
  addScaleBar(position = "bottomright", options = scaleBarOptions(metric = FALSE))

#trial run w/London
london.crimes.files <- Sys.glob(
  paste0(system.file('examples/data/London-Crimes', package='leaflet.extras'),
         '/*/*-city-of-london-street.csv.zip'))
london.crimes <- suppressMessages(
  purrr::map(
    london.crimes.files,
    ~readr::read_csv(.) %>%
      dplyr::select(Latitude, Longitude) %>%
      dplyr::filter(!is.na(Latitude))) %>%
    magrittr::set_names(basename(Sys.glob(
      paste0(system.file('examples/data/London-Crimes', package='leaflet.extras'),
             '/2016*')))))

leaf <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron)

purrr::walk(
  names(london.crimes),
  function(month) {
    leaf <<- leaf %>%
      addHeatmap(
        data = london.crimes[[month]],
        layerId = month, group = month,
        lng=~Longitude, lat=~Latitude,
        blur = 20, max = 0.05, radius = 15)
  })

leaf %>%
  setView(-0.094106, 51.515, 14) %>%
  addLayersControl(
    baseGroups = names(london.crimes),
    options = layersControlOptions(collapsed = FALSE)
  )

#view london crimes
head(london.crimes)

#to make this work you will have to make the data file a list of lists
View(london.crimes)
#####
london.crimes.files <- Sys.glob(
  paste0(system.file('examples/data/London-Crimes', package='leaflet.extras'),
         '/*/*-city-of-london-street.csv.zip'))
london.crimes <- suppressMessages(
  purrr::map(
    london.crimes.files,
    ~readr::read_csv(.) %>%
      dplyr::select(Latitude, Longitude) %>%
      dplyr::filter(!is.na(Latitude))) %>%
    magrittr::set_names(basename(Sys.glob(
      paste0(system.file('examples/data/London-Crimes', package='leaflet.extras'),
             '/2016*')))))

leaf <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron)

purrr::walk(
  names(london.crimes),
  function(month) {
    leaf <<- leaf %>%
      addHeatmap(
        data = london.crimes[[month]],
        layerId = month, group = month,
        lng=~Longitude, lat=~Latitude,
        blur = 20, max = 0.05, radius = 15)
  })

leaf %>%
  setView(-0.094106, 51.515, 14) %>%
  addLayersControl(
    baseGroups = names(london.crimes),
    options = layersControlOptions(collapsed = FALSE)
  )

