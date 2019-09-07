##################################################
## Project: Illinois County Choropleth
## Script purpose: Use R Leaflet to build choropleth map
## Date: September 6, 2019
## Author: Zack Larsen
##################################################



# Setup -------------------------------------------------------------------


library(pacman)
library(tidyverse)
library(magrittr)

p_load(flexdashboard, leaflet, leaflet.extras, jsonlite, geojsonio, geojsonsf,
       rgdal, here, conflicted, readxl, data.table, DataExplorer, esquisse,
       mlr, parsnip, ranger)

conflict_prefer("filter", "dplyr")
conflict_prefer("geojson_sf", "geojsonsf")

# Data Prep ---------------------------------------------------------------

# US Zip Code Shapefile
#shape <- readOGR(dsn = "data/tl_2019_us_zcta510", layer = "tl_2019_us_zcta510")



# Subset so that we only have the zip codes listed in the datafile 
#il_shapes <- shape[shape$ZCTA5CE10 %in% extra_data$`Zip Code`,]

# Remove shape, as we no longer need in memory:
#rm(shape)




# IL ZCTA geojson
# https://geodata.lib.berkeley.edu/catalog/TG00ILZCTA
#IL_ZCTA <- read_json("data/TG00ILZCTA-geojson.json")
#IL_ZCTA <- geojsonio::geojson_read("data/TG00ILZCTA-geojson.json", what = "sp")
IL_ZCTA <- geojson_sf('data/TG00ILZCTA-geojson.json')




# https://simplemaps.com/data/us-zips
us_zips <- fread('data/simplemaps_uszips_basicv1.6/uszips.csv')

us_zips %<>% 
  filter(state_id == 'IL') %>% 
  select(state_id, zip, county_name, population, density, lat, lng) %>% 
  rename(ZCTA = 'zip') %>% 
  mutate_at(vars(ZCTA), as.character)

#glimpse(us_zips)









# LHS <- IL_ZCTA@data
# RHS <- us_zips %>% 
#   select(zip, county_name, population, density, lat, lng)
# 
# glimpse(LHS)
# glimpse(RHS)
# 
# # Change zip from integer to factor to match types with geojson object:
# RHS %<>% 
#   mutate_at(vars(zip), as.factor)
# 
# joined_data <- LHS %>% 
#   inner_join(RHS, by = c("ZCTA" = "zip"))
# 
# joined_data
# 
# # Add the us_zips-joined data to the actual shapefile for the
# # Illinois Zip Codes:
# IL_ZCTA@data <- joined_data





# Join census ZCTA code geojson shapefile object to population data:
mapdf <- inner_join(IL_ZCTA, us_zips, by = 'ZCTA') 



# Join census ZCTA code geojson shapefile object to population data
# but ONLY FOR Cook and DuPage counties:
cook_dupage <- inner_join(IL_ZCTA, us_zips %>% filter(county_name %in% c('Cook', 'DuPage')), by = 'ZCTA') 




# Experiments -------------------------------------------------------------

# us_zips %>% 
#   head()
# 
# leaflet(us_zips, options = leafletOptions(minZoom = 7, maxZoom = 12)) %>%
#   setView(lng = -88.08716, lat = 42.06394, zoom = 7) %>% 
#   # Base tile groups
#   addProviderTiles(providers$Stamen.Terrain, group = "Terrain") %>% 
#   addMarkers(~lng, ~lat)









labels <- sprintf(
  "<strong>Zip Code#: %s</strong><br/>
  County Name: %s<br/>
  Population: %s<br/>",
  mapdf$ZCTA,
  mapdf$county_name,
  mapdf$population
) %>% 
  lapply(htmltools::HTML)

pop_pal <- colorBin("YlOrRd", mapdf$population, 9, pretty = TRUE)
density_pal <- colorBin("YlOrRd", mapdf$density, 9, pretty = TRUE)

leaflet(mapdf, options = leafletOptions(minZoom = 7, maxZoom = 12)) %>%
  setView(lng = -88.08716, lat = 42.06394, zoom = 7) %>% 
  # Base tile groups
  addProviderTiles(providers$Stamen.Terrain, group = "Terrain") %>% 
  #addMarkers(~lng, ~lat) %>% 
  addPolygons(fillColor = ~pop_pal(population),
              weight = 1,
              opacity = 1,
              color = "white",
              #dashArray = "3",
              fillOpacity = 0.9,
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                #dashArray = "",
                fillOpacity = 0.3,
                bringToFront = TRUE),
              label = ~labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"),
              group = "Population"
  ) %>% 
  addPolygons(fillColor = ~density_pal(density),
              weight = 1,
              opacity = 1,
              color = "white",
              #dashArray = "3",
              fillOpacity = 0.9,
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                #dashArray = "",
                fillOpacity = 0.3,
                bringToFront = TRUE),
              label = ~labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"),
              group = "Density"
  ) %>% 
  addLayersControl(
    overlayGroups = c("Population", "Density"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>% 
  hideGroup("Density")











cook_dupage$ZCTA


cook_dupage %>% 
  filter(ZCTA == 60155)


labels <- sprintf(
  "<strong>Zip Code#: %s</strong><br/>
  County Name: %s<br/>
  Population: %s<br/>
  Population Density: %s<br/>",
  cook_dupage$ZCTA,
  cook_dupage$county_name,
  cook_dupage$population,
  cook_dupage$density
) %>% 
  lapply(htmltools::HTML)

pop_pal <- colorBin("YlOrRd", cook_dupage$population, 8, pretty = TRUE)
density_pal <- colorBin("YlOrRd", cook_dupage$density, 8, pretty = TRUE)

leaflet(cook_dupage, options = leafletOptions(minZoom = 9, maxZoom = 12)) %>%
  setView(lng = -87.85636, lat = 41.85781, zoom = 10) %>% 
  # Base tile groups
  addProviderTiles(providers$Stamen.Terrain, group = "Terrain") %>% 
  #addMarkers(~lng, ~lat) %>% 
  addPolygons(fillColor = ~pop_pal(population),
              weight = 1,
              opacity = 1,
              color = "white",
              #dashArray = "3",
              fillOpacity = 0.9,
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                #dashArray = "",
                fillOpacity = 0.3,
                bringToFront = TRUE),
              label = ~labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"),
              group = "Population"
  ) %>% 
  addPolygons(fillColor = ~density_pal(density),
              weight = 1,
              opacity = 1,
              color = "white",
              #dashArray = "3",
              fillOpacity = 0.9,
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                #dashArray = "",
                fillOpacity = 0.3,
                bringToFront = TRUE),
              label = ~labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"),
              group = "Density"
  ) %>% 
  addLayersControl(
    overlayGroups = c("Population", "Density"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>% 
  hideGroup("Density")







