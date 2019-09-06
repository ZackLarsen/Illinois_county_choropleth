# Setup -------------------------------------------------------------------


library(pacman)
library(tidyverse)
library(magrittr)

p_load(flexdashboard, leaflet, leaflet.extras, jsonlite, geojsonio,
       rgdal, here, conflicted, readxl, data.table, DataExplorer, esquisse,
       mlr, parsnip, ranger)

conflict_prefer("filter", "dplyr")





# Data Prep ---------------------------------------------------------------



# Shapefile
shape <- readOGR(dsn = "data/tl_2019_us_zcta510", layer = "tl_2019_us_zcta510")


# Subset so that we only have the zip codes listed in the datafile 
il_shapes <- shape[shape$ZCTA5CE10 %in% extra_data$`Zip Code`,]

# Remove shape, as we no longer need in memory:
rm(shape)




LHS <- il_shapes@data
RHS <- extra_data

LHS %<>% 
  as.data.frame() %>% 
  mutate_all(as.character) %>% 
  mutate_all(as.numeric) %>% 
  select(ZCTA5CE10, ALAND10, AWATER10) %>% 
  mutate(ATOTAL10 = ALAND10 + AWATER10)

joined_data <- LHS %>% 
  inner_join(RHS, by = c("ZCTA5CE10" = "Zip Code"))



joined_data







# Counts_of_xxxxx --------------------------------------------------------------


joined_data %>% 
  select(-ALAND10, -AWATER10, ATOTAL10) %>% 
  select(ZCTA5CE10, var1, var2, var3) %>% 
  mutate(ratio = var1 / ifelse(var2 == 0, 1, var3)) %>% 
  arrange(-var3) %>% 
  head()




# Had to remove the arrange() call below, which was causing the zip code shapefile to plot
# things in the incorrect order, making zip codes appear in the wrong place on the map:
extra_data <- joined_data %>% 
  select(-ALAND10, -AWATER10, -ATOTAL10, -ratio) %>% 
  replace_na(replace = list(var3 = 0)) %>% 
  mutate(
    ratio1 = var1 / ifelse(var3 == 0, 1, var2),
    binary = ifelse(var3 == 0 & var2 > 0, 100, 0)
  ) %>% 
  replace_na(replace = list(ratio1 = 0))





il_shapes@data <- extra_data





# Create the HTML content to serve as the popup label
labels <- sprintf(
  "<strong>Zip Code#: %s</strong><br/>
  __in zip code: %s<br/>
  var1 %s<br/>
  var2: %s<br/>
  var3: %s<br/>",
  il_shapes$ZCTA5CE10,
  il_shapes$var1,
  il_shapes$var2,
  il_shapes$var3
) %>% 
  lapply(htmltools::HTML)



pal <- colorBin("YlOrRd", il_shapes$ratio1, 5, pretty = TRUE)


# Binary for zero / nonzero:
bin_pal <- colorBin("YlOrRd", il_shapes$binary, 2, pretty = FALSE)





leaflet(il_shapes) %>%
  # Base tile groups
  addProviderTiles(providers$Stamen.Terrain, group = "Terrain") %>%
  # Overlay groups
  addPolygons(fillColor = ~pal(ratio1),
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
              group = "var1"
  ) %>% 
  addPolygons(fillColor = ~pal2(var2),
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
              group = "var2"
  ) %>%   
  # This is the binary one for...
  # We are keeping it out of the layers control buttons so it always displays in 
  # the background
  addPolygons(fillColor = ~bin_pal(binary),
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
              group = "Binary"
  ) %>% 
  #addMiniMap() %>% 
  # Layers control
  addLayersControl(
    overlayGroups = c("var1", "var2", "var3"), # Notice "Binary" is missing here on purpose
    options = layersControlOptions(collapsed = FALSE, autoZIndex = TRUE),
    position = "topright"
  ) %>%
  # The hideGroup() calls below ensure that we start the map with all layers deselected:
  hideGroup("var1") %>% 
  hideGroup("var2") %>% 
  hideGroup("var3")
