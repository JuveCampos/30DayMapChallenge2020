# Librerias
library(sf)
library(tidyverse)
library(leaflet)

# Bases de datos ----
rutas <- st_read("metro85.shp")
class(rutas)
rutas <- st_zm(rutas)
ent <- st_read("cdmx_shape.shp")

# Mapa ----
leaflet() %>%
  # addTiles() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = ent,
              fillColor = "purple",
              color = "purple",
              fillOpacity = 0.1) %>%
addPolylines(data = rutas,
             color = "green")

