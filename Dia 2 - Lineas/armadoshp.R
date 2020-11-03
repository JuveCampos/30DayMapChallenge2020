library(sf)
library(tidyverse)
a <- st_read("https://raw.githubusercontent.com/JuveCampos/Shapes_Resiliencia_CDMX_CIDE/master/geojsons/Division%20Politica/DivisionEstatal.geojson")
cdmx <- a %>% 
  filter(ENTIDAD == "Ciudad de México")
plot(cdmx,  max.plot = 1)

st_write(cdmx, "/Users/admin/Desktop/Map Challenge 2020/cdmx_shape.geojson")
st_write(cdmx, "/Users/admin/Desktop/Map Challenge 2020/cdmx_shape.shp")
