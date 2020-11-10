# Librerias
library(leaflet)
library(sf)
library(tidyverse)
library(mapview)

# Funciones propias
source("https://raw.githubusercontent.com/JuveCampos/DataVizRepo/f134de2ce6bb42478fa6b2a0dd5076705cfca48c/R/R%20-%20leaflet/Mapas_zona_metropolitana/Norte.R")
niveles <- function(x) levels(as.factor(x))
b <- function(x) paste0("<b>", x, "</b>")

# Lectura de Bases de Datos
# ESTAS BASES DE DATOS NO ESTAN EN ESTE REPOSITORIO
# PUEDES HALLARLAS EN ESTE OTRO: 
# https://github.com/JuveCampos/ProyectosEnShiny/tree/master/shinyRedNacionalCaminos/www
edos <- list.files("www/BasesDeDatosAbreviadas", pattern = ".rds") %>% 
  str_remove(pattern = ".rds")

mapx <- st_read("https://raw.githubusercontent.com/JuveCampos/MexicoSinIslas/master/Sin_islas.geojson") %>% 
  st_transform(crs = 4326)

for (edo in edos[1:32]){

edosRoads2 <- readRDS(paste0("www/BasesDeDatosAbreviadas/",edo, ".rds"))

estado <- mapx %>% 
  filter(ENTIDAD == edo)

# Labels Esto no se uso obvio
lab <-  lapply(paste0(b("Nombre: "), edosRoads2$NOMBRE, "<br>",
                      b("Condición: "), edosRoads2$CONDICION, "<br>",
                      b("Tipo de Vialidad: "), edosRoads2$TIPO_VIAL, "<br>", 
                      b("Cubrimiento: "), paste(edosRoads2$COND_PAV, edosRoads2$RECUBRI), "<br>", 
                      b("Carriles: "), edosRoads2$CARRILES, "<br>",
                      b("Velocidad: "), edosRoads2$VELOCIDAD, "km/h"
), htmltools::HTML)

# Mapa guardado en el objeto mapa
mapita<- leaflet(edosRoads2) %>%
    addProviderTiles("CartoDB.DarkMatter",
                     options = providerTileOptions(opacity = 0.55)) %>%
    addPolylines(data = estado,
                 weight = 0.8,
                 dashArray = c(3,2),
                 color = "white") %>%
    addPolylines(weight = edosRoads2$isCarretera*10,
                 opacity = 0.9,
                 label = lab,
                 color = "white") %>%
    norte(posicion = "topright")

mapshot(mapita, file = paste0(edo, ".png"))
print(edo)
}

  
# Elaboracion del *.gif
library(magick)
list.files("03_Graficas/", pattern = "*.png", full.names = T)[c(4,5,8,1,9,7,6,2,12,11,10,3)] %>% 
  map(image_read) %>% # lee cada archivo de imagen
  image_join() %>% # junta las imagenes
  image_animate(fps=2) %>% # animates, can opt for number of loops
  image_write("5 Raster.gif") # guarda en el directorio actual

