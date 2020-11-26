# Mapa del 25 de Noviembre ----

##################################################
# Defunciones de COVID-19 por estado (municipios)#
##################################################

# Código elaborado por: Juvenal Campos y Ami Sosa 

# Librerias ----
library(tidyverse)
library(rebus)
library(curl)
library(sf)
library(leaflet)
library(htmlwidgets)
library(htmltools)

# Setup ----
Sys.setlocale("LC_ALL", "es_ES.UTF-8") 
options(scipen=999)

# Bases de datos ----

# Descarga automatica de los datos de hoy: 
# Nombre del archivo (correr despues de las 7 p.m.)
descarga <- function(){
  archivo <<- Sys.Date() %>%
    as.character() %>%
    as_tibble() %>%
    mutate(anio = "20",
           mes = str_remove_all(string = str_extract(value,
                                                     pattern = "\\-\\d\\d\\-"),
                                pattern = "\\-"),
           dia = str_extract(value,
                             pattern = "\\d\\d$"),
           fecha = paste0(anio, mes, dia, "COVID19MEXICO.csv")) %>% 
    pull(fecha)
  
  # Descargo el archivo
  curl::curl_download(url = "http://datosabiertos.salud.gob.mx/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip", 
                      destfile = "www/datosHoyCovid.zip")
  print("Ya se descargó el archivo más reciente")
  # Deszipeo el archivo  
  zip::unzip("www/datosHoyCovid.zip", 
             exdir = "www/Estados")
  print("Ya se descomprimió en la carpeta www/")
}

# Descargamos los datos
descarga()

# Info de mapa ----
muni <- st_read("www/Shapes/mpios.geojson")

# Info de casos ----
list.files("www/Estados") # Ver archivos en la carpeta 
casos <- read_csv(paste0("www/Estados/", sort(list.files("www/Estados"))[length(list.files("www/Estados"))]))

# Casos por municipio de residencia ----
casos <- casos %>% 
  mutate(CVEGEO = paste0(ENTIDAD_RES, MUNICIPIO_RES)) 

# Fallecimientos por municipio 
fallecimientos <- casos %>%
  filter(RESULTADO_LAB == 1) %>% 
  filter(!is.na(FECHA_DEF)) %>% # Presentan fecha de fallecimiento
  ungroup() %>% 
  group_by(CVEGEO) %>% 
  count() %>% 
  rename("Fallecimientos" = n)

# Pegamos los datos ----
mapa <- merge(muni, enfermos, by = "CVEGEO", all.x = TRUE) %>% 
  merge(fallecimientos, by = "CVEGEO", all.x = TRUE) %>%
  mutate(Casos = case_when(is.na(Casos) ~ 0L,
                           TRUE ~ Casos),
         Fallecimientos = case_when(is.na(Fallecimientos) ~ 0L,
                                    TRUE ~ Fallecimientos))

norte <- function(mapa_leaflet, 
                  ancho = 40, 
                  position = 'topleft', 
                  direccion_img_norte = "http://ian.umces.edu/imagelibrary/albums/userpics/10002/normal_ian-symbol-north-arrow-2.png"){
  # 1. Descargamos la imagen
  
  north.arrow.icon <- paste0("<img src='", 
                             direccion_img_norte,
                             "' style='width:",
                             as.character(ancho), "px;'>")
  # Lo incluimos en una funcion de RLeaflet
  if (!require("leaflet")) install.packages("leaflet") # Asegurarnos que este instalado Leaflet
  addControl(mapa_leaflet, 
             html = north.arrow.icon, position = "topright", 
             className = "fieldset {
             border: 0;}") 
}

cat <- "Casos"
estado <- "Morelos"

edo <- mapa %>% 
  filter(NOM_ENT == estado)

edo_mor <- edo %>% 
  mutate(mor = case_when(between(Casos, 1,25) ~ "1-25 Casos", 
                         between(Casos, 26,50) ~ "26-50 Casos",
                         between(Casos, 51,100) ~ "51 a 100 Casos",
                         between(Casos, 101,500) ~ "101 a 500 Casos",
                         between(Casos, 501, 1000) ~ "501 a 1000 Casos",
                         between(Casos, 1000,10000) ~ "mas de 1000 Casos")) %>% 
  mutate(mor = factor(mor, levels = c("1-25 Casos","26-50 Casos","51 a 100 Casos","101 a 500 Casos","501 a 1000 Casos","mas de 1000 Casos"), 
                      ordered = TRUE))

pal_casos_mor <- colorFactor(palette = "magma",
                             rev = TRUE, 
                             domain = edo_mor$mor)

pal_casos <- colorFactor(palette = "magma",
                         rev = TRUE, 
                         domain = levels(edo_mor$Casos))

# Mapa ---- 
edo_mor %>% 
  leaflet(options = leafletOptions(zoomControl = FALSE)) %>% 
  addPolygons(fillColor = pal_casos_mor(edo_mor$mor),
              fillOpacity = 1,
              color = "white",
              opacity = 1,
              label = label, 
              weight = 1) %>% 
  addLegend(position = "bottomright", 
            pal = pal_casos_mor, values = edo_mor$mor, 
            title = "Casos por municipio",
            opacity = 1) %>% 
  addScaleBar(position = c("bottomleft"), options = scaleBarOptions
              (maxWidth = 100, metric = TRUE, updateWhenIdle = TRUE)) %>% 
  norte() %>% 
  addProviderTiles(providers$CartoDB.Positron, options = tileOptions(opacity = 0.6)) 


# Mapa ---- 
edo %>% 
  leaflet(options = leafletOptions(zoomControl = FALSE)) %>% 
  addPolygons(fillColor = pal_casos(edo$Casos),
              fillOpacity = 1,
              label = label, 
              weight = 0.5) %>% 
  addLegend(position = "bottomright", 
            pal = pal_casos, values = edo$Casos, 
            title = "Casos por municipio",
            opacity = 1) %>% 
  addScaleBar(position = c("bottomleft"), options = scaleBarOptions
              (maxWidth = 100, metric = TRUE, updateWhenIdle = TRUE)) %>% 
  norte() %>% 
  addProviderTiles(providers$CartoDB.Positron, options = tileOptions(opacity = 0.6)) 
