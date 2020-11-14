# Librerias
library(sf)
library(raster)
library(tidyverse)
library(leaflet)
library(webshot)
library(htmlwidgets)

# Modelo de Elevación Digital, descargado de acá: https://www.inegi.org.mx/app/geo2/elevacionesmex/
tip <- paste0("CEM_V3_20170619_R15_E17/Morelos_r15m.bil")

r <- raster(tip)

pal <- colorNumeric(palette = c("black", "gray90"), domain = values(r),
                    na.color = "transparent", reverse = FALSE)

# Creamos mapa en Leaflet #
(m <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
    # addTiles() %>%
    addRasterImage(r,
                   colors = pal,
                   opacity = 0.9) %>% addLegend(pal = pal, values = values(r),
                    title = paste0("<b style = 'color: #c26400;'>Modelo de Elevación Digital<br>Elevación sobre el nivel del mar<br>Estado de Morelos. ", "</b>"),
                    position = "bottomright",
                    labFormat = labelFormat(suffix = " msnmm"))  )

htmlwidgets::saveWidget(m, "elevacionMorelos.html")



