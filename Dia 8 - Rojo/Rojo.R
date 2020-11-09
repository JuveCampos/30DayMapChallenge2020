options(scipen = 999)
library(sf)
library(tidyverse)
library(leaflet)

# Bases de datos ----
agro <- readr::read_csv("http://infosiap.siap.gob.mx/gobmx/datosAbiertos/ProduccionAgricola/Cierre_agricola_mun_2019.csv",
                        locale = locale(encoding = "WINDOWS-1252"))

# Municipios
muni <- st_read("https://raw.githubusercontent.com/JuveCampos/Shapes_Resiliencia_CDMX_CIDE/master/geojsons/Division%20Politica/mpios_con_menos_islas_aun.geojson")

# Centroides
ctrd  <- st_centroid(muni) %>%
  select(-c(AREA, PERIMETER,COV_,COV_ID))

# Filtramos a jitomate
jitomate <- agro %>%
  filter(Nomcultivo == "Tomate rojo (jitomate)") %>%
  select(Nomestado, Idestado, Idmunicipio, Nommunicipio, Volumenproduccion) %>%
  mutate(Idestado = case_when(str_length(Idestado) == 1 ~ paste0('0', Idestado),
                             str_length(Idestado) == 2 ~ paste0(Idestado)),
         Idmunicipio = case_when(str_length(Idmunicipio) == 1 ~ paste0('00', Idmunicipio),
                                 str_length(Idmunicipio) == 2 ~ paste0('0', Idmunicipio),
                                 str_length(Idmunicipio) == 3 ~ paste0(Idmunicipio))) %>%
  mutate(CVEGEO = paste0(Idestado,Idmunicipio)) %>%
  group_by(CVEGEO) %>%
  summarise(Volumenproduccion = sum(Volumenproduccion))

# Verificar cuantos registros hay por municipio
# jitomate %>%
#   group_by(CVEGEO) %>%
#   count() %>%
#   arrange(-n)

a <- left_join(ctrd, jitomate, by = "CVEGEO") %>%
  filter(!is.na(Volumenproduccion))

a <- a %>%
  mutate(x = st_coordinates(a)[,1],
         y = st_coordinates(a)[,2])

class(a)

a %>%
  ggplot() +
  geom_sf(data = muni) +
  geom_point(aes(x = x, y = y, size = Volumenproduccion),
             color = "red")

# a$Volumenproduccion
# a$Volumenproduccion %>% max()
# a$Volumenproduccion %>% min()

a <- a %>%
  mutate(radio = 20*(Volumenproduccion - min(Volumenproduccion))/max(Volumenproduccion)) %>%
  mutate(radio = ifelse(radio < 1, yes = 1, no = radio))

plot(density(a$radio))


leaflet(a) %>%
  addTiles() %>%
  addCircleMarkers(radius = ~radio,
                   fillColor = "red",
                   fillOpacity = 0.8,
                   opacity = 1,
                   weight = 0.3,
                   color = "black",
                   label = lapply(paste0("<b>Estado: </b>", a$NOM_ENT, "<br>",
                                         "<b>Municipio: </b>", a$NOM_MUN, "<br>",
                                         "<b>Valor de Producción: </b>$", prettyNum(a$Volumenproduccion, big.mark = ","), " pesos mexicanos (2019) <br>"),
                                  htmltools::HTML))
