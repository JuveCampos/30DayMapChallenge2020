# Librerias
library(tidyverse)
library(sf)

# Bases de datos 

# Guardado y conversion a rds
# bd <- read_csv("FB Data Maps/population_mex_2018-10-01.csv")
# saveRDS(bd, "FB Data Maps/FB_data.rds")

# Obtención de los limites de coahuila
coah <- st_read("https://raw.githubusercontent.com/JuveCampos/Shapes_Resiliencia_CDMX_CIDE/master/geojsons/Division%20Politica/DivisionEstatal.geojson")
coah <- coah %>% 
  filter(ENTIDAD == "Coahuila de Zaragoza")

# Secciones Electorales ----
secc <- st_read("COAHUILA DE ZARAGOZA/SECCION.shp") %>% 
  st_transform(crs = 4326)

# Poblacion ----
pop <- readRDS("FB Data Maps/FB_data.rds")
# Conversion a objeto sf
pop_sf <- st_as_sf(pop, coords = c("longitude",  "latitude"), 
         crs = 4326, 
         agr = "constant")
# Nos deshacemos del tibble
rm(pop)
# Nos quedamos con los puntos de Coahuila
pop_coah <- st_intersection(pop_sf, coah)
# Lo guardamos como rds
saveRDS(pop_coah, "pop_coah.rds")

# Lo leemos de nuevoi
coah_pop <- readRDS("pop_coah.rds")

# Pruebas
# coah_pop_ind <- sample(1:nrow(coah_pop))[1:(0.1*nrow(coah_pop))]
# coah_pop_2 <- coah_pop[coah_pop_ind,]
# class(coah_pop_2)
# plot(coah_pop_2, max.plot = 1)
# int <- st_intersection(pop_sf, secc_1)
# plot(int, max.plot = 1)

# Datos electorales ----
elec <- readxl::read_xlsx("Diputaciones2020_Distrito-Municipio.xlsx") %>% 
  select(nom_mun, seccion, total_votos, pan, pri, prd, pt, pvem, udc, mc, morena, unidos, prc, zapata, nulos)

# Proporción de votos por cada sección para cada partido
props <- elec %>% 
    pivot_longer(cols = 3:15, 
                 names_to = "partido", 
                 values_to = "votos") %>% 
  filter(partido != "total_votos") %>% 
  group_by(seccion, partido) %>% 
  summarise(sum_votos = sum(votos)) %>%
  ungroup() %>% 
  group_by(seccion) %>% 
  mutate(pp_votos = sum_votos/sum(sum_votos))

props_muni <- elec %>% 
  pivot_longer(cols = 3:15, 
               names_to = "partido", 
               values_to = "votos") %>% 
  filter(partido != "total_votos") %>% 
  group_by(nom_mun, partido) %>% 
  summarise(sum_votos = sum(votos)) %>%
  ungroup() %>% 
  group_by(nom_mun) %>% 
  mutate(pp_votos = sum_votos/sum(sum_votos))

# Secciones para correr el loop
secciones <- unique(secc$seccion)

# Abrimos Municipios
coah_muni <- st_read("COAHUILA DE ZARAGOZA/MUNICIPIO.shp") %>% 
  st_transform(crs = 4326)

mpios <- unique(props_muni$nom_mun)
i <- mpios[1]

props_muni$nom_mun <- str_replace_all(props_muni$nom_mun, c("ACUÑA" = "ACU?A", 
                                                            "CASTAÑOS" = "CASTA?OS", 
                                                            "CUATRO CI√âNEGAS" = "CUATROCIENEGAS", 
                                                            "JIM√âNEZ" = "JIMENEZ", 
                                                            "JU√ÅREZ" = "JUAREZ", 
                                                            "M√öZQUIZ" = "MUZQUIZ", 
                                                            "TORRE√ìN" = "TORREON", 
                                                            "VILLA UNI√ìN" = "VILLA UNION"))

unique(props_muni$nom_mun)[!(unique(props_muni$nom_mun) %in% unique(coah_muni$nombre))]
mpios <- unique(props_muni$nom_mun)
# Loop por Municipio
for (i in mpios){
  # i  <- 1
  elec_1 <- props_muni %>% 
    filter(nom_mun == i)
  
  secc_1 <- coah_muni %>% 
    filter(nombre == i)
  
  # elec_1 #proporciones
  # secc_1 # shp de la seccion
  # coah_pop_2 # Poblacion_coahuila
  
  # Puntos para el DTTO 3
  ptos_3 <- st_intersection(coah_pop, secc_1)
  
  partidos <- unique(elec_1$partido)
  
  vec_1 <- lapply(partidos, function(x){
    rep(x, elec_1 %>% 
          filter(partido == x) %>% 
          pull(pp_votos)*nrow(ptos_3))
  })
  
  cols_votos <- sample(unlist(vec_1)) %>% 
    str_replace_all(c("pvem" = "otros", 
                      "nulos" = "otros", 
                      "unidos" = "otros", 
                      "prc" = "otros", 
                      "udc" = "otros", 
                      "mc" = "otros", 
                      "zapata"= "otros",
                      "prd" = "otros", 
                      "pt" = "otros"))
  
  if(length(cols_votos)<nrow(ptos_3)){
    cols_votos <- append(cols_votos, 
                         rep("otros", (nrow(ptos_3)-length(cols_votos))))
  }
  
  ptos_3 <- cbind(ptos_3, cols_votos)
  
  ptos_3 <- ptos_3 %>% 
    select(cols_votos)
  # st_write(ptos_3, paste0("ptos_por_seccion/seccion_", 
  #                         3, 
  #                         ".geojson"
  #                         ))
  
  saveRDS(ptos_3, paste0("ptos_por_municipio/mpio_", 
                         i, 
                         ".rds"
  ))
  print(paste0("paso: ", i, " completado"))
}


# Loop por Seccion
for (i in secciones){
    elec_1 <- props %>% 
      filter(seccion == i)
    
    secc_1 <- secc %>% 
      filter(seccion == i)
    
    # elec_1 #proporciones
    # secc_1 # shp de la seccion
    # coah_pop_2 # Poblacion_coahuila
    
    # Puntos para el DTTO 3
    ptos_3 <- st_intersection(coah_pop, secc_1)
    
    partidos <- unique(elec_1$partido)
    
    vec_1 <- lapply(partidos, function(x){
      rep(x, elec_1 %>% 
            filter(partido == x) %>% 
            pull(pp_votos)*nrow(ptos_3))
    })
    
    cols_votos <- sample(unlist(vec_1)) %>% 
      str_replace_all(c("pvem" = "otros", 
                                  "nulos" = "otros", 
                                  "unidos" = "otros", 
                                  "prc" = "otros", 
                                  "udc" = "otros", 
                                  "mc" = "otros", 
                                  "zapata"= "otros",
                                  "prd" = "otros", 
                                  "pt" = "otros"))
    
    if(length(cols_votos)<nrow(ptos_3)){
      cols_votos <- append(cols_votos, 
             rep("otros", (nrow(ptos_3)-length(cols_votos))))
    }
    
    ptos_3 <- cbind(ptos_3, cols_votos)
    
    ptos_3 <- ptos_3 %>% 
      select(cols_votos)
    # st_write(ptos_3, paste0("ptos_por_seccion/seccion_", 
    #                         3, 
    #                         ".geojson"
    #                         ))
    
    saveRDS(ptos_3, paste0("ptos_por_seccion/seccion_", 
                                                    i, 
                                                    ".rds"
                                                    ))
print(paste0("paso: ", i, " completado"))
}

# class(ptos_3)
# 
# ptos_3$cols_votos
# 
# ptos_3 %>% 
#   ggplot(aes(color = cols_votos)) + 
#   geom_sf() + 
#   scale_color_manual(values = c("brown", 
#                                 "gray", 
#                                 "red"))
# 
# 

# rm(ls.all = T)
library(rebus)
# list.files("ptos_por_seccion/") %>% 
#   str_extract(pattern = PUNCT %R% capture(one_or_more(WRD)) %R% END) %>% 
#   table()

files <- paste0("ptos_por_municipio/", 
                list.files("ptos_por_municipio/"))

b <- lapply(files, readRDS)

ptos2 <- do.call(rbind, b)
# write_rds(ptos2, "puntos_coahuila.rds")
write_rds(ptos2, "puntos_coahuila_mpio.rds")
class(ptos2)

# Secciones Electorales ----
# secc <- st_read("COAHUILA DE ZARAGOZA/SECCION.shp") %>% 
#   st_transform(crs = 4326)

# Abrimos Municipios
coah_muni <- st_read("COAHUILA DE ZARAGOZA/MUNICIPIO.shp") %>% 
  st_transform(crs = 4326)

ptos2 %>%
  filter(cols_votos == "morena") %>% 
  ggplot(aes(color = cols_votos)) +
  geom_sf(data = coah_muni, fill = "white", color = "black") +
  geom_sf(alpha = 0.01) +
  scale_color_manual(values = c("brown")) + 
  theme_void()

ggsave("votosMorena.png", 
       device = "png", 
       height = 13, 
       width = 8.5)

ptos2 %>%
  filter(cols_votos == "pri") %>% 
  ggplot(aes(color = cols_votos)) +
  geom_sf(data = coah_muni, fill = "white", color = "black") +
  geom_sf(alpha = 0.01) +
  scale_color_manual(values = c("red")) + 
  theme_void()

ggsave("votosPRI.png", 
       device = "png", 
       height = 13, 
       width = 8.5)


ptos2 %>%
  filter(cols_votos == "pan") %>% 
  ggplot(aes(color = cols_votos)) +
  geom_sf(data = coah_muni, fill = "white", color = "black") +
  geom_sf(alpha = 0.01) +
  scale_color_manual(values = c("blue")) + 
  theme_void()

ggsave("votosPAN.png", 
       device = "png", 
       height = 13, 
       width = 8.5)

# library(leaflet)
# leaflet(secc) %>% 
#   addPolygons(label = secc$seccion) 
# unlist(b)
# ptos <- readRDS("ptos_por_seccion/seccion_1.rds")