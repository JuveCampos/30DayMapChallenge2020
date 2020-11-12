library(rvest)
library(tidyverse)
library(rebus)
library(leaflet)

url <- "http://www.inafed.gob.mx/work/enciclopedia/EMM17morelos/municipios/17006a.html"

code <- read_html(url)
tablas <- html_table(code, fill = T)
presi <- tablas[[10]] %>% as_tibble()

lista <- list()

lista_correcta <- tibble::tribble(
  ~Municipio,	~Lista,
  1,	6,
  2,	7,
  3,	6,
  4,	6,
  5,	6,
  6,	10,
  7,	11,
  8,	6,
  9,	6,
  10,	5,
  11,	5,
  12,	5,
  13,	6,
  14,	6,
  15,	6,
  16,	5,
  17,	5,
  18,	6,
  19,	6,
  20,	7,
  21,	6,
  22,	5,
  23,	5,
  24,	6,
  25,	5,
  26,	6,
  27,	5,
  28,	7,
  29,	8,
  30,	5,
  31,	7,
  32,	5,
  33,	5)


presidentes_morelos <- tibble()

# Hice un loop porque me dio flojera hacer un lapply...
for(i in c(paste0("0", 1:9), 10:33)){
  print(i)
  url <- paste0("http://www.inafed.gob.mx/work/enciclopedia/EMM17morelos/municipios/170",i,"a.html")
  code <- read_html(url)
  tablas <- html_table(code, fill = T)
  
  presi <- tablas[[pull(lista_correcta[i,2])]] %>% as_tibble() %>% 
     mutate(mpio = i)
  
  lista[[i]] <- presi
  
  closeAllConnections()
}

# Convertimos la lista a dataframe
presidentes <- do.call(rbind.data.frame, lista)

# Limpiamos un poco la base 
library(rebus)

pat <- START %R% one_or_more(WRD)

# Extraigo los nombres 
nombres <- presidentes %>% 
  mutate(nombre = str_extract(X1, 
                              pattern = pat)) %>% 
  count(nombre) %>% 
  arrange(-n)

presidentes <- presidentes %>% 
  mutate(nombre = str_extract(X1, 
                              pattern = pat)) 

# El nombre mas comun de presidentes es José
# Nombres de Mujer: Teresa, Silvia, Lilia, Sara, Profa, Pragedis, Ma, Lucila, Julia, Isabel, Hortencia, Guisela, Fluencia, Encarnación, Eloisa, Edith, Dulce, Dra, Delia, Claudia, Buenaventura, Ana, Teofanes, Profra, Marta, Irma, Gloria,  
nombres_mujer <- c("Teresa", 
  "Silvia", 
  "Lilia", 
  "Sara", 
  "Profa", 
  "Pragedis", 
  "^Ma$", 
  "Lucila", 
  "Julia", 
  "Isabel", 
  "Hortencia", 
  "Guisela", 
  "Fluencia", 
  "Encarnación", 
  "Eloisa", 
  "Edith", 
  "Dulce", 
  "Dra", 
  "Delia", 
  "Claudia", 
  "Buenaventura", 
  "^Ana$", 
  "Profra", 
  "Marta", 
  "Irma", 
  "Gloria")  

presidentes <- presidentes %>% 
  filter(!(nombre %in% c("Presidente", 
                         "Nombre", "ND", "nd"))) %>% 
  mutate(NM = str_detect(nombre, pattern = or1(nombres_mujer)))

# Escribimos bases de datos para analizarlas de manera manual 
openxlsx::write.xlsx(lista, "/home/juvenal/Escritorio/presidentes_morelos.xlsx")
openxlsx::write.xlsx(presidentes, "/home/juvenal/Escritorio/presidentes_morelos_v2.xlsx")

# Aca cambie el nombre del archivo procesado a presidentes_morelos_verificado.xlsx
presidentes <- readxl::read_xlsx("presidentes_morelos_verificado.xlsx")

# Me quedo solo con las mujeres
presidentas <- presidentes %>% 
  filter(es_mujer == 1) %>% 
  mutate(CVEGEO = paste0("170", mpio)) %>% 
  mutate(Año_inicio = str_extract(periodo, 
                                  pattern = START %R% one_or_more(DGT)))


# Municipios
# La capa de municipios hay que descargarla de http://www.conabio.gob.mx/informacion/metadata/gis/muni_2018gw.xml?_httpcache%20=%20yes&_xsl=/db/metadata/xsl/fgdc_html.xsl&_indent%20=%20no
# Y colocarla en esta carpeta
mpios <- sf::st_read("muni_2018gw.shp") %>% 
  filter(CVE_ENT == "17") %>% 
  select(CVEGEO, NOM_ENT, NOM_MUN) %>% 
  mutate(CVEGEO = as.character(CVEGEO))

# Claves de municipio 
claves_municipio <- mpios %>% 
  as_tibble() %>% 
  select(CVEGEO, NOM_MUN)

print(claves_municipio, n = Inf)

# Pego datos de poblacion (INEGI, Encuesta Intercensal 2015)
pop_muni <- tibble::tribble(
  ~CVEGEO,   ~ANIO,     ~Pob,
  17001L, 2015L,  17772L,
  17002L, 2015L,  22079L,
  17003L, 2015L,  35689L,
  17004L, 2015L,  85521L,
  17005L, 2015L,   9768L,
  17006L, 2015L, 194786L,
  17007L, 2015L, 366321L,
  17008L, 2015L,  99493L,
  17009L, 2015L,  19231L,
  17010L, 2015L,  17238L,
  17011L, 2015L, 214137L,
  17012L, 2015L,  57121L,
  17013L, 2015L,  15690L,
  17014L, 2015L,   9967L,
  17015L, 2015L,  26713L,
  17016L, 2015L,  18580L,
  17017L, 2015L,  66435L,
  17018L, 2015L, 116143L,
  17019L, 2015L,  27187L,
  17020L, 2015L,  46946L,
  17021L, 2015L,   7772L,
  17022L, 2015L,  20698L,
  17023L, 2015L,   7166L,
  17024L, 2015L,  52110L,
  17025L, 2015L,  33844L,
  17026L, 2015L,  17714L,
  17027L, 2015L,  11992L,
  17028L, 2015L,  68984L,
  17029L, 2015L, 102690L,
  17030L, 2015L,  52651L,
  17031L, 2015L,  36159L,
  17032L, 2015L,   9370L,
  17033L, 2015L,  15844L)

# Pegamos a presidentas el nombre de clave de municipio 
presidentas <- merge(presidentas,claves_municipio, by = "CVEGEO") %>% 
  merge(pop_muni, by = "CVEGEO")

# Genero Atributos para mapa. Requiero Municipio y Numero
atributos <- presidentas %>%
  group_by(CVEGEO, NOM_MUN) %>%
  summarise(Numero_presidentas = n(),
            Informacion_presidentas = paste0("<b style = 'color:green;'>Municipio: </b>", first(NOM_MUN), "<br>",
                                             "<b style = 'color:green;'>Población (2015): </b>", prettyNum(first(Pob), big.mark = ","), " habitantes<br>",
                                             paste0("<b>Nombre: </b>", Nombre, "<br>",
                                                    "<b>Periodo: </b>", periodo, "<br>",
                                                    "<b>Partido: </b>", Partido, "<br>",
                                                    collapse = "<br>")) )

# Juntamos geometrias y atributos:  
mapa <- merge(mpios, atributos, all.x = TRUE)  %>% 
  mutate(Numero_presidentas = ifelse(is.na(Numero_presidentas), 
                                    yes = 0, 
                                    no = Numero_presidentas))

# Hacemos el mapa: 
pal_color <- colorFactor(domain = unique(mapa$Numero_presidentas), 
                          palette = "magma", 
                          na.color = "gray")

# Hacemos el mapa
map <- mapa %>% 
  leaflet() %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(color = "white",
              dashArray = c(1,1),
              weight = 0.7,
              popup = mapa$Informacion_presidentas,
              fillColor = pal_color(mapa$Numero_presidentas), 
              opacity = 1, 
              fillOpacity = 0.8) %>% 
  addLegend(pal = pal_color, 
            values = mapa$Numero_presidentas, 
            position = "bottomright",
            title = "Numero de presidentas<br>que ha tenido cada municipio<br><b style = 'color:red'>Estado de Morelos</b>")
  
htmlwidgets::saveWidget(map, "mapaPresidentas.html")

