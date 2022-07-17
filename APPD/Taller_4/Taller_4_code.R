########################################################################/
#### Diplomado Ciencia de Datos para políticas Públicas UC (DCDPP) #####/
##### Aproximaciones a las políticas públicas desde los datos ##########/
########### Taller 4: API - Visualización geográfica y texto ###########
########################################################################/
# José Daniel Conejeros - jdconejeros@uc.cl
# Naim Bro -  naim.bro@gmail.com
# Material: https://github.com/JDConejeros/DS_PP_UC
########################################################################/

# En este taller nos enfocaremos:

# 1. Uso de APIS para la extracción de datos geográficos.

# 2. Aplicaciones de análisis de texto. 

# 3. Introducción a la programación funcional tipo purrr

# Desactivar notación científica y limpiar la memoria
options(scipen=999)
rm(list=(ls()))

# Vamos a instroducir una función de instala/carga de librerías
# Ajustes preliminares (paquetes)
install_load <- function(packages){
  for (i in packages) {
    if (i %in% rownames(installed.packages())) {
      library(i, character.only=TRUE)
    } else {
      install.packages(i, repos = "http://cran.us.r-project.org")
      library(i, character.only = TRUE)
    }
  }
}

pkg <- c("rio", "dplyr", "tidyr", "lubridate", "stringr", "lubridate", # Manipulación de datos 
         "ggplot2", "ggpubr", "patchwork", # Visualización de datos 
         "googleway", # Extracción de datos geográficos desde la API de google maps
         "ggmap", # Extracción de datos geográficos desde la API de google maps
         "robotstxt", "rvest", # webscrapping
         "tidytext", "topicmodels" # Análisis de texto 
         )  

# Aplicamos nuestra función
install_load(pkg)

# Info
sessionInfo()

########################################################################/
# 1. Extracción de datos geográficos -------------
########################################################################/
# https://cran.r-project.org/web/packages/googleway/googleway.pdf
# En este caso vamos a utilizar la interfaz de aplicación de usuario de google maps.
# Para acceder a la API debemos construir una cuenta: https://developers.google.com/maps/documentation/places/web-service/cloud-setup
# Clave de API: 
# Registre su propia key
key <- ""

# Con esto ya podemos hacer la consulta desde la API de google.
# Aplicamos la función: 
?google_places

## 1.1 Primera extracción -----
google_places(search_string = 'Colegio', # Lo que me interesa buscar 
              location=c(-33.24067764650778, -70.57466430324301), # Ubicación de la zona: lat, long 
              radius=10000, # radio en metros
              key=key)

# Guardemos esto en un objeto
escuelas <- google_places(search_string = 'Colegio', # Lo que me interesa buscar 
                          place_type = "school",
                          location=c(-33.441951012543456, -70.65393022891996), # Ubicación de la zona: lat, long 
                          radius=15000, # radio en metros: mayor radio implica realizar más búsquedas
                          key=key)

# Las funciones place  nos permiten extraer info del objeto generado en la consulta.
df_escuelas <- cbind(
  id=place(escuelas), # ID del lugar
  name=place_name(escuelas), # Nombre del lugar
  place_location(escuelas), # lat y long
  dir=geocode_address(escuelas) # Dirección 
)

dim(df_escuelas)

# Podemos notar que se extraen solo 20 resultados. 
# Esta es una limitación de la librería

## 1.2 Extender las extracciones -----

# Para obtener más resultados 
escuelas2 <- google_places(search_string = 'Colegio', # Lo que me interesa buscar 
                           place_type = "school",
                           location=c(-33.441951012543456, -70.65393022891996), # Ubicación de la zona: lat, long 
                           radius=15000, # radio en metros: mayor radio implica realizar más búsquedas
                           key=key,
                           page_token = escuelas$next_page_token)

data_result <- tibble(rbind(
  df_escuelas, 
  cbind(
    id=place(escuelas2), # ID del lugar
    name=place_name(escuelas2), # Nombre del lugar
    place_location(escuelas2), # lat y long
    dir=geocode_address(escuelas2) # Dirección 
)))

data_result 

# ¿Podemos seguir extrayendo más data
escuelas2$next_page_token

# Nuevamente
escuelas3 <- google_places(search_string = 'Colegio', # Lo que me interesa buscar 
                           place_type = "school",
                           location=c(-33.441951012543456, -70.65393022891996), # Ubicación de la zona: lat, long 
                           radius=15000, # radio en metros: mayor radio implica realizar más búsquedas
                           key=key,
                           page_token = escuelas2$next_page_token)

data_result <- tibble(rbind(
  data_result, 
  cbind(
    id=place(escuelas3), # ID del lugar
    name=place_name(escuelas3), # Nombre del lugar
    place_location(escuelas3), # lat y long
    dir=geocode_address(escuelas3) # Dirección 
  )))

data_result 

escuelas3$next_page_token 
# Ya no podemos obtener más resultados: (1) no hay más data, (2) límite de extracciones
# Solución: Debemos ajustar las coordenadas de búsqueda

## 1.2 Otras aplicaciones útiles -----
# Podemos buscar por coordenadas específicas
escuelas <- google_places(location = c(-33.24067764650778, -70.57466430324301),
              keyword = "school",
              radius = 150,
              key = key)

# Otras funciones de interés
?google_distance() # Permite evualar distancias y tiempo de viaje entre dos lugares.

google_distance(origins = list(c("Diagonal Paraguay 406, Santiago"),
                               c("Campus San Joaquín, Pontificia Universidad Católica de Chile - Vicuña Mackenna, Macul"),
                               c(-33.44068015685966, -70.64114638826919)),
                destinations = c("Palacio de La Moneda"),
                key = key)

## 1.3 Visualizaciones de mapas -----
?google_map

google_map(key = key, location=c(-33.24067764650778, -70.57466430324301), search_box = T) %>%
  add_traffic() %>% 
  add_markers(lat = data_result$lat, lon = data_result$lng, info_window = data_result$name) %>% 
  add_heatmap(lat = -33.38690629782117, lon = -70.56630517207768, option_radius = 0.05)

## 1.4 Extracción a partir de una fuente de datos -----
# Vamos a leer una muestra aleatoria de datos del SIMCE. 
rm(list=ls())
# Registre su propia key
key <- "" # Recuerde siempre usar un key de entrada

# Tengo la información de los colegios 
simce <- import("data/simce_2016_colegios.rds")

set.seed(2020) # Plantamos una semilla para reproducir procesos aleatorios
simce <- simce %>% 
  filter(rbd %in% sample(simce$rbd, size=250))

# Revisemos NA
sapply(simce, function(x) sum(is.na(x)))

# Extraígo direcciones 
dir <- c()
for(i in 1:nrow(simce)){

  query <- google_reverse_geocode(
    location = c(simce$latitud[i], simce$longitud[i]), 
    key = key)

  dir[i] <- geocode_address(query)[1]
  
}

dir

# Agregamos los datos a la fuente original 
simce <- cbind(simce, dir)
  
## 1.5 Otra alternativa de extracción -----
# https://cran.r-project.org/web/packages/ggmap/ggmap.pdf
# Utilizar API-key propia
# Registre su propia key
register_google(key="")

?mutate_geocode

simce2 <- simce %>% mutate_geocode(location = nom_rbd, output = "more")
simce2 <- simce %>% mutate_geocode(location = dir, output = "latlon")

sapply(simce2, function(x) sum(is.na(x)))

# Con esto ya puedo empezar a trabar en visualizaciones

# 1.6 Visualización de datos geográficos -------------

# Podemos usar la librería googleway
# https://cran.r-project.org/web/packages/googleway/vignettes/googleway-vignette.html
google_map(data = simce, key = key) %>%
  #add_markers(lat = "latitud", lon = "longitud", info_window = "nom_rbd") %>% 
  add_heatmap(lat = "latitud", lon = "longitud",option_radius = 0.005,
            weight = 'media_mate', 
            option_gradient = c("plum1", "purple1", "peachpuff"))

# Podemos usar ggmap
# https://datanalytics.com/libro_r/introduccion-a-ggmap.html
# qmap es una versión ajustada de get_map
?qmap
?get_map

qmap("Santiago de Chile", zoom = 10, maptype = "toner", source = "stamen")
qmap("Santiago de Chile", zoom = 10, maptype = "hybrid")
qmap("Santiago de Chile", zoom = 10, maptype = "watercolor", source = "stamen")
qmap("Santiago de Chile", zoom = 10, maptype = "terrain-background", source = "stamen")
qmap("Santiago de Chile", zoom = 10, maptype = "toner-lite", source = "stamen")
qmap("Santiago de Chile", zoom = 10, maptype = "satellite")
qmap("Santiago de Chile", zoom = 10, maptype = "roadmap")

# Vemos el map
stgo <- qmap("Santiago de Chile", zoom = 10, maptype = "roadmap")
stgo

# Lo podemos hacer directo como ggmap
stgo <- ggmap(get_googlemap(center = c(lon = -70.62959070869968, lat = -33.47009262285013),
                            zoom = 10, scale = 2,
                            maptype ='roadmap',
                            color = 'color'))
stgo

stgo + 
  geom_point(aes(x = longitud, y = latitud), data = simce, size = 0.5)

# Vamos a agregar una pequeña modificación a nuestra data SIMCE
simce <- simce %>% 
  mutate(fem=if_else(media_mate>=quantile(media_mate, probs=0.90), "90% superior", "Resto de la muestra"))

# Agreguemos la faceta a nuestro gráfico
stgo + 
  geom_point(aes(x = longitud, y = latitud), data = simce, size = 0.5) +
  facet_wrap(~fem)

# Agreguemos una capa discreta
stgo + 
  geom_point(aes(x = longitud, y = latitud, color=stringr::str_to_title(pago_matricula)), data = simce, size = 2, alpha=1) +
  scale_colour_discrete("Pago Matrícula") +
  labs(x="Longitud", y="Latitud") +
  facet_wrap(~fem) +
  theme_minimal() + 
  theme(legend.position = "right",
        legend.text = element_text(size=8), 
        legend.title = element_text(size=8)) 

# Podemos agregar densidades
# Discretas 
stgo + stat_bin2d(
  aes(x = longitud, y = latitud, colour = pago_matricula, fill = pago_matricula),
  size = .5, bins = 30, alpha = 1/2,
  data = simce)  +
  facet_wrap(~fem)

# Las alternativas vistas en clase SF
# Una buena guía: https://arcruz0.github.io/libroadp/maps.html
# Otra referencia: https://geocompr.robinlovelace.net/spatial-class.html

# Otras fuentes de información con data geográfica
# https://ideocuc-ocuc.hub.arcgis.com/
# https://www.ide.cl/index.php/informacion-territorial/descargar-informacion-territorial

########################################################################/
# 2. Webscrapping y análisis de texto -------------
########################################################################/

# 3.1 Contexto del análisis  --------------------------------------------------
# En este ejercicio exploratorio se propone realizar un análisis temáticos para las columnas de opinión 
# producidas por un centro de investigación autodefinido como independiente para evaluar los temas principales 
# en un período de tiempo o ciertas temáticas de interés.

# 3.2 Scraping de la página --------------------------------------------------
# En general no se observan impedimentos explícitos para operar con los textos respectivos a columnas de opinión
get_robotstxt("https://fundacionsol.cl/blog/actualidad-1/tag/columnas-de-opinion-1316") # Fundación SOL (FS)

# 3.1 Extraer información --------------------------------------------------
# Vamos hacer un testo inicial antes de realizar un procedimiento de extracción completo. 

## a. Títular de las columnas de la primera página ----
enlace1 <- read_html("https://fundacionsol.cl/blog/actualidad-1/tag/columnas-de-opinion-1316")
install.packages("xml2")
library(xml2)

columnas_FS <- enlace1 %>% 
  html_nodes(".h5") %>% 
  html_text(trim = TRUE)

## b. Enlaces para cada columna ----
enlace_columnas_FS <- enlace1 %>% 
  html_nodes(".h5") %>% 
  html_attr("href") 

enlace_columnas_FS <- paste0("https://fundacionsol.cl", enlace_columnas_FS)
enlace_columnas_FS

# c. Guardamos la información en una base de datos ----
baseFS <- tibble(titular = columnas_FS,
                 enlace_columna = enlace_columnas_FS)

#################################################################################################################/
# 4. Webscraping (Completo) + Procesamiento del texto --------------------------------------------------
#################################################################################################################/
# 4.1 Fundación SOL ----
# a. Generamos una función de extracción para todos los tituales y URL de las columnas de opinión----
# Enlaces, titulares y fechas 
obtener_columnasFS <- function(numero_pagina){
  
  Sys.sleep(3)
  
  enlace <- paste0("https://fundacionsol.cl/blog/actualidad-1/tag/columnas-de-opinion-1316/page/", numero_pagina)
  
  html <- read_html(enlace) # Leemos el enlace
  
  FS1 <- html %>% 
    html_nodes(".h5") %>% 
    html_text(trim = TRUE)  # Objeto con el título de la columna
  
  FS2 <- html %>% 
    html_nodes(".h5") %>% 
    html_attr("href")
  
  FS2 <- paste0("https://fundacionsol.cl", FS2) # Enlace de las columnas
  
  tibble(titulo = FS1, 
         enlace_columna = FS2) 
}
# Generamos una data con la información 
library(purrr)
ColumnasFS <- map_df(1, obtener_columnasFS) # Ajustamos las primeras 12 columnas
ColumnasFS

# b.  Generamos una función para extraer fechas -----
fechas_FS1 <- function(enlace){
  Sys.sleep(3)
  read_html(enlace) %>%
    html_node("head > meta:nth-child(12)") %>% 
    html_attr(name="content") %>% 
    str_extract("^.{10}") %>% 
    as_date() %>% 
    format("%d-%m-%Y")
}

# Agregamos las fechas a la base de datos 
ColumnasFS$fechas <- map(ColumnasFS$enlace_columna[1:nrow(ColumnasFS)], fechas_FS1) %>%
  unlist() 

# Ajustamos la información de las fechas separando días-mes-año para luego filtrar los meses-años de interés.
ColumnasFS <- ColumnasFS %>% mutate(fechas=as_date(as.character(fechas), format="%d-%m-%Y"),
                                    dia=day(fechas),
                                    mes=month(fechas),
                                    year=year(fechas)) 

ColumnasFS

# Filtramos con la información respectiva a 2019 - 2020 
ColumnasFS2 <- ColumnasFS %>% filter(year>=2022)

# c. Generamos una función para agregar el cuerpo de la columna -----
cuerpo_FS <- function(enlace) {
  Sys.sleep(3)
  
  html_noticia <- read_html(enlace)
  
  cuerpo <- html_noticia  %>% 
    html_nodes("#o_wblog_post_content > div.o_wblog_post_content_field.o_wblog_read_text") %>% 
    html_text() %>% 
    toString() 
}

# Agregamos la información a la base de datos
ColumnasFS2$cuerpo <- map(ColumnasFS2$enlace_columna[1:nrow(ColumnasFS2)], cuerpo_FS) %>% unlist() 

ColumnasFS2

# Ajustamos los textos
glimpse(ColumnasFS2)

# Guardamos la información extraída en un base de datos 
install.packages("glue"); library(glue)
write.csv(ColumnasFS2, glue("data/FS_{today()}.csv"), row.names = F)

#################################################################################################################/
# 5. Análisis de texto --------------------------------------------------
#################################################################################################################/
# 5.1 Preparación de los textos --------------------------------------------------
# Una vez guardado los texto en csv, procedemos a limpiar la memoria para ajustar los análisis
rm(list = ls())

# Cargamos las base de datos 
FS <- read.csv("data/FS_2021-08-07.csv")

# Generamos las stopwords
algunas_stopwords <- read_csv("https://raw.githubusercontent.com/7PartidasDigital/AnaText/master/datos/diccionarios/vacias.txt")
mas_stopwords <- tibble(palabra = c("va", "tiene", "ser", "puede", "dice", "hacer", "hace", "columnas", "columna"))

# Unimos ambos objetos:
las_stopwords <- bind_rows(algunas_stopwords, mas_stopwords)

# 5.2 Análisis de bigramas --------------------------------------------------
# A continuación se presenta un análisis de bigramas dada las temáticas de análisis. 

# a. Espacio Público  --------------------------------------------------
# Generamos la información del bigrama
bigramasEP <- EP %>% 
  unnest_tokens(input = cuerpo,
                output = palabra,
                token = "ngrams",
                n = 2) %>% 
  filter(!is.na(palabra)) %>% 
  count(palabra, sort = TRUE) %>% 
  separate(palabra,
           into = c("palabra_1", "palabra_2"),
           sep = " ") %>% 
  filter(!palabra_1 %in% las_stopwords$palabra) %>% 
  filter(!palabra_2 %in% las_stopwords$palabra)
bigramasEP
# Guardamos nuestros resultados en excel para el reporte general
openxlsx::write.xlsx(bigramasEP[1:10,], "data/bigramasEP.xlsx")

# b. Libertad y desarrollo  --------------------------------------------------
# Generamos la información del bigrama
bigramasLYD <- LYD %>% 
  unnest_tokens(input = cuerpo,
                output = palabra,
                token = "ngrams",
                n = 2) %>% 
  filter(!is.na(palabra)) %>% 
  count(palabra, sort = TRUE) %>% 
  separate(palabra,
           into = c("palabra_1", "palabra_2"),
           sep = " ") %>% 
  filter(!palabra_1 %in% las_stopwords$palabra) %>% 
  filter(!palabra_2 %in% las_stopwords$palabra)
bigramasLYD
# Guardamos nuestros resultados en excel para el reporte general
openxlsx::write.xlsx(bigramasLYD[1:10,], "data/bigramasLYD.xlsx")

# c. Fundación SOL  --------------------------------------------------
# Generamos la información del bigrama
bigramasFS <- FS %>% 
  unnest_tokens(input = cuerpo,
                output = palabra,
                token = "ngrams",
                n = 2) %>% 
  filter(!is.na(palabra)) %>% 
  count(palabra, sort = TRUE) %>% 
  separate(palabra,
           into = c("palabra_1", "palabra_2"),
           sep = " ") %>% 
  filter(!palabra_1 %in% las_stopwords$palabra) %>% 
  filter(!palabra_2 %in% las_stopwords$palabra)
bigramasFS
# Guardamos nuestros resultados en excel para el reporte general
openxlsx::write.xlsx(bigramasFS[1:10,], "data/bigramasFS.xlsx")

# 5.3 LDA  --------------------------------------------------
# Realizamos un análisis temático para cada institución con el fin de observar variantes en las temáticas latentes que se abordan. 

# a. Fundación SOL  --------------------------------------------------
# Ajustamos los textos para el análisis
frecuencias_columnas_FS <- FS %>% 
  mutate(documento = 1:nrow(FS), .before = titulo) %>% 
  unnest_tokens(input = cuerpo, output = palabra) %>% 
  anti_join(las_stopwords) %>% 
  count(documento, palabra, sort = TRUE)

# Generamos el LDA para el análisis temático
columnas_dtm <- frecuencias_columnas_FS %>% 
  cast_dtm(documento, palabra, n)

columnas_lda <- LDA(columnas_dtm, k = 3, control = list(seed= 1234))

columnas_temas <- tidy(columnas_lda, matrix = "beta")

# Generamos una representación gráfica del análisis
gFS <- columnas_temas %>% 
  group_by(topic) %>% 
  slice_max(beta, n = 10) %>% 
  arrange(topic, -beta) %>%
  mutate(term = reorder_within(term, beta, topic)) %>% 
  ggplot(aes(y = term, beta, fill = factor(topic))) +
  geom_col(show.legend = F)+
  facet_wrap(~topic, scales = "free") +
  scale_y_reordered() +
  theme_light() +
  theme(axis.text=element_text(size=7))
ggsave("FS_LDA.png", gFS)

# b. Libertad y desarrollo  --------------------------------------------------
# Ajustamos los textos para el análisis
frecuencias_columnas_LYD <- LYD %>% 
  mutate(documento = 1:nrow(LYD), .before = titulo) %>% 
  unnest_tokens(input = cuerpo, output = palabra) %>% 
  anti_join(las_stopwords) %>% 
  count(documento, palabra, sort = TRUE)

# Generamos el LDA para el análisis temático
columnas_dtm <- frecuencias_columnas_LYD %>% 
  cast_dtm(documento, palabra, n)

columnas_lda <- LDA(columnas_dtm, k = 3, control = list(seed= 1234))

columnas_temas <- tidy(columnas_lda, matrix = "beta")

# Generamos una representación gráfica del análisis
gLYD <- columnas_temas %>% 
  group_by(topic) %>% 
  slice_max(beta, n = 10) %>% 
  arrange(topic, -beta) %>%
  mutate(term = reorder_within(term, beta, topic)) %>% 
  ggplot(aes(y = term, beta, fill = factor(topic))) +
  geom_col(show.legend = F)+
  facet_wrap(~topic, scales = "free") +
  scale_y_reordered() +
  theme_light() +
  theme(axis.text=element_text(size=7))
ggsave("LYD_LDA.png", gLYD)

# c. Espacio Público  --------------------------------------------------
# Ajustamos los textos para el análisis
frecuencias_columnas_EP <- EP %>% 
  mutate(documento = 1:nrow(EP), .before = titulo) %>% 
  unnest_tokens(input = cuerpo, output = palabra) %>% 
  anti_join(las_stopwords) %>% 
  count(documento, palabra, sort = TRUE)

# Generamos el LDA para el análisis temático
columnas_dtm <- frecuencias_columnas_EP %>% 
  cast_dtm(documento, palabra, n)

columnas_lda <- LDA(columnas_dtm, k = 3, control = list(seed= 1234))

columnas_temas <- tidy(columnas_lda, matrix = "beta")

# Generamos una representación gráfica del análisis
gEP <- columnas_temas %>% 
  group_by(topic) %>% 
  slice_max(beta, n = 10) %>% 
  arrange(topic, -beta) %>%
  mutate(term = reorder_within(term, beta, topic)) %>% 
  ggplot(aes(y = term, beta, fill = factor(topic))) +
  geom_col(show.legend = F)+
  facet_wrap(~topic, scales = "free") +
  scale_y_reordered() +
  theme_light() +
  theme(axis.text=element_text(size=7))
ggsave("EP_LDA.png", gEP)


#################################################################################################################/
# 6. Herramientas de programación funcional --------------------------------------------------
#################################################################################################################/




########################################################################/
# FIN TALLER 4 -------------
########################################################################/
