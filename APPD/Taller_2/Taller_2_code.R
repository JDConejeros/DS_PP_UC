########################################################################/
#### Diplomado Ciencia de Datos para políticas Públicas UC (DCDPP) #####
##### Aproximaciones a las políticas públicas desde los datos ##########
########### Taller 2: Procesamiento y Webscraping ######################
########################################################################/
# José Daniel Conejeros - jdconejeros@uc.cl
# Naim Bro -  naim.bro@gmail.com
# Material: https://github.com/JDConejeros/DS_PP_UC
########################################################################/

# En este taller nos enfocaremos:

# 1. Revisar las principales herramientas de  Tidyverse para la importación, 
# procesamiento y análisis de datos.

# 2. Aplicar herramientas de webscrapping y el uso de API's.

# 3. Simulación del test de código

########################################################################/
# 1. Trabajo con bases de datos -------------
########################################################################/

# Veamos algunas configuraciones preliminares
# Opciones de formato
options(scipen=999) # Desactiva la notación científica.
options(max.print = 99999999) # Nos permite ver más resultados en la consola.

# En esta oportunidad vamos a trabajar con 3 bases de datos públicas
# del Ministerio de educación sobre rendimiento académico.

# Fuente de los datos: https://datosabiertos.mineduc.cl/
# https://datosabiertos.mineduc.cl/rendimiento-por-estudiante-2/

# Revise la carpeta data: ¿Cómo podemos leer bases de datos de manera simultánea? 

# Podemos trabajar guardando nuestro directorio en un objeto
directorio <- getwd()
directorio <- paste0(directorio, "/data") 
directorio

# Veamos los archivos de ese directorio
list.files(directorio, pattern = "*.csv")
list.files(directorio, pattern = "*.rds")

# ¿Cuál es la diferencia entre .csv y .rds?
# Guardemos los nombres de archivos en un vector de caracteres
archivos <- list.files(directorio, pattern = "*.csv")

# Vamos a aplicar la lectura de datos a través de la librería rio 
install.packages("rio")
library(rio)

# Podemos leer las tres bases de datos de forma simultánea con un bucle for
inicio <- Sys.time() # Contemos el tiempo
for(i in archivos){
  df <- rio::import(paste0(directorio, "/", i))
  
  name_df <- paste0("rendimiento", stringr::str_extract(i, pattern = "_20[0-2][0-9]")) 
  
  assign(name_df, df) # Asignamos
}
# Tiempo de ejecución
Sys.time()-inicio

# También podemos usar las funciones maps de la librería purrr (taller 4)

# Limpiamos un poco la memoria 
dfs <-  as.list(ls(pattern = "rend*")) # Vector con nombres de objetos con cierto patrón
rm(list=ls()[! ls() %in% c(dfs, "directorio", "archivos")])

#########################z###############################################/
# 2. Librerias de Tidyverse -------------
########################################################################/
# Puedes revisar el detalle de cada librería en: https://www.tidyverse.org/

# dplyr: procesamiento de variables y datos.
# tidyr: trabajo con bases de datos ordenadas. 
# readr: lectura de bases de datos. 
# purrr: Herramienta para trabajar funciones, vectores e iteraciones. 
# tibble: gestiñon de marco de datos. 
# stringr: trabajo con variables de tipo caracter (textos).
# forcats: trabajo con variables de tipo factor (variables cualitativas). 
# ggplot2: visualización de datos. 

# Para instalar todas estas librerías
install.packages("tidyverse")

########################################################################/
# 3. Exploración de los datos -------------
########################################################################/

# La librería dplyr cuenta con funciones para la exploración y manipulación de datos
library(dplyr) 
# Uso del operador pipe %>% 
rendimiento_2019 %>% glimpse() # Vista previa 
rendimiento_2019 %>% head()    # Primera 6 observaciones
rendimiento_2019 %>% colnames() # Nombres columnas/variables
rendimiento_2019 %>% select(AGNO,NOM_COM_RBD,MRUN) %>% head()

# Primer problema: a veces es un poco problemático trabajar con variables 
# que cuentan mayúsculas o carateres especiales: "ñ", "´".
# Opten por estandarizar el lenguaje

# Veamos los nombres de todas las variables en todos los datos
for(i in dfs){
  print(colnames(get(i)))
}

# Transforma las variables en minúsculas
library(stringr)
colnames(rendimiento_2019) <- str_to_lower(colnames(rendimiento_2019))
rendimiento_2019 %>% colnames()

# También existen alternativas
# tolower de R base 
colnames(rendimiento_2020) <- tolower(colnames(rendimiento_2020))
rendimiento_2020 %>% colnames()

# librería janitor
install.packages("janitor")
library(janitor)
colnames(rendimiento_2021) <- janitor::make_clean_names(names(rendimiento_2021))
rendimiento_2021 %>% colnames()

# Ejercicio propuesto 1: aplicar el for para hacer esto en todas las bases de datos
dfs
for(i in dfs){
  data <- get(i) # Cargo los datos
  
  colnames(data) <- str_to_lower(colnames(data)) # Ajusta los nombres a minuscula
  print(colnames(data)) # Vemos el resultado 
  
  data <- data %>% mutate(ajustada="Si") # Vamos a generar la variable auxiliar para mirar modificaciones
  
  assign(i, data) # Dejamos en el enviroment el resultado
}

rendimiento_2019[, c("mrun", "ajustada")]

########################################################################/
# 4. Procesamiento de datos  -------------
########################################################################/

## 4.1 Select -------------
# Seleccionamos columnas/variables
data1 <- rendimiento_2019 %>% select(23:26,10:14)
data2 <- rendimiento_2019 %>% select("mrun", "agno", "cod_reg_rbd":"nom_com_rbd")
data3 <- rendimiento_2019 %>% select("mrun", "rbd", "cod_depe", "cod_depe2", "cod_ense2", "asistencia")

data1 %>% colnames()
data2 %>% colnames()
data3 %>% colnames()

## 4.2 Rename -------------

data2 <- data2 %>% rename(id=mrun)
data3 <- data3 %>% rename(folio=mrun)

## 4.3 Filter -------------
# Filtramos por una condición 
table(data1$gen_alu) # 1: Masculino // 2: Femenino // 0: ¿?
data_m <- data1 %>% filter(gen_alu==1) # Masculino
data_f <- data1 %>% filter(gen_alu==2) # Femenino

# Generamos dos condicionnes:
data_asistencia <- data3 %>% filter(asistencia>=90 & rbd==3442)
data_asistencia2 <- data3 %>% filter(asistencia>=90 | rbd==3442)

nrow(data_asistencia) # N de la muestra
nrow(data_asistencia2) # N de la muestra

## 4.4 Arrange -------------
# Ordenamos la BBDD con una variable 
head(data2, n=15)

data_arrange <- data2 %>% arrange(cod_reg_rbd)  # Por defecto el orden es creciente
head(data_arrange, n=15)

data_arrange2 <- data2 %>% arrange(desc(cod_reg_rbd)) # Decreciente
head(data_arrange2, n=15)

########################################################################/
# 5. Fundir BBDD -------------
########################################################################/

## 5.1 Append BBDD  -----------------------------------

# Dplyr
glimpse(data_m)
glimpse(data_f)

data_append <- data_m %>% add_row(data_f) 
glimpse(data_append )

# R Base:
data_append2 <- rbind(data_m, data_f)

# ¿Qué pasa cuando no tenemos la misma cantidad de variable 
data_m2 <- data_m %>% select(!gen_alu)

data_append <- data_m2 %>% add_row(data_f) 
data_append2 <- rbind(data_m2, data_f)

# Ya hay una función programada para este escenario
data_append3 <- data_m2 %>% bind_rows(data_f) 

# Veamos que realizo la función para solucionar el problema
data_append3[(1709290:1709310),]
data_append3[(1709290:1709310),"gen_alu"]

## 5.2 Join /Antijoin  -----------------------------------

# Veamos esto con un ejemplo sencillo
x <- data.frame(idx = 1:5, letras = letters[1:5])
y <- data.frame(idy = c(2:6,7), num = c(12:16,3))

x
y

# Trae los resultados de las tablas que cumplen con la condición de comparación entre columnas.
x %>% inner_join(y, by=c("idx"="idy")) #Utilizan una lleva para realizar el match. Solo los match.

# Lo podemos guardar
xy <- x %>% inner_join(y, by=c("idx"="idy"))

# Trae todos los resultados de las tablas que cumplen con la condición de comparación entre columnas
# y, adicionalmente, trae todos los datos de la tabla de la izquierda.
x %>% left_join(y, by=c("idx"="idy"))

# Trae todos los resultados de las tablas que cumplen con la condición de comparación entre columnas
# y, adicionalmente, trae todos los datos de la tabla de la derecha.
x %>% right_join(y, by=c("idx"="idy"))

# Trae los resultados de las tablas que cumplen con la condición de comparación entre columnas, 
# además de los resultados de las o registros de las tablas de la derecha y la izquierda.
x %>% full_join(y, by=c("idx"="idy"))

# Trae los elementos que no tiene información para la base de destino.
x %>% anti_join(y, by=c("idx"="idy"))

# Para el estudio personal
x %>% semi_join(y, by=c("idx"="idy"))

test <- x %>% nest_join(y, by=c("idx"="idy"))

########################################################################/
# 6. Procesamiento de variables -------------
########################################################################/
# Vamos a despejar la memoria 
rm(list=ls()[! ls() %in% c(dfs, "directorio", "archivos")])

## 6.1 Generemos una variable continua  ---------------------------
table(rendimiento_2019$prom_gral)
class(rendimiento_2019$prom_gral)

rendimiento_2019 <- rendimiento_2019 %>% 
  mutate(promedio_ajustado=prom_gral,
         promedio_ajustado=str_replace(promedio_ajustado, pattern = ",", replacement = "."),
         promedio_ajustado=as.numeric(promedio_ajustado),
         promedio_ajustado=if_else(promedio_ajustado==0, NA_real_, promedio_ajustado),
  )

table(rendimiento_2019$promedio_ajustado, useNA = "ifany")

summary(rendimiento_2019$promedio_ajustado) # Vemos el resultado
rendimiento_2019 %>% select(mrun, prom_gral, promedio_ajustado) %>% head()

## 6.2 Generemos un factor (categórica)  ---------------------------

install.packages("sjPlot")
library(sjPlot)
view_df(rendimiento_2019) # Nos permite explorar la metadata

install.packages("labelled")
library(labelled) # Manejador de etiquetas
var_label(rendimiento_2019$gen_alu)
set_variable_labels(rendimiento_2019$gen_alu)

rendimiento_2019 <- rendimiento_2019 %>% mutate(genero=as.numeric(gen_alu)-1,
                                                genero=factor(genero, 
                                                              levels=c(0,1), 
                                                              labels=c("Masculino", "Femenino")))

var_label(rendimiento_2019$genero) <- "Género del estudiante"
var_label(rendimiento_2019$genero)
set_variable_labels(rendimiento_2019$genero)

rendimiento_2019 %>% select(gen_alu, genero) %>% head()
table(rendimiento_2019$gen_alu, rendimiento_2019$genero, useNA = "ifany")

## 6.3 A partir del uso de condicionales  ---------------------------

summary(rendimiento_2019$promedio_ajustado) 
table(rendimiento_2019$promedio_ajustado, useNA = "ifany")
summary(is.na(rendimiento_2019$promedio_ajustado)) # Veamos los missing

# Vamos a generar una variable categórica de 3 grupos
# 25% "inferior" (1)
# 50% "normal"   (2) 
# 25% "superior" (3)

### 6.3.1 if_else()  ---------------------------
quantile(rendimiento_2019$promedio_ajustado, probs = 0.25, na.rm = T)
quantile(rendimiento_2019$promedio_ajustado, probs = 0.75, na.rm = T)

rendimiento_2019 <- rendimiento_2019 %>% 
  mutate(prom_cat = if_else(promedio_ajustado<=quantile(promedio_ajustado, probs = 0.25, na.rm = T), 1, 
                            if_else(promedio_ajustado>quantile(promedio_ajustado, probs = 0.25, na.rm = T) &
                                      promedio_ajustado<quantile(promedio_ajustado, probs = 0.75, na.rm = T),2,
                                    if_else(promedio_ajustado>=quantile(promedio_ajustado, probs = 0.75, na.rm = T), 3, NA_real_))))

# Etiquetamos la variable
var_label(rendimiento_2019$prom_cat) <- "Grupos de rendimiento académico" 

# Validamos 
table(rendimiento_2019$prom_cat, useNA = "ifany")
aggregate(rendimiento_2019$promedio_ajustado~rendimiento_2019$prom_cat, FUN=summary)
rendimiento_2019 %>% select(promedio_ajustado, prom_cat) %>% head(n=20)

### 6.3.2 case_when()  ---------------------------
rendimiento_2019 <- rendimiento_2019 %>% 
  mutate(prom_cat2 =  case_when(promedio_ajustado <= quantile(promedio_ajustado, probs = 0.25, na.rm = T)    ~ 1, 
                                promedio_ajustado > quantile(promedio_ajustado, probs = 0.25, na.rm = T) & 
                                  promedio_ajustado < quantile(promedio_ajustado, probs = 0.75, na.rm = T) ~ 2, 
                                promedio_ajustado >= quantile(promedio_ajustado, probs = 0.75, na.rm = T)    ~ 3, 
                                TRUE ~ NA_real_))

# Validamos 
table(rendimiento_2019$prom_cat, rendimiento_2019$prom_cat2, useNA = "ifany")
rendimiento_2019 %>% select(promedio_ajustado, prom_cat, prom_cat2) %>% head(n=20)

### 6.3.1 Indexación  ---------------------------
rendimiento_2019$prom_cat3[rendimiento_2019$promedio_ajustado <= quantile(rendimiento_2019$promedio_ajustado, probs = 0.25, na.rm = T)] <- 1
rendimiento_2019$prom_cat3[rendimiento_2019$promedio_ajustado >  quantile(rendimiento_2019$promedio_ajustado, probs = 0.25, na.rm = T) & 
                             rendimiento_2019$promedio_ajustado < quantile(rendimiento_2019$promedio_ajustado, probs = 0.75, na.rm = T)] <- 2
rendimiento_2019$prom_cat3[rendimiento_2019$promedio_ajustado >= quantile(rendimiento_2019$promedio_ajustado, probs = 0.75, na.rm = T)] <- 3

table(rendimiento_2019$prom_cat, rendimiento_2019$prom_cat3, useNA = "ifany")
rendimiento_2019 %>% select(promedio_ajustado, prom_cat, prom_cat2, prom_cat3) %>% head(n=20)

# Eliminamos variables creadas
rendimiento_2019$prom_cat2 <- NULL
rendimiento_2019$prom_cat3 <- NULL

# Podemos guardar nuestros ajustes 
saveRDS(rendimiento_2019, "data/rendimiento19.rds")

########################################################################/
# 7. Test de código -------------
########################################################################/

# Instalamos el curso
install.packages('swirl')
library(swirl)

# Veamos el demo
demo_lesson()

# Realicemos nuestro primer test
install_course()

# Cargamos el curso
swirl()

# Sigue cada uno de los pasos que se indican 

# Presione 0 o "escape" para salir del curso una vez enviada la respuesta.

########################################################################/
# 8. Webscrapping y API's -------------
########################################################################/
rm(list=ls())

# Podemos obtener diverso tipo de informaciones a partir del webscraping
# A continuación a modo demostrativo extraeremos información de las canciones más escuchadas en spotify
install.packages('spotifyr') # Paquete de R que extrae info de la api
install.packages("geniusr") # Herramientas para interactuar con APIS

library(spotifyr)
library(geniusr)

# Documentación disponible en: https://www.rcharlie.com/spotifyr/

# Primero tenemos que configurar una cuenta de desarrollador
# https://developer.spotify.com/dashboard/

# Extramoe los datos de la cuenta 
Sys.setenv(SPOTIFY_CLIENT_ID = 'a09106ed23f04ef9aa33a7f565ecaf43')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'f2c4441efbff4abea5d7fc2f5e0bb535')

# Generamos token de acceso
access_token <- get_spotify_access_token()

# Extracciones de información 
radiohead <- get_artist_audio_features('radiohead')
radiohead_top <- get_artist_top_tracks(unique(radiohead$artist_id),
                                       market = "CL", # ISO 3166-1 alfa-2 
                                       authorization = access_token,
                                       include_meta_info = TRUE)

top50_chile <- get_playlist_tracks(playlist_id="37i9dQZEVXbL0GavIqMTeb", 
                                   fields = c("track.name", "track.artists(name)",
                                   ))
top50_chile$country <- "Chile"

top50_hongkong <- get_playlist_tracks("37i9dQZEVXbLwpL8TjsxOG", fields = c("track.name", "track.artists(name)"))
top50_hongkong$country <- "HongKong"

list_plist <- list(top50_chile, top50_hongkong)

all_plist <- tibble(track.artists = NA, track.name = NA, country = NA)

for(i in list_plist){
  all_plist <- all_plist %>% bind_rows(i)
}

get_my_top_artists_or_tracks(type = 'artists', time_range = 'long_term', limit = 5) %>% 
  select(name, genres) %>% 
  rowwise %>% 
  mutate(genres = paste(genres, collapse = ', ')) %>% 
  ungroup()

# En el próximo taller: extracción de datos a partir de la API de google MAPS.


########################################################################/
# 9. Ejericicos propuestos -------------
########################################################################/

# a. Completar los test de código: 
# Test 1: Hasta mañana (29/05/2022) a las 23:59 hrs.  Puede enviar los intentos que quiera.

# Test 2: Hasta el domingo (05/06/2022). Puede enviar los intentos que quiera.

library(swirl)
install_course()

#Cargamos el curso
swirl()

# b. Realizar los siguientes procedimientos para las 3 bases de datos al mismo tiempo.

# b1. Seleccionar las columnas: agno, rbd, cod_reg_rbd, cod_com_rbd, cod_depe, cod_jor, 
# mrun, gen_alu, cod_reg_alu, cod_com_alu, nom_com_alu, prom_gral, asistencia, sit_fin.

# b2. Ajustar el promedio general como una variable numérica, codifique los 0 como NA.

# b3. Ajustar la asistencia a una variable numérica. 

# b4. Ordene los datos de forma descreciente el promedio general.

# b5. Una todas las bases de datos por filas y deje el objeto en su enviroment

# b6. Guarde la base final en un formato .rds.

# Tip: Pude usar un bucle for para esto

# c. Busque un tema que sea de su interés y aplique un breve webscraping

########################################################################/
# FIN TALLER 2 -------------
########################################################################/
