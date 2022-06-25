########################################################################/
#### Diplomado Ciencia de Datos para políticas Públicas UC (DCDPP) #####
##### Aproximaciones a las políticas públicas desde los datos ##########
########### Taller 3: Visualización de datos ######################
########################################################################/
# José Daniel Conejeros - jdconejeros@uc.cl
# Naim Bro -  naim.bro@gmail.com
# Material: https://github.com/JDConejeros/DS_PP_UC
########################################################################/

# En este taller nos enfocaremos:

# 1. Uso de APIS para la extracción de datos y manipulación.

# 2. Introducción a la visualización de datos. 

# 3. Resolver dudas de la tarea en R.

# Desactivar notación científica y limpiar la memoria
options(scipen=999)
rm(list=(ls()))

########################################################################/
# 1. Apliquemos una extracción más compleja en Spotify -------------
########################################################################/

# Vamos a generar funciones más complejas 
# Primero tenemos que configurar una cuenta de desarrollador
# https://developer.spotify.com/dashboard/

install.packages('spotifyr') # Paquete de R que extrae info de la api
install.packages("geniusr") # Herramientas para interactuar con APIS
library(spotifyr)
library(geniusr)
library(dplyr)
library(tidyr)

# Extramoe los datos de la cuenta (hay que crearse una cuenta de desarrollador)

# Generamos token de acceso


# Lista de generos que nos van a interesar
# Aplicaremos una lista reducida para no demandar tanto a la API


# Obtenemos los ids de cada canción para generar la extracción 


# Obtengo las características de las canciones 
# Función que genera la extracción de los identificadores de las canciones 

# Unimos ambas bases de datos 

# Ajustamos los casos duplicados porque pueden estar en más de una lista




########################################################################/
# 2. Trabajo con visualización de datos -------------
########################################################################/

rm(list=ls()[! ls() %in% c("data_spotify")])

# Veamos algunas configuraciones preliminares
#install.packages("ggplot2")
library(ggplot2)
ggplot(data=data_spotify)

## 2.1 Argumentos Básicos ----------------------------------------------------------------
# Datos:input de información para generar la figura. 
# Geometrías (geom_): forma geométrica que representaran los datos,
# Estética (aes()): estética de los objetos geométricos y estadísticos, 
# Escalas (scale): mapas entre los datos y las dimensiones estéticas, 
# Transformaciones estadísticas (stat_): resúmenes estadísticos de los datos
# Sistemas de coordenadas (coord_): mapear los datos del gráfico.
# Facetas:la disposición de los datos en una cuadrícula de gráficos.
# Temas (theme()): ajuste visuales del gráfico (incorpora variados elementos adicionales).

# Es importante ir agregando información por capas

# Generemos un gráfico mejor 

# Aplicación de temas:

# Podemos dejar fijo un tema con el siguiente código:

# theme_bw()
# theme_classic()
# theme_light()
# theme_minimal()

## 2.2 Facetas  ----------------------------------------------------------------

# Podemos agregar facetas


## 2.3 Guardar gráficos  ----------------------------------------------------------------

# Camino 1: Directo

# Camino 2: Múltiples gráficos

#install.packages("ggpubr")
library(ggpubr)

# Camino 3: Ejecutar y guardar

########################################################################/
# 3. Ejemplos de gráficos --------------------
########################################################################/

## 3.1 Histogramas ----------------------------------------------------------------


## 3.2 Gráficos de Barras ----------------------------------------------------------------


## 3.3 Gráficos de torta ----------------------------------------------------------------

# Formato Rosquilla



## 3.4 Box-Plots ----------------------------------------------------------------
library(ggthemes)


########################################################################/
# 4. Podemos realizar extracciones de repositorios -------------
########################################################################/

#install.packages("")
library(stringr)
library(lubridate)
library(RColorBrewer)

# Cargamos la BBDD: 
# Reporte de casos nuevos COVID. Datos en formato ancho:
data <- read.csv("https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto13/CasosNuevosCumulativo.csv")
data %>% glimpse()
View(data)

# Ajustamos como serie de tiempo

# Verifiquemos la cantidad de filas

# Graficamos
# Podemos fijar una paleta de colores:  http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
display.brewer.all(colorblindFriendly = TRUE)
colores <- brewer.pal(n=4, name="Set2")


########################################################################/
# 5. Animar figuras -------------
########################################################################/


########################################################################/
# FIN TALLER 3 -------------
########################################################################/
