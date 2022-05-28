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

# Veamos los archivos de ese directorio

# ¿Cuál es la diferencia entre .csv y .rds?
# Guardemos los nombres de archivos en un vector de caracteres


# Vamos a aplicar la lectura de datos a través de la librería rio 

# Podemos leer las tres bases de datos de forma simultánea con un bucle for

# Tiempo de ejecución


# También podemos usar las funciones maps de la librería purrr (taller 4)

# Limpiamos un poco la memoria 

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


# Primer problema: a veces es un poco problemático trabajar con variables 
# que cuentan mayúsculas o carateres especiales: "ñ", "´".
# Opten por estandarizar el lenguaje

# Veamos los nombres de todas las variables en todos los datos

# Transforma las variables en minúsculas
library(stringr)

# También existen alternativas
# tolower de R base 

# librería janitor
install.packages("janitor")
library(janitor)

# Ejercicio propuesto 1: aplicar el for para hacer esto en todas las bases de datos

########################################################################/
# 4. Procesamiento de datos  -------------
########################################################################/

## 4.1 Select -------------
# Seleccionamos columnas/variables


## 4.2 Rename -------------



## 4.3 Filter -------------
# Filtramos por una condición 



# Generamos dos condicionnes:



## 4.4 Arrange -------------
# Ordenamos la BBDD con una variable 



########################################################################/
# 5. Fundir BBDD -------------
########################################################################/

## 5.1 Append BBDD  -----------------------------------

# Dplyr


# R Base:


# ¿Qué pasa cuando no tenemos la misma cantidad de variable 


# Ya hay una función programada para este escenario


# Veamos que realizo la función para solucionar el problema

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

test_data <- x %>% nest_join(y, by=c("idx"="idy"))

########################################################################/
# 6. Procesamiento de variables -------------
########################################################################/
# Vamos a despejar la memoria 
rm(list=ls()[! ls() %in% c(dfs, "directorio", "archivos")])

## 6.1 Generemos una variable continua  ---------------------------



## 6.2 Generemos un factor (categórica)  ---------------------------

install.packages("sjPlot")
library(sjPlot)


install.packages("labelled")
library(labelled) # Manejador de etiquetas

## 6.3 A partir del uso de condicionales  ---------------------------

summary(rendimiento_2019$promedio_ajustado) 
table(rendimiento_2019$promedio_ajustado, useNA = "ifany")
summary(is.na(rendimiento_2019$promedio_ajustado)) # Veamos los missing

# Vamos a generar una variable categórica de 3 grupos
# 25% "inferior" (1)
# 50% "normal"   (2) 
# 25% "superior" (3)

### 6.3.1 if_else()  ---------------------------



### 6.3.2 case_when()  ---------------------------



### 6.3.1 Indexación  ---------------------------



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
Sys.setenv(SPOTIFY_CLIENT_ID = '')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '')


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
