########################################################################/
#### Diplomado Ciencia de Datos para políticas Públicas UC (DCDPP) #####
##### Aproximaciones a las políticas públicas desde los datos ##########
############### Taller 1: Introducción a R ############################
########################################################################/
# José Daniel Conejeros - jdconejeros@uc.cl
# Naim Bro -  naim.bro@gmail.com
# Guía del curso: https://naimbro.github.io/programa_diplomado_2021.html
# Talleres: https://www.dropbox.com/sh/wydiai5eilqi85w/AADBxa-AqgGGaYlGfCLEgC7ja?dl=0
# Tutorial Inicial: https://jdconejeros.shinyapps.io/Guia_inicial_R_DSPP/?_ga=2.215615251.544333766.1621133935-1633333738.1619907686
########################################################################/

# En este taller nos enfocaremos en una breve "introducción" a R 
# para el procesamiento datos. 

########################################################################/
### Interactuando con R y RStudio
########################################################################/

# : Comentarios que no se ejecutan como comandos
# + : Sigue el comando en la próxima linea
# ; : Para escribir más de una función en la misma línea
# Recomendación general: todo lo que tenga que ver con procesamiento, análisis de bases de datos, entre otros, 
# deben ser sin tilde ni ñ. Te evitarás bastantes problemas. Además se recomienda en trabajar con minúsculas.

# Directorio 
getwd() #Consultar directorio
mi_directorio <- getwd() #Consultar directorio

setwd("/Users/josedanielconejeros/Dropbox") #Fijar directorio

#Windows (\)
setwd("\Users\josedanielconejeros\Dropbox")

#Mac (/)
setwd("/Users/josedanielconejeros/Dropbox")

setwd(mi_directorio)

# Generar proyectos en R

########################################################################/
# Tema 1: Objetos ----------------------------------------------------
########################################################################/

# 1.1 Vectores y variables  --------------------------------------------

c(1) #Vector de un elemento
c(1, 2 , 3 , 4) #Crear un vector de números
c(1, "hola" , "chao" , 4) #Crear un vector números y carácteres

#Secuencia de valores
1:4
4:1
?seq
seq(1, 4) 
seq(4, 1)
-1:2
seq(-1, 2)
seq(2, 8, by=2)
seq(0, 1, by=0.1)
seq(0, 1, length=11)
rep(0:1, 10)
?length

#Variables 
numeros_palabras <- c(1, "hola" , "chao" , 4)
numeros_palabras

secuencia <- seq(0, 1, length=11)
secuencia

#Operaciones aritméticas con vectores
c(1, 2, 3, 4)/2
(1:4)/2
(1:4)*(4:1)

log(c(0.1, 1, 10, 100), base=10)

c(1, 2 , 3 , 4) + c(4, 3)
c(1, 2 , 3 , 4) + c(4, 3, 2) #Produce warning cuando no son múltiplos

#Operaciones aritméticas con variables 
secuencia <- seq(0, 1, length=11)
secuencia
secuencia <- secuencia*secuencia
secuencia

promedio <- sum(secuencia)/11
promedio

promedio2 <- mean(secuencia) 
promedio2


# 1.2 Factores  ------------------------------------------------

#Almacenamiento de variables categóricas 
#¿Cuál es la diferencia con una variable continua?
#Dummy
sexo <- c("Femenino", "Masculino", "Masculino", "Femenino")
sexo 
sexo[3] 

#Nominales
animales <- c("Elefante", "Jirafa", "Mono", "Caballo")
animales
animales[1]

#Ordinal 
acuerdo <- c("Muy en Desacuerdo", "Desacuerdo", "Ni en desacuerdo", "Ni en desacuerdo" , 
             "Deacuerdo", "Muy deacuerdo")
acuerdo[5]

#¿Cuál es el problema con estas variables?
class(sexo)

#Debemos crear factores numéricos con etiquetas
sexo <- factor(c(0,1,1,0))
sexo
#Generamos etiquetas
sexo <- factor(sexo, labels = c("Mujer", "Hombre"))
sexo

########################################################################/
# Tema 2: Matrices, listas y arreglos   ------------------------------------------
########################################################################/

# 2.1 Matrices  ----------------------------------------------------------------

#Ejemplo de matriz
matrix(1:9,3,3) #Matriz de 3filasx3columnas con valores del 1 al 9

#Matrices como objetos
x <- matrix(1:9,3,3)
x

y <- matrix(1:8,2,4,byrow = FALSE) #Genera una matriz con 2 filas y 4 columnas
y

z <- matrix(1:8,2,4,byrow = TRUE) #Genera la matriz completándola por filas
z
?matrix

#Podemos construir una matriz con datos
edad <- c(23, 45, 67, 89)
sexo <- c(1, 0, 1, 0)
peso <- c(80, 60, 70, 50)
altura <- c(180, 160, 200, 140)
promedio <- mean(altura)

matriz_a <- cbind(edad, sexo)
matriz_a
matriz_b <- cbind(peso, altura)
matriz_b

#Combinar matrices
matriz <- cbind(matriz_a, matriz_b)
matriz
matriz[1,2] #Podemos ver el elemento 1 de la columna 2

#Operaciones matemáticas con matrices
y
z
diff <- y - z
diff

sum <- y + z
sum

# 2.2 Listas  ------------------------------------------

#Creamos una lista
#matriz, valor, vector numérico, vetor de caracteres
objeto <- list(matriz, promedio, sexo, animales) 
objeto

#Vemos los elementos cuatro de la lista: 
objeto <- list(objeto, x)
objeto[[1]]*2
objeto[[2]]

# 2.3 Arreglos  ------------------------------------------

# Matrices Multidimensional
matriz_a <- array(matriz_a, c(4+6))

########################################################################/
# Test 1  --------------------------
########################################################################/
# Instalamos el curso
install.packages('swirl')
library(swirl)
install_course()

#Cargamos el curso
swirl()

# Presione 0 o "escape" para salir del curso una vez enviada la respuesta.

########################################################################/
# Tema 3: Operadores  --------------------------
########################################################################/

# Lógico:
# & Y 
# | O
# ! Negación
# ~ Negación

# Aritmético:
# + Suma
# - Resta
# * Multiplicación 
# / División
# ^ Potencia

# Relacional:
# > Mayor que 
# < Menor que
# >= Mayor o igual que 
# <= Menor o igual que 
# == Igual
# != No igual 
# ~= No igual

# Otros:
# NA Valor perdido

a <- 3
b <- 2.999
c <- 1

c =! a
a == b  # ¿Qué pasa si solo uso =?

a > b 
b < c

a > b & b < c
a > b | b < c

########################################################################/
# Tema 4: Explorar funciones y argumentos  --------------------------
########################################################################/

# 3.1 Funciones  ---------------------------------------------------
a <- c(1)
a <- seq(1:100)
b <- c("uno", "dos", "tres")
log("a")
log(b)
log(a) #Por DEFAULT es logaritmo natural, en base a euler 2,718.
log(a, base=10)
log(2,718) #Cercano a 1 porque euler elevado a 1 = euler
log(100, base=10)
log10(100)

# 3.2 Argumentos  -------------------------------------------------

# Podemos explorar los argumentos de una función 
help("log") #Para saber argumentos (elementos de la función)
?log
args("log")

?sum
args("sum")

# 3.3  Podemos crear nuestras propias funciones -------------------

#Veamos una función simple
fun <- function(x){
  result <- round(sqrt(x /10), digits = 1) # paso 1: subpaso a, b, c
  return(result) # paso 2
}

fun2 <- function(x){mean(x)}

#Usamos las funciones
fun(100)
fun2(100)

#Veamos una función más compleja
func <- function(x){ifelse(-10 <= x  & x < -5, x*2-1,
                           ifelse(-5<=x & x < 0, (3*x-2)/(x+1), 
                                  ifelse(1<=x & x < 5, exp(x+1), "Valor no está en el dominio"))) 
}

func(-11)
func(-7)
func(-2)
func(0)
func(2)
func(6)

########################################################################/
# Tema 5: Iteraciones  --------------------------
########################################################################/

# Veamos una iteración sencilla 
vector <- c()
for(i in 1:10){
  vector[i] <- i^2
}
vector  

# Apliquemos la función anterior

func(-11)
func(-5)
func(0)
func(3)
func(5)
func(11)

vector2 <- c()
for(i in 1:15){
  vector2[i] <- func(i)
}
vector2 

vector3 <- vector()
for(i in -1:-15){
  vector3[i+16] <- func(i)
}
vector3 

########################################################################/
# Test 2  --------------------------
########################################################################/
#Cargamos el curso
swirl()
# Presione 0 o "escape" para salir del curso una vez enviada la respuesta.

########################################################################/
# Tema 6: Uso de librerías/paquetes  -----------------------------------
########################################################################/

rm(list = ls()) #Limpiamos la memoria
library() #Puedo revisar los paquetes instalados
install.packages("libreria") #Para instalar
#Las librerías se instalan sólo una vez, pero deben ser cargadas si se quieren utilizar en la sesión de trabajo
library("libreria") 

install.packages("tidyverse") # Todas las herramientas para análisis de datos
library(tidyverse)
library(dplyr)
dplyr::filter() #Llamar funciones de la libreria
library(help = "dplyr")

#Funciones base de R
library(help = "base") 
search() #Revisamos los paquetes y herramientas instaladas

########################################################################/
# Tema 7: Importar/Explorar/Exportar una base de datos  ----------------
########################################################################/

# 7.1 Importar base en formato .csv:  ----------------------------------------------------------------

data <- read.csv("Talleres/Taller 1/data/simce2m2016_extracto.csv")

# 7.2 Explorar  ----------------------------------------------------------------

# Dada la heterogeneidad de nuestro objeto lo vamos a transformar en un marco de datos
typeof(data)
data <- as.data.frame(data)

dim(data)   # Observaciones y variables
colnames(data) # Nombre de nuestras variables 
str(data)   # Visor de nuestras variables

head(data, n=10)  # Primeras 10 observaciones
head(data[2])      # por nombres
head(data[[2]])    # por ubicación 

tail(data, n=10)  # Últimas 10 observaciones
tail(data[,4], n=10)
tail(data[4,])
tail(data)
tail(15,15)
tail(1:20, 15)

data[,-6] # ¿Qué nos imprime esto?
data[,c(1, 3)] # Veamos la primera y última fila de una BBDD [fila, columna]
data[,c("idalumno","ptje_mate2m_alu")] # ¿Cuál es la diferencia?
data$idalumno
print(data$idalumno)

data[data$cod_depe2 == "Municipal" &  data$ptje_mate2m_alu<100,] # Usemos condiciones 

data[data$ptje_mate2m_alu  %in% c(100, 120),] # Comparar con varios valores

data[data$ptje_mate2m_alu==100 | data$ptje_mate2m_alu==120,] # Equivalente

# 7.3 Modificar  ----------------------------------------------------------------

# Generemos una variable nueva
data$media_ptjes <- (data$ptje_mate2m_alu + data$ptje_lect2m_alu)/2

# Podemos filtrar nuestra base de datos bajo conficiones:
data[data$cod_depe2 == "Municipal" &  data$ptje_mate2m_alu<100,]

# Podemos ordenar nuestra BBDD
data[order(data$ptje_mate2m_alu),]

# 7.3 Descriptivos  ----------------------------------------------------------------

# Estadísticos Descriptivos
# Usando summary
summary(data)
summary(data$ptje_lect2m_alu)
summary(data[4])
summary(data[,4])
summary(data[4,])
library(Hmisc)
describe(data)
sd(data$ptje_lect2m_alu, na.rm = TRUE)
sd(data$ptje_lect2m_alu)

# Tablas de frecuencia 
table(data$cod_depe2)
prop.table(table(data$cod_depe2))
prop.table(table(data$cod_depe2))*100
round(prop.table(table(data$cod_depe2)),2)
sort(prop.table(table(data$cod_depe2))*100, decreasing = TRUE)
View(data)

########################################################################/
# Test 3  --------------------------
########################################################################/
#Cargamos el curso
swirl()
# Presione 0 o "escape" para salir del curso una vez enviada la respuesta.
########################################################################/

# 7.4 Exportamos  ----------------------------------------------------------------

# 7.4.1 Base en formato .txt  ----------------------------------------------------------------
write.table(data, file="Talleres/Taller 1/data/simce_modificada.txt", sep="\t")

# 7.4.2 Base en formato .csv  ----------------------------------------------------------------
write.csv(data, file="Talleres/Taller 1/data/simce_modificada.csv", row.names = FALSE)

# 7.4.3 Base en formato .xlsx  ----------------------------------------------------------------
library(openxlsx)
write.xlsx(data, file="Talleres/Taller 1/data/simce_modificada.xlsx")

# 7.4.4 Base en formato .sav  ----------------------------------------------------------------
library(haven)
write_sav(data, "Talleres/Taller 1/data/simce_modificada.sav")

# 7.4.5 Base en formato .sas  ----------------------------------------------------------------
write_sas(data, "Talleres/Taller 1/data/simce_modificada.sas7bdat")

# 7.4.5 Base en formato .dta  ----------------------------------------------------------------
write_dta(data, "Talleres/Taller 1/data/simce_modificada.dta")

# 7.4.6 Base en formato .Rdata  ----------------------------------------------------------------
save(data, file="Talleres/Taller 1/data/simce_modificada.Rdata")

########################################################################/
# EJERCICIO PROPUESTO -------------
########################################################################/

# Explore la BBDD y trate de describir el siguiente código
#install.packages("dplyr")
library(dplyr)
sum <- function(x, data){
  data %>%
    dplyr::summarize(count=length(na.omit({{ x }})),
                     media=mean({{ x }}, na.rm=TRUE),
                     sd=sd({{ x }}, na.rm=TRUE),
                     min=min({{ x }}, na.rm=TRUE),
                     q25=quantile({{ x }}, na.rm=TRUE, probs=0.25),
                     q50=quantile({{ x }}, na.rm=TRUE, probs=0.50),
                     q75=quantile({{ x }}, na.rm=TRUE, probs=0.75),
                     max=max({{ x }}, na.rm=TRUE))
}

sum(data=data, x=ptje_mate2m_alu)

########################################################################/
# FIN TALLER 1 -------------
########################################################################/

