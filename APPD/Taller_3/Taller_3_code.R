########################################################################/
#### Diplomado Ciencia de Datos para políticas Públicas UC (DCDPP) #####/
##### Aproximaciones a las políticas públicas desde los datos ##########/
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
Sys.setenv(SPOTIFY_CLIENT_ID = 'a09106ed23f04ef9aa33a7f565ecaf43')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'f2c4441efbff4abea5d7fc2f5e0bb535') # Disponible hasta el próximo taller.

# Generamos token de acceso
access_token <- get_spotify_access_token()

# Lista de generos que nos van a interesar
# Aplicaremos una lista reducida para no demandar tanto a la API
generos <- c('rap', 'rock', 'latin')# Para cada género tendremos 4 subgeneros

# Agregue más subgeneros para tener mayor información.
subgeneros <- data.frame(genero = c(rep('rap',2), rep('rock',2), rep('latin',2)),
                         subgenero = c('hip hop', 'trap', 
                                       'classic rock','hard rock',
                                       'tropical', 'reggaeton'),
                         stringsAsFactors = FALSE)

playlist_ids <- tibble() # Creamos una tabla vacía para guardar la información de las 20 canciones de estos playlist.

# Aplicamos nuestra primera interación.
# Objetivo: obtener información de las canciones de las playlist por generos
for(g in seq_along(subgeneros$subgenero)){
  
  out <- search_spotify(q = subgeneros$subgenero[g], type = 'playlist', market = 'US', limit = 20) # Obtengo las playlist para el contexto de USA (US)
  out <- out %>%
    select(name, id) %>% # Selecciono solo el nombre de la canción con su identificador único.
    mutate(subgenero = subgeneros$subgenero[g], # Agregamos una colmuna con el subgenero
           genero = subgeneros$genero[g]) # Agregamos una columna con el género
  
  playlist_ids <- rbind(playlist_ids, out)  # Unimos con el data frame vacío de la línea 55
}

# Obtenemos los ids de cada canción para generar la extracción 
playlist_songs <- NULL # Generamos un nuevo objeto vacío

for(i in seq_along(playlist_ids$id)){ # Para cada una de las canciones de for anterior haremos lo siguiente
  
  out <- get_playlist_tracks(playlist_id = playlist_ids$id[i]) # Obtener toda la información de la canción a partir del id. 
  
  out <- out %>%
    filter(!is.na(track.id)) %>% # Filtramos por información id no identificada.
    unnest(cols = 'track.artists') %>% # Sacamos la información de la lista track.artists
    group_by(track.id) %>% # Agrupamos para limpiar los duplicados
    mutate(row_number = 1:n(), 
           track.artist = name) %>% # Generamos un conteo ordenado por grupo para luego filtrar por el primer caso
    ungroup() %>%
    filter(row_number == 1) %>% # Nos quedamos con el primer caso que representa la fila no duplicada
    select(track.id, track.name, track.artist, track.popularity, track.album.id, track.album.name, track.album.release_date) %>% # Nos quedamos con las variables relevantes
    mutate(playlist_name = playlist_ids$name[i], # Agregamos una columna con el nombre de la canción
           playlist_id = playlist_ids$id[i],  # Agregamos una columna con el id
           playlist_genero = playlist_ids$genero[i],  # Agregamos una columna con el genero
           playlist_subgenero = playlist_ids$subgenero[i])   # Agregamos una columna con el subgenero
  
  playlist_songs <- rbind(playlist_songs, out) # Unimos con nuestro objeto de arranque vacío
  
}

# Obtengo las características de las canciones 
# Función que genera la extracción a partir de los identificadores de las canciones 
get_track_features <- function(ids) {
  
  ids <- ids[!is.na(ids)] # limpio mis ids missing 
  len <- length(ids)  # veo el lago de esos ids
  reps <- floor(len/100) * 100 # vemos cuantas veces repetiremos el proceso
  intervalo <- c(seq(from = 0, to = reps, by = 100), len) # un intervalo de repeticiones
  
  data <- data.frame()  # dataframe vacio
  for(r in seq_along(intervalo)){
    inicio <- intervalo[r] # inicio de la extracción 
    fin <- intervalo[r + 1] - 1 # fin de la extracción
    if(is.na(fin)) break # cuando termina no tengo más obs por ende tengo que decirle "break"
    
    info <- get_track_audio_features(ids = ids[inicio:fin]) # Con la info extraigo del id 
    data <- rbind(data, info) # agrego a este objeto vacio de datos la información
    
  }
  
  return(data)
  
}

# Aplico mi función para realizar la extracción con la información de las canciones
head(playlist_songs$track.id) # Aquí están los ids de las canciones que va a utilizar

playlist_audio <- get_track_features(ids = playlist_songs$track.id)

# Unimos ambas bases de datos: canciones con características a partir de los ids  
data_spotify <- playlist_songs %>%
  left_join(select(playlist_audio, -track_href, -uri, -analysis_url, -type, -time_signature), by = c('track.id' = 'id')) %>%
  unique() %>% # Eliminamos duplicados
  filter(!is.na(danceability)) # Quitamos si es que hay missing values

# Ajustamos los casos duplicados porque pueden estar en más de una lista
table(duplicated(data_spotify$track.id))

data_spotify <- data_spotify %>% 
  group_by(playlist_genero, playlist_subgenero, track.id) %>%
  mutate(row_number = 1:n()) %>% # Para limpiar duplicados
  ungroup() %>% 
  arrange(track.id) # Ordenamos por id

data_spotify <- data_spotify %>%   
  filter(row_number == 1) %>% # Filtramos los duplicados
  select(-row_number) # Eliminamos la variable generada para esto

# Finalmente guardamos los datos de nuestra extracción
write.csv(data_spotify, 'data/extraccion_spotify.csv', row.names=FALSE)

########################################################################/
# 2. Trabajo con visualización de datos -------------
########################################################################/

data_spotify <- rio::import("data/extraccion_spotify.csv")

# Nos quedamos solo con el objeto que contiene nuestra extracción de información 
rm(list=ls()[! ls() %in% c("data_spotify")])

# Realicemos unos ajustes preliminares a los datos 
glimpse(data_spotify)

data_spotify <- data_spotify %>% 
  mutate(dur=duration_ms/60000, # Transforma la duración en minutos
         # Transformamos los valores en mayusculas y factores
         genero=factor(stringr::str_to_title(playlist_genero)), 
         subgenero=factor(stringr::str_to_title(playlist_subgenero))) 

# Veamos algunas configuraciones preliminares
#install.packages("ggplot2")
library(ggplot2)
ggplot(data=data_spotify) # Fondo en blanco para comenzar a trabajar

## 2.1 Argumentos Básicos ----------------------------------------------------------------
# Datos:input de información para generar la figura. 
# Geometrías (geom_): forma geométrica que representaran los datos,
# Estética (aes()): estética de los objetos geométricos y estadísticos, 
# Escalas (scale): mapas entre los datos y las dimensiones estéticas, 
# Transformaciones estadísticas (stat_): resúmenes estadísticos de los datos
# Sistemas de coordenadas (coord_): mapear los datos del gráfico.
# Facetas:la disposición de los datos en una cuadrícula de gráficos.
# Temas (theme()): ajuste visuales del gráfico (incorpora variados elementos adicionales).

# Lo más importante es enteder que la información se agrega por capas
ggplot(data=data_spotify, aes(x=dur, y=energy)) #Agrego ejes: capa2
g1 <- ggplot(data=data_spotify, aes(x=dur, y=energy)) #Guardo mi gráfico en un objeto
g1

ggplot(data=data_spotify, aes(x=dur, y=energy)) + 
  geom_point()  #Agrego geometría: points. capa3

g1 + geom_point()

ggplot(data=data_spotify, aes(x=dur, y=energy)) + 
  geom_point() +  #Agrego geometría: points. capa3
  geom_line()  #Agrego geometría: lines. capa3

ggplot(data=data_spotify, aes(x=dur, y=energy))  + 
  geom_point(color="steelblue", shape="triangle", size=0.5, alpha=0.5)  # Ajusto parámetros de la capa

ggplot(data=data_spotify, aes(x=dur, y=energy)) + 
  geom_line(color = "firebrick", linetype = "dotted", size = 0.3)  # Ajusto parámetros de la capa

# Generemos un gráfico mejor 
# Podemos agregar un filtro dentro de la figura
ggplot(data=data_spotify[data_spotify$playlist_subgenero=="reggaeton" & data_spotify$dur<=4,],
       aes(x=dur, y=energy))  + 
  geom_point(size=0.5) + # Puedo ver lo que ocurre por grupos 
  geom_smooth(method = "lm") + # Agregar una línea de tendencia 
  labs(title = "Mi primer ggplot", x="Duración del tema (mins)", y="Indicador de Energía") # Agregamos etiquetas

# Podemos agregar un filtro dentro de la figura 
data_spotify %>% filter(track.artist=="Wisin & Yandel") %>% 
  ggplot(aes(x=dur, y=energy)) +
  geom_point() +
  geom_smooth()

ggplot(data=data_spotify[data_spotify$track.artist=="Wisin & Yandel",], aes(x=dur, y=energy))  + 
  geom_point(size=0.5) + # Puedo ver lo que ocurre por grupos 
  geom_smooth(method = "lm") + # Agregar una línea de tendencia 
  labs(title = "Mi primer ggplot", x="Duración del tema (mins)", y="Indicador de Energía") # Agregamos etiquetas

# Una figura más compleja 
ggplot(data=data_spotify, aes(x=dur, y=energy, color=factor(genero))) + 
  geom_point(size=0.2) + # Puedo ver lo que ocurre por grupos 
  scale_x_continuous(n.breaks = 10, limits = c(0,5)) + 
  labs(color = "Genero musical") + # Una etiqueta a la legenda
  labs(title = "Mi primer ggplot", x="Duración del tema (mins)", y="Indicador de Energía") + # Agregamos etiquetas
  theme_light(base_size=11) +
  theme(plot.title = element_text(size = 15, face="bold"),
        axis.text.x=element_text(size=5),
        axis.text.y=element_text(size=5),
        axis.title.x=element_text(size=10), 
        axis.title.y=element_text(size=10),
        legend.position = "top",
        legend.title = element_text(size=10))

# Aplicación de temas:

# Podemos dejar fijo un tema con el siguiente código:
theme_set(theme_bw())

# theme_bw()
# theme_classic()
# theme_light()
# theme_minimal()

## 2.2 Facetas  ----------------------------------------------------------------

# Podemos agregar facetas
ggplot(data=data_spotify, aes(x=dur, y=energy, color=factor(subgenero))) + 
  geom_point(size=0.5) + # Puedo ver lo que ocurre por grupos 
  scale_x_continuous(n.breaks = 10) + 
  labs(title = "Mi primer ggplot", x="Duración del tema (mins)", y="Indicador de Energía") + # Agregamos etiquetas
  labs(color = "Subgenero") +
  theme_light() +
  theme(plot.title = element_text(size = 15, face="bold"),
        axis.text.x=element_text(size=5),
        axis.text.y=element_text(size=5),
        axis.title.x=element_text(size=10), 
        axis.title.y=element_text(size=10),
        legend.position = "right",
        legend.title = element_text(size=10)) +
  facet_wrap(~genero, ncol = 3, scales="fixed") 

## 2.3 Guardar gráficos  ----------------------------------------------------------------

# Camino 1: Directo
g1 <- ggplot(data=data_spotify, aes(x=dur, y=energy, color=factor(subgenero))) + 
  geom_point(size=0.5) + # Puedo ver lo que ocurre por grupos 
  scale_x_continuous(n.breaks = 10) + 
  labs(title = "Mi primer ggplot", x="Duración del tema (mins)", y="Indicador de Energía") + # Agregamos etiquetas
  labs(color = "Subgenero") +
  theme_light() +
  theme(plot.title = element_text(size = 15, face="bold"),
        axis.text.x=element_text(size=5),
        axis.text.y=element_text(size=5),
        axis.title.x=element_text(size=10), 
        axis.title.y=element_text(size=10),
        legend.position = "right",
        legend.title = element_text(size=10)) +
  facet_wrap(~genero, nrow = 3) 

# Otras alternativas para facet_wrap -> ggarrange, patchwork

# Guardar gráficos (ajustar a ruta específica)
ggsave("figuras/g1.png", plot=g1)

?ggsave

########################################################################/
# 3. Ejemplos de gráficos --------------------
########################################################################/

## 3.1 Histogramas ----------------------------------------------------------------

# Ejemplo para ver la densidad de una variable continua
# aplicamos geom_histogram y sus argumentos
ggplot(data=data_spotify, aes(x=dur)) +
  geom_histogram(aes(y=..density..),
                 position = "identity", binwidth = 0.1,
                 colour="black", fill="white") +
  scale_x_continuous(n.breaks=20) +
  labs(title="Distribución por género",
       x="Energía", y = "Densidad") +
  geom_density(col="red") + 
  facet_wrap(~genero, nrow = 3) +
  theme_bw()

# Veamos todas las áreas

caract <- colnames(select(data_spotify, danceability:dur, -duration_ms))
caract # Vector con el nombre de las características

data_spotify %>%
  select(c(subgenero, caract)) %>%
  pivot_longer(cols = caract) %>%
  ggplot(aes(x = value)) +
  geom_density(aes(color = subgenero), alpha = 0.5) +
  facet_wrap(~stringr::str_to_title(name), ncol = 3, scales = 'free') +
  labs(title = 'Características de las canciones por subgenero',
       x = NULL, y = "Densidad", color="Subgenero") +
  theme_light() +
  theme(plot.title = element_text(size = 10, face="bold"),
        axis.text.x=element_text(size=5),
        axis.text.y=element_text(size=5),
        axis.title.y=element_text(size=10),
        legend.position = "top")

## 3.2 Gráficos de Barras ----------------------------------------------------------------

# En general es mejor trabajar con datos resumidos pues hacen más eficiente la ejecusión de gráficos
tabla <- data_spotify %>% 
  group_by(subgenero) %>% 
  summarise(popu=mean(track.popularity, na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(subgenero=ordered(subgenero, levels=c("Classic Rock", "Hard Rock",
                                             "Hip Hop", "Trap",
                                             "Reggaeton", "Tropical")))

tabla 
levels(tabla$subgenero)

ggplot(tabla, aes(x=subgenero, y=popu, fill=subgenero)) +         
  geom_bar(stat = "identity", position = "dodge", width = 0.75) +
  scale_y_continuous(limits = c(0,100),
                     n.breaks = 5) +
  geom_text(aes(label=format(round(popu,1), decimal.mark=",")), 
            vjust=-0.75,
            size=3,
            color="gray40",
            fontface="bold") +
  labs(title = "Gráfico de popularidad según subgenero",
       subtitle = "Extracciones de Spotify",
       caption = "Elaboración propia.",
       y="Índice de popularidad") +
  scale_fill_brewer(palette = "Set2") +
  geom_vline(xintercept = 2.5, col='red', lwd=0.5, linetype="dotted") + 
  geom_vline(xintercept = 4.5, col='red', lwd=0.5, linetype="dotted") + 
  theme_bw() + 
  theme(plot.title = element_text(size = 10, face="bold"),
        plot.subtitle = element_text(size = 8, color="gray40"),
        plot.caption = element_text(size = 8, color="gray40"),
        axis.text.x=element_text(size=8),
        axis.text.y=element_text(size=8),
        axis.title.y=element_text(size=10),
        axis.title.x = element_blank(),
        panel.background = element_rect(fill="transparent"),
        panel.grid = element_line(color="white"),
        legend.position = "none")


## 3.3 Gráficos de torta ----------------------------------------------------------------

# Construimos una tabla con las proporciones
pie_info <- data_spotify %>%
  filter(!is.na(subgenero)) %>% 
  dplyr::group_by(subgenero) %>%
  dplyr::arrange(subgenero) %>% 
  dplyr::summarize(n_all = n()) %>% 
  dplyr::mutate(prop=(n_all/sum(n_all)*100))

# Definimos colores
colores <- c("#0073C2FF", "#EFC000FF", "#868686FF",
             "#0073C2FF", "#EFC000FF", "#868686FF")

library(RColorBrewer)
RColorBrewer::display.brewer.all()

ggplot(pie_info, aes(x = "", y = prop, fill = subgenero)) +
  geom_bar(stat = "identity", color = "white", width = 1) +
  coord_polar(theta = "y", start = 0) + # Nos permite generar una circunferencia 
  geom_text(aes(x=1, label = paste0(round(prop,1), "%")), position = position_stack(vjust = .5), 
            color = "white", size=5, fontface = "bold") +
  labs(title="Gráfico de torta", caption = "Fuente: Elaboración propia") +
  scale_fill_manual(values = colores, name="Subgenero") +
  theme_void() 

# Es más útil trabajar en un formato tipo rosquilla
# Generamos los datos 

rosquilla_info  <- data_spotify  %>%                               
  filter(!is.na(subgenero)) %>% 
  group_by(subgenero) %>%
  dplyr::summarize(count= n()) %>%
  mutate(categoria=as.factor(subgenero)) %>%
  mutate(fraction=count/sum(count)) %>%
  mutate(percent=round((fraction*100),2)) %>%
  mutate(ymax=cumsum(fraction)) %>% # Nueva variable con la suma acumulada
  mutate(ymin=c(0, head(ymax, n=-1))) %>%
  mutate(labelPosition=(ymax + ymin)/2) %>% # Etiqueta de los valores
  mutate(label= paste0(round(percent,1), "%"))

ggplot(rosquilla_info, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=forcats::fct_inorder(categoria))) +
  geom_rect() +
  ggrepel::geom_label_repel(x=3.5, aes(label = paste(percent,"%"), x = 100, y = (ymin + ymax)/2),inherit.aes = F, show.legend = F, size = 4) +
  scale_fill_brewer(palette=1) +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  labs(fill=" ", size= 3,
       title="Distribución por subgenero")+
  theme_void() +
  theme(legend.position = "top", 
        legend.text=element_text(size=10),
        plot.title = element_text(hjust = 0.15, vjust = 3))

## 3.4 Box-Plots ----------------------------------------------------------------

library(ggthemes)
library(RColorBrewer)

RColorBrewer::display.brewer.all()
paleta <- RColorBrewer::brewer.pal(6, "Set1")

box <- ggplot(data=data_spotify, aes(x=subgenero, y=dur, fill=subgenero)) + 
  geom_boxplot(width=0.5, alpha=0.75) + 
  scale_fill_manual(values=paleta) +
  labs(title = "Gráficos de caja según género",
       subtitle = "Extracciones de spotify",
       caption = "Elaboración propia",
       y="Duración") +
  facet_wrap(~genero, scales = "free_x") + 
  theme_minimal() +
  theme(legend.position = "none",
        axis.title.x = element_blank())
box
  
box + geom_jitter(width = .3, alpha = .3)
box + geom_violin(fill = "gray80", size = 1, alpha = .5)

# Combinemos
box + geom_violin(fill = "gray80", size = 1, alpha = .5) +
  geom_jitter(alpha = .05, width = .1) +
  coord_flip()

## 3.5 Correlaciones ----------------------------------------------------------------

# Preparamos los datos
tabla <- data_spotify %>%
  filter(genero=="Rock") %>% # Filtramos por el género en particular
  select(caract) %>% # Seleccionamos las características de interés
  scale() %>% # dejamos todo en puntajes z
  cor() # Estimamos correlaciones 

# La librería que permite hacer gráficos de correlaciones es corrplot
install.packages("corrplot")
library(corrplot)

# Vamos a guardar nuestro gráfico en un objeto
corrplot(tabla, method = 'color',
         order = 'hclust',
         type = 'upper',
         diag = FALSE,
         addCoef.col = "grey30",
         number.cex = 0.6,
         tl.col = "gray40",
         tl.srt=45,
         tl.cex = 0.7,
         mar = c(rep(0.25, 4)))

## 3.6 Unir gráficos ----------------------------------------------------------------
# Podemos unir varios gráficos en una misma figura, para esto podemos usar: 
# intall.packages("ggpubr") # o también
# intall.packages("patchwork")

library(ggpubr) # Mayor detalle en: https://rpkgs.datanovia.com/ggpubr/reference/ggarrange.html
ggarrange(g1, box, nrow = 2)
ggarrange(g1, box, ncol = 2)

library(patchwork) # Mayor detalle en: https://patchwork.data-imaginist.com/
g1+box

g1|box

g1/box

# Para todos los casos necesitamos los objetos guardados (gg)

########################################################################/
# 4. Podemos realizar extracciones de repositorios -------------
########################################################################/

#install.packages("")
library(stringr)
library(lubridate)

# Cargamos la BBDD: 
# Reporte de casos nuevos COVID. Datos en formato ancho:
data <- read.csv("https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto13/CasosNuevosCumulativo.csv")
data %>% glimpse()
View(data)

# Ajustamos como serie de tiempo
data_figura <- data %>% slice(-17) %>% 
  pivot_longer(cols=!Region, names_to="date", values_to = "casos") %>% # Pivoteamos la BBDD  
  mutate(date=str_replace_all(date, c("[X]"="", "[.]"="-")),           # Reemplazamos valores a partir de una expresión regular
         date=lubridate::ymd(date))                                    # Ajustamos la fecha

# Verifiquemos la cantidad de filas
data_figura %>% head(n=30)
unique(data_figura$Region) # Veamos los valores para las regiones

reg <- unique(data_figura$Region)
reg

# Graficamos
# Podemos fijar una paleta de colores:  http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
display.brewer.all(colorblindFriendly = TRUE)
colores <- brewer.pal(n=4, name="Set2")

g1 <- data_figura %>% 
  ggplot(aes(x=date, y=casos, color=Region)) +
  geom_line() +
  #scale_colour_manual(values=paleta) +
  #scale_color_viridis(discrete = TRUE, option = "D") +
  scale_x_date(date_labels = "%b %Y",
               date_breaks = "12 week", 
               guide = guide_axis(n.dodge = 2)) +
  labs(x="Semanas del año", y = "Casos de Covid-19",
       color = "Región")  +
  geom_vline(xintercept=ymd("2021-02-01"), linetype="dotdash") +
  #annotate("text", x = ymd("2021-02-01")+70, y = 5, label = 'Inicio vacunación', size=3, fontface="bold") + 
  facet_wrap(~factor(Region, levels=reg), ncol = 4, scale = "free") +
  geom_label(aes(x = ymd("2021-02-01"), y = 10, label = "Inicio vacunación"),
             color = "white", size = 2, hjust = "middle", label.size = NA, fill = "#d36f6f") + 
  theme_light(base_size = 12) + 
  theme(axis.text=element_text(size=8),
        axis.text.x = element_text(size=6),
        #axis.text.x = element_text(angle=45, vjust = 0.5, hjust=0.05),
        legend.position="none",
        strip.text = element_text(size = 10, face = "bold", color = "gray40"),
        strip.background = element_rect(fill="white", colour="gray", linetype="dashed"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour="grey", linetype="dotted", size=0.3)) 

g1 # Revisamos nuestro resultado

# Podemos guardar la figura con varios ajustes de formato
ggsave(plot = g1,
       filename = "figuras/g2.png",
       res = 300,
       width = 30,
       height = 15,
       units = 'cm',
       scaling = 0.7,
       device = ragg::agg_png)
?ggsave
########################################################################/
# 5. Animar figuras -------------
########################################################################/

# Podemos crear un gif animado a partir de la librería gganimate
install.packages("gganimate") # más info en: https://gganimate.com/
library(gganimate)

ganimado <- g1 + transition_reveal(date) + 
  labs(title = "Casos de Covid-19 para el día: {frame_along}")  + 
  ease_aes('linear')

animate(ganimado, width = 1200, height = 900, fps = 30, duration = 30, 
        rewind = FALSE, renderer = gifski_renderer("gganim_covid.gif"))

########################################################################/
# FIN TALLER 3 -------------
########################################################################/
