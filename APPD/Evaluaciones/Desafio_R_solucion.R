########################################################################/
#### Diplomado Ciencia de Datos para políticas Públicas UC (DCDPP) #####/
##################### Análisis de datos I ##############################/
########### Solución Desafío R ###########
########################################################################/
# José Daniel Conejeros - jdconejeros@uc.cl
# Material: https://github.com/JDConejeros/DS_PP_UC
########################################################################/

rm(list=(ls())) # Limpiamos el enviroment

fun <- function(areas, desde, hasta){
  
  # 0. Ajustes preliminares -----
  # Revisamos la info de la sesión
  sessionInfo()
  
  # Desactivar notación científica y limpiar la memoria
  options(scipen=999)
  
  Sys.setlocale(category = "LC_ALL", "en_US.UTF-8") # Ajuste de idioma
  
  # 0. Cargamos las librerías de trabajo -----
  # Vamos a instroducir una subfunción de instala/carga de librerías
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
  
  pkg <- c("rio", # Lectura de datos
           "dplyr", "tidyr", "stringr", # Manipulación de datos 
           "ggplot2" # Visualización de datos 
  )
  
  # Aplicamos nuestra subfunción
  install_load(pkg)
  
  # Lectura de la data 
  
  # P1. Lectura de las bases de datos -----
  # Directorios complemetarios
  dir <- getwd()
  dir_files <- paste0(dir, "/APPD/Evaluaciones/data/")
  files <- list.files(dir_files, pattern = ".csv")

  df <- data.frame() # Data frame vacío auxiliar
  data <- data.frame() # Data frame vacío con el resultado final

  for(i in files){
    # Lectura de la data
    df <- import(paste0(dir_files, i))

    # P2. Procesamiento de datos -----

    ## P2a. -----
    colnames(df) <- tolower(colnames(df))
    colnames(df)

    ## P2b. -----
    df <- df %>%
      select(-v1) %>%
      pivot_longer(cols=!c("area", "months"), names_to="year", values_to = "temperature_change")

    ## P2c. -----
    df <- df %>%
      mutate(year=str_replace(year,"y",""))

    ## P2d. -----
    df <- df %>%
      mutate(date=as.Date(paste(months, "15", year), format="%b %d %Y"))

    ## P2e. -----
    df <- df %>%
      select(area, months, year, temperature_change, date)

    data <- rbind(data, df)
  }

  # Devolvemos la tabla final al enviroment
  assign("df", data, 1)
  
  # P3. Construímos nuestro gráfico -----
  
  fig <- data %>%
    filter(area==areas & as.numeric(year)>=desde & as.numeric(year)<=hasta) %>%
    ggplot(aes(x=date, y=temperature_change, group=1)) +
    geom_point(aes(y=temperature_change), shape=1, size=0.5, color="#F5B041") +
    geom_line(aes(y=temperature_change), size=0.75, linetype=1, color="#F5B041") +
    geom_text(data = . %>% filter(date == max(date)),
              aes(label = paste0(round(temperature_change, 1), "ºC")),
              vjust = 0.3, hjust = -0.1,
              show.legend = FALSE, fontface="bold") +
    geom_text(data = . %>% filter(date == min(date)),
              aes(label = paste0(round(temperature_change, 1), "ºC")),
              vjust = 0.3, hjust = 1,
              show.legend = FALSE, fontface="bold") +
    scale_x_date(breaks = "7 year",
                 date_labels = "%Y") +
    labs(x="Serie temporal (años)",
         y = "Cambio de temperatura",
         title=paste("Área:", areas),
         caption="Elaboración propia a partir de los datos de cambio de temperatura entre 1961 - 2019") +
    theme_light(base_size = 10) +
    theme(axis.text.x=element_text(size=10,  vjust=0.5, angle = 0),
          axis.text.y=element_text(size=10),
          plot.title = element_text(size=12, hjust=0.0),
          legend.position="none",
          strip.text = element_text(size = 12, face = "bold", color = "gray40"),
          strip.background = element_rect(fill="white", colour="gray60", linetype="solid"),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank())

  fig

  # Guardamos la figura
  # Debes crear un subdirectorio llamado evaluaciones y otro que se llame figuras_solucion
  subdir <-  "/APPD/Evaluaciones/resultados/grafico_"

  ggsave(plot = fig,
         filename = paste0(dir, subdir, areas, ".png"),
         res = 300,
         width = 25,
         height = 15,
         units = 'cm',
         scaling = 1,
         device = ragg::agg_png)

  # Guardamos la data 
  subdir <-  "/APPD/Evaluaciones/resultados/"
  write.csv(data, paste0(dir, subdir, "df", ".csv"))
  
  
  # Removemos los objetos secundarios
  rm(dir, dir_files, files, i)

  # FIN DE LA FUNCIÓN -----
  
}

# P4. Construímos nuestro gráfico -----

fun(areas="World", desde=1961, hasta=2019)

fun(areas="OECD", desde=1961, hasta=2019)

fun(areas="South America", desde=1961, hasta=2019)

fun(areas="Chile", desde=1961, hasta=2019)





