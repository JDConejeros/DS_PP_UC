
data <- tibble()

# Maquetear 
library(rio)
df <- rio::import("chile.csv")

# 2a.
library(stringr)
#stringr::str_to_lower()
colnames(df) <- tolower(colnames(df)) 
colnames(df)

# 2b.
library(tidyr)
df <- df %>% 
  select(-v1) %>% 
  pivot_longer(cols=!c("area", "months"), names_to="year", values_to = "temperature_change")
#spread y gather

# 2c. 
str_remove("y1961", pattern = "y")
?stringr::str_replace()
str_replace("y1961", pattern = "y", "")

df <- df %>% 
  mutate(year=str_replace(year,"y",""))

# 2d. 
df <- df %>%
  mutate(date=as.Date(paste(months, "15", year), format="%b %d %Y"))

sessionInfo() # Información de mi sesión de R
# Local original: en_US.UTF-8

Sys.setlocale(category = "LC_ALL", "es_ES.UTF-8")
Sys.setlocale(category = "LC_ALL", "en_US.UTF-8")

class(df$year)
class(df$months)

class(df$date)

data <- rbind(data, df)

# Visualización 
# install.packages("ggplot2")
library(ggplot2)

data %>% 
  ggplot(aes(x=date, y=temperature_change)) +
  geom_point(size=0.5, color="orange") +
  geom_line(size=0.5, color="orange") +
  geom_text(data=data %>% filter(date==max(data$date)), 
            aes(label=paste0(temperature_change, "ºC"))
            ) +
  geom_text(data=data %>% filter(date==min(data$date)), 
            aes(label=paste0(temperature_change, "ºC"))
  ) +
  labs(x="Tiempo", y="Cambio de temperatura", 
       title="Chile", caption="Elaboración propia") +
  scale_x_date(breaks = "5 year",
               date_labels = "%Y") +
  theme(axis.text.x = element_text(angle = 45)) 

?theme

