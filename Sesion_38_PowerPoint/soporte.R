library(tidyverse)
library(flextable)

# Previa ----
# https://github.com/r4hr/kiwi2022/blob/main/encuesta-kiwi-2022.Rmd
verde <-  "#1FC3AA"
lila <- "#8624F5"

# Cargar datos de la Encuesta KIWI de Sueldos de RH 2022
rh <- read.csv("Sesion_38_PowerPoint/kiwi22_clean.csv",
               sep = ";")


rh %>% 
  group_by(puesto) %>% 
  summarise(promedio = mean(sueldo_dolar))


analisis1 <- rh %>% 
  group_by(puesto,genero) %>% 
  summarise(sueldo_mediana = median(sueldo_dolar)) %>% 
  ungroup()

ggplot(analisis1, aes(x = puesto, y = sueldo_mediana, fill = genero)) +
  geom_col(position = "dodge")

tabla1 <- analisis1 %>% 
  pivot_wider(id_cols = puesto, 
              names_from = genero,
              values_from = sueldo_mediana)
tabla1
