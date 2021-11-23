# Centro ---------------------------------------
library(tidyverse)
library(sf)

comunas <- st_read('https://bitsandbricks.github.io/data/CABA_comunas.geojson')

comuna_2 <- comunas %>% 
  filter(comunas == 2)

comuna_2 <- comuna_2 %>% 
  st_centroid()

ggplot()+
  geom_sf(data=comunas, color="gray")+
  geom_sf(data=comuna_2, color="red", shape=4, stroke=2, size=1) +
  theme_minimal()


# k-means Pablo Casas -------------------------
library(funModeling)
library(DataExplorer)

sysarmy <- read_delim("Datasets/sysarmy_clean.csv", delim = ";")

status(sysarmy)

# Seleccionamos variables categóricas
variables_c <- status(sysarmy) %>% 
  filter(type=="character" & unique<10) %>% 
  pull(variable)

variables_c

variables_n <- status(sysarmy) %>% 
  filter(type=="numeric") %>% 
  pull(variable)

describe(sysarmy$Tengo)

# Candidatas:
v_sel <- c("X.Tenés.guardias.", 
           "X.Programás.como.hobbie.", 
           "X.Tuviste.ajustes.por.inflación.en.2019.", 
           "Salario.mensual.BRUTO..en.tu.moneda.local.",
           "Tengo")

# Copiado descaradamente del script de Pablo Casas
d5=select(sysarmy, all_of(v_sel))

status(d5)

glimpse(d5)

d5_dum=dummify(d5)  # variables dummy o one hot encoding

glimpse(d5_dum)


# Explorar:
status(d5_dum)


# Escalar para kmeans!
d6=scale(d5_dum)
d6

fit_cluster=kmeans(d6, 3)

# Copia!
d_fit=d5_dum

# Asignacion del cluster
d_fit$cluster=fit_cluster$cluster

d_fit

# Visualizando. Vamos a contar una historia!!
coord_plot(data=d_fit, 
           group_var="cluster", 
           group_func=mean, 
           print_table=TRUE)








# Foto -----------------------------------------

library(jpeg)
library(scales)
library(extrafont) # la primera vez ejecutar font_import()

loadfonts()
font <- "Nunito"


foto <- readJPEG("Archivos/beauty.jpg")

class(foto)

str(foto)

foto_dim <- dim(foto)
foto_dim

foto_rgb <- data.frame(
  x = rep(1:foto_dim[2], each = foto_dim[1]),
  y = rep(foto_dim[1]:1, foto_dim[2]),
  R = as.vector(foto[,,1]), #slicing our array into three
  G = as.vector(foto[,,2]),
  B = as.vector(foto[,,3]))

head(foto_rgb)

summary(foto_rgb)


foto_kmeans <- kmeans(foto_rgb[,c("R","G","B")], centers = 20, iter.max = 30)

str(foto_kmeans)

# La paleta de colores de la foto de mi hija
show_col(rgb(foto_kmeans$centers))

# Los datos
analisis <- sysarmy %>%
  select(Trabajo.de, Salario.mensual.BRUTO..en.tu.moneda.local.) %>%
  rename(Puesto = Trabajo.de,
         Sueldo_Bruto = Salario.mensual.BRUTO..en.tu.moneda.local.)

top_20_puestos <- analisis %>%
  select(Puesto, Sueldo_Bruto) %>%
  group_by(Puesto) %>%
  tally(sort = TRUE) %>% 
  top_n(20) %>%
  select(Puesto)

top_20_puestos <- as.vector(top_20_puestos$Puesto)

analisis %>%
  filter(Puesto %in% top_20_puestos) %>%
  group_by(Puesto) %>%
  summarise(Sueldo_Promedio = mean(Sueldo_Bruto)) %>%
  ungroup() %>% 
  ggplot(aes(x = reorder(Puesto, Sueldo_Promedio), y = Sueldo_Promedio, fill = Puesto))+
  geom_col(position = "dodge") +
  scale_fill_manual(values = rgb(foto_kmeans$centers))+
  scale_y_continuous(labels = scales::comma_format(big.mark = ".", decimal.mark = ","))+
  coord_flip()+
  theme(panel.background = element_blank(),
        panel.grid.major.x = element_line(colour = "#D7DBDD"),
        text = element_text(family = "Ubuntu Mono"),
        legend.position = "bottom",
        legend.text = element_text(size = 6))+
  labs(title = "Sueldo bruto promedio por puesto y género (en AR$)",
       subtitle = "Fuente: Encuesta de Sueldos de Sysarmy",
       caption = "Club de R para RRHH",
       x = "",
       y = "")


