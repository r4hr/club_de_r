#### Como creo un objeto ####

# Con el símbolo <-
objeto1 <- 20 

# Con el símbolo =
objeto2 = 30

# Con espacios
objeto_3 <- 40 + 50

# En líneas diferentes
objeto_4 <- 50 +
  60


# Paquetes 
install.packages("ggplot2") # Para instalar paquetes
library(ggplot2) # Para cargar/invocar el paquete


#### Capítulo 2 Ciencia de Datos para Gente Sociable ####

# Instalar paquete ggplot2
install.packages("ggplot2")

# Cargar el paquete ggplot2
library(ggplot2)

# Con el símbolo <- (como una flecha a la izquierda) se asignan cosas a los objetos
tipico <- "Hola Mundo"

tipico

# También se puede usar el signo igual, aunque puede confundir en el trabajo
tipica = "Hola Mundo"

tipica

# Dos signos iguales se usa para ver si dos cosas son iguales
tipico == tipica

# Se usa esto # para hacer comentarios 
library(tidyverse)

# devtools::install_github("r-lib/gargle")

#### Libro Ciencia de Datos Capítulo 2 ####

# Cargo los datos que vamos a usar de esta url y lo asigno al objeto mortalidad
mortalidad <- read.csv('https://bitsandbricks.github.io/data/mortalidad_infantil_caba_2016.csv')

# A partir de ahora, cada vez necesite estos datos, sólo invoco el nombre del objeto
mortalidad

# Funciones para explorar datasets
dim(mortalidad) # De dimensión, nos dice la cantidad de columnas y de filas
names(mortalidad) # Nos da los nombres de las variables, muy útil cuando tenemos muchas columnas
head(mortalidad) # Te muestra las primeras 6 filas
tail(mortalidad) # Te muestra las últimas 6 filas
head(mortalidad, 10) # Tanto con head() como con tail() puedo indicar la cantidad de columnas que quiero ver
glimpse(mortalidad) # Indica la cantidad de observaciones, variables, los nombres de columnas, tipo de variables, y una muestra de los primeros registros

# Summary nos da las principales medidas estadísticas de las variables numéricas
# Min y Max (Mínimo y Máximo valor)
# 1st y 3rd Qu. (Primer y tercer cuartil)
# Mean y Median (Promedio y mediana)
summary(mortalidad)


# Nuestro primer gráfico con el paquete ggplot2 (Grammar of graphics plot)

ggplot(mortalidad, aes(x = factor(Comuna), y = Tasa2016)) +
  geom_col()


# Que pasa si no uso la función factor()
ggplot(mortalidad, aes(x = Comuna, y = Tasa2016)) +
  geom_col()


#### Análisis geográfico ####
library(sf)

# Cargo un dataset con información geográfica de los barrios y comunas de la Ciudad Autónoma de Buenos Aires
comunas <- st_read('https://bitsandbricks.github.io/data/CABA_comunas.geojson')

# Exploro el dataset
names(comunas)
head(comunas)

# Hago un gráfico para ver la Ciudad Autónoma de Buenos Aires
ggplot(comunas) +
  geom_sf()

# Le asigno un color por Comuna
ggplot(comunas) +
  geom_sf(aes(fill = comunas))

# Agrega Av. Rivadavia que divide a la ciudad en 2
rivadavia <- st_read('https://bitsandbricks.github.io/data/avenida_rivadavia.geojson')

# Combino los gráficos de Comuna, incluyendo a la Av. Rivadavia 
ggplot(comunas) +
  geom_sf(aes(fill = comunas)) +
  geom_sf(data = rivadavia, color = "red")

#### Agregar nueva columna ####

# Creamos un vector con la región a la que pertenece cada comuna
nueva_columna <- c("Sur", "Norte", "Sur", "Sur", "Sur", "Norte", "Sur", "Sur", 
                   "Sur", "Norte", "Norte", "Norte", "Norte", "Norte", "Norte")

# La función mutate agrega columnas
comunas <- mutate(comunas, ubicacion = nueva_columna) # "ubicacion" es el nombre de la columna nueva

# Acá podemos ver que la columna ubicacion se añadió a la tabla comunas
glimpse(comunas)

# Ahora actualizamos el mapa, coloreando las comunas del Sur y del Norte
ggplot(comunas) +
  geom_sf(aes(fill = ubicacion)) + 
  geom_sf(data = rivadavia, color = "red")

# Agregamos a la tabla de mortalidad la ubicación
mortalidad <- mutate(mortalidad, ubicación = nueva_columna)

head(mortalidad)

#### Analizar la mortalidad por zona de la ciudad ####

# Graficamos para analizar la mortalidad infantil por Comuna
ggplot(comunas) +
  geom_sf(aes(fill = mortalidad$Tasa2016)) + # Con el símbolo $ se elige la columna específica a graficar
  geom_sf(data = rivadavia, color = "red") +
  scale_fill_distiller(palette = "Spectral")

# Si bien los mapas son llamativos, a veces los gráficos de barra son más claros
ggplot(mortalidad) +
  geom_col(aes(x = Comuna, y = Tasa2016, fill = ubicación)) +
  labs(title = "Mortalidad infantil en la Ciudad Autónoma de Buenos Aires", # con esta función le agregamos un título al gráfico
       subtitle = "Año 2016",
       y = "tasa") 

# Dividimos los datos por zona
comunas_al_sur <- filter(mortalidad, ubicación == "Sur")

comunas_al_norte <- filter(mortalidad, ubicación == "Norte")

# Calculamos la diferencia entre el promedio de mortalidad de unas y otras.
mean(comunas_al_sur$Tasa2016) / mean(comunas_al_norte$Tasa2016)


