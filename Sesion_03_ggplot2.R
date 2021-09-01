s <- livecode::serve_file() # No copien esta l?nea de c?digo

#### Librerías ####


library(googlesheets4) 
library(gargle)
library(tidyverse)
library(ggthemes)
library(lubridate)

options(scipen = 999) # Cambia la notaci?n cient?fica de los gr?ficos

#### Archivos ####

# Carga el archivo desde google sheets.
nomina <- sheets_read("1UliFjEjab9skkSGp_QVYv3ZQc132dqBLV-97vppQtAg")
puestos <- sheets_read("1UliFjEjab9skkSGp_QVYv3ZQc132dqBLV-97vppQtAg", sheet = "Puestos")
encuesta_sysarmy <- sheets_read("1_db6zEAMvr-1GQjJb4hV-rSQfJ9w6GmezbqKJ2JJn7I", skip = 9) # Con este vamos a trabajar despu?s
hr_data <- read.csv("HRDataset_v13.csv")

#### Tarea de la semana anterior ####

mensuales <- nomina %>%
  mutate(ID_CAT = unlist(ID_CAT),
         ID_CAT = as.factor(ID_CAT)) %>%
  filter(!ID_CAT %in% c("1", "2", "3", "4", "5")) %>%
  left_join(puestos) %>%
  mutate(Rangos_Edad = case_when(
    EDAD %in% 18:30 ~ "Hasta 30",
    EDAD %in% 31:40 ~ "Entre 31 y 40",
    EDAD %in% 41:50 ~ "Entre 41 y 50",
    EDAD %in% 51:70 ~ "M?s de 50"),
    Rangos_Edad = factor(Rangos_Edad, levels = c("Hasta 30", "Entre 31 y 40",
                                                 "Entre 41 y 50", "M?s de 50")))


# Calcular sueldos promedios
sueldos_promedios <- mensuales %>%
  group_by(PUESTO) %>%
  summarise(Sueldo_Promedio = mean(SUELDO)) %>%
  arrange(-Sueldo_Promedio)

sueldos_promedios

#  Crear rangos de edad con *case_when()* y calcular el sueldo promedio.
mensuales <- mensuales %>%
  mutate(Rangos_Edad = case_when(
    EDAD %in% 18:30 ~ "Hasta 30",
    EDAD %in% 31:40 ~ "Entre 31 y 40",
    EDAD %in% 41:50 ~ "Entre 41 y 50",
    EDAD %in% 51:70 ~ "M?s de 50"),
    Rangos_Edad = factor(Rangos_Edad, levels = c("Hasta 30", 
                                                 "Entre 31 y 40",
                                                 "Entre 41 y 50",
                                                 "M?s de 50"))) 

sueldo_edad <- mensuales%>%
  group_by(Rangos_Edad) %>%
  summarise(Sueldo_Promedio = mean(SUELDO))

sueldo_edad


# ¿En qu? puestos hay una mayor diferencia entre el sueldo m?ximo y el m?nimo?

# Primero hay que filtrar los puestos que tengan s?lo una persona  

dif_sueldos <- mensuales %>%
  filter(PUESTO != "GERENTE GENERAL") %>% # Lo saco porque s?lo hay 1 puesto para comparar
  group_by(PUESTO) %>%
  summarise(Sueldo_Min = min(SUELDO),
            Sueldo_Max = max(SUELDO),
            Dif_Sueldo = Sueldo_Max - Sueldo_Min) %>%
  arrange(-Dif_Sueldo)

dif_sueldos  



#### Sesi?n 3 - Gr?ficos con ggplot2 ####

# Creamos un objeto con los datos de n?mina y de puestos.

mensuales <- nomina %>%
  mutate(ID_CAT = unlist(ID_CAT),
         ID_CAT = as.factor(ID_CAT)) %>%
  filter(!ID_CAT %in% c("1", "2", "3", "4", "5")) %>%
  left_join(puestos) %>%
  mutate(Rangos_Edad = case_when(
    EDAD %in% 18:30 ~ "Hasta 30",
    EDAD %in% 31:40 ~ "Entre 31 y 40",
    EDAD %in% 41:50 ~ "Entre 41 y 50",
    EDAD %in% 51:70 ~ "M?s de 50"),
    Rangos_Edad = factor(Rangos_Edad, levels = c("Hasta 30", "Entre 31 y 40",
                                                 "Entre 41 y 50", "M?s de 50")))
head(mensuales)
glimpse(mensuales)

#### Histograma ####

# Histograma b?sico
ggplot(mensuales, aes(SUELDO)) +
  geom_histogram()

# Cambiando la cantidad de bins
ggplot(mensuales, aes(SUELDO))+
  geom_histogram(bins = 15)+
  ggtitle("bins = 15")

# Cambiando el rango de los bins
ggplot(mensuales, aes(SUELDO))+
  geom_histogram(binwidth = 2500)+
  ggtitle("bindwidth = $2500")


#### Boxplots ####

# Boxplot b?sico
ggplot(mensuales, aes(SUELDO)) +
  geom_boxplot()

# Boxplot separado por puesto (variable categ?rica)
ggplot(mensuales, aes(x= PUESTO, y = SUELDO)) +
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90))

# Tambi?n se puede invertir con coord_flip()
ggplot(mensuales, aes(x= PUESTO, y = SUELDO)) +
  geom_boxplot()+
  coord_flip()

#### Gr?fico de barras ####

# geom_col nos permite hacer gr?ficos de barra, y tenemos que especificar los valores de x y de y
# geom_col grafica el valor que figura en la observaci?n
mensuales %>%
  group_by(PUESTO) %>%
  summarise(Sueldo_Promedio = mean(SUELDO)) %>%
  ggplot(aes(x = PUESTO, y = Sueldo_Promedio))+
  geom_col()+ ggtitle("geom_col") +
  theme(axis.text.x = element_text(angle = 90))


# geom_bar cuenta la cantidad de observaciones que tenemos en el dataset
ggplot(mensuales, aes(x = PUESTO))+
  geom_bar()+ ggtitle("geom_bar")+
  theme(axis.text.x = element_text(angle = 90))

# Con geom_bar podemos hacer los gr?ficos apilados
ggplot(mensuales, aes(x = AREA, fill = Rangos_Edad))+
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90))+
  ggtitle("Empleados por rango de edad por ?rea")

# Tambi?n podemos mostrar la proporci?n de las variables en un gr?fico apilado al 100%
# dentro de geom_bar modificamos el par?metro position = "fill"
ggplot(mensuales, aes(x = AREA, fill = Rangos_Edad))+
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))+
  ggtitle("Proporci?n de rangos de edad por ?rea")+
  coord_flip()+
  scale_fill_colorblind() # funci?n del paquete ggthemes

# Tambi?n podemos poner una columna al lado de la otra.
# Usamos el par?metro position = "dodge"
mensuales %>%
  filter(AREA %in% c("LAMINADO", "TEMPLADO")) %>%
  ggplot(aes(x = AREA, fill = Rangos_Edad))+
  geom_bar(position = "dodge")+
  ggtitle("Rangos de edad en Laminado y Templado")+
  scale_fill_colorblind() # funci?n del paquete ggthemes

# Cuando en el eje x no se grafica una variable ordinal, podemos ordenar el gr?fico. Comparemos.

# Gr?fico sin ordenar
mensuales %>%
  group_by(PUESTO) %>%
  summarise(Sueldo_Promedio = mean(SUELDO)) %>%
  ggplot(aes(x = PUESTO, y = Sueldo_Promedio))+
  geom_col()+
  ggtitle("Sueldos promedios por puestos")+
  theme(axis.text.x = element_text(angle = 90))

# Gr?fico ordenado de mayor a menor con reorder()
mensuales %>%
  group_by(PUESTO) %>%
  summarise(Sueldo_Promedio = mean(SUELDO)) %>%
  ggplot(aes(x = reorder(PUESTO, -Sueldo_Promedio), y = Sueldo_Promedio))+
  geom_col()+
  ggtitle("Sueldos promedios por puesto")+
  theme(axis.text.x = element_text(angle = 90))


#### L?neas ####

# Para los gr?ficos de l?nea vamos a usar un dataset descargado de Kaggle
# Tambi?n vamos a usar funciones del paquete lubridate para asegurarnos de cargar bien las fechas
library(googledrive)

temp_data <- drive_get(path = "HRDataset_v13.csv") 

# Descarga el archivo a la computadora, en la carpeta del proyecto
drive_download(file = "HRDataset_v13.csv",
               overwrite=TRUE) 

datos_rh <- read.csv("HRDataset_v13.csv")

# Vamos a crear un objeto s?lo con los empleados dados de baja
hr_terminate <- hr_data %>%
  select(Sex, DateofTermination) %>%
  filter(DateofTermination != "") %>%
  mutate(DateofTermination = mdy(DateofTermination), # Le indica a R cu?l es el formato de la fecha
         Year_Termination = year(DateofTermination), # Extrae el año de la fecha.
         Sex = factor(Sex),
         Count = 1) %>%
  group_by(Year_Termination, Sex) %>%
  summarise(Terminations = sum(Count))

# Gr?fico de l?neasa
ggplot(hr_terminate, aes(x = Year_Termination, y = Terminations, color = Sex))+
  geom_line(size = 1)

# Gr?fico mejorado
ggplot(hr_terminate, aes(x = Year_Termination, y = Terminations, color = Sex))+
  geom_line(size = 1) +
  geom_point(size = 2)+
  theme_minimal() +
  labs(title = "Bajas por año y género",
       x = "",
       y = "")

#### Scatter plots ####

# Ahora vamos a usar los datos de la encuesta de sueldos de Sysarmy

# Creamos un vector con las filas que nos interesan y lo pasamos a un nuevo objeto
seleccion <- c("Años de experiencia","Trabajo de","Salario mensual BRUTO (en tu moneda local)", "Me identifico" )

sysarmy <- encuesta_sysarmy %>%
  select(seleccion)

# Limpiamos un poco el dataset
sysarmy <- sysarmy %>%
  rename(Sueldo = "Salario mensual BRUTO (en tu moneda local)",
         Puesto = "Trabajo de",
         Experiencia = "Años de experiencia",
         Sexo = "Me identifico") %>%
  filter(between(Sueldo,20000,1000000)) %>%
  mutate(Experiencia = unlist(Experiencia), 
         Experiencia = as.numeric(Experiencia),
         Puesto = factor(Puesto),
         Sexo = factor(Sexo))

summary(sysarmy) # Nos aseguramos que los datos sean coherentes.

# Elegimos sólo 3 puestos, los que tienen más observaciones
analisis_puestos <- sysarmy %>%
  filter(Puesto %in% c("Developer", "SysAdmin / DevOps / SRE", "Technical Leader"))

# Scatter plot básico con dos variables numéricas
ggplot(analisis_puestos, aes(x = Sueldo, y = Experiencia))+
  geom_point()

# Scatter plot básico con una variable categórica, y una variable numérica
ggplot(analisis_puestos, aes(x = Puesto, y = Sueldo)) +
  geom_point()

# Mejorando este último gráfico con los parámetros jitter, alpha y size
ggplot(analisis_puestos, aes(x = Puesto, y = Sueldo, color = Sexo)) +
  geom_point(position = "jitter", alpha = 0.5, size = 2) +
  scale_color_colorblind() + # Del paquete ggthemes
  labs(title = "Sueldos brutos por puesto",
       subtitle = "Argentina 2020",
       caption = "Fuente: Encuesta de Sueldos Sysarmy 2020.1",
       x = "", y = "")+
  scale_y_continuous(labels = scales::comma_format(big.mark = ".", decimal.mark = ","))
