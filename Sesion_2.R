

#### Introducción ####
library(tidyverse)

# Vector - Conjunto de valores
names <- c("John", "Mike", "Ringo", "George")

# Elegir un elemento de un vector
names[2] <- "Paul"
names
# names[5] <- "Mick" así agrega un nuevo elemento

# Creamos otros vectores
instrument <- factor(c("Guitar", "Bass", "Drums", "Guitar"))
year <- c(1940, 1942, 1940, 1943)
lives <- c(0,1, 1, 0)

# Dataframe - Tabla
df <- data.frame(names, instrument, year, lives)
df

# Seleccionar elementos de un dataframe
df[4,3] # [fila,columna]

# Seleccionar toda una fila
df[1,]

# Seleccionar toda una columna
df[,3]

# Seleccionar varias columnas 
df[,2:4] # Columnas contiguas

# Seleccionar columnas puntuales
df[,c(1,4)]
df[, c("names", "lives")]

# También se usa el signo $ para algunas cosas
df$name

# Hay otro tipo de dataframe propio tidyverse que internamente tiene un par de propiedades
# Los tibble
df <- tibble(df)
df


#### Práctica RRHH ####



library(googlesheets4) # Para leer las hojas de cálculo de Google
library(gargle)
library(tidyverse)


# Carga el archivo desde google sheets.
nomina <- sheets_read("1UliFjEjab9skkSGp_QVYv3ZQc132dqBLV-97vppQtAg")
puestos <- sheets_read("1UliFjEjab9skkSGp_QVYv3ZQc132dqBLV-97vppQtAg", sheet = "Puestos")

# También podríamos usar el paquete googledrive para cargar un archivo por su nombre
library(googledrive)

nomina2 <- drive_get("Nomina") %>%
  read_sheet()


#Exploremos el dataset
glimpse(nomina)
head(nomina)

# La columna ID_CAT R la carga como una lista porque combina números y letras, lo cual nos va a complicar su uso.
nomina$ID_CAT <- unlist(nomina$ID_CAT)

glimpse(nomina)

#### Tidyverse ####

# Supongamos que quiero ver los empleados que tienen hijos, así que necesito el ID, y el área ordenado por área. 
arrange(filter(select(nomina, AREA,ID, HIJOS), HIJOS > 0), AREA)

# Una forma análoga sería hacerlo por pasos
seleccion_1 <- select(nomina, AREA, ID, HIJOS)
seleccion_2 <- filter(seleccion_1, HIJOS > 0)
seleccion_2
arrange(seleccion_2, AREA)

# Tidyverse lo que permite es trabajar de manera intuitiva, prolija y por etapas.
# El operador %>% del paquete dplyr permite encadenar funciones y ser más prolijo la lectura del código
hijos <- nomina %>%
  select(AREA, ID, HIJOS) %>%
  filter(HIJOS>0) %>%
  arrange(AREA)

# Algunas opciones de la función select
nomina %>%
  select_if(is.numeric) # para elegir las variables que son numéricas

# Por ejemplo si quiero odenar las columnas por una columna en especial e incluir el resto del dataset
nomina %>%
  select(AREA, everything())

# Si quiero filtrar por dos condiciones que sean "Y" puedo usar los símbolos & o una coma 
nomina %>%
  filter(ANTIGUEDAD < 1 & AREA == "INSERTOS")

nomina %>%
  filter(ANTIGUEDAD < 1, AREA == "INSERTOS")

# También puedo elegir por dos sectores, por ejemplo de INSERTOS y LAMINADO
nomina %>%
  filter(AREA == "FINANZAS", AREA == "RRHH")  

# Como una persona no está registrada en dos áreas en simultáneo, usamos el operador lógico | (or)
nomina %>%
  filter(AREA == "FINANZAS"| AREA == "RRHH")

# Una alternativa
nomina %>%
  filter(AREA %in% c("FINANZAS", "RRHH"))

# Puedo usar el símbolo ! para lo que no sea igual a ese criterio
nomina %>%
  filter(AREA != "CALIDAD") # Not equal

# También puedo filtrar varias condiciones.
# Por ejemplo, las categorías 1, 2, 3, 4, y 5 representan a los operarios. Si yo quiero elegir los no-operarios puedo hacerlo de la siguiente manera

mensuales <- nomina %>%
  filter(!ID_CAT %in% c("1", "2", "3", "4", "5"))
mensuales

# Arrange - Ordenar los datos
nomina %>%
  select(ID, EDAD, AREA) %>%
  arrange(EDAD)

# Funciona también para variables de texto
nomina %>%
  select(ID, AREA, N_CATEG) %>%
  arrange(AREA)

# arrange() por default ordena ascendentemente los datos. Para hacerlo descendentemente hay dos opciones
# Con la función desc()
nomina %>%
  select(ID, EDAD) %>%
  arrange(desc(EDAD))

# Usando el signo -
nomina %>%
  select(ID, ANTIGUEDAD) %>%
  arrange(-ANTIGUEDAD)

# group_by
nomina %>%
  select(AREA, ANTIGUEDAD) %>%
  group_by(AREA) %>%
  summarise(Ant_Promedio = mean(ANTIGUEDAD))

# Mutate
nomina %>%
  select(ID, EDAD) %>%
  mutate(Diferencia_Edad = EDAD - mean(EDAD))# Calculamos la diferencia entre la edad del empleado y el promedio de edad
mean(nomina$EDAD)


# También se usa para cambiar el tipo de dato que tiene una columna
glimpse(nomina)

# Quiero que la variable ID_CAT sea de tipo factor
nomina <- nomina %>%
  mutate(ID_CAT = factor(ID_CAT))

glimpse(nomina)

# Rename
nomina <- nomina %>%
  rename(CATEGORIA = N_CATEG)

glimpse(nomina)
  
#### Join - unir datos de dos tablas ####

# La función que más vamos a usar es left_join
# Carguemos un nuevo dataset
puestos <- sheets_read("1UliFjEjab9skkSGp_QVYv3ZQc132dqBLV-97vppQtAg", sheet = "Puestos")

glimpse(puestos)

tabla_nueva <- left_join(nomina, puestos, by = "ID")
tabla_nueva


nom_full <- nomina %>%
  left_join(puestos)

head(nom_full)

#### Secuencia de análisis ####
# Seleccionar y Agrupar
# Resumir
# Graficar

nom_full %>%
  select(AREA, PUESTO, SUELDO) %>%
  filter(!is.na(PUESTO)) %>% # !is.na devuelve los valores que no son neutros de la variable puesto.
  group_by(AREA) %>%
  summarise(Sueldo_Promedio = mean(SUELDO)) %>%
  ggplot(aes(x = AREA, y = Sueldo_Promedio)) +
  geom_col() 

nom_full %>%
  select(AREA, PUESTO, SUELDO) %>%
  filter(!is.na(PUESTO)) %>% # !is.na devuelve los valores que no son neutros de la variable puesto.
  group_by(AREA) %>%
  summarise(Sueldo_Promedio = mean(SUELDO)) %>%
  ggplot(aes(x = reorder(AREA, Sueldo_Promedio), y = Sueldo_Promedio)) +
  geom_col() +
  coord_flip()
