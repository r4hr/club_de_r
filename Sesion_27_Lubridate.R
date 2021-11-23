library(tidyverse)
library(readr)
library(openxlsx)
library(lubridate)
library(gganimate)


# Ejemplos -----------------------------------------------------

d1 <- "Enero 1, 2010"
d2 <- "2015-Mar-07"
d3 <- "06-Jun-2017"
d4 <- c("Agosto 19 (2015)", "Julio 1 (2015)")
d5 <- "12/30/14" # Diciembre 30, 2014

mdy(d1)
ymd(d2)
dmy(d3)
mdy(d4)
mdy(d5)




# Datos --------------------------------------------------------

# Cargamos 1er dataset
hr_data <- read_delim("Datasets/HRDataset_v13.csv", delim = ";")

#Nos quedamos sólo con algunos campos
hr_data <- hr_data %>% 
  select(EmpID, Sex, DOB, DateofHire, DateofTermination)

# Cargamos 2do dataset
fichadas <- read.xlsx("Datasets/fichadas.xlsx")


# Funciones básicas -------------------------------------------

hr_data <- hr_data %>% 
  mutate(DOB = mdy(DOB),
         DateofHire = mdy(DateofHire),
         DateofTermination = mdy(DateofTermination))

ejemplo <- pull(hr_data[1,3])
ejemplo


# Gráfico de años
hr_data %>% 
  mutate(anio = year(DateofHire)) %>% 
  group_by(anio) %>% 
  summarise(total = n()) %>% 
  ggplot(aes(x = anio, y = total)) +
  geom_line() +
  labs(title = "Evolución anual de contrataciones",
       x = "", y = "") 

hr_data %>% 
  mutate(anio = floor_date(DateofHire, unit = "year")) %>% 
  group_by(anio) %>% 
  summarise(total = n()) %>% 
  ggplot(aes(x = anio, y = total)) +
  geom_line() +
  labs(title = "Evolución anual de contrataciones",
         x = "", y = "") 


hr_data %>% 
  filter(is.na(DateofTermination)) %>% 
  mutate(antiguedad = today() - DateofHire)

hr_data %>% 
  filter(is.na(DateofTermination)) %>% 
  mutate(antiguedad = as.period(interval(start = DateofHire, end = today())))

hr_data %>% 
  filter(is.na(DateofTermination)) %>% 
  mutate(antiguedad = as.period(interval(start = DateofHire, end = today()))$year)

# Fichadas ----------------------------------------

glimpse(fichadas)

fichas <- fichadas %>% 
  mutate(Fecha = dmy(Fecha),
         Entrada = dmy_hm(Entrada),
         Salida = dmy_hm(Salida))

glimpse(fichas)

fichas %>% 
  mutate(hs_trabajadas = Salida - Entrada) %>% 
  head()

fichas2 <- fichadas %>% 
  mutate(Fecha = dmy(Fecha),
         Entrada = dmy_hm(Entrada, tz = "America/Buenos_Aires"),
         Salida = dmy_hm(Salida, tz = "America/Buenos_Aires"))


fichas2 <- fichas %>% 
  mutate(intervalo_trabajado = Entrada %--% Salida,
         horas_trabajadas = as.period(intervalo_trabajado))


glimpse(fichas2)
str(fichas2)         

fichas3 <- fichas2 %>% 
  mutate(Hs.Laborables = hm(Hs.Laborables),
         vagos = intervalo_trabajado < Hs.Laborables) %>% 
  filter(vagos == 1) %>% 
  select(Fecha, Hs.Laborables, intervalo_trabajado, vagos)



fichas3 %>% 
  mutate(dia_semana = wday(Fecha, label = TRUE)) %>% 
  ggplot(aes(x = dia_semana)) +
  geom_bar()










library(gargle)
library(googlesheets4)
library(dplyr)
library(lubridate)
library(ggplot2)
library(gganimate)
library(gapminder)

head(gapminder)

p <- ggplot(
  gapminder, 
  aes(x = gdpPercap, y=lifeExp, size = pop, colour = country)
) +
  geom_point(show.legend = FALSE, alpha = 0.7) +
  scale_color_viridis_d() +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  labs(x = "GDP per capita", y = "Life expectancy")
p

p + transition_time(year) +
  labs(title = "Year: {frame_time}")


  