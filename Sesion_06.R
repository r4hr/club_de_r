library(googlesheets4)
library(gargle)
library(scales)
library(tidyverse)


edades <- read_sheet("1A5zzDcQm86h9B4Jet3niS-4v3iubsFHk1l3TM5zGths", range = "A1:B12")

promedio_edad <- round(mean(edades$Edad),1)

# El promedio de edad del Club de R para RRHH es 
promedio_edad 

# El promedio como referencia de las edades de los participantes.
ggplot(edades, aes(x = Edad, y = Inicial, label = Inicial))+
  geom_point() + labs(y="")+
  geom_text(nudge_x = 0.3, nudge_y = 0.3) +
  geom_vline(xintercept = promedio_edad, color = "red", size = 1.2, alpha = 0.5)

# Una de las características del promedio es que las distancias de las observaciones al promedio es la menor posible.

truco_promedio <- edades %>%
  mutate(Diferencia_Prom = abs(promedio_edad - Edad))

truco_promedio

# Si sumo todas las diferencias de la columna nueva... cuánto da?

sum(truco_promedio$Diferencia_Prom)

# Veamos ahora las diferencias si en vez del promedio de 32.3, lo calculo con un valor de 27
truco_promedio2 <- edades %>%
  mutate(Diferencia = abs(27 - Edad))

sum(truco_promedio2$Diferencia)

sum(truco_promedio$Diferencia_Prom) < sum(truco_promedio2$Diferencia)


# Medidas de resumen: mediana, promedio, mín/max, 1° y 3° Cuartil
summary(edades$Edad)

#### Varianza y desvío estándar ####

# Utilizo el dataset "HRDataset_v13" para analizar la dispersión de los sueldos en 3 sectores.
hr_data <- read.csv("HRDataset_v13.csv")
hr_data$Department <- factor(hr_data$Department)    

hr_data %>%
  filter(Department %in% c("IT/IS", "Production       ", "Sales")) %>%
  group_by(Department) %>%
  summarise(Promedio = mean(PayRate), 
            Desvio = sd(PayRate))

# Scatterplot: Nótese la dispersión que hay en los sueldos de cada sector. 
# Hay sectores con más concentración de puntos, y en IT hay una gran dispersión.
hr_data %>%
  filter(Department %in% c("IT/IS", "Production       ", "Sales"), !is.na(PayRate)) %>%
  ggplot(aes(Department, PayRate, color = Department))+
  geom_point(alpha = 0.3, size = 3)

# Los mismos datos pero representados con un boxplot
hr_data %>%
  filter(Department %in% c("IT/IS", "Production       ", "Sales"), !is.na(PayRate)) %>%
  ggplot(aes(Department, PayRate, fill = Department))+
  geom_boxplot()



#### Regresiones lineales ####

# Explicación Regresiones lineales Libro Walter Sosa Escudero
Ingreso <- c(2,34,41,44,64,71,85,90,91,93)
Gasto <- c(2,12,11,14,21,20,27,27,29,27)
Regresion <- data.frame(Ingreso, Gasto)


library(ggthemes)
ggplot(Regresion, aes(Ingreso, Gasto))+
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = F)+
  geom_hline(yintercept = mean(Gasto), linetype = 2, color = "grey50") +
  geom_segment(aes(x = 50, y = 0, yend = 15.64, xend = 50), position = "identity", linetype = 3)+
  geom_segment(aes(x = 0, y = 15.64, yend = 15.64, xend = 50), position = "identity", linetype = 3)+
  ggtitle("Gráfico 1")


# Creo un data frame para visualizar una correlación negativa

p<- c(25,22,20,16,12,8,5,3,18,23,13,9,23,8,2)
a<- c(3,5,8,12,16,20,22,25,7,6,15,18,7,19,23)
dfn <- data.frame(a, p)

ggplot(dfn, aes(p, a))+
  geom_point(size = 3) + geom_smooth(method = "lm", se = F)+
  ggtitle("Correlación negativa")

# Creo un data frame para simular una correlación positiva.
r <- c(1,2,3,4,5,6,7,8,9,10)
sesiones <- c(1,2,3,4,5,6,7,8,9,10)
dfp <- data.frame(r, sesiones)

ggplot(dfp, aes(sesiones, r))+
  geom_point(size = 3) + geom_smooth(method = "lm", se = F)+
  ggtitle("Cuanto saben de R los participantes del Club de R")



#### Regresiones Lineales datos de Sysarmy ####

# Carga de los datos
gs4_deauth()
options(scipen = 999) # Cambia la notación científica de los gráficos
encuesta_sysarmy <- sheets_read("1_db6zEAMvr-1GQjJb4hV-rSQfJ9w6GmezbqKJ2JJn7I", skip = 9)

# Limpieza de datos
analisis <- encuesta_sysarmy %>%
  select('Trabajo de','Me identifico', `Salario mensual BRUTO (en tu moneda local)`, 'Años de experiencia', "Cantidad de empleados") %>%
  rename(Genero = 'Me identifico', 
         Puesto = 'Trabajo de',
         Sueldo_Bruto = `Salario mensual BRUTO (en tu moneda local)`,
         Experiencia = 'Años de experiencia',
         Empleados = 'Cantidad de empleados') %>%
  filter(!is.na(Empleados),
         Puesto %in% c("Developer", "QA / Tester"),
         between(Sueldo_Bruto, 20000, 1000000),
         Genero != "Otros",
  ) %>%
  mutate(Experiencia = as.numeric(unlist(Experiencia)),
         Empleados = factor(Empleados, levels = c("1-10","11-50","51-100","101-200","201-500",
                                                  "501-1000","1001-2000","2001-5000",
                                                  "5001-10000")),
         Genero = factor(Genero),
         Puesto = factor(Puesto))

# Creo dos objetos para analizar
qa <- analisis %>%
  filter(Puesto == "QA / Tester")

dev <- analisis %>%
  filter(Puesto == "Developer")

mod_qa <- lm(Sueldo_Bruto~Experiencia, data = qa)
mod_qa

library(broom)

tidy(mod_qa)

# Residuales
qa_res <- residuals(mod_qa)
head(augment(mod_qa))

# Gráfico con recta de regresión.
analisis %>%
  filter(Puesto == "QA / Tester") %>%
  ggplot(aes(Experiencia, Sueldo_Bruto))+
  geom_point(alpha = 0.3, size = 2)+
  geom_smooth(method = "lm")+
  scale_y_continuous(labels = scales::comma_format(big.mark = ".", decimal.mark = ","))

# Agrego los residuales al dataframe de qa
qa <- qa %>% mutate(Residuos = qa_res)

# Analizo si la distribución de los residuales es normal
# Primero, con un histograma
ggplot(qa, aes(Residuos))+
  geom_histogram()

# Luego con un gráfico de densidad
ggplot(qa, aes(Residuos))+
  geom_density()

# Hago una "normalización" de los residuos para ver su posición relativa de la distancia de los residuos respecto de la recta de regresión
library(clusterSim)
qa_res_nor <- data.Normalization (residuals(mod_qa), type = "n1", normalization = "file" ) # Ojo al espacio entre el nombre de la función y el paréntesis
qa_res_nor

# Agrego los residuos normalizados al dataframe qa
qa <- qa %>% mutate(res_nor = qa_res_nor)

# Hago un gráfico de los residuales para ver cuántos puntos hay fuera de los límites de +2 y -2.
ggplot(qa, aes(Experiencia, res_nor))+
  geom_point(alpha = 0.4)+
  geom_hline(yintercept = 0, color = "red")+
  geom_hline(yintercept = 2, color = "red", linetype = 2)+
  geom_hline(yintercept = -2, color = "red", linetype = 2)


#### Regresión Lineal Múltiple ####
qa_rm <- lm(Sueldo_Bruto~Experiencia+Genero, data = qa)
qa_rm

library(moderndive)
get_regression_table(qa_rm)

library(ggthemes)

# Ahora hay dos rectas de regresión, en función de los géneros
ggplot(qa, aes(Experiencia, Sueldo_Bruto, color = Genero ))+
  geom_point(alpha = 0.4, size = 2.5)+
  geom_smooth(method = "lm", se = FALSE)+
  scale_color_colorblind()+
  scale_y_continuous(labels = scales::comma_format(big.mark = ".", decimal.mark = ","))

# Separo los gráficos
ggplot(qa, aes(Experiencia, Sueldo_Bruto, color = Genero ))+
  geom_point(alpha = 0.4, size = 2.5)+
  geom_smooth(method = "lm", se = FALSE)+
  scale_color_colorblind()+
  scale_y_continuous(labels = scales::comma_format(big.mark = ".", decimal.mark = ","))+
  facet_grid(Genero~.)





