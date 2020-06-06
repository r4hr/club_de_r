library(googlesheets4)
library(tidyverse)

edades <- read_sheet("1tA4u-zrYYea9ZEZ77laglqYUkH0Fyguzm_CWlzbz7yg", range = "G1:H16")


# Eliminemos los NA
edades <- edades %>%
  filter(!is.na(Edad))
edades



promedio_edad <- round(mean(edades$Edad),1)

ggplot(edades, aes(x = Edad, y = Inicial, label = Inicial))+
  geom_point() +
  geom_text(nudge_x = 0.3, nudge_y = 0.3) +
  geom_vline(xintercept = promedio_edad, color = "red", size = 1.2, alpha = 0.5)

truco_promedio <- edades %>%
  mutate(Diferencia_Prom = abs(promedio_edad - Edad))

truco_promedio

# Si sumo todas las diferencias de la columna nueva... cuánto da?

sum(truco_promedio$Diferencia_Prom)

truco_promedio <- edades %>%
  mutate(Diferencia_Prom = abs(40 - Edad))

sum(truco_promedio$Diferencia_Prom)


# Explicación Regresiones lineales
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

hr_data <- read.csv("HRDataset_v13.csv")
hr_data$Department <- factor(hr_data$Department)    

hr_data %>%
  filter(Department %in% c("IT/IS", "Production       ", "Sales")) %>%
  group_by(Department) %>%
  summarise(Promedio = mean(PayRate), 
            Desvio = sd(PayRate))

hr_data %>%
  filter(Department %in% c("IT/IS", "Production       ", "Sales"), !is.na(PayRate)) %>%
  ggplot(aes(Department, PayRate, color = Department))+
  geom_point(alpha = 0.3, size = 3) +
  geom_hline(yintercept = mean(PayRate[Department=="Sales"], na.rm = T))

p<- c(25,22,20,16,12,8,5,3,18,23,13,9,23,8,2)
a<- c(3,5,8,12,16,20,22,25,7,6,15,18,7,19,23)
dfn <- data.frame(a, p)

r <- c(1,2,3,4,5,6,7,8,9,10)
sesiones <- c(1,2,3,4,5,6,7,8,9,10)
dfp <- data.frame(r, sesiones)

ggplot(dfn, aes(p, a))+
  geom_point(size = 3) + geom_smooth(method = "lm", se = F)
ggplot(dfp, aes(sesiones, r))+
  geom_point(size = 3) + geom_smooth(method = "lm", se = F)+
  ggtitle("Cuanto sabe de R los participantes del Club de R")


gs4_deauth()
options(scipen = 999) # Cambia la notación científica de los gráficos
encuesta_sysarmy <- sheets_read("1_db6zEAMvr-1GQjJb4hV-rSQfJ9w6GmezbqKJ2JJn7I", skip = 9)

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
  

summary(analisis)

analisis %>%
  filter(Puesto == "QA / Tester") %>%
  cor(Experiencia, Sueldo_Bruto) %>%
  ggplot(aes(Experiencia, Sueldo_Bruto))+
  geom_point(alpha = 0.3)+
  geom_smooth(method = "lm")

analisis %>%
  filter(Puesto == "Developer") %>%
  ggplot(aes(Experiencia, Sueldo_Bruto))+
  geom_point(alpha = 0.3)+
  geom_smooth(method = "lm")

qa <- analisis %>%
  filter(Puesto == "QA / Tester")

dev <- analisis %>%
  filter(Puesto == "Developer")

mod_qa <- lm(Sueldo_Bruto~Experiencia, data = qa)
mod_qa

library(broom)

tidy(mod_qa)

qa_res <- residuals(mod_qa)
head(augment(mod_qa))


library(scales)
point <- format_format(big.mark = ".", decimal.mark = ",", scientific = FALSE)
qa_inter <- unlist(tidy(mod_qa)[1,2])
comma(qa_inter)

qa <- qa %>% mutate(Residuos = qa_res)

ggplot(qa, aes(Residuos))+
  geom_histogram()

ggplot(qa, aes(Residuos))+
  geom_density()

ggplot(qa, aes(sample = Residuos))+
  geom_qq()

library(clusterSim)
qa_res_nor <- data.Normalization (residuals(mod_qa), type = "n1", normalization = "file" )
qa_res_nor

qa <- qa %>% mutate(res_nor = qa_res_nor)

ggplot(qa, aes(Experiencia, res_nor))+
  geom_point()
