library(tidyverse)
library(funModeling)
library(scales)
library(extrafont)
library(ggalt)

# Levanta el archivo y filtra los datos de Argentina
rhar <- read_delim("Datasets/kiwi_rh.csv", delim = ",") %>% 
  filter(pais == "Argentina")

# Obtenemos información de las variables numéricas
profiling_num(rhar)

# Obtenemos los valores de los percentiles 5 y 95 de la variable sueldo_bruto
p05 <- profiling_num(rhar)[8,6]
p95 <- profiling_num(rhar)[8,10]

# Filtramos los sueldos que están dentro de los percentiles 5 y 95
rhar <- rhar %>% 
  filter(between(sueldo_bruto, p05, p95))

ggplot(rhar, aes(x = sueldo_bruto)) +
  geom_density(fill = "blue", alpha = 0.4, color = "darkblue")

# brecha salarial ----------------------------------------------

brecha <- rhar %>% 
  select(puesto, sueldo_bruto, genero) %>% 
filter(genero %in% c("Femenino", "Masculino"), 
       puesto %in% c("Gerente","Jefe", "HRBP","Responsable", "Analista", "Administrativo")) %>% 
  mutate(puesto = factor(puesto, levels = c("Administrativo","Analista", 
                                            "HRBP", "Responsable","Jefe", "Gerente"))) %>% 
  group_by(genero, puesto) %>% 
  summarise(media_salarial = mean(sueldo_bruto))

brecha_graf <- brecha %>% 
  pivot_wider(., names_from = genero, values_from = media_salarial) %>% 
  mutate(brecha = percent((Masculino-Femenino)/Masculino, 1),
         x = (Masculino + Femenino)/2)

brecha_graf

# Calcular el indicador de brecha salarial 
brecha_promedio <- round(mean(brecha_graf$gap*100))

brecha_promedio

ggplot(brecha_graf, 
       aes(x = Femenino, xend = Masculino, y = puesto, 
           group = puesto, label = brecha)) +
  geom_dumbbell(color = "#808080",
                size_x = 3, size_xend = 3,
                colour_x = colores[1],
                colour_xend = colores[2]) +
  geom_text(data = brecha_graf, 
            aes(x, puesto, label = brecha), nudge_y = .2) +
  labs(title = "Brecha salarial por puestos",
       subtitle = "Sueldos promedios en Argentina",
       x = "",
       y = NULL, 
       caption = fuente) +
  scale_x_continuous(labels = comma_format(big.mark = ".", decimal.mark = ",")) +
  scale_color_manual(values = colores) +
  theme_minimal()+
  theme(plot.background = element_rect(fill = "#FBFCFC"),
        text = element_text(family = "Ubuntu Mono"))