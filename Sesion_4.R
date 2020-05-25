#Librerías

library(googlesheets4) 
library(gargle)
library(tidyverse)
library(ggthemes)
library(lubridate)
library(scales)

#### Datos ####

# Carga el archivo desde google sheets.
nomina <- sheets_read("1UliFjEjab9skkSGp_QVYv3ZQc132dqBLV-97vppQtAg")
puestos <- sheets_read("1UliFjEjab9skkSGp_QVYv3ZQc132dqBLV-97vppQtAg", sheet = "Puestos")


mensuales <- nomina %>%
  mutate(ID_CAT = unlist(ID_CAT),
         ID_CAT = as.factor(ID_CAT)) %>%
  filter(!ID_CAT %in% c("1", "2", "3", "4", "5")) %>%
  left_join(puestos, by = "ID") %>%
  mutate(Rangos_Edad = case_when(
    EDAD %in% 18:30 ~ "Hasta 30",
    EDAD %in% 31:40 ~ "Entre 31 y 40",
    EDAD %in% 41:50 ~ "Entre 41 y 50",
    EDAD %in% 51:70 ~ "Más de 50"),
    Rangos_Edad = factor(Rangos_Edad, levels = c("Hasta 30", "Entre 31 y 40",
                                                 "Entre 41 y 50", "Más de 50")))

# Guardar un dataframe en un archivo
write.csv(mensuales, file = "mensuales.csv", sep = ";")

library(openxlsx) # Para cargar y escribir datos en Excel
write.xlsx(x = mensuales, file = "mensuales.xlsx")


# Preprocesamiento
sueldos <- mensuales %>%
  group_by(PUESTO) %>%
  summarise(Sueldo_Promedio = mean(SUELDO))


# Creamos un objeto con la primera línea de código para hacer un gráfico
grafico <- ggplot(sueldos, aes(reorder(PUESTO, -Sueldo_Promedio), Sueldo_Promedio))

# Ahora para hacer un gráfico sólo necesitamos invocar el geom que necesitemos.
grafico +
  geom_point()

grafico + 
  geom_col()

#### Temas ####

# Podemos cambiar el estilo de gráfico con alguno de los temas nativos de ggplot2
grafico + geom_col() +
  theme_bw()

grafico + geom_col() +
  theme_classic()

grafico + geom_col() +
  theme_dark()

grafico + geom_col() +
  theme_light()

grafico + geom_col() +
  theme_linedraw()

grafico + geom_col() +
  theme_minimal()

grafico + geom_col() +
  theme_void()

# La librería ggthemes (entre muchas otras) añade temas diferentes para usar. Algunos ejemplos

grafico + geom_col() +
  theme_economist_white()

grafico + geom_col() +
  theme_tufte()

grafico + geom_col() +
  theme_wsj()

# ggthemes también tiene una paleta de colores apta para daltónicos.

# Gráfico de colores por default
grafico + geom_col(aes(fill = PUESTO))

# Gráfico con paleta apta para daltónicos
grafico + geom_col(aes(fill = PUESTO)) +
  scale_fill_colorblind() # El gráfico no refleja 2 barras porque la paleta tiene 8 colores


#### Texto en los gráficos ####

##### Títulos

# Modificar títulos con ggtitle, y ejes con ylab y xlab
grafico + geom_col() + 
  ggtitle("Sueldo promedio por puesto") + 
  xlab("Puestos") +
  ylab("Sueldo Promedio")

# Lo mismo pero con la funció labs
grafico + geom_col() + 
  labs(title = "Sueldo promedio por puesto",
       subtitle = "FY 16",
       x = "Puesto", 
       y = "Sueldo Promedio",
       caption = "En AR$")

##### Etiquetasde los ejes

# Rotando el ángulo de las etiquetas
grafico + geom_col()+
  ggtitle("Sueldo promedio por puesto")+
  theme(axis.text.x = element_text(angle = 90))

grafico + geom_col()+
  ggtitle("Sueldo promedio por puesto")+
  theme(axis.text.x = element_text(angle = 45,vjust = 0.6))

# Controlando el solapamiento de las etiquetas
grafico + geom_col()+
  ggtitle("Sueldo promedio por puesto") +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))

grafico + geom_col()+
  ggtitle("Sueldo promedio por puesto") +
  scale_x_discrete(guide = guide_axis(n.dodge = 3))

# Rotando los ejes con coord_flip()
grafico + geom_col()+
  ggtitle("Sueldo promedio por puesto") +
  coord_flip()

# ... o mapeando las variables categóricas en el eje y
ggplot(sueldos, aes(Sueldo_Promedio, reorder(PUESTO, Sueldo_Promedio)))+
  geom_col()


# Agregar el separador de miles en los ejes numéricos (paquete scales)
grafico + geom_col()+
  ggtitle("Sueldo promedio por puesto")+
  scale_y_continuous(labels = comma_format(big.mark = ".", decimal.mark = ","))
# Por más que el gráfico no tenga decimales, es conveniente configurar los decimales para evitar mensajes de error.

# Agregar las etiquetas de datos al gráfico
#Fuera de la barra
grafico + geom_col()+
  geom_text(aes(label=round(Sueldo_Promedio),
                hjust=-0.2),position = position_dodge(width=0.9),  size=3.5)+
  scale_y_continuous(limits = c(0,100000))+ # Aumentar la escala del eje para que entre la etiqueta en el gráfico.
  labs(x="",y="")+
  ggtitle("Sueldo promedio por puesto")+
  coord_flip()

# Dentro de la barra
grafico + geom_col()+
  geom_text(aes(label=round(Sueldo_Promedio),
                hjust=1.5), colour = "white")+
  coord_flip()

#### Colores ####

# Coloreando por uno de los atributos del data frame (dentro de aes)
grafico + geom_col(aes(fill=PUESTO))+
  scale_y_continuous(labels = comma_format(big.mark = ".", decimal.mark = ","))+
  labs(x="",y="")+
  ggtitle("Sueldo promedio por puesto")+
  coord_flip()

# Diferencia entre fill y color para un gráfico de barras
grafico + geom_col(aes(color=PUESTO))+
  scale_y_continuous(labels = comma_format(big.mark = ".", decimal.mark = ","))+
  labs(x="",y="")+
  ggtitle("Sueldo promedio por puesto")+
  coord_flip()

# Qué pasa cuando quiero cambiar un color dentro de aes?
grafico + geom_col(aes(fill="blue"))+
  scale_y_continuous(labels = comma_format(big.mark = ".", decimal.mark = ","))+
  labs(x="",y="")+
  ggtitle("Sueldo promedio por puesto")+
  coord_flip()

# La forma correcta de pasar un color específico
grafico + geom_col(fill="blue")+
  scale_y_continuous(labels = comma_format(big.mark = ".", decimal.mark = ","))+
  labs(x="",y="")+
  ggtitle("Sueldo promedio por puesto")+
  coord_flip()


# Usando colores de la librería RColorBrewer
library(RColorBrewer)

grafico + geom_col(aes(fill=PUESTO))+
  scale_fill_brewer(palette = "PuOr")+
scale_y_continuous(labels = comma_format(big.mark = ".", decimal.mark = ","))+
  labs(x="",y="")+
  ggtitle("Sueldo promedio por puesto")+
  coord_flip()

# Gradientes
avg_sueldo <- mean(sueldos$Sueldo_Promedio)

grafico + geom_col(aes(fill=Sueldo_Promedio))+
  scale_fill_gradient(low = "orange", high = "blue")+
  scale_y_continuous(labels = comma_format(big.mark = ".", decimal.mark = ","))+
  labs(x="",y="")+
  ggtitle("Sueldo promedio por puesto")+coord_flip()

# Agregar una línea de referencia a los gráficos
grafico + geom_col(fill="#5DADE2")+
  scale_y_continuous(labels = comma_format(big.mark = ".", decimal.mark = ","))+
  labs(x="",y="")+
  ggtitle("Sueldo promedio por puesto")+
  geom_hline(yintercept = mean(sueldos$Sueldo_Promedio),
             color = "red",
             linetype = 2)+
  theme(axis.text.x = element_text(angle = 90))
