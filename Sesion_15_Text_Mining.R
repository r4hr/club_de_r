library(googlesheets4)
library(tidyverse)
library(gargle)
library(funModeling)
library(tidytext)
library(wordcloud2)

# Estilos para los gráficos
zy <- theme(panel.background = element_blank(),
            panel.grid.major.y = element_line(colour = "#F4F6F6"),
            axis.line = element_line(colour = "grey"))

zx <- theme(panel.background = element_blank(),
            panel.grid.major.x = element_line(colour = "#F4F6F6"),
            axis.line = element_line(colour = "grey"))

# Dataset
EncuestaHomeOffice <- sheets_read("1g2q3c_MMrBc4MehO4Yjktpu2fk7s7M8Bn2wIgV6yQHo")

glimpse(EncuestaHomeOffice)


hos <- EncuestaHomeOffice %>%
  select("¿Creés que va a cambiar la forma de trabajar después de esta crisis?",
         "Justifica la respuesta") %>% 
  rename("Cambios_Futuros" = "¿Creés que va a cambiar la forma de trabajar después de esta crisis?",
         "Comentarios" = "Justifica la respuesta") %>%
  filter(!is.na(Comentarios))


#### Text Mining ####
# Fuente: http://www.aic.uva.es/cuentapalabras/palabras-vacias.html



hos %>%
  group_by(Cambios_Futuros) %>%
  tally()

eho_text <- hos %>%
  select(Cambios_Futuros, Comentarios) %>%
  filter(!is.na(Comentarios)) %>%
  mutate(Comentarios = as.character(Comentarios))

eho_text


eho_text_pal <- eho_text %>%
  unnest_tokens(palabra, Comentarios)

eho_text_pal

# Un lexicon más exhaustivo y detallado
vacias <- read_csv("https://raw.githubusercontent.com/7PartidasDigital/AnaText/master/datos/diccionarios/vacias.txt",
                   locale = default_locale())
vacias


# Hacer un anti_join para eliminar las palabras del corpus que están en el listado del lexicon
eho_text_vacio <- eho_text_pal %>%
  anti_join(vacias)

eho_text_vacio

# Si quiero armar un listado específico de palabras para eliminar del análisis, luego uso un anti_join
vacias_adhoc <- tibble(palabra = c("trabajo", "home", "office", "van", "va"))

# Hay varias palabras que se repiten y que no aportan mucho valor así que las elimino.
eho_text_vacio <- eho_text_vacio %>%
  anti_join(vacias_adhoc)
eho_text_vacio



eho_text_vacio %>%
  count(palabra, sort = TRUE)

# Grafico por cantidad de palabras
eho_text_vacio %>%
  count(palabra, sort = TRUE) %>%
  filter(n>9) %>%
  ggplot(aes(x= reorder(palabra, n), y = n, fill = n))+
  geom_bar(stat = "identity") + 
  scale_fill_gradient(low = "#D6DBDF", high = "#34495E")+
  zx+
  ylab("Cantidad de apariciones") +
  xlab(NULL)+
  ggtitle("Comentarios Encuesta Home Office",
          subtitle = "Palabras con 10 o más apariciones")+
  coord_flip()

# Ordeno los comentarios en base a la variable "Cambios_Futuros"
library(forcats)
eho_text_vacio$Cambios_Futuros <- fct_relevel(eho_text_vacio$Cambios_Futuros, "Sí", "Tal vez", "No")

escala <- c("#4445f8", "#c7beca", "#da8a10" )

ze <- scale_fill_manual(values = escala)



eho_text_vacio %>%
  group_by(Cambios_Futuros) %>%
  count(palabra, sort = TRUE) %>%
  top_n(10) %>%
  ggplot(aes(x = reorder_within(palabra, n, Cambios_Futuros), y = n, fill = Cambios_Futuros)) +
  scale_fill_manual(values = escala)+
  geom_bar(stat = 'identity', show.legend = FALSE) +
  scale_x_reordered()+
  facet_wrap(~Cambios_Futuros, ncol = 2, scales = "free")+
  labs(x = "", y= "Frecuencia Absoluta")+
  ggtitle("Top 10 de palabras por Respuesta",
          subtitle = "Pregunta: ¿Creés que va a cambiar la forma de trabajar?")+
  coord_flip() +
  zx

###### Análisis de Sentimientos

# Lexicon de sentimientos
sentimientos <- read_tsv("https://raw.githubusercontent.com/7PartidasDigital/AnaText/master/datos/diccionarios/sentimientos_2.txt",
                         col_types = "cccn",
                         locale = default_locale())
sentimientos

# Modificación de la función get_sentiments de tidyverse
source("https://raw.githubusercontent.com/7PartidasDigital/R-LINHD-18/master/get_sentiments.R")

## Análisis General
eho_text_nrc <- eho_text_vacio %>%
  right_join(get_sentiments("nrc")) %>%
  filter(!is.na(sentimiento)) %>%
  count(sentimiento, sort = TRUE)

eho_text_nrc

sum(eho_text_nrc$n)

feelings <- c("negativo", "positivo", "negativo", "negativo", "negativo", "positivo", "positivo", "positivo")

eho_text_nrc %>%
  filter(sentimiento != "negativo", sentimiento !="positivo") %>%
  cbind(feelings) %>%
  ggplot(aes(reorder(sentimiento, n), n, fill = feelings)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  scale_fill_manual(values = c("#F5B041","#5DADE2"))+
  zx +
  coord_flip() +
  labs(title="Ranking de sentimientos",
       x = "Sentimiento",
       y = "Cantidad de Apariciones")

eho_text_vacio %>%
  right_join(get_sentiments("nrc")) %>%
  filter(!is.na(sentimiento)) %>%
  group_by(sentimiento) %>%
  count(palabra, sort = TRUE) %>%
  filter(sentimiento == "miedo")

eho_text_vacio %>%
  right_join(get_sentiments("nrc")) %>%
  filter(!is.na(sentimiento)) %>%
  group_by(sentimiento) %>%
  count(palabra, sort = TRUE) %>%
  filter(sentimiento == "confianza")


eho_text_vacio %>%
  group_by(Cambios_Futuros) %>%
  mutate(recuento_palabras = 1:n(),
         indice = recuento_palabras %/% 10 + 1) %>%
  inner_join(get_sentiments("nrc")) %>%
  count(Cambios_Futuros, indice = indice, sentimiento) %>%
  ungroup() %>%
  spread(sentimiento, "n", fill = 0) %>%
  mutate(sentimiento = positivo - negativo) %>%
  ggplot(aes(indice, sentimiento, fill = Cambios_Futuros)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~Cambios_Futuros, ncol=1, scales = "free_x")+
  zy + 
  scale_fill_manual(values = escala)+
  geom_hline(yintercept = 0,
             linetype = "dashed",
             colour = "red",
             size = 0.4) +
  ggtitle("Evolución de los Sentimientos por Respuestas")+
  labs(caption = "Agrupados por si cree que va a cambiar la forma de trabajar")

library(wordcloud2)
library(webshot)
#webshot::install_phantomjs()


eho_text_vacio %>%
  filter(Cambios_Futuros == "Sí") %>%
  count(palabra, sort = TRUE) %>%
  ungroup() %>%
  wordcloud2( size = 0.6, shape = "triangle-forward",color = rep_len(c("#4445f8", "#7563fa", "#9881fc", "#b59ffe"), nrow(.)))

eho_text_vacio %>%
  filter(Cambios_Futuros == "No") %>%
  count(palabra, sort = TRUE) %>%
  ungroup() %>%
  wordcloud2(size = 0.6,shape = "diamond",color = rep_len(c("#da8a10", "#e59d43", "#eeb069", "#f6c38e"), nrow(.)))

eho_text_vacio %>%
  filter(Cambios_Futuros == "Tal vez") %>%
  count(palabra, sort = TRUE) %>%
  ungroup() %>%
  wordcloud2(size = 0.6,color = rep_len(c("#c7beca", "#a293a7", "#c8bccb", "#d8bfd8", "#c5cbe1"), nrow(.)))

