

#datafrom

#https://www.kaggle.com/jessemostipak/hotel-booking-demand/download
#https://www.kaggle.com/jessemostipak/hotel-booking-demand?select=hotel_bookings.csv


backcolor<-"white"
colortext<-"black"

#Defino paleta de colores
palette30<-c("#FFEC33",  "#FF8A33", "#EC4176", "#33DDFF", "#A13770", "#FD7FC4", "#5564eb", "grey")


#Eligo la fuente que quiero utilizar
library(extrafont) # la primera vez ejecutar font_import()
loadfonts()
font<- "Lato-Light" #Fuente que voy a utlizar


library(tidyverse) 

hotel_bookings <- read_csv("https://raw.githubusercontent.com/AnguloB/datosdemiercoles/master/00_30diasDeGraficos/15_dendograma/hotel_bookings.csv")

hotel_bookings<-hotel_bookings%>%
  filter(arrival_date_year=="2017")%>%
  filter(hotel=="Resort Hotel")%>%
  filter(reservation_status=="Canceled")%>%
  filter(arrival_date_month=="August")


glimpse(hotel_bookings)

variables<-c( "stays_in_week_nights",
              "adults", "children", 
              "booking_changes", 
              "adr" ,"total_of_special_requests" )

# Selecciono las variables numericas
data<-hotel_bookings[,variables]


library(ggdendro)

# Compute distances and hierarchical clustering
dd <- dist(scale(data), method = "euclidean")
hc <- hclust(dd, method = "complete")

#http://www.sthda.com/english/wiki/beautiful-dendrogram-visualizations-in-r-5-must-known-methods-unsupervised-machine-learning#ggdendro-package-ggplot2-and-dendrogram

# Build dendrogram object from hclust results
dend <- as.dendrogram(hc)

# Extract the data (for rectangular lines)
hcdata <- dendro_data(hc, k=8)

hcdata


ggplot(hcdata$segments) + 
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend), color=palette30[3])+
  ylim(0, 15)+
  theme_bw()+
  theme(text=element_text(family = font),
        plot.background = element_rect(fill = backcolor, color=NA), 
        strip.background =element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.background = element_rect(fill=backcolor,
                                         size=0.5, linetype="solid"),
        plot.title = element_text(size=15, hjust=0,face="bold", color="#9B77CF"), 
        plot.caption = element_text(size=10, hjust = 1), 
        axis.text.x = element_text(),
        axis.text.y = element_text(), 
        legend.position = "bottom")+ 
  labs(title = "Perfil de la cancelación de estancias en Resorts (Agosto 2017)",  
       x="", 
       y = "", fill="Puntuación\nde felicidad",
       caption = "Hecho por @AnguloBrunet \n #30díasdegráficos \n Fuente: https://www.kaggle.com/jessemostipak/hotel-booking-demand")


# Ejemplo 2 ----------------------------------------

# https://www.r-graph-gallery.com/335-custom-ggraph-dendrogram.html

# Libraries
library(ggraph)
library(igraph)
library(tidyverse)
theme_set(theme_void())

# Creación del dataset

# data: edge list
d1 <- data.frame(from="origin", to=paste("group", seq(1,7), sep=""))
d2 <- data.frame(from=rep(d1$to, each=7), to=paste("subgroup", seq(1,49), sep="_"))
edges <- rbind(d1, d2)

# We can add a second data frame with information for each node!
name <- unique(c(as.character(edges$from), as.character(edges$to)))
vertices <- data.frame(
  name=name,
  group=c( rep(NA,8) ,  rep( paste("group", seq(1,7), sep=""), each=7)),
  cluster=sample(letters[1:4], length(name), replace=T),
  value=sample(seq(10,30), length(name), replace=T)
)

# Create a graph object
mygraph <- graph_from_data_frame( edges, vertices=vertices)


# Layout circular o lineal

# Layout Lineal
ggraph(mygraph, layout = 'dendrogram', circular = FALSE) + 
  geom_edge_diagonal() 

# Layout circular
ggraph(mygraph, layout = 'dendrogram', circular = TRUE) + 
  geom_edge_diagonal()

# Edge Style

# Lineal
ggraph(mygraph, layout = 'dendrogram') + 
  geom_edge_link()

# Diagonal
ggraph(mygraph, layout = 'dendrogram') + 
  geom_edge_diagonal()


# Labels & Nodes

# Agrega etiquetas
ggraph(mygraph, layout = 'dendrogram') + 
  geom_edge_diagonal() +
  geom_node_text(aes( label=name, filter=leaf) , angle=90 , hjust=1, nudge_y = -0.01) +
  ylim(-.4, NA)

# Agrega la terminación con puntos
ggraph(mygraph, layout = 'dendrogram') + 
  geom_edge_diagonal() +
  geom_node_text(aes( label=name, filter=leaf) , angle=90 , hjust=1, nudge_y = -0.04) +
  geom_node_point(aes(filter=leaf) , alpha=0.6) +
  ylim(-.5, NA)

# Agregando colores a las etiquetas
ggraph(mygraph, layout = 'dendrogram') + 
  geom_edge_diagonal() +
  geom_node_text(aes( label=name, filter=leaf, color=group) , angle=90 , hjust=1, nudge_y=-0.1) +
  geom_node_point(aes(filter=leaf, size=value, color=group) , alpha=0.6) +
  ylim(-.6, NA) +
  theme(legend.position="none")
