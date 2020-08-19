##### Antes que nada empezarlo como notebook!!! y de ahi avanzar



# load the plotly R package
library(plotly)

# load the diamonds dataset from the ggplot2 package
data(diamonds, package = "ggplot2")
diamonds

#Intentar esto con la cosa nomina


# create three visualizations of the diamonds dataset
plot_ly(diamonds, x = ~cut)
plot_ly(diamonds, x = ~cut, y = ~clarity)
plot_ly(diamonds, x = ~cut, color = ~clarity, colors = "Accent")



p <- ggplot(diamonds, aes(x = log(price), color = clarity)) + 
  geom_freqpoly(stat = "density") + 
  facet_wrap(~cut)
ggplotly(p)

#Fuente: https://plotly-r.com/overview.html


library(yarrr)

pirates

ggplot(pirates, aes(x= sex, y = beard.length))+
  geom_boxplot()
