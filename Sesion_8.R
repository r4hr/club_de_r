library(readr)
library(tidyverse)
library(caret)
library(rpart)
library(rpart.plot)



datos_rh <- read_csv("https://raw.githubusercontent.com/mlambolla/Analytics_HR_Attrition/master/HR_comma_sep.csv")


# Cambiemos el nombre de la variable 'sales' por 'department' y cambiemos los valores de 'salary'
datos_rh <- datos_rh %>% 
  rename(department = sales) %>%
  mutate(salary = as.numeric(case_when(
    salary == 'low' ~ 0,
    salary == 'medium' ~ 1,
    salary == 'high' ~ 2
  )))

set.seed(234)

# Crea un índice con las filas seleccionadas para generar un training set con el 70% de los datos.
modelo_hr <- createDataPartition(y = datos_rh$left, p = 0.7,
                                 list = FALSE)

# Creo un dataframe con los datos de entrenamiento seleccionados en el modelo_hr
modelo_hr_train <- datos_rh[modelo_hr,]

# Creo un dataframe de testing con las filas que no están incluidas en el índice.
modelo_hr_test <- datos_rh[-modelo_hr,]


#### Regression Tree ####

arbol_hr_train <- rpart(left ~.-department, data = modelo_hr_train, method = "class")

arbol_hr_train


prp(arbol_hr_train, extra=106)


arbol_hr_test <- predict(arbol_hr_train, newdata = modelo_hr_test)



# Veamos los 10 primeros registros
arbol_hr_test[1:10,]

hist(arbol_hr_test)

#Agrego los resultados del modelo a los datos de test
modelo_hr_test$score_arbol <- arbol_hr_test[,2]
glimpse(modelo_hr_test)

modelo_hr_test <- modelo_hr_test %>% 
  mutate(prediccion_arbol = ifelse(score_arbol > 0.5, 1, 0))


# Creamos la matriz de confusión, con los valores predichos y los valores reales de la variable target.
conf_matrix_arbol <- table(modelo_hr_test$prediccion_arbol, modelo_hr_test$left)
conf_matrix_arbol

resultados_arbol <- confusionMatrix(conf_matrix_arbol)

accuracy_arbol <- resultados_arbol[[3]][1]

resultados_arbol

#### Arbol 2 ####

control <- rpart.control(minbucket = 300)
arbol_hr_fit <- rpart(left ~. -department, data = modelo_hr_train,method = "class", control = control)

prp(arbol_hr_fit, extra=106)


arbol_hr_test2 <- predict(arbol_hr_fit, newdata = modelo_hr_test)




# Veamos los 10 primeros registros
arbol_hr_test2[1:10,]

hist(arbol_hr_test2)

#Agrego los resultados del modelo a los datos de test
modelo_hr_test$score_arbol2 <- arbol_hr_test2[,2]
glimpse(modelo_hr_test)

modelo_hr_test <- modelo_hr_test %>% 
  mutate(prediccion_arbol2 = ifelse(score_arbol2 > 0.5, 1, 0))


# Creamos la matriz de confusión, con los valores predichos y los valores reales de la variable target.
conf_matrix_arbol2 <- table(modelo_hr_test$prediccion_arbol2, modelo_hr_test$left)
conf_matrix_arbol2

resultados_arbol2 <- confusionMatrix(conf_matrix_arbol2)

accuracy_arbol2 <- resultados_arbol2[[3]][1]

resultados_arbol2


#### Random Forest ####

library(caret)
library(randomForest)
library(funModeling)

modelo_hr_train <- modelo_hr_train %>% 
  mutate(left = factor(left))

data_x <- modelo_hr_train %>% select(-left)
data_y <- modelo_hr_train$left

modelo_hr_rf_train <-  train(x = data_x,
                             y = data_y, 
                             method = "rf",
                             ntree =5 )# caret tiene 230 modelos disponibles

## Generando el modelo predictivo
modelo_hr_rf <- randomForest(left ~ ., modelo_hr_train, ntree=5)

varImpPlot(modelo_hr_rf)



# Genereando la probabilidad de la clase yes
score_rf <- predict(modelo_hr_rf, 
                    modelo_hr_test, 
                    type = 'prob')

score_rf[1:20,]

# Agrego una columna con los resultados de las probabilidades
modelo_hr_test$score_rf <- score_rf[,2]

# En función de las probabilidades hago mi predicción
modelo_hr_test <- modelo_hr_test %>% 
  mutate(prediccion_rf = ifelse(score_rf > 0.5, 1, 0))


# Genero la matriz de confusión
conf_matrix_rf <- table(modelo_hr_test$prediccion_rf, modelo_hr_test$left)

# Veo todas las métricas del modelo
resultados_rf <- confusionMatrix(conf_matrix_rf)
accuracy_rf <- resultados_rf[[3]][1]

resultados_rf

# Validación
gain_lift(data = modelo_hr_test, score = "score_rf", target = "left") 


#### Comparación de los 3 modelos ####

# Arbol de decisión original
accuracy_arbol

# Arbol de decisión "chico"
accuracy_arbol2

# Random Forest
accuracy_rf