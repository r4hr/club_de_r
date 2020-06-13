library(readr)
library(tidyverse)

datos_rh <- read_csv("https://raw.githubusercontent.com/mlambolla/Analytics_HR_Attrition/master/HR_comma_sep.csv")

glimpse(datos_rh)

# Nos aseguramos que no hay datos faltantes
any(is.na(datos_rh))

# Cambiemos el nombre de la variable 'sales' por 'department' y cambiemos los valores de 'salary'
datos_rh <- datos_rh %>% 
  rename(department = sales) %>%
  mutate(salary = as.numeric(case_when(
    salary == 'low' ~ 0,
    salary == 'medium' ~ 1,
    salary == 'high' ~ 2
  )))

# Vemos la cantidad de bajas del dataset
datos_rh %>% 
  count(left)



# Crear datasets de training y de test.
library(caret)

set.seed(234)

# Crea un índice con las filas seleccionadas para generar un training set con el 70% de los datos.
modelo_hr <- createDataPartition(y = datos_rh$left, p = 0.7,
                                      list = FALSE)

# Creo un dataframe con los datos de entrenamiento seleccionados en el modelo_hr
modelo_hr_train <- datos_rh[modelo_hr,]

# Creo un dataframe de testing con las filas que no están incluidas en el índice.
modelo_hr_test <- datos_rh[-modelo_hr,]


# Me aseguro que las proporciones de bajas (left = 1) sean similares en los datos de training y de testing
modelo_hr_train %>%
  summarise(turnover = mean(left))

modelo_hr_test %>%
  summarise(turnover = mean(left))


# Hago las estimaciones del modelo
modelo_glm <- glm(left~., family = "binomial", 
                 data = modelo_hr_train)

summary(modelo_glm)

# El p-value de department de las mayorías de sus valores es mayor a 0.05, con lo cual la variable no es estadísticamente significativa
# Sacamos department del cálculo generando un nuevo modelo.
modelo_glm2 <- glm(left ~. -department, family = "binomial",
                   data = modelo_hr_train)

summary(modelo_glm2)

library(car)

# Hago un análisis de multicolinealidad calculando el VIF
vif(modelo_glm2)

# Con predict estimamos las probabilidades para cada uno de los empleados en training
pred_train <- predict(modelo_glm2, newdata = modelo_hr_train, type = "response")

# Veamos las primeras 50 probabilidades
pred_train[1:50]

# Graficamos la distribución de las predicciones.
hist(pred_train)


# Ahora hacemos lo mismo con los datos de testing
pred_test <- predict(modelo_glm2, newdata = modelo_hr_test, type = "response")

# Veamos las primeras 50 probabilidades
pred_test[1:50]

# Graficamos la distribución de las predicciones.
hist(pred_test)

# Todas las probabilidades en testing las agregamos en una columna llamada "score"
modelo_hr_test$score <- pred_test


# Creamos una nueva columna con la predicción, cuando la probabilidad es mayor a 0.5 la predicción es 1
modelo_hr_test <- modelo_hr_test %>% 
  mutate(prediccion = ifelse(score > 0.5, 1, 0))

# Vemos las dos nuevas columnas
glimpse(modelo_hr_test)

# Creamos la matriz de confusión, con los valores predichos y los valores reales de la variable target.
conf_matrix <- table(modelo_hr_test$prediccion, modelo_hr_test$left)
conf_matrix

# Veamos todas las métricas de la matriz con esta función del paquete caret
confusionMatrix(conf_matrix)


# Hacemos una curva ROC
library(ROCR)

pred <- prediction(modelo_hr_test$score, modelo_hr_test$left)
perf <- performance(pred, "tpr", "fpr")
plot(perf,colorize = TRUE)
