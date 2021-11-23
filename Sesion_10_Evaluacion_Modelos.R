library(readr)
library(tidyverse)

datos_rh <- read_csv("https://raw.githubusercontent.com/mlambolla/Analytics_HR_Attrition/master/HR_comma_sep.csv")

glimpse(datos_rh)


# Sacamos el campo 'sales' y cambiemos los valores de 'salary'
datos_rh <- datos_rh %>% 
  mutate(salary = as.numeric(case_when(
    salary == 'low' ~ 0,
    salary == 'medium' ~ 1,
    salary == 'high' ~ 2
  ))) %>% 
  select(satisfaction_level, last_evaluation, number_project,
         average_montly_hours, time_spend_company, Work_accident,
         promotion_last_5years, salary, left)

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

# Creo un dataframe de testing con las filas que no estÃ¡n incluidas en el índice.
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


library(car)

# Hago un análisis de multicolinealidad calculando el VIF
vif(modelo_glm)

# Con predict estimamos las probabilidades para cada uno de los empleados en training
pred_train <- predict(modelo_glm, newdata = modelo_hr_train, type = "response")

# Veamos las primeras 50 probabilidades
pred_train[1:50]

# Graficamos la distribución de las predicciones.
hist(pred_train)


# Ahora hacemos lo mismo con los datos de testing
pred_test <- predict(modelo_glm, newdata = modelo_hr_test, type = "response")

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
plot(perf)

auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
auc

library(pROC)

pROC_obj <- roc(modelo_hr_test$left, modelo_hr_test$score,
                smoothed = FALSE,
                # argumentos del intervalo de confianza
                ci=TRUE, ci.alpha=0.9, stratified=FALSE,
                # argumentos del grÃ¡fico
                plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                print.auc=TRUE, show.thres=TRUE)


gain_lift(data = modelo_hr_test, score = "score", target = "left")

roc_imp <- filterVarImp(x = modelo_hr_train[, 1:8], y = modelo_hr_train$left)
roc_imp

#### Modelo con dos variables ####

# Hago las estimaciones del modelo
modelo_glm_2v <- glm(left~satisfaction_level+salary, family = "binomial", 
                  data = modelo_hr_train)

summary(modelo_glm_2v)


library(car)

# Hago un anÃ¡lisis de multicolinealidad calculando el VIF
vif(modelo_glm_2v)

# Con predict estimamos las probabilidades para cada uno de los empleados en training
pred_train_2v <- predict(modelo_glm_2v, newdata = modelo_hr_train, type = "response")

# Veamos las primeras 50 probabilidades
pred_train_2v[1:50]

# Ahora hacemos lo mismo con los datos de testing
pred_test_2v <- predict(modelo_glm_2v, newdata = modelo_hr_test, type = "response")

# Veamos las primeras 50 probabilidades
pred_test_2v[1:50]

modelo_hr_test_2v <- modelo_hr_test

# Todas las probabilidades en testing las agregamos en una columna llamada "score"
modelo_hr_test_2v$score <- pred_test_2v


# Creamos una nueva columna con la predicción, cuando la probabilidad es mayor a 0.5 la predicción es 1
modelo_hr_test_2v <- modelo_hr_test_2v %>% 
  mutate(prediccion = ifelse(score > 0.5, 1, 0))

# Creamos la matriz de confusión, con los valores predichos y los valores reales de la variable target.
conf_matrix_2v <- table(modelo_hr_test_2v$prediccion, modelo_hr_test_2v$left)
conf_matrix_2v

# Veamos todas las métricas de la matriz con esta función del paquete caret
confusionMatrix(conf_matrix_2v)

#### Otra cosa ####

ggplot(modelo_hr_test, aes(x = last_evaluation, y = satisfaction_level, color = factor(left)))+
  geom_point(alpha = 0.8)+
  scale_color_manual(values = c("#BFC9CA","#2874A6"))


# Seleccionamos las variables para elegir los clusters
variables_cluster <- modelo_hr_test %>%
  select(last_evaluation, satisfaction_level)


# Preparo los datos para hacer el cÃ¡lculo
vc <- scale(variables_cluster)

# Corro el algoritmo de clustering
fit_vc <- kmeans(vc, 3)

modelo_hr_test_kmeans <- modelo_hr_test

# Agrego los clusters ajustados (calculados) al dataset
modelo_hr_test_kmeans$cluster <- fit_vc$cluster

library(ggthemes)
ggplot(modelo_hr_test_kmeans, aes(x = last_evaluation, y = satisfaction_level, color = factor(cluster)))+
  geom_point(alpha = 0.8)+
  scale_color_colorblind()
  


modelo_hr_c1 <- modelo_hr_test_kmeans %>% 
  filter(cluster == 2)

conf_matrix_c1 <- table(modelo_hr_c1$prediccion, modelo_hr_c1$left)

# Veamos todas las métricas de la matriz con esta función del paquete caret
confusionMatrix(conf_matrix_c1)

# Visualizando. Vamos a contar una historia!!
library(funModeling)
coord_plot(data=modelo_hr_test, 
           group_var="cluster", 
           group_func=mean, 
           print_table=TRUE)

gain_lift(data = modelo_hr_test, score = "score", target = "left")
gain_lift(data = modelo_hr_test_2v, score = "score", target = "left")


#### Random Forest ####
library(caret)
library(randomForest)
library(funModeling)

# Cambiamos a left como una variable de tipo factor
modelo_hr_train <- modelo_hr_train %>% mutate(left = factor(left))

# Dividimos el dataset en predictores y variable dependiente
data_x <- modelo_hr_train %>% select(-left)
data_y <- modelo_hr_train$left

#Entrenamos el modelo
modelo_hr_rf_train <-  train(x = data_x,
                             y = data_y, 
                             method = "rf",
                             ntree =5)

## Generando el modelo predictivo
modelo_hr_rf <- randomForest(left ~ ., modelo_hr_train, ntree=5)

# Generando la probabilidad de la clase yes
score_rf <- predict(modelo_hr_rf, 
                    modelo_hr_test, 
                    type = 'prob')


modelo_hr_test$score_rf <- score_rf[,2]

# Validación
gain_lift(data = modelo_hr_test, score = "score_rf", target = "left")

modelo_hr_test <- modelo_hr_test %>% 
  mutate(prediccion_rf = ifelse(score_rf > 0.5, 1, 0))

conf_matrix_rf <- table(modelo_hr_test$prediccion_rf, modelo_hr_test$left)

confusionMatrix(conf_matrix_rf)

glimpse(modelo_hr_test)

# Agrego los clusters ajustados (calculados) al dataset
modelo_hr_test$cluster <- fit_vc$cluster

modelo_rf_c2 <- modelo_hr_test %>% 
  filter(cluster == 2)

conf_matrix_rf_c2 <- table(modelo_rf_c2$prediccion_rf, modelo_rf_c2$left)

# Veamos todas las métricas de la matriz con esta función del paquete caret
confusionMatrix(conf_matrix_rf_c2)


