############################################################
#         Problem Set 2. Making Money with ML?
#           Big data and Machine Learning
#             Universidad de los Andes
#___________________________________________________________

#Authors:

#- Lucia Fillippo
#- Miguel Angel Victoria Simbaqueva 
#- Irina Andrea Vélez López  
#- Daniel Casas Bautista  

# Initial configuration --------------------------------------------------------

rm(list = ls())

path_sript <- rstudioapi::getActiveDocumentContext()$path
path_folder <- dirname(path_sript)
setwd(path_folder)


# Librerías para la sesión de trabajo ------------------------------------------
if(!require(pacman)) install.packages("pacman")
require(pacman)

p_load(tidyverse,rio,
       sf,
       leaflet,
       tmaptools,
       osmdata,
       nngeo,
       magrittr,
       rgeos,
       rio,
       rstudioapi,
       stargazer,
       glmnet)

pacman::p_load(sf, spatialRF, purrr)

# Import data -------------------------------------------------------------
load("../stores/data.Rdata")
load("../stores/base_fin_test.Rdata")
url_submission_template <- "https://raw.githubusercontent.com/irivelez/PS2_Making_Money_with_ML/master/stores/submission_template.csv"
submission_template <- read.csv(url_submission_template)

# Transforming data -------------------------------------------------------------
# Unir las bases de datos por "property_id"
merged_data <- merge(db, submission_template, by = "property_id", all.x = TRUE)

# Reemplazar los NAs en "price" de data2 con los valores no NA de data1
merged_data$price <- ifelse(is.na(merged_data$price.x), merged_data$price.y, merged_data$price.x)

# Quitar las columnas redundantes
merged_data <- merged_data[, !(names(merged_data) %in% c("price.x", "price.y"))]

# Depurar
rm(submission_template)
summary(merged_data$price)
merged_data[is.na(merged_data)] <- 0

# Create ln_price
merged_data$ln_price <- log(merged_data$price)

# Dividiendo la muestra en Train/test ------------------------------------------
p_load(caret)
train.index <- createDataPartition(merged_data$ln_price, p=0.3)$Resample1
train_df <- merged_data[train.index,]
test_df <- merged_data[-train.index,]

# Observar si los grupos quedaron balanceados y visualizar la distribución de los datos
summary(train_df$ln_price)
summary(test_df$ln_price)

ggplot(train_df, aes(x = ln_price)) +
  geom_histogram(fill = "darkblue") +
  theme_bw() +
  labs(x = "Precio de venta", y = "Cantidad")

ggplot(test_df, aes(x = ln_price)) +
  geom_histogram(fill = "darkblue") +
  theme_bw() +
  labs(x = "Precio de venta", y = "Cantidad")



# Análisis descriptivo (precio) ------------------------------------------------
summary(train_df$ln_price) %>%
  as.matrix() %>%
  as.data.frame() %>%
  mutate(V1 = scales::dollar(V1))


summary(test_df$ln_price) %>%
  as.matrix() %>%
  as.data.frame() %>%
  mutate(V1 = scales::dollar(V1))

# Spatial Dependence ------------------------------------------------------
merged_data_sf <- st_as_sf(
  merged_data,
  # "coords" is in x/y order -- so longitude goes first!
  coords = c("Longitude", "Latitude"),
  # Set our coordinate reference system to EPSG:4326,
  # the standard WGS84 geodetic coordinate reference system
  crs = 4326
)

# Spatial Blocks ----------------------------------------------------------
p_load(spatialsample,sf)

install.packages("caret")
require("caret")

set.seed(1357)
block_folds <- spatial_block_cv(method ="cv", v = 5)
autoplot(block_folds)

# Models ------------------------------------------------------------------

## LM Model ####
# Para correr este modelo se tuvo que pasar de una proporción en los datos de train de 0.2 para que funcionara
block_folds <- trainControl(method = "CV", number = 5)
modelo1_caret_lm <- train(ln_price~surface_covered_new, 
                          data = train_df, 
                          method = 'lm',
                          trControl= block_folds )

modelo1_caret_lm

## Evaluar modelo LM
y_hat_insample_lm <- predict(modelo1_caret_lm,train_df)
y_hat_outsample_lm <- predict(modelo1_caret_lm,test_df)

results <- data.frame(ID = test_df$property_id, Pred_Price = y_hat_outsample_lm)

write.csv(results, "predicciones.csv", row.names = FALSE)



p_load(MLmetrics)
# Insample
MAE(y_pred = y_hat_insample_lm, y_true = train_df$ln_price)
MAPE(y_pred = y_hat_insample_lm, y_true = train_df$ln_price)

# Outsample
MAE(y_pred = y_hat_outsample_lm, y_true = train_df$ln_price)
MAPE(y_pred = y_hat_outsample_lm, y_true = train_df$ln_price)



## Loocv Model 1 ####
# Para correr este modelo se tuvo que pasar de una proporción en los datos de train de 0.2 para que funcionara
ctrl_loocv <- trainControl(
  method = "loocv")

modelo1_caret_loocv_lm <- train(ln_price~surface_covered_new, 
                                data = train_df, 
                                method = 'lm',
                                trControl= ctrl_loocv )
modelo1_caret_loocv_lm

## Evaluar modelo loocv 
y_hat_insample_loocv <- predict(modelo1_caret_loocv_lm,train_df)
y_hat_outsample_loocv <- predict(modelo1_caret_loocv_lm,test_df)

# Insample
MAE(y_pred = y_hat_insample_loocv, y_true = train_df$ln_price)
MAPE(y_pred = y_hat_insample_loocv, y_true = train_df$ln_price)

# Outsample
MAE(y_pred = y_hat_outsample_loocv, y_true = train_df$ln_price)
MAPE(y_pred = y_hat_outsample_loocv, y_true = train_df$ln_price)

## Loocv Model 2 ####
# Para correr este modelo se tuvo que pasar de una proporción en los datos de train de 0.2 para que funcionara
modelo2_caret_loocv_lm <- train(ln_price~surface_covered_new+rooms+bedrooms+bathrooms+
                                  parqueaderoT+ascensorT+bañoprivado+balcon+vista+remodelado+
                                  Es_apartamento+distancia_parque+area_parque+distancia_sport_centre+
                                  distancia_swimming_pool, 
                                data = train_df, 
                                method = 'lm',
                                trControl= ctrl_loocv)
modelo2_caret_loocv_lm

## Evaluar modelo loocv 
y_hat_insample_loocv2 <- predict(modelo2_caret_loocv_lm,train_df)
y_hat_outsample_loocv2 <- predict(modelo2_caret_loocv_lm,test_df)

# Insample
MAE(y_pred = y_hat_insample_loocv2, y_true = train_df$ln_price)
MAPE(y_pred = y_hat_insample_loocv2, y_true = train_df$ln_price)

# Outsample
MAE(y_pred = y_hat_outsample_loocv2, y_true = train_df$ln_price)
MAPE(y_pred = y_hat_outsample_loocv2, y_true = train_df$ln_price)




## RPart Model ####
# Para correr este modelo se tuvo una proporción 0.8 en los datos de train

modelo1 <- train(
  ln_price~surface_covered_new+rooms+bedrooms+bathrooms+
    parqueaderoT+ascensorT+bañoprivado+balcon+vista+remodelado+
    Es_apartamento+distancia_parque+area_parque+distancia_sport_centre+
    distancia_swimming_pool,
  data = train_df,
  method ="rpart",
  trControl = ctrl_loocv,
  metric ="RMSE",
  maximize = F
)

results <- data.frame(model = factor (c("modelo1_caret_lm", 
                                        "modelo1_caret_loocv_lm",
                                        "modelo2_caret_loocv_lm"), 
                                      ordered = TRUE))
results

p_load(rattle)
modelo1$finalModel
fancyRpartPlot(modelo1$finalModel)

## Evaluar modelo RPart

y_hat_insample1 <- predict(modelo1,train_df)
y_hat_outsample1 <- predict(modelo1,test_df)

p_load(MLmetrics)

# Insample
MAE(y_pred = y_hat_insample1, y_true = train_df$ln_price)
MAPE(y_pred = y_hat_insample1, y_true = train_df$ln_price)

# Outsample
MAE(y_pred = y_hat_outsample1, y_true = train_df$ln_price)
MAPE(y_pred = y_hat_outsample1, y_true = train_df$ln_price)

# Notas: Este es un modelo simple en el que no se modifican los grid

## Ranger Model ####
# Para correr este modelo se tuvo que pasar de una proporción en los datos de train de 0.2 para que funcionara

# Grid
p_load(ranger)

tungrid_rf <- expand.grid(
  min.node.size=c(10,30,50,70,100),
  mtry=c(3,5,10),
  splitrule=c("variance")
)

modelo2 <- train(
  ln_price~surface_covered_new+rooms+bedrooms+bathrooms+
    parqueaderoT+ascensorT+bañoprivado+balcon+vista+remodelado+
    Es_apartamento+distancia_parque+area_parque+distancia_sport_centre+
    distancia_swimming_pool,
  data = train_df,
  method="ranger",
  trControl=block_folds,
  metric="RMSE",
  maximize=F,
  tuneGrid=tungrid_rf
)

plot(modelo2)

## Evaluar modelo Ranger 

y_hat_outsample_ranger1 <- predict(modelo2, base_fin_test)
y_hat_outsample_ranger1_cop <- exp(y_hat_outsample_ranger1)

results_4 <- data.frame(property_id = base_fin_test$property_id, price = y_hat_outsample_ranger1_cop)
write.csv(results_4, "predicciones_4.csv", row.names = FALSE)


## Modelo Ranger 2

tungrid_rf <- expand.grid(
  min.node.size=c(5,10,30,50),
  mtry=c(3,5,10),
  splitrule=c("variance")
)

modelo3 <- train(
  ln_price~surface_covered_new+rooms+bedrooms+bathrooms+
    parqueaderoT+ascensorT+bañoprivado+balcon+vista+remodelado+
    Es_apartamento+distancia_parque+area_parque+distancia_sport_centre+
    distancia_swimming_pool,
  data = train_df,
  method="ranger",
  trControl=block_folds,
  metric="RMSE",
  maximize=F,
  tuneGrid=tungrid_rf
)

plot(modelo3)

## Evaluar modelo Ranger 

y_hat_outsample_ranger2 <- predict(modelo3, base_fin_test)
y_hat_outsample_ranger2_cop <- exp(y_hat_outsample_ranger2)

results_5 <- data.frame(property_id = base_fin_test$property_id, price = y_hat_outsample_ranger2_cop)
write.csv(results_5, "predicciones_5.csv", row.names = FALSE)

## Modelo Ranger 3

tungrid_rf <- expand.grid(
  min.node.size=c(10,20,30,40,50,70),
  mtry=c(10,12,15),
  splitrule=c("variance")
)

modelo4 <- train(
  ln_price~surface_covered_new+rooms+bedrooms+bathrooms+
    parqueaderoT+ascensorT+bañoprivado+balcon+vista+remodelado+
    Es_apartamento+distancia_parque+area_parque+distancia_sport_centre+
    distancia_swimming_pool,
  data = train_df,
  method="ranger",
  trControl=block_folds,
  metric="RMSE",
  maximize=F,
  tuneGrid=tungrid_rf
)

plot(modelo4)

## Evaluar modelo Ranger 

y_hat_outsample_ranger3 <- predict(modelo4, base_fin_test)
y_hat_outsample_ranger3_cop <- exp(y_hat_outsample_ranger3)

results_7 <- data.frame(property_id = base_fin_test$property_id, price = y_hat_outsample_ranger3_cop)
write.csv(results_7, "predicciones_7.csv", row.names = FALSE)


# Notas modelo Ranger
# Se puede revisar si aumentar la proporción para que la evaluación fuera de muestra sea mejor
# Sobre el desempleño, se pueden cambiar los parámetros, tal vez min.node.size pueda empezar desde antes



## Boosting Model ####
install.packages("h2o", repos = "https://cran.r-project.org")

library(h2o)
h2o.init(nthreads = 4)

tunegrid_gbm <- expand.grid(
  learn_rate=c(0.1, 0.01, 0.001),
  ntrees= c(50,100,500),
  max_depth=10,
  min_rows=70,
  col_sample_rate=0.2
)

modelo3 <- train(
  ln_price~surface_covered_new+rooms+bedrooms+bathrooms+
    parqueaderoT+ascensorT+bañoprivado+balcon+vista+remodelado+
    Es_apartamento+distancia_parque+area_parque+distancia_sport_centre+
    distancia_swimming_pool,
  data = train_df,
  method="gbm_h2o",
  trControl=cv3,
  metric='RMSE',
  tuneGrid=tunegrid_gbm
)

### 

install.packages("xgboost")
library(xgboost)

# Definir la matriz de características y la variable objetivo
X <- train_df[, c("surface_covered_new", "rooms", "bedrooms", "bathrooms", "parqueaderoT", "ascensorT", "bañoprivado", "balcon", "vista", "remodelado", "Es_apartamento", "distancia_parque", "area_parque", "distancia_sport_centre", "distancia_swimming_pool")]
y <- train_df$ln_price

# Definir la grilla de afinamiento
tunegrid_xgboost <- expand.grid(
  eta = c(0.1, 0.01, 0.001),
  nrounds = c(50, 100, 500),
  max_depth = 10,
  min_child_weight = 70,
  subsample = 0.2
)

# Ajustar el modelo de boosting
modelo3 <- xgboost(data = as.matrix(X), label = y, objective = "reg:linear", nthread = -1,
                   eta = tunegrid_xgboost$eta, nrounds = tunegrid_xgboost$nrounds,
                   max_depth = tunegrid_xgboost$max_depth, min_child_weight = tunegrid_xgboost$min_child_weight,
                   subsample = tunegrid_xgboost$subsample, eval_metric = "rmse")


  

# Notas: No sé por qué no me funcionó, revisar

#______________________________________
# Notas generales: 

# 1. El analisis descriptivo puede ser como en este libro:
# https://bloqueneon.uniandes.edu.co//content/enforced/205135-UN_202313_MECA_4107_13/05_Trees_n_Boosting.html?ou=205135

# 2. Recordar que se puso la variable price en ln, al final de cada modelo hay que retornarla y ver su performance.

# 3. Completar la parte de autocorrelacion espacial (Elastic net), ya que es logico que haya autocorrelacion

# 3. Mejorar los modelos consiste en cambiar los hiperparametros de cada modelo, añadir o mejorar variables, encontrar
# el numero optimo de particion entre train y test para que mejore la prediccion fuera de muestra pero que no se demore tanto,
# eso toca a ojo.

# 4. Entender muy bien el significado de cada hiperparametro en cada modelo para explicar por que seleccionamos los nuestros 
# en el doc

# 5. Si queda tiempo, se pueden probar otros modelos, tomando como base: 
# https://topepo.github.io/caret/available-models.html

# 6. Si nos sigue sobrando tiempo, probar ridge y lasso









