############################################################
#         Problem Set 2. Making Money with ML?
#           Big data and Machine Learning
#             Universidad de los Andes
##########################################################

#Authors:

#- Lucia Fillippo
#- Miguel Angel Victoria Simbaqueva 
#- Irina Andrea Vélez López  
#- Daniel Casas Bautista  

# Initial configuration ---------------------------------------------------

rm(list = ls())

path_sript <- rstudioapi::getActiveDocumentContext()$path
path_folder <- dirname(path_sript)
setwd(path_folder)


## llamado librerías de la sesión
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


# Import data -------------------------------------------------------------
load("../stores/data.Rdata")
url_submission_template <- "https://raw.githubusercontent.com/irivelez/PS2_Making_Money_with_ML/master/stores/submission_template.csv"
submission_template <- read.csv(url_submission_template)

# Unir las bases de datos por "property_id"
merged_data <- merge(db, submission_template, by = "property_id", all.x = TRUE)

# Reemplazar los NAs en "price" de data2 con los valores no NA de data1
merged_data$price <- ifelse(is.na(merged_data$price.x), merged_data$price.y, merged_data$price.x)

# Quitar las columnas redundantes
merged_data <- merged_data[, !(names(merged_data) %in% c("price.x", "price.y"))]

# Depurar
rm(db_merge)
rm(submission_template)
summary(merged_data$price)
merged_data[is.na(merged_data)] <- 0

# Transforming data -------------------------------------------------------
# Create ln_price
merged_data$ln_price <- log(merged_data$price)

# Train/test
p_load(caret)
train.index <- createDataPartition(merged_data$ln_price, p=0.2)$Resample1
train_df <- merged_data[train.index,]
test_df <- merged_data[-train.index,]

# Observar si los grupos quedaron balanceados
summary(train_df$ln_price)
summary(test_df$ln_price)


# View data ---------------------------------------------------------------
ggplot(train_df, aes(x = ln_price)) +
  geom_histogram(fill = "darkblue") +
  theme_bw() +
  labs(x = "Precio de venta", y = "Cantidad")

# Descriptivas - Variable de interes (precio)
summary(train_df$ln_price) %>%
  as.matrix() %>%
  as.data.frame() %>%
  mutate(V1 = scales::dollar(V1))












# Models ------------------------------------------------------------------

## Spatial Dependence ------------------------------------------------------
merged_data_sf <- st_as_sf(
  merged_data,
  # "coords" is in x/y order -- so longitude goes first!
  coords = c("Longitude", "Latitude"),
  # Set our coordinate reference system to EPSG:4326,
  # the standard WGS84 geodetic coordinate reference system
  crs = 4326
)

p_load(spatialsample,sf)
buffer_folds <- spatial_buffer_vfold_cv(merged_data_sf, radius=40,buffer=5)

# Grafica
autoplot(buffer_folds)

# Nota: Revisar si se pueden completar estos modelos de autocorrelación espacial ya que
# no tenemos Neighborhood 


#___________________________
### Folds
require("caret")
set.seed(0101)
cv3 <- trainControl(method = "cv", number = 5)
#__________________________

## LM Model ####
# Para correr este modelo se tuvo que pasar de una proporción en los datos de train de 0.2 para que funcionara
modelo1_caret_lm <- train(ln_price~surface_covered_new, 
                          data = train_df, 
                          method = 'lm',
                          trControl= cv3 )

modelo1_caret_lm

## Evaluar modelo LM
y_hat_insample_lm <- predict(modelo1_caret_lm,train_df)
y_hat_outsample_lm <- predict(modelo1_caret_lm,test_df)

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
                                data = train, 
                                method = 'lm',
                                trControl= ctrl_loocv )
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
  method="rpart",
  trControl=cv3,
  metric="RMSE",
  maximize=F
)

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
  trControl=cv3,
  metric="RMSE",
  maximize=F,
  tuneGrid=tungrid_rf
)

plot(modelo2)

## Evaluar modelo Ranger 

y_hat_insample2 <- predict(modelo2,train_df)
y_hat_outsample2 <- predict(modelo2,test_df)

# Insample
MAE(y_pred = y_hat_insample2, y_true = train_df$ln_price)
MAPE(y_pred = y_hat_insample2, y_true = train_df$ln_price)

# Outsample
MAE(y_pred = y_hat_outsample2, y_true = train_df$ln_price)
MAPE(y_pred = y_hat_outsample2, y_true = train_df$ln_price)


# Notas modelo Ranger
# Se puede revisar si aumentar la proporción para que la evaluación fuera de muestra sea mejor
# Sobre el desempleño, se pueden cambiar los parámetros, tal vez min.node.size pueda empezar desde antes



## Boosting Model ####
install.packages("h2o")
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









