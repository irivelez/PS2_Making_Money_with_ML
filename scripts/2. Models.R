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
# RPart Model ####
# Para correr este modelo se tuvo una proporción 0.8 en los datos de train

cv3 <- trainControl(method = "cv", number = 5)

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

## Evaluar modelo RPart ####

y_hat_insample1 <- predict(modelo1,train_df)
y_hat_outsample1 <- predict(modelo1,test_df)

p_load(MLmetrics)

# Insample
MAE(y_pred = y_hat_insample1, y_true = train_df$ln_price)
MAPE(y_pred = y_hat_insample1, y_true = train_df$ln_price)

# Outsample
MAE(y_pred = y_hat_outsample1, y_true = train_df$ln_price)
MAPE(y_pred = y_hat_outsample1, y_true = train_df$ln_price)


# Ranger Model ####
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

## Evaluar modelo Ranger ####

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













## LM ####
### Modelo 1 lm ####
model1_lm<-lm(ln_price~1, data = train)
summary(model1_lm)
coef(model1_lm)

#prediction on new data
test$yhat_model1_lm<-predict(model1_lm,newdata = test)
MSE_model1_lm <- with(test,mean((ln_price-yhat_model1_lm)^2))
MSE_model1_lm

### Modelo 2 lm ####
model2_lm<-lm(ln_price~surface_covered_new, data = train)
summary(model2_lm)
coef(model2_lm)

#prediction on new data
test$yhat_model2_lm<-predict(model2_lm,newdata = test)
MSE_model2_lm <- with(test,mean((ln_price-yhat_model2_lm)^2))
MSE_model2_lm

### Modelo 3 lm ####
model3_lm<-lm(ln_price~surface_covered_new+rooms+bedrooms+bathrooms+
                parqueaderoT+ascensorT+bañoprivado+balcon+vista+remodelado+
                Es_apartamento+distancia_parque+area_parque+distancia_sport_centre+
                distancia_swimming_pool, data = train)
summary(model3_lm)
coef(model3_lm)

#prediction on new data
test$yhat_model3_lm<-predict(model3_lm,newdata = test)
MSE_model3_lm <- with(test,mean((ln_price-yhat_model3_lm)^2))
MSE_model3_lm



# Create vars with all the MSE
mse1_lm<-with(test,round(mean((ln_price-yhat_model1_lm)^2),20))
mse2_lm<-with(test,round(mean((ln_price-yhat_model2_lm)^2),20))
mse3_lm<-with(test,round(mean((ln_price-yhat_model3_lm)^2),20))


#put them in a vector
mse_lm<-c(mse1_lm,mse2_lm,mse3_lm)

#create a data frame
mse_lm_db<-data.frame(model=factor(c("model1","model2","model3"),ordered=TRUE),
               MSE=mse_lm)
mse_lm_db

# FOLDS LM ####
require("caret")
set.seed(0101)
ctrl <- trainControl(
  method = "cv",  # crossvalidation
  number = 5) # número de folds

modelo1_caret_lm <- train(ln_price~surface_covered_new, 
                      data = train, 
                      method = 'lm',
                      trControl= ctrl )

modelo1_caret_lm

# Modelo 1 loocv
ctrl_loocv <- trainControl(
  method = "loocv")

modelo1_caret_loocv_lm <- train(ln_price~surface_covered_new, 
                            data = train, 
                            method = 'lm',
                            trControl= ctrl_loocv )
modelo1_caret_loocv_lm

# Modelo 2 loocv
modelo2_caret_loocv_lm <- train(ln_price~surface_covered_new+rooms+bedrooms+bathrooms+
                                  parqueaderoT+ascensorT+bañoprivado+balcon+vista+remodelado+
                                  Es_apartamento+distancia_parque+area_parque+distancia_sport_centre+
                                  distancia_swimming_pool, 
                                data = train, 
                                method = 'lm',
                                trControl= ctrl_loocv )
modelo2_caret_loocv_lm



## Ridge ####
## Groups
# Variables X
names(train)
X0 <- as.matrix(train  %>% select(-property_id,-price,-surface_covered,-geometry,
                                  -ln_price))

# Variable Y
Y <- train$ln_price

## Models
# Model 1
ridge0 <- glmnet(
  x = X0,
  y = Y,
  alpha = 0 #ridge
)


library(glmnet)

# Variables X
X0 <- as.matrix(train %>% 
                  dplyr::select(-property_id, -price, -surface_covered, -geometry, -ln_price))

# Variable Y
Y <- as.matrix(train$ln_price)
class(Y)

## Models
# Model 1
ridge0 <- glmnet(x = X0, y = Y, alpha = 0)




# Variables X
names(train)
X0 <- sparse.model.matrix(~ . - property_id - price - surface_covered - geometry - ln_price, data = train)

# Variable Y
Y <- train$ln_price

## Models
# Model 1
ridge0 <- glmnet(x = X0, y = Y, alpha = 0)


# Grafica 1 ridge
plot(ridge0, xvar = "lambda")

# Model with CARET
p_load("caret")
set.seed(123)
fitControl <- trainControl(## 5-fold CV, 10 better
  method = "cv",
  number = 5)

ridge1<-train(ln_price~surface_covered_new+rooms+bedrooms+bathrooms+
               parqueaderoT+ascensorT+bañoprivado+balcon+vista+remodelado+
               Es_apartamento+distancia_parque+area_parque+distancia_sport_centre+
               distancia_swimming_pool,
             data=train,
             method = 'glmnet', 
             trControl = fitControl,
             tuneGrid = expand.grid(alpha = 0, #Ridge
                                    lambda = seq(0,20,0.01))
) 

plot(ridge1$results$lambda,
     ridge1$results$RMSE,
     xlab="lambda",
     ylab="Root Mean-Squared Error (RMSE)"
     )

ridge1$bestTune

coef_ridge <- coef(ridge1$finalModel, ridge1$bestTune$lambda)
coef_ridge

ridge1$results$RMSE[which.min(ridge1$results$lambda)]
ridge1$results$MAE[which.min(ridge1$results$lambda)]

## Lasso ####
lasso<-train(ln_price~surface_covered_new+rooms+bedrooms+bathrooms+
               parqueaderoT+ascensorT+bañoprivado+balcon+vista+remodelado+
               Es_apartamento+distancia_parque+area_parque+distancia_sport_centre+
               distancia_swimming_pool,
             data=train,
             method = 'glmnet', 
             trControl = fitControl,
             tuneGrid = expand.grid(alpha = 1, #lasso
                                    lambda = ridge0$lambda)
) 


## Compare Ridge-Lasso
RMSE_df <- cbind()







# Reg 1 Lasso
lasso_no_pen <- glmnet(
  x = X0,
  y = Y,
  alpha = 1, #lasso
  lambda=0
)

lasso_no_pen$beta
summary(lm(Y~X0))





















## CART ####



## Bosque ####







# Test --------------------------------------------------------------------


# Completar base
base <- left_join(db, submission_template, by = "property_id")
summary(base)


