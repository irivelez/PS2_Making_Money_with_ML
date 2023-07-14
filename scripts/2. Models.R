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

base <- left_join(submission_template, db, by = "property_id")
base <- base %>% select(-price.y)
base <-  base  %>%   drop_na(surface_covered_new) 



# Transforming data -------------------------------------------------------

# Create ln_price
base$ln_price <- log(base$price.x)
base[is.na(base)] <- 0

# Train/Test
set.seed(123)
sample <- sample(c(TRUE, FALSE), nrow(base), replace=TRUE, prob=c(0.7,0.3))
sum(sample)/nrow(base)
train  <- base[sample, ] 
test   <- base[!sample, ] 

# Models ------------------------------------------------------------------
## Groups
# Variables X
names(base)
X0 <- as.matrix(base  %>% select(-property_id,-geometry,
                                 -price.x,-ln_price))
X0[is.na(X0)] <- 0

# Variable Y
Y <- as.matrix(base$ln_price)


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


## Ridge ####


## Lasso ####

  
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









