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


# Option 1: Groups ------------------------------------------------------------------

# Como se juntaron las bases train y tes, luego se filtraron los datos  que solo son de chapinero
# y se añadieron nuevas variables; para entrenar los modelos es necesario tener divididos el grupo
# de entrenamiento y de control nuevamente. Por lo cual debemos realizar nuevamente esta división:

### Train Group
filtro <- is.na(combine_chapinero$price)
Train_combine <- combine_chapinero[!filtro, ]

# Grafica
ggplot(Train_combine, aes(x = price)) +
  geom_histogram(fill = "darkblue") +
  theme_bw() +
  labs(x = "Precio de venta", y = "Cantidad")

#Descriptivas - Variable de interes (precio)
summary(Train_combine$price) %>%
  as.matrix() %>%
  as.data.frame() %>%
  mutate(V1 = scales::dollar(V1))

### Test Group
Test_combine <- combine_chapinero[filtro, ]




# Option 2: Submission template  ---------------------------------------------------
bd <- left_join(submission_template, combine_chapinero, by = "property_id")



# Transforming data -------------------------------------------------------

# Create ln_price
bd$ln_price <- log(bd$price.x)

# Train/Test
set.seed(123)
sample <- sample(c(TRUE, FALSE), nrow(bd), replace=TRUE, prob=c(0.7,0.3))
sum(sample)/nrow(bd)
train  <- bd[sample, ] 
test   <- bd[!sample, ] 



# Models ------------------------------------------------------------------


## LM ####
## CONTINUAR CON MODELOS SIMPLES
model1_lm<-lm(ln_price~1,data=train)
summary(model1_lm)

stargazer(model1_lm,
          type = "text", 
          dep.var.labels = "Precio del casa",
          digits = 0
)


## Ridge ####


## Lasso ####

# Variables X
names(bd)
X0 <- as.matrix(bd  %>% select(-property_id,-price.y,-city,-month,-year,-surface_total,-property_type,
                               -operation_type,-title,-description,-geometry,-MetrosCuadrados,-NumeroBanos,
                               -price.x,-ln_price,-MetrosCuadrados_new))
X0[is.na(X0)] <- 0

# Variable Y
Y <- bd$ln_price
  
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









