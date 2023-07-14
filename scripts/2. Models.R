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
       rstudioapi)


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


# Models ------------------------------------------------------------------







