###########################################################
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
       glmnet,
       MLmetrics,
       GGally)

pacman::p_load(sf, spatialRF, purrr)


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

# Estadísticas descriptivasD
DescriptvasVar <- merged_data[,c(12,14,15,16,17,19)]

ggpairs(DescriptvasVar, columns = 1:9, ggplot2::aes(colour = parqueaderoT)) +
  theme_bw()

ggpairs(DescriptvasVar, columns = 1:6) +
  theme_bw()

# Variables dummy
dummy <- merged_data[,6:11]

variables_dummy <- names(dummy[, sapply(dummy, is.numeric)])

for (var in variables_dummy) {
  dummy[[var]] <- factor(dummy[[var]], labels = c("Si", "No"))
}

# Gráfico de barras para cada variable dummy
for (var in variables_dummy) {
  ggplot(dummy, aes(x = .data[[var]], fill = .data[[var]])) +
    geom_bar() +
    labs(title = paste("Distribución de la variable", var),
         x = var,
         y = "Conteo") +
    theme_bw()
}


# Suponiendo que tienes un data frame llamado "dummy" con tus variables dummy (0/1)
# Asegurarse de que las variables dummy sean de tipo factor con etiquetas "Si" y "No"
variables_dummy <- names(dummy[, sapply(dummy, is.numeric)])

for (var in variables_dummy) {
  dummy[[var]] <- factor(dummy[[var]], labels = c("Si", "No"))
}

# Gráfico de barras para cada variable dummy
for (var in variables_dummy) {
  ggplot(dummy, aes(x = .data[[var]], fill = .data[[var]])) +
    geom_bar() +
    labs(title = paste("Distribución de la variable", var),
         x = var,
         y = "Conteo") +
    theme_bw()
}














