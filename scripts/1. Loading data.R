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
    

### Initial Configuration
rm(list = ls())
setwd("C:/Users/Daniel Casas/OneDrive - Universidad de los Andes/Escritorio/DANIEL CASAS/MEcA/Clases/Big data and machine learning/Talleres/Taller 2/Base de Kaggle")

## llamado librerías de la sesión
require(pacman)

p_load(tidyverse,rio,
       sf,
       leaflet,
       tmaptools,
       osmdata,
       nngeo,
       magrittr,
       rgeos)


### Importamos los datos en formato CSV y los convertimos a formato RDS

#Para test
test <- read.csv("test.csv")
saveRDS(test, "test.rds")

#Para train
train <- read.csv("train.csv")
saveRDS(train, "train.rds")

#Para submission_template
submission_template <- read.csv("submission_template.csv")
saveRDS(submission_template, "submission_template.rds")


### Habiendo cargado la base de datos, debemos limpiarla de mayúsculas y tildes para luego combinarlas

#Ponemos en minúscula los caracteres de description y title en la base train
train$description <-str_to_lower(string = train$description)
train$title <-str_to_lower(string = train$title)

#Se ponen en minúscula los caracteres de description y title en la base test
test$description <-str_to_lower(string = test$description)
test$title <-str_to_lower(string = test$title)

#Eliminamos las tildes
train$description <- iconv(train$description, from = "UTF-8", to = "ASCII//TRANSLIT")
test$description <- iconv(test$description, from = "UTF-8", to = "ASCII//TRANSLIT")

#Eliminamos carácteres especiales
train$description <- str_replace_all(train$description, "[^[:alnum:]]", " ")
test$description <- str_replace_all(test$description, "[^[:alnum:]]", " ")

#Eliminamos espacios extras
train$description <- gsub("\\s+", " ", str_trim(train$description))
test$description <- gsub("\\s+", " ", str_trim(test$description))


#Combinar columnas
combine <- bind_rows(train,test) %>% st_as_sf(coords=c("lon","lat"),crs=4326)
class(combine)


#Utilizaremos únicamente datos para los apartamentos ubicados en Chapinero

#Creamos el mapa con leaflet
leaflet() %>% addTiles() %>% addCircles(data=combine)
str(combine)

#Definimos las zonas a analizar
chapinero <- getbb(place_name = "UPZ Chapinero, Bogota", 
                   featuretype = "boundary:administrative", 
                   format_out = "sf_polygon") %>% .$multipolygon


leaflet() %>% addTiles() %>% addPolygons(data= chapinero, col = "red")
st_crs(combine)
st_crs(chapinero)



chapinero <- st_transform(chapinero,st_crs(combine))
combine_chapinero <- combine[chapinero,]
combine_chapinero <- st_crop(combine, chapinero)

leaflet() %>% addTiles() %>% addCircles(data=combine_chapinero) %>% addPolygons(data = chapinero, col = "red")
available_features()
available_tags("amenity")

