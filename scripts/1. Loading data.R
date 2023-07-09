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


### Importamos los datos desde Github en formato CSV y los convertimos a formato RDS

#Para test
url_test <- "https://raw.githubusercontent.com/irivelez/PS2_Making_Money_with_ML/master/stores/test.csv"
test <- read.csv(url_test)
saveRDS(test, "test.rds")


#Para train
url_train <- "https://raw.githubusercontent.com/irivelez/PS2_Making_Money_with_ML/master/stores/train.csv"
train <- read.csv(url_train)
saveRDS(train, "train.rds")

#Para submission_template
url_submission_template <- "https://raw.githubusercontent.com/irivelez/PS2_Making_Money_with_ML/master/stores/submission_template.csv"
submission_template <- read.csv(url_submission_template)
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


##########################################################################
#Utilizaremos únicamente datos para los apartamentos ubicados en Chapinero
##########################################################################


#Creamos el mapa interactivo con la biblioteca leaflet agregando círculos 
#a partir de las coordenadas de datos de nuestra base "combine"

leaflet() %>% addTiles() %>% addCircles(data=combine)
str(combine)


#Definimos las zonas a analizar mediante el código getbb del paquete osmdata
#que nos da los límites geoespaciales para de la UPZ Chapinero en un formato
#espacial (sf); además, el multipolygon extrae los polígonos de la respuesta 
#y los asigna a la variable que hemos llamado chapinero.

chapinero <- getbb(place_name = "UPZ Chapinero, Bogota", 
                   featuretype = "boundary:administrative", 
                   format_out = "sf_polygon") %>% .$multipolygon


#Aquí creamos el mapa solo de Chapinero con los datos de la variable que creamos
leaflet() %>% addTiles() %>% addPolygons(data= chapinero, col = "red")
st_crs(combine)
st_crs(chapinero)

#Esto transforma la variable chapinero para que coincida con el CRS de combine
chapinero <- st_transform(chapinero,st_crs(combine))

#Creamos un objeto llamado combine_chapinero que contiene solo los datos de
#combine que se encuentran en Chapinero
combine_chapinero <- combine[chapinero,]
combine_chapinero <- st_crop(combine, chapinero)

#Aquí se verán juntas las áreas
leaflet() %>% addTiles() %>% addCircles(data=combine_chapinero) %>% addPolygons(data = chapinero, col = "red")
available_features()
available_tags("amenity")



