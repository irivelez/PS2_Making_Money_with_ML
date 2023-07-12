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
#que nos da los límites geoespaciales para de las UPZ Chapinero en un formato
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


### Aquí la idea es crear algunas variables que identifiquemos de la base de datos y nos puedan
### servir como controles para la predicción. El truco es extraer el texto de description para
### convertir ese texto en variable. El problem set nos pide dos variables de las bases de datos
### y cuatro variables que tendremos que sacar de otro lado

### Variable número 1: Parqueadero
Descripc <- combine_chapinero$description
parqueaderoT_aux1<-str_detect( Descripc,"parqueadero")
parqueaderoT_aux2<-str_detect( Descripc,"parqueaderos") 
parqueaderoT_aux3<-str_detect( Descripc,"parqeadero") 
parqueaderoT_aux4<-str_detect( Descripc,"parqeaderos") 
parqueaderoT_aux5<-str_detect( Descripc,"garaje") 
parqueaderoT_aux6<-str_detect( Descripc,"garajes") 
parqueaderoT_aux7<-str_detect( Descripc,"garage") 
parqueaderoT_aux8<-str_detect( Descripc,"garages") 
parqueaderoT_aux9<-str_detect( Descripc,"garjes") 
parqueaderoT_aux10<-str_detect( Descripc,"garje") 
parqueaderoT<-ifelse(parqueaderoT_aux1==TRUE|parqueaderoT_aux2==TRUE| parqueaderoT_aux3==TRUE|parqueaderoT_aux4==TRUE|parqueaderoT_aux5==TRUE|parqueaderoT_aux6==TRUE|parqueaderoT_aux7==TRUE|parqueaderoT_aux8==TRUE|parqueaderoT_aux9 == TRUE |parqueaderoT_aux10==TRUE , 1,0 )
parqueaderoT<-data.frame(parqueaderoT)
summary(parqueaderoT)
parqueaderoT[is.na(parqueaderoT)] = 0 #Se imputa cero a los datos NA, porque existen datos donde no había descripción
summary(parqueaderoT)
combine_chapinero <- cbind(combine_chapinero, parqueaderoT)
# Contar el número de veces que la variable toma el valor de "1"
count_ascensor <- sum(combine_chapinero$ascensorT == 1)
count_ascensor   ###2312 de 11222 hogares tienen ascensor (21%)
  
### Variable número 2: Parqueadero
ascensorT_aux1<-str_detect( Descripc,"ascensor")
ascensorT_aux2<-str_detect( Descripc,"acensor") 
ascensorT_aux3<-str_detect( Descripc,"asensor") 
ascensorT_aux4<-str_detect( Descripc,"elevador") 
ascensorT_aux5<-str_detect( Descripc,"ascensores") 
ascensorT_aux6<-str_detect( Descripc,"acensores") 
ascensorT_aux7<-str_detect( Descripc,"asensores") 
ascensorT_aux8<-str_detect( Descripc,"elevadores") 
ascensorT<-ifelse(ascensorT_aux1==TRUE|ascensorT_aux2==TRUE| ascensorT_aux3==TRUE|ascensorT_aux4==TRUE|ascensorT_aux5==TRUE|ascensorT_aux6==TRUE|ascensorT_aux7==TRUE|ascensorT_aux8==TRUE, 1,0 )
ascensorT<-data.frame(ascensorT)
summary(ascensorT)
ascensorT[is.na(ascensorT)] = 0 #Se imputa cero a los datos NA, porque existen datos donde no había descripción
summary(ascensorT)
combine_chapinero <- cbind(combine_chapinero, ascensorT)
count_parking <- sum(combine_chapinero$parqueaderoT == 1)
count_parking   ###7733 de 11222 hogares tienen ascensor (69%)


### Variable número 3: Baño privado
bañoprivado_aux1 <-str_detect( Descripc,"bano privado")
bañoprivado_aux2 <-str_detect( Descripc,"baño privado")
bañoprivado <-ifelse(bañoprivado_aux1==TRUE|bañoprivado_aux2==TRUE, 1,0 )
bañoprivado <-data.frame(bañoprivado)
summary(bañoprivado)
bañoprivado[is.na(bañoprivado)] = 0 #Se imputa cero a los datos NA, porque existen datos donde no había descripción
summary(bañoprivado)
combine_chapinero <- cbind(combine_chapinero, bañoprivado)
count_baño <- sum(combine_chapinero$bañoprivado == 1)
count_baño   ###1251 de 11222 hogares tienen ascensor (11%)


### Variable número 4: Balcón/terraza
balcon_aux1 <-str_detect( Descripc,"balcon")
balcon_aux2 <-str_detect( Descripc,"balcón")
balcon_aux3 <-str_detect( Descripc,"terraza")
balcon_aux4 <-str_detect( Descripc,"mirador")
balcon_aux5 <-str_detect( Descripc,"azotea")
balcon_aux6 <-str_detect( Descripc,"asotea")
balcon_aux7 <-str_detect( Descripc,"cornisa")
balcon_aux8 <-str_detect( Descripc,"corniza")
balcon_aux9 <-str_detect( Descripc,"corniza")
balcon <-ifelse(balcon_aux1==TRUE|balcon_aux2==TRUE| balcon_aux3==TRUE|balcon_aux4==TRUE|balcon_aux5==TRUE|balcon_aux6==TRUE|balcon_aux7==TRUE|balcon_aux8==TRUE|balcon_aux9==TRUE, 1,0 )
balcon <-data.frame(balcon)
summary(balcon)
balcon[is.na(balcon)] = 0 #Se imputa cero a los datos NA, porque existen datos donde no había descripción
summary(balcon)
combine_chapinero <- cbind(combine_chapinero, balcon)
count_balcon <- sum(combine_chapinero$balcon == 1)
count_balcon   ###4997 de 11222 hogares tienen ascensor (45%)

### Apartir de las variables creadas anteriormente, a continuación nos remitimos a crear los cuatro predictores
### de fuentes externas, esto con el propósito de añadir determinantes a la vivienda. 

##Creación de la variable distancia de parques a observaciones.
#Cargamos la base de datos geoespaciales de parques.

library(sf)
datos_sf <- st_read("/Users/luciafillippo/Downloads/parque.gpkg")
plot(datos_sf$SHAPE)

