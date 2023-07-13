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


# Import data -------------------------------------------------------------

### Importamos los datos desde Github en formato CSV y los convertimos a formato RDS

#Para test
url_test <- "https://raw.githubusercontent.com/irivelez/PS2_Making_Money_with_ML/master/stores/test.csv"
test <- read.csv(url_test)
saveRDS(test, "test.rds")
summary(test)
head(test)

#Para train
url_train <- "https://raw.githubusercontent.com/irivelez/PS2_Making_Money_with_ML/master/stores/train.csv"
train <- read.csv(url_train)
saveRDS(train, "train.rds")
summary(train)
head(train)

#Para submission_template
url_submission_template <- "https://raw.githubusercontent.com/irivelez/PS2_Making_Money_with_ML/master/stores/submission_template.csv"
submission_template <- read.csv(url_submission_template)
saveRDS(submission_template, "submission_template.rds")


# Clean data --------------------------------------------------------------

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

# Base final 
combine <- bind_rows(train,test) %>% st_as_sf(coords=c("lon","lat"),crs=4326)
class(combine)


# View data ---------------------------------------------------------------

## Datos en general
glimpse(train)


## Grafica - Variable de interes (precio)
# Su distribución es más como una bernulli
ggplot(train, aes(x = price)) +
  geom_histogram(fill = "darkblue") +
  theme_bw() +
  labs(x = "Precio de venta", y = "Cantidad")

## ## Descriptivas - Variable de interes (precio)
summary(train$price) %>%
  as.matrix() %>%
  as.data.frame() %>%
  mutate(V1 = scales::dollar(V1))

# Ubicacion Chapinero -----------------------------------------------------

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

# Description data --------------------------------------------------------

### Aquí la idea es crear algunas variables que identifiquemos de la base de datos y nos puedan
### servir como controles para la predicción. El truco es extraer el texto de description para
### convertir ese texto en variable. El problem set nos pide dos variables de las bases de datos
### y cuatro variables que tendremos que sacar de otro lado

### Variable número 1: Parqueadero ####
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
parqueaderoT<-ifelse(parqueaderoT_aux1==TRUE|parqueaderoT_aux2==TRUE| parqueaderoT_aux3==TRUE|
                       parqueaderoT_aux4==TRUE|parqueaderoT_aux5==TRUE|parqueaderoT_aux6==TRUE|
                       parqueaderoT_aux7==TRUE|parqueaderoT_aux8==TRUE|parqueaderoT_aux9 == TRUE |
                       parqueaderoT_aux10==TRUE , 1,0 )
parqueaderoT<-data.frame(parqueaderoT)
summary(parqueaderoT)
parqueaderoT[is.na(parqueaderoT)] = 0 #Se imputa cero a los datos NA, porque existen datos donde no había descripción
summary(parqueaderoT)
combine_chapinero <- cbind(combine_chapinero, parqueaderoT)
# Contar el número de veces que la variable toma el valor de "1"
prop_parking <- round(sum(combine_chapinero$parqueaderoT == 1)/nrow(combine_chapinero)*100,1)
paste("El",prop_parking, "%", "de apartamentos tienen parqueadero")

# Limpiar ambiente
rm(parqueaderoT_aux1,parqueaderoT_aux2,parqueaderoT_aux3,parqueaderoT_aux4,
   parqueaderoT_aux5,parqueaderoT_aux6,parqueaderoT_aux7,parqueaderoT_aux8,
   parqueaderoT_aux9,parqueaderoT_aux10)
rm(parqueaderoT)

### Variable número 2: Elevador ####
ascensorT_aux1<-str_detect( Descripc,"ascensor")
ascensorT_aux2<-str_detect( Descripc,"acensor") 
ascensorT_aux3<-str_detect( Descripc,"asensor") 
ascensorT_aux4<-str_detect( Descripc,"elevador") 
ascensorT_aux5<-str_detect( Descripc,"ascensores") 
ascensorT_aux6<-str_detect( Descripc,"acensores") 
ascensorT_aux7<-str_detect( Descripc,"asensores") 
ascensorT_aux8<-str_detect( Descripc,"elevadores") 
ascensorT<-ifelse(ascensorT_aux1==TRUE|ascensorT_aux2==TRUE| ascensorT_aux3==TRUE|
                    ascensorT_aux4==TRUE|ascensorT_aux5==TRUE|ascensorT_aux6==TRUE|
                    ascensorT_aux7==TRUE|ascensorT_aux8==TRUE, 1,0 )
ascensorT<-data.frame(ascensorT)
summary(ascensorT)
ascensorT[is.na(ascensorT)] = 0 #Se imputa cero a los datos NA, porque existen datos donde no había descripción
summary(ascensorT)
combine_chapinero <- cbind(combine_chapinero, ascensorT)
# Contar
prop_ascensor <- round(sum(combine_chapinero$ascensorT == 1)/nrow(combine_chapinero)*100,1)
paste("El",prop_ascensor, "%", "de apartamentos tienen ascensor")

# Limpiar ambiente
rm(ascensorT_aux1,ascensorT_aux2,ascensorT_aux3,ascensorT_aux4,
   ascensorT_aux5,ascensorT_aux6,ascensorT_aux7,ascensorT_aux8)
rm(ascensorT)

### Variable número 3: Baño privado ####
bañoprivado_aux1 <-str_detect( Descripc,"bano privado")
bañoprivado_aux2 <-str_detect( Descripc,"baño privado")
bañoprivado <-ifelse(bañoprivado_aux1==TRUE|bañoprivado_aux2==TRUE, 1,0 )
bañoprivado <-data.frame(bañoprivado)
summary(bañoprivado)
bañoprivado[is.na(bañoprivado)] = 0 #Se imputa cero a los datos NA, porque existen datos donde no había descripción
summary(bañoprivado)
combine_chapinero <- cbind(combine_chapinero, bañoprivado)
# Contar
prop_bano <- round(sum(combine_chapinero$bañoprivado == 1)/nrow(combine_chapinero)*100,1)
paste("El",prop_bano, "%", "de apartamentos tienen baño privado")

# Limpiar ambiente
rm(bañoprivado_aux1,bañoprivado_aux2)
rm(bañoprivado)

### Variable número 4: Balcón/terraza ####
balcon_aux1 <-str_detect( Descripc,"balcon")
balcon_aux2 <-str_detect( Descripc,"balcón")
balcon_aux3 <-str_detect( Descripc,"terraza")
balcon_aux4 <-str_detect( Descripc,"mirador")
balcon_aux5 <-str_detect( Descripc,"azotea")
balcon_aux6 <-str_detect( Descripc,"asotea")
balcon_aux7 <-str_detect( Descripc,"cornisa")
balcon_aux8 <-str_detect( Descripc,"corniza")
balcon_aux9 <-str_detect( Descripc,"corniza")
balcon <-ifelse(balcon_aux1==TRUE|balcon_aux2==TRUE| balcon_aux3==TRUE|balcon_aux4==TRUE|
                  balcon_aux5==TRUE|balcon_aux6==TRUE|balcon_aux7==TRUE|balcon_aux8==TRUE|
                  balcon_aux9==TRUE, 1,0 )
balcon <-data.frame(balcon)
summary(balcon)
balcon[is.na(balcon)] = 0 #Se imputa cero a los datos NA, porque existen datos donde no había descripción
summary(balcon)
combine_chapinero <- cbind(combine_chapinero, balcon)
# Contar
prop_balcon <- round(sum(combine_chapinero$balcon == 1)/nrow(combine_chapinero)*100,1)
paste("El",prop_balcon, "%", "de apartamentos tienen balcón")

# Limpiar ambiente
rm(balcon_aux1,balcon_aux2,balcon_aux3,balcon_aux4,balcon_aux5,balcon_aux6,
   balcon_aux7,balcon_aux8,balcon_aux9)
rm(balcon)

### Variable número 5: Vista ####
vista_aux1 <-str_detect( Descripc,"vista")
vista <-ifelse(vista_aux1==TRUE, 1,0 )
vista <-data.frame(vista)
summary(vista)
vista[is.na(vista)] = 0 #Se imputa cero a los datos NA, porque existen datos donde no había descripción
summary(vista)
combine_chapinero <- cbind(combine_chapinero, vista)
# Contar
prop_vista <- round(sum(combine_chapinero$vista == 1)/nrow(combine_chapinero)*100,1)
paste("El",prop_vista, "%", "de apartamentos tienen vista")

# Limpiar ambiente
rm(vista_aux1)
rm(vista)

### Variable número 6: Remodelado ####
remod_aux1 <-str_detect( Descripc,"remodelado")
remod_aux2 <-str_detect( Descripc,"remodelados")
remod_aux3 <-str_detect( Descripc,"renovado")
remod_aux4 <-str_detect( Descripc,"renobado")
remod_aux5 <-str_detect( Descripc,"mejorado")
remod_aux6 <-str_detect( Descripc,"moderno")
remod_aux7 <-str_detect( Descripc,"modernos")
remod_aux8 <-str_detect( Descripc,"modernizado")
remod_aux9 <-str_detect( Descripc,"modernizados")
remod_aux10 <-str_detect( Descripc,"integral")
remod_aux11 <-str_detect( Descripc,"integrales")
remodelado <-ifelse(remod_aux1==TRUE|remod_aux2==TRUE|remod_aux3==TRUE|remod_aux4==TRUE|
                       remod_aux5==TRUE|remod_aux6==TRUE|remod_aux7==TRUE|remod_aux8==TRUE|
                       remod_aux9==TRUE|remod_aux10==TRUE|remod_aux11==TRUE, 1,0 )
remodelado <-data.frame(remodelado)
summary(remodelado)
remodelado[is.na(remodelado)] = 0 #Se imputa cero a los datos NA, porque existen datos donde no había descripción
summary(remodelado)
combine_chapinero <- cbind(combine_chapinero, remodelado)
# Contar
prop_remod <- round(sum(combine_chapinero$remodelado == 1)/nrow(combine_chapinero)*100,1)
paste("El",prop_remod, "%", "de apartamentos está remodelado")

# Limpiar ambiente
rm(remod_aux1,remod_aux2,remod_aux3,remod_aux4,remod_aux5,remod_aux6,
   remod_aux7,remod_aux8,remod_aux9,remod_aux10,remod_aux11)
rm(remodelado)

### Variable número 7: Admon incluida  ####
admon_aux1 <-str_detect( Descripc,"administracion incluida")
admon_aux2 <-str_detect( Descripc,"admon incluida")
admon_aux3 <-str_detect( Descripc,"incluye administracion")
admon_aux4 <-str_detect( Descripc,"incluye admon")
admon_aux5 <-str_detect( Descripc,"administracion cubierta")
admon_aux6 <-str_detect( Descripc,"administracion cubiertos")
admon <-ifelse(admon_aux1==TRUE|admon_aux2==TRUE|admon_aux3==TRUE|admon_aux4==TRUE|
                 admon_aux5==TRUE|admon_aux6==TRUE, 1,0 )
admon <-data.frame(admon)
summary(admon)
admon[is.na(admon)] = 0 #Se imputa cero a los datos NA, porque existen datos donde no había descripción
summary(admon)
combine_chapinero <- cbind(combine_chapinero, admon)
# Contar
prop_admon <- round(sum(combine_chapinero$admon == 1)/nrow(combine_chapinero)*100,1)
paste("El",prop_admon, "%", "de apartamentos incluyen administración")

# Limpiar ambiente
rm(admon_aux1,admon_aux2,admon_aux3,admon_aux4,admon_aux5,admon_aux6)
rm(admon)


### Variable número 8: Metros cuadrados  ####
combine_chapinero <- combine_chapinero %>%
  mutate(MetrosCuadrados = as.numeric(str_extract(description, "\\d+(?=\\s*(?:metros cuadrados|m²|m2|mts2?|mt2|m\\^2|mtrs2?|mtrs))|(?<!\\d)0")))
combine_chapinero$MetrosCuadrados[is.na(combine_chapinero$MetrosCuadrados)] = 0

### Variable número 9: Casa/apartamento  ####
combine_chapinero$Es_apartamento <- ifelse(combine_chapinero$property_type == "Apartamento", 1, 0)
table(combine_chapinero$Es_apartamento)

### Variable número 10: Baños corregido  ####
combine_chapinero$NumeroBanos <- as.numeric(str_extract(combine_chapinero$description, "\\d+(?:\\.\\d+)?(?=\\s*(?:bano|banos))"))
# combinar variable baño
combine_chapinero <- combine_chapinero %>%
  mutate(bathrooms = ifelse(is.na(bathrooms), NumeroBanos, bathrooms))


### Tabla de proporciones final ####
nombres <- c("Parqueadero", "Elevador","Baño privado", "Balcón","Vista",
             "Remodelado","Admon incluida")
valores <- c(prop_parking, prop_ascensor,prop_bano,prop_balcon,prop_vista,
             prop_remod,prop_admon)

Tabla_prop <- data.frame(Descripcion = nombres, Proporcion = valores)
Tabla_prop

# External data -----------------------------------------------------------
available_tags("leisure")
# TODO A : combine_chapinero


# ### Variable número 1: Distancia al parque ####

parques <- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key = "leisure" , value = "park") 
parques_sf <- osmdata_sf(parques)
parques_geometria <- parques_sf$osm_polygons %>% 
  select(osm_id, name)

# Calculamos el centroide de cada parque
centroides <- gCentroid(as(parques_geometria$geometry, "Spatial"), byid = T)

leaflet() %>%
  addTiles() %>%
  addPolygons(data = parques_geometria, col = "green",
              opacity = 0.8, popup = parques_geometria$name) %>%
  addCircles(lng = centroides$x, 
             lat = centroides$y, 
             col = "red", opacity = 1, radius = 1)
# Ahora vamos a calcular la distancia de cada apartamento al centroide de cada parque
db_sf <- st_as_sf(db, coords = c("lon", "lat"))
st_crs(db_sf) <- 4326
centroides_sf <- st_as_sf(centroides, coords = c("x", "y"))
# Esto va a ser demorado!
dist_matrix <- st_distance(x = db_sf, y = centroides_sf)

# Encontramos la distancia mínima a un parque
dist_min <- apply(dist_matrix, 1, min)
db$distancia_parque <- dist_min
db_sf$distancia_parque <- dist_min

p <- ggplot(db, aes(x = distancia_parque)) +
  geom_histogram(bins = 50, fill = "darkblue", alpha = 0.4) +
  labs(x = "Distancia mínima a un parque en metros", y = "Cantidad",
       title = "Distribución de la distancia a los parques") +
  theme_bw()
ggplotly(p)





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



