library(leaflet)
library(rgbif)
library(rgdal)
library(sp)
library(maps)
library(spatialEco)
library(tidyr)
library(sf)
library(mapview)


#######Este programita saca la base de datos del GBIF, limpia los dstos con los criterios gbif_issues()
###### y cruza el mapa de clima mundial con los datos.
#####Al final pone el numero de climas que intersectan los puntos
rm(list=ls())



######################################################################################################################
#####################################################################################################################

####Importar datos desde una base de datos (es decir una base de datos propria o ya manejada)

#datos <- read.scv("mis_datos.csv",header=TRUE)
#head(datos)


####### Meter el lado de clima mundial y transformarlo a un mapa de tipo Spatial polygons dataframe (done)


world_climate = st_read('1976-2000.shp', stringsAsFactors = FALSE,
                 crs = 4326) # has +proj=longlat +datum=WGS84
#plot(world_climate)
str(world_climate)
clima_mundial = st_transform(world_climate, "+init=epsg:4326")
plot(clima_mundial)


#La clissificacion del gridcode en los climas es el siguiente basado en 
#Rubel, F., and M. Kottek, 2010: Observed and projected climate shifts 1901-2100 depicted 
#by world maps of the Köppen-Geiger climate classification. Meteorol. Z., 19, 135-141. DOI: 10.1127/0941-2948/2010/0430.

#MAPS
#11 ... Af
#12 ... Am
#13 ... As
#14 ... Aw
#21 ... BWk
#22 ... BWh
#26 ... BSk
#27 ... BSh
#31 ... Cfa
#32 ... Cfb
#33 ... Cfc
#34 ... Csa
#35 ... Csb
#36 ... Csc
#37 ... Cwa
#38 ... Cwb
#39 ... Cwc
#41 ... Dfa
#42 ... Dfb
#43 ... Dfc
#44 ... Dfd
#45 ... Dsa
#46 ... Dsb
#47 ... Dsc
#48 ... Dsd
#49 ... Dwa
#50 ... Dwb
#51 ... Dwc
#52 ... Dwd
#61 ... EF
#62 ... ET



##########     IMPORTACION DE DATOS DESDE GBIF    #######################################

##Importar los datos de presencia desde gbif
#Solo tienes que ponerle el nombre de la especie
#el key es un identificados unico que usa gbif para cada especie (done)

#####POn el nombre de la especie
Species_name <- c('Bromus sterilis')

##No modificar nada de aqui


Puntos.de.especie <-occ_search(scientificName = Species_name,limit = 200000)
key <- name_backbone(name = Species_name, rank='species')$usageKey
occ_search(taxonKey = key)

base.presencias <- Puntos.de.especie ###renombrar la base de datos para no perde la original o si guieres poner varias



########-----------RUN THIS FUNCTION FIRST--------------------------#########################
###Los datos de GBIF por lo general estan sucios. Necesitamos darles una limpiada
#Al menos quitar los datos que no tienen referencia geografica NA's
gbif_issues()               ###Esto te da la lista de problemas asociados a la base de datos

######DATA cleaner del GBIF
datos <-base.presencias
datos1 <-datos %>%occ_issues(-bri,-ccm,-cdiv,-conti,-cucdmis,-cdout,-cuiv,-cum,-gdativ,-iddatunl,-preneglon,-preswcd,-rdativ,-refuriiv,-rdatm,-txmatfuz,-zerocd)


####Ver los puntos de ocurrencia en mapa abierto (done)
m <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=datos1$data$decimalLongitude, lat=datos1$data$decimalLatitude, popup= datos1$data$name) %>%
  addMouseCoordinates(style = "basic")
print(m)
datos1$data$issues
datos2 <- cbind(datos1$data$decimalLongitude,datos1$data$decimalLatitude)
str(datos2)
colnames(datos2) = c("Longitude","Latitude")
datos2 <-as.data.frame(datos2)
head(datos2)
print(dim(datos2))

datos3 <-drop_na(datos2) ####Quitar todos los datos que tengan NA en las coordenadas
print(dim(datos3))  ###te da las dimensiones de la base de datos, o el numero de registros es el primer numero que escupe

##Generar el mapa sin los NA de la base de datos

m <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=datos3$data$decimalLongitude, lat=datos3$data$decimalLatitude, popup= datos3$data$name) %>%
  addMouseCoordinates(style = "basic")
print(m)



#####Make a SpatialPointsdataframe con de los datos limpios
coordenadas <- datos3[,c(1,2)]
coordenadas.espaciales <- st_as_sf(x = coordenadas, 
                                   coords = c("Longitude", "Latitude"),
                                   crs = "+proj=longlat +datum=WGS84")


########### NOT SURE THIS IS USEFUL FOR ANYTHING    ####################
######Generar el mapa en leflet y ponerle los puntos de presencia de la especie (done)
#leaflet(clima_mundial) %>%
#  addPolygons(data=clima_mundial,
#              fillColor="green",
#              opacity=0.1,
#              weight = 0.1)%>%
#  addMarkers(lng=~datos$data$decimalLongitude, lat=~datos$data$decimalLatitude, popup= datos$data$name)


#####Para hacer el mapa interactivo

puntos.en.climas <- st_intersection(coordenadas.espaciales,clima_mundial)
mapview(coordenadas.espaciales)
mapview(puntos.en.climas)
mapa.climas.con.puntos <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addFeatures(data = puntos.en.climas) %>%
  addMouseCoordinates(style = "basic")
mapview(clima_mundial)+mapview(puntos.en.climas)


#########Generar el valor de el numero de tipos de clima

Numero.de.climas <- function(puntos.en.climas){
  resumen <- as.data.frame(table(puntos.en.climas$GRIDCODE))
  print(length(resumen$Var1))
}
Numero.de.climas(puntos.en.climas)
table(puntos.en.climas$GRIDCODE)


####Climas de Mexico de Koppen
mexico_climate = st_read('climas_mexico_chido.shp', stringsAsFactors = FALSE,
                        crs = 4326) # has +proj=longlat +datum=WGS84

table(mexico_climate$GRIDCODE)

######Comparara los climas de los puntos y los de Mexico

clima_presencias_mundiales <- as.vector(puntos.en.climas$GRIDCODE)
climas_mexico <- as.vector(mexico_climate$GRIDCODE)

table(climas_mexico)
table(clima_presencias_mundiales)


climas_comunes <- intersect(clima_presencias_mundiales,climas_mexico)
climas_comunes <-as.data.frame(climas_comunes)
climas_comunes

############ PARA LOS SUELOS #############


world_soils = st_read('DSMW.shp', stringsAsFactors = FALSE,
                        crs = 4326) # has +proj=longlat +datum=WGS84
###plot(world_soils) tener cuidado porque tarda horas en desplegar el mapa de suelos
world_soils <- st_transform(world_soils, "+init=epsg:4326")
###plot(world_soils)  



puntos.en.suelos <- st_intersection(coordenadas.espaciales,world_soils)
mapview(coordenadas.espaciales)
mapview(puntos.en.suelos)
mapa.suelos.con.puntos <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addFeatures(data = puntos.en.suelos) %>%
  addMouseCoordinates(style = "basic")
#mapview(world_soils)+mapview(puntos.en.suelos)


#########Generar el valor de el numero de tipos de suelo

Numero.de.suelos <- function(puntos.en.suelos){
length(table(puntos.en.suelos$DOMSOI))
}

head(puntos.en.suelos)

Numero.de.suelos(puntos.en.suelos)
Tipos.de.suelos.FAO <- table(puntos.en.suelos$FAOSOIL)
Tipos.de.suelos.DOMSOI <- table(puntos.en.suelos$DOMSOI)



###Suelos de Mexico

mexico_soils = st_read('suelos_mexico.shp', stringsAsFactors = FALSE,
                         crs = 4326) # has +proj=longlat +datum=WGS84
str(mexico_soils)
######Comparara los climas de los puntos y los de Mexic

suelo_presencias_mundiales <- as.vector(puntos.en.suelos$DOMSOI)
suelos_mexico <- as.vector(mexico_soils$DOMSOI)

table(suelos_mexico)
table(suelo_presencias_mundiales)


suelos_comunes <- intersect(suelo_presencias_mundiales,suelos_mexico)
suelos_comunes <- as.data.frame(suelos_comunes)
suelos_comunes


####Salidas en CSV de clima y Suelo. Cambiar el nombre de la especie en el archivo csv

write.csv(suelos_comunes, paste0("Species_suelos", Species_name,".csv"), row.names=F)

write.csv(climas_comunes,paste0("Species_climate", Species_name,".csv"), row.names=F)

