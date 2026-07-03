# ==============================================================================
# Cruce de presencias de GBIF con clima y suelo (mundial y Mexico)
# ------------------------------------------------------------------------------
# Version limpia: paquetes retirados/obsoletos eliminados y mapas sin leaflet.
#
# Paquetes que se quitaron de la version original y por que:
#   - rgdal  : RETIRADO de CRAN (oct 2023). Toda su funcionalidad ya vive en 'sf'.
#              El script solo usaba st_* (de sf), asi que no hacia falta.
#   - sp     : superado por 'sf'. No se usaba directamente.
#   - maps   : no se usaba en ninguna parte.
#   - spatialEco : no se usaba en ninguna parte.
#   - leaflet + leafem (addTiles/addMarkers/addFeatures/addMouseCoordinates):
#              reemplazados por 'mapview' para los mapas interactivos.
#
# Nota: mapview() se apoya en leaflet internamente. Si necesitas CERO
#       dependencia de leaflet, se pueden usar mapas estaticos con
#       ggplot2 + geom_sf (te lo puedo dejar armado si lo prefieres).
# ==============================================================================

library(rgbif)
library(sf)
library(tidyr)
library(mapview)

####### Este programita saca la base de datos del GBIF, limpia los datos con los
####### criterios de gbif_issues() y cruza el mapa de clima mundial con los datos.
####### Al final pone el numero de climas que intersectan los puntos.

rm(list = ls())

# sf usa S2 para coordenadas geograficas (lat/long) desde la version 1.0. Algunos
# shapefiles antiguos traen geometrias invalidas que rompen st_intersection().
# Por eso mas abajo se usa st_make_valid(). Si aun asi fallara alguna
# interseccion, se puede desactivar S2 con:  sf::sf_use_s2(FALSE)

# ------------------------------------------------------------------------------
# Importar datos desde una base de datos propia (opcional)
# ------------------------------------------------------------------------------
# datos <- read.csv("mis_datos.csv", header = TRUE)
# head(datos)

# ------------------------------------------------------------------------------
# Clima mundial (Koppen-Geiger) -> objeto sf en WGS84 (EPSG:4326)
# ------------------------------------------------------------------------------
getwd()

clima_mundial <- st_read('1976-2000/1976-2000.shp', stringsAsFactors = FALSE)
if (is.na(st_crs(clima_mundial))) st_crs(clima_mundial) <- 4326   # si falta .prj
clima_mundial <- st_transform(clima_mundial, 4326)                # WGS84
clima_mundial <- st_make_valid(clima_mundial)
# plot(st_geometry(clima_mundial))

# La clasificacion del GRIDCODE en los climas es la siguiente, basada en
# Rubel, F., and M. Kottek, 2010: Observed and projected climate shifts 1901-2100
# depicted by world maps of the Koppen-Geiger climate classification.
# Meteorol. Z., 19, 135-141. DOI: 10.1127/0941-2948/2010/0430.
#
# 11 Af   12 Am   13 As   14 Aw
# 21 BWk  22 BWh  26 BSk  27 BSh
# 31 Cfa  32 Cfb  33 Cfc  34 Csa  35 Csb  36 Csc  37 Cwa  38 Cwb  39 Cwc
# 41 Dfa  42 Dfb  43 Dfc  44 Dfd  45 Dsa  46 Dsb  47 Dsc  48 Dsd
# 49 Dwa  50 Dwb  51 Dwc  52 Dwd
# 61 EF   62 ET

# ------------------------------------------------------------------------------
# Importacion de datos de presencia desde GBIF
# ------------------------------------------------------------------------------
# Solo tienes que poner el nombre de la especie.
# 'key' es el identificador unico que usa GBIF para cada especie.
Species_name <- 'Bromus sterilis'   # <-- Pon aqui el nombre de la especie

# Para descargas muy grandes conviene occ_download() en lugar de occ_search(),
# pero se conserva occ_search() para mantener el flujo original.
Puntos.de.especie <- occ_search(scientificName = Species_name, limit = 200)
key <- name_backbone(name = Species_name, rank = 'species')$usageKey
# occ_search(taxonKey = key)   # llamada exploratoria; el resultado no se usaba

base.presencias <- Puntos.de.especie   # renombrar para no perder la original

# ------------------------------------------------------------------------------
# Limpieza de los datos de GBIF
# ------------------------------------------------------------------------------
gbif_issues()   # lista de problemas asociados a la base de datos

datos  <- base.presencias
datos1 <- datos %>%
  occ_issues(-bri, -ccm, -cdiv, -conti, -cucdmis, -cdout, -cuiv, -cum,
             -gdativ, -iddatunl, -preneglon, -preswcd, -rdativ, -refuriiv,
             -rdatm, -txmatfuz, -zerocd)

# ------------------------------------------------------------------------------
# Coordenadas limpias -> objeto sf
# ------------------------------------------------------------------------------
datos2 <- data.frame(
  Longitude = datos1$data$decimalLongitude,
  Latitude  = datos1$data$decimalLatitude
)
datos3 <- drop_na(datos2)          # quita todos los registros sin coordenadas
print(dim(datos3))                 # numero de registros (primer valor) y columnas

coordenadas.espaciales <- st_as_sf(
  datos3,
  coords = c("Longitude", "Latitude"),
  crs = 4326
)

# Mapa interactivo de las presencias (antes con leaflet, ahora con mapview)
mapview(coordenadas.espaciales, col.regions = "red", layer.name = "Presencias")

# ------------------------------------------------------------------------------
# Interseccion de los puntos con el clima mundial
# ------------------------------------------------------------------------------
puntos.en.climas <- st_intersection(coordenadas.espaciales, clima_mundial)

mapview(clima_mundial, alpha.regions = 0.2, layer.name = "Clima mundial") +
  mapview(puntos.en.climas, zcol = "GRIDCODE", layer.name = "Puntos por clima")

# Numero de tipos de clima que tocan los puntos
Numero.de.climas <- function(puntos.en.climas) {
  resumen <- as.data.frame(table(puntos.en.climas$GRIDCODE))
  print(length(resumen$Var1))
}
Numero.de.climas(puntos.en.climas)
table(puntos.en.climas$GRIDCODE)

# ------------------------------------------------------------------------------
# Climas de Mexico (Koppen) y comparacion con las presencias
# ------------------------------------------------------------------------------
mexico_climate <- st_read('climas_mexico_chido/climas_mexico_chido.shp', stringsAsFactors = FALSE)
if (is.na(st_crs(mexico_climate))) st_crs(mexico_climate) <- 4326
table(mexico_climate$GRIDCODE)

clima_presencias_mundiales <- as.vector(puntos.en.climas$GRIDCODE)
climas_mexico              <- as.vector(mexico_climate$GRIDCODE)

table(climas_mexico)
table(clima_presencias_mundiales)

climas_comunes <- intersect(clima_presencias_mundiales, climas_mexico)
climas_comunes <- as.data.frame(climas_comunes)
climas_comunes

# ------------------------------------------------------------------------------
# Suelos mundiales (DSMW / FAO)
# ------------------------------------------------------------------------------
world_soils <- st_read('DSMW/DSMW.shp', stringsAsFactors = FALSE)
if (is.na(st_crs(world_soils))) st_crs(world_soils) <- 4326
world_soils <- st_transform(world_soils, 4326)
world_soils <- st_make_valid(world_soils)
# plot(st_geometry(world_soils))  # cuidado: el mapa de suelos tarda muchisimo

puntos.en.suelos <- st_intersection(coordenadas.espaciales, world_soils)

mapview(puntos.en.suelos, zcol = "DOMSOI", layer.name = "Puntos por suelo")
# mapview(world_soils) + mapview(puntos.en.suelos)  # pesado; usar con cuidado

# Numero de tipos de suelo que tocan los puntos
Numero.de.suelos <- function(puntos.en.suelos) {
  length(table(puntos.en.suelos$DOMSOI))
}
head(puntos.en.suelos)
Numero.de.suelos(puntos.en.suelos)
Tipos.de.suelos.FAO    <- table(puntos.en.suelos$FAOSOIL)
Tipos.de.suelos.DOMSOI <- table(puntos.en.suelos$DOMSOI)

# ------------------------------------------------------------------------------
# Suelos de Mexico y comparacion con las presencias
# ------------------------------------------------------------------------------
mexico_soils <- st_read('suelos_mexico/suelos_mexico.shp', stringsAsFactors = FALSE)
if (is.na(st_crs(mexico_soils))) st_crs(mexico_soils) <- 4326
str(mexico_soils)

suelo_presencias_mundiales <- as.vector(puntos.en.suelos$DOMSOI)
suelos_mexico              <- as.vector(mexico_soils$DOMSOI)

table(suelos_mexico)
table(suelo_presencias_mundiales)

suelos_comunes <- intersect(suelo_presencias_mundiales, suelos_mexico)
suelos_comunes <- as.data.frame(suelos_comunes)
suelos_comunes

# ------------------------------------------------------------------------------
# Salidas en CSV de clima y suelo (el nombre incluye la especie)
# ------------------------------------------------------------------------------
sp_file <- gsub(" ", "_", Species_name)   # evita espacios en el nombre del archivo
write.csv(suelos_comunes, paste0("Species_suelos_",  sp_file, ".csv"), row.names = FALSE)
write.csv(climas_comunes, paste0("Species_climate_", sp_file, ".csv"), row.names = FALSE)