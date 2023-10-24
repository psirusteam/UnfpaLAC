#########################################################
# Proyecto MRP - Leave no one behind                    #
# Mapas de pobreza CEPAL                                #
# Lectura y preparación de las bases de datos           #
# Autor: Stalyn Guerrero & Andrés Gutiérrez             #
#########################################################

### Cleaning R environment ###

rm(list = ls())

#################
### Libraries ###
#################
memory.limit(500000)

library(tidyverse)
library(sampling)
library(reticulate) # Conexión con Python
library(rgee) # Conexión con Google Earth Engine
library(sf) # Paquete para manejar datos geográficos
library(concaveman)
library(geojsonio)
library(magrittr)
library(sp)
library(haven)

####################################################
### Loading datasets: EH and Population census ###
####################################################
tasa_desocupacion <-readRDS("GTM/2014-2015/1.D6/Data/tasa_desocupacion.rds")


#######################################
### configuración inicial de Python ###
#######################################

#rgee_environment_dir = "C://Users//sguerrero//Anaconda3//envs//rgee_py//python.exe"
#rgee_environment_dir =  "C:/Users/agutierrez1/Anaconda3/envs/rgee_py/python.exe"
rgee_environment_dir =  "C:/Users/gnieto/Anaconda3/envs/rgee_py/python.exe"
# Configurar python (Algunas veces no es detectado y se debe reiniciar R)
reticulate::use_python(rgee_environment_dir, required = T)
rgee::ee_install_set_pyenv(py_path = rgee_environment_dir, py_env = "rgee_py")
Sys.setenv(RETICULATE_PYTHON = rgee_environment_dir)
Sys.setenv(EARTHENGINE_PYTHON = rgee_environment_dir)
rgee::ee_Initialize(drive = T)
###################################################
### Arreglar la shape                           ###
### Aplicar el CONVEX HULL a los multipolígonos ###
###################################################

## revisando ECUivia
GTM <- read_sf("GTM/2014-2015/ShapesGTM/departamentos_gtm.shp")
GTM %<>% mutate(depto = str_pad(departamen, pad = "0", width = 2), area = NULL)
##### 


####################
### Luminosidad ###
###################

luces = ee$ImageCollection("NOAA/DMSP-OLS/NIGHTTIME_LIGHTS") %>%
  ee$ImageCollection$filterDate("2013-01-01", "2014-01-01") %>%
  ee$ImageCollection$map(function(x) x$select("stable_lights")) %>%
  ee$ImageCollection$toBands()
ee_print(luces)

GTM_luces <- map(unique(GTM$depto), function (x ){
  cat(x,"\n")
  ee_extract(
    x = luces,
    y = GTM["depto"] %>% filter(depto == x),
    ee$Reducer$sum(),
    sf = FALSE
  )})

GTM_luces<-bind_rows(GTM_luces)

saveRDS(GTM_luces, "GTM/2014-2015/1.D6/Data/GTM_luces.rds")
#GTM_luces<-readRDS("GTM/2014-2015/1.D6/Data/GTM_luces.rds")

#################
### Urbanismo ###
#################

tiposuelo = ee$ImageCollection("COPERNICUS/Landcover/100m/Proba-V-C3/Global") %>%
  ee$ImageCollection$filterDate("2016-01-01", "2016-12-31") %>%
  ee$ImageCollection$map(function(x) x$select("urban-coverfraction", "crops-coverfraction")) %>% 
  ee$ImageCollection$toBands()
ee_print(tiposuelo)

GTM_urbano_cultivo <- map(unique(GTM$depto), function (x ){
  cat(x,"\n")
  ee_extract(
    x = tiposuelo,
    y = GTM["depto"] %>% filter(depto == x),
    ee$Reducer$sum(),
    sf = FALSE
    
  )})

GTM_urbano_cultivo<-bind_rows(GTM_urbano_cultivo)

saveRDS(GTM_urbano_cultivo, "GTM/2014-2015/1.D6/Data/GTM_urbano_cultivo.rds")
#GTM_urbano_cultivo<-readRDS("GTM/2014-2015/1.D6/Data/GTM_urbano_cultivo.rds")

####################
###############
### Guardar ###
###############

colnames(GTM_urbano_cultivo) <- c("depto",
                                  "crops.coverfraction",
                                  "urban.coverfraction")

colnames(GTM_luces) <- c("depto",
                         "stable_lights")


### Validaciones 

y<-GTM_luces %>% 
  full_join(tasa_desocupacion)

z<-GTM_urbano_cultivo %>% 
  full_join(tasa_desocupacion)


##### Unión promedios con la tasa de desocupación

tasa_desocupacion %<>% 
  full_join(GTM_luces) %>% 
  full_join(GTM_urbano_cultivo)

cor(tasa_desocupacion[, -1 ])

saveRDS(tasa_desocupacion, "GTM/2014-2015/1.D6/Data/tasa_desocupacion.rds")
saveRDS(tasa_desocupacion, "GTM/2014-2015/2.D6m/Data/tasa_desocupacion.rds")
saveRDS(tasa_desocupacion, "GTM/2014-2015/3.NI/Data/tasa_desocupacion.rds")

