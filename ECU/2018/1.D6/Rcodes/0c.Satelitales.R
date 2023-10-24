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
tasa_desocupacion <-
  readRDS("ECU/2018/1.D6/Data/tasa_desocupacion.rds")
sum(complete.cases(tasa_desocupacion))

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
ECU <- read_sf("ECU/2018/ShapeECU/ecuador_Cantones.shp")

#####


####################
### Luminosidad ###
###################

luces = ee$ImageCollection("NOAA/DMSP-OLS/NIGHTTIME_LIGHTS") %>%
  ee$ImageCollection$filterDate("2013-01-01", "2014-01-01") %>%
  ee$ImageCollection$map(function(x)
    x$select("stable_lights")) %>%
  ee$ImageCollection$toBands()
ee_print(luces)

ECU_luces <- map(unique(ECU$DPA_CANTON), function (x) {
  cat(x, "\n")
  ee_extract(
    x = luces,
    y = ECU["DPA_CANTON"] %>% filter(DPA_CANTON == x),
    ee$Reducer$sum(),
    sf = FALSE
  )
})

ECU_luces <- bind_rows(ECU_luces)

saveRDS(ECU_luces, "ECU/2018/1.D6/Data/ECU_luces.rds")
#ECU_luces<-readRDS("ECU/2018/1.D6/Data/Ecu_luces.rds")

#################
### Urbanismo ###
#################

tiposuelo = ee$ImageCollection("COPERNICUS/Landcover/100m/Proba-V-C3/Global") %>%
  ee$ImageCollection$filterDate("2016-01-01", "2016-12-31") %>%
  ee$ImageCollection$map(function(x)
    x$select("urban-coverfraction", "crops-coverfraction")) %>%
  ee$ImageCollection$toBands()
ee_print(tiposuelo)

ECU_urbano_cultivo <- map(unique(ECU$DPA_CANTON), function (x) {
  cat(x, "\n")
  ee_extract(
    x = tiposuelo,
    y = ECU["DPA_CANTON"] %>% filter(DPA_CANTON == x),
    ee$Reducer$sum(),
    sf = FALSE
    
  )
})

ECU_urbano_cultivo <- bind_rows(ECU_urbano_cultivo)

saveRDS(ECU_urbano_cultivo,
        "ECU/2018/1.D6/Data/ECU_urbano_cultivo.rds")
#ECU_urbano_cultivo<-readRDS("ECU/2018/1.D6/Data/ECU_urbano_cultivo.rds")

####################
###############
### Guardar ###
###############

colnames(ECU_urbano_cultivo) <- c("mpio",
                                  "crops.coverfraction",
                                  "urban.coverfraction")

colnames(ECU_luces) <- c("mpio",
                         "stable_lights")


### Validaciones

y <- ECU_luces %>%
  full_join(tasa_desocupacion)

z <- ECU_urbano_cultivo %>%
  full_join(tasa_desocupacion)


##### Unión promedios con la tasa de desocupación

tasa_desocupacion %<>%
  full_join(ECU_luces) %>%
  full_join(ECU_urbano_cultivo)

cor(tasa_desocupacion[,-1])

sum(complete.cases(tasa_desocupacion))

saveRDS(tasa_desocupacion,"ECU/2018/1.D6/Data/tasa_desocupacion.rds")
saveRDS(tasa_desocupacion,"ECU/2018/2.D6m/Data/tasa_desocupacion.rds")        
saveRDS(tasa_desocupacion,"ECU/2018/4.NI/Data/tasa_desocupacion.rds")