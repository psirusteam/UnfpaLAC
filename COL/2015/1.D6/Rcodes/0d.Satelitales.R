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
#memory.limit(500000)

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
  readRDS("COL/2015/1.D6/Data/tasa_desocupacion.rds")
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

## revisando COLivia
COL <- read_sf("COL/2015/Shape.COL/dv_Municipio.shp") 
COL %<>% mutate(mpio = COD_DANE, nombre = NOM_MUNICI,
                      mpio = case_when(mpio == "18209" ~ "18029",
                                      mpio == "25480" ~ "25483",
                                                 TRUE ~ mpio))
table(COL$mpio)

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

COL_luces <- map(unique(COL$mpio), function (x) {
  cat(x, "\n")
  ee_extract(
    x = luces,
    y = COL["mpio"] %>% filter(mpio == x),
    ee$Reducer$sum(),
    sf = FALSE
  ) 

})

COL_luces <- bind_rows(COL_luces)
sum(complete.cases(COL_luces))

saveRDS(COL_luces, "COL/2015/1.D6/Data/COL_luces.rds")
#COL_luces<-readRDS("COL/2015/1.D6/Data/COL_luces.rds")

#################
### Urbanismo ###
#################

tiposuelo = ee$ImageCollection("COPERNICUS/Landcover/100m/Proba-V-C3/Global") %>%
  ee$ImageCollection$filterDate("2016-01-01", "2016-12-31") %>%
  ee$ImageCollection$map(function(x)
    x$select("urban-coverfraction", "crops-coverfraction")) %>%
  ee$ImageCollection$toBands()
ee_print(tiposuelo)

COL_urbano_cultivo <- map(unique(COL$mpio), function (x) {
  cat(x, "\n")
  ee_extract(
    x = tiposuelo,
    y = COL["mpio"] %>% filter(mpio == x),
    ee$Reducer$sum(),
    sf = FALSE
    
  )
})

COL_urbano_cultivo <- bind_rows(COL_urbano_cultivo)
sum(complete.cases(COL_urbano_cultivo))

saveRDS(COL_urbano_cultivo,
        "COL/2015/1.D6/Data/COL_urbano_cultivo.rds")
#COL_urbano_cultivo<-readRDS("COL/2015/1.D6/Data/COL_urbano_cultivo.rds")

####################
###############
### Guardar ###
###############

colnames(COL_urbano_cultivo) <- c("mpio",
                                  "crops.coverfraction",
                                  "urban.coverfraction")

colnames(COL_luces) <- c("mpio",
                         "stable_lights")


### Validaciones

x <- COL_luces %>%
  full_join(tasa_desocupacion)

y <- COL_urbano_cultivo %>%
  full_join(tasa_desocupacion)

#### verificando si existe algún código que no haga match
z <- tasa_desocupacion %>%
  anti_join(COL_luces)

##### Unión promedios con la tasa de desocupación

tasa_desocupacion %<>%
  full_join(COL_luces) %>%
  full_join(COL_urbano_cultivo)

cor(tasa_desocupacion[,-1])

sum(complete.cases(tasa_desocupacion))
#openxlsx::write.xlsx(tasa_desocupacion, file = "COL/2015/1.D6/Output/Tablas/tabladesocupacion.xlsx")

saveRDS(tasa_desocupacion,"COL/2015/1.D6/Data/tasa_desocupacion.rds")
saveRDS(tasa_desocupacion,"COL/2015/2.D6m/Data/tasa_desocupacion.rds")        
saveRDS(tasa_desocupacion,"COL/2015/3.NI/Data/tasa_desocupacion.rds")
