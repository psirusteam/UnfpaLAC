############################################################################################################################################
#############################################################################################################################################
# # 
# # Proceso: Estimación indicadores de planificación
# #              familiar utilizando ENADID 2018
# # 
# # Historia Archivo:
# #   Creation : 24/06/2022
# #
# #
# # Autor: Stalyn Guerrero y Andrés Gutiérrez
# #  
# # Institution:  CEPAL             
# # 
# # Modification:   
#############################################################################################################################################
#############################################################################################################################################
##                                    Verificando librerias e instalando las faltantes  
#############################################################################################################################################

## Función que examina si un paquete se encuentra instalado, si lo está sigue derecho, si no está instalado, lo instala

paquetes <- c("readr","readxl","survey","openxlsx","dplyr","foreign","srvyr")
has <- paquetes %in% rownames(installed.packages())
if(any(!has)) install.packages(paquetes [!has])


#############################################################################################################################################
##                                    Cargando Librerias
#############################################################################################################################################
library(readr)
library(readxl)
library(openxlsx)
library(foreign)
library(dplyr)
library(survey)
library(srvyr)

######################################################################################################################################################
###                             Limpiando la memoria 
######################################################################################################################################################

rm(list = ls(all=T))
gc()

###########################################################################################################################

######################################################################################################################################################
##                                    Cargando Insumos
######################################################################################################################################################

ENADID<-readRDS("MEX/2018/1.D6/Data/ENADID_MUJER.rds")

######################################################################################################################################################

#### Explorando

names(ENADID)

table(ENADID$area, useNA = "a")
table(ENADID$edad, useNA = "a")
table(ENADID$etnia, useNA = "a")
table(ENADID$discapacidad, useNA = "a")
table(ENADID$anoest, useNA = "a")
table(ENADID$unida, useNA = "a")
n_distinct(ENADID$dam2)
table(ENADID$dam2, useNA = "a")

#######################

# Creación objetivo diseño de muestreo complejo para unidas y total

#### Unidas

ENADID_Unidas <- ENADID %>% filter(unida == "Unida")

options(survey.lonely.psu = "adjust") 

diseno = ENADID_Unidas %>% as_survey_design(ids = upm,
                                              strat = estrato, 
                                              weights = fexp, nest = TRUE)

#### Total

#####################

table(ENADID_Unidas$usametodo, useNA = "a")
table(ENADID_Unidas$usamoderno, useNA = "a")
table(ENADID_Unidas$usamoderno,ENADID_Unidas$usametodo, useNA = "a")
#######################################
## Indicador D6: Uso Anticonceptivos ##
#######################################

####################
## Nivel Nacional ##
####################


D6_nal = diseno %>% #group_by(dominio) %>%
  summarise(n = unweighted(n()),
            D6 = survey_mean(usametodo,
                             vartype = c("cv")))

D6_nal
D6_nal <- as.data.frame(D6_nal)
D6_nal$D6 <- D6_nal$D6 * 100
D6_nal$D6 <- round(D6_nal$D6, digits = 1)
D6_nal

# En 2018, el porcentaje de mujeres usuarias de métodos
# anticonceptivos es de 53.4% de las mujeres en edad fértil.
weighted.mean(ENADID$usametodo,ENADID$fexp)#ok

# Para el caso de mujeres casadas o unidas de 15 a 49, 
# el porcentaje de usuarias pasó de 72.3% a 73.1 por ciento.
# (http://estadistica.inmujeres.gob.mx/formas/tarjetas/Uso_anticonceptivos.pdf)
D6_nal

####################################### Desagregaciones #######################

### Obtener estimaciones directas Para las siguientes desagregaciones:

## Dept
## Mcipio
## area
## anoest
## etnia
## edad
## unida
## discapacidad


####### Por Departamento

## Usa métodos de Planificación D6

# design.base.total
# design.base.unidas
aux_estimate <- function(dis,indicador, group = NULL){
  
  tab <- dis %>% group_by_at(group) %>%
    summarise(n = unweighted(n()),
              Prom = survey_mean(!!sym(indicador),
                               vartype = c("cv"))) %>% 
    mutate(Prom = round(Prom*100,digits = 1),
           Prom_cv = round(Prom_cv*100,digits = 1)) %>% 
    arrange_at(group)

names(tab) <-  str_replace(names(tab),"Prom",indicador)
tab
}

estimacion_dir <-
  purrr::map(c(
    "dam",
    "area",
    "edad",
    "unida",
    "anoest",
    "etnia",
    "discapacidad"
  ),
  function(x)
    aux_estimate(diseno, "usametodo", x))

estimacion_dir2 <-
  purrr::map(list(c("area", "anoest"),
                  c("dam", "edad")),
             function(x){
               
               aux_estimate(diseno, "usametodo", x)})


c(estimacion_dir,estimacion_dir2) %>% 
  openxlsx::write.xlsx(file = "MEX/2018/1.D6/Output/Tablas/Estimacion_dir.xlsx")

