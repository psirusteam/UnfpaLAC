############################################################################################################################################
#############################################################################################################################################
# # 
# # Proceso: Construcción Base ENSMI Completa (Todos los módulos)
# #
# # 
# # Historia Archivo:
# #   Creation : 29/06/2022
# #
# #
# # Autor: Gabriel Nieto y Andrés Gutiérrez
# #  
# # Institution:  CEPAL             
# # 
# # Modification:   
#############################################################################################################################################
#############################################################################################################################################
##                                    Verificando librerias e instalando las faltantes  
#############################################################################################################################################

## FunciÃ³n que examina si un paquete se encuentra instalado, si lo estÃ¡ sigue derecho, si no estÃ¡ instalado, lo instala

paquetes <- c("readr","readxl","faraway","wesanderson","dplyr","factoextra","corrplot","reshape2","reshape",
              "PerformanceAnalytics","foreign","plotly","FactoMineR","homals", "nnet", "tidyverse", "gapminder")
has <- paquetes %in% rownames(installed.packages())
if(any(!has)) install.packages(paquetes [!has])


#############################################################################################################################################
##                                    Cargando Librerias
#############################################################################################################################################
library(readr)
library(readxl)
library(readxl)
library(faraway)
library(openxlsx)
library(ggplot2)
library(wesanderson)
library(dplyr)
library(factoextra)
library(corrplot)
library(plotly)
library(reshape2)
library(reshape)
library(PerformanceAnalytics)
library(foreign)
library(FactoMineR)
library(homals)
library(nnet)
library(foreign)
library(tidyverse)


######################################################################################################################################################
###                             Limpiando la memoria 
######################################################################################################################################################

rm(list = ls(all=T))
gc()

######################################################################################################################################################
##                                    Definiendo rutas de entradas y salidas
########################################################################################################################

dir()

###########################################################################################################################

######################################################################################################################################################
##                                    Cargando Insumos
######################################################################################################################################################

load("GTM/2014-2015/D6/Data/data_Guatemala.RData")
Ensmi = data_Guatemala 
rm(data_Guatemala)

##################################################################################################################################################

names(Ensmi)
### Construcción de indicadores

## Estado Civil/Conyugal
levels(Ensmi$V501)
levels(Ensmi$V502)
levels(Ensmi$V503)
levels(Ensmi$V504)

# casado
Ensmi$ec.casado = ifelse(Ensmi$V501=="Married",1,0)

# Conviviente
Ensmi$ec.conviviente = ifelse(Ensmi$V501=="Living with partner",1,0)

# Separado
Ensmi$ec.separada = ifelse(Ensmi$V501=="No longer living together/separated",1,0)

# Viudo
Ensmi$ec.viuda = ifelse(Ensmi$V501=="Widowed",1,0)

# Divorciado
Ensmi$ec.divorciada = ifelse(Ensmi$V501=="Divorced",1,0)

# Soltero
Ensmi$ec.soltera = ifelse(Ensmi$V501=="Never in union",1,0)


## Uso metodo anticonceptivos

levels(Ensmi$V313)
Ensmi$usametodo     = ifelse(Ensmi$V313%in%c("Modern method","Traditional method",
                                             "Folkloric method"),1,0)
table(Ensmi$V150)


Ensmi$usamoderno = ifelse(Ensmi$V313=="Modern method",1,0)
table(Ensmi$usamoderno)

## Mujeres sexualmente activas

Ensmi$MSA = ifelse(Ensmi$V536 == "Active in last 4 weeks",
                   "Si","No")
Ensmi$act.sex = Ensmi$V536
### 

levels(Ensmi$V626)

## Necesidad insatisfecha de planificacion familiar

Ensmi$Nec_ins_pf_E = ifelse(Ensmi$V626=="Unmet need for spacing",1,0)
Ensmi$Nec_ins_pf_L = ifelse(Ensmi$V626 == "Unmet need for limiting",1,0)
Ensmi$Nec_ins_pf_T = Ensmi$Nec_ins_pf_E + Ensmi$Nec_ins_pf_L

## Necesidad satisfecha de planificacion familiar

Ensmi$Nec_sat_pf_E = ifelse(Ensmi$V626=="Using for spacing",1,0)
Ensmi$Nec_sat_pf_L = ifelse(Ensmi$V626 == "Using for limiting",1,0)
Ensmi$Nec_sat_pf_T = Ensmi$Nec_sat_pf_E + Ensmi$Nec_sat_pf_L

## Demanda total de planificacion familiar

Ensmi$Dem_tot_pf_E = ifelse(Ensmi$V626%in%c("Unmet need for spacing",
                                            "Using for spacing","Spacing failure"),1,0)
Ensmi$Dem_tot_pf_L = ifelse(Ensmi$V626%in%c("Unmet need for limiting",
                                            "Using for limiting","Limiting failure"),1,0)
Ensmi$Dem_tot_pf_T = Ensmi$Dem_tot_pf_E + Ensmi$Dem_tot_pf_L

## Falla del método

Ensmi$Falla_met = ifelse(Ensmi$V626%in%c("Spacing failure",
                                         "Limiting failure"),1,0)

## Demanda satisfecha (de uso de métodos anticonceptivos)
levels(Ensmi$V626)
table(Ensmi$V626)
table(as.numeric(Ensmi$V626))

Ensmi$DS = ifelse(as.numeric(Ensmi$V626)%in%c(4,5,6,7),1,
                  ifelse(as.numeric(Ensmi$V626)%in%c(2,3),0,NA))

######### Creando Dummys para cada indicador


### D6

Ensmi$D6 = ifelse(Ensmi$usametodo==1,1,0)


### D6m

Ensmi$D6m = ifelse(Ensmi$usamoderno==1,1,0)

### NI

Ensmi$NI = ifelse(Ensmi$Nec_sat_pf_T==1,1,0)

######D7
Ensmi$D7 = ifelse(Ensmi$usamoderno==1&(Ensmi$Falla_met==1|
                                         Ensmi$Nec_sat_pf_T==1),1,0)


##############################################
## Variables para el calculo de indicadores ##
##############################################

## Ajuste ponderador

# considerando que mujeres entre 12-14 años tienen ponderador
Ensmi$FEXT = Ensmi$V005*5547079/sum(Ensmi$V005)

# Considerando solo a las mujeres de 15-49 pero incluyendo a las demas pues poseen el ponderador V005
Ensmi$FEXM = Ensmi$V005*4668986/sum(Ensmi$V005)
levels(Ensmi$V501)
levels(Ensmi$V502)


################

saveRDS(Ensmi,"GTM/2014-2015/D6/Output/Ensmi.rds")

