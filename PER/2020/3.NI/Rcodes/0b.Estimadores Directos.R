############################################################################################################################################
#############################################################################################################################################
# # 
# # Proceso: Estimación indicadores de planificación
# #              familiar utilizando ENDS 2019
# # 
# # Historia Archivo:
# #   Creation : 24/06/2022
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

EndesMEF = readRDS("PER/2020/3.NI/Data/ENDESMEF_Est.rds")

######################################################################################################################################################

#### Explorando

table(EndesMEF$area, useNA = "a")
table(EndesMEF$edad, useNA = "a")
table(EndesMEF$etnia, useNA = "a")
table(EndesMEF$discapacidad, useNA = "a")
#table(EndesMEF$escolaridad, useNA = "a")
table(EndesMEF$unida, useNA = "a")

# Creación objetivo diseño de muestreo complejo para unidas y total

#### Unidas

EndesMEF_Unidas <- EndesMEF %>% 
  filter(unida == "Unida", edad != "12-14" )


options(survey.lonely.psu = "adjust") 

design.base.unidas = EndesMEF_Unidas %>% as_survey_design(ids = upm,
                                              strat = estrato, 
                                              weights = fexp, nest = TRUE)
### factor de expansión

EndesMEF_edad <- EndesMEF %>% 
  filter(edad != "12-14")

design.base.total = EndesMEF_edad %>% as_survey_design(ids = upm,
                                                       strat = estrato, 
                                                       weights = fexp_tot, nest = TRUE)

#### Total

EndesMEF_edad <- EndesMEF %>% 
  filter(edad != "12-14")

design.base = EndesMEF_edad %>% as_survey_design(ids = upm,
                                                 strat = estrato, 
                                                 weights = fexp, nest = TRUE)

# Flags

nflag = 100
cvflag = 30
nefflag = 68
dfflag = 10
yflag = 100

#####################

table(EndesMEF_Unidas$usametodo, useNA = "a")
table(EndesMEF_Unidas$usamoderno, useNA = "a")
table(EndesMEF_Unidas$nec_insat, useNA = "a")

#############################################
## Indicador NI: Necesidades Insatisfechas ##
#############################################

####################
## Nivel Nacional ##
####################

####### Informe nacional NECESIDAD DE SERVICIOS DE PLANIFICACIÓN FAMILIAR 
#######                   DE LAS MUJERES ACTUALMENTE UNIDAS

####### Cuadro anexo Excel Cap 006 6.6

NI_nal = design.base.unidas %>% #group_by(dominio) %>%
  summarise(n = unweighted(n()),
            NI = survey_mean(nec_insat,
                              vartype = c("cv")))

NI_nal
NI_nal <- as.data.frame(NI_nal)
NI_nal$NI <- NI_nal$NI * 100
NI_nal$NI <- round(NI_nal$NI, digits = 1)
NI_nal


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


####### Informe nacional NECESIDAD DE SERVICIOS DE PLANIFICACIÓN FAMILIAR 
#######                   DE LAS MUJERES ACTUALMENTE UNIDAS

### 
####### Por departamento Cuadro anexo Excel Cap 006 6.7 

## Necesidades Insatisfechas NI

# design.base.total
# design.base.unidas

NI_Depto = design.base.unidas %>% group_by(Depto) %>%
  summarise(n = unweighted(n()),
            NI = survey_mean(nec_insat,
                              vartype = c("cv")))

NI_Depto
NI_Depto$NI <- NI_Depto$NI * 100
NI_Depto$NI <- round(NI_Depto$NI, digits = 1)
NI_Depto$NI_cv <- NI_Depto$NI_cv * 100
NI_Depto$NI_cv <- round(NI_Depto$NI_cv, digits = 1)
#NI_Depto    <-NI_Depto[order(-NI_Depto$NI),]
NI_Depto

openxlsx::write.xlsx(NI_Depto, file = "PER/2020/3.NI/Output/Tablas/NI_Depto.xlsx")


####### Informe nacional NECESIDAD DE SERVICIOS DE PLANIFICACIÓN FAMILIAR 
#######                   DE LAS MUJERES ACTUALMENTE UNIDAS

### 
####### Por Area Cuadro anexo Excel Cap 006 6.7

# design.base.total
# design.base.unidas

NI_area = design.base.unidas %>% group_by(area) %>%
  summarise(n = unweighted(n()),
            NI = survey_mean(nec_insat,
                              vartype = c("cv")))

NI_area <- as.data.frame(NI_area)
NI_area
NI_area$NI <- NI_area$NI * 100
NI_area$NI <- round(NI_area$NI, digits = 1)
NI_area$area <- factor(x= NI_area$area,
                        levels=c("Urbano","Rural"))
NI_area

openxlsx::write.xlsx(NI_area, file = "PER/2020/3.NI/Output/Tablas/NI_area.xlsx")

####### Informe nacional NECESIDAD DE SERVICIOS DE PLANIFICACIÓN FAMILIAR 
#######                   DE LAS MUJERES ACTUALMENTE UNIDAS

### 
####### Por Edad Cuadro anexo Excel Cap 006 6.6

# design.base.total
# design.base.unidas

NI_edad = design.base.unidas %>% group_by(edad) %>%
  summarise(n = unweighted(n()),
            NI = survey_mean(nec_insat,
                              vartype = c("cv")))

NI_edad <- as.data.frame(NI_edad)
NI_edad$NI <- NI_edad$NI * 100
NI_edad$NI <- round(NI_edad$NI, digits = 1)
NI_edad<-NI_edad[order(-NI_edad$NI),]
NI_edad$NI_cv <- NI_edad$NI_cv * 100
NI_edad$NI_cv <- round(NI_edad$NI_cv, digits = 1)
NI_edad

openxlsx::write.xlsx(NI_edad, file = "PER/2020/3.NI/Output/Tablas/NI_edad.xlsx")

####### Informe nacional NECESIDAD DE SERVICIOS DE PLANIFICACIÓN FAMILIAR 
#######                   DE LAS MUJERES ACTUALMENTE UNIDAS

### 
####### Por Años de estudio Cuadro anexo Excel Cap 006 6.6 

# design.base.total
# design.base.unidas

NI_anoest = design.base.unidas %>% group_by(anoest) %>%
  summarise(n = unweighted(n()),
            NI = survey_mean(nec_insat,
                              vartype = c("cv")))

NI_anoest <- as.data.frame(NI_anoest)
NI_anoest$NI <- NI_anoest$NI * 100
NI_anoest$NI <- round(NI_anoest$NI, digits = 1)
table(NI_anoest$anoest)
NI_anoest$anoest <- factor(x= NI_anoest$anoest,
                            levels=c("Sin educación","Primaria",
                                     "Secundaria","Superior"))
NI_anoest

openxlsx::write.xlsx(NI_anoest, file = "PER/2020/3.NI/Output/Tablas/NI_anoest.xlsx")

####### Etnia

# design.base.total
# design.base.unidas

NI_etnia = design.base.unidas %>% group_by(etnia) %>%
  summarise(n = unweighted(n()),
            NI = survey_mean(nec_insat,
                              vartype = c("cv")))

NI_etnia <- as.data.frame(NI_etnia)
NI_etnia$NI <- NI_etnia$NI * 100
NI_etnia$NI <- round(NI_etnia$NI, digits = 1)
NI_etnia$NI_cv <- NI_etnia$NI_cv * 100
NI_etnia$NI_cv <- round(NI_etnia$NI_cv, digits = 1)
NI_etnia<-NI_etnia[order(-NI_etnia$NI),]
NI_etnia

openxlsx::write.xlsx(NI_etnia, file = "PER/2020/3.NI/Output/Tablas/NI_etnia.xlsx")

####### Discapacidad

NI_discap = design.base.unidas %>% group_by(discapacidad) %>%
  summarise(n = unweighted(n()),
            NI = survey_mean(nec_insat,
                              vartype = c("cv")))

NI_discap <- as.data.frame(NI_discap)
NI_discap$NI <- NI_discap$NI * 100
NI_discap$NI <- round(NI_discap$NI, digits = 1)
NI_discap

openxlsx::write.xlsx(NI_discap, file = "PER/2020/3.NI/Output/Tablas/NI_Discp.xlsx")

##### Resultados desagregaciones covariables

NI_nal
NI_area
NI_anoest
NI_etnia
NI_edad
NI_discap
NI_Depto

########################### Por Area y Escolaridad ###############################

## Necesidades Insatisfechas NI

NI_area_anoest = design.base.unidas %>% group_by(area, anoest) %>%
  summarise(n = unweighted(n()),
            NI = survey_mean(nec_insat,
                              vartype = c("cv")))

NI_area_anoest
NI_area_anoest$NI <- NI_area_anoest$NI * 100
NI_area_anoest$NI <- round(NI_area_anoest$NI, digits = 1)
NI_area_anoest$NI_cv <- NI_area_anoest$NI_cv * 100
NI_area_anoest$NI_cv <- round(NI_area_anoest$NI_cv, digits = 1)
NI_area_anoest<-NI_area_anoest[order(-NI_area_anoest$NI),]
NI_area_anoest

openxlsx::write.xlsx(NI_area_anoest, file = "PER/2020/3.NI/Output/Tablas/NI_area_escolaridad.xlsx")


########################### Por Depto y Edad ###############################

## Necesidades Insatisfechas NI

NI_depto_edad = design.base.unidas %>% group_by(Depto, edad) %>%
  summarise(n = unweighted(n()),
            NI = survey_mean(nec_insat,
                              vartype = c("cv")))

NI_depto_edad
NI_depto_edad$NI <- NI_depto_edad$NI * 100
NI_depto_edad$NI <- round(NI_depto_edad$NI, digits = 1)
NI_depto_edad$NI_cv <- NI_depto_edad$NI_cv * 100
NI_depto_edad$NI_cv <- round(NI_depto_edad$NI_cv, digits = 1)
NI_depto_edad    <-NI_depto_edad[order(-NI_depto_edad$NI_cv),]
NI_depto_edad

openxlsx::write.xlsx(NI_depto_edad, file = "PER/2020/3.NI/Output/Tablas/NI_Depto_edad.xlsx")


########################### Por Depto y Edad ###############################

## Necesidades Insatisfechas NI


NI_mpio = design.base.unidas %>% group_by(mpio) %>%
  summarise(n = unweighted(n()),
            NI = survey_mean(nec_insat,
                              vartype = c("cv")))

NI_mpio
NI_mpio$NI <- NI_mpio$NI * 100
NI_mpio$NI <- round(NI_mpio$NI, digits = 1)
NI_mpio$NI_cv <- NI_mpio$NI_cv * 100
NI_mpio$NI_cv <- round(NI_mpio$NI_cv, digits = 1)
#NI_mpio    <-NI_mpio[order(-NI_mpio$NI_cv),]
NI_mpio

openxlsx::write.xlsx(NI_mpio, file = "PER/2020/3.NI/Output/Tablas/NI_mpio.xlsx")

