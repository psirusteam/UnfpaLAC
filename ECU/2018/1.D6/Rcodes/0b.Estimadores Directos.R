############################################################################################################################################
#############################################################################################################################################
# # 
# # Proceso: Estimación indicadores de planificación
# #              familiar utilizando ENSANUT 2018
# # ◘
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

Ensanut<-readRDS("ECU/2018/1.D6/Data/base_estimaciones19.rds")

######################################################################################################################################################

#### Explorando

names(Ensanut)

table(Ensanut$area, useNA = "a")
table(Ensanut$edad, useNA = "a")
table(Ensanut$etnia, useNA = "a")
table(Ensanut$discapacidad, useNA = "a")
table(Ensanut$anoest, useNA = "a")
table(Ensanut$unida, useNA = "a")
table(Ensanut$mpio, useNA = "a")

#######################

# Creación objetivo diseño de muestreo complejo para unidas y total

#### Unidas

Ensanut_Unidas <- Ensanut %>% 
  filter(unida == "Unida", edad != "12-14" )


options(survey.lonely.psu = "adjust") 

diseno = Ensanut_Unidas %>% as_survey_design(ids = upm,
                                              strat = estrato, 
                                              weights = fexp, nest = TRUE)

#### Total

Ensanut_edad <- Ensanut %>% 
  filter(edad != "12-14")

design.base = Ensanut_edad %>% as_survey_design(ids = upm,
                                                 strat = estrato, 
                                                 weights = fexp, nest = TRUE)

# Flags

nflag = 100
cvflag = 30
nefflag = 68
dfflag = 10
yflag = 100

#####################

table(Ensanut_Unidas$usametodo, useNA = "a")
table(Ensanut_Unidas$usametodo, useNA = "a")
table(Ensanut_Unidas$nec_insat, useNA = "a")

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

D6_Depto = diseno %>% group_by(Depto) %>%
  summarise(n = unweighted(n()),
            D6 = survey_mean(usametodo,
                             vartype = c("cv")))

D6_Depto
D6_Depto$D6 <- D6_Depto$D6 * 100
D6_Depto$D6 <- round(D6_Depto$D6, digits = 1)
D6_Depto$D6_cv <- D6_Depto$D6_cv * 100
D6_Depto$D6_cv <- round(D6_Depto$D6_cv, digits = 1)
D6_Depto    <-D6_Depto[order(-D6_Depto$D6),]
D6_Depto

openxlsx::write.xlsx(D6_Depto, file = "ECU/2018/1.D6/Output/Tablas/D6_Depto.xlsx")
# 
# 
####### Area (Mujeres Unidas)

# design.base.total
# design.base.unidas

D6_area = diseno %>% group_by(area) %>%
  summarise(n = unweighted(n()),
            D6 = survey_mean(usametodo,
                             vartype = c("cv")))

D6_area <- as.data.frame(D6_area)
D6_area
D6_area$D6 <- D6_area$D6 * 100
D6_area$D6 <- round(D6_area$D6, digits = 1)
D6_area$area <- factor(x= D6_area$area,
                       levels=c("Urbano","Rural"))
D6_area

openxlsx::write.xlsx(D6_area, file = "ECU/2018/1.D6/Output/Tablas/D6_area.xlsx")

####### Edad

# design.base.total
# design.base.unidas

D6_edad = diseno %>% group_by(edad) %>%
  summarise(n = unweighted(n()),
            D6 = survey_mean(usametodo,
                             vartype = c("cv")))

D6_edad <- as.data.frame(D6_edad)
D6_edad$D6 <- D6_edad$D6 * 100
D6_edad$D6 <- round(D6_edad$D6, digits = 1)
D6_edad<-D6_edad[order(-D6_edad$D6),]
D6_edad$D6_cv <- D6_edad$D6_cv * 100
D6_edad$D6_cv <- round(D6_edad$D6_cv, digits = 1)
D6_edad

openxlsx::write.xlsx(D6_edad, file = "ECU/2018/1.D6/Output/Tablas/D6_edad.xlsx")

####### Años estudio (Mujeres Unidas)

# design.base.total
# design.base.unidas

D6_anoest = diseno %>% group_by(anoest) %>%
  summarise(n = unweighted(n()),
            D6 = survey_mean(usametodo,
                             vartype = c("cv")))

D6_anoest <- as.data.frame(D6_anoest)
D6_anoest$D6 <- D6_anoest$D6 * 100
D6_anoest$D6 <- round(D6_anoest$D6, digits = 1)
table(D6_anoest$anoest)

D6_anoest

openxlsx::write.xlsx(D6_anoest, file = "ECU/2018/1.D6/Output/Tablas/D6_anoest.xlsx")

####### Etnia

# design.base.total
# design.base.unidas

D6_etnia = diseno %>% group_by(etnia) %>%
  summarise(n = unweighted(n()),
            D6 = survey_mean(usametodo,
                             vartype = c("cv")))

D6_etnia <- as.data.frame(D6_etnia)
D6_etnia$D6 <- D6_etnia$D6 * 100
D6_etnia$D6 <- round(D6_etnia$D6, digits = 1)
D6_etnia$D6_cv <- D6_etnia$D6_cv * 100
D6_etnia$D6_cv <- round(D6_etnia$D6_cv, digits = 1)
D6_etnia<-D6_etnia[order(-D6_etnia$D6),]
D6_etnia

openxlsx::write.xlsx(D6_etnia, file = "ECU/2018/1.D6/Output/Tablas/D6_etnia.xlsx")

####### Discapacidad

D6_discap = diseno %>% group_by(discapacidad) %>%
  summarise(n = unweighted(n()),
            D6 = survey_mean(usametodo,
                             vartype = c("cv")))

D6_discap <- as.data.frame(D6_discap)
D6_discap$D6 <- D6_discap$D6 * 100
D6_discap$D6 <- round(D6_discap$D6, digits = 1)
D6_discap

openxlsx::write.xlsx(D6_discap, file = "ECU/2018/1.D6/Output/Tablas/D6_Discp.xlsx")

##### Resultados desagregaciones covariables

D6_nal
D6_area
D6_anoest
D6_etnia
D6_edad
D6_discap
#D6_Depto

########################### Por Area y Escolaridad ###############################

## Usa métodos de Planificación D6

D6_area_anoest = diseno %>% group_by(area, anoest) %>%
  summarise(n = unweighted(n()),
            D6 = survey_mean(usametodo,
                             vartype = c("cv")))

D6_area_anoest
D6_area_anoest$D6 <- D6_area_anoest$D6 * 100
D6_area_anoest$D6 <- round(D6_area_anoest$D6, digits = 1)
D6_area_anoest$D6_cv <- D6_area_anoest$D6_cv * 100
D6_area_anoest$D6_cv <- round(D6_area_anoest$D6_cv, digits = 1)
D6_area_anoest<-D6_area_anoest[order(-D6_area_anoest$D6),]
D6_area_anoest

openxlsx::write.xlsx(D6_area_anoest, file = "ECU/2018/1.D6/Output/Tablas/D6_area_escolaridad.xlsx")


########################### Por Depto y Edad ###############################

# ## Usa métodos de Planificación D6
# 
D6_Depto_edad = diseno %>% group_by(Depto, edad) %>%
   summarise(n = unweighted(n()),
             D6 = survey_mean(usametodo,
                              vartype = c("cv")))
#
D6_Depto_edad
D6_Depto_edad$D6 <- D6_Depto_edad$D6 * 100
D6_Depto_edad$D6 <- round(D6_Depto_edad$D6, digits = 1)
D6_Depto_edad$D6_cv <- D6_Depto_edad$D6_cv * 100
D6_Depto_edad$D6_cv <- round(D6_Depto_edad$D6_cv, digits = 1)
D6_Depto_edad    <-D6_Depto_edad[order(-D6_Depto_edad$D6_cv),]
D6_Depto_edad

openxlsx::write.xlsx(D6_Depto_edad, file = "ECU/2018/1.D6/Output/Tablas/D6_Depto_edad.xlsx")


########################### Por Depto y Edad ###############################

## Usa métodos de Planificación D6


D6_mpio = diseno %>% group_by(mpio) %>%
  summarise(n = unweighted(n()),
            D6 = survey_mean(usametodo,
                             vartype = c("cv")))

D6_mpio
D6_mpio$D6 <- D6_mpio$D6 * 100
D6_mpio$D6 <- round(D6_mpio$D6, digits = 1)
D6_mpio$D6_cv <- D6_mpio$D6_cv * 100
D6_mpio$D6_cv <- round(D6_mpio$D6_cv, digits = 1)
#D6_mpio    <-D6_mpio[order(-D6_mpio$D6_cv),]
D6_mpio

openxlsx::write.xlsx(D6_mpio, file = "ECU/2018/1.D6/Output/Tablas/D6_mpio.xlsx")

