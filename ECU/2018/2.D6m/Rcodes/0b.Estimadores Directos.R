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

#Ensanut<-readRDS("ECU/2018/2.D6m/Data/base_estimaciones.rds")
Ensanut<-readRDS("ECU/2018/2.D6m/Data/base_estimaciones19.rds")

######################################################################################################################################################

#### Explorando
names(Ensanut)

table(Ensanut$area, useNA = "a")
table(Ensanut$edad, useNA = "a")
table(Ensanut$etnia, useNA = "a")
#table(Ensanut$discap, useNA = "a")
table(Ensanut$anoest, useNA = "a")
table(Ensanut$unida, useNA = "a")
table(Ensanut$usametodo, useNA = "a")
table(Ensanut$usamoderno, useNA = "a")

# Creación objetivo diseño de muestreo complejo para unidas y total

### Mujeres que usan métodos de planificación

muj_usame <-Ensanut %>% 
  filter(usametodo == 1)

#### Unidas

Ensanut_Unidas <- muj_usame %>% 
  filter(unida == "Unida", edad != "12-14" )

table(Ensanut_Unidas$usamoderno)
table(Ensanut_Unidas$edad)

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
table(Ensanut_Unidas$usamoderno, useNA = "a")
table(Ensanut_Unidas$nec_insat, useNA = "a")
table(Ensanut_edad$edad)
table(Ensanut$edad)
table(Ensanut$unida)


#######################################
## Indicador D6m: Uso Anticonceptivos ##
#######################################

####################
## Nivel Nacional ##
####################


D6m_nal = diseno %>% #group_by(dominio) %>%
  summarise(n = unweighted(n()),
            D6m = survey_mean(usamoderno,
                             vartype = c("cv")))

D6m_nal
D6m_nal <- as.data.frame(D6m_nal)
D6m_nal$D6m <- D6m_nal$D6m * 100
D6m_nal$D6m <- round(D6m_nal$D6m, digits = 1)
D6m_nal


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


####### Por Departamento  (Mujeres Unidas) Cuadro 8.4.1.2 Pag 58

## Usa métodos de Planificación D6m

# design.base.total
# design.base.unidas

D6m_Depto = diseno %>% group_by(Depto) %>%
  summarise(n = unweighted(n()),
            D6m = survey_mean(usamoderno,
                             vartype = c("cv")))

D6m_Depto
D6m_Depto$D6m <- D6m_Depto$D6m * 100
D6m_Depto$D6m <- round(D6m_Depto$D6m, digits = 1)
D6m_Depto$D6m_cv <- D6m_Depto$D6m_cv * 100
D6m_Depto$D6m_cv <- round(D6m_Depto$D6m_cv, digits = 1)
D6m_Depto    <-D6m_Depto[order(-D6m_Depto$D6m),]
D6m_Depto

openxlsx::write.xlsx(D6m_Depto, file = "ECU/2018/2.D6m/Output/Tablas/D6m_Depto.xlsx")

# 
####### Area (Mujeres Unidas) 

# design.base.total
# design.base.unidas

D6m_area = diseno %>% group_by(area) %>%
  summarise(n = unweighted(n()),
            D6m = survey_mean(usamoderno,
                             vartype = c("cv")))

D6m_area <- as.data.frame(D6m_area)
D6m_area
D6m_area$D6m <- D6m_area$D6m * 100
D6m_area$D6m <- round(D6m_area$D6m, digits = 1)
D6m_area$area <- factor(x= D6m_area$area,
                       levels=c("Urbano","Rural"))
D6m_area

openxlsx::write.xlsx(D6m_area, file = "ECU/2018/2.D6m/Output/Tablas/D6m_area.xlsx")

####### Edad 

# design.base.total
# design.base.unidas

D6m_edad = diseno %>% group_by(edad) %>%
  summarise(n = unweighted(n()),
            D6m = survey_mean(usamoderno,
                             vartype = c("cv")))

D6m_edad <- as.data.frame(D6m_edad)
D6m_edad$D6m <- D6m_edad$D6m * 100
D6m_edad$D6m <- round(D6m_edad$D6m, digits = 1)
D6m_edad<-D6m_edad[order(-D6m_edad$D6m),]
D6m_edad$D6m_cv <- D6m_edad$D6m_cv * 100
D6m_edad$D6m_cv <- round(D6m_edad$D6m_cv, digits = 1)
D6m_edad

openxlsx::write.xlsx(D6m_edad, file = "ECU/2018/2.D6m/Output/Tablas/D6m_edad.xlsx")

####### Años estudio (Mujeres Unidas) Cuadro 8.4.1.1 Pag 56

# design.base.total
# design.base.unidas

D6m_anoest = diseno %>% group_by(anoest) %>%
  summarise(n = unweighted(n()),
            D6m = survey_mean(usamoderno,
                             vartype = c("cv")))

D6m_anoest <- as.data.frame(D6m_anoest)
D6m_anoest$D6m <- D6m_anoest$D6m * 100
D6m_anoest$D6m <- round(D6m_anoest$D6m, digits = 1)
table(D6m_anoest$anoest)

D6m_anoest

openxlsx::write.xlsx(D6m_anoest, file = "ECU/2018/2.D6m/Output/Tablas/D6m_anoest.xlsx")

####### Etnia

# design.base.total
# design.base.unidas

D6m_etnia = diseno %>% group_by(etnia) %>%
  summarise(n = unweighted(n()),
            D6m = survey_mean(usamoderno,
                             vartype = c("cv")))

D6m_etnia <- as.data.frame(D6m_etnia)
D6m_etnia$D6m <- D6m_etnia$D6m * 100
D6m_etnia$D6m <- round(D6m_etnia$D6m, digits = 1)
D6m_etnia$D6m_cv <- D6m_etnia$D6m_cv * 100
D6m_etnia$D6m_cv <- round(D6m_etnia$D6m_cv, digits = 1)
D6m_etnia<-D6m_etnia[order(-D6m_etnia$D6m),]
D6m_etnia

openxlsx::write.xlsx(D6m_etnia, file = "ECU/2018/2.D6m/Output/Tablas/D6m_etnia.xlsx")

####### Discapacidad

D6m_discap = diseno %>% group_by(discapacidad) %>%
  summarise(n = unweighted(n()),
            D6m = survey_mean(usamoderno,
                             vartype = c("cv")))

D6m_discap <- as.data.frame(D6m_discap)
D6m_discap$D6m <- D6m_discap$D6m * 100
D6m_discap$D6m <- round(D6m_discap$D6m, digits = 1)
D6m_discap

openxlsx::write.xlsx(D6m_discap, file = "ECU/2018/2.D6m/Output/Tablas/D6m_Discp.xlsx")

##### Resultados desagregaciones covariables

D6m_nal
D6m_area
D6m_anoest
D6m_etnia
D6m_edad
D6m_discap
#D6m_Depto

########################### Por Area y Escolaridad ###############################

## Usa métodos de Planificación D6m

D6m_area_anoest = diseno %>% group_by(area, anoest) %>%
  summarise(n = unweighted(n()),
            D6m = survey_mean(usamoderno,
                             vartype = c("cv")))

D6m_area_anoest
D6m_area_anoest$D6m <- D6m_area_anoest$D6m * 100
D6m_area_anoest$D6m <- round(D6m_area_anoest$D6m, digits = 1)
D6m_area_anoest$D6m_cv <- D6m_area_anoest$D6m_cv * 100
D6m_area_anoest$D6m_cv <- round(D6m_area_anoest$D6m_cv, digits = 1)
D6m_area_anoest<-D6m_area_anoest[order(-D6m_area_anoest$D6m),]
D6m_area_anoest

openxlsx::write.xlsx(D6m_area_anoest, file = "ECU/2018/2.D6m/Output/Tablas/D6m_area_escolaridad.xlsx")


########################### Por Depto y Edad ###############################

# ## Usa métodos de Planificación D6m
# 
D6m_depto_edad = diseno %>% group_by(Depto, edad) %>%
  summarise(n = unweighted(n()),
            D6m = survey_mean(usamoderno,
                             vartype = c("cv")))

D6m_depto_edad
D6m_depto_edad$D6m <- D6m_depto_edad$D6m * 100
D6m_depto_edad$D6m <- round(D6m_depto_edad$D6m, digits = 1)
D6m_depto_edad$D6m_cv <- D6m_depto_edad$D6m_cv * 100
D6m_depto_edad$D6m_cv <- round(D6m_depto_edad$D6m_cv, digits = 1)
D6m_depto_edad    <-D6m_depto_edad[order(-D6m_depto_edad$D6m_cv),]
D6m_depto_edad

openxlsx::write.xlsx(D6m_depto_edad, file = "ECU/2018/2.D6m/Output/Tablas/D6m_Depto_edad.xlsx")


########################### Por Depto y Edad ###############################

## Usa métodos de Planificación D6m


D6m_mpio = diseno %>% group_by(mpio) %>%
  summarise(n = unweighted(n()),
            D6m = survey_mean(usamoderno,
                             vartype = c("cv")))

D6m_mpio
D6m_mpio$D6m <- D6m_mpio$D6m * 100
D6m_mpio$D6m <- round(D6m_mpio$D6m, digits = 1)
D6m_mpio$D6m_cv <- D6m_mpio$D6m_cv * 100
D6m_mpio$D6m_cv <- round(D6m_mpio$D6m_cv, digits = 1)
#D6m_mpio    <-D6m_mpio[order(-D6m_mpio$D6m_cv),]
D6m_mpio

openxlsx::write.xlsx(D6m_mpio, file = "ECU/2018/2.D6m/Output/Tablas/D6m_mpio.xlsx")

