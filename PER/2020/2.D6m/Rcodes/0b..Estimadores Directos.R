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

EndesMEF = readRDS("PER/2020/2.D6m/Data/ENDESMEF_Est.rds")

######################################################################################################################################################

#### Explorando

table(EndesMEF$area, useNA = "a")
table(EndesMEF$edad, useNA = "a")
table(EndesMEF$etnia, useNA = "a")
table(EndesMEF$discapacidad, useNA = "a")
table(EndesMEF$anoest, useNA = "a")
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

#######################################
## Indicador D6m: Uso Anticonceptivos ##
#######################################

####################
## Nivel Nacional ##
####################

#######  Informe nacional USO ACTUAL DE MÉTODOS ANTICONCEPTIVOS 
#######                   ENTRE LAS MUJERES ACTUALMENTE UNIDAS

####### Cuadro anexo Excel Cap 004 4.2

D6m_nal = design.base %>% #group_by(dominio) %>%
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


#######  Informe nacional USO ACTUAL DE MÉTODOS ANTICONCEPTIVOS 
#######                   ENTRE LAS MUJERES ACTUALMENTE UNIDAS

####### Por Departamento Cuadro anexo Excel Cap 004 4.4

## Usa métodos de Planificación D6m

# design.base.total
# design.base.unidas

D6m_Depto = design.base.unidas %>% group_by(Depto) %>%
  summarise(n = unweighted(n()),
            D6m = survey_mean(usamoderno,
                             vartype = c("cv")))

D6m_Depto
D6m_Depto$D6m <- D6m_Depto$D6m * 100
D6m_Depto$D6m <- round(D6m_Depto$D6m, digits = 1)
D6m_Depto$D6m_cv <- D6m_Depto$D6m_cv * 100
D6m_Depto$D6m_cv <- round(D6m_Depto$D6m_cv, digits = 1)
#D6m_Depto    <-D6m_Depto[order(-D6m_Depto$D6m),]
D6m_Depto

openxlsx::write.xlsx(D6m_Depto, file = "PER/2020/2.D6m/Output/Tablas/D6m_Depto.xlsx")


#######  Informe nacional USO ACTUAL DE MÉTODOS ANTICONCEPTIVOS 
#######                   ENTRE LAS MUJERES ACTUALMENTE UNIDAS

####### Por Area Cuadro anexo Excel Cap 004 4.4

# design.base.total
# design.base.unidas

D6m_area = design.base.unidas %>% group_by(area) %>%
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

openxlsx::write.xlsx(D6m_area, file = "PER/2020/2.D6m/Output/Tablas/D6m_area.xlsx")

#######  Informe nacional USO ACTUAL DE MÉTODOS ANTICONCEPTIVOS 
#######                   ENTRE LAS MUJERES ACTUALMENTE UNIDAS

####### Por edad Cuadro anexo Excel Cap 004 4.2

# design.base.total
# design.base.unidas

D6m_edad = design.base.total %>% group_by(edad) %>%
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

openxlsx::write.xlsx(D6m_edad, file = "PER/2020/2.D6m/Output/Tablas/D6m_edad.xlsx")

#######  Informe nacional USO ACTUAL DE MÉTODOS ANTICONCEPTIVOS 
#######                   ENTRE LAS MUJERES ACTUALMENTE UNIDAS

####### Años de estudio Cuadro anexo Excel Cap 004 4.3

# design.base.total
# design.base.unidas

D6m_anoest = design.base.unidas %>% group_by(anoest) %>%
  summarise(n = unweighted(n()),
            D6m = survey_mean(usamoderno,
                             vartype = c("cv")))

D6m_anoest <- as.data.frame(D6m_anoest)
D6m_anoest$D6m <- D6m_anoest$D6m * 100
D6m_anoest$D6m <- round(D6m_anoest$D6m, digits = 1)
table(D6m_anoest$anoest)
D6m_anoest$anoest <- factor(x= D6m_anoest$anoest,
                           levels=c("Sin educación","Primaria",
                                    "Secundaria","Superior"))
D6m_anoest

openxlsx::write.xlsx(D6m_anoest, file = "PER/2020/2.D6m/Output/Tablas/D6m_anoest.xlsx")

####### Etnia

# design.base.total
# design.base.unidas

D6m_etnia = design.base.total %>% group_by(etnia) %>%
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

openxlsx::write.xlsx(D6m_etnia, file = "PER/2020/2.D6m/Output/Tablas/D6m_etnia.xlsx")

####### Discapacidad

D6m_discap = design.base.total %>% group_by(discapacidad) %>%
  summarise(n = unweighted(n()),
            D6m = survey_mean(usamoderno,
                             vartype = c("cv")))

D6m_discap <- as.data.frame(D6m_discap)
D6m_discap$D6m <- D6m_discap$D6m * 100
D6m_discap$D6m <- round(D6m_discap$D6m, digits = 1)
D6m_discap

openxlsx::write.xlsx(D6m_discap, file = "PER/2020/2.D6m/Output/Tablas/D6m_Discp.xlsx")

##### Resultados desagregaciones covariables

D6m_nal
D6m_area
D6m_anoest
D6m_etnia
D6m_edad
D6m_discap
D6m_Depto

########################### Por Area y Escolaridad ###############################

## Usa métodos de Planificación D6m

D6m_area_anoest = design.base.total %>% group_by(area, anoest) %>%
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

openxlsx::write.xlsx(D6m_area_anoest, file = "PER/2020/2.D6m/Output/Tablas/D6m_area_escolaridad.xlsx")


########################### Por Depto y Edad ###############################

## Usa métodos de Planificación D6m

D6m_depto_edad = design.base.total %>% group_by(Depto, edad) %>%
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

openxlsx::write.xlsx(D6m_depto_edad, file = "PER/2020/2.D6m/Output/Tablas/D6m_Depto_edad.xlsx")


########################### Por Depto y Edad ###############################

## Usa métodos de Planificación D6m


D6m_mpio = design.base.total %>% group_by(mpio) %>%
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

openxlsx::write.xlsx(D6m_mpio, file = "PER/2020/2.D6m/Output/Tablas/D6m_mpio.xlsx")

