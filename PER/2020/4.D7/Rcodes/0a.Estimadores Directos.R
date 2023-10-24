############################################################################################################################################
#############################################################################################################################################
# # 
# # Proceso: Estimación indicadores de planificación
# #              familiar utilizando ENDS 2020
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

EndesMEF = readRDS("PER/2020/4.D7/Data/ENDESMEF_Est.rds")

######################################################################################################################################################

names(EndesMEF)

#### Explorando

table(EndesMEF$area, useNA = "a")
table(EndesMEF$edad, useNA = "a")
table(EndesMEF$etnia, useNA = "a")
table(EndesMEF$anoest, useNA = "a")
table(EndesMEF$discapacidad, useNA = "a")
table(EndesMEF$usamoderno, useNA = "a")
table(EndesMEF$usametodo, useNA = "a")
table(EndesMEF$nec_insat, useNA = "a")
EndesMEF$Nec_sat_pf_T[is.na(EndesMEF$Nec_sat_pf_T)] <- 0
table(EndesMEF$Nec_sat_pf_T, useNA = "a")
table(EndesMEF$Falla_met, useNA = "a")

####### Creando variable de demanda total para cacular la estimación
####### ya qe en el informe no está la demanda satisfecha por métodos modernos

EndesMEF$Pds<- ifelse(EndesMEF$Nec_sat_pf_T == 1, 1,
                      ifelse(EndesMEF$nec_insat ==1, 1,
                             ifelse(EndesMEF$Falla_met==1,1,0)))

table(EndesMEF$Pds, useNA = "a")

## validación de cálculos
# EndesMEF$Pds2<-(EndesMEF$Nec_sat_pf_T + EndesMEF$nec_insat + EndesMEF$Falla_met)
# table(EndesMEF$Pds2, useNA = "a")


# Creación objetivo diseño de muestreo complejo para unidas y total


#### Unidas

EndesMEF_Unidas <- EndesMEF %>% 
  filter(unida == "Unida", edad != "12-14")


options(survey.lonely.psu = "adjust") 

design.base.unidas = EndesMEF_Unidas %>% as_survey_design(ids = upm,
                                                          strat = estrato, 
                                                          weights = fexp, nest = TRUE)

#### Total

EndesMEF_edad <- EndesMEF %>% 
  filter(edad != "12-14")

design.base.total = EndesMEF_edad %>% as_survey_design(ids = upm,
                                                       strat = estrato, 
                                                       weights = fexp, nest = TRUE)

# Flags

nflag = 100
cvflag = 30
nefflag = 68
dfflag = 10
yflag = 100

#####################


#########################################################################
## Indicador D7: necesidad satisfecha mediante uso de metodos modernos ##
#########################################################################

####################
## Nivel Nacional ##
####################  Cuadro informe nacional (Anexo en Excel 4.2)

## Porcentaje de demanda satisfecha 

D7_nal = design.base.unidas %>% #group_by(dominio) %>%
  summarise(n = unweighted(n()),
            D7 = survey_mean(Pds,
                             vartype = c("cv")))

D7_nal
D7_nal <- as.data.frame(D7_nal)
D7_nal$D7 <- D7_nal$D7 * 100
D7_nal$D7 <- round(D7_nal$D7, digits = 1)
D7_nal

####################
## Nivel Nacional ##
####################
## Porcentaje de demanda satisfecha metodos modernos

D7_nal_mm = design.base.unidas %>% #group_by(dominio) %>%
  summarise(n = unweighted(n()),
            D7 = survey_ratio(usamoderno,usametodo + nec_insat,
                              vartype = c("cv")))
D7_nal_mm
D7_D7_nal_mmnal <- as.data.frame(D7_nal_mm)
D7_nal_mm$D7 <- D7_nal_mm$D7 * 100
D7_nal_mm$D7 <- round(D7_nal_mm$D7, digits = 1)
D7_nal_mm


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
#######      (Mujeres Unidas) Cuadro informe nacional (Anexo en Excel 4.4)

## Usa métodos de Planificación D7

# design.base.total
# design.base.unidas

D7_Depto = design.base.unidas %>% group_by(Depto) %>%
  summarise(n = unweighted(n()),
            D7 = survey_mean(Pds,
                             vartype = c("cv")))

D7_Depto
D7_Depto$D7 <- D7_Depto$D7 * 100
D7_Depto$D7 <- round(D7_Depto$D7, digits = 1)
D7_Depto$D7_cv <- D7_Depto$D7_cv * 100
D7_Depto$D7_cv <- round(D7_Depto$D7_cv, digits = 1)
D7_Depto    <-D7_Depto[order(-D7_Depto$D7),]
D7_Depto

openxlsx::write.xlsx(D7_Depto, file = "PER/2020/4.D7/Output/Tablas/D7_Depto.xlsx")


####### Area (Mujeres Unidas) Cuadro informe nacional (Anexo en Excel 4.4)

# design.base.total
# design.base.unidas

D7_area = design.base.unidas %>% group_by(area) %>%
  summarise(n = unweighted(n()),
            D7 = survey_mean(Pds,
                             vartype = c("cv")))

D7_area <- as.data.frame(D7_area)
D7_area
D7_area$D7 <- D7_area$D7 * 100
D7_area$D7 <- round(D7_area$D7, digits = 1)
D7_area$area <- factor(x= D7_area$area,
                       levels=c("Urbano","Rural"))
D7_area

openxlsx::write.xlsx(D7_area, file = "PER/2020/4.D7/Output/Tablas/D7_area.xlsx")

####### Edad Cuadro informe nacional (Anexo en Excel 4.2)

# design.base.total
# design.base.unidas

D7_edad = design.base.unidas %>% group_by(edad) %>%
  summarise(n = unweighted(n()),
            D7 = survey_mean(Pds,
                             vartype = c("cv")))

D7_edad <- as.data.frame(D7_edad)
D7_edad$D7 <- D7_edad$D7 * 100
D7_edad$D7 <- round(D7_edad$D7, digits = 1)
D7_edad<-D7_edad[order(-D7_edad$D7),]
D7_edad$D7_cv <- D7_edad$D7_cv * 100
D7_edad$D7_cv <- round(D7_edad$D7_cv, digits = 1)
D7_edad

openxlsx::write.xlsx(D7_edad, file = "PER/2020/4.D7/Output/Tablas/D7_edad.xlsx")

####### Años estudio 
#######(Mujeres Unidas) Cuadro informe nacional (Anexo en Excel 4.3) 

# design.base.total
# design.base.unidas

D7_anoest = design.base.unidas %>% group_by(anoest) %>%
  summarise(n = unweighted(n()),
            D7 = survey_mean(Pds,
                             vartype = c("cv")))

D7_anoest <- as.data.frame(D7_anoest)
D7_anoest$D7 <- D7_anoest$D7 * 100
D7_anoest$D7 <- round(D7_anoest$D7, digits = 1)
table(D7_anoest$anoest)
D7_anoest$anoest <- factor(x= D7_anoest$anoest,
                           levels=c("Sin educación","Primaria",
                                    "Secundaria","Superior"))
D7_anoest

openxlsx::write.xlsx(D7_anoest, file = "PER/2020/4.D7/Output/Tablas/D7_anoest.xlsx")

####### Etnia

# design.base.total
# design.base.unidas

D7_etnia = design.base.total %>% group_by(etnia) %>%
  summarise(n = unweighted(n()),
            D7 = survey_ratio(usamoderno,usametodo + nec_insat,
                              vartype = c("cv")))

D7_etnia <- as.data.frame(D7_etnia)
D7_etnia$D7 <- D7_etnia$D7 * 100
D7_etnia$D7 <- round(D7_etnia$D7, digits = 1)
D7_etnia$D7_cv <- D7_etnia$D7_cv * 100
D7_etnia$D7_cv <- round(D7_etnia$D7_cv, digits = 1)
D7_etnia<-D7_etnia[order(-D7_etnia$D7),]
D7_etnia

openxlsx::write.xlsx(D7_etnia, file = "PER/2020/4.D7/Output/Tablas/D7_etnia.xlsx")

####### Discapacidad

D7_discap = design.base.total %>% group_by(discapacidad) %>%
  summarise(n = unweighted(n()),
            D7 = survey_ratio(usamoderno,usametodo + nec_insat,
                              vartype = c("cv")))

D7_discap <- as.data.frame(D7_discap)
D7_discap$D7 <- D7_discap$D7 * 100
D7_discap$D7 <- round(D7_discap$D7, digits = 1)
D7_discap

openxlsx::write.xlsx(D7_discap, file = "PER/2020/4.D7/Output/Tablas/D7_Discp.xlsx")

##### Resultados desagregaciones covariables

D7_nal
D7_area
D7_anoest
D7_etnia
D7_edad
D7_discap
D7_Depto

########################### Por Area y Escolaridad ###############################

## Usa métodos de Planificación D7

D7_area_anoest = design.base.total %>% group_by(area, anoest) %>%
  summarise(n = unweighted(n()),
            D7 = survey_ratio(usamoderno,usametodo + nec_insat,
                              vartype = c("cv")))

D7_area_anoest
D7_area_anoest$D7 <- D7_area_anoest$D7 * 100
D7_area_anoest$D7 <- round(D7_area_anoest$D7, digits = 1)
D7_area_anoest$D7_cv <- D7_area_anoest$D7_cv * 100
D7_area_anoest$D7_cv <- round(D7_area_anoest$D7_cv, digits = 1)
D7_area_anoest<-D7_area_anoest[order(-D7_area_anoest$D7),]
D7_area_anoest

openxlsx::write.xlsx(D7_area_anoest, file = "PER/2020/4.D7/Output/Tablas/D7_area_escolaridad.xlsx")


########################### Por Depto y Edad ###############################

## Usa métodos de Planificación D7

D7_depto_edad = design.base.total %>% group_by(Depto, edad) %>%
  summarise(n = unweighted(n()),
            D7 = survey_ratio(usamoderno,usametodo + nec_insat,
                              vartype = c("cv")))

D7_depto_edad
D7_depto_edad$D7 <- D7_depto_edad$D7 * 100
D7_depto_edad$D7 <- round(D7_depto_edad$D7, digits = 1)
D7_depto_edad$D7_cv <- D7_depto_edad$D7_cv * 100
D7_depto_edad$D7_cv <- round(D7_depto_edad$D7_cv, digits = 1)
D7_depto_edad    <-D7_depto_edad[order(-D7_depto_edad$D7_cv),]
D7_depto_edad

openxlsx::write.xlsx(D7_depto_edad, file = "PER/2020/4.D7/Output/Tablas/D7_Depto_edad.xlsx")


########################### Por Depto y Edad ###############################

## Usa métodos de Planificación D7


D7_mpio = design.base.total %>% group_by(mpio) %>%
  summarise(n = unweighted(n()),
            D7 = survey_ratio(usamoderno,usametodo + nec_insat,
                              vartype = c("cv")))

D7_mpio
D7_mpio$D7 <- D7_mpio$D7 * 100
D7_mpio$D7 <- round(D7_mpio$D7, digits = 1)
D7_mpio$D7_cv <- D7_mpio$D7_cv * 100
D7_mpio$D7_cv <- round(D7_mpio$D7_cv, digits = 1)
#D7_mpio    <-D7_mpio[order(-D7_mpio$D7_cv),]
D7_mpio

openxlsx::write.xlsx(D7_mpio, file = "PER/2020/4.D7/Output/Tablas/D7_mpio.xlsx")

