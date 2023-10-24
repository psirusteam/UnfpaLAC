############################################################################################################################################
#############################################################################################################################################
# # 
# # Proceso: Estimación indicadores de planificación
# #              familiar utilizando ENDS 2015
# # 
# # Historia Archivo:
# #   Creation : 30/06/2022
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
library(formattable)

######################################################################################################################################################
###                             Limpiando la memoria 
######################################################################################################################################################

rm(list = ls(all=T))
gc()

###########################################################################################################################

######################################################################################################################################################
##                                    Cargando Insumos
######################################################################################################################################################

ENDS<-readRDS("COL/2015/4.D7/Data/base_estimaciones.rds")

######################################################################################################################################################

#### Explorando

table(ENDS$area, useNA = "a")
table(ENDS$edad, useNA = "a")
table(ENDS$etnia, useNA = "a")
table(ENDS$anoest, useNA = "a")
table(ENDS$unida, useNA = "a")
table(ENDS$casada, useNA = "a")
table(ENDS$discapacidad, useNA = "a")


# Creación objetivo diseño de muestreo complejo para unidas y total


################################ Total mujeres entrevistadas 13-49 años

options(survey.lonely.psu = "adjust")

design.base.total = ENDS %>% as_survey_design(ids = upm,
                                              strat = estrato, 
                                              weights = fexp, nest = TRUE)


################################ Total mujeres Unidas

ENDS_Unidas <- ENDS %>% 
  filter(unida == "Unida")

options(survey.lonely.psu = "adjust") 

design.base.unidas = ENDS_Unidas %>% as_survey_design(ids = upm,
                                                      strat = estrato, 
                                                      weights = fexp, nest = TRUE)

#######################

# Flags

nflag = 100
cvflag = 30
nefflag = 68
dfflag = 10
yflag = 100

#####################


##########################################################################
##        Indicador D7: Demanda satisfecha por métodos modernos               ##
##########################################################################

#############################################################################
##                              ESTIMACIONES DIRECTAS                      ##
#############################################################################

####################
## Nivel Nacional ##
####################

# design.base.total
# design.base.unidas


D7_nal = design.base.unidas %>% #group_by(dominio) %>%
  #D7_nal = design.base.unidas %>% #group_by(dominio) %>%  
  summarise(n = unweighted(n()),
            D7 = survey_ratio(usamoderno, usametodo + nec_insat),
                             vartype = c("cv"))

D7_nal
D7_nal <- as.data.frame(D7_nal)
D7_nal$D7 <- D7_nal$D7 * 100
D7_nal$D7 <- round(D7_nal$D7, digits = 1)
D7_nal$D7_se <- D7_nal$D7_se * 100
D7_nal$D7_se <- round(D7_nal$D7_se, digits = 1)
D7_nal


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

## Usa métodos de Planificación NI

# design.base.total
# design.base.unidas

D7_Depto = design.base.unidas %>% group_by(Depto) %>%
  summarise(n = unweighted(n()),
            D7 = survey_ratio(usamoderno,usametodo + nec_insat,
                             vartype = c("cv")))

D7_Depto
D7_Depto$D7 <- D7_Depto$D7 * 100
D7_Depto$D7 <- round(D7_Depto$D7, digits = 1)
D7_Depto$D7_cv <- D7_Depto$D7_cv * 100
D7_Depto$D7_cv <- round(D7_Depto$D7_cv, digits = 1)
D7_Depto    <-D7_Depto[order(-D7_Depto$D7),]
D7_Depto

openxlsx::write.xlsx(D7_Depto, file = "COL/2015/4.D7/Output/Tablas/D7_Depto.xlsx")


####### Area (Mujeres Unidas) Cuadro 8.4.1.1 Pag 56

# design.base.total
# design.base.unidas

D7_area = design.base.unidas %>% group_by(area) %>%
  summarise(n = unweighted(n()),
            D7 = survey_ratio(usamoderno,usametodo + nec_insat,
                             vartype = c("cv")))

D7_area <- as.data.frame(D7_area)
D7_area
D7_area$D7 <- D7_area$D7 * 100
D7_area$D7 <- round(D7_area$D7, digits = 1)
D7_area$area <- factor(x= D7_area$area,
                       levels=c("Urbano","Rural"))
D7_area

openxlsx::write.xlsx(D7_area, file = "COL/2015/4.D7/Output/Tablas/D7_area.xlsx")

####### Edad (Graf Informe Nacional 8.3.1 Pag.53)

# design.base.total
# design.base.unidas

D7_edad = design.base.unidas %>% group_by(edad) %>%
  summarise(n = unweighted(n()),
            D7 = survey_ratio(usamoderno,usametodo + nec_insat,
                             vartype = c("cv")))

D7_edad <- as.data.frame(D7_edad)
D7_edad$D7 <- D7_edad$D7 * 100
D7_edad$D7 <- round(D7_edad$D7, digits = 1)
D7_edad<-D7_edad[order(-D7_edad$D7),]
D7_edad$D7_cv <- D7_edad$D7_cv * 100
D7_edad$D7_cv <- round(D7_edad$D7_cv, digits = 1)
D7_edad

openxlsx::write.xlsx(D7_edad, file = "COL/2015/4.D7/Output/Tablas/D7_edad.xlsx")

####### Años estudio (Mujeres Unidas) Cuadro 8.4.1.1 Pag 56

# design.base.total
# design.base.unidas

D7_anoest = design.base.unidas %>% group_by(anoest) %>%
  summarise(n = unweighted(n()),
            D7 = survey_ratio(usamoderno,usametodo + nec_insat,
                             vartype = c("cv")))

D7_anoest <- as.data.frame(D7_anoest)
D7_anoest$D7 <- D7_anoest$D7 * 100
D7_anoest$D7 <- round(D7_anoest$D7, digits = 1)
table(D7_anoest$anoest)
D7_anoest$anoest <- factor(x= D7_anoest$anoest,
                           levels=c("Sin educación","Primaria",
                                    "Secundaria","Superior"))
D7_anoest

openxlsx::write.xlsx(D7_anoest, file = "COL/2015/4.D7/Output/Tablas/D7_anoest.xlsx")

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

openxlsx::write.xlsx(D7_etnia, file = "COL/2015/4.D7/Output/Tablas/D7_etnia.xlsx")

####### Discapacidad

D7_discap = design.base.unidas %>% group_by(discapacidad) %>%
  summarise(n = unweighted(n()),
            D7 = survey_ratio(usamoderno,usametodo + nec_insat,
                             vartype = c("cv")))

D7_discap <- as.data.frame(D7_discap)
D7_discap$D7 <- D7_discap$D7 * 100
D7_discap$D7 <- round(D7_discap$D7, digits = 1)
D7_discap

openxlsx::write.xlsx(D7_discap, file = "COL/2015/4.D7/Output/Tablas/D7_Discp.xlsx")

##### Resultados desagregaciones covariables

D7_nal
D7_area
D7_anoest
D7_etnia
D7_edad
D7_discap
D7_Depto

########################### Por Area y Escolaridad ###############################

## Usa métodos de Planificación NI

D7_area_anoest = design.base.unidas %>% group_by(area, anoest) %>%
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

openxlsx::write.xlsx(D7_area_anoest, file = "COL/2015/4.D7/Output/Tablas/D7_area_escolaridad.xlsx")


########################### Por Depto y Edad ###############################

## Usa métodos de Planificación NI

D7_depto_edad = design.base.unidas %>% group_by(Depto, edad) %>%
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

openxlsx::write.xlsx(D7_depto_edad, file = "COL/2015/4.D7/Output/Tablas/D7_Depto_edad.xlsx")


########################### Por Depto y Edad ###############################

## Usa métodos de Planificación D7


D7_mpio = design.base.unidas %>% group_by(mpio) %>%
  summarise(n = unweighted(n()),
            D7 = survey_ratio(usamoderno,usametodo + nec_insat,
                             vartype = c("cv")))

D7_mpio
D7_mpio$D7 <- D7_mpio$D7 * 100
D7_mpio$D7 <- round(D7_mpio$D7, digits = 1)
D7_mpio$D7_cv <- D7_mpio$D7_cv * 100
D7_mpio$D7_cv <- round(D7_mpio$D7_cv, digits = 1)
D7_mpio    <-D7_mpio[order(-D7_mpio$D7_cv),]
D7_mpio

openxlsx::write.xlsx(D7_mpio, file = "COL/2015/4.D7/Output/Tablas/D7_mpio.xlsx")

