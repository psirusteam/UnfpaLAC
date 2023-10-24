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

ENDS<-readRDS("COL/2015/2.D6m/Data/base_estimaciones.rds")

######################################################################################################################################################


#### Explorando

table(ENDS$area, useNA = "a")
table(ENDS$edad, useNA = "a")
table(ENDS$etnia, useNA = "a")
table(ENDS$anoest, useNA = "a")
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

###############################################################################
###############################################################################
#############################################################################
##                              ESTIMACIONES DIRECTAS                      ##
#############################################################################

####################
## Nivel Nacional ##
####################

# design.base.total
# design.base.unidas


D6m_nal = design.base.total %>% #group_by(dominio) %>%
  #D6m_nal = design.base.unidas %>% #group_by(dominio) %>%  
  summarise(n = unweighted(n()),
            D6m = survey_mean(usamoderno,
                             vartype = c("cv")))

D6m_nal
D6m_nal <- as.data.frame(D6m_nal)
D6m_nal$D6m <- D6m_nal$D6m * 100
D6m_nal$D6m <- round(D6m_nal$D6m, digits = 1)
D6m_nal$D6m_cv <- D6m_nal$D6m_cv * 100
D6m_nal$D6m_cv <- round(D6m_nal$D6m_cv, digits = 1)
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

D6m_Depto = design.base.unidas %>% group_by(Depto) %>%
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

openxlsx::write.xlsx(D6m_Depto, file = "COL/2015/2.D6m/Output/Tablas/D6m_Depto.xlsx")


####### Area (Mujeres Unidas) Cuadro 8.4.1.1 Pag 56

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

openxlsx::write.xlsx(D6m_area, file = "COL/2015/2.D6m/Output/Tablas/D6m_area.xlsx")

####### Edad (Graf Informe Nacional 8.3.1 Pag.53)

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

openxlsx::write.xlsx(D6m_edad, file = "COL/2015/2.D6m/Output/Tablas/D6m_edad.xlsx")

####### Años estudio (Mujeres Unidas) Cuadro 8.4.1.1 Pag 56

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

openxlsx::write.xlsx(D6m_anoest, file = "COL/2015/2.D6m/Output/Tablas/D6m_anoest.xlsx")

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

openxlsx::write.xlsx(D6m_etnia, file = "COL/2015/2.D6m/Output/Tablas/D6m_etnia.xlsx")

####### Discapacidad

D6m_discap = design.base.unidas %>% group_by(discapacidad) %>%
  summarise(n = unweighted(n()),
            D6m = survey_mean(usamoderno,
                             vartype = c("cv")))

D6m_discap <- as.data.frame(D6m_discap)
D6m_discap$D6m <- D6m_discap$D6m * 100
D6m_discap$D6m <- round(D6m_discap$D6m, digits = 1)
D6m_discap

openxlsx::write.xlsx(D6m_discap, file = "COL/2015/2.D6m/Output/Tablas/D6m_Discp.xlsx")

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

D6m_area_anoest = design.base.unidas %>% group_by(area, anoest) %>%
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

openxlsx::write.xlsx(D6m_area_anoest, file = "COL/2015/2.D6m/Output/Tablas/D6m_area_escolaridad.xlsx")


########################### Por Depto y Edad ###############################

## Usa métodos de Planificación D6m

D6m_depto_edad = design.base.unidas %>% group_by(Depto, edad) %>%
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

openxlsx::write.xlsx(D6m_depto_edad, file = "COL/2015/2.D6m/Output/Tablas/D6m_Depto_edad.xlsx")


########################### Por Depto y Edad ###############################

## Usa métodos de Planificación D6m


D6m_mpio = design.base.unidas %>% group_by(mpio) %>%
  summarise(n = unweighted(n()),
            D6m = survey_mean(usamoderno,
                             vartype = c("cv")))

D6m_mpio
D6m_mpio$D6m <- D6m_mpio$D6m * 100
D6m_mpio$D6m <- round(D6m_mpio$D6m, digits = 1)
D6m_mpio$D6m_cv <- D6m_mpio$D6m_cv * 100
D6m_mpio$D6m_cv <- round(D6m_mpio$D6m_cv, digits = 1)
D6m_mpio    <-D6m_mpio[order(-D6m_mpio$D6m_cv),]
D6m_mpio

openxlsx::write.xlsx(D6m_mpio, file = "COL/2015/2.D6m/Output/Tablas/D6m_mpio.xlsx")

