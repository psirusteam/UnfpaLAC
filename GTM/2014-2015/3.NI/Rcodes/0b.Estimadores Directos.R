############################################################################################################################################
#############################################################################################################################################
# # 
# # Proceso: Estimación indicadores de planificación
# #              familiar utilizando ENDES 2021
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

Ensmi = readRDS("GTM/2014-2015/3.NI/Data/ENSMI_Estima.rds")

######################################################################################################################################################

names(Ensmi)

#### Explorando

table(Ensmi$area, useNA = "a")
table(Ensmi$edad, useNA = "a")
table(Ensmi$etnia, useNA = "a")
table(Ensmi$act_sex, useNA = "a")
Ensmi$act_sex[is.na(Ensmi$act_sex)] <- 0
table(Ensmi$escolaridad, useNA = "a")
table(Ensmi$unida, useNA = "a")
table(Ensmi$tipodemetodo, useNA = "a")

##### Adecuando los datos

names(Ensmi)

Ensmi <- Ensmi %>%
  transmute(
    Depto = Depto,
    usametodo  = usametodo,
    usamoderno = usamoderno,
    nec_insat  = nec_insat,
    
    
    area = case_when(area == 1 ~ "Urbano", 
                     TRUE ~ "Rural"),
    
    edad = case_when(edad %in% 0:14 ~  "12-14",      # 12 a 14
                     edad %in% 15:19 ~ "15-19",      # 15 a 19
                     edad %in% 20:24 ~ "20-24",      # 20 a 24
                     edad %in% 25:29 ~ "25-29",      # 25 a 29
                     edad %in% 30:34 ~ "30-34",      # 30 a 34
                     edad %in% 35:39 ~ "35-39",      # 35 a 39
                     edad %in% 40:44 ~ "40-44",      # 40 a 44
                     edad %in% 40:49 ~ "45-49",      # 45 a 49
                     TRUE ~ "Error"
    ),               
    
    etnia = case_when(etnia %in% c(1,4)   ~  "Indigena", # Indigena
                      etnia == 3          ~ "Afrodescendientes", #Afro
                      etnia %in% c(2,6,8) ~ "Otro",   # Otro
                      TRUE ~ "ERROR"
    ),
    
    # anoest = case_when(
    #   escolaridad == 0    ~ "Sin educación",  # NS/NR
    #   escolaridad == 1    ~ "Primaria",  # NS/NR
    #   escolaridad == 2    ~ "Secundaria",  # NS/NR
    #   escolaridad == 3    ~ "Superior",  # NS/NR
    #   TRUE ~ "Error"
    #                                        
    # ),
    
    escolaridad = as.character(escolaridad),
    
    unida = case_when(unida == 1 ~ "Unida", 
                      unida == 0 ~ "No unida"
    ),
    
    act_sexual = case_when(act_sex == 1 ~ "Activa", 
                           act_sex == 0 ~ "No activa"
    ),
    
    tipodemetodo = tipodemetodo,
    upm = upm,
    estrato = estrato,
    fexp = fexp,
    Nec_ins_pf_T = Nec_ins_pf_T,
    Nec_sat_pf_T = Nec_sat_pf_T,
    Falla_met = Falla_met
    
  )

####

table(Ensmi$area, useNA = "a")
table(Ensmi$edad, useNA = "a")
table(Ensmi$etnia, useNA = "a")
table(Ensmi$escolaridad, useNA = "a")
table(Ensmi$act_sexual, useNA = "a")

# Creación objetivo diseño de muestreo complejo para unidas y total

#### Unidas

Ensmi_Unidas <- Ensmi %>% 
  filter(unida == "Unida", edad != "12-14")#, act_sexual == "Activa")


options(survey.lonely.psu = "adjust") 

design.base.unidas = Ensmi_Unidas %>% as_survey_design(ids = upm,
                                                       strat = estrato, 
                                                       weights = fexp, nest = TRUE)

#### Total

Ensmi_edad <- Ensmi %>% 
  filter(edad != "12-14")

design.base.total = Ensmi_edad %>% as_survey_design(ids = upm,
                                                    strat = estrato, 
                                                    weights = fexp, nest = TRUE)

# Flags

nflag = 100
cvflag = 30
nefflag = 68
dfflag = 10
yflag = 100

#####################


#######################################
## Indicador NI: Uso Anticonceptivos ##
#######################################

####################
## Nivel Nacional ##
####################    
####### Necesidad y demanda de planificación familiar entre las mujeres casadas o unidas
#######                    Cuadro 7.13a   Pag 169


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


####### Por Departamento  Cuadro 7.13b Pag 170
####### Necesidad y demanda de planificación familiar entre las mujeres casadas o unidas 

## Usa métodos de Planificación NI

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
NI_Depto    <-NI_Depto[order(-NI_Depto$NI),]
NI_Depto

openxlsx::write.xlsx(NI_Depto, file = "GTM/2014-2015/3.NI/Output/Tablas/NI_Depto.xlsx")


####### Area (Mujeres Unidas)  Cuadro 7.13b Pag 170

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

openxlsx::write.xlsx(NI_area, file = "GTM/2014-2015/3.NI/Output/Tablas/NI_area.xlsx")

####### Edad (Cuadro 7.13a  Pag. 169
####### Necesidad y demanda de planificación familiar entre las mujeres casadas o unidas 
####### 

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

openxlsx::write.xlsx(NI_edad, file = "GTM/2014-2015/3.NI/Output/Tablas/NI_edad.xlsx")

####### Escolaridad (Cuadro 7.13a  Pag. 169
####### Necesidad y demanda de planificación familiar entre las mujeres casadas o unidas 
####### 

# design.base.total
# design.base.unidas

NI_anoest = design.base.unidas %>% group_by(escolaridad) %>%
  summarise(n = unweighted(n()),
            NI = survey_mean(nec_insat,
                             vartype = c("cv")))

NI_anoest <- as.data.frame(NI_anoest)
NI_anoest$NI <- NI_anoest$NI * 100
NI_anoest$NI <- round(NI_anoest$NI, digits = 1)
table(NI_anoest$escolaridad)
NI_anoest$escolaridad <- factor(x= NI_anoest$escolaridad,
                                levels=c("Sin educación","Primaria completa",
                                         "Primaria incompleta",
                                         "Secundaria","Superior"))
NI_anoest

openxlsx::write.xlsx(NI_anoest, file = "GTM/2014-2015/3.NI/Output/Tablas/NI_anoest.xlsx")

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

openxlsx::write.xlsx(NI_etnia, file = "GTM/2014-2015/3.NI/Output/Tablas/NI_etnia.xlsx")

# ####### Discapacidad
# 
# NI_discap = design.base.total %>% group_by(discapacidad) %>%
#   summarise(n = unweighted(n()),
#             NI = survey_mean(nec_insat,
#                              vartype = c("cv")))
# 
# NI_discap <- as.data.frame(NI_discap)
# NI_discap$NI <- NI_discap$NI * 100
# NI_discap$NI <- round(NI_discap$NI, digits = 1)
# NI_discap
# 
# openxlsx::write.xlsx(NI_discap, file = "GTM/2014-2015/3.NI/Output/Tablas/NI_Discp.xlsx")

##### Resultados desagregaciones covariables

NI_nal
NI_area
NI_anoest
NI_etnia
NI_edad
#NI_discap
NI_Depto

########################### Por Area y Escolaridad ###############################

## Usa métodos de Planificación NI

NI_area_anoest = design.base.unidas %>% group_by(area, escolaridad) %>%
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

openxlsx::write.xlsx(NI_area_anoest, file = "GTM/2014-2015/3.NI/Output/Tablas/NI_area_escolaridad.xlsx")


########################### Por Depto y Edad ###############################

## Usa métodos de Planificación NI

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

openxlsx::write.xlsx(NI_depto_edad, file = "GTM/2014-2015/3.NI/Output/Tablas/NI_Depto_edad.xlsx")


########################### Por Municipio ###############################

## Usa métodos de Planificación NI

# 
# NI_mpio = design.base.total %>% group_by(mpio) %>%
#   summarise(n = unweighted(n()),
#             NI = survey_mean(nec_insat,
#                              vartype = c("cv")))
# 
# NI_mpio
# NI_mpio$NI <- NI_mpio$NI * 100
# NI_mpio$NI <- round(NI_mpio$NI, digits = 1)
# NI_mpio$NI_cv <- NI_mpio$NI_cv * 100
# NI_mpio$NI_cv <- round(NI_mpio$NI_cv, digits = 1)
# #NI_mpio    <-NI_mpio[order(-NI_mpio$NI_cv),]
# NI_mpio
# 
# openxlsx::write.xlsx(NI_mpio, file = "GTM/2014-2015/3.NI/Output/Tablas/NI_mpio.xlsx")

