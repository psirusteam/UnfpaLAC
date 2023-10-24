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

Ensmi = readRDS("GTM/2014-2015/4.D7/Data/ENSMI_Estima.rds")

######################################################################################################################################################


#### Explorando

table(Ensmi$area, useNA = "a")
table(Ensmi$edad, useNA = "a")
table(Ensmi$etnia, useNA = "a")
table(Ensmi$anoest, useNA = "a")
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
## Indicador D7: Uso Anticonceptivos ##
#######################################

####################
## Nivel Nacional ##
####################    
####### Necesidad y demanda de planificación familiar entre las mujeres casadas o unidas
#######                    Cuadro 7.13a   Pag 169


D7_nal = design.base.unidas %>% #group_by(dominio) %>%
  cascade(n = unweighted(n()),
            D7 = survey_ratio(usamoderno,usametodo + nec_insat,
                             vartype = c("cv")))

D7_nal <- as.data.frame(D7_nal)
D7_nal$D7 <- D7_nal$D7 * 100
D7_nal$D7 <- round(D7_nal$D7, digits = 1)
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


####### Por Departamento  Cuadro 7.13b Pag 170
####### Necesidad y demanda de planificación familiar entre las mujeres casadas o unidas 

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

openxlsx::write.xlsx(D7_Depto, file = "GTM/2014-2015/4.D7/Output/Tablas/D7_Depto.xlsx")


####### Area (Mujeres Unidas)  Cuadro 7.13b Pag 170

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

openxlsx::write.xlsx(D7_area, file = "GTM/2014-2015/4.D7/Output/Tablas/D7_area.xlsx")

####### Edad (Cuadro 7.13a  Pag. 169
####### Necesidad y demanda de planificación familiar entre las mujeres casadas o unidas 
####### 

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

openxlsx::write.xlsx(D7_edad, file = "GTM/2014-2015/4.D7/Output/Tablas/D7_edad.xlsx")

####### Escolaridad (Cuadro 7.13a  Pag. 169
####### Necesidad y demanda de planificación familiar entre las mujeres casadas o unidas 
####### 

# design.base.total
# design.base.unidas

D7_anoest = design.base.unidas %>% group_by(escolaridad) %>%
  summarise(n = unweighted(n()),
            D7 = survey_ratio(usamoderno,usametodo + nec_insat,
                             vartype = c("cv")))

D7_anoest <- as.data.frame(D7_anoest)
D7_anoest$D7 <- D7_anoest$D7 * 100
D7_anoest$D7 <- round(D7_anoest$D7, digits = 1)
table(D7_anoest$escolaridad)
D7_anoest$escolaridad <- factor(x= D7_anoest$escolaridad,
                                levels=c("Sin educación","Primaria completa",
                                         "Primaria incompleta",
                                         "Secundaria","Superior"))
D7_anoest

openxlsx::write.xlsx(D7_anoest, file = "GTM/2014-2015/4.D7/Output/Tablas/D7_anoest.xlsx")

####### Etnia

# design.base.total
# design.base.unidas

D7_etnia = design.base.unidas %>% group_by(etnia) %>%
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

openxlsx::write.xlsx(D7_etnia, file = "GTM/2014-2015/4.D7/Output/Tablas/D7_etnia.xlsx")

# ####### Discapacidad
# 
# D7_discap = design.base.total %>% group_by(discapacidad) %>%
#   summarise(n = unweighted(n()),
#             D7 = survey_ratio(usamoderno,usametodo + nec_insat,
#                              vartype = c("cv")))
# 
# D7_discap <- as.data.frame(D7_discap)
# D7_discap$D7 <- D7_discap$D7 * 100
# D7_discap$D7 <- round(D7_discap$D7, digits = 1)
# D7_discap
# 
# openxlsx::write.xlsx(D7_discap, file = "GTM/2014-2015/4.D7/Output/Tablas/D7_Discp.xlsx")

##### Resultados desagregaciones covariables

D7_nal
D7_area
D7_anoest
D7_etnia
D7_edad
#D7_discap
D7_Depto

########################### Por Area y Escolaridad ###############################

## Usa métodos de Planificación NI

D7_area_anoest = design.base.unidas %>% group_by(area, escolaridad) %>%
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

openxlsx::write.xlsx(D7_area_anoest, file = "GTM/2014-2015/4.D7/Output/Tablas/D7_area_escolaridad.xlsx")


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

openxlsx::write.xlsx(D7_depto_edad, file = "GTM/2014-2015/4.D7/Output/Tablas/D7_Depto_edad.xlsx")


########################### Por Municipio ###############################

## Usa métodos de Planificación NI

# 
# D7_mpio = design.base.total %>% group_by(mpio) %>%
#   summarise(n = unweighted(n()),
#             D7 = survey_ratio(usamoderno,usametodo + nec_insat,
#                              vartype = c("cv")))
# 
# D7_mpio
# D7_mpio$D7 <- D7_mpio$D7 * 100
# D7_mpio$D7 <- round(D7_mpio$D7, digits = 1)
# D7_mpio$D7_cv <- D7_mpio$D7_cv * 100
# D7_mpio$D7_cv <- round(D7_mpio$D7_cv, digits = 1)
# #D7_mpio    <-D7_mpio[order(-D7_mpio$D7_cv),]
# D7_mpio
# 
# openxlsx::write.xlsx(D7_mpio, file = "GTM/2014-2015/4.D7/Output/Tablas/D7_mpio.xlsx")

