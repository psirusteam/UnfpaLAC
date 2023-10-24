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

Ensmi = readRDS("GTM/2014-2015/2.D6m/Data/ENSMI_Estima.rds")

######################################################################################################################################################


#### Explorando

table(Ensmi$area, useNA = "a")
table(Ensmi$edad, useNA = "a")
table(Ensmi$etnia, useNA = "a")
table(Ensmi$anoest, useNA = "a")
#table(Ensmi$discapacidad, useNA = "a")
table(Ensmi$escolaridad, useNA = "a")
Ensmi$escolaridad[is.na(Ensmi$escolaridad)] <- "Inicial"
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

# Creación objetivo diseño de muestreo complejo para unidas y total

#### Unidas

Ensmi_Unidas <- Ensmi %>% 
  filter(unida == "Unida", edad != "12-14" )


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
## Indicador D6m: Uso Anticonceptivos ##
#######################################

####################
## Nivel Nacional ##
####################    Cuadro 7.3a   Pag 153


D6m_nal = design.base.unidas %>% #group_by(dominio) %>%
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


####### Por Departamento  Cuadro 7.4b Pag 157
####### Uso actual de métodos anticonceptivos en mujeres casadas o unidas 

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

openxlsx::write.xlsx(D6m_Depto, file = "GTM/2014-2015/2.D6m/Output/Tablas/D6m_Depto.xlsx")


####### Area (Mujeres Unidas) Cuadro 7.4b Pag 157

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

openxlsx::write.xlsx(D6m_area, file = "GTM/2014-2015/2.D6m/Output/Tablas/D6m_area.xlsx")

####### Edad (Cuadro 7.3a Uso actual de métodos anticonceptivos según edad en 
####### mujeres Pag.153)

# design.base.total
# design.base.unidas

D6m_edad = design.base.unidas %>% group_by(edad) %>%
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

openxlsx::write.xlsx(D6m_edad, file = "GTM/2014-2015/2.D6m/Output/Tablas/D6m_edad.xlsx")

####### Años estudio Cuadro 7.4a  Pag 155
####### Uso actual de métodos anticonceptivos en mujeres casadas o unidas

# design.base.total
# design.base.unidas

D6m_anoest = design.base.unidas %>% group_by(escolaridad) %>%
  summarise(n = unweighted(n()),
            D6m = survey_mean(usamoderno,
                             vartype = c("cv")))

D6m_anoest <- as.data.frame(D6m_anoest)
D6m_anoest$D6m <- D6m_anoest$D6m * 100
D6m_anoest$D6m <- round(D6m_anoest$D6m, digits = 1)
table(D6m_anoest$escolaridad)
D6m_anoest$escolaridad <- factor(x= D6m_anoest$escolaridad,
                                levels=c("Sin educación","Primaria completa",
                                         "Primaria incompleta",
                                         "Secundaria","Superior"))
D6m_anoest

openxlsx::write.xlsx(D6m_anoest, file = "GTM/2014-2015/2.D6m/Output/Tablas/D6m_anoest.xlsx")

####### Etnia

# design.base.total
# design.base.unidas

D6m_etnia = design.base.unidas %>% group_by(etnia) %>%
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

openxlsx::write.xlsx(D6m_etnia, file = "GTM/2014-2015/2.D6m/Output/Tablas/D6m_etnia.xlsx")

# ####### Discapacidad
# 
# D6m_discap = design.base.total %>% group_by(discapacidad) %>%
#   summarise(n = unweighted(n()),
#             D6m = survey_mean(usamoderno,
#                              vartype = c("cv")))
# 
# D6m_discap <- as.data.frame(D6m_discap)
# D6m_discap$D6m <- D6m_discap$D6m * 100
# D6m_discap$D6m <- round(D6m_discap$D6m, digits = 1)
# D6m_discap
# 
# openxlsx::write.xlsx(D6m_discap, file = "GTM/2014-2015/2.D6m/Output/Tablas/D6m_Discp.xlsx")

##### Resultados desagregaciones covariables

D6m_nal
D6m_area
D6m_anoest
D6m_etnia
D6m_edad
#D6m_discap
D6m_Depto

########################### Por Area y Escolaridad ###############################

## Usa métodos de Planificación D6m

D6m_area_anoest = design.base.total %>% group_by(area, escolaridad) %>%
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

openxlsx::write.xlsx(D6m_area_anoest, file = "GTM/2014-2015/2.D6m/Output/Tablas/D6m_area_escolaridad.xlsx")


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

openxlsx::write.xlsx(D6m_depto_edad, file = "GTM/2014-2015/2.D6m/Output/Tablas/D6m_Depto_edad.xlsx")


########################### Por Municipio ###############################

## Usa métodos de Planificación D6m

# 
# D6m_mpio = design.base.total %>% group_by(mpio) %>%
#   summarise(n = unweighted(n()),
#             D6m = survey_mean(usamoderno,
#                              vartype = c("cv")))
# 
# D6m_mpio
# D6m_mpio$D6m <- D6m_mpio$D6m * 100
# D6m_mpio$D6m <- round(D6m_mpio$D6m, digits = 1)
# D6m_mpio$D6m_cv <- D6m_mpio$D6m_cv * 100
# D6m_mpio$D6m_cv <- round(D6m_mpio$D6m_cv, digits = 1)
# #D6m_mpio    <-D6m_mpio[order(-D6m_mpio$D6m_cv),]
# D6m_mpio
# 
# openxlsx::write.xlsx(D6m_mpio, file = "GTM/2014-2015/2.D6m/Output/Tablas/D6m_mpio.xlsx")

