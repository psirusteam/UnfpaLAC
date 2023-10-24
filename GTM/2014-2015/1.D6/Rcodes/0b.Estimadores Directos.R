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

Ensmi = readRDS("GTM/2014-2015/1.D6/Data/ENSMI_Estima.rds")

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
## Indicador D6: Uso Anticonceptivos ##
#######################################

####################
## Nivel Nacional ##
####################    Cuadro 7.3a   Pag 153


D6_nal = design.base.unidas %>% #group_by(dominio) %>%
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


####### Por Departamento  Cuadro 7.4b Pag 157
####### Uso actual de métodos anticonceptivos en mujeres casadas o unidas 

## Usa métodos de Planificación D6

# design.base.total
# design.base.unidas

D6_Depto = design.base.unidas %>% group_by(Depto) %>%
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

openxlsx::write.xlsx(D6_Depto, file = "GTM/2014-2015/1.D6/Output/Tablas/D6_Depto.xlsx")


####### Area (Mujeres Unidas) Cuadro 7.4b Pag 157

# design.base.total
# design.base.unidas

D6_area = design.base.unidas %>% group_by(area) %>%
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

openxlsx::write.xlsx(D6_area, file = "GTM/2014-2015/1.D6/Output/Tablas/D6_area.xlsx")

####### Edad (Cuadro 7.3a Uso actual de métodos anticonceptivos según edad en 
####### mujeres Pag.153)

# design.base.total
# design.base.unidas

D6_edad = design.base.unidas %>% group_by(edad) %>%
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

openxlsx::write.xlsx(D6_edad, file = "GTM/2014-2015/1.D6/Output/Tablas/D6_edad.xlsx")

####### Años estudio Cuadro 7.4a  Pag 155
####### Uso actual de métodos anticonceptivos en mujeres casadas o unidas

# design.base.total
# design.base.unidas

D6_anoest = design.base.unidas %>% group_by(escolaridad) %>%
  summarise(n = unweighted(n()),
            D6 = survey_mean(usametodo,
                             vartype = c("cv")))

D6_anoest <- as.data.frame(D6_anoest)
D6_anoest$D6 <- D6_anoest$D6 * 100
D6_anoest$D6 <- round(D6_anoest$D6, digits = 1)
table(D6_anoest$escolaridad)
D6_anoest$escolaridad <- factor(x= D6_anoest$escolaridad,
                                levels=c("Sin educación","Primaria completa",
                                         "Primaria incompleta",
                                         "Secundaria","Superior"))
D6_anoest

openxlsx::write.xlsx(D6_anoest, file = "GTM/2014-2015/1.D6/Output/Tablas/D6_anoest.xlsx")

####### Etnia

# design.base.total
# design.base.unidas

D6_etnia = design.base.unidas %>% group_by(etnia) %>%
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

openxlsx::write.xlsx(D6_etnia, file = "GTM/2014-2015/1.D6/Output/Tablas/D6_etnia.xlsx")

# ####### Discapacidad
# 
# D6_discap = design.base.total %>% group_by(discapacidad) %>%
#   summarise(n = unweighted(n()),
#             D6 = survey_mean(usametodo,
#                              vartype = c("cv")))
# 
# D6_discap <- as.data.frame(D6_discap)
# D6_discap$D6 <- D6_discap$D6 * 100
# D6_discap$D6 <- round(D6_discap$D6, digits = 1)
# D6_discap
# 
# openxlsx::write.xlsx(D6_discap, file = "GTM/2014-2015/1.D6/Output/Tablas/D6_Discp.xlsx")

##### Resultados desagregaciones covariables

D6_nal
D6_area
D6_anoest
D6_etnia
D6_edad
#D6_discap
D6_Depto

########################### Por Area y Escolaridad ###############################

## Usa métodos de Planificación D6

D6_area_anoest = design.base.total %>% group_by(area, escolaridad) %>%
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

openxlsx::write.xlsx(D6_area_anoest, file = "GTM/2014-2015/1.D6/Output/Tablas/D6_area_escolaridad.xlsx")


########################### Por Depto y Edad ###############################

## Usa métodos de Planificación D6

D6_depto_edad = design.base.total %>% group_by(Depto, edad) %>%
  summarise(n = unweighted(n()),
            D6 = survey_mean(usametodo,
                             vartype = c("cv")))

D6_depto_edad
D6_depto_edad$D6 <- D6_depto_edad$D6 * 100
D6_depto_edad$D6 <- round(D6_depto_edad$D6, digits = 1)
D6_depto_edad$D6_cv <- D6_depto_edad$D6_cv * 100
D6_depto_edad$D6_cv <- round(D6_depto_edad$D6_cv, digits = 1)
D6_depto_edad    <-D6_depto_edad[order(-D6_depto_edad$D6_cv),]
D6_depto_edad

openxlsx::write.xlsx(D6_depto_edad, file = "GTM/2014-2015/1.D6/Output/Tablas/D6_Depto_edad.xlsx")


########################### Por Municipio ###############################

## Usa métodos de Planificación D6

# 
# D6_mpio = design.base.total %>% group_by(mpio) %>%
#   summarise(n = unweighted(n()),
#             D6 = survey_mean(usametodo,
#                              vartype = c("cv")))
# 
# D6_mpio
# D6_mpio$D6 <- D6_mpio$D6 * 100
# D6_mpio$D6 <- round(D6_mpio$D6, digits = 1)
# D6_mpio$D6_cv <- D6_mpio$D6_cv * 100
# D6_mpio$D6_cv <- round(D6_mpio$D6_cv, digits = 1)
# #D6_mpio    <-D6_mpio[order(-D6_mpio$D6_cv),]
# D6_mpio
# 
# openxlsx::write.xlsx(D6_mpio, file = "GTM/2014-2015/1.D6/Output/Tablas/D6_mpio.xlsx")

