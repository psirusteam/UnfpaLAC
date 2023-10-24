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

EndesMEF = readRDS("PER/2019/1.D6/Data/ENDESMEF_Estima.rds")

######################################################################################################################################################


#### Explorando

table(EndesMEF$area, useNA = "a")
table(EndesMEF$edad, useNA = "a")
table(EndesMEF$etnia, useNA = "a")
table(EndesMEF$anoest, useNA = "a")
table(EndesMEF$discapacidad, useNA = "a")
table(EndesMEF$escolaridad, useNA = "a")
EndesMEF$escolaridad[is.na(EndesMEF$escolaridad)] <- "Inicial"
table(EndesMEF$escolaridad, useNA = "a")
table(EndesMEF$unida, useNA = "a")
table(EndesMEF$tipodemetodo, useNA = "a")

##### Adecuando los datos

names(EndesMEF)

#EndesMEF<-readRDS("PER/2019/1.D6/Data/base_estimaciones.rds")

EndesMEF <- EndesMEF %>%
  transmute(
    mpio = mpio, 
    Depto = Depto,
    Provincia = Provincia,
    usametodo  = usametodo,
    usamoderno = usamoderno,
    nec_insat  = nec_insat,
    
    
    area = case_when(area == "Urbana" ~ "Urbano", 
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
    
    etnia = case_when(etnia == 1 ~  "Indigena",
                      etnia == 2 ~  "Afrodescendiente",
                             TRUE ~  "Otro"
    ),
    
    anoest = case_when(
      escolaridad == 0    ~ "Sin educación",  # NS/NR
      escolaridad == 1    ~ "Primaria",  # NS/NR
      escolaridad == 2    ~ "Secundaria",  # NS/NR
      escolaridad == 3    ~ "Superior",  # NS/NR
      TRUE ~ "Error"
                                           
    ),
    
    discapacidad = case_when(
      discapacidad == 1 ~ "Dispacitado",
      discapacidad == 0 ~ "No Discapacitado"
      
    ),
    
    unida = case_when(unida == 1 ~ "Unida", 
                      unida == 2 ~ "No unida"
    ),
    
    tipodemetodo = tipodemetodo,
    upm = upm,
    estrato = estrato,
    fexp = fexp,
    Nec_ins_pf_T = Nec_ins_pf_T,
    Nec_sat_pf_T = Nec_sat_pf_T,
    Falla_met = Falla_met
    
  )

saveRDS(EndesMEF,"PER/2019/1.D6/Data/base_estimaciones.rds")
saveRDS(EndesMEF,"PER/2019/2.D6m/Data/base_estimaciones.rds")
saveRDS(EndesMEF,"PER/2019/3.NI/Data/base_estimaciones.rds")
saveRDS(EndesMEF,"PER/2019/4.D7/Data/base_estimaciones.rds")

####

table(EndesMEF$area, useNA = "a")
table(EndesMEF$edad, useNA = "a")
table(EndesMEF$etnia, useNA = "a")
table(EndesMEF$anoest, useNA = "a")
table(EndesMEF$discapacidad, useNA = "a")

# Creación objetivo diseño de muestreo complejo para unidas y total

#### Unidas

EndesMEF_Unidas <- EndesMEF %>% 
  filter(unida == "Unida", edad != "12-14" )


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


#######################################
## Indicador D6: Uso Anticonceptivos ##
#######################################

####################
## Nivel Nacional ##
####################


#######  Informe nacional USO ACTUAL DE MÉTODOS ANTICONCEPTIVOS 
#######                   ENTRE LAS MUJERES ACTUALMENTE UNIDAS

####### Cuadro anexo Excel Cap 004 4.2

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


####### Por Departamento  (Mujeres Unidas) Cuadro 8.4.1.2 Pag 58

####### Informe nacional USO ACTUAL DE MÉTODOS ANTICONCEPTIVOS 
#######                   ENTRE LAS MUJERES ACTUALMENTE UNIDAS

### 
####### Por departamento Cuadro anexo Excel Cap 004 4.4

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

openxlsx::write.xlsx(D6_Depto, file = "PER/2019/1.D6/Output/Tablas/D6_Depto.xlsx")


####### Informe nacional USO ACTUAL DE MÉTODOS ANTICONCEPTIVOS 
#######                   ENTRE LAS MUJERES ACTUALMENTE UNIDAS

### 
####### Por Area Cuadro anexo Excel Cap 004 4.4

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

openxlsx::write.xlsx(D6_area, file = "PER/2019/1.D6/Output/Tablas/D6_area.xlsx")

####### Informe nacional USO ACTUAL DE MÉTODOS ANTICONCEPTIVOS 
#######                   ENTRE LAS MUJERES ACTUALMENTE UNIDAS

### 
####### Por edad Cuadro anexo Excel Cap 004 4.2

# design.base.total
# design.base.unidas

D6_edad = design.base.total %>% group_by(edad) %>%
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

openxlsx::write.xlsx(D6_edad, file = "PER/2019/1.D6/Output/Tablas/D6_edad.xlsx")

####### Informe nacional USO ACTUAL DE MÉTODOS ANTICONCEPTIVOS 
#######                   ENTRE LAS MUJERES ACTUALMENTE UNIDAS

### 
####### Por años de estudio Cuadro anexo Excel Cap 004 4.3

# design.base.total
# design.base.unidas

D6_anoest = design.base.unidas %>% group_by(anoest) %>%
  summarise(n = unweighted(n()),
            D6 = survey_mean(usametodo,
                             vartype = c("cv")))

D6_anoest <- as.data.frame(D6_anoest)
D6_anoest$D6 <- D6_anoest$D6 * 100
D6_anoest$D6 <- round(D6_anoest$D6, digits = 1)
table(D6_anoest$anoest)
D6_anoest$anoest <- factor(x= D6_anoest$anoest,
                           levels=c("Sin educación","Primaria",
                                    "Secundaria","Superior"))
D6_anoest

openxlsx::write.xlsx(D6_anoest, file = "PER/2019/1.D6/Output/Tablas/D6_anoest.xlsx")

####### Etnia

# design.base.total
# design.base.unidas

D6_etnia = design.base.total %>% group_by(etnia) %>%
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

openxlsx::write.xlsx(D6_etnia, file = "PER/2019/1.D6/Output/Tablas/D6_etnia.xlsx")

####### Discapacidad

D6_discap = design.base.total %>% group_by(discapacidad) %>%
  summarise(n = unweighted(n()),
            D6 = survey_mean(usametodo,
                             vartype = c("cv")))

D6_discap <- as.data.frame(D6_discap)
D6_discap$D6 <- D6_discap$D6 * 100
D6_discap$D6 <- round(D6_discap$D6, digits = 1)
D6_discap

openxlsx::write.xlsx(D6_discap, file = "PER/2019/1.D6/Output/Tablas/D6_Discp.xlsx")

##### Resultados desagregaciones covariables

D6_nal
D6_area
D6_anoest
D6_etnia
D6_edad
D6_discap
D6_Depto

########################### Por Area y Escolaridad ###############################

## Usa métodos de Planificación D6

D6_area_anoest = design.base.total %>% group_by(area, anoest) %>%
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

openxlsx::write.xlsx(D6_area_anoest, file = "PER/2019/1.D6/Output/Tablas/D6_area_escolaridad.xlsx")


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

openxlsx::write.xlsx(D6_depto_edad, file = "PER/2019/1.D6/Output/Tablas/D6_Depto_edad.xlsx")


########################### Por Depto y Edad ###############################

## Usa métodos de Planificación D6


D6_mpio = design.base.total %>% group_by(Provincia) %>%
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

openxlsx::write.xlsx(D6_mpio, file = "PER/2019/1.D6/Output/Tablas/D6_mpio.xlsx")

