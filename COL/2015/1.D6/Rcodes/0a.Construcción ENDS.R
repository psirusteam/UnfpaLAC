############################################################################################################################################
#############################################################################################################################################
# #
# # Proceso: Construcción BAse ENDS Completa (Todos los modulos)
# #
# #
# # Historia Archivo:
# #   Creation : 22/09/2022
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

######################################################################################################################################################
###                             Limpiando la memoria
######################################################################################################################################################

rm(list = ls(all = T))
gc()


## Función que examina si un paquete se encuentra instalado, si lo está sigue derecho, si no está instalado, lo instala

paquetes <-
  c("readr",
    "readxl",
    "survey",
    "openxlsx",
    "dplyr",
    "foreign",
    "srvyr")
has <- paquetes %in% rownames(installed.packages())
if (any(!has))
  install.packages(paquetes [!has])


#############################################################################################################################################
##                                    Cargando Librerias
#############################################################################################################################################
library(readr)
library(readxl)
library(openxlsx)
library(foreign)
library(tidyverse)
library(survey)
library(haven)
library(srvyr)
library(stringr)
library(data.table)
library(TeachingSampling)

###############################################################################################################

######################################################################################################################################################
##                                    Cargando Insumos
######################################################################################################################################################


#### Cargando los modulos de la encuesta ENDS 2015 Stata

# Ejecutar una sola vez y luego comentar

# data_COIR71DT = read_dta("COL/2015/1.D6/Data/Modulos/COIR71FL.DTA")
# saveRDS(data_COIR71DT,"COL/2015/1.D6/Data/COIR71FL.rds")
# 
# data_COPR71DT = read_dta("COL/2015/1.D6/Data/Modulos/COPR71FL.DTA")
# saveRDS(data_COPR71DT, "COL/2015/1.D6/Data/COPR71FL.rds")

#################################
#################################
#################################
## Cargando Insumo RDS

data_COIR71DT<-readRDS("COL/2015/1.D6/Data/COIR71FL.rds") # Individual
data_COPR71DT<-readRDS( "COL/2015/1.D6/Data/COPR71FL.rds") # Persona


##################################################################################################################################################


### Identificación variables 

#    Variable                       

# Departaento (shdepto/shdepto2)                          
# Area  v025                                                                       
# Municipio                                                    
# Sexo  N/A                                                
# Edad  (v012)                            
# Etnia (v131)                                                   
# discapacidad N/A             
# Años Estudio (hv108/sh17a/sh17b)  
#
#                    Indicadores estudio D6, D6m, NI
#                                               
#    Variable                          Modulo            
# D6 v301//302/304/307                                            
# D6m                                                    
# NI V626                                                          
# "Unida" V501                                                                  
# "Activa" V536                                                            
#                                             
#                Variables del diseño muestral              
# 
#   Variable                          Modulo       
# upm                 (hv021)                                
# estrato             (hv023)                               
# factor de expansión (hv005)     
###################################################################

####### variables identificadas para el estudio

names_var = c("hhid","hv000","hv001","hv002","hv003","hv004","hv108",
              "hv005","shdepto","shmunip","caseid","v000",
              "v001","v002","v003","v004","v005","v025","v012",
              "v131","v301", "v302", "v312","v313","v501","v502","v536","v626","v626a",
              "hhid", "hvidx","hv000","hv001","hv002","hv003", "hv106",
              "hv004","hv010","hv021","hv023","hv005","ha1","sh09","hv104",
              "sh53a","sh53b","sh53c","sh53d","sh53e","sh53f","sh53g",
              "sh53h","sh53i")

#Nombres a emparejar en los datos
names_var = tolower(names_var)

## Variables para usar en las base de Individual Record (IR) y Person Record (PR)
names.use.IR = names(data_COIR71DT)[(names(data_COIR71DT) %in% names_var)]
names.use.PR = names(data_COPR71DT)[(names(data_COPR71DT) %in% names_var)]

######### Definiendo base para estandarización de datos

Data_ENDS_IND = data_COIR71DT %>% as.data.table() %>% .[,..names.use.IR]
Data_ENDS_PER = data_COPR71DT %>% as.data.table() %>% .[,..names.use.PR]
names(Data_ENDS_IND)
names(Data_ENDS_PER)


######### Creación del id o llave para unir las bases

## id para la unión
Data_ENDS_IND = Data_ENDS_IND %>% .[,id := gsub(" ", "", caseid, fixed = TRUE),]
names(Data_ENDS_IND)

## id para el merge
Data_ENDS_PER = Data_ENDS_PER %>% .[,a := paste0(hhid,hvidx),] %>% 
  .[,id := gsub(" ", "", a, fixed = TRUE),]


#### Creación Covariables interés

##area

table(Data_ENDS_IND$v025, useNA = "a")
labelled::generate_dictionary(Data_ENDS_IND %>%
                                select(v025))

Data_ENDS_IND$area <- ifelse(Data_ENDS_IND$v025 == 1, 1,0) 
table(Data_ENDS_IND$area, useNA = "a")

## Edad

table(Data_ENDS_IND$v012, useNA = "a")
labelled::generate_dictionary(Data_ENDS_IND %>%
                                select(v012))

Data_ENDS_IND$edad <- Data_ENDS_IND$v012
table(Data_ENDS_IND$edad, useNA = "a")


## Etnia 

table(Data_ENDS_IND$v131, useNA = "a")
labelled::generate_dictionary(Data_ENDS_IND %>%
                                select(v131))

Data_ENDS_IND$etnia = Data_ENDS_IND$v131
table(Data_ENDS_IND$etnia, useNA = "a")

## Años de estudio 

table(Data_ENDS_PER$hv108, useNA = "a")
labelled::generate_dictionary(Data_ENDS_PER %>%
                                select(hv108))

Data_ENDS_PER$anoest <- Data_ENDS_PER$hv108
table(Data_ENDS_PER$anoest, useNA = "a")

############### Construcción de indicadores

## Estado Civil/Conyugal


table(Data_ENDS_IND$v501, useNA = "a")
labelled::generate_dictionary(Data_ENDS_IND %>%
                                select(v501))
### Union2
Data_ENDS_IND$unida2 <- ifelse(Data_ENDS_IND$v502== 1,1,0)

table(Data_ENDS_IND$unida2, useNA = "a")

### Unida
Data_ENDS_IND$unida <- ifelse(Data_ENDS_IND$v501== 1,1,
                              ifelse(Data_ENDS_IND$v501== 2, 1,0))
table(Data_ENDS_IND$unida, useNA = "a")

## Mujeres sexualmente activas

table(Data_ENDS_IND$v536, useNA = "a")
labelled::generate_dictionary(Data_ENDS_IND %>%
                                select(v536))

Data_ENDS_IND$act.sex = ifelse(Data_ENDS_IND$v536 == 1,1,0)
table(Data_ENDS_IND$act.sex, useNA = "a")                   


# Incapacidad

## Discapacidad 

Data_ENDS_PER = Data_ENDS_PER %>%
  mutate(
    dis.oir = ifelse(sh53a == 1, 1, 0),
    dis.hablar = ifelse(sh53b == 1, 1, 0),
    dis.ver = ifelse(sh53c == 1, 1, 0), 
    dis.moverse = ifelse(sh53d == 1, 1, 0),
    dis.sostener = ifelse(sh53e == 1, 1, 0),
    dis.entender = ifelse(sh53f == 1, 1, 0),
    dis.cuidarse = ifelse(sh53f == 1, 1, 0),
    dis.relacionarse = ifelse(sh53h == 1, 1, 0)
  ) %>%
  rowwise() %>%
  mutate(
    discapacidad = max(
      dis.oir,
      dis.hablar,
      dis.ver,
      dis.moverse,
      dis.sostener,
      dis.entender,
      dis.cuidarse,
      dis.relacionarse
    )
    
  )

table(Data_ENDS_PER$discapacidad, useNA = "a")

Data_ENDS_PER$discapacidad[is.na(Data_ENDS_PER$discapacidad)] <- 0

table(Data_ENDS_PER$discapacidad, useNA = "a")


########################### Construcción de Indicadores de interés

## Uso metodo anticonceptivos

## categorias metodo

table(Data_ENDS_IND$v312,useNA = "a")

Data_ENDS_IND$usoM <- Data_ENDS_IND$v312 

Data_ENDS_IND$tipodemetodo <- Data_ENDS_IND$v312

## Usametodo

table(Data_ENDS_IND$v313,useNA = "a")
labelled::generate_dictionary(Data_ENDS_IND %>%
                                select(v313))

Data_ENDS_IND$usametodo     = ifelse(Data_ENDS_IND$v313%in%c(1:3),1,0)
table(Data_ENDS_IND$usametodo,useNA = "a")

## Usamoderno

Data_ENDS_IND$usamoderno = ifelse(Data_ENDS_IND$v313== 3,1,0)
table(Data_ENDS_IND$usamoderno, useNA = "a")


## Necesidad insatisfecha de planificacion familiar Definición 2

table(Data_ENDS_IND$v626, useNA = "a")
labelled::generate_dictionary(Data_ENDS_IND %>%
                                select(v626))

Data_ENDS_IND$v626[is.na(Data_ENDS_IND$v626)] <- 99
table(Data_ENDS_IND$v626, useNA = "a")
Data_ENDS_IND <- Data_ENDS_IND %>% 
  filter(!v626 == 99)
table(Data_ENDS_IND$v626, useNA = "a")

Data_ENDS_IND$Nec_ins_pf_E = ifelse(Data_ENDS_IND$v626== 1,1,0)
Data_ENDS_IND$Nec_ins_pf_L = ifelse(Data_ENDS_IND$v626 == 2,1,0)
Data_ENDS_IND$Nec_ins_pf_T = Data_ENDS_IND$Nec_ins_pf_E + Data_ENDS_IND$Nec_ins_pf_L
table(Data_ENDS_IND$Nec_ins_pf_T, useNA = "a")


## Necesidad insatisfecha de planificacion familiar Definición 3

table(Data_ENDS_IND$v626a, useNA = "a")
labelled::generate_dictionary(Data_ENDS_IND %>%
                                select(v626a))

Data_ENDS_IND$v626a[is.na(Data_ENDS_IND$v626a)] <- 99
table(Data_ENDS_IND$v626a, useNA = "a")
Data_ENDS_IND <- Data_ENDS_IND %>% 
  filter(!v626a == 99)
table(Data_ENDS_IND$v626a, useNA = "a")

Data_ENDS_IND$Nec_ins_pf_E3 = ifelse(Data_ENDS_IND$v626a== 1,1,0)
Data_ENDS_IND$Nec_ins_pf_L3 = ifelse(Data_ENDS_IND$v626a == 2,1,0)
Data_ENDS_IND$Nec_ins_pf_T3 = Data_ENDS_IND$Nec_ins_pf_E3 + 
  Data_ENDS_IND$Nec_ins_pf_L3
table(Data_ENDS_IND$Nec_ins_pf_T3, useNA = "a")


## Necesidad satisfecha de planificacion familiar

Data_ENDS_IND$Nec_sat_pf_E = ifelse(Data_ENDS_IND$v626== 3 ,1,0)
Data_ENDS_IND$Nec_sat_pf_L = ifelse(Data_ENDS_IND$v626 == 4 ,1,0)
Data_ENDS_IND$Nec_sat_pf_T = Data_ENDS_IND$Nec_sat_pf_E + Data_ENDS_IND$Nec_sat_pf_L
table(Data_ENDS_IND$Nec_sat_pf_T, useNA = "a")

########## Demanda de métodos Anticonceptivos

##### Demanda de métodos

Data_ENDS_IND$demanda_metod = ifelse(Data_ENDS_IND$casada== 1 ,1, # mujeres cadasas
                                     ifelse(Data_ENDS_IND$v626a %in% c(1:4),1,0)) # mujeres en demanda

table(Data_ENDS_IND$Nec_sat_pf_T, useNA = "a")


## Falla del método

Data_ENDS_IND$Falla_met = ifelse(Data_ENDS_IND$v626%in%c(5,6),1,0)
table(Data_ENDS_IND$Falla_met, useNA = "a")

##############################################
## Variables para el calculo de indicadores ##
##############################################

## D6
table(Data_ENDS_IND$usametodo, useNA = "a")
Data_ENDS_IND$D6 <- ifelse(Data_ENDS_IND$usametodo == 1, 1,0)
table(Data_ENDS_IND$D6, useNA = "a")

## D6m
table(Data_ENDS_IND$usamoderno, useNA = "a")
Data_ENDS_IND$D6m <- ifelse(Data_ENDS_IND$usamoderno == 1, 1,0)
table(Data_ENDS_IND$D6m, useNA = "a")

## NI Definición 2
table(Data_ENDS_IND$Nec_ins_pf_T, useNA = "a")
Data_ENDS_IND$NI  <- ifelse(Data_ENDS_IND$Nec_ins_pf_T == 1,1,0)
table(Data_ENDS_IND$NI, useNA = "a")

## NI definición 3
table(Data_ENDS_IND$Nec_ins_pf_T3, useNA = "a")
Data_ENDS_IND$NI3  <- ifelse(Data_ENDS_IND$Nec_ins_pf_T3 == 1,1,0)
table(Data_ENDS_IND$NI3, useNA = "a")


##############################################
## Variables diseño                         ##
##############################################

## Ajuste ponderador

# Total de mujeres en edad fertil ## 13126296


# Considerando solo a las mujeres de 15-49 pero incluyendo a las demas pues poseen el ponderador V005

labelled::generate_dictionary(Data_ENDS_IND %>%
                                select(v005))

Data_ENDS_IND$fexp = Data_ENDS_IND$v005*13126296/sum(Data_ENDS_IND$v005)
table(Data_ENDS_IND$fexp, useNA = "a")

### Estrato
table(Data_ENDS_PER$hv023,useNA = "a")
labelled::generate_dictionary(Data_ENDS_PER %>%
                                select(hv023))

Data_ENDS_PER$estrato = Data_ENDS_PER$hv023
table(Data_ENDS_PER$estrato,useNA = "a")

### upm
table(Data_ENDS_PER$hv021,useNA = "a")
labelled::generate_dictionary(Data_ENDS_PER %>%
                                select(hv021))

Data_ENDS_PER$upm = Data_ENDS_PER$hv021
table(Data_ENDS_PER$upm,useNA = "a")

####### Uniendo las bases

ENDS15 = left_join(Data_ENDS_IND, Data_ENDS_PER
                   , by = "id")

###################
###################################

### Verificación Depto y Municipio

### Depto
table(ENDS15$shdepto, useNA = "a")
labelled::generate_dictionary(ENDS15 %>%
                                select(shdepto))

##### Adecuación Depto

ENDS15 <- ENDS15 %>%
  mutate(
    Depto = case_when(
      shdepto == 5 ~  "Antioquia" ,
      shdepto == 8 ~  "Atlántico",
      shdepto == 11 ~ "Bogotá",
      shdepto == 13 ~ "Bolivar" ,
      shdepto == 15 ~ "Boyacá"  ,
      shdepto == 17 ~ "Caldas" ,
      shdepto == 18 ~ "Caquetá" ,
      shdepto == 19 ~ "Cauca" ,
      shdepto == 20 ~ "Cesar" ,
      shdepto == 23 ~ "Córdoba",
      shdepto == 25 ~ "Cundinamarca" ,
      shdepto == 27 ~ "Chocó" ,
      shdepto == 41 ~ "Huila" ,
      shdepto == 44 ~ "La Guajira" ,
      shdepto == 47 ~ "Magdalena" ,
      shdepto == 50 ~ "Meta" ,
      shdepto == 52 ~ "Nariño" ,
      shdepto == 54 ~ "Norte de Santander",
      shdepto == 63 ~ "Quindío" ,
      shdepto == 66 ~ "Risaralda",
      shdepto == 68 ~ "Santander" ,
      shdepto == 70 ~ "Sucre",
      shdepto == 73 ~ "Tolima" ,
      shdepto == 76 ~ "Valle del Cauca" ,
      shdepto == 81 ~ "Arauca" ,
      shdepto == 85 ~ "Casanare" ,
      shdepto == 86 ~ "Putumayo" ,
      shdepto == 88 ~ "San Andrés" ,
      shdepto == 91 ~ "Amazonas" ,
      shdepto == 94 ~ "Guainía",
      shdepto == 95 ~ "Guaviare" ,
      shdepto == 97 ~ "Vaupés",
      shdepto == 99 ~ "Vichada"
    ),
    shdepto = str_pad(
      string = shdepto,
      width = 2,
      pad = "0"
    ))

table(ENDS15$Depto, useNA = "a")
ENDS15$cod_depto <- ENDS15$shdepto

### Verificando variables
names(ENDS15)


#############

#### Mpio
table(ENDS15$shmunip, useNA = "a")
labelled::generate_dictionary(ENDS15 %>%
                                select(shmunip))

ENDS15 <- ENDS15 %>%
  mutate(
    cod_mpio = str_pad(
      string = shmunip,
      width = 3,
      pad = "0"
    ))

table(ENDS15$cod_mpio, useNA = "a")

##### creación Municipio

ENDS15$municipio<- with(ENDS15, paste0(cod_depto,cod_mpio))

###### Filtrando por mujeres en edad fertil 15-49
###### Eliminando edades 13 y 14 años

table(ENDS15$hv104, useNA = "a") ## sexo male = 1, 
##      female = 2  

table(ENDS15$edad, useNA = "a")


##### Seleccionando las variables que se requieren para la encuesta

ENDS <- ENDS15 %>%
  select(upm, estrato, fexp, 
         municipio, area, anoest, etnia, 
         edad, unida, discapacidad,
         D6, D6m, NI3,)

colnames(ENDS) <- c("upm","estrato", "fexp",
                    "mpio", "area", 
                    "anoest","etnia","edad", "unida", "discapacidad",
                    "D6", "D6m", "NI")

table(ENDS$edad, useNA = "a")

ENDS <-ENDS %>% 
  filter(edad >= 15)

table(ENDS15$edad, useNA = "a")

class(ENDS)
################

saveRDS(ENDS,"COL/2015/1.D6/Data/ENDS.rds")
saveRDS(ENDS,"COL/2015/2.D6m/Data/ENDS.rds")
saveRDS(ENDS,"COL/2015/3.NI/Data/ENDS.rds")


##### Seleccionando las variables que se requieren para validar las 
##### estimaciones directas

table(ENDS15$edad, useNA = "a")

ENDS_Estima <- ENDS15 %>%
  select(upm, estrato, fexp, 
         Nec_ins_pf_T, Nec_sat_pf_T, Falla_met,
         Depto, cod_depto, municipio,cod_mpio,
         area, hv106, etnia, 
         edad, unida, discapacidad, act.sex,
         D6, D6m, NI3, demanda_metod, tipodemetodo, usoM, v626a)

colnames(ENDS_Estima) <- c("upm","estrato", "fexp","Nec_ins_pf_T","Nec_sat_pf_T",
                           "Falla_met",
                           "Depto","cod_depto", "mpio", "cod_mpio", "area", 
                           "anoest","etnia","edad", "unida", "discapacidad", "sexual_act",
                           "usametodo", "usamoderno", "nec_insat", "demanda_metod", 
                           "tipodemetodo", "usoM", "NecIns_3")

################

saveRDS(ENDS_Estima,"COL/2015/1.D6/Data/ENDS_Estima.rds")
saveRDS(ENDS_Estima,"COL/2015/2.D6m/Data/ENDS_Estima.rds")
saveRDS(ENDS_Estima,"COL/2015/3.NI/Data/ENDS_Estima.rds")
saveRDS(ENDS_Estima,"COL/2015/4.D7/Data/ENDS_Estima.rds")
saveRDS(ENDS15,"COL/2015/4.D7/Data/ENDS15.rds")


