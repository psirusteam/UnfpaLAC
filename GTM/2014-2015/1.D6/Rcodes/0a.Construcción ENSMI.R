############################################################################################################################################
#############################################################################################################################################
# # 
# # Proceso: Construcción Base ENSMI Completa (Todos los módulos)
# #
# # 
# # Historia Archivo:
# #   Creation : 29/06/2022
# #
# #
# # Autor: Gabriel Nieto y Andrés Gutiérrez
# #  
# # Institution:  CEPAL             
# # 
# # Modification:   
#############################################################################################################################################

######################################################################################################################################################
###                             Limpiando la memoria 
######################################################################################################################################################

rm(list = ls(all=T))
gc()


#############################################################################################################################################
##                                    Verificando librerias e instalando las faltantes  
#############################################################################################################################################

## FunciÃ³n que examina si un paquete se encuentra instalado, si lo estÃ¡ sigue derecho, si no estÃ¡ instalado, lo instala

paquetes <- c("readr","readxl","faraway","wesanderson","dplyr","factoextra","corrplot","reshape2","reshape",
              "PerformanceAnalytics","foreign","plotly","FactoMineR","homals", "nnet", "tidyverse", "gapminder")
has <- paquetes %in% rownames(installed.packages())
if(any(!has)) install.packages(paquetes [!has])


#############################################################################################################################################
##                                    Cargando Librerias
#############################################################################################################################################
library(readr)
library(readxl)
library(openxlsx)
library(foreign)
library(tidyverse)
library(haven)

######################################################################################################################################################
##                                    Definiendo rutas de entradas y salidas
########################################################################################################################

dir()

###########################################################################################################################

######################################################################################################################################################
##                                    Cargando Insumos
######################################################################################################################################################

# 
# M1_GUBR71FL<-read_dta("GTM/2014-2015/1.D6/Data/Modulos/GUBR71DT/GUBR71FL.DTA")
# M2_GUHR71FL<-read_dta("GTM/2014-2015/1.D6/Data/Modulos/GUHR71DT/GUHR71FL.DTA")
# M3_GUIR71FL<-read_dta("GTM/2014-2015/1.D6/Data/Modulos/GUIR71DT/GUIR71FL.DTA")
# M4_GUKR71FL<-read_dta("GTM/2014-2015/1.D6/Data/Modulos/GUKR71DT/GUKR71FL.DTA")
# M5_GUMR71FL<-read_dta("GTM/2014-2015/1.D6/Data/Modulos/GUMR71DT/GUMR71FL.DTA")
# M6_GUPR71FL<-read_dta("GTM/2014-2015/1.D6/Data/Modulos/GUPR71DT/GUPR71FL.DTA")
# 
# #### Guardando archivos
# 
# Mod1_GUBR71FL<-saveRDS(M1_GUBR71FL, file= "GTM/2014-2015/1.D6/Data/GUBR71FL.rds")
# Mod2_hogar<-saveRDS(M2_GUHR71FL, file= "GTM/2014-2015/1.D6/Data/GUHR71FL.rds")
# Mod3_mujeres<-saveRDS(M3_GUIR71FL, file= "GTM/2014-2015/1.D6/Data/GUIR71FL.rds")
# Mod4_GUKR71FL<-saveRDS(M4_GUKR71FL, file= "GTM/2014-2015/1.D6/Data/GUKR71FL.rds")
# Mod5_GUMR71FL<-saveRDS(M5_GUMR71FL, file= "GTM/2014-2015/1.D6/Data/GUMR71FL.rds")
# Mod6_personas<-saveRDS(M6_GUPR71FL, file= "GTM/2014-2015/1.D6/Data/GUPR71FL.rds")

#### Cargando las bases las bases en RDS

#Mod1_GUBR71FL<-readRDS("GTM/2014-2015/1.D6/Data/GUBR71FL.rds")
Mod2_hogar<-readRDS("GTM/2014-2015/1.D6/Data/GUHR71FL.rds")
Mod3_mujeres<-readRDS("GTM/2014-2015/1.D6/Data/GUIR71FL.rds")
#Mod4_GUKR71FL<-readRDS("GTM/2014-2015/1.D6/Data/GUKR71FL.rds")
#Mod5_GUMR71FL<-readRDS("GTM/2014-2015/1.D6/Data/GUMR71FL.rds")
Mod6_personas<-readRDS("GTM/2014-2015/1.D6/Data/GUPR71FL.rds")
# 

##################################################################################################################################################


### Identificación variables 

#    Variable                       Modulo

# Departaento (shdepto/shdepto2)    Mod2_hogar                       
# Area  v025                        Mod3_mujeres                                               
# Municipio                                                    
# Sexo  N/A                                                
# Edad  (v012)                      Mod3_mujeres      
# Etnia (v131)                      Mod3_mujeres                             
# discapacidad N/A             
# Años Estudio (hv108/sh17a/sh17b)  Mod6_personas
#
#                    Indicadores estudio D6, D6m, NI
#                                               
#    Variable                          Modulo            
# D6 v301//302/304/307             Mod3_mujeres                               
# D6m                              Mod3_mujeres                      
# NI V626                          Mod3_mujeres                                
# "Unida" V501                                                                  
# "Activa" V536                                                            
#                                             
#                Variables del diseño muestral              
# 
#   Variable                          Modulo       
# upm                 (hv021)     Mod6_personas                           
# estrato             (hv023)     Mod6_personas                          
# factor de expansión (hv005)     Mod6_personas
###################################################################

hogar<-Mod2_hogar[,c("hhid","hv000","hv001","hv002","hv003","hv004",
                     "hv005","shdepto","shdepto2")]


mujeres<-Mod3_mujeres[,c("caseid","v000","v001","v002","v003","v004",
                         "v005","v025","v012","v131","v301", "v302", "v312",
                         "v313","v501","v536","v626", "v626a")]

personas<-Mod6_personas[,c("hhid", "hvidx","hv000","hv001","hv002","hv003","hv004",
                           "hv010","hv021","hv023","hv005","ha1","sh17a","sh17b","hv108","sheduc")]

#########

# Unión de mujeres con personas

names(Mod6_personas)
names(personas)
names(Mod3_mujeres)
names(mujeres)

# Creación de llaves en las bases de datos de personas y mujeres

personas$clustertemp <- sprintf("%05d", personas$hv001)
personas$hhtemp <- sprintf("%05d", personas$hv002)
personas$linetemp <- sprintf("%05d", personas$hvidx)

mujeres$clustertemp <- sprintf("%05d", mujeres$v001)
mujeres$hhtemp <- sprintf("%05d", mujeres$v002)
mujeres$linetemp <- sprintf("%05d", mujeres$v003)

personas$llave <- with(personas, paste0(clustertemp, hhtemp, linetemp))
length(unique(personas$llave))

mujeres$llave <- with(mujeres, paste0(clustertemp, hhtemp, linetemp))
length(unique(mujeres$llave))

Temp1 <- mujeres %>% 
  left_join(personas, by = "llave") 

# Creación de llaves en las bases de datos de mujeres y hogares

Temp1$clustertemp <- sprintf("%05d", Temp1$hv001)
Temp1$hhtemp <- sprintf("%05d", Temp1$hv002)
Temp1$llavehogar <- with(Temp1, paste0(clustertemp, hhtemp))
length(unique(Temp1$llavehogar))

hogar$clustertemp <- sprintf("%05d", hogar$hv001)
hogar$hhtemp <- sprintf("%05d", hogar$hv002)
hogar$llavehogar <- with(hogar, paste0(clustertemp, hhtemp))
length(unique(hogar$llavehogar))

Temp2 <- Temp1 %>% 
  left_join(hogar) 

table(Temp2$shdepto, useNA = "a")

### Validaciones


table(Temp2$shdepto, useNA = "a")
table(Temp2$v025, useNA = "a")
table(Temp2$v131, useNA = "a")
table(Temp2$sh17a, useNA = "a")
table(Temp2$sh17b, useNA = "a")
table(Temp2$v012, useNA = "a")


# ##### Creando y/o adecuando las variables de interes --------------------

ENSMI <- Temp2

ENSMI <- ENSMI %>%
  mutate(
    Depto = case_when(
      shdepto == 1 ~ "Guatemala" ,
      shdepto == 2 ~ "El Progreso",
      shdepto == 3 ~ "Sacatepequez",
      shdepto == 4 ~ "Chimaltenango" ,
      shdepto == 5 ~ "Escuintla"  ,
      shdepto == 6 ~ "Santa Rosa" ,
      shdepto == 7 ~ "Solola" ,
      shdepto == 8 ~ "Totonicapan" ,
      shdepto == 9 ~ "Quetzaltenango" ,
      shdepto == 10 ~ "Suchitepequez",
      shdepto == 11 ~ "Retalhuleu" ,
      shdepto == 12 ~ "San Marcos" ,
      shdepto == 13 ~ "huehuetenango" ,
      shdepto == 14 ~ "Quiche" ,
      shdepto == 15 ~ "Baja Verapaz" ,
      shdepto == 16 ~ "Alta Verapaz" ,
      shdepto == 17 ~ "Peten" ,
      shdepto == 18 ~  "Izabal",
      shdepto == 19 ~ "Zacapa" ,
      shdepto == 20 ~ "Chiquimula",
      shdepto == 21 ~ "Jalapa" ,
      shdepto == 22 ~ "Jutiapa" 
    ),
    shdepto = str_pad(
      string = shdepto,
      width = 2,
      pad = "0"
    ))

table(ENSMI$Depto, useNA = "a")
table(ENSMI$shdepto2, useNA = "a")

##area

table(ENSMI$v025, useNA = "a")
ENSMI$area <- ifelse(ENSMI$v025 == 1, 1,0) 
table(ENSMI$area, useNA = "a")

## Edad

table(ENSMI$v012, useNA = "a")
ENSMI$edad <- ENSMI$v012
table(ENSMI$edad, useNA = "a")


## Etnia 

table(ENSMI$v131, useNA = "a")
ENSMI$etnia = ENSMI$v131
table(ENSMI$etnia, useNA = "a")

## Años de estudio 

table(ENSMI$hv108, useNA = "a")
ENSMI$anoest <- ENSMI$hv108
table(ENSMI$anoest, useNA = "a")


### Escolaridad

table(ENSMI$sheduc, useNA = "a")



ENSMI$escolaridad<- ifelse(ENSMI$sheduc == 0, "Sin educación",
                           ifelse(ENSMI$sheduc == 1, "Primaria incompleta",
                                  ifelse(ENSMI$sheduc == 2, "Primaria completa",
                                         ifelse(ENSMI$sheduc == 3, "Secundaria",
                                                ifelse(ENSMI$sheduc == 4, "Secundaria",
                                                       ifelse(ENSMI$sheduc == 5, "Superior",
                                                              ifelse(ENSMI$sheduc == 6, "Sin educación",99)))))))

############### Construcción de indicadores

## Estado Civil/Conyugal


table(ENSMI$v501, useNA = "a")

### union

ENSMI$unida <- ifelse(ENSMI$v501== 1,1,
                      ifelse(ENSMI$v501== 2, 1,0))

table(ENSMI$unida, useNA = "a")


## Mujeres sexualmente activas

table(ENSMI$v536, useNA = "a")
ENSMI$act.sex = ifelse(ENSMI$v536 == 1,1,0)
table(ENSMI$act.sex, useNA = "a")                   


## Uso metodo anticonceptivos

## Usametodo

table(ENSMI$v313,useNA = "a")
ENSMI$usametodo     = ifelse(ENSMI$v313%in%c(1:3),1,0)
table(ENSMI$usametodo,useNA = "a")

## Usamoderno

ENSMI$usamoderno = ifelse(ENSMI$v313== 3,1,0)
table(ENSMI$usamoderno, useNA = "a")


## Necesidad insatisfecha de planificacion familiar

table(ENSMI$v626, useNA = "a")
table(ENSMI$v626a, useNA = "a")
ENSMI$v626a[is.na(ENSMI$v626a)] <- 99
table(ENSMI$v626a, useNA = "a")
ENSMI <- ENSMI %>% 
  filter(!v626a == 99)
table(ENSMI$v626a, useNA = "a")

ENSMI$Nec_ins_pf_E = ifelse(ENSMI$v626a== 1,1,0)
ENSMI$Nec_ins_pf_L = ifelse(ENSMI$v626a == 2,1,0)
ENSMI$Nec_ins_pf_T = ENSMI$Nec_ins_pf_E + ENSMI$Nec_ins_pf_L
table(ENSMI$Nec_ins_pf_T, useNA = "a")


## Necesidad satisfecha de planificacion familiar

ENSMI$Nec_sat_pf_E = ifelse(ENSMI$v626== 3 ,1,0)
ENSMI$Nec_sat_pf_L = ifelse(ENSMI$v626 == 4 ,1,0)
ENSMI$Nec_sat_pf_T = ENSMI$Nec_sat_pf_E + ENSMI$Nec_sat_pf_L
table(ENSMI$Nec_sat_pf_T, useNA = "a")

## Falla del método

ENSMI$Falla_met = ifelse(ENSMI$v626%in%c(5,6),1,0)
table(ENSMI$Falla_met, useNA = "a")


##############################################
## Variables para el calculo de indicadores ##
##############################################

## D6
table(ENSMI$usametodo, useNA = "a")
ENSMI$D6 <- ifelse(ENSMI$usametodo == 1, 1,0)
table(ENSMI$D6, useNA = "a")

## D6m
table(ENSMI$usamoderno, useNA = "a")
ENSMI$D6m <- ifelse(ENSMI$usamoderno == 1, 1,0)
table(ENSMI$D6m, useNA = "a")

## NI
table(ENSMI$Nec_ins_pf_T, useNA = "a")
ENSMI$NI  <- ifelse(ENSMI$Nec_ins_pf_T == 1,1,0)
table(ENSMI$NI, useNA = "a")


##############################################
## Variables diseño                         ##
##############################################

## Ajuste ponderador

# Total de mujeres en edad fertil actualmente ## 4.641.986
# Total mujeres  en Guatemala ## 8.694.024

# Considerando solo a las mujeres de 15-49 pero incluyendo a las demas pues poseen el ponderador V005
ENSMI$fexp = ENSMI$v005*4641986/sum(ENSMI$v005)
table(ENSMI$fexp, useNA = "a")

### Estrato
table(ENSMI$hv023,useNA = "a")
ENSMI$estrato = ENSMI$hv023
table(ENSMI$estrato,useNA = "a")

### upm
table(ENSMI$hv021,useNA = "a")
ENSMI$upm = ENSMI$hv021
table(ENSMI$upm,useNA = "a")


##### Seleccionando las variables que se requieren para la encuesta

ENSMI15 <- ENSMI %>%
  select(upm, estrato, fexp, 
         shdepto, area, anoest, etnia, edad, unida,
         D6, D6m, NI)

colnames(ENSMI15) <- c("upm","estrato", "fexp",
                       "depto", "area", "anoest","etnia","edad", "unida",
                       "D6", "D6m", "NI")

################

saveRDS(ENSMI15,"GTM/2014-2015/1.D6/Data/ENSMI15.rds")

#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################


#########################################################################
##### Seleccionando las variables que se requieren para validar las 
##### estimaciones directas

table(ENSMI$edad, useNA = "a")

table(ENSMI$v312)
table(ENSMI$Nec_ins_pf_T)
table(ENSMI$Nec_sat_pf_T)
table(ENSMI$Falla_met)
table(ENSMI$Depto)


ENSMI_Estima <- ENSMI %>%
  select(upm,estrato,fexp,shdepto, Depto,
         Nec_ins_pf_T, Nec_sat_pf_T, Falla_met,
         area, anoest,etnia, 
         edad, unida, act.sex,
         D6, D6m, NI, v312,escolaridad)

colnames(ENSMI_Estima) <- c("upm","estrato", "fexp","Cod_Depto", "Depto", 
                               "Nec_ins_pf_T","Nec_sat_pf_T", "Falla_met",
                               "area","anoest","etnia","edad", "unida", "act_sex", 
                               "usametodo", "usamoderno", "nec_insat",
                               "tipodemetodo","escolaridad")

################

saveRDS(ENSMI_Estima,"GTM/2014-2015/1.D6/Data/ENSMI_Estima.rds")
saveRDS(ENSMI_Estima,"GTM/2014-2015/2.D6m/Data/ENSMI_Estima.rds")
saveRDS(ENSMI_Estima,"GTM/2014-2015/3.NI/Data/ENSMI_Estima.rds")
saveRDS(ENSMI_Estima,"GTM/2014-2015/4.D7/Data/ENSMI_Estima.rds")




