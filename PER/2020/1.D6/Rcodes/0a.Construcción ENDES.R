############################################################################################################################################
#############################################################################################################################################
# #
# # Proceso: Construcción BAse ENDES Completa (Todos los modulos) 2019
# #                                                ------------- reporte 2020
# #
# # Historia Archivo:
# #   Creation : 23/06/2022
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
library(srvyr)
library(TeachingSampling)



###############################################################################################################

######################################################################################################################################################
##                                    Cargando Insumos
######################################################################################################################################################


#### Cargando los modulos de la encuesta ENDES para la creación de la  Base
#
###### 
#
# #
# REC0111 = read.spss("PER/2020/1.D6/Data/Modulos/691-Modulo66/Modulo66/REC0111.sav",to.data.frame = TRUE)
# REC21 = read.spss("PER/2020/1.D6/Data/Modulos/691-Modulo67/Modulo67/REC21.SAV", to.data.frame = TRUE)
# RE223132 = read.spss("PER/2020/1.D6/Data/Modulos/691-Modulo67/Modulo67/RE223132.SAV", to.data.frame = TRUE)
# RE516171 = read.spss("PER/2020/1.D6/Data/Modulos/691-Modulo71/Modulo71/RE516171.SAV",to.data.frame = TRUE)
# REC91 = read.spss("PER/2020/1.D6/Data/Modulos/691-Modulo66/Modulo66/REC91.SAV",to.data.frame = TRUE)
# #
# RECH0 = read.spss("PER/2020/1.D6/Data/Modulos/691-Modulo64/Modulo64/RECH0.SAV",to.data.frame = TRUE)
# RECH1 = read.spss("PER/2020/1.D6/Data/Modulos/691-Modulo64/Modulo64/RECH1.SAV",to.data.frame = TRUE)
# RECH23 = read.spss("PER/2020/1.D6/Data/Modulos/691-Modulo65/Modulo65/RECH23.SAV",to.data.frame = TRUE)
# RECH4 = read.spss("PER/2020/1.D6/Data/Modulos/691-Modulo64/Modulo64/RECH4.SAV",to.data.frame = TRUE)
# #
# # ### modificacion variable de emparejamiento
# RECH4$CASEID = ifelse(RECH4$IDXH4<10,paste(paste(RECH4$HHID,""),RECH4$IDXH4),
#                        paste(RECH4$HHID,RECH4$IDXH4))
# RECH1$CASEID = ifelse(RECH1$HVIDX<10, paste(paste(RECH1$HHID,""),RECH1$HVIDX),
#                        paste(RECH1$HHID,RECH1$HVIDX))
# #
# # ### Union bases encuestas
# #
# ENDES1 = REC0111 %>%
#    left_join(RECH1, by = "CASEID") %>%
#    left_join(RECH4, by = "CASEID") %>%
#    left_join(RE223132, by = "CASEID") %>%
#    left_join(REC91, by = "CASEID") %>%
#    left_join(RE516171, by = "CASEID")
# 
# #REC0111
# 
# dim(ENDES1)
# 
# #
# # ### Union bases hogar
# #
# ENDES = ENDES1 %>% left_join(RECH0, by = "HHID") %>%
#    left_join(RECH23, by = "HHID")
# 
# dim(ENDES)
# 
# ENDESMEF = ENDES %>% mutate(FEXM = V005*7845787/sum(V005))
# #ENDESMEF = ENDES %>% mutate(FEXT = V005*8620020/sum(V005))
# 
# ENDESMEF= ENDESMEF %>%
#    filter(FEXM!=0) %>%
#    select(
#      #Variables que coinciden con censo y factor ponderador (a excepcion de edad quinquenal V013)
#      HHID, CASEID, V005, HV005, FEXM, V001,V012,V013, V021,V022,V023, V024, HV023, HV024, SPROVIN, SHPROVIN,
#      SDISTRI,SHDISTRI,V025,HV021,HV022,HV023,HV024, HV025,SHTOTH,HV215,HV214,HV213,V113,HV202,SH42,SH51,SH52,
#      HV205,V119,HV206,SH71,SH72,V161,HV226,V120,V121,V122,V123,V124,V125,V153,HV207,HV208,
#      HV209,HV210,HV211,HV212,HV243A,HV243B,HV243C,HV243D,HV103,V150,HV101,HV104,HV220,HV105,
#      HV102,SH11A,SH11B,SH11C,SH11D,SH11E,SH11Y,SH11Z,S229B1,QD333_1,SH61L,SH61O,SH61N,SH61K,SH61P,SH61J,SH61Q,
#      QD333_2 ,QD333_3,QD333_4,QD333_5,QD333_6,S119,V155,S108N,S108Y,S108G,SH15N,SH15Y,
#      HV109,S111,HV110,SH13,V716,V717,V501,HV115,S119D,V130,V201,V310,V206,V207,
#      ## variables para indicadores de planificacion familiar (por ahora)
#      V312,V313,V536,V501,V502,V602,V605,V604,V623,V624,V625,V626,V632,V149
#    )
# 
# dim(ENDESMEF)
# 
# saveRDS(ENDESMEF, file = "PER/2020/1.D6/Data/ENDESMEF.rds")

## cargando la base de datos guardada

ENDESMEF <- readRDS(file = "PER/2020/1.D6/Data/ENDESMEF.rds")


##### Eliminando los modulos cargados

# rm(RECH0,RECH1,RECH4, RECHM,RECH23,REC91,REC011,REC2,RE2231,REC41,REC94,DIT,REC42,REC43,
#    REC95,RE516171,RE758081,REC82,REC83,REC84DV,REC44,RECH5,RECH6,REC93DVdisciplina,CSALUD01,
#    CSALUD08,PSXH,ps_beca18,ps_COMEDOR,ps_PENSIONES65,ps_QALIWARMA,ps_TRABAJA,ps_VL,ps_WAWAWASI,
#    REC0111,REC21,RE223132,RE516171,REC91,RECH0,RECH1,RECH23,RECH4,ENDES,ENDES1)

# Adecuación de las bases de datos  ---------------------------------------

##############################################
## Ajuste Covariables               ##
##############################################

ENDESMEF = ENDESMEF %>% mutate(FEXT = V005*8620020/sum(V005))
names(ENDESMEF)
###############################
## Creacion Departamento y provincia##
###############################

## depto

ENDESMEF$Departamento = ENDESMEF$HV024

### Asignación de codigos


ENDESMEF <- ENDESMEF %>%
  mutate(
    Cod_depto = case_when(
      Departamento == "Amazonas" ~ 01 ,
      Departamento == "Ancash" ~ 02,
      Departamento == "Apurimac" ~ 03,
      Departamento == "Arequipa" ~ 04 ,
      Departamento == "Ayacucho" ~ 05 ,
      Departamento == "Cajamarca" ~ 06 ,
      Departamento == "Callao" ~ 07 ,
      Departamento == "Cusco" ~ 08 ,
      Departamento == "Huancavelica" ~ 09 ,
      Departamento == "Huanuco" ~ 10,
      Departamento == "Ica" ~ 11 ,
      Departamento == "Junin" ~ 12 ,
      Departamento == "La Libertad" ~ 13 ,
      Departamento == "Lambayeque" ~ 14 ,
      Departamento == "Lima" ~ 15 ,
      Departamento == "Loreto" ~ 16 ,
      Departamento == "Madre de Dios" ~ 17 ,
      Departamento == "Moquegua" ~ 18 ,
      Departamento == "Pasco" ~ 19 ,
      Departamento == "Piura" ~ 20,
      Departamento == "Puno" ~ 21 ,
      Departamento == "San Martin" ~ 22 ,
      Departamento == "Tacna" ~ 23 ,
      Departamento == "Tumbes" ~ 24 ,
      Departamento == "Ucayali" ~ 25
    ),
    Cod_depto = str_pad(
      string = Cod_depto,
      pad = "0",
      width = 2,
    ),
    Cod_provin = str_pad(
      string = SHPROVIN,
      pad = "0",
      width = 2,
    ),
    Cod_provi_full = paste0(Cod_depto, Cod_provin)
  )

table(ENDESMEF$Cod_provin, useNA = "a")
table(ENDESMEF$Cod_provi_full, useNA = "a")

ENDESMEF$Provin_nom =  paste0(ENDESMEF$Departamento, "-", ENDESMEF$Cod_provin)
table(ENDESMEF$Provin_nom, useNA = "a")


# Eliminando base ENDESMEF
#rm(ENDESMEF)

#----

## Area

ENDESMEF$area = ENDESMEF$V025
table(ENDESMEF$area, useNA = "a")

## Edad

ENDESMEF$edad = ENDESMEF$V012
table(ENDESMEF$edad, useNA = "a")

## Discapacidad del grupo de Washington

ENDESMEF = ENDESMEF %>%
  mutate(
    dis.ver = ifelse(QD333_1 == "Si", 1, 0),
    dis.oir = ifelse(QD333_2 == "Si", 1, 0),
    dis.hablar = ifelse(QD333_3 == "Si", 1, 0),
    dis.moverse = ifelse(QD333_4 == "Si", 1, 0),
    dis.entender = ifelse(QD333_5 == "Si", 1, 0),
    dis.relacionarse = ifelse(QD333_6 == "Si", 1, 0)
  ) %>%
  rowwise() %>%
  mutate(
    discapacidad = max(
      dis.ver,
      dis.oir,
      dis.hablar,
      dis.moverse,
      dis.entender,
      dis.relacionarse
    )
    
  )

table(ENDESMEF$discapacidad, useNA = "a")

ENDESMEF$discapacidad[is.na(ENDESMEF$discapacidad)] <- 0

table(ENDESMEF$discapacidad, useNA = "a")


## Etnia

levels(ENDESMEF$S119D)
ENDESMEF$etnia = ENDESMEF$S119D
table(ENDESMEF$S119D)

#### Años de estudio
table(ENDESMEF$SH15N)

table(ENDESMEF$SH15N, useNA = "a")
levels(ENDESMEF$SH15N)
table(ENDESMEF$SH15Y, useNA = "a")

##### minimnos de educacion


ENDESMEF$proxy1 <- ifelse(ENDESMEF$SH15N == "Preescolar",
                          0,
                          ifelse(
                            ENDESMEF$SH15N == "Primario",
                            6,
                            
                            ifelse(
                              ENDESMEF$SH15N == "Secundario",
                              6,
                              ifelse(
                                ENDESMEF$SH15N == "Superior, no universitario",
                                11,
                                ifelse(
                                  ENDESMEF$SH15N == "Superior universitario",
                                  11,
                                  ifelse(ENDESMEF$SH15N == "Posgrado", 15, 0)
                                )
                              )
                            )
                          ))

table(ENDESMEF$proxy1, useNA = "a")


ENDESMEF$proxy2 <-
  ifelse(
    ENDESMEF$SH15Y == "Educación dada en grados",
    0,
    ifelse(ENDESMEF$SH15Y == "No lo sé", 0, ENDESMEF$SH15Y)
  )

table(ENDESMEF$proxy2, useNA = "a")

ENDESMEF$proxy3 <- ENDESMEF$proxy2 + ENDESMEF$proxy1

table(ENDESMEF$proxy3, useNA = "a")


ENDESMEF$anoest <- ifelse(
  ENDESMEF$SH15N == "Primario",
  6,
  ifelse(
    ENDESMEF$SH15N == "Secundario" & ENDESMEF$proxy3 > 11,
    11,
    ENDESMEF$proxy3
  )
)

table(ENDESMEF$anoest, useNA = "a")

ENDESMEF$anoest[is.na(ENDESMEF$anoest)] <- 11

table(ENDESMEF$anoest, useNA = "a")


####### adecuando años de estudio por grado

table(ENDESMEF$SH15N, useNA = "a")

ENDESMEF$escolaridad <- ifelse(ENDESMEF$SH15N == "Inicial", 0,
                               ifelse(ENDESMEF$SH15N == "Preescolar", 0,
                                    ifelse(ENDESMEF$SH15N == "Primario", 1,
                                       ifelse(ENDESMEF$SH15N == "Secundario", 2,
                                         ifelse(ENDESMEF$SH15N == "Superior, no universitario", 3,
                                                ifelse(ENDESMEF$SH15N == "Superior universitario", 3,
                                                       ifelse(ENDESMEF$SH15N == "Posgrado",3, 99)))))))

ENDESMEF$escolaridad[is.na(ENDESMEF$escolaridad)] <- 2
table(ENDESMEF$escolaridad, useNA = "a")

####### adecuando etnia
table(ENDESMEF$S119D, useNA = "a")

ENDESMEF$etnia2 <- ifelse(ENDESMEF$S119D == "Quechua", 1,
                          ifelse(ENDESMEF$S119D == "Aimara", 1,
                                 ifelse(ENDESMEF$S119D == "Nativo o indigena de la Amazonía", 1,
                                        ifelse(ENDESMEF$S119D == "Parte de otro pueblo indigena u originario", 1,
                                               ifelse(ENDESMEF$S119D == "Negro/ Moreno/ Zambo/ Mulato/Pueblo Afroperuano o afrodescendiente", 2,
                                                      ifelse(ENDESMEF$S119D == "Blanco", 3,
                                                             ifelse(ENDESMEF$S119D == "Mestizo", 3,
                                                                    ifelse(ENDESMEF$S119D == "Otro", 3,
                                                                           ifelse(ENDESMEF$S119D == "No sabe", 3,99)))))))))


table(ENDESMEF$etnia2, useNA = "a")

##############################################
## Variables diseño muestral                 ##
##############################################

## Ajuste ponderador

# Considerando solo a las mujeres de 15-49 pero incluyendo a las demas pues poseen el ponderador V005
ENDESMEF$fexp = (ENDESMEF$V005 * 7845787 / sum(ENDESMEF$V005)) ### averiguar  cuantas mujeres están es
## este rango de edad segun el INEi

### Estrato
table(ENDESMEF$V022, useNA = "always")
ENDESMEF = ENDESMEF %>% rename(estrato = V022)
table(ENDESMEF$estrato, useNA = "always")

### upm
table(ENDESMEF$V021, useNA = "always")
ENDESMEF = ENDESMEF %>% rename(upm = V021)
table(ENDESMEF$upm, useNA = "always")

##############################################
##Creacion Indicdores                 ##
##############################################


## Mujeres en UNION

ENDESMEF$union_1 = ifelse(ENDESMEF$V501 %in% c("Casado", "Viviendo juntos"),
                          "Unida",
                          "No unida")
ENDESMEF$union_2 = ifelse(ENDESMEF$V502 == "Actualmente casada", 
                          "Unida", "No unida")


## Mujeres Unidas

ENDESMEF$unida = ifelse(ENDESMEF$V501 %in% c("Casado",
                                             "Viviendo juntos"),1,2)
table(ENDESMEF$unida)

## Uso metodo anticonceptivos

levels(ENDESMEF$V313)

ENDESMEF$usametodo = ifelse(ENDESMEF$V313 %in% c("Método moderno",
                                                 "Método tradicional", 
                                                 "Método Folclórico"),1,0)
                            
                            
ENDESMEF$usamoderno = ifelse(ENDESMEF$V313 == "Método moderno", 1, 0)
ENDESMEF$metodo = ENDESMEF$V313

## Mujeres sexualmente activas

table(ENDESMEF$V536)

ENDESMEF$MSA = ifelse(ENDESMEF$V536 == "Activo en las últimas 4 semanas",
                                        "Si", "No")
ENDESMEF$act.sex = ENDESMEF$V536
###

levels(ENDESMEF$V626)
table(ENDESMEF$V626, useNA = "a")

## Necesidad insatisfecha de planificacion familiar

ENDESMEF$Nec_ins_pf_E = ifelse(ENDESMEF$V626 == "NeceSIDAd insatisfecha de espacio", 1, 0)
ENDESMEF$Nec_ins_pf_L = ifelse(ENDESMEF$V626 == "NeceSIDAd insatisfecha de limitar", 1, 0)
ENDESMEF$Nec_ins_pf_T = ifelse(ENDESMEF$Nec_ins_pf_E == 1 |
                                 ENDESMEF$Nec_ins_pf_L == 1, 1, 0)

## Necesidad satisfecha de planificacion familiar

ENDESMEF$Nec_sat_pf_E = ifelse(ENDESMEF$V626 == "Uso de espacio", 1, 0)
ENDESMEF$Nec_sat_pf_L = ifelse(ENDESMEF$V626 == "Uso de límite", 1, 0)
ENDESMEF$Nec_sat_pf_T = ifelse(ENDESMEF$Nec_sat_pf_E == 1 |
                                 ENDESMEF$Nec_sat_pf_L == 1, 1, 0)

## Demanda total de planificacion familiar

ENDESMEF$Dem_tot_pf_E = ifelse(ENDESMEF$V626 %in% c("Necesidad no satisfecha de espacio",
                                                    "Uso de espacio",
                                                    "Falla de espacio"),1,0)

ENDESMEF$Dem_tot_pf_L = ifelse( ENDESMEF$V626 %in% c("Necesidad no satisfecha de límite",
                                                     "Uso de límite",
                                                     "Falla de límite"),1,0)

ENDESMEF$Dem_tot_pf_T = ENDESMEF$Dem_tot_pf_E + ENDESMEF$Dem_tot_pf_L

## Falla del método

ENDESMEF$Falla_met = ifelse(ENDESMEF$V626 %in% c("Falla de espacio",
                                                 "Falla de límite"), 1, 0)

## Demanda satisfecha (de uso de métodos anticonceptivos)
levels(ENDESMEF$V626)
table(ENDESMEF$V626)
table(as.numeric(ENDESMEF$V626))

ENDESMEF$DS = ifelse(as.numeric(ENDESMEF$V626) %in% c(4, 5, 6, 7),
                     1,
                     ifelse(as.numeric(ENDESMEF$V626) %in% c(2, 3), 0, NA))


### modificacion niveles

#ENDESMEF = ENDESMEF %>% filter(edad >= 15)


# ##### Creando y/o adecuando las variables de interes --------------------

################## Seleccionado Covariables #####################
#
# Área                       area                               #
# Municipio/Canton           upm (4 primeros digitos)           #
# Sexo                       HV104                              #
# Edad                       Edad                               #
# Etnia                      etnia                              #
# discapacidad               -----                              #
# Años Estudio               f1_s2_19_1 / f1_s2_19_2            #
#
# Indicadores               D6, D6m, NI                         #
###############                                                 #
# upm                       HV021                               #
# estrato                   V022                                #
# factor de expansión       FEXM                                #
#                                                               #
#################################################################
###################################################################


## D6
table(ENDESMEF$usametodo, useNA = "a")
ENDESMEF$D6 <- ifelse(ENDESMEF$usametodo == 1, 1, 0)
table(ENDESMEF$D6, useNA = "a")

## D6m
table(ENDESMEF$usamoderno, useNA = "a")
ENDESMEF$D6m <- ifelse(ENDESMEF$usamoderno == 1, 1, 0)
table(ENDESMEF$D6m, useNA = "a")

## NI
table(ENDESMEF$Nec_ins_pf_T, useNA = "a")
ENDESMEF$NI  <- ifelse(ENDESMEF$Nec_ins_pf_T == 1, 1, 0)
table(ENDESMEF$NI, useNA = "a")
ENDESMEF$NI <- ifelse(ENDESMEF$unida != 1, NA, ENDESMEF$NI)
ENDESMEF$NI[is.na(ENDESMEF$NI)] <- 0
table(ENDESMEF$NI, useNA = "a")


##### Seleccionando las variables que se requieren para la encuesta

EndesMEF20 <- ENDESMEF %>%
  select(upm,estrato,FEXM,Cod_provi_full,
         area,anoest,etnia,discapacidad,edad,unida,
         D6,D6m,NI)

colnames(EndesMEF20) <- c(
  "upm","estrato","fexp","mpio",
  "area","anoest","etnia","discap","edad","unida",
  "D6","D6m","NI"
)

### Salvando Base de datos

saveRDS(EndesMEF20, file = "PER/2020/1.D6/Data/EndesMEF20.rds")

####################################################################################
####################################################################################
####################################################################################
##### Seleccionando las variables que se requieren para validar las 
##### estimaciones directas

table(ENDESMEF$edad, useNA = "a")

table(ENDESMEF$V312)
table(ENDESMEF$Nec_ins_pf_T)
table(ENDESMEF$Nec_sat_pf_T)
table(ENDESMEF$Falla_met)
table(ENDESMEF$Departamento)


ENDESMEF_Estima <- ENDESMEF %>%
  select(upm,estrato,fexp,FEXM,FEXT,Cod_provi_full, Departamento,Provin_nom,
         Nec_ins_pf_T, Nec_sat_pf_T, Falla_met,
         area, anoest,etnia2, 
         edad, unida, discapacidad,
         D6, D6m, NI, V312,escolaridad)

colnames(ENDESMEF_Estima) <- c("upm","estrato","fexp","FEXM","fexp_tot","mpio", "Depto", "Provincia",
                               "Nec_ins_pf_T","Nec_sat_pf_T", "Falla_met",
                               "area","anoest","etnia","edad", "unida", "discapacidad",
                               "usametodo", "usamoderno", "nec_insat", "tipodemetodo","escolaridad")

################

names(ENDESMEF_Estima)

head(ENDESMEF_Estima$fexp)
head(ENDESMEF_Estima$FEXM)
head(ENDESMEF_Estima$fexp_tot)

####################

saveRDS(ENDESMEF_Estima,"PER/2020/1.D6/Data/ENDESMEF_Estima.rds")
saveRDS(ENDESMEF_Estima,"PER/2020/2.D6m/Data/ENDESMEF_Estima.rds")
saveRDS(ENDESMEF_Estima,"PER/2020/3.NI/Data/ENDESMEF_Estima.rds")
saveRDS(ENDESMEF_Estima,"PER/2020/4.D7/Data/ENDESMEF_Estima.rds")


