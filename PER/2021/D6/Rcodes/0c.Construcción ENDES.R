############################################################################################################################################
#############################################################################################################################################
# # 
# # Proceso: Construcción BAse ENDES Completa (Todos los modulos)
# #
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

###############################################################################################################

######################################################################################################################################################
##                                    Cargando Insumos
######################################################################################################################################################


###### 
# Modulo 1629

RECH0 = read.spss("PER/2021/D6/Data/Módulos/760-Modulo1629/RECH0.SAV",to.data.frame = TRUE)
RECH1 = read.spss("PER/2021/D6/Data/Módulos/760-Modulo1629/RECH1.SAV",to.data.frame = TRUE)
RECH4 = read.spss("PER/2021/D6/Data/Módulos/760-Modulo1629/RECH4.SAV",to.data.frame = TRUE)
RECHM = read.spss("PER/2021/D6/Data/Módulos/760-Modulo1629/RECHM.sav",to.data.frame = TRUE)

# Modulo 1630

RECH23 = read.spss("PER/2021/D6/Data/Módulos/760-Modulo1630/RECH23.SAV",to.data.frame = TRUE)


# Modulo 1631

REC91 =   read.spss("PER/2021/D6/Data/Módulos/760-Modulo1631/REC91.SAV",to.data.frame = TRUE)
REC0111 = read.spss("PER/2021/D6/Data/Módulos/760-Modulo1631/REC0111.SAV",to.data.frame = TRUE)


# Modulo 1632

REC21    = read.spss("PER/2021/D6/Data/Módulos/760-Modulo1632/REC21.SAV", to.data.frame = TRUE)
RE223132 = read.spss("PER/2021/D6/Data/Módulos/760-Modulo1632/RE223132.SAV", to.data.frame = TRUE)


# Modulo 1633

REC41 = read.spss("PER/2021/D6/Data/Módulos/760-Modulo1633/REC41.SAV",to.data.frame = TRUE)
REC94 = read.spss("PER/2021/D6/Data/Módulos/760-Modulo1633/REC94.SAV",to.data.frame = TRUE)


# Modulo 1634

DIT   = read.spss("PER/2021/D6/Data/Módulos/760-Modulo1634/DIT.SAV",to.data.frame = TRUE)
REC42 = read.spss("PER/2021/D6/Data/Módulos/760-Modulo1634/REC42.SAV",to.data.frame = TRUE)
REC43 = read.spss("PER/2021/D6/Data/Módulos/760-Modulo1634/REC43.SAV",to.data.frame = TRUE)
REC95 = read.spss("PER/2021/D6/Data/Módulos/760-Modulo1634/REC95.SAV",to.data.frame = TRUE)

# Modulo 1635

RE516171 = read.spss("PER/2021/D6/Data/Módulos/760-Modulo1635/RE516171.SAV",to.data.frame = TRUE)


# Modulo 1636

RE758081 = read.spss("PER/2021/D6/Data/Módulos/760-Modulo1636/RE758081.SAV",to.data.frame = TRUE)
REC82    = read.spss("PER/2021/D6/Data/Módulos/760-Modulo1636/REC82.SAV",to.data.frame = TRUE)


# Modulo 1637

REC83   = read.spss("PER/2021/D6/Data/Módulos/760-Modulo1637/REC83.SAV",to.data.frame = TRUE)
REC84DV = read.spss("PER/2021/D6/Data/Módulos/760-Modulo1637/REC84DV.SAV",to.data.frame = TRUE)


# Modulo 1638

REC44 = read.spss("PER/2021/D6/Data/Módulos/760-Modulo1638/REC44.SAV",to.data.frame = TRUE)
RECH5 = read.spss("PER/2021/D6/Data/Módulos/760-Modulo1638/RECH5.SAV",to.data.frame = TRUE)
RECH6 = read.spss("PER/2021/D6/Data/Módulos/760-Modulo1638/RECH6.SAV",to.data.frame = TRUE)


# Modulo 1639

REC93DVdisciplina = read.spss("PER/2021/D6/Data/Módulos/760-Modulo1639/REC93DVdisciplina.SAV",to.data.frame = TRUE)



# Modulo 1640

CSALUD01 = read.spss("PER/2021/D6/Data/Módulos/760-Modulo1640/CSALUD01.SAV",to.data.frame = TRUE)
CSALUD08 = read.spss("PER/2021/D6/Data/Módulos/760-Modulo1640/CSALUD08.SAV",to.data.frame = TRUE)



# Modulo 1641

PSXH           = read.spss("PER/2021/D6/Data/Módulos/760-Modulo1641/Programas Sociales x Hogar.SAV",to.data.frame = TRUE)
ps_beca18      = read.spss("PER/2021/D6/Data/Módulos/760-Modulo1641/ps_beca18.SAV",to.data.frame = TRUE)
ps_COMEDOR     = read.spss("PER/2021/D6/Data/Módulos/760-Modulo1641/ps_COMEDOR.SAV",to.data.frame = TRUE)
ps_PENSIONES65 = read.spss("PER/2021/D6/Data/Módulos/760-Modulo1641/ps_PENSIONES65.SAV",to.data.frame = TRUE)
ps_QALIWARMA   = read.spss("PER/2021/D6/Data/Módulos/760-Modulo1641/ps_QALIWARMA.SAV",to.data.frame = TRUE)
ps_TRABAJA     = read.spss("PER/2021/D6/Data/Módulos/760-Modulo1641/ps_TRABAJA.SAV",to.data.frame = TRUE)
ps_VL          = read.spss("PER/2021/D6/Data/Módulos/760-Modulo1641/ps_VL.SAV",to.data.frame = TRUE)
ps_WAWAWASI    = read.spss("PER/2021/D6/Data/Módulos/760-Modulo1641/ps_WAWAWASI.SAV",to.data.frame = TRUE)


############# FILTRO - CREACION BASE

REC0111  = read.spss("PER/2021/D6/Data/Módulos/760-Modulo1631/REC0111.SAV",to.data.frame = TRUE)
REC21    = read.spss("PER/2021/D6/Data/Módulos/760-Modulo1632/REC21.SAV", to.data.frame = TRUE)
RE223132 = read.spss("PER/2021/D6/Data/Módulos/760-Modulo1632/RE223132.SAV", to.data.frame = TRUE)
RE516171 = read.spss("PER/2021/D6/Data/Módulos/760-Modulo1635/RE516171.SAV",to.data.frame = TRUE)
REC91    = read.spss("PER/2021/D6/Data/Módulos/760-Modulo1631/REC91.SAV",to.data.frame = TRUE)
RECH0    = read.spss("PER/2021/D6/Data/Módulos/760-Modulo1629/RECH0.SAV",to.data.frame = TRUE)
RECH1    = read.spss("PER/2021/D6/Data/Módulos/760-Modulo1629/RECH1.SAV",to.data.frame = TRUE)
RECH23   = read.spss("PER/2021/D6/Data/Módulos/760-Modulo1630/RECH23.SAV",to.data.frame = TRUE)
RECH4    = read.spss("PER/2021/D6/Data/Módulos/760-Modulo1629/RECH4.SAV",to.data.frame = TRUE)

### modificacion variable de emparejamiento
RECH4$CASEID = ifelse(RECH4$IDXH4<10,paste(paste(RECH4$HHID,""),RECH4$IDXH4),
                      paste(RECH4$HHID,RECH4$IDXH4))

RECH1$CASEID = ifelse(RECH1$HVIDX<10, paste(paste(RECH1$HHID,""),RECH1$HVIDX),
                      paste(RECH1$HHID,RECH1$HVIDX))

### Union bases encuestas

ENDES1 = REC0111 %>% left_join(RECH1, by = "CASEID") %>%
  left_join(RECH4, by = "CASEID") %>%
  left_join(RE223132, by = "CASEID") %>%
  left_join(REC91, by = "CASEID") %>%
  left_join(RE516171, by = "CASEID")


### Union bases hogar

ENDES = ENDES1 %>% left_join(RECH0, by = "HHID") %>% 
  left_join(RECH23, by = "HHID")

ENDESMEF = ENDES %>% mutate(FEXM = V005*7845787/sum(V005)) %>%
  filter(FEXM!=0) %>%
  select(
    #Variables que coinciden con censo y factor ponderador (a excepcion de edad quinquenal V013)
    HHID, CASEID, V005, HV005, FEXM,V001,V012,V013, V021,V022,V023, V024, HV023, HV024, SPROVIN, SHPROVIN,
    SDISTRI,SHDISTRI,V025,HV021,HV022,HV023,HV024, HV025,SHTOTH,HV215,HV214,HV213,V113,HV202,SH42,SH51,SH52,
    HV205,V119,HV206,SH71,SH72,V161,HV226,V120,V121,V122,V123,V124,V125,V153,HV207,HV208,
    HV209,HV210,HV211,HV212,HV243A,HV243B,HV243C,HV243D,HV103,V150,HV101,HV104,HV220,HV105,
    HV102,SH11A,SH11B,SH11C,SH11D,SH11E,SH11Y,SH11Z,S229B1,QD333_1,SH61L,SH61O,SH61N,SH61K,SH61P,SH61J,SH61Q,
    QD333_2 ,QD333_3,QD333_4,QD333_5,QD333_6,S119,V155,S108N,S108Y,S108G,
    HV109,S111,HV110,SH13,V716,V717,V501,HV115,S119D,V130,V201,V310,V206,V207,
    ## variables para indicadores de planificacion familiar (por ahora)
    V313,V536,V501,V502,V602,V605,V604,V623,V624,V625,V626,V632,V149
  )

saveRDS(ENDESMEF, file = "PER/2021/D6/Output/ENDESMEF.rds")


# creacion variable de hacinamiento

haci.endes = RECH0 %>% select(HHID,HV009) %>%
  left_join(RECH23,by = "HHID") %>%
  select(HHID,HV009,SH72)

haci.endes = haci.endes %>% mutate(
  ind.hacinamiento = HV009 /SH72
) 

saveRDS(haci.endes,"PER/2021/D6/Output/IndiceHacinamientoEndes.rds")

