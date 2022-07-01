############################################################################################################################################
#############################################################################################################################################
# # 
# # Process: Construcción Base ENSANUT Completa (Todos los modulos)
# #
# # 
# # File history:
# #   Creation : 30/06/2022
# #
# #
# # Creation: Gabriel J. Nieto 
# #  
# # 
# # Modification:   
#############################################################################################################################################
#############################################################################################################################################
##                                    Verificando librerias e instalando las faltantes  
#############################################################################################################################################

## Función que examina si un paquete se encuentra instalado, si lo estÃ¡ sigue derecho, si no estÃ¡ instalado, lo instala

paquetes <- c("readr","readxl","faraway","wesanderson","dplyr","factoextra","corrplot","reshape2","reshape",
              "PerformanceAnalytics","foreign","plotly","FactoMineR","homals", "nnet", "tidyverse", "gapminder")
has <- paquetes %in% rownames(installed.packages())
if(any(!has)) install.packages(paquetes [!has])


#############################################################################################################################################
##                                    Cargando Librerias
#############################################################################################################################################
library(readr)
library(readxl)
library(readxl)
library(faraway)
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

######################################################################################################################################################
##                                    Definiendo rutas de entradas y salidas
########################################################################################################################

setwd("C:/Users/Gabriel Nieto/Dropbox/Encuestas de Hogares/Ecuador/DATOS_ENSANUT_2018")
dir()

###########################################################################################################################

######################################################################################################################################################
##                                    Cargando Insumos
######################################################################################################################################################


###### 
# Modulos

load("ecu_personas.RData")
load("ecu_hogar.RData")
load("ecu_etiqueta.RData")
load("ecu_mujeres.RData")
load("ecu_lactancia.RData")
load("ecu_salud_niñez.RData")
load("ecu_hombres.RData")
load("ecu_fact_riesgo.RData")
load("ecu_info_niñez.RData")

#############################################

### Unir Bases Personas

ENSANUT1 = ecu_personas %>% left_join(ecu_mujeres, by = "id_per") %>%
  left_join(ecu_hombres, by = "id_per") 


ENSANUT18= ENSANUT1 %>% left_join(ecu_hogar, by = "id_hogar")

############# FILTRO - CREACION BASE

#### Filtro se hará cuanso se vinculen los datos correspondientes al Censo 

saveRDS(ENSANUT18, file = "ENSANUT.rds")

#eliminar Variables repetidas)
ENSANUT = ENSANUT18[c(1:264,             ## Personas
                      271:799,           ## Mujeres
                      804:946,951:954,  ## Hombres
                      962:1076          ## Hogares
)]
##### Definición Variables para establecer los modelos 

#ENSANUT <- ENSANUT[c(1:8,)] 

## variables para indicadores de planificación familiar (por ahora)
#  V313,V536,V501,V502,V602,V605,V604,V623,V624,V625,V626,V632,V149)

rm(ecu_personas,ecu_hogar,ecu_etiqueta,ecu_fact_riesgo,
   ecu_hombres, ecu_info_niñez, ecu_lactancia, ecu_mujeres,
   ecu_salud_niñez,ENSANUT18,ENSANUT1)

###### MÉTODOS

##Mujeres BD4
ENSANUT$f2_s6_603_1[is.na(ENSANUT$f2_s6_603_1)] <- 0
ENSANUT$m_Vasectomia   =ifelse(ENSANUT$f2_s6_603_1  =="si",1,0)

ENSANUT$f2_s6_603_2[is.na(ENSANUT$f2_s6_603_2)] <- 0
ENSANUT$m_Ligadura     =ifelse(ENSANUT$f2_s6_603_2  =="si",1,0)                                  

ENSANUT$f2_s6_603_3[is.na(ENSANUT$f2_s6_603_3)] <- 0
ENSANUT$m_Implante     =ifelse(ENSANUT$f2_s6_603_3  =="si",1,0)

ENSANUT$f2_s6_603_4[is.na(ENSANUT$f2_s6_603_4)] <- 0
ENSANUT$m_Inyeccion    =ifelse(ENSANUT$f2_s6_603_4  =="si",1,0)

ENSANUT$f2_s6_603_5[is.na(ENSANUT$f2_s6_603_5)] <- 0
ENSANUT$m_DIU          =ifelse(ENSANUT$f2_s6_603_5  =="si",1,0)

ENSANUT$f2_s6_603_6[is.na(ENSANUT$f2_s6_603_6)] <- 0
ENSANUT$m_Pildora      =ifelse(ENSANUT$f2_s6_603_6  =="si",1,0)

ENSANUT$f2_s6_603_7[is.na(ENSANUT$f2_s6_603_7)] <- 0
ENSANUT$m_Condon_fem   =ifelse(ENSANUT$f2_s6_603_7  =="si",1,0)

ENSANUT$f2_s6_603_8[is.na(ENSANUT$f2_s6_603_8)] <- 0
ENSANUT$m_Condon_mas   =ifelse(ENSANUT$f2_s6_603_8  =="si",1,0)

ENSANUT$f2_s6_603_9[is.na(ENSANUT$f2_s6_603_9)] <- 0
ENSANUT$m_Pastilla_eme =ifelse(ENSANUT$f2_s6_603_9  =="si",1,0)

ENSANUT$f2_s6_603_10[is.na(ENSANUT$f2_s6_603_10)] <- 0
ENSANUT$m_Ritmo        =ifelse(ENSANUT$f2_s6_603_10 =="si",1,0)

ENSANUT$f2_s6_603_11[is.na(ENSANUT$f2_s6_603_11)] <- 0
ENSANUT$m_Retiro       =ifelse(ENSANUT$f2_s6_603_11 =="si",1,0)

ENSANUT$f2_s6_603_12[is.na(ENSANUT$f2_s6_603_12)] <- 0
ENSANUT$m_Lactancia    =ifelse(ENSANUT$f2_s6_603_12 =="si",1,0)


##Hombres BD7
ENSANUT$f3_s3_305_1[is.na(ENSANUT$f3_s3_305_1)] <- 0
ENSANUT$h_Vasectomia   =ifelse(ENSANUT$f3_s3_305_1  =="si",1,0)

ENSANUT$f3_s3_305_2[is.na(ENSANUT$f3_s3_305_2)] <- 0
ENSANUT$h_Ligadura     =ifelse(ENSANUT$f3_s3_305_2  =="si",1,0)                                  

ENSANUT$f3_s3_305_3[is.na(ENSANUT$f3_s3_305_3)] <- 0
ENSANUT$h_Implante     =ifelse(ENSANUT$f3_s3_305_3  =="si",1,0)

ENSANUT$f3_s3_305_4[is.na(ENSANUT$f3_s3_305_4)] <- 0
ENSANUT$h_Inyeccion    =ifelse(ENSANUT$f3_s3_305_4  =="si",1,0)

ENSANUT$f3_s3_305_5[is.na(ENSANUT$f3_s3_305_5)] <- 0
ENSANUT$h_DIU          =ifelse(ENSANUT$f3_s3_305_5  =="si",1,0)

ENSANUT$f3_s3_305_6[is.na(ENSANUT$f3_s3_305_6)] <- 0
ENSANUT$h_Pildora      =ifelse(ENSANUT$f3_s3_305_6  =="si",1,0)

ENSANUT$f3_s3_305_7[is.na(ENSANUT$f3_s3_305_7)] <- 0
ENSANUT$h_Condon_fem   =ifelse(ENSANUT$f3_s3_305_7  =="si",1,0)

ENSANUT$f3_s3_305_8[is.na(ENSANUT$f3_s3_305_8)] <- 0
ENSANUT$h_Condon_mas   =ifelse(ENSANUT$f3_s3_305_8  =="si",1,0)

ENSANUT$f3_s3_305_9[is.na(ENSANUT$f3_s3_305_9)] <- 0
ENSANUT$h_Pastilla_eme =ifelse(ENSANUT$f3_s3_305_9  =="si",1,0)

ENSANUT$f3_s3_305_10[is.na(ENSANUT$f3_s3_305_10)] <- 0
ENSANUT$h_Ritmo        =ifelse(ENSANUT$f3_s3_305_10 =="si",1,0)

ENSANUT$f3_s3_305_11[is.na(ENSANUT$f3_s3_305_11)] <- 0
ENSANUT$h_Retiro       =ifelse(ENSANUT$f3_s3_305_11 =="si",1,0)

ENSANUT$f3_s3_305_12[is.na(ENSANUT$f3_s3_305_12)] <- 0
ENSANUT$h_Lactancia    =ifelse(ENSANUT$f3_s3_305_12 =="si",1,0)

####     USA METODO  
ENSANUT <- ENSANUT %>%
  dplyr::rowwise() %>%
  dplyr::mutate(Metod_Sum  = sum(m_Vasectomia, m_Ligadura,   m_Implante,
                                 m_Inyeccion,  m_DIU,        m_Pildora,
                                 m_Condon_fem, m_Condon_mas, m_Pastilla_eme,
                                 m_Ritmo,      m_Retiro,     m_Lactancia, 
                                 h_Vasectomia, h_Ligadura,   h_Implante,
                                 h_Inyeccion,  h_DIU,        h_Pildora,
                                 h_Condon_fem, h_Condon_mas, h_Pastilla_eme,
                                 h_Ritmo,      h_Retiro,     h_Lactancia,na.rm = TRUE),
                
                Metod_Max  = max(m_Vasectomia, m_Ligadura,   m_Implante,
                                 m_Inyeccion,  m_DIU,        m_Pildora,
                                 m_Condon_fem, m_Condon_mas, m_Pastilla_eme,
                                 m_Ritmo,      m_Retiro,     m_Lactancia,
                                 h_Vasectomia, h_Ligadura,   h_Implante,
                                 h_Inyeccion,  h_DIU,        h_Pildora,
                                 h_Condon_fem, h_Condon_mas, h_Pastilla_eme,
                                 h_Ritmo,      h_Retiro,     h_Lactancia, na.rm = TRUE))


ENSANUT$usametodo <- ifelse(ENSANUT$Metod_Max == 1,1,0)
table(ENSANUT$usametodo)


#### ####     USA MODERNO       
ENSANUT <- ENSANUT %>%
  dplyr::rowwise() %>%
  dplyr::mutate(Modern_Sum  = sum(m_Vasectomia, m_Ligadura,  m_Implante,
                                  m_Inyeccion,  m_DIU,        m_Pildora,
                                  m_Condon_fem, m_Condon_mas, m_Pastilla_eme,
                                  
                                  h_Vasectomia, h_Ligadura,   h_Implante,
                                  h_Inyeccion,  h_DIU,        h_Pildora,
                                  h_Condon_fem, h_Condon_mas, h_Pastilla_eme, na.rm = TRUE),
                
                
                Modern_Max  = max(m_Vasectomia, m_Ligadura,   m_Implante,
                                  m_Inyeccion,  m_DIU,        m_Pildora,
                                  m_Condon_fem, m_Condon_mas, m_Pastilla_eme,
                                  
                                  h_Vasectomia, h_Ligadura,   h_Implante,
                                  h_Inyeccion,  h_DIU,        h_Pildora,
                                  h_Condon_fem, h_Condon_mas, h_Pastilla_eme, na.rm = TRUE))


ENSANUT$usamoderno <- ifelse(ENSANUT$Modern_Max == 1,1,0)
table(ENSANUT$usamoderno)

##################### 

##### Definiendo variables sobre necesidades insatisfechas


table(ENSANUT$f2_s2_201)
### ¿ no quería embarazarse?     
### ¿quería esperar más tiempo? Espacio Tiempo
ENSANUT$f2_s2_201[is.na(ENSANUT$f2_s2_201)] <- 0

ENSANUT$embarazo_Actual= ifelse(ENSANUT$f2_s2_201  =="¿ no quería embarazarse?", 1,
                                ifelse(ENSANUT$f2_s2_201  =="¿quería esperar más tiempo?",1,0))

#######

table(ENSANUT$f2_s2_202_1)
### >12 meses
ENSANUT$f2_s2_202_1[is.na(ENSANUT$f2_s2_202_1)] <- 0

ENSANUT$tiempo_espera_EAM= ifelse(ENSANUT$f2_s2_202_1  >= 1, 1,0)

#######

table(ENSANUT$f2_s2_202_2)
### >1 año
ENSANUT$f2_s2_202_2[is.na(ENSANUT$f2_s2_202_2)] <- 0

ENSANUT$tiempo_espera_EAA= ifelse(ENSANUT$f2_s2_202_2  >= 1, 1,0)

#######

table(ENSANUT$f2_s6_613)
### la pareja se opone?
### no conoce/no tiene información sobre métodos anticonceptivos?
### por motivos económicos
### por pena o vergüenza 

#######
ENSANUT$f2_s6_613[is.na(ENSANUT$f2_s6_613)] <- 0

ENSANUT$Razon_no_uso_met= ifelse(ENSANUT$f2_s6_613  =="la pareja se opone?", 1,
                                 ifelse(ENSANUT$f2_s6_613  =="no conoce/no tiene información sobre métodos anticonceptivos?",1,
                                        ifelse(ENSANUT$f2_s6_613  =="por motivos económicos",1,
                                               ifelse(ENSANUT$f2_s6_613  =="por pena o vergüenza",1,0))))

#######

table(ENSANUT$f2_s6_614)
### Opción si
ENSANUT$f2_s6_614[is.na(ENSANUT$f2_s6_614)] <- 0

ENSANUT$Desea_MA= ifelse(ENSANUT$f2_s6_614 == "si", 1,0)

######


table(ENSANUT$f2_s6_617) #*****
### Opción no
ENSANUT$f2_s6_617[is.na(ENSANUT$f2_s6_617)] <- 0

ENSANUT$Obten_MA= ifelse(ENSANUT$f2_s6_617 == "no", 1,0)

#######

table(ENSANUT$f2_s6_618)
### junta de beneficencia
### Partera
### fundación / ong
### hospital/clínica privada 
### consultorio particular ***
### farmacia o botica
### no sabe/ no responde
ENSANUT$f2_s6_618[is.na(ENSANUT$f2_s6_618)] <- 0

ENSANUT$Donde_Obt_Metod= ifelse(ENSANUT$f2_s6_618  =="junta de beneficencia*", 1,
                                ifelse(ENSANUT$f2_s6_618  =="partera",1,
                                       ifelse(ENSANUT$f2_s6_618  =="fundación / ong**",1,
                                              ifelse(ENSANUT$f2_s6_618  =="hospital/clínica privada",1,
                                                     ifelse(ENSANUT$f2_s6_618  =="consultorio particular",1,
                                                            ifelse(ENSANUT$f2_s6_618  =="farmacia o botica",1,
                                                                   ifelse(ENSANUT$f2_s6_618  =="no sabe/ no responde",1,0)))))))



########

table(ENSANUT$f2_s6_619)
### >120
ENSANUT$f2_s6_619[is.na(ENSANUT$f2_s6_619)] <- 0

ENSANUT$tiempoMin_obten_MA= ifelse(ENSANUT$f2_s6_619  >= 60, 1,0)

########

table(ENSANUT$f2_s6_619_1)
### >2 horas
ENSANUT$f2_s6_619_1[is.na(ENSANUT$f2_s6_619_1)] <- 0

ENSANUT$tiempoHor_obten_MA= ifelse(ENSANUT$f2_s6_619_1  >= 1, 1,0)

########

table(ENSANUT$f2_s6_623)
### su esposo/pareja?
### familiares?
ENSANUT$f2_s6_623[is.na(ENSANUT$f2_s6_623)] <- 0

ENSANUT$Decid_lig= ifelse(ENSANUT$f2_s6_623  =="esposo/pareja?", 1,
                          ifelse(ENSANUT$f2_s6_623  =="familiares?",1,0))

########


table(ENSANUT$f2_s6_624)
### Opción no
ENSANUT$f2_s6_624[is.na(ENSANUT$f2_s6_624)] <- 0

ENSANUT$Tomar_misma_dec= ifelse(ENSANUT$f2_s6_624  == "no", 1, 
                                ifelse(ENSANUT$f2_s6_624  == "no sabe/ no responde", 1,0))
#######


table(ENSANUT$f2_s6_625)
### junta de beneficencia
### Partera
### fundación / ong
### consultorio particular ***
ENSANUT$f2_s6_625[is.na(ENSANUT$f2_s6_625)] <- 0

ENSANUT$Lugar_obten_met= ifelse(ENSANUT$f2_s6_625  =="junta de beneficencia*", 1,
                                ifelse(ENSANUT$f2_s6_625  =="partera",1,
                                       ifelse(ENSANUT$f2_s6_625  =="fundación / ong**",1,
                                              ifelse(ENSANUT$f2_s6_625  =="hospital/clínica privada",1,
                                                     ifelse(ENSANUT$f2_s6_625  =="consultorio particular",1,
                                                            ifelse(ENSANUT$f2_s6_625  =="farmacia o botica",1,
                                                                   ifelse(ENSANUT$f2_s6_625  =="no sabe/ no responde",1,0)))))))






########

table(ENSANUT$f2_s6_626)
### >60
ENSANUT$f2_s6_626[is.na(ENSANUT$f2_s6_626)] <- 0

ENSANUT$Met_obt_min= ifelse(ENSANUT$f2_s6_626  >= 60, 1,0)

#######

table(ENSANUT$f2_s6_626_1)
### >1
ENSANUT$f2_s6_626_1[is.na(ENSANUT$f2_s6_626_1)] <- 0

ENSANUT$Met_obt_hor= ifelse(ENSANUT$f2_s6_626_1  >= 1, 1,0)

#######

table(ENSANUT$f2_s6_628)
### sólo su pareja?
### otra persona? 
ENSANUT$f2_s6_628[is.na(ENSANUT$f2_s6_628)] <- 0

ENSANUT$Q_toma_decis= ifelse(ENSANUT$f2_s6_628  == "sólo su pareja?", 1, 
                             ifelse(ENSANUT$f2_s6_628  == "otra persona?", 1,0))


######

table(ENSANUT$f2_s7_701)
### no quiere hijos/ más hijos/as
ENSANUT$f2_s7_701[is.na(ENSANUT$f2_s7_701)] <- 0

ENSANUT$Mas_hijos= ifelse(ENSANUT$f2_s7_701  == "no quiere hijos/ más hijos/as", 1, 
                          ifelse(ENSANUT$f2_s7_701  == "indecisa", 1,0))


#######

##### Calculo de Número de hijos por mujer 

table(ENSANUT$f2_s2_217_1) ### Total Hijos en Casa 
table(ENSANUT$f2_s2_217_2) ### Total Hijos  fuera de casa


ENSANUT <- ENSANUT %>%
  dplyr::rowwise() %>%
  dplyr::mutate(Total_hijos_M = sum(f2_s2_217_1, f2_s2_217_2,
                                    na.rm = TRUE))
###

table(ENSANUT$f2_s7_703)
### Hacer la diferencia respecto del número de hijos

ENSANUT <- ENSANUT %>%
  dplyr::rowwise() %>%
  dplyr::mutate(Diff_hijos = ((f2_s7_703)-(Total_hijos_M)))


ENSANUT$Hijos_deseo = ifelse(ENSANUT$Diff_hijos < 0, 1,0)
ENSANUT$Hijos_deseo[is.na(ENSANUT$Hijos_deseo)] <- 0                          

#######

table(ENSANUT$f2_s8_800f)
###  no 
###  no sabe
ENSANUT$f2_s8_800f[is.na(ENSANUT$f2_s8_800f)] <- 0

ENSANUT$Info_metod= ifelse(ENSANUT$f2_s8_800f  == "no", 1, 
                           ifelse(ENSANUT$f2_s8_800f  == "no sabe/ no responde", 1,0))


######

table(ENSANUT$f2_s8_837)
### se molestaría?
### no aceptaría?
ENSANUT$f2_s8_837[is.na(ENSANUT$f2_s8_837)] <- 0

ENSANUT$UsoCondon_Par= ifelse(ENSANUT$f2_s8_837  == "se molestaría?", 1, 
                              ifelse(ENSANUT$f2_s8_837  == "no aceptaría?", 1,0))

########## Uniendo las columnas que se requieren para definir NI


ENSANUT <- ENSANUT %>%
  dplyr::rowwise() %>%
  dplyr::mutate(Var_NecI = max(embarazo_Actual,    tiempo_espera_EAM,  Met_obt_min,
                               tiempo_espera_EAA,  Razon_no_uso_met,   Desea_MA,
                               Obten_MA,           Donde_Obt_Metod,    tiempoMin_obten_MA,
                               tiempoHor_obten_MA, Decid_lig,          Tomar_misma_dec,
                               Lugar_obten_met,    Met_obt_min,        Met_obt_hor,
                               Q_toma_decis,       Mas_hijos,          Hijos_deseo,
                               Info_metod,         UsoCondon_Par, 
                               na.rm = TRUE))

#

table(ENSANUT$Var_NecI)

####### Variable Necesidades Insatisfechas

ENSANUT$Nec_insatis= ifelse(ENSANUT$Var_NecI == 1, 1,0)
table(ENSANUT$Nec_insatis)

#########################################
################



################# Definiendo variables sobre necesidades Satisfechas

table(ENSANUT$f2_s6_617)
### Opción si

ENSANUT$Obten_MA_ns= ifelse(ENSANUT$f2_s6_617 == "si", 1,0)

#######


table(ENSANUT$f2_s6_618)
### centro clínico quirúrgico ambulatorio iess
### dispensario seguro campesino
### establecimientos de salud del msp
### hospital/policlínico - ffaa/policía
### consejo provincial / unidad municipal de salud

ENSANUT$Donde_Obt_Metod_ns= ifelse(ENSANUT$f2_s6_618  =="centro clínico quirúrgico ambulatorio iess", 1,
                                   ifelse(ENSANUT$f2_s6_618  =="dispensario seguro campesino",1,
                                          ifelse(ENSANUT$f2_s6_618  =="establecimientos de salud del msp",1,
                                                 ifelse(ENSANUT$f2_s6_618  =="hospital/policlínico - ffaa/policía",1,
                                                        ifelse(ENSANUT$f2_s6_618  =="consejo provincial / unidad municipal de salud",1,
                                                               ifelse(ENSANUT$f2_s6_618  =="hospital de especialidades del iess",1,0))))))



########


table(ENSANUT$f2_s6_623)
### usted y su esposo/pareja?
### usted, esposo/pareja y médico?
### usted?

ENSANUT$Decid_lig_ns= ifelse(ENSANUT$f2_s6_623  =="usted y su esposo/pareja?", 1,
                             ifelse(ENSANUT$f2_s6_623  =="usted?",1,
                                    ifelse(ENSANUT$f2_s6_623  =="usted, esposo/pareja y médico?",1,
                                           ifelse(ENSANUT$f2_s6_623  =="médico?",1,0))))
######

table(ENSANUT$f2_s6_624)
### Opción si
ENSANUT$Tomar_misma_dec_ns= ifelse(ENSANUT$f2_s6_624  == "si", 1,0) 



table(ENSANUT$f2_s6_627)
### para espaciar o posponer los embarazos?
### no quiere tener hijos/as?
### ya no quiere tener más hijos/as?
ENSANUT$f2_s6_627[is.na(ENSANUT$f2_s6_627)] <- 0

ENSANUT$Raz_Uso_Metod_ns= ifelse(ENSANUT$f2_s6_627  == "para espaciar o posponer los embarazos?", 1,
                                 ifelse(ENSANUT$f2_s6_627  == "no quiere tener hijos/as?", 1,
                                        ifelse(ENSANUT$f2_s6_627  == "ya no quiere tener más hijos/as?", 1,0))) 

########

table(ENSANUT$f2_s6_628)
### ambos (usted y su pareja)? 
### sólo usted?

ENSANUT$Q_toma_decis_ns= ifelse(ENSANUT$f2_s6_628  == "ambos (usted y su pareja)?", 1, 
                                ifelse(ENSANUT$f2_s6_628  == "sólo usted?", 1,0))

########

table(ENSANUT$f2_s8_800f)
###  si

ENSANUT$Info_metod_ns= ifelse(ENSANUT$f2_s8_800f  == "si", 1,0) 


#######

table(ENSANUT$Diff_hijos)

###### Diferencia para necesidades satisfechas

ENSANUT$Hijos_deseo_ns = ifelse(ENSANUT$Diff_hijos >= 1, 1,0)
ENSANUT$Hijos_deseo_ns[is.na(ENSANUT$Hijos_deseo_ns)] <- 0                          


########## Uniendo las columnas que se requieren para definir NS


ENSANUT <- ENSANUT %>%
  dplyr::rowwise() %>%
  dplyr::mutate(Var_Necsat = max(Obten_MA_ns,        Donde_Obt_Metod_ns,  Decid_lig_ns,
                                 Tomar_misma_dec_ns, Raz_Uso_Metod_ns,    Q_toma_decis_ns,
                                 Info_metod_ns,      Hijos_deseo_ns,
                                 na.rm = TRUE))
######

table(ENSANUT$Var_Necsat)

####### Variable Necesidades Insatisfechas

ENSANUT$Nec_satisf= ifelse(ENSANUT$Var_Necsat == 1, 1,0)
table(ENSANUT$Nec_satisf)


########### Falla del método

table(ENSANUT$f2_s2_201)
### ¿ no quería embarazarse?     
### ¿quería esperar más tiempo? Espacio Tiempo

ENSANUT$Var_fallamet= ifelse(ENSANUT$f2_s2_201  =="¿ no quería embarazarse?", 1,
                             ifelse(ENSANUT$f2_s2_201  =="¿quería esperar más tiempo?",1,0))

table(ENSANUT$Var_fallamet)

##########

###### Grupos de Edad

ENSANUT$EdadQ<- ifelse(ENSANUT$f1_s2_3_1 <= 11, "< a 11",
                       ifelse(ENSANUT$f1_s2_3_1 <= 14, "12-14",
                              ifelse(ENSANUT$f1_s2_3_1 >= 15, ">= 15",0)))


table(ENSANUT$EdadQ)


####### Casada o Conviviente
table(ENSANUT$f2_s9_900)

ENSANUT$ec.casado      = ifelse(ENSANUT$f2_s9_900 == "casado?",1,0)
ENSANUT$ec.conviviente = ifelse(ENSANUT$f2_s9_900 == "unión de hecho?",1,
                                ifelse(ENSANUT$f2_s9_900 == "unión libre?",1,0))




####### Salvando Base de datos

saveRDS(ENSANUT, file = "ENSANUT.rds")   

