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
######################################################################################################################################################
###                             Limpiando la memoria 
######################################################################################################################################################

rm(list = ls(all=T))
gc()



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
library(haven)



######################################################################################################################################################
##                                    Definiendo rutas de entradas y salidas
########################################################################################################################

dir()

###########################################################################################################################

######################################################################################################################################################
##                                    Cargando Insumos
######################################################################################################################################################

# ### Descargando los datos
# 
# ecu_personas<-read_dta("ECU/2018/Metadatos/Data/1_BDD_ENS2018_f1_personas.dta")
# ecu_hogar<-read_dta("ECU/2018/Metadatos/Data/2_BDD_ENS2018_f1_hogar.dta")
# ecua_mujeres<-read_dta("ECU/2018/Metadatos/Data/4_BDD_ENS2018_f2_mef2.dta")
# ecu_mujeres<-read_dta("ECU/2018/Metadatos/Data/4_BDD_ENS2018_f2_mef2.dta")
# #
# demanda <- ecua_mujeres %>% select(id_per,entrcmc,imputed1)
# 
# Data_demandaSat = ecu_mujeres %>%
#   left_join(demanda, by = "id_per")

#### GUardando las bases en RDS

# ecu_personas<-saveRDS(ecu_personas,file ="ECU/2018/1.D6/Data/ecu_personas")
# ecu_hogar<-saveRDS(ecu_hogar,file =      "ECU/2018/1.D6/Data/ecu_hogar")
# ecu_mujeres<-saveRDS(ecu_mujeres,file ="ECU/2018/1.D6/Data/ecu_mujeres")
# Data_demandaSat<-saveRDS(Data_demandaSat,file ="ECU/2018/1.D6/Data/Data_demandaSat")
#
#  
# 
# 
# #### Cargando las bases las bases en RDS
# # 
# ecu_personas<-readRDS("ECU/2018/1.D6/Data/ecu_personas.rds")
# ecu_hogar<-readRDS("ECU/2018/1.D6/Data/ecu_hogar.rds")
# ecu_mujeres<-readRDS("ECU/2018/1.D6/Data/ecu_mujeres.rds")
# Data_demandaSat<-readRDS("ECU/2018/1.D6/Data/Data_demandaSat")

#############################################

### Uniendo las bases
# 
# ENSANUT_18 = Data_demandaSat  %>%
#   left_join(ecu_personas, by =c("id_per", "id_hogar")) %>%
#   left_join(ecu_hogar)

# ############# FILTRO - CREACION BASE
# 
# #### Filtro se hará cuando se vinculen los datos correspondientes al Censo 
# 
# saveRDS(ENSANUT_18, file = "ECU/2018/1.D6/Data/ENSANUT_18.rds")

ENSANUT_18 <- readRDS( file = "ECU/2018/1.D6/Data/ENSANUT_18.rds")

ENSANUT19 <-ENSANUT_18

##### Filtrando por mujeres entre 15 y 49 años

# ENSANUT19<-ENSANUT19 %>% ######### 41.113 mujeres de 15 años o más
#   filter(f2_s1_101 >= 15) 

names(ENSANUT19)
table(ENSANUT19$f2_s1_101) 

#### validación de uso de metodos

# ecu_mujeres<-ecu_mujeres %>% 
#   filter(f2_s1_101 >= 15) 
# 
# table(ecu_mujeres$f2_s6_603_1,useNA = "a")
# table(ecu_mujeres$f2_s6_603_2,useNA = "a")
# table(ecu_mujeres$f2_s6_603_3,useNA = "a")
# table(ecu_mujeres$f2_s6_603_4,useNA = "a")
# table(ecu_mujeres$f2_s6_603_5,useNA = "a")
# table(ecu_mujeres$f2_s6_603_6,useNA = "a")
# table(ecu_mujeres$f2_s6_603_7,useNA = "a")
# table(ecu_mujeres$f2_s6_603_8,useNA = "a")
# table(ecu_mujeres$f2_s6_603_9,useNA = "a")
# table(ecu_mujeres$f2_s6_603_10,useNA = "a")
# table(ecu_mujeres$f2_s6_603_11,useNA = "a")
# table(ecu_mujeres$f2_s6_603_12,useNA = "a")

######### Definiendo codigo UPM para obtener el municipio o canton
ENSANUT19$Cod_depto <-substr(ENSANUT19$upm, start = 1, stop = 2)
ENSANUT19$Cod_depto<-as.factor(ENSANUT19$Cod_depto)
table(ENSANUT19$Cod_depto)
class(ENSANUT19$Cod_depto)


ENSANUT19$Depto <- ifelse(ENSANUT19$Cod_depto == "01", "Azuay",
                     ifelse(ENSANUT19$Cod_depto == "02", "Bolivar",
                       ifelse(ENSANUT19$Cod_depto == "03", "Cañar",
                         ifelse(ENSANUT19$Cod_depto == "04", "Carchi",
                            ifelse(ENSANUT19$Cod_depto == "05", "Cotopaxi",
                             ifelse(ENSANUT19$Cod_depto == "06", "Chimborazo",
                              ifelse(ENSANUT19$Cod_depto == "07", "El Oro",
                               ifelse(ENSANUT19$Cod_depto == "08", "Esmeraldas",
                                 ifelse(ENSANUT19$Cod_depto == "09", "Guayas",
                                  ifelse(ENSANUT19$Cod_depto == "10", "Imbabura",
                                   ifelse(ENSANUT19$Cod_depto == "11", "Loja",
                                    ifelse(ENSANUT19$Cod_depto == "12", "Los Ríos",
                                     ifelse(ENSANUT19$Cod_depto == "13", "Manabi",
                                      ifelse(ENSANUT19$Cod_depto == "14", "Morona Santiago",
                                       ifelse(ENSANUT19$Cod_depto == "15", "Napo",
                                        ifelse(ENSANUT19$Cod_depto == "16", "Pastaza",
                                         ifelse(ENSANUT19$Cod_depto == "17", "Pichincha",
                                          ifelse(ENSANUT19$Cod_depto == "18", "Tungurahua",
                                           ifelse(ENSANUT19$Cod_depto == "19", "Zamora Chinchipe",
                                            ifelse(ENSANUT19$Cod_depto == "20", "Galapgos",
                                             ifelse(ENSANUT19$Cod_depto == "21", "Sucumbios",
                                              ifelse(ENSANUT19$Cod_depto == "22", "Orellana",
                                               ifelse(ENSANUT19$Cod_depto == "23", "Santo Domingo de los Tsachilas",
                                                ifelse(ENSANUT19$Cod_depto == "24", "Santa Elena",
                                                 ifelse(ENSANUT19$Cod_depto == "90", "Zonas no Delimitadas",NA)))))))))))))))))))))))))
                                                                                                

table(ENSANUT19$Depto, useNA = "a")

######### Definiendo codigo UPM para obtener el municipio o canton
ENSANUT19$mpio <-substr(ENSANUT19$upm, start = 1, stop = 4)
ENSANUT19$mpio<-as.factor(ENSANUT19$mpio)

#Borrando bases innecesarias 
# rm(ecu_personas,ecu_hogar,ecu_mujeres)

table(ENSANUT19$area)

############# seleccionando Variables para establecer los modelos 

###### Definiendo variable Unida y Sexualmente activa

#### Avtiva Sexualmente

table(ENSANUT19$f2_s8_832_semanas)
ENSANUT19$f2_s8_832_semanas[is.na(ENSANUT19$f2_s8_832_semanas)] <- 0
ENSANUT19$sexactiv  =ifelse(ENSANUT19$f2_s8_832_semanas <=4,1,0)
table(ENSANUT19$sexactiv)

#### Unida

table(ENSANUT19$f2_s9_900, useNA = "a")
ENSANUT19$f2_s9_900[is.na(ENSANUT19$f2_s9_900)] <- 0
ENSANUT19$unida =ifelse(ENSANUT19$f2_s9_900 %in%c(1:3),1,0)
table(ENSANUT19$unida)

######## Construcción Aciva Sexual y unida

####     Mujeres activas sex y unidas  

ENSANUT19 <- ENSANUT19 %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    ActivSexUnida= max(
      sexactiv,
      unida,
      na.rm = TRUE),
    
  )

table(ENSANUT19$ActivSexUnida)


###### Definiendo variable edad

ENSANUT19 <- ENSANUT19 %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    Grupo_edad = case_when(
      f1_s2_3_1 %in% 0:14 ~ "1",       # 5 a 14
      f1_s2_3_1 %in% 15:29 ~ "2",      # 15 a 29
      f1_s2_3_1 %in% 30:44 ~ "3",      # 30 a 44
      f1_s2_3_1 %in% 45:64 ~ "4",      # 45 a 64
      TRUE ~ "5"
    ) 
  )     

###### Construcción los indicadores de interés

#table(ecu_mujeres$f2_s6_603_2, useNA = "a")

table(ENSANUT19$f2_s6_603_1,useNA = "a")
ENSANUT19$f2_s6_603_1[is.na(ENSANUT19$f2_s6_603_1)] <- 0
ENSANUT19$m_Vasectomia   =ifelse(ENSANUT19$f2_s6_603_1  ==1,1,0)
table(ENSANUT19$m_Vasectomia)

table(ENSANUT19$f2_s6_603_2)
ENSANUT19$f2_s6_603_2[is.na(ENSANUT19$f2_s6_603_2)] <- 0
ENSANUT19$m_Ligadura=ifelse(ENSANUT19$f2_s6_603_2  ==1,1,0)                                  
table(ENSANUT19$m_Ligadura)


table(ENSANUT19$f2_s6_603_3)
ENSANUT19$f2_s6_603_3[is.na(ENSANUT19$f2_s6_603_3)] <- 0
ENSANUT19$m_Implante     =ifelse(ENSANUT19$f2_s6_603_3  ==1,1,0)
table(ENSANUT19$m_Implante)

table(ENSANUT19$f2_s6_603_4)
ENSANUT19$f2_s6_603_4[is.na(ENSANUT19$f2_s6_603_4)] <- 0
ENSANUT19$m_Inyeccion    =ifelse(ENSANUT19$f2_s6_603_4  ==1,1,0)
table(ENSANUT19$m_Inyeccion)

table(ENSANUT19$f2_s6_603_5)
ENSANUT19$f2_s6_603_5[is.na(ENSANUT19$f2_s6_603_5)] <- 0
ENSANUT19$m_DIU          =ifelse(ENSANUT19$f2_s6_603_5  ==1,1,0)
table(ENSANUT19$m_DIU)

table(ENSANUT19$f2_s6_603_6)
ENSANUT19$f2_s6_603_6[is.na(ENSANUT19$f2_s6_603_6)] <- 0
ENSANUT19$m_Pildora      =ifelse(ENSANUT19$f2_s6_603_6  ==1,1,0)
table(ENSANUT19$m_Pildora)

table(ENSANUT19$f2_s6_603_7)
ENSANUT19$f2_s6_603_7[is.na(ENSANUT19$f2_s6_603_7)] <- 0
ENSANUT19$m_Condon_fem   =ifelse(ENSANUT19$f2_s6_603_7  ==1,1,0)
table(ENSANUT19$m_Condon_fem)

table(ENSANUT19$f2_s6_603_8)
ENSANUT19$f2_s6_603_8[is.na(ENSANUT19$f2_s6_603_8)] <- 0
ENSANUT19$m_Condon_mas   =ifelse(ENSANUT19$f2_s6_603_8  ==1,1,0)
table(ENSANUT19$m_Condon_mas)

table(ENSANUT19$f2_s6_603_9)
ENSANUT19$f2_s6_603_9[is.na(ENSANUT19$f2_s6_603_9)] <- 0
ENSANUT19$m_Pastilla_eme =ifelse(ENSANUT19$f2_s6_603_9  ==1,1,0)
table(ENSANUT19$m_Pastilla_eme)

table(ENSANUT19$f2_s6_603_10)
ENSANUT19$f2_s6_603_10[is.na(ENSANUT19$f2_s6_603_10)] <- 0
ENSANUT19$m_Ritmo        =ifelse(ENSANUT19$f2_s6_603_10 ==1,1,0)
table(ENSANUT19$m_Ritmo)

table(ENSANUT19$f2_s6_603_11)
ENSANUT19$f2_s6_603_11[is.na(ENSANUT19$f2_s6_603_11)] <- 0
ENSANUT19$m_Retiro       =ifelse(ENSANUT19$f2_s6_603_11 ==1,1,0)
table(ENSANUT19$m_Retiro)

table(ENSANUT19$f2_s6_603_12)
ENSANUT19$f2_s6_603_12[is.na(ENSANUT19$f2_s6_603_12)] <- 0
ENSANUT19$m_Lactancia    =ifelse(ENSANUT19$f2_s6_603_12 ==1,1,0)
table(ENSANUT19$m_Lactancia)

table(ENSANUT19$f2_s6_603_13)
ENSANUT19$f2_s6_603_13[is.na(ENSANUT19$f2_s6_603_13)] <- 0
ENSANUT19$m_Otro    =ifelse(ENSANUT19$f2_s6_603_13 ==1,1,0)
table(ENSANUT19$m_Otro)


######## Construcción usametodo y usamoderno

####     usametodo  

ENSANUT19 <- ENSANUT19 %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    usametodo= max(
      m_Vasectomia,
      m_Ligadura,
      m_Implante,
      m_Inyeccion,
      m_DIU,
      m_Pildora,
      m_Condon_fem,
      m_Condon_mas,
      m_Pastilla_eme,
      m_Ritmo,
      m_Retiro,
      m_Lactancia,
      m_Otro,
      na.rm = TRUE),
    
    usamoderno= max(
      m_Vasectomia,
      m_Ligadura,
      m_Implante,
      m_Inyeccion,
      m_DIU,
      m_Pildora,
      m_Condon_fem,
      m_Condon_mas,
      m_Pastilla_eme,
      na.rm = TRUE      
    )
  )

#### Tablas verificación

### Usametodo
ENSANUT19$usam<- ifelse(  ENSANUT19$f2_s6_603_1==1 | 
                            ENSANUT19$f2_s6_603_2==1 |
                            ENSANUT19$f2_s6_603_3==1 |
                            ENSANUT19$f2_s6_603_4==1 |
                            ENSANUT19$f2_s6_603_5==1 |
                            ENSANUT19$f2_s6_603_6==1 |
                            ENSANUT19$f2_s6_603_7==1 |
                            ENSANUT19$f2_s6_603_8==1 |
                            ENSANUT19$f2_s6_603_9==1 |
                            ENSANUT19$f2_s6_603_10==1 |
                            ENSANUT19$f2_s6_603_11==1 |
                            ENSANUT19$f2_s6_603_12==1 |
                            ENSANUT19$f2_s6_603_13==1,1,0)

table(ENSANUT19$usametodo)
table(ENSANUT19$usam)


### Usamoderno
ENSANUT19$usamod<- ifelse(  ENSANUT19$f2_s6_603_1==1 | 
                              ENSANUT19$f2_s6_603_2==1 |
                              ENSANUT19$f2_s6_603_3==1 |
                              ENSANUT19$f2_s6_603_4==1 |
                              ENSANUT19$f2_s6_603_5==1 |
                              ENSANUT19$f2_s6_603_6==1 |
                              ENSANUT19$f2_s6_603_7==1 |
                              ENSANUT19$f2_s6_603_8==1 |
                              ENSANUT19$f2_s6_603_9==1,1,0)

table(ENSANUT19$usamod)
table(ENSANUT19$usamoderno)

table(ENSANUT19$unida, ENSANUT19$usametodo, useNA = "a")
table(ENSANUT19$sexactiv, ENSANUT19$usametodo, useNA = "a")
table(ENSANUT19$ActivSexUnida, ENSANUT19$usametodo, useNA = "a")
table(ENSANUT19$usametodo, ENSANUT19$usamoderno, useNA = "a")


############################################
##################### 

##### Definiendo necesidades insatisfechas NI de acuerdo al INEC

####  T14_i3. Porcentaje de mujeres en edad fértil con necesidad insatisfecha 
#### 		  de planificación familiar 

####### Nota: deben tomarse las mujeres mayores de 15 años

### edad
table(ENSANUT19$f2_s1_101)

# ENSANUT19<-ENSANUT19 %>% ######### 41.113 mujeres de 15 años o más
#   filter(f2_s1_101 >= 15) 
# ENSANUT19    

#### Se modifica el Estado Civil

table(ENSANUT19$f2_s9_900, useNA = "a")
table(ENSANUT19$f2_s9_901, useNA = "a")


#### 
ENSANUT19$gen_civil<- ifelse(ENSANUT19$f2_s9_900 >= 1 & ENSANUT19$f2_s9_900 <= 3, 1,
                             ifelse(ENSANUT19$f2_s9_900 == 7 & ENSANUT19$f2_s9_901 == 2,3,
                                    ifelse(ENSANUT19$f2_s9_900 == 7, 2,
                                           ifelse(ENSANUT19$f2_s9_900 >= 4 & ENSANUT19$f2_s9_900 <= 6, 2,NA))))


table(ENSANUT19$gen_civil, useNA = "a")

##### Mujeres activas sexualmente

table(ENSANUT19$f2_s8_832_dias, useNA = "a")
table(ENSANUT19$f2_s8_832_semanas, useNA = "a")
table(ENSANUT19$f2_s8_832_meses, useNA = "a")
table(ENSANUT19$f2_s8_832_anios, useNA = "a")

ENSANUT19b<-ENSANUT19 %>% 
  filter(f2_s8_832_anios <55)

ENSANUT19$ActivS = ((ENSANUT19$f2_s8_832_dias) +
                      (ENSANUT19$f2_s8_832_semanas * 7) +
                      (ENSANUT19$f2_s8_832_meses *30) +
                      (ENSANUT19$f2_s8_832_anios*365))


ENSANUT19$gen_activas<- ifelse(ENSANUT19$f2_s8_832_anios<55,ENSANUT19$ActivS, NA)
#ENSANUT19$gen_activas[is.na(ENSANUT19$gen_activas)] <- 1000
table(ENSANUT19$gen_activas, useNA = "a")

MAS30<-ENSANUT19 %>% ######### 24.345 mujeres Activas sexualmente
  filter(gen_activas %in% c(0:30)) 
MAS30 

table(MAS30$gen_activas, useNA = "a")

#### Activas sex

ENSANUT19$musexAct<- ifelse(ENSANUT19$gen_activas %in% c(0:30),1,NA)
table(ENSANUT19$musexAct, useNA = "a")

# ### Casadas y activas sexualmente

ENSANUT19$musa<- ifelse(ENSANUT19$gen_activas %in% c(0:30) &
                          ENSANUT19$gen_civil > 1, 1,NA)
table(ENSANUT19$musa, useNA = "a")

# 
# ###### Mujeres Activas Sexualmente
# 
# ### 1. Otros
# ### 2. Casadas o Unidas y Activas sexualmente
# 

ENSANUT19$unidas_ActSex<- ifelse(ENSANUT19$gen_civil == 1, 1,
                                 ifelse(ENSANUT19$musa == 1,2, NA))

table(ENSANUT19$unidas_ActSex, useNA = "a")


###### Mujeres Usuarias método moderno

ENSANUT19$usuarias<- ifelse(ENSANUT19$f2_s6_604 ==1 & 
                              ENSANUT19$f2_s6_620<=9, 1,NA)

table(ENSANUT19$usuarias, useNA = "a")

###### Mujeres NO Usuarias

ENSANUT19$no_usuarias<- ifelse(ENSANUT19$f2_s6_604 >=2, 1,NA)

table(ENSANUT19$no_usuarias, useNA = "a")


###### Mujeres Embrazadas y Amenorreicas
####  Embarazadas y Amenorreicas (Nunca han usado o no usan actualmente)

## * Tiempo desde el último nacimiento (diferencia en CMC, meses)
names(ENSANUT19)

ENSANUT19$tsinceb<- ENSANUT19$entrcmc.x-ENSANUT19$imputed1.x
table(ENSANUT19$tsinceb, useNA = "a")

#### * Tiempo desde el último período en meses


ENSANUT19$tsincep1<- as.integer((ENSANUT19$f2_s8_843_dias/30 + 
                                ENSANUT19$f2_s8_843_semanas/4 +
                                ENSANUT19$f2_s8_843_meses + 
                                ENSANUT19$f2_s8_843_anios*12))
table(ENSANUT19$tsincep1, useNA = "a")
sum(table(ENSANUT19$tsincep1))


ENSANUT19$tsincep <- ifelse(ENSANUT19$f2_s8_843_dias<77,ENSANUT19$f2_s8_843_dias,NA)
                     
sum(table(ENSANUT19$tsincep))
table(ENSANUT19$tsincep, useNA = "a")

### *Solo amenorrea de menos de 24 meses

ENSANUT19$gen_embarazo<- ifelse(ENSANUT19$no_usuarias ==1 & 
                                  ENSANUT19$f2_s2_200==1, 1,
                                ifelse(ENSANUT19$no_usuarias ==1 & 
                                         ENSANUT19$f2_s2_200>=2 &
                                         ENSANUT19$f2_s8_843_dias ==77 &
                                         ENSANUT19$tsinceb, 2,NA))

#ENSANUT19$gen_embarazo[is.na(ENSANUT19$gen_embarazo)] <- 0
table(ENSANUT19$gen_embarazo, useNA = "a")

### renombrando embarazo

ENSANUT19$emm<-ENSANUT19$gen_embarazo
table(ENSANUT19$emm, useNA = "a")


####***1.2.1.1. Planeado
####*** Tiempo espera para embarazo 01 hubiera esquerido esperar por embarazo 
####*** 02 quisiera esperar antes del próximo hijo

###### Mujeres Espera del embarazo---------------- 


ENSANUT19$gen_espera01 = ifelse(ENSANUT19$f2_s2_202_1< 88,
                                ((ENSANUT19$f2_s2_202_1) + (ENSANUT19$f2_s2_202_2*12)),NA)

#ENSANUT19$gen_espera01[is.na(ENSANUT19$gen_espera01)] <- 0
table(ENSANUT19$gen_espera01, useNA = "a")
summary(ENSANUT19$gen_espera01)                                

ENSANUT19$gen_espera02 = ifelse(ENSANUT19$f2_s7_702 < 30,
                                ((ENSANUT19$f2_s7_702) + (ENSANUT19$f2_s7_702_1*12)),NA)

#ENSANUT19$gen_espera02[is.na(ENSANUT19$gen_espera02)] <- 0
table(ENSANUT19$gen_espera02, useNA = "a")
summary(ENSANUT19$gen_espera02)  

###### Validaciones

table(ENSANUT19$f2_s7_702, useNA = "a")
table(ENSANUT19$f2_s7_702_1, useNA = "a")


##### 
ENSANUT19$espera24 <- ifelse(ENSANUT19$gen_espera01 > 24 | is.na(ENSANUT19$gen_espera01), 1,0)
table(ENSANUT19$espera24)


##### Planeado-----------------------

### prueba 

ENSANUT19$p_plan <- ifelse(ENSANUT19$no_usuarias == 1 &
                             ENSANUT19$gen_embarazo == 1 &
                             ENSANUT19$f2_s2_201 == 1, 1,
                           
                           ifelse(ENSANUT19$no_usuarias == 1 &
                                    ENSANUT19$gen_embarazo == 1 &
                                    ENSANUT19$f2_s2_201==2 &
                                    ENSANUT19$gen_espera01 <=24, 1,

                            ifelse(ENSANUT19$no_usuarias == 1 &
                                     ENSANUT19$gen_embarazo == 2 &
                                     ENSANUT19$f2_s7_701==1 &
                                     ENSANUT19$gen_espera02 <=24,1,0)))


table(ENSANUT19$p_plan)


ENSANUT19$emb_planeado <- ifelse(ENSANUT19$no_usuarias == 1 &
                                   ENSANUT19$gen_embarazo == 1 &
                                   ENSANUT19$f2_s2_201 == 1, 1,
                                 
                                 ifelse(ENSANUT19$no_usuarias == 1 &
                                          ENSANUT19$gen_embarazo == 1 &
                                          ENSANUT19$f2_s2_201==2 &
                                          ENSANUT19$gen_espera01 <=24, 1,
                                        
                                        ifelse(ENSANUT19$no_usuarias == 1 &
                                                 ENSANUT19$gen_embarazo == 2 &
                                                 ENSANUT19$f2_s7_701==1 &
                                                 ENSANUT19$gen_espera02 <=24, 1, NA)))

table(ENSANUT19$emb_planeado, useNA = "a")


##### Inoportuno -----------------------

ENSANUT19$emb_inoportuno <- ifelse(ENSANUT19$no_usuarias %in% 1 &
                                     ENSANUT19$gen_embarazo%in% 2 &
                                     ENSANUT19$f2_s7_701%in% 1 &
                                     (ENSANUT19$gen_espera02 >24 | is.na(ENSANUT19$gen_espera02)), 1, 
                                   
                                   ifelse(ENSANUT19$no_usuarias %in% 1 &
                                            ENSANUT19$gen_embarazo %in% 1 &
                                            ENSANUT19$f2_s2_201 %in% 2 & (ENSANUT19$gen_espera01 >24 | is.na(ENSANUT19$gen_espera01)),1,NA))

table(ENSANUT19$emb_inoportuno, useNA = "a")


##### No deseado -----------------------

prueb_no_deseado<- ifelse(ENSANUT19$no_usuarias == 1 &
                            ENSANUT19$gen_embarazo == 1 &
                            ENSANUT19$f2_s2_201 == 3, 1,
                          
                   ifelse(ENSANUT19$no_usuarias == 1 &
                            ENSANUT19$gen_embarazo == 2 &
                            ENSANUT19$f2_s7_701==2, 1,NA))

table(prueb_no_deseado, useNA = "a")



ENSANUT19$emb_No_deseado <- ifelse(ENSANUT19$no_usuarias == 1 &
                                     ENSANUT19$gen_embarazo == 1 &
                                     ENSANUT19$f2_s2_201 == 3, 1,
                                   
                                   ifelse(ENSANUT19$no_usuarias == 1 &
                                            ENSANUT19$gen_embarazo == 2 &
                                            ENSANUT19$f2_s7_701==2, 1,
                                          
                                          ifelse(ENSANUT19$no_usuarias == 1 &
                                                   ENSANUT19$gen_embarazo == 2 &
                                                   ENSANUT19$f2_s7_701 == 4, 1, NA)))


table(ENSANUT19$emb_No_deseado, useNA = "a")


##### No sabe No responde NS/NR -----------------------

ENSANUT19$nsnr <- ifelse(ENSANUT19$gen_embarazo <= 2 &
                           ENSANUT19$emb_planeado != 1 &
                           ENSANUT19$emb_inoportuno != 1 &
                           ENSANUT19$emb_No_deseado != 1, 1, NA)

table(ENSANUT19$nsnr, useNA = "a")


###### Mujeres NO Embrazadas Ni Amenorreicas

table(ENSANUT19$f2_s2_200, useNA = "a")
table(ENSANUT19$no_usuarias, useNA = "a")
table(ENSANUT19$f2_s8_843_dias, useNA = "a")
summary(ENSANUT19$f2_s8_843_dias, useNA = "a")

ENSANUT19$prueba<- ifelse(ENSANUT19$f2_s8_843_dias != 77 , 1,  NA)
table(ENSANUT19$prueba)


ENSANUT19$no_emb_am <- ifelse( ENSANUT19$no_usuarias == 1 &
                                 ENSANUT19$f2_s2_200 >= 2 &
                                 (ENSANUT19$f2_s8_843_dias != 77| is.na(ENSANUT19$f2_s8_843_dias)), 1, NA)

table(ENSANUT19$no_emb_am, useNA = "a")


################################### Mujeres Infertiles

###### Tiempo retraso en la regla

table(ENSANUT19$f2_s8_843_dias)
table(ENSANUT19$f2_s8_843_semanas)
table(ENSANUT19$f2_s8_843_meses)
table(ENSANUT19$f2_s8_843_anios)


ENSANUT19$ctr <-as.integer(ENSANUT19$f2_s8_843_dias+
                             ENSANUT19$f2_s8_843_semanas * 7 +
                             ENSANUT19$f2_s8_843_meses *30 +              
                             ENSANUT19$f2_s8_843_anios*365)
                          
ENSANUT19$ctr<- as.integer(ENSANUT19$ctr/30)

ENSANUT19$tiempo_regla <- ifelse(ENSANUT19$f2_s8_843_dias < 77, ENSANUT19$ctr, NA)
table(ENSANUT19$tiempo_regla, useNA = "a")


###### Mujeres Retraso


ENSANUT19$gen_retraso <- ifelse(ENSANUT19$no_usuarias == 1 &
                                  ENSANUT19$no_emb_am == 1 &
                                  (ENSANUT19$tiempo_regla >=3 | is.na(ENSANUT19$tiempo_regla)), 1,NA)

#ENSANUT19$gen_retraso[is.na(ENSANUT19$gen_retraso)] <- 0
table(ENSANUT19$gen_retraso, useNA = "a")


###### Mujeres Menopausica

table(ENSANUT19$no_usuarias, useNA = "a")
table(ENSANUT19$no_emb_am, useNA = "a")
table(ENSANUT19$gen_retraso, useNA = "a")
table(ENSANUT19$f2_s7_701, useNA = "a")
table(ENSANUT19$f2_s6_612, useNA = "a")

#########

### Prueba

ENSANUT19$prueba <-  ifelse(ENSANUT19$no_usuarias == 1,1,NA)
table(ENSANUT19$prueba, useNA = "a")

ENSANUT19$prueba <-  ifelse(ENSANUT19$no_usuarias == 1 & 
                              ENSANUT19$no_emb_am ==  1,1,NA)
table(ENSANUT19$prueba, useNA = "a")

ENSANUT19$prueba <-  ifelse(ENSANUT19$no_usuarias == 1 & 
                              ENSANUT19$no_emb_am ==  1 &
                              (ENSANUT19$gen_retraso != 1 | is.na(ENSANUT19$gen_retraso)) &
                              (ENSANUT19$f2_s7_701==3 | ENSANUT19$f2_s6_612==2), 1,NA)
table(ENSANUT19$prueba, useNA = "a")
                              
##### definiendo menopausia                  

ENSANUT19$gen_menopausia <- ifelse(ENSANUT19$no_usuarias == 1 & 
                                     ENSANUT19$no_emb_am ==  1 &
                                     (ENSANUT19$gen_retraso != 1 | is.na(ENSANUT19$gen_retraso)) &
                                     (ENSANUT19$f2_s7_701==3 | ENSANUT19$f2_s6_612==2), 1,NA)

#ENSANUT19$gen_menopausia[is.na(ENSANUT19$gen_menopausia)] <- 0
table(ENSANUT19$gen_menopausia, useNA = "a")

##### *----* Unidas o sexualmente activas Infertiles*----*

ENSANUT19$mes_entrevista <- ENSANUT19$fecha_mes
ENSANUT19$anio_entrevista <- ENSANUT19$fecha_anio

#### Hijo Muerto

#________EJEMPLO mes_ultimo== ifelse(ultimo==2,f2_s2_213_1, NA )

ENSANUT19$f2_s2_213_2[is.na(ENSANUT19$f2_s2_213_2)] <- 0
table(ENSANUT19$f2_s2_213_2, useNA = "a")

ENSANUT19$f2_s2_213_1[is.na(ENSANUT19$f2_s2_213_1)] <- 0
table(ENSANUT19$f2_s2_213_1, useNA = "a")

ENSANUT19$hm<-((ENSANUT19$f2_s2_213_2*100)+(ENSANUT19$f2_s2_213_1))
table(ENSANUT19$hm, useNA = "a")

ENSANUT19$hm1<-ifelse(ENSANUT19$f2_s2_213_2 == 8888 | ENSANUT19$f2_s2_213_2 == 7777,0,
                      ENSANUT19$f2_s2_213_2)
ENSANUT19$hm2<-ifelse(ENSANUT19$f2_s2_213_1 == 88 | ENSANUT19$f2_s2_213_1 == 77,0,
                      ENSANUT19$f2_s2_213_1)

ENSANUT19$hijo_muerto<-ifelse(ENSANUT19$f2_s2_213_2 == 8888 | ENSANUT19$f2_s2_213_1== 88, 0,
                              ifelse(ENSANUT19$f2_s2_213_2 == 7777 | ENSANUT19$f2_s2_213_1== 77, 0, 
                                     ENSANUT19$hm))

table(ENSANUT19$hijo_muerto, useNA = "a")


#### _________  **Hijos nacidos vivos

ENSANUT19$f2_s2_218_1_b3[is.na(ENSANUT19$f2_s2_218_1_b3)] <- 0
table(ENSANUT19$f2_s2_218_1_b3, useNA = "a")

ENSANUT19$f2_s2_218_1_b2[is.na(ENSANUT19$f2_s2_218_1_b2)] <- 0
table(ENSANUT19$f2_s2_218_1_b2, useNA = "a")

ENSANUT19$hv<-((ENSANUT19$f2_s2_218_1_b3*100)+(ENSANUT19$f2_s2_218_1_b2))
table(ENSANUT19$hv, useNA = "a")

ENSANUT19$hijo_vivo<-ifelse(ENSANUT19$f2_s2_218_1_b2 == 88 | ENSANUT19$f2_s2_218_1_b3== 8888, 0,
                            ENSANUT19$hv)

table(ENSANUT19$hijo_vivo, useNA = "a")

##### *----* Ultimo Embarazo*----*

## Ultimo Embarazo
ENSANUT19$ultimo <- ifelse(ENSANUT19$hijo_vivo > ENSANUT19$hijo_muerto, 1,
                           ifelse(ENSANUT19$hijo_muerto > ENSANUT19$hijo_vivo, 2,NA))
table(ENSANUT19$ultimo, useNA = "a")

## Mes Ultimo Embarazo
ENSANUT19$mes_ultimo<-ifelse(ENSANUT19$ultimo == 2, ENSANUT19$f2_s2_213_1,
                             ifelse(ENSANUT19$ultimo == 1, ENSANUT19$f2_s2_218_1_b2, NA))
table(ENSANUT19$mes_ultimo, useNA = "a")

## Año Ultimo Embarazo
ENSANUT19$anio_ultimo<-ifelse(ENSANUT19$ultimo == 2, ENSANUT19$f2_s2_213_2,
                              ifelse(ENSANUT19$ultimo == 1, ENSANUT19$f2_s2_218_1_b3, NA))
table(ENSANUT19$anio_ultimo, useNA = "a")

### fecha de union la pareja (casadas o unidas)

ENSANUT19$mes_union <- ifelse(ENSANUT19$f2_s9_900 <=3 & ENSANUT19$f2_s9_904_2 != 77, 
                              ENSANUT19$f2_s9_904_2, NA)
table(ENSANUT19$mes_union, useNA = "a")

ENSANUT19$anio_union <- ifelse(ENSANUT19$f2_s9_900 <=3 & ENSANUT19$f2_s9_904_3 != 7777, 
                               ENSANUT19$f2_s9_904_3, NA)
table(ENSANUT19$anio_union, useNA = "a")

##*-*No uso del metodo anticonceptivo

ENSANUT19$no_uso <- ifelse(ENSANUT19$f2_s6_604==2, 1, NA)
table (ENSANUT19$no_uso, useNA = "a")

################################################# creando a

ENSANUT19$a <- ENSANUT19$mes_entrevista+(ENSANUT19$anio_entrevista*12)
table(ENSANUT19$a, useNA = "a")
summary(ENSANUT19$a, useNA = "a") #### ok

################################################# creando b

ENSANUT19$b <- ENSANUT19$mes_ultimo+(ENSANUT19$anio_ultimo*12)
table(ENSANUT19$b, useNA = "a")
summary(ENSANUT19$b, useNA = "a") #### ok

################################################# creando c

ENSANUT19$c <- ENSANUT19$no_uso
table(ENSANUT19$c, useNA = "a") ### ok

################################################# creando d

ENSANUT19$d <- ENSANUT19$mes_union+(ENSANUT19$anio_union*12)
table(ENSANUT19$d, useNA = "a")
summary(ENSANUT19$d, useNA = "a") ### ok

################################################# creando a_d

### prueba sin el condicional
ENSANUT19$prub_a_d <- (ENSANUT19$a-ENSANUT19$d)/12
table (ENSANUT19$prub_a_d, useNA = "a")
summary (ENSANUT19$prub_a_d, useNA = "a")

###  prueba para embarazo table de embarazo
table (ENSANUT19$gen_embarazo, useNA = "a")

ENSANUT19$emb_P1= ifelse(ENSANUT19$gen_embarazo !=1,1,0)
table (ENSANUT19$emb_P1, useNA = "a")
                       
### GENERANDO LA VARIBALE                         
ENSANUT19$a_d= ifelse((ENSANUT19$gen_embarazo !=1 | is.na(ENSANUT19$gen_embarazo)),
                             (ENSANUT19$a-ENSANUT19$d)/12, NA )

table(ENSANUT19$a_d, useNA = "a")
summary(ENSANUT19$a_d, useNA = "a") ### ok


################################################# creando a_b

ENSANUT19$a_b= ifelse((ENSANUT19$gen_embarazo !=1 | is.na(ENSANUT19$gen_embarazo)),
                      (ENSANUT19$a-ENSANUT19$b)/12,NA )
table(ENSANUT19$a_b, useNA = "a")
sum(table(ENSANUT19$a_b, useNA = "a"))
summary(ENSANUT19$a_b) ### ok

###########

### infertil

table(ENSANUT19$a_d, useNA = "a")
table(ENSANUT19$a_b, useNA = "a")
table(ENSANUT19$c, useNA = "a")
table(ENSANUT19$gen_embarazo, useNA = "a")
table(ENSANUT19$f2_s6_604, useNA = "a")
table(ENSANUT19$gen_retraso, useNA = "a")
table(ENSANUT19$gen_menopausia, useNA = "a")

#################################################################### Pruebas

ENSANUT19$prueba1 <-  ifelse(ENSANUT19$a_d >=5, 1 ,0)
table(ENSANUT19$prueba1, useNA = "a")


ENSANUT19$prue_Inf <-  ifelse((ENSANUT19$gen_embarazo !=1 | is.na(ENSANUT19$gen_embarazo)) &
                                ENSANUT19$f2_s6_604 ==2 & 
                                (ENSANUT19$gen_retraso !=1 | is.na(ENSANUT19$gen_retraso)) &
                                (ENSANUT19$gen_menopausia !=1 | is.na(ENSANUT19$gen_menopausia)) &
                                (ENSANUT19$a_d >=5| is.na(ENSANUT19$a_d)) & 
                                (ENSANUT19$a_b>=5 | is.na(ENSANUT19$a_b)) & 
                                ENSANUT19$c ==  1, 1, NA) 

table(ENSANUT19$prue_Inf, useNA = "a")

ENSANUT19$infertil_01<- ifelse((ENSANUT19$gen_embarazo !=1 | is.na(ENSANUT19$gen_embarazo)) &
                                 ENSANUT19$f2_s6_604 ==2 & 
                               (ENSANUT19$gen_retraso !=1 | is.na(ENSANUT19$gen_retraso)) &
                               (ENSANUT19$gen_menopausia !=1 | is.na(ENSANUT19$gen_menopausia)) &
                                 (ENSANUT19$a_d >=5| is.na(ENSANUT19$a_d)) & 
                                 (ENSANUT19$a_b>=5 | is.na(ENSANUT19$a_b)) & 
                                 ENSANUT19$c ==  1, 1, NA) 

#ENSANUT19$infertil_01[is.na(ENSANUT19$infertil_01)] <- 0
table(ENSANUT19$infertil_01, useNA = "a")

##### *----* Total mujeres Infertiles*----*

table(ENSANUT19$no_usuarias,useNA = "a") #### NA Stata 24419 / R == 0 / 24419
table(ENSANUT19$no_emb_am,useNA = "a")   #### Ok
table(ENSANUT19$gen_retraso,useNA = "a") #### NA Stata 39462 / R == 0 / 39462
table(ENSANUT19$gen_menopausia,useNA = "a")  #### NA Stata 40955 / R == 0 / 40955
table(ENSANUT19$infertil_01,useNA = "a") #### Ok
table(ENSANUT19$f2_s7_701,useNA = "a") #### Ok
table(ENSANUT19$f2_s7_702,useNA = "a") #### Ok
table(ENSANUT19$f2_s6_612, useNA = "a")#### Ok 


###### Prueba 2

ENSANUT19$infertil_1<- ifelse(ENSANUT19$no_usuarias %in% 1 & 
                                ENSANUT19$no_emb_am %in% 1 & 
                                ENSANUT19$gen_retraso %in% 1,1,
                              ifelse(ENSANUT19$no_usuarias %in% 1 & 
                                       ENSANUT19$no_emb_am %in%1 & 
                                       (ENSANUT19$gen_menopausia %in% 1 | ENSANUT19$f2_s7_702 %in% 44),2,
                                     ifelse(ENSANUT19$no_usuarias %in% 1 & 
                                              ENSANUT19$no_emb_am %in% 1 & 
                                              ENSANUT19$infertil_01 %in% 1,3,
                                            ifelse(ENSANUT19$no_usuarias %in% 1 & 
                                                     ENSANUT19$no_emb_am %in% 1 & 
                                                     ENSANUT19$f2_s7_701 %in% 4, 4,
                                                   ifelse(ENSANUT19$no_usuarias %in% 1 & 
                                                            ENSANUT19$no_emb_am %in% 1 & 
                                                            ENSANUT19$f2_s6_612 <=4, 5,
                                                          NA)))))


table (ENSANUT19$infertil_1, useNA = "a")


# ###### Construccion variable
# 
# ENSANUT19$infertil_1<- ifelse(ENSANUT19$no_usuarias %in% 1 & 
#                                 ENSANUT19$no_emb_am %in% 1 & 
#                                 ENSANUT19$gen_retraso %in% 1,1,
#                        
#                        ifelse(ENSANUT19$no_usuarias %in% 1 & 
#                               ENSANUT19$no_emb_am %in% 1 &
#                               (ENSANUT19$gen_menopausia %in% 1 | ENSANUT19$f2_s7_702==44),2,
#                        
#                        ifelse(ENSANUT19$no_usuarias %in% 1 & 
#                                 ENSANUT19$no_emb_am %in% 1 & 
#                                 ENSANUT19$infertil_01 %in% 1,3,
#                        
#                        ifelse(ENSANUT19$no_usuarias %in% 1 & 
#                                 ENSANUT19$no_emb_am %in% 1 & 
#                                 ENSANUT19$f2_s7_701 %in% 4, 4,
#                        
#                        ifelse(ENSANUT19$no_usuarias %in% 1 & 
#                                 ENSANUT19$no_emb_am %in% 1 & 
#                                 ENSANUT19$f2_s6_612 <=4, 5, NA)))))


ENSANUT19$infertil<- ENSANUT19$infertil_1

table(ENSANUT19$infertil, useNA = "a")

##### *----* Total mujeres fertiles*----*

table(ENSANUT19$no_usuarias)
table(ENSANUT19$no_emb_am)
table(ENSANUT19$infertil)


ENSANUT19$fertil= ifelse(ENSANUT19$no_usuarias %in% 1 & 
                           ENSANUT19$no_emb_am %in% 1 & 
                           (ENSANUT19$infertil > 5 | is.na(ENSANUT19$infertil)),1,NA)

table(ENSANUT19$fertil, useNA = "a")


##### *----* Desea Después*----*

ENSANUT19$desea_despues<- ifelse(ENSANUT19$no_usuarias==1 & 
                                   ENSANUT19$no_emb_am==1 & ENSANUT19$fertil==1 &
                                   ENSANUT19$f2_s7_701==1 & 
                                   (ENSANUT19$f2_s7_702_1>2 & ENSANUT19$f2_s7_702!=33),1,NA)

table(ENSANUT19$desea_despues, useNA = "a")


##### *----* No Desea*----*

table(ENSANUT19$f2_s7_701, useNA = "a")

## prueba
ENSANUT19$prt<-ifelse(ENSANUT19$no_usuarias==1 & 
                        ENSANUT19$no_emb_am==1 & 
                        ENSANUT19$fertil==1, 1,NA)

table(ENSANUT19$prt, useNA = "a")

####
ENSANUT19$ne<-ifelse(ENSANUT19$f2_s7_701>=2 | is.na(ENSANUT19$f2_s7_701),1,NA)
table(ENSANUT19$ne)

ENSANUT19$no_desea<- ifelse(ENSANUT19$no_usuarias == 1 & 
                              ENSANUT19$no_emb_am == 1 & 
                              ENSANUT19$fertil == 1 &
                              (ENSANUT19$f2_s7_701>=2 | is.na(ENSANUT19$f2_s7_701)),1,NA)

table(ENSANUT19$no_desea, useNA = "a")


##### *----* Desea Pronto*----*
ENSANUT19$pronto<- ifelse(ENSANUT19$no_usuarias==1 & 
                            ENSANUT19$no_emb_am==1 & ENSANUT19$fertil==1 &
                            ENSANUT19$f2_s7_701 ==1 &
                            (ENSANUT19$f2_s7_702_1<=2 | ENSANUT19$f2_s7_702==33),1,NA)

table(ENSANUT19$pronto, useNA = "a")


################################# *----* NIA*----*

##### ***Nececidad para espaciar

ENSANUT19$espaciar<- ifelse(ENSANUT19$emb_inoportuno==1 | ENSANUT19$desea_despues==1, 1,0)
ENSANUT19$espaciar[is.na(ENSANUT19$espaciar)] <- 0
table(ENSANUT19$espaciar, useNA = "a")

##### ***Necesidades para Limitar

ENSANUT19$limitar<- ifelse(ENSANUT19$emb_No_deseado==1 | ENSANUT19$no_desea==1, 1,0)
ENSANUT19$limitar[is.na(ENSANUT19$limitar)] <- 0
table(ENSANUT19$limitar, useNA = "a")

##### *** NIA

ENSANUT19$nia<- ifelse(ENSANUT19$espaciar==1 | ENSANUT19$limitar==1 ,1,0)
                     
ENSANUT19$nia[is.na(ENSANUT19$nia)] <- 0
table(ENSANUT19$nia,useNA = "a")


###########

##### *** NIA 1549

table(ENSANUT19$f2_s1_101, useNA = "a")
table(ENSANUT19$gen_activas, useNA = "a")
table(ENSANUT19$f2_s9_900, useNA = "a")
table(ENSANUT19$nia, useNA= "a")
table(ENSANUT19$f2_s9_900, useNA = "a")

##########


###### Creando archivo nuevo realizando los filtros de las condicionales

ENSANUT20 <- ENSANUT19 %>% 
  filter(f2_s1_101  %in% c(15:49),gen_activas %in% c(0:30) | 
           f2_s9_900 %in% c(1:3))

table(ENSANUT20$nia,useNA = "a")

#####

##### Prueba pasos previos

ENSANUT19$femj<- ifelse(ENSANUT19$f2_s1_101 %in%c(15:49),1,0)
table(ENSANUT19$femj)

ENSANUT19$p1549<- ifelse(ENSANUT19$f2_s1_101 %in%c(15:49) &
                           (ENSANUT19$gen_activas %in% c(0:30) | 
                              ENSANUT19$f2_s9_900 %in% c(1:3)),1,NA)

table(ENSANUT19$p1549,useNA = "a")

#####

ENSANUT19$nia1549<- ifelse(ENSANUT19$nia == 1 & ENSANUT19$p1549 == 1,1,NA)

table(ENSANUT19$nia1549, useNA = "a")

table(ENSANUT19$nia, ENSANUT19$p1549,useNA = "a")


#####  arreglando la variable case_when

ENSANUT19 <- ENSANUT19 %>%
  mutate(
    nia1549 = case_when( nia == 1 & p1549 ==1 ~ 1,
                         nia == 0 & p1549 ==1 ~ 0,
                         TRUE ~ NA_real_)
                        
)

table(ENSANUT19$nia, ENSANUT19$nia1549,useNA = "a")
table(ENSANUT19$nia1549, ENSANUT19$p1549,useNA = "a")

table(ENSANUT19$nia1549,useNA = "a")

#### Verificando la posible estimación
weighted.mean(x=1-(ENSANUT19$nia1549),
              w= ENSANUT19$fexp,na.rm = T )


##### ***Planificación Familiar

ENSANUT19$muj<- ifelse(ENSANUT19$usuarias==1 | ENSANUT19$nia==100 ,0,
                       ifelse(ENSANUT19$usuarias==1 ,100, NA))

table(ENSANUT19$muj,useNA = "a")

#PF1 Proporción de mujeres en edad de procrear (de 15 a 49 años) que practican 
#la planificación familiar con métodos modernos (ODS 3.7.1)
#(tienen sus necesidades de planificación satisfechas con métodos modernos)
#===============================================================================

##Prubea

ENSANUT19$moderno1<- ifelse(ENSANUT19$f2_s1_101 %in%c(15:49),1,NA)
table(ENSANUT19$moderno1, useNA = "a")


ENSANUT19$moderno2<- ifelse( ENSANUT19$f2_s6_603_1==1 | 
                             ENSANUT19$f2_s6_603_2==1 |
                             ENSANUT19$f2_s6_603_3==1 |
                             ENSANUT19$f2_s6_603_4==1 |
                             ENSANUT19$f2_s6_603_5==1 |
                             ENSANUT19$f2_s6_603_6==1 |
                             ENSANUT19$f2_s6_603_7==1 |
                             ENSANUT19$f2_s6_603_8==1 |
                             ENSANUT19$f2_s6_603_9==1,1,0)

table(ENSANUT19$moderno2, useNA = "a")


###### Moderno

ENSANUT19$moderno<- ifelse(ENSANUT19$femj == 1 &
                                (ENSANUT19$f2_s6_603_1==1 | 
                                ENSANUT19$f2_s6_603_2==1 |
                                ENSANUT19$f2_s6_603_3==1 |
                                ENSANUT19$f2_s6_603_4==1 |
                                ENSANUT19$f2_s6_603_5==1 |
                                ENSANUT19$f2_s6_603_6==1 |
                                ENSANUT19$f2_s6_603_7==1 |
                                ENSANUT19$f2_s6_603_8==1 |
                                ENSANUT19$f2_s6_603_9==1),1,0)

table(ENSANUT19$moderno)

#### Verificando la posible estimación
weighted.mean(x=1-(ENSANUT19$moderno),
              w= ENSANUT19$fexp,na.rm = T) 
             

###### USo MEtodo

ENSANUT19$usomet<- ifelse(     ENSANUT19$f2_s6_603_1==1 | 
                                ENSANUT19$f2_s6_603_2==1 |
                                ENSANUT19$f2_s6_603_3==1 |
                                ENSANUT19$f2_s6_603_4==1 |
                                ENSANUT19$f2_s6_603_5==1 |
                                ENSANUT19$f2_s6_603_6==1 |
                                ENSANUT19$f2_s6_603_7==1 |
                                ENSANUT19$f2_s6_603_8==1 |
                                ENSANUT19$f2_s6_603_9==1 |
                                ENSANUT19$f2_s6_603_10==1|
                                ENSANUT19$f2_s6_603_11==1 |
                                ENSANUT19$f2_s6_603_12==1 |
                                ENSANUT19$f2_s6_603_13==1,1,0)

table(ENSANUT19$usomet,useNA = "a")
# 
# #### Verificando la posible estimación
# weighted.mean(x=1-(ENSANUT19$usomet),
#               w= ENSANUT19$fexp,na.rm = T)

######## **Demanda satisfecha por métodos modernos

#### primer Filtro
ENSANUT19$metmod0<- ifelse(ENSANUT19$f2_s1_101 %in%c(15:49) &
                             (ENSANUT19$gen_activas %in% c(0:30) | 
                                ENSANUT19$f2_s9_900 %in% c(1:3)),1,NA)
table(ENSANUT19$metmod0, useNA = "a")

### Primera condicional completa
ENSANUT19$metmod05 <-ifelse(ENSANUT19$metmod0==1 &
                      (ENSANUT19$nia1549 == 1 | ENSANUT19$usomet == 1),0,NA)
                      
table(ENSANUT19$metmod05, useNA = "a") 

### Segunda condicional completa
ENSANUT19$metmod1 <-ifelse(ENSANUT19$metmod0==1 &
                              ENSANUT19$moderno == 1,1,NA)

table(ENSANUT19$metmod1, useNA = "a") 

############## Definición variable

ENSANUT19$metmod <- ifelse (ENSANUT19$metmod1 == 1,1,
                            ifelse(ENSANUT19$metmod05 == 0, 0,NA))

table(ENSANUT19$metmod, useNA = "a")


table(ENSANUT19$metmod05, ENSANUT19$metmod1,useNA = "a")


### Prueba

ENSANUT19$Met <- ifelse(ENSANUT19$f2_s1_101%in%c(15:49) &
                (ENSANUT19$gen_activas<=30 | ENSANUT19$f2_s9_900%in%c(1:3)) &
                (ENSANUT19$nia1549==1 | ENSANUT19$usomet==1),0,
              
                    ifelse(ENSANUT19$f2_s1_101%in%c(15:49) &
                       (ENSANUT19$gen_activas<=30 | ENSANUT19$f2_s9_900%in%c(1:3))& 
                       ENSANUT19$ moderno==1 ,1,NA))

table(ENSANUT19$Met,useNA = "a")
##### 

ENSANUT19 <- ENSANUT19 %>%
  mutate(
    metmod = case_when( metmod05 == 0 & metmod1 == 1 ~ 1,
                        metmod05 == 0 & is.na(metmod1) ~ 0,
                        TRUE ~ NA_real_)
    
  )

table(ENSANUT19$metmod, useNA = "a")

table(ENSANUT19$nia, ENSANUT19$nia1549,useNA = "a")
table(ENSANUT19$nia1549, ENSANUT19$p1549,useNA = "a")

table(ENSANUT19$nia1549,useNA = "a")

##### Variable Metmod
table(ENSANUT19$metmod, useNA = "a")

# ############## Definición variable MEtmod 1549
# 
ENSANUT19$metmod_1549 <- ifelse (ENSANUT19$metmod == 1 &
                                   ENSANUT19$f2_s1_101 %in%c(15:49), 1,NA)

table(ENSANUT19$metmod_1549, useNA = "a")


#### Verificando la posible estimación
weighted.mean(x=1-(ENSANUT19$nia1549),
              w= ENSANUT19$fexp,na.rm = T )

######## Creando Dummys para cada Indicador de Interés

## D6
ENSANUT19$D6 <- ifelse(ENSANUT19$usametodo == 1, 1,0)
table(ENSANUT19$D6)

## D6m
ENSANUT19$D6m <- ifelse(ENSANUT19$moderno == 1, 1,0)
table(ENSANUT19$D6m)

## NI INEC
ENSANUT19$NI= ifelse(ENSANUT19$nia == 1, 1,0)
table(ENSANUT19$NI, useNA = "a")

### Demanda Satisfecha

ENSANUT19$DS=ifelse(ENSANUT19$nia1549 == 1,1,0)
#ENSANUT19$DS[is.na(ENSANUT19$DS)] <- 0
table(ENSANUT19$DS, useNA = "a")


ENSANUT19$DSMM=ifelse(ENSANUT19$metmod == 1,1,0)
#ENSANUT19$DS[is.na(ENSANUT19$DSMM)] <- 0
table(ENSANUT19$DSMM, useNA = "a")


################## Seleccionado Covariables #####################
#
# Área                       area                               #
# Municipio/Canton           upm (4 primeros digitos)           #
# Sexo                       sexo                               #
# Edad                       f1_s2_3_1                          #
# Etnia                      etnia                              #
# discapacidad               -----                              #
# Años Estudio               f1_s2_19_1 / f1_s2_19_2            #
#                                                               #
###############                                                 #
# upm                       upm                                 #
# estrato                   estrato                             #
# factor de expansión       fexp                                #      
#                                                               #
#################################################################
###################################################################


###### Discapacidad

table(ENSANUT19$f1_s2_11, useNA = "a")
ENSANUT19$discap <- ifelse(ENSANUT19$f1_s2_11 == 1 | ENSANUT19$f1_s3_30_1 > 0, 1,0) 
table(ENSANUT19$f1_s2_11, useNA = "a")
table(ENSANUT19$discap, useNA = "a")
ENSANUT19$discap<- ifelse(is.na(ENSANUT19$discap),0,ENSANUT19$discap)
table(ENSANUT19$discap, useNA = "a")


###### Años estudio

ENSANUT19$Anoest<- ifelse(ENSANUT19$f1_s2_19_1 == 1, 0,
                          ifelse(ENSANUT19$f1_s2_19_1 ==2 & ENSANUT19$f1_s2_19_1%in%c(0,3), (2 * ENSANUT19$f1_s2_19_2),
                                 ifelse(ENSANUT19$f1_s2_19_1==3 & ENSANUT19$f1_s2_19_1 %in%c(4,10), (3 + ENSANUT19$f1_s2_19_2),
                                        ifelse(ENSANUT19$f1_s2_19_1 == 1, (3 + ENSANUT19$f1_s2_19_2) & ENSANUT19$f1_s2_19_1 %in%c(4,10),
                                               ifelse(ENSANUT19$f1_s2_19_1 == 3, 1,
                                                      ifelse(ENSANUT19$f1_s2_19_1 == 4,(1 + ENSANUT19$f1_s2_19_2),
                                                             ifelse(ENSANUT19$f1_s2_19_1 == 5, ENSANUT19$f1_s2_19_2,
                                                                    ifelse(ENSANUT19$f1_s2_19_1 == 6,(7+ ENSANUT19$f1_s2_19_2),
                                                                           ifelse(ENSANUT19$f1_s2_19_1 == 7,(10+ ENSANUT19$f1_s2_19_2),
                                                                                  ifelse(ENSANUT19$f1_s2_19_1 == 8,(13+ ENSANUT19$f1_s2_19_2),
                                                                                         ifelse(ENSANUT19$f1_s2_19_1 == 9,(13+ ENSANUT19$f1_s2_19_2),
                                                                                                ifelse(ENSANUT19$f1_s2_19_1 == 10,(18+ ENSANUT19$f1_s2_19_2),NA))))))))))))


#Convirtirendo NA en 99    
ENSANUT19$Anoest[is.na(ENSANUT19$Anoest)] <- 99

table(ENSANUT19$Anoest,useNA = "a")                            
table(ENSANUT19$Anoest, ENSANUT19$f1_s2_3_1)
hist(ENSANUT19$Anoest)

##### Seleccionando las variables que se requieren para la encuesta

ENSANUT2 <- ENSANUT19 %>%
  select(upm, estrato, fexp, Depto, mpio, area, discap,Anoest,
         sexo, etnia.x, f1_s2_3_1, unida, D6, D6m, NI)

names(ENSANUT2)

colnames(ENSANUT2) <- c("upm","estrato",  "fexp", "depto", "mpio","area","discap","anoest",
                       "sexo", "etnia", "edad", "unida" ,"D6", "D6m", "NI")

### Salvando Base de datos

saveRDS(ENSANUT2, file = "ECU/2018/4.D7/Data/ENSANUT2.rds")

####################################################################################
####################################################################################
####################################################################################
##### Seleccionando las variables que se requieren para validar las 
##### estimaciones directas

table(ENSANUT2$edad, useNA = "a")


ENSANUT_Estima <- ENSANUT19 %>%
  select(upm, estrato, fexp, Depto, mpio, area, discap, Anoest,
         sexo, etnia.x, f1_s2_3_1, unida, D6, D6m, NI, musexAct, DS,nia1549 )

names(ENSANUT_Estima)

colnames(ENSANUT_Estima) <- c("upm","estrato",  "fexp", "depto", "mpio","area","discap","anoest",
                              "sexo", "etnia", "edad", "unida" ,"D6", "D6m", "NI","activas_Sex", "DS","DSMM")

################


saveRDS(ENSANUT_Estima,"ECU/2018/4.D7/Data/ENSANUT_Estima.rds")

#################

names(ENSANUT_Estima)

Ensanut <- ENSANUT_Estima %>%
  transmute(
    mpio = mpio,
    Depto = depto,
    usametodo  = D6,
    usamoderno = D6m,
    nec_insat  = NI,
    DS  = DS,
    DSMM  = DSMM,
    
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
    
    etnia = case_when(etnia == 1 ~  "Indigena",
                      etnia == 2 ~  "Afrodescendiente",
                      TRUE ~  "Otro"
    ),
    
    anoest = case_when( anoest == 99        ~ "NS/NR",# NS/NR
                        anoest %in% 0       ~ "Sin educación", # Sin educacion
                        anoest %in% c(1:6)  ~ "1-6 años", # 1 - 6
                        anoest %in% c(7:12) ~ "7-12 años", # 7 - 12
                        anoest  >  12       ~ "Más de 12 años", # mas de 12
                        TRUE ~ "Error"
    ),
    
    discapacidad = case_when(
      discap == 1 ~ "Dispacitado",
      discap == 0 ~ "No Discapacitado"
      
    ),
    
    unida = case_when(unida == 1 ~ "Unida", 
                      unida == 0 ~ "No unida"
    ),
    
    upm = upm,
    estrato = estrato,
    fexp = fexp,
    
  )

####################

saveRDS(Ensanut,"ECU/2018/1.D6/Data/base_estimaciones19.rds")
saveRDS(Ensanut,"ECU/2018/2.D6m/Data/base_estimaciones19.rds")
saveRDS(Ensanut,"ECU/2018/3.NI/Data/base_estimaciones19.rds")
saveRDS(Ensanut,"ECU/2018/4.D7/Data/base_estimaciones19.rds")





