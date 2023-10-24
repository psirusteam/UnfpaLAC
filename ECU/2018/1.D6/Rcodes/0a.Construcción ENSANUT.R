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

# # ### Descargando los datos
# # 
# ecu_personas<-read_dta("ECU/2018/Metadatos/Data/1_BDD_ENS2018_f1_personas.dta")
# ecu_hogar<-read_dta("ECU/2018/Metadatos/Data/2_BDD_ENS2018_f1_hogar.dta")
# ecu_mujeres<-read_dta("ECU/2018/Metadatos/Data/4_BDD_ENS2018_f2_mef.dta")
# 
# 
# #### GUardando las bases en RDS
# 
# ecu_personas<-saveRDS(ecu_personas,file ="ECU/2018/1.D6/Data/ecu_personas")
# ecu_hogar<-saveRDS(ecu_hogar,file ="ECU/2018/1.D6/Data/ecu_hogar")
# ecu_mujeres<-saveRDS(ecu_mujeres,file ="ECU/2018/1.D6/Data/ecu_mujeres")

#### Cargando las bases las bases en RDS

ecu_personas<-readRDS("ECU/2018/1.D6/Data/ecu_personas.rds")
ecu_hogar<-readRDS("ECU/2018/1.D6/Data/ecu_hogar.rds")
ecu_mujeres<-readRDS("ECU/2018/1.D6/Data/ecu_mujeres.rds")


#############################################

### Uniendo las bases

ENSANUT18 = ecu_mujeres  %>%
  left_join(ecu_personas, by =c("id_per", "id_hogar")) %>%
  left_join(ecu_hogar)

############# FILTRO - CREACION BASE

#### Filtro se hará cuando se vinculen los datos correspondientes al Censo 

saveRDS(ENSANUT18, file = "ECU/2018/1.D6/Data/ENSANUT.rds")

#ENSANUT18 <- readRDS( file = "ECU/2018/1.D6/Data/ENSANUT.rds")


##### Filtrando por mujeres entre 15 y 49 años

ENSANUT18<-ENSANUT18 %>% ######### 41.113 mujeres de 15 años o más
  filter(f2_s1_101 >= 15) 

table(ENSANUT18$f2_s1_101) 

#### validación de uso de metodos

ecu_mujeres<-ecu_mujeres %>% 
  filter(f2_s1_101 >= 15) 

table(ecu_mujeres$f2_s6_603_1,useNA = "a")
table(ecu_mujeres$f2_s6_603_2,useNA = "a")
table(ecu_mujeres$f2_s6_603_3,useNA = "a")
table(ecu_mujeres$f2_s6_603_4,useNA = "a")
table(ecu_mujeres$f2_s6_603_5,useNA = "a")
table(ecu_mujeres$f2_s6_603_6,useNA = "a")
table(ecu_mujeres$f2_s6_603_7,useNA = "a")
table(ecu_mujeres$f2_s6_603_8,useNA = "a")
table(ecu_mujeres$f2_s6_603_9,useNA = "a")
table(ecu_mujeres$f2_s6_603_10,useNA = "a")
table(ecu_mujeres$f2_s6_603_11,useNA = "a")
table(ecu_mujeres$f2_s6_603_12,useNA = "a")

######### Definiendo codigo UPM para obtener el municipio o canton
ENSANUT18$Cod_depto <-substr(ENSANUT18$upm, start = 1, stop = 2)
ENSANUT18$Cod_depto<-as.factor(ENSANUT18$Cod_depto)
table(ENSANUT18$Cod_depto)

ENSANUT18 <- ENSANUT18 %>%
  mutate(
    Depto = case_when(
      Cod_depto == 01 ~ "Azuay" ,
      Cod_depto == 02 ~ "Bolivar",
      Cod_depto == 03 ~ "Cañar",
      Cod_depto == 04 ~ "Carchi" ,
      Cod_depto == 05 ~ "Cotopaxi"  ,
      Cod_depto == 06 ~ "Chimborazo" ,
      Cod_depto == 07 ~ "El Oro" ,
      Cod_depto == 08 ~ "Esmeraldas" ,
      Cod_depto == 09 ~ "Guayas" ,
      Cod_depto == 10 ~ "Imbabura",
      Cod_depto == 11 ~ "Loja " ,
      Cod_depto == 12 ~ "Los Ríos" ,
      Cod_depto == 13 ~ "Manabi" ,
      Cod_depto == 14 ~ "Morona Santiago" ,
      Cod_depto == 15 ~ "Napo" ,
      Cod_depto == 16 ~ "Pastaza" ,
      Cod_depto == 17 ~ "Pichincha" ,
      Cod_depto == 18 ~ "Tungurahua",
      Cod_depto == 19 ~ "Zamora Chinchipe" ,
      Cod_depto == 20 ~ "Galapgos",
      Cod_depto == 21 ~ "Sucumbios" ,
      Cod_depto == 22 ~ "Orellana",
      Cod_depto == 23 ~ "Santo Domingo de los Tsachilas",
      Cod_depto == 24 ~ "Santa Elena" ,
      Cod_depto == 90 ~ "Zonas no Delimitadas"
    ),
    shdepto = str_pad(
      string = Cod_depto,
      width = 2,
      pad = "0"
    ))

table(ENSANUT18$Depto, useNA = "a")

######### Definiendo codigo UPM para obtener el municipio o canton
ENSANUT18$mpio <-substr(ENSANUT18$upm, start = 1, stop = 4)
ENSANUT18$mpio<-as.factor(ENSANUT18$mpio)

#Borrando bases innecesarias 
# rm(ecu_personas,ecu_hogar,ecu_mujeres)

table(ENSANUT18$area)

############# seleccionando Variables para establecer los modelos 

###### Definiendo variable Unida y Sexualmente activa

#### Avtiva Sexualmente

table(ENSANUT18$f2_s8_832_semanas)
ENSANUT18$f2_s8_832_semanas[is.na(ENSANUT18$f2_s8_832_semanas)] <- 0
ENSANUT18$sexactiv  =ifelse(ENSANUT18$f2_s8_832_semanas <=4,1,0)
table(ENSANUT18$sexactiv)

#### Unida

table(ENSANUT18$f2_s9_900, useNA = "a")
ENSANUT18$f2_s9_900[is.na(ENSANUT18$f2_s9_900)] <- 0
ENSANUT18$unida =ifelse(ENSANUT18$f2_s9_900 %in%c(1:3),1,0)
table(ENSANUT18$unida)

######## Construcción Aciva Sexual y unida

####     Mujeres activas sex y unidas  

ENSANUT18 <- ENSANUT18 %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    ActivSexUnida= max(
      sexactiv,
      unida,
      na.rm = TRUE),
    
  )

table(ENSANUT18$ActivSexUnida)


###### Definiendo variable edad

ENSANUT18 <- ENSANUT18 %>%
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

table(ecu_mujeres$f2_s6_603_2, useNA = "a")

table(ENSANUT18$f2_s6_603_1,useNA = "a")
ENSANUT18$f2_s6_603_1[is.na(ENSANUT18$f2_s6_603_1)] <- 0
ENSANUT18$m_Vasectomia   =ifelse(ENSANUT18$f2_s6_603_1  ==1,1,0)
table(ENSANUT18$m_Vasectomia)

table(ENSANUT18$f2_s6_603_2)
ENSANUT18$f2_s6_603_2[is.na(ENSANUT18$f2_s6_603_2)] <- 0
ENSANUT18$m_Ligadura=ifelse(ENSANUT18$f2_s6_603_2  ==1,1,0)                                  
table(ENSANUT18$m_Ligadura)


table(ENSANUT18$f2_s6_603_3)
ENSANUT18$f2_s6_603_3[is.na(ENSANUT18$f2_s6_603_3)] <- 0
ENSANUT18$m_Implante     =ifelse(ENSANUT18$f2_s6_603_3  ==1,1,0)
table(ENSANUT18$m_Implante)

table(ENSANUT18$f2_s6_603_4)
ENSANUT18$f2_s6_603_4[is.na(ENSANUT18$f2_s6_603_4)] <- 0
ENSANUT18$m_Inyeccion    =ifelse(ENSANUT18$f2_s6_603_4  ==1,1,0)
table(ENSANUT18$m_Inyeccion)

table(ENSANUT18$f2_s6_603_5)
ENSANUT18$f2_s6_603_5[is.na(ENSANUT18$f2_s6_603_5)] <- 0
ENSANUT18$m_DIU          =ifelse(ENSANUT18$f2_s6_603_5  ==1,1,0)
table(ENSANUT18$m_DIU)

table(ENSANUT18$f2_s6_603_6)
ENSANUT18$f2_s6_603_6[is.na(ENSANUT18$f2_s6_603_6)] <- 0
ENSANUT18$m_Pildora      =ifelse(ENSANUT18$f2_s6_603_6  ==1,1,0)
table(ENSANUT18$m_Pildora)

table(ENSANUT18$f2_s6_603_7)
ENSANUT18$f2_s6_603_7[is.na(ENSANUT18$f2_s6_603_7)] <- 0
ENSANUT18$m_Condon_fem   =ifelse(ENSANUT18$f2_s6_603_7  ==1,1,0)
table(ENSANUT18$m_Condon_fem)

table(ENSANUT18$f2_s6_603_8)
ENSANUT18$f2_s6_603_8[is.na(ENSANUT18$f2_s6_603_8)] <- 0
ENSANUT18$m_Condon_mas   =ifelse(ENSANUT18$f2_s6_603_8  ==1,1,0)
table(ENSANUT18$m_Condon_mas)

table(ENSANUT18$f2_s6_603_9)
ENSANUT18$f2_s6_603_9[is.na(ENSANUT18$f2_s6_603_9)] <- 0
ENSANUT18$m_Pastilla_eme =ifelse(ENSANUT18$f2_s6_603_9  ==1,1,0)
table(ENSANUT18$m_Pastilla_eme)

table(ENSANUT18$f2_s6_603_10)
ENSANUT18$f2_s6_603_10[is.na(ENSANUT18$f2_s6_603_10)] <- 0
ENSANUT18$m_Ritmo        =ifelse(ENSANUT18$f2_s6_603_10 ==1,1,0)
table(ENSANUT18$m_Ritmo)

table(ENSANUT18$f2_s6_603_11)
ENSANUT18$f2_s6_603_11[is.na(ENSANUT18$f2_s6_603_11)] <- 0
ENSANUT18$m_Retiro       =ifelse(ENSANUT18$f2_s6_603_11 ==1,1,0)
table(ENSANUT18$m_Retiro)

table(ENSANUT18$f2_s6_603_12)
ENSANUT18$f2_s6_603_12[is.na(ENSANUT18$f2_s6_603_12)] <- 0
ENSANUT18$m_Lactancia    =ifelse(ENSANUT18$f2_s6_603_12 ==1,1,0)
table(ENSANUT18$m_Lactancia)

table(ENSANUT18$f2_s6_603_13)
ENSANUT18$f2_s6_603_13[is.na(ENSANUT18$f2_s6_603_13)] <- 0
ENSANUT18$m_Otro    =ifelse(ENSANUT18$f2_s6_603_13 ==1,1,0)
table(ENSANUT18$m_Otro)


######## Construcción usametodo y usamoderno

####     usametodo  

ENSANUT18 <- ENSANUT18 %>%
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
ENSANUT18$usam<- ifelse(  ENSANUT18$f2_s6_603_1==1 | 
                          ENSANUT18$f2_s6_603_2==1 |
                          ENSANUT18$f2_s6_603_3==1 |
                          ENSANUT18$f2_s6_603_4==1 |
                          ENSANUT18$f2_s6_603_5==1 |
                          ENSANUT18$f2_s6_603_6==1 |
                          ENSANUT18$f2_s6_603_7==1 |
                          ENSANUT18$f2_s6_603_8==1 |
                          ENSANUT18$f2_s6_603_9==1 |
                          ENSANUT18$f2_s6_603_10==1 |
                          ENSANUT18$f2_s6_603_11==1 |
                          ENSANUT18$f2_s6_603_12==1 |
                          ENSANUT18$f2_s6_603_13==1,1,0)

table(ENSANUT18$usametodo)
table(ENSANUT18$usam)


### Usamoderno
ENSANUT18$usamod<- ifelse(  ENSANUT18$f2_s6_603_1==1 | 
                            ENSANUT18$f2_s6_603_2==1 |
                            ENSANUT18$f2_s6_603_3==1 |
                            ENSANUT18$f2_s6_603_4==1 |
                            ENSANUT18$f2_s6_603_5==1 |
                            ENSANUT18$f2_s6_603_6==1 |
                            ENSANUT18$f2_s6_603_7==1 |
                            ENSANUT18$f2_s6_603_8==1 |
                            ENSANUT18$f2_s6_603_9==1,1,0)

table(ENSANUT18$usamod)
table(ENSANUT18$usamoderno)

table(ENSANUT18$unida, ENSANUT18$usametodo, useNA = "a")
table(ENSANUT18$sexactiv, ENSANUT18$usametodo, useNA = "a")
table(ENSANUT18$ActivSexUnida, ENSANUT18$usametodo, useNA = "a")
table(ENSANUT18$usametodo, ENSANUT18$usamoderno, useNA = "a")

#######################################

##### Definición mujeres postergar la maternidad o no desean hijos

### ¿ No quiere hijos/ más hijos/as?     
### ¿Indecisa

table(ENSANUT18$f2_s7_701)
#ENSANUT18$f2_s7_701[is.na(ENSANUT18$f2_s7_701)] <- 0

ENSANUT18$no_deseo_hijos= ifelse(ENSANUT18$f2_s7_701  == 2, 1,
                                 ifelse(ENSANUT18$f2_s7_701  == 5,1,0))

table(ENSANUT18$no_deseo_hijos)

### ¿ no quería embarazarse?     
### ¿quería esperar más tiempo? Espacio Tiempo

table(ENSANUT18$f2_s7_702)
ENSANUT18$f2_s7_702[is.na(ENSANUT18$f2_s7_702)] <- 0

ENSANUT18$no_embarazo= ifelse(ENSANUT18$f2_s7_702 >0, 1,0)

table(ENSANUT18$no_embarazo)

### Variable postergar maternidad 

ENSANUT18 <- ENSANUT18 %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    posterg= max(
      no_deseo_hijos,
      no_embarazo,
      na.rm = TRUE),
  )

table(ENSANUT18$posterg)

##################### 

##### Definiendo necesidades insatisfechas NI POr CEPAL 

table(ENSANUT18$posterg)


ENSANUT18 <- ENSANUT18 %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    NICP = case_when(
      posterg == 1 & usametodo == 0 ~ 1,
      posterg == 1 & usametodo == 1 ~ 0, 
      TRUE ~ 0 #### se codifica en cero porque el 
      #### dejar en NA se elimina la mitad de la muestra
    )
  )

table(ENSANUT18$NICP)

############################################
##################### 

##### Definiendo necesidades insatisfechas NI de acuerdo al INEC

####  T14_i3. Porcentaje de mujeres en edad fértil con necesidad insatisfecha 
#### 		  de planificación familiar 

####### Nota: deben tomarse las mujeres mayores de 15 años

### edad
table(ENSANUT18$f2_s1_101)

# ENSANUT18<-ENSANUT18 %>% ######### 41.113 mujeres de 15 años o más
#   filter(f2_s1_101 >= 15) 
# ENSANUT18    

#### Se modifica el Estado Civil

table(ENSANUT18$f2_s9_900, useNA = "a")
table(ENSANUT18$f2_s9_901, useNA = "a")


#### 
ENSANUT18$gen_civil<- ifelse(ENSANUT18$f2_s9_900 >= 1 & ENSANUT18$f2_s9_900 <= 3, 1,
                             ifelse(ENSANUT18$f2_s9_900 == 7 & ENSANUT18$f2_s9_901 == 2,3,
                                ifelse(ENSANUT18$f2_s9_900 == 7, 2,
                             ifelse(ENSANUT18$f2_s9_900 >= 4 & ENSANUT18$f2_s9_900 <= 6, 2,NA))))
                                     

table(ENSANUT18$gen_civil, useNA = "a")



##### Mujeres activas sexualmente

table(ENSANUT18$f2_s8_832_dias, useNA = "a")
table(ENSANUT18$f2_s8_832_semanas, useNA = "a")
table(ENSANUT18$f2_s8_832_meses, useNA = "a")
table(ENSANUT18$f2_s8_832_anios, useNA = "a")

ENSANUT18b<-ENSANUT18 %>% 
  filter(f2_s8_832_anios <55)

ENSANUT18$ActivS = ((ENSANUT18$f2_s8_832_dias) +
                      (ENSANUT18$f2_s8_832_semanas * 7) +
                      (ENSANUT18$f2_s8_832_meses *30) +
                      (ENSANUT18$f2_s8_832_anios*365))


ENSANUT18$gen_activas<- ifelse(ENSANUT18$f2_s8_832_anios<55,ENSANUT18$ActivS, NA)
ENSANUT18$gen_activas[is.na(ENSANUT18$gen_activas)] <- 1000
table(ENSANUT18$gen_activas, useNA = "a")

MAS30<-ENSANUT18 %>% ######### 24.343 mujeres Activas sexualmente
  filter(gen_activas %in% c(0:30)) 
MAS30 

table(MAS30$gen_activas, useNA = "a")

#### Activas sex

ENSANUT18$musexAct<- ifelse(ENSANUT18$gen_activas %in% c(0:30),1,0)
table(ENSANUT18$musexAct, useNA = "a")

# ### Casadas y activas sexualmente
# ENSANUT18$musa<- ifelse(ENSANUT18$gen_activas %in% c(0:30) & 
#                           ENSANUT18$gen_civil > 1, 1,0)
# table(ENSANUT18$musa, useNA = "a")
# 
# ###### Mujeres Activas Sexualmente
# 
# ### 1. Otros
# ### 2. Casadas o Unidas y Activas sexualmente
# 
# ENSANUT18$unidas_ActSex<- ifelse(ENSANUT18$gen_civil == 2, 1,
#                                  ifelse(ENSANUT18$musa == 1,2, 99))
# 
# table(ENSANUT18$unidas_ActSex, useNA = "a")


###### Mujeres Usuarias

ENSANUT18$usuarias<- ifelse(ENSANUT18$f2_s6_604 ==1 & 
                              ENSANUT18$f2_s6_620<=9, 1,0)

table(ENSANUT18$usuarias, useNA = "a")

###### Mujeres NO Usuarias

ENSANUT18$no_usuarias<- ifelse(ENSANUT18$f2_s6_604 >=2, 1,0)

table(ENSANUT18$no_usuarias, useNA = "a")


###### Mujeres Embrazadas y Amenorreicas
####  Embarazadas y Amenorreicas (Nunca han usado o no usan actualmente)

ENSANUT18$gen_embarazo<- ifelse(ENSANUT18$no_usuarias ==1 & 
                                  ENSANUT18$f2_s2_200==1, 1,
                                ifelse(ENSANUT18$no_usuarias ==1 & 
                                         ENSANUT18$f2_s2_200>=2 &
                                         ENSANUT18$f2_s8_843_dias ==77, 2,0))

ENSANUT18$gen_embarazo[is.na(ENSANUT18$gen_embarazo)] <- 0
table(ENSANUT18$gen_embarazo, useNA = "a")

###### Mujeres Espera del embarazo---------------- 


ENSANUT18$gen_espera01 = ifelse(ENSANUT18$f2_s2_202_1< 88,
                           ((ENSANUT18$f2_s2_202_1) + (ENSANUT18$f2_s2_202_2*12)),NA)

#ENSANUT18$gen_espera01[is.na(ENSANUT18$gen_espera01)] <- 0
table(ENSANUT18$gen_espera01, useNA = "a")
summary(ENSANUT18$gen_espera01)                                

ENSANUT18$gen_espera02 = ifelse(ENSANUT18$f2_s7_702 < 30,
                                ((ENSANUT18$f2_s7_702) + (ENSANUT18$f2_s7_702_1*12)),NA)

#ENSANUT18$gen_espera02[is.na(ENSANUT18$gen_espera02)] <- 0
table(ENSANUT18$gen_espera02, useNA = "a")
summary(ENSANUT18$gen_espera02)  


##### 
ENSANUT18$espera24 <- ifelse(ENSANUT18$gen_espera01 > 24 | is.na(ENSANUT18$gen_espera01), 1,0)
table(ENSANUT18$espera24)


##### Planeado-----------------------

### prueba 

ENSANUT18$p_plan <- ifelse(ENSANUT18$no_usuarias == 1 &
                             ENSANUT18$gen_embarazo == 1 &
                             ENSANUT18$f2_s2_201 == 1, 1,
                          
                            ifelse(ENSANUT18$no_usuarias == 1 &
                                    ENSANUT18$gen_embarazo == 1 &
                                    ENSANUT18$f2_s2_201==2 &
                                    (ENSANUT18$gen_espera01 <=24 | is.na(ENSANUT18$gen_espera01)), 1,0))
table(ENSANUT18$p_plan)


ENSANUT18$emb_planeado <- ifelse(ENSANUT18$no_usuarias == 1 &
                                   ENSANUT18$gen_embarazo == 1 &
                                   ENSANUT18$f2_s2_201 == 1, 1,
                                 
                                 ifelse(ENSANUT18$no_usuarias == 1 &
                                          ENSANUT18$gen_embarazo == 1 &
                                          ENSANUT18$f2_s2_201==2 &
                                          (ENSANUT18$gen_espera01 <=24 | is.na(ENSANUT18$gen_espera01)), 1,
                                        
                                        ifelse(ENSANUT18$no_usuarias == 1 &
                                                 ENSANUT18$gen_embarazo == 2 &
                                                 ENSANUT18$f2_s7_701==1 &
                                                 (ENSANUT18$gen_espera02 <=24 | is.na(ENSANUT18$gen_espera02)), 1, NA)))

table(ENSANUT18$emb_planeado, useNA = "a")


##### Inoportuno -----------------------

ENSANUT18$emb_inoportuno <- ifelse(ENSANUT18$no_usuarias %in% 1 &
                                     ENSANUT18$gen_embarazo%in% 2 &
                                     ENSANUT18$f2_s7_701%in% 1 &
                                     (ENSANUT18$gen_espera02 >24 | is.na(ENSANUT18$gen_espera02)), 1, 
                                   
                            ifelse(ENSANUT18$no_usuarias %in% 1 &
                                     ENSANUT18$gen_embarazo %in% 1 &
                                     ENSANUT18$f2_s2_201 %in% 2 & (ENSANUT18$gen_espera01 >24 | is.na(ENSANUT18$gen_espera01)),1,NA))
                                          
table(ENSANUT18$emb_inoportuno, useNA = "a")


##### No deseado -----------------------

ENSANUT18$emb_No_deseado <- ifelse(ENSANUT18$no_usuarias == 1 &
                                     ENSANUT18$gen_embarazo == 1 &
                                     ENSANUT18$f2_s2_201 == 3, 1,
                                   
                                   ifelse(ENSANUT18$no_usuarias == 1 &
                                            ENSANUT18$gen_embarazo == 2 &
                                            ENSANUT18$f2_s7_701==2, 1,
                                          
                                          ifelse(ENSANUT18$no_usuarias == 1 &
                                                   ENSANUT18$gen_embarazo == 2 &
                                                   ENSANUT18$f2_s7_701 == 4, 1, 0)))


table(ENSANUT18$emb_No_deseado, useNA = "a")


##### No sabe No responde NS/NR -----------------------

ENSANUT18$nsnr <- ifelse(ENSANUT18$gen_embarazo <= 2 &
                           ENSANUT18$emb_planeado != 1 &
                           ENSANUT18$emb_inoportuno != 1 &
                           ENSANUT18$emb_No_deseado != 1, 1, NA)

table(ENSANUT18$nsnr, useNA = "a")


###### Mujeres NO Embrazadas Ni Amenorreicas

table(ENSANUT18$f2_s2_200, useNA = "a")
table(ENSANUT18$no_usuarias, useNA = "a")
table(ENSANUT18$f2_s8_843_dias, useNA = "a")
summary(ENSANUT18$f2_s8_843_dias, useNA = "a")

ENSANUT18$prueba<- ifelse(ENSANUT18$f2_s8_843_dias != 77 , 1,  0)
table(ENSANUT18$prueba)


ENSANUT18$no_emb_am <- ifelse( ENSANUT18$no_usuarias == 1 &
                               ENSANUT18$f2_s2_200 >= 2 &
                               (ENSANUT18$f2_s8_843_dias != 77| is.na(ENSANUT18$f2_s8_843_dias)), 1, NA)

table(ENSANUT18$no_emb_am, useNA = "a")


################################### Mujeres Infertiles

###### Tiempo retraso en la regla

ENSANUT18$ctr <-as.integer((ENSANUT18$f2_s8_843_dias) +
                    (ENSANUT18$f2_s8_843_semanas * 7) +
                     (ENSANUT18$f2_s8_843_meses *30) +
                     (ENSANUT18$f2_s8_843_anios*365)/30)
summary(ENSANUT18$ctr, useNA = "a")


ENSANUT18$tiempo_regla <- ifelse(ENSANUT18$f2_s8_843_dias < 77 | is.na(ENSANUT18$f2_s8_843_dias),
                                 ((ENSANUT18$f2_s8_843_dias) +
                                  (ENSANUT18$f2_s8_843_semanas * 7) +
                                  (ENSANUT18$f2_s8_843_meses *30) +
                                  (ENSANUT18$f2_s8_843_anios*365))/30,NA)


#ENSANUT18$tiempo_regla <- ifelse(ENSANUT18$f2_s8_843_dias < 77, ENSANUT18$ctr, NA)

#ENSANUT18$tiempo_regla[is.na(ENSANUT18$tiempo_regla)] <- 0
table(ENSANUT18$tiempo_regla, useNA = "a")
summary(ENSANUT18$tiempo_regla) 

###### Mujeres Retraso


ENSANUT18$gen_retraso <- ifelse(ENSANUT18$no_usuarias == 1 &
                                  ENSANUT18$no_emb_am == 1 &
                                  (ENSANUT18$tiempo_regla >=3 | is.na(ENSANUT18$tiempo_regla)), 1,NA)

ENSANUT18$gen_retraso[is.na(ENSANUT18$gen_retraso)] <- 0
table(ENSANUT18$gen_retraso, useNA = "a")


###### Mujeres Menopausica

table(ENSANUT18$no_usuarias, useNA = "a")
table(ENSANUT18$no_emb_am, useNA = "a")
table(ENSANUT18$gen_retraso, useNA = "a")
table(ENSANUT18$f2_s7_701, useNA = "a")
table(ENSANUT18$f2_s6_612, useNA = "a")

#########

### Prueba

ENSANUT18$prueba <-  ifelse(ENSANUT18$no_usuarias == 1 &
                                       ENSANUT18$no_emb_am == 1 &
                                      (ENSANUT18$f2_s7_701==3 | ENSANUT18$f2_s6_612==2) &
                                       ENSANUT18$gen_retraso != 1 , 1,NA)
                                       
table(ENSANUT18$prueba)


ENSANUT18$gen_menopausia <- ifelse(ENSANUT18$no_usuarias == 1 &
                                     ENSANUT18$no_emb_am == 1 &
                                     (ENSANUT18$f2_s7_701==3 | ENSANUT18$f2_s6_612==2) &
                                     ENSANUT18$gen_retraso != 1 , 1,NA)

ENSANUT18$gen_menopausia[is.na(ENSANUT18$gen_menopausia)] <- 0
table(ENSANUT18$gen_menopausia, useNA = "a")

##### *----* Unidas o sexualmente activas Infertiles*----*

ENSANUT18$mes_entrevista <- ENSANUT18$fecha_mes
ENSANUT18$anio_entrevista <- ENSANUT18$fecha_anio

#### Hijo Muerto

#________EJEMPLO mes_ultimo== ifelse(ultimo==2,f2_s2_213_1, NA )

ENSANUT18$f2_s2_213_2[is.na(ENSANUT18$f2_s2_213_2)] <- 0
table(ENSANUT18$f2_s2_213_2, useNA = "a")

ENSANUT18$f2_s2_213_1[is.na(ENSANUT18$f2_s2_213_1)] <- 0
table(ENSANUT18$f2_s2_213_1, useNA = "a")

ENSANUT18$hm<-((ENSANUT18$f2_s2_213_2*100)+(ENSANUT18$f2_s2_213_1))
table(ENSANUT18$hm, useNA = "a")

ENSANUT18$hm1<-ifelse(ENSANUT18$f2_s2_213_2 == 8888 | ENSANUT18$f2_s2_213_2 == 7777,0,
                      ENSANUT18$f2_s2_213_2)
ENSANUT18$hm2<-ifelse(ENSANUT18$f2_s2_213_1 == 88 | ENSANUT18$f2_s2_213_1 == 77,0,
                      ENSANUT18$f2_s2_213_1)

ENSANUT18$hijo_muerto<-ifelse(ENSANUT18$f2_s2_213_2 == 8888 | ENSANUT18$f2_s2_213_1== 88, 0,
                              ifelse(ENSANUT18$f2_s2_213_2 == 7777 | ENSANUT18$f2_s2_213_1== 77, 0, 
                                     ENSANUT18$hm))

table(ENSANUT18$hijo_muerto, useNA = "a")


#### _________  **Hijos nacidos vivos

ENSANUT18$f2_s2_218_1_b3[is.na(ENSANUT18$f2_s2_218_1_b3)] <- 0
table(ENSANUT18$f2_s2_218_1_b3, useNA = "a")

ENSANUT18$f2_s2_218_1_b2[is.na(ENSANUT18$f2_s2_218_1_b2)] <- 0
table(ENSANUT18$f2_s2_218_1_b2, useNA = "a")

ENSANUT18$hv<-((ENSANUT18$f2_s2_218_1_b3*100)+(ENSANUT18$f2_s2_218_1_b2))
table(ENSANUT18$hv, useNA = "a")

ENSANUT18$hijo_vivo<-ifelse(ENSANUT18$f2_s2_218_1_b2 == 88 | ENSANUT18$f2_s2_218_1_b3== 8888, 0,
                            ENSANUT18$hv)

table(ENSANUT18$hijo_vivo, useNA = "a")

##### *----* Ultimo Embarazo*----*

## Ultimo Embarazo
ENSANUT18$ultimo <- ifelse(ENSANUT18$hijo_vivo > ENSANUT18$hijo_muerto, 1,
                           ifelse(ENSANUT18$hijo_muerto > ENSANUT18$hijo_vivo, 2,0))
table(ENSANUT18$ultimo, useNA = "a")

## Mes Ultimo Embarazo
ENSANUT18$mes_ultimo<-ifelse(ENSANUT18$ultimo == 2, ENSANUT18$f2_s2_213_1,
                             ifelse(ENSANUT18$ultimo == 1, ENSANUT18$f2_s2_218_1_b2, NA))
table(ENSANUT18$mes_ultimo, useNA = "a")

## Año Ultimo Embarazo
ENSANUT18$anio_ultimo<-ifelse(ENSANUT18$ultimo == 2, ENSANUT18$f2_s2_213_2,
                              ifelse(ENSANUT18$ultimo == 1, ENSANUT18$f2_s2_218_1_b3, NA))
table(ENSANUT18$anio_ultimo, useNA = "a")

### fecha de union la pareja (casadas o unidas)

ENSANUT18$mes_union <- ifelse(ENSANUT18$f2_s9_900 <=3 & ENSANUT18$f2_s9_904_2 != 77, 
                              ENSANUT18$f2_s9_904_2, NA)
table(ENSANUT18$mes_union, useNA = "a")

ENSANUT18$anio_union <- ifelse(ENSANUT18$f2_s9_900 <=3 & ENSANUT18$f2_s9_904_3 != 7777, 
                               ENSANUT18$f2_s9_904_3, NA)
table(ENSANUT18$anio_union, useNA = "a")

##*-*No uso del metodo anticonceptivo

ENSANUT18$no_uso <- ifelse(ENSANUT18$f2_s6_604==2, 1, NA)
table (ENSANUT18$no_uso)

################################################# creando a

ENSANUT18$a <- ENSANUT18$mes_entrevista+(ENSANUT18$anio_entrevista*12)
table(ENSANUT18$a, useNA = "a")
summary(ENSANUT18$a, useNA = "a") #### ok

################################################# creando b

ENSANUT18$b <- ENSANUT18$mes_ultimo+(ENSANUT18$anio_ultimo*12)
table(ENSANUT18$b, useNA = "a")
summary(ENSANUT18$b, useNA = "a") #### ok

################################################# creando c

ENSANUT18$c <- ENSANUT18$no_uso
table(ENSANUT18$c, useNA = "a") ### ok

################################################# creando c

ENSANUT18$d <- ENSANUT18$mes_union+(ENSANUT18$anio_union*12)
table(ENSANUT18$d, useNA = "a")
summary(ENSANUT18$d, useNA = "a") ### ok

################################################# creando a_d

ENSANUT18$prub_a_d <- (ENSANUT18$a-ENSANUT18$d)/12
summary (ENSANUT18$prub_a_d, useNA = "a")

table (ENSANUT18$prub_a_d, useNA = "a")

table (ENSANUT18$gen_embarazo, useNA = "a")

ENSANUT18$a_d= ifelse(ENSANUT18$gen_embarazo !=1, (ENSANUT18$a-ENSANUT18$d)/12,NA )
table(ENSANUT18$a_d, useNA = "a")
summary(ENSANUT18$a_d, useNA = "a") ### ok


################################################# creando a_b

ENSANUT18$a_b= ifelse(ENSANUT18$gen_embarazo !=1, (ENSANUT18$a-ENSANUT18$b)/12,NA )
table(ENSANUT18$a_b, useNA = "a")
summary(ENSANUT18$a_b) ### ok

###########

### infertil

table(ENSANUT18$a_d, useNA = "a")
table(ENSANUT18$a_b, useNA = "a")
table(ENSANUT18$c, useNA = "a")
table(ENSANUT18$gen_embarazo, useNA = "a")
table(ENSANUT18$f2_s6_604, useNA = "a")
table(ENSANUT18$gen_retraso, useNA = "a")
table(ENSANUT18$gen_menopausia, useNA = "a")

#################################################################### Pruebas

ENSANUT18$prueba1 <-  ifelse(ENSANUT18$a_d >=5, 1 ,0)
table(ENSANUT18$prueba1, useNA = "a")


ENSANUT18$prueba <-  ifelse(ENSANUT18$gen_embarazo !=1 &
                              ENSANUT18$f2_s6_604 ==2& 
                              ENSANUT18$gen_retraso !=1 &
                              ENSANUT18$gen_menopausia !=1 &
                              (ENSANUT18$a_d >=5 | is.na(ENSANUT18$a_d)) & 
                              (ENSANUT18$a_b>=5 | is.na(ENSANUT18$a_b)), 1,NA)

table(ENSANUT18$prueba, useNA = "a")

ENSANUT18$infertil_01<- ifelse(ENSANUT18$gen_embarazo !=1 &
                                  ENSANUT18$f2_s6_604 ==2 & 
                                  ENSANUT18$gen_retraso !=1 &
                                  ENSANUT18$gen_menopausia !=1 &
                                 (ENSANUT18$a_d >=5| is.na(ENSANUT18$a_d)) & 
                                 (ENSANUT18$a_b>=5 | is.na(ENSANUT18$a_b)) & 
                                  ENSANUT18$c ==  1, 1, NA) 

#ENSANUT18$infertil_01[is.na(ENSANUT18$infertil_01)] <- 0
table(ENSANUT18$infertil_01, useNA = "a")



##### *----* Total mujeres Infertiles*----*

table(ENSANUT18$no_usuarias,useNA = "a") #### NA Stata 24419 / R == 0 / 24419
table(ENSANUT18$no_emb_am,useNA = "a")   #### Ok
table(ENSANUT18$gen_retraso,useNA = "a") #### NA Stata 39462 / R == 0 / 39462
table(ENSANUT18$gen_menopausia,useNA = "a")  #### NA Stata 40955 / R == 0 / 40955
table(ENSANUT18$infertil_01,useNA = "a") #### Ok
table(ENSANUT18$f2_s7_701,useNA = "a") #### Ok
table(ENSANUT18$f2_s7_702,useNA = "a") #### Ok
table(ENSANUT18$f2_s6_612, useNA = "a")#### Ok 


###### Prueba 2

ENSANUT18$infertil_1<- ifelse(ENSANUT18$no_usuarias %in% 1 & 
                                ENSANUT18$no_emb_am %in% 1 & 
                                ENSANUT18$gen_retraso %in% 1,1,
                 ifelse(ENSANUT18$no_usuarias %in% 1 & 
                                ENSANUT18$no_emb_am %in%1 & 
                                (ENSANUT18$gen_menopausia %in% 1 | ENSANUT18$f2_s7_702 %in% 44),2,
                 ifelse(ENSANUT18$no_usuarias %in% 1 & 
                                 ENSANUT18$no_emb_am %in% 1 & 
                                 ENSANUT18$infertil_01 %in% 1,3,
                        ifelse(ENSANUT18$no_usuarias %in% 1 & 
                                 ENSANUT18$no_emb_am %in% 1 & 
                                 ENSANUT18$f2_s7_701 %in% 4, 4,
                        ifelse(ENSANUT18$no_usuarias %in% 1 & 
                                        ENSANUT18$no_emb_am %in% 1 & 
                                        ENSANUT18$f2_s6_612 <=4, 5,
                               NA)))))
                

table (ENSANUT18$infertil_1, useNA = "a")


# ###### Construccion variable
# 
# ENSANUT18$infertil_1<- ifelse(ENSANUT18$no_usuarias %in% 1 & 
#                                 ENSANUT18$no_emb_am %in% 1 & 
#                                 ENSANUT18$gen_retraso %in% 1,1,
#                        
#                        ifelse(ENSANUT18$no_usuarias %in% 1 & 
#                               ENSANUT18$no_emb_am %in% 1 &
#                               (ENSANUT18$gen_menopausia %in% 1 | ENSANUT18$f2_s7_702==44),2,
#                        
#                        ifelse(ENSANUT18$no_usuarias %in% 1 & 
#                                 ENSANUT18$no_emb_am %in% 1 & 
#                                 ENSANUT18$infertil_01 %in% 1,3,
#                        
#                        ifelse(ENSANUT18$no_usuarias %in% 1 & 
#                                 ENSANUT18$no_emb_am %in% 1 & 
#                                 ENSANUT18$f2_s7_701 %in% 4, 4,
#                        
#                        ifelse(ENSANUT18$no_usuarias %in% 1 & 
#                                 ENSANUT18$no_emb_am %in% 1 & 
#                                 ENSANUT18$f2_s6_612 <=4, 5, NA)))))


ENSANUT18$infertil<- ENSANUT18$infertil_1

table(ENSANUT18$infertil, useNA = "a")

##### *----* Total mujeres fertiles*----*

table(ENSANUT18$no_usuarias)
table(ENSANUT18$no_emb_am)
table(ENSANUT18$infertil)


ENSANUT18$fertil= ifelse(ENSANUT18$no_usuarias %in% 1 & 
                           ENSANUT18$no_emb_am %in% 1 & 
                           (ENSANUT18$infertil > 5 | is.na(ENSANUT18$infertil)),1,NA)

table(ENSANUT18$fertil)


##### *----* Desea Después*----*

ENSANUT18$desea_despues<- ifelse(ENSANUT18$no_usuarias==1 & 
                                   ENSANUT18$no_emb_am==1 & ENSANUT18$fertil==1 &
                                   ENSANUT18$f2_s7_701==1 & 
                                   (ENSANUT18$f2_s7_702_1>2 & ENSANUT18$f2_s7_702!=33),1,NA)
table(ENSANUT18$desea_despues)


##### *----* No Desea*----*

table(ENSANUT18$f2_s7_701, useNA = "a")

## prueba
ENSANUT18$prt<-ifelse(ENSANUT18$no_usuarias==1 & 
                        ENSANUT18$no_emb_am==1 & 
                        ENSANUT18$fertil==1, 1,NA)

table(ENSANUT18$prt, useNA = "a")

####
ENSANUT18$ne<-ifelse(ENSANUT18$f2_s7_701>=2 | is.na(ENSANUT18$f2_s7_701),1,NA)
table(ENSANUT18$ne)

ENSANUT18$no_desea<- ifelse(ENSANUT18$no_usuarias == 1 & 
                            ENSANUT18$no_emb_am == 1 & 
                            ENSANUT18$fertil == 1 &
                            (ENSANUT18$f2_s7_701>=2 | is.na(ENSANUT18$f2_s7_701)),1,NA)
table(ENSANUT18$no_desea)


##### *----* Desea Pronto*----*
ENSANUT18$pronto<- ifelse(ENSANUT18$no_usuarias==1 & 
                            ENSANUT18$no_emb_am==1 & ENSANUT18$fertil==1 &
                            ENSANUT18$f2_s7_701 ==1 &
                            (ENSANUT18$f2_s7_702_1<=2 | ENSANUT18$f2_s7_702==33),1,NA)

table(ENSANUT18$pronto)


################################# *----* NIA*----*

##### ***Nececidad para espaciar

ENSANUT18$espaciar<- ifelse(ENSANUT18$emb_inoportuno==1 | ENSANUT18$desea_despues==1, 1,0)
table(ENSANUT18$espaciar, useNA = "a")

##### ***Necesidades para Limitar

ENSANUT18$limitar<- ifelse(ENSANUT18$emb_No_deseado==1 | ENSANUT18$no_desea==1, 1,0)
table(ENSANUT18$limitar)

##### *** NIA

ENSANUT18$nia<- ifelse(ENSANUT18$espaciar==1 | ENSANUT18$limitar==1 ,100,
                       ifelse(is.na(ENSANUT18$nia),0,0))

ENSANUT18$nia[is.na(ENSANUT18$nia)] <- 0
table(ENSANUT18$nia,useNA = "a")

##### ***Planificación Familiar

ENSANUT18$muj<- ifelse(ENSANUT18$usuarias==1 | ENSANUT18$nia==100 ,0,
                       ifelse(ENSANUT18$usuarias==1 ,100, NA))

table(ENSANUT18$muj,useNA = "a")


######## Creando Dummys para cada Indicador de Interés

## D6
ENSANUT18$D6 <- ifelse(ENSANUT18$usametodo == 1, 1,0)
table(ENSANUT18$D6)

## D6m
ENSANUT18$D6m <- ifelse(ENSANUT18$usamoderno == 1, 1,0)
table(ENSANUT18$D6m)

## NI CEPAL
ENSANUT18$NIC= ifelse(ENSANUT18$NICP == 1, 1,0)
table(ENSANUT18$NIC, useNA = "a")

## NI INEC
ENSANUT18$NI= ifelse(ENSANUT18$nia == 100, 1,0)
table(ENSANUT18$NI, useNA = "a")


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

table(ENSANUT18$f1_s2_11, useNA = "a")
ENSANUT18$discap <- ifelse(ENSANUT18$f1_s2_11 == 1 | ENSANUT18$f1_s3_30_1 > 0, 1,0) 
table(ENSANUT18$f1_s2_11, useNA = "a")
table(ENSANUT18$discap, useNA = "a")
ENSANUT18$discap<- ifelse(is.na(ENSANUT18$discap),0,ENSANUT18$discap)
table(ENSANUT18$discap, useNA = "a")


###### Años estudio

ENSANUT18$Anoest<- ifelse(ENSANUT18$f1_s2_19_1 == 1, 0,
                          ifelse(ENSANUT18$f1_s2_19_1 ==2 & ENSANUT18$f1_s2_19_1%in%c(0,3), (2 * ENSANUT18$f1_s2_19_2),
                                 ifelse(ENSANUT18$f1_s2_19_1==3 & ENSANUT18$f1_s2_19_1 %in%c(4,10), (3 + ENSANUT18$f1_s2_19_2),
                                        ifelse(ENSANUT18$f1_s2_19_1 == 1, (3 + ENSANUT18$f1_s2_19_2) & ENSANUT18$f1_s2_19_1 %in%c(4,10),
                                               ifelse(ENSANUT18$f1_s2_19_1 == 3, 1,
                                                      ifelse(ENSANUT18$f1_s2_19_1 == 4,(1 + ENSANUT18$f1_s2_19_2),
                                                             ifelse(ENSANUT18$f1_s2_19_1 == 5, ENSANUT18$f1_s2_19_2,
                                                                    ifelse(ENSANUT18$f1_s2_19_1 == 6,(7+ ENSANUT18$f1_s2_19_2),
                                                                           ifelse(ENSANUT18$f1_s2_19_1 == 7,(10+ ENSANUT18$f1_s2_19_2),
                                                                                  ifelse(ENSANUT18$f1_s2_19_1 == 8,(13+ ENSANUT18$f1_s2_19_2),
                                                                                         ifelse(ENSANUT18$f1_s2_19_1 == 9,(13+ ENSANUT18$f1_s2_19_2),
                                                                                                ifelse(ENSANUT18$f1_s2_19_1 == 10,(18+ ENSANUT18$f1_s2_19_2),NA))))))))))))


#Convirtirendo NA en 99    
ENSANUT18$Anoest[is.na(ENSANUT18$Anoest)] <- 99

table(ENSANUT18$Anoest,useNA = "a")                            
table(ENSANUT18$Anoest, ENSANUT18$f1_s2_3_1)
hist(ENSANUT18$Anoest)

##### Seleccionando las variables que se requieren para la encuesta

ENSANUT <- ENSANUT18 %>%
  select(upm, estrato, fexp,Depto, mpio, area, discap,Anoest,
         sexo, etnia.x, f1_s2_3_1, unida, D6, D6m, NI)

names(ENSANUT)

colnames(ENSANUT) <- c("upm","estrato",  "fexp", "depto", "mpio","area","discap","anoest",
                       "sexo", "etnia", "edad", "unida" ,"D6", "D6m", "NI")

### Salvando Base de datos

saveRDS(ENSANUT, file = "ECU/2018/1.D6/Data/ENSANUT.rds")
saveRDS(ENSANUT, file = "ECU/2018/2.D6m/Data/ENSANUT.rds")
saveRDS(ENSANUT, file = "ECU/2018/3.NI/Data/ENSANUT.rds")
#saveRDS(ENSANUT, file = "ECU/2018/4.D7/Data/ENSANUT.rds")

####################################################################################
####################################################################################
####################################################################################
##### Seleccionando las variables que se requieren para validar las 
##### estimaciones directas

table(ENSANUT$edad, useNA = "a")


ENSANUT_Estima <- ENSANUT18 %>%
  select(upm, estrato, fexp,Depto, mpio, area, discap, Anoest,
         sexo, etnia.x, f1_s2_3_1, unida, D6, D6m, NI, NICP, musexAct)

names(ENSANUT_Estima)

colnames(ENSANUT_Estima) <- c("upm","estrato",  "fexp", "depto", "mpio","area","discap","anoest",
                              "sexo", "etnia", "edad", "unida" ,"D6", "D6m", "NI", "NICP", "activas_Sex")

################

saveRDS(ENSANUT_Estima,"ECU/2018/1.D6/Data/ENSANUT_Estima.rds")
saveRDS(ENSANUT_Estima,"ECU/2018/2.D6m/Data/ENSANUT_Estima.rds")
saveRDS(ENSANUT_Estima,"ECU/2018/3.NI/Data/ENSANUT_Estima.rds")
#saveRDS(ENSANUT_Estima,"ECU/2018/4.D7/Data/ENSANUT_Estima.rds")

#################

names(ENSANUT_Estima)

Ensanut <- ENSANUT_Estima %>%
  transmute(
    mpio = mpio, 
    depto = depto,
    usametodo  = D6,
    usamoderno = D6m,
    nec_insat  = NI,
    
    
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

saveRDS(Ensanut,"ECU/2018/1.D6/Data/base_estimaciones.rds")
saveRDS(Ensanut,"ECU/2018/2.D6m/Data/base_estimaciones.rds")
saveRDS(Ensanut,"ECU/2018/3.NI/Data/base_estimaciones.rds")
saveRDS(Ensanut,"ECU/2018/4.D7/Data/base_estimaciones.rds")






