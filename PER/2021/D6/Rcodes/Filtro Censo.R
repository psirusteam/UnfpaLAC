############################################################################################################################################
#############################################################################################################################################
# # 
# # Proceso: Filtros en la base CENSO
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

paquetes <- c("readr","readxl","survey","openxlsx","dplyr","foreign","srvyr","TeachingSampling")
has <- paquetes %in% rownames(installed.packages())
if(any(!has)) install.packages(paquetes [!has])


#############################################################################################################################################
##                                    Cargando Librerias
#############################################################################################################################################

library(foreign)
library(dplyr)

######################################################################################################################################################
###                             Limpiando la memoria 
######################################################################################################################################################

rm(list = ls(all=T))
gc()

###########################################################################################################################

######################################################################################################################################################
##                                    Cargando Insumos
######################################################################################################################################################


CensoPero = readRDS("PER/2021/D6/Data/PersonasCenso.rds")

######################################################################################################################################################


CensoPero$edad = as.numeric(CensoPero$C5_P4_1)-1


sum(is.na(CensoPero$C5_P1))
levels(CensoPero$C5_P1)
CensoPero %>% filter(C5_P1 == "Jefe o jefa del hogar") %>% summarise(n = n())

########## Vivienda

ViviendasCenso = read.spss("PER/2021/D6/Data/CPV2017_VIV 2708.sav",to.data.frame = TRUE)

#ViviendasCenso = ViviendasCenso %>% select(
#  - c(1:10),-12,-13,-27
#)
#saveRDS(ViviendasCenso,"ViviendasCenso.rds")


######### Hogares
#HogaresCenso = readRDS("HogaresCenso.rds")
#HogaresCenso = HogaresCenso %>% select(
#  -c(1:10),-37,-38
#)
#saveRDS(HogaresCenso,"HogaresCenso.rds")


########## Match Hogares, viviendas, personas

ViviendasCenso = readRDS("PER/2021/D6/Data/ViviendasCenso.rds")
HogaresCenso = readRDS("PER/2021/D6/Data/HogaresCenso.rds")
#rm(HogaresCenso)
CensoPero = CensoPero %>% left_join(ViviendasCenso, by = "ID_VIV_IMP_F")
CensoPero = CensoPero %>% select(
  -C5_P7, -C5_P7A_COD,-C5_P7B_COD,-C5_P7C_COD,-C5_P7D_COD, 
  -C5_P6, -C5_P6A_COD,-C5_P6B_COD,-C5_P6C_COD,-C5_P6D_COD,
  -C5_P5A_COD,-C5_P5B_COD,-C5_P5C_COD,-C5_P5D_COD,
  -C5_P10A, 
  -C5_P15,-C5_P15A_COD,-C5_P15B_COD,-C5_P15C_COD,-C5_P15D_COD,
  -C5_P23,-C5_P23A_COD,-C5_P23B_COD,-C5_P23C_COD,-C5_P23D_COD,
  -C5_P22, 
)
CensoPero = CensoPero %>% left_join(HogaresCenso, by = "ID_HOG_IMP_F")
####### creacion variable sexo hogar

SexoJefe = CensoPero %>% filter(C5_P1 == "Jefe o jefa del hogar") %>%
  select(sexoJefe = C5_P2,ID_HOG_IMP_F)

CensoPero = CensoPero %>%
  left_join(SexoJefe, by = "ID_HOG_IMP_F")

## union bases final

CensoPero = CensoPero %>% filter(C5_P2 == "Mujer"&
                                   edad >=15&edad<=49)

saveRDS(CensoPero,"PER/2021/D6/Output/CensoMEFHV.rds") 


###################### Creacion variable indice de hacinamiento
CensoPero = readRDS("PER/2021/D6/Data/PersonasCenso.rds")

CensoPero = CensoPero %>% select(ID_POB_IMP_F,ID_VIV_IMP_F)
ViviendasCenso = ViviendasCenso %>% select(ID_VIV_IMP_F,C2_P12)

CensoPero = CensoPero %>% left_join(ViviendasCenso, by = "ID_VIV_IMP_F")
CensoPero$hab = as.numeric(CensoPero$C2_P12)
Indice.Hac2 = CensoPero %>% group_by(ID_VIV_IMP_F) %>%
  summarise(nro.pero = n(),
            habi = mean(hab),
            indice.haci = nro.pero/habi)
tail(Indice.Hac2)
Indice.Hac = Indice.Hac2

saveRDS(Indice.Hac,"PER/2021/D6/Output/IndiceHacinamientoCenso.rds")
