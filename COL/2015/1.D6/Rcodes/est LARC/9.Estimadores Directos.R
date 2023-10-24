############################################################################################################################################
#############################################################################################################################################
# # 
# # Proceso: Estimación indicadores de planificación LARC
# #              familiar utilizando ENDS 2015
# # 
# # Historia Archivo:
# #   Creation : 30/06/2022
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
library(formattable)

######################################################################################################################################################
###                             Limpiando la memoria 
######################################################################################################################################################

rm(list = ls(all=T))
gc()

###########################################################################################################################

######################################################################################################################################################
##                                    Cargando Insumos
######################################################################################################################################################

ENDS = readRDS("COL/2015/1.D6/Data/ENDS_Estima.rds")

######################################################################################################################################################


#### Explorando

table(ENDS$area, useNA = "a")
table(ENDS$edad, useNA = "a")
table(ENDS$etnia, useNA = "a")
table(ENDS$anoest, useNA = "a")
table(ENDS$discapacidad, useNA = "a")

##### Adecuando los datos

names(ENDS)

ENDS <- ENDS %>%
  transmute(
    mpio = mpio, 
    Depto= Depto,
    usametodo  = usametodo,
    usamoderno = usamoderno,
    nec_insat  = nec_insat,
    
    
    area = case_when(area == 1 ~ "Urbano", 
                     TRUE ~ "Rural"),
    
    edad = case_when(edad %in% 0:14 ~  "13-14",      # 13 a 14
                     edad %in% 15:19 ~ "15-19",      # 15 a 19
                     edad %in% 20:24 ~ "20-24",      # 20 a 24
                     edad %in% 25:29 ~ "25-29",      # 25 a 29
                     edad %in% 30:34 ~ "30-34",      # 30 a 34
                     edad %in% 35:39 ~ "35-39",      # 35 a 39
                     edad %in% 40:44 ~ "40-44",      # 40 a 44
                     edad %in% 40:49 ~ "45-49",      # 45 a 49
                     TRUE ~ "Error"
    ),               
    
    etnia = case_when(etnia == 1        ~ "Indigena", # Indigena
                      etnia == 2        ~ "Otros", ## Gitano ROM
                      etnia %in% c(3:5) ~ "NARP", # afro negro mulato
                      TRUE              ~ "Ninguno"  # Otro
    ),
    
    anoest = case_when(
      anoest == 0          ~ "Sin educación",  # NS/NR
      anoest == 1          ~ "Primaria",   # Sin educacion
      anoest == 2          ~ "Secundaria",   # 1 - 6
      anoest == 3          ~ "Superior",   # 7 - 12
      anoest == 8          ~ "Sin educación",   # mas de 12
      TRUE ~ "Error"
    ),
    
    discapacidad = case_when(
    discapacidad == 1 ~ "Dispacitado",
    discapacidad == 0 ~ "No Discapacitado"
    
    ),
    
    unida = case_when(unida == 1 ~ "Unida", 
                      unida == 0 ~ "No unida"
    ),
    
    sex_act = case_when(sexual_act == 1 ~ "Activa", 
                        sexual_act == 0 ~ "No activa"
    ),
    
    usometodo = case_when(
      tipodemetodo == 0         ~ "No usan",
      tipodemetodo == 1         ~ "Píldora",
      tipodemetodo == 2         ~ "DIU",
      tipodemetodo %in% c(8:10,12,13,18) ~ "Métodos tradicionales",
      tipodemetodo %in% c(3:5,11,14:17,19,20) ~ "Otros modernos",
      tipodemetodo %in% c(6:7)  ~ "Esterilización",
      TRUE ~ "Error"
    ),
    
    categoriaMetod = case_when(
           usoM == 0    ~ "No usan",
           usoM == 1    ~ "Píldora",
           usoM == 2    ~ "DIU",
           usoM == 3    ~ "Inyección mensual",
           usoM == 4    ~ "Diafragma",
           usoM == 5    ~ "Condón masculino",
           usoM == 6    ~ "Esterilización femenina",
           usoM == 7    ~ "Esterilización masculina",
           usoM == 8    ~ "Abstinencia periódica",
           usoM == 9    ~ "Retiro",
           usoM == 10   ~ "Otros tradicionales",
           usoM == 11   ~ "Implantes/Norplant",
           usoM == 12   ~ "Abstinencia prolongada",
           usoM == 13   ~ "Amenorrea de la lactancia (LAM)",
           usoM == 14   ~ "Condón femenino",
           usoM == 15   ~ "Espuma o Jalea",
           usoM == 16   ~ "Anticoncepción de emergencia",
           usoM == 17   ~ "Parche",
           usoM == 18   ~ "Ritmo",
           usoM == 19   ~ "Inyecciones cada tres meses",
           usoM == 20   ~ "Anillo vaginal",
                        TRUE ~ "Error"
    ),
    
    upm = upm,
    estrato = estrato,
    fexp = fexp,
    Nec_ins_pf_T = Nec_ins_pf_T,
    Nec_sat_pf_T = Nec_sat_pf_T,
    Falla_met = Falla_met,
    usoM,
    
  )
####
#saveRDS(ENDS,"COL/2015/1.LARC/Data/base_estimaciones.rds")


table(ENDS$area, useNA = "a")
table(ENDS$edad, useNA = "a")
table(ENDS$etnia, useNA = "a")
table(ENDS$anoest, useNA = "a")
table(ENDS$discapacidad, useNA = "a")

# Creación objetivo diseño de muestreo complejo para unidas y total


################################ Total mujeres entrevistadas 13-49 años

options(survey.lonely.psu = "adjust")

design.base.total = ENDS %>% as_survey_design(ids = upm,
                                         strat = estrato, 
                                         weights = fexp, nest = TRUE)

################################ Total mujeres Unidas

ENDS_Unidas <- ENDS %>% 
  filter(unida == "Unida")

options(survey.lonely.psu = "adjust") 

design.base.unidas = ENDS_Unidas %>% as_survey_design(ids = upm,
                                              strat = estrato, 
                                              weights = fexp, nest = TRUE)

#######################

# Flags

nflag = 100
cvflag = 30
nefflag = 68
dfflag = 10
yflag = 100

###############################################################################
#############################################################################
##                              ESTIMACIONES DIRECTAS                      ##
#############################################################################

####################
## Nivel Nacional ##
####################

# design.base.total
# design.base.unidas


LARC_nal = design.base.total %>% #group_by(dominio) %>%
  #LARC_nal = design.base.unidas %>% #group_by(dominio) %>%  
  summarise(n = unweighted(n()),
            LARC = survey_mean(usoM %in% c(2,11),
                              vartype = c("cv")))

LARC_nal
LARC_nal <- as.data.frame(LARC_nal)
LARC_nal$LARC <- LARC_nal$LARC * 100
LARC_nal$LARC <- round(LARC_nal$LARC, digits = 1)
LARC_nal$LARC_cv <- LARC_nal$LARC_cv * 100
LARC_nal$LARC_cv <- round(LARC_nal$LARC_cv, digits = 1)
LARC_nal


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

## Usa métodos de Planificación LARC

# design.base.total
# design.base.unidas

LARC_Depto = design.base.unidas %>% group_by(Depto) %>%
  summarise(n = unweighted(n()),
            LARC = survey_mean(usoM %in% c(2,11),
                              vartype = c("cv")))

LARC_Depto
LARC_Depto$LARC <- LARC_Depto$LARC * 100
LARC_Depto$LARC <- round(LARC_Depto$LARC, digits = 1)
LARC_Depto$LARC_cv <- LARC_Depto$LARC_cv * 100
LARC_Depto$LARC_cv <- round(LARC_Depto$LARC_cv, digits = 1)
LARC_Depto    <-LARC_Depto[order(-LARC_Depto$LARC),]
LARC_Depto

openxlsx::write.xlsx(LARC_Depto, file = "COL/2015/1.D6/Rcodes/est LARC/Tablas/LARC_Depto.xlsx")


####### Area (Mujeres Unidas) Cuadro 8.4.1.1 Pag 56

# design.base.total
# design.base.unidas

LARC_area = design.base.unidas %>% group_by(area) %>%
  summarise(n = unweighted(n()),
            LARC = survey_mean(usoM %in% c(2,11),
                              vartype = c("cv")))

LARC_area <- as.data.frame(LARC_area)
LARC_area
LARC_area$LARC <- LARC_area$LARC * 100
LARC_area$LARC <- round(LARC_area$LARC, digits = 1)
LARC_area$area <- factor(x= LARC_area$area,
                        levels=c("Urbano","Rural"))
LARC_area

openxlsx::write.xlsx(LARC_area, file = "COL/2015/1.D6/Rcodes/est LARC/Tablas/LARC_area.xlsx")

####### Edad (Graf Informe Nacional 8.3.1 Pag.53)

# design.base.total
# design.base.unidas

LARC_edad = design.base.total %>% group_by(edad) %>%
  summarise(n = unweighted(n()),
            LARC = survey_mean(usoM %in% c(2,11),
                              vartype = c("cv")))

LARC_edad <- as.data.frame(LARC_edad)
LARC_edad$LARC <- LARC_edad$LARC * 100
LARC_edad$LARC <- round(LARC_edad$LARC, digits = 1)
LARC_edad<-LARC_edad[order(-LARC_edad$LARC),]
LARC_edad$LARC_cv <- LARC_edad$LARC_cv * 100
LARC_edad$LARC_cv <- round(LARC_edad$LARC_cv, digits = 1)
LARC_edad

openxlsx::write.xlsx(LARC_edad, file = "COL/2015/1.D6/Rcodes/est LARC/Tablas/LARC_edad.xlsx")

####### Años estudio (Mujeres Unidas) Cuadro 8.4.1.1 Pag 56

# design.base.total
# design.base.unidas

LARC_anoest = design.base.unidas %>% group_by(anoest) %>%
  summarise(n = unweighted(n()),
            LARC = survey_mean(usoM %in% c(2,11),
                              vartype = c("cv")))

LARC_anoest <- as.data.frame(LARC_anoest)
LARC_anoest$LARC <- LARC_anoest$LARC * 100
LARC_anoest$LARC <- round(LARC_anoest$LARC, digits = 1)
table(LARC_anoest$anoest)
LARC_anoest$anoest <- factor(x= LARC_anoest$anoest,
                            levels=c("Sin educación","Primaria",
                                     "Secundaria","Superior"))
LARC_anoest

openxlsx::write.xlsx(LARC_anoest, file = "COL/2015/1.D6/Rcodes/est LARC/Tablas/LARC_anoest.xlsx")

####### Etnia

# design.base.total
# design.base.unidas

LARC_etnia = design.base.total %>% group_by(etnia) %>%
  summarise(n = unweighted(n()),
            LARC = survey_mean(usoM %in% c(2,11),
                              vartype = c("cv")))

LARC_etnia <- as.data.frame(LARC_etnia)
LARC_etnia$LARC <- LARC_etnia$LARC * 100
LARC_etnia$LARC <- round(LARC_etnia$LARC, digits = 1)
LARC_etnia$LARC_cv <- LARC_etnia$LARC_cv * 100
LARC_etnia$LARC_cv <- round(LARC_etnia$LARC_cv, digits = 1)
LARC_etnia<-LARC_etnia[order(-LARC_etnia$LARC),]
LARC_etnia

openxlsx::write.xlsx(LARC_etnia, file = "COL/2015/1.D6/Rcodes/est LARC/Tablas/LARC_etnia.xlsx")

####### Discapacidad

LARC_discap = design.base.unidas %>% group_by(discapacidad) %>%
  summarise(n = unweighted(n()),
            LARC = survey_mean(usoM %in% c(2,11),
                              vartype = c("cv")))

LARC_discap <- as.data.frame(LARC_discap)
LARC_discap$LARC <- LARC_discap$LARC * 100
LARC_discap$LARC <- round(LARC_discap$LARC, digits = 1)
LARC_discap

openxlsx::write.xlsx(LARC_discap, file = "COL/2015/1.D6/Rcodes/est LARC/Tablas/LARC_Discp.xlsx")

##### Resultados desagregaciones covariables

LARC_nal
LARC_area
LARC_anoest
LARC_etnia
LARC_edad
LARC_discap
LARC_Depto

########################### Por Area y Escolaridad ###############################

## Usa métodos de Planificación LARC

LARC_area_anoest = design.base.unidas %>% group_by(area, anoest) %>%
  summarise(n = unweighted(n()),
            LARC = survey_mean(usoM %in% c(2,11),
                              vartype = c("cv")))

LARC_area_anoest
LARC_area_anoest$LARC <- LARC_area_anoest$LARC * 100
LARC_area_anoest$LARC <- round(LARC_area_anoest$LARC, digits = 1)
LARC_area_anoest$LARC_cv <- LARC_area_anoest$LARC_cv * 100
LARC_area_anoest$LARC_cv <- round(LARC_area_anoest$LARC_cv, digits = 1)
LARC_area_anoest<-LARC_area_anoest[order(-LARC_area_anoest$LARC),]
LARC_area_anoest

openxlsx::write.xlsx(LARC_area_anoest, file = "COL/2015/1.D6/Rcodes/est LARC/Tablas/LARC_area_escolaridad.xlsx")


########################### Por Depto y Edad ###############################

## Usa métodos de Planificación LARC

LARC_depto_edad = design.base.unidas %>% group_by(Depto, edad) %>%
  summarise(n = unweighted(n()),
            LARC = survey_mean(usoM %in% c(2,11),
                              vartype = c("cv")))

LARC_depto_edad
LARC_depto_edad$LARC <- LARC_depto_edad$LARC * 100
LARC_depto_edad$LARC <- round(LARC_depto_edad$LARC, digits = 1)
LARC_depto_edad$LARC_cv <- LARC_depto_edad$LARC_cv * 100
LARC_depto_edad$LARC_cv <- round(LARC_depto_edad$LARC_cv, digits = 1)
LARC_depto_edad    <-LARC_depto_edad[order(-LARC_depto_edad$LARC_cv),]
LARC_depto_edad

openxlsx::write.xlsx(LARC_depto_edad, file = "COL/2015/1.D6/Rcodes/est LARC/Tablas/LARC_Depto_edad.xlsx")


########################### Por Depto y Edad ###############################

## Usa métodos de Planificación LARC


LARC_mpio = design.base.unidas %>% group_by(mpio) %>%
  summarise(n = unweighted(n()),
            LARC = survey_mean(usoM %in% c(2,11),
                              vartype = c("cv")))

LARC_mpio
LARC_mpio$LARC <- LARC_mpio$LARC * 100
LARC_mpio$LARC <- round(LARC_mpio$LARC, digits = 1)
LARC_mpio$LARC_cv <- LARC_mpio$LARC_cv * 100
LARC_mpio$LARC_cv <- round(LARC_mpio$LARC_cv, digits = 1)
LARC_mpio    <-LARC_mpio[order(-LARC_mpio$LARC_cv),]
LARC_mpio

openxlsx::write.xlsx(LARC_mpio, file = "COL/2015/1.D6/Rcodes/est LARC/Tablas/LARC_mpio.xlsx")

