############################################################################################################################################
#############################################################################################################################################
# # 
# # Proceso: Estimación indicadores de planificación
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

names(ENDS)

#### Explorando

table(ENDS$area, useNA = "a")
table(ENDS$edad, useNA = "a")
table(ENDS$etnia, useNA = "a")
table(ENDS$anoest, useNA = "a")
table(ENDS$discapacidad, useNA = "a")

##### Adecuando los datos

names(ENDS)

#ENDS<-readRDS("COL/2015/1.D6/Data/base_estimaciones.rds")

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

# saveRDS(ENDS,"COL/2015/1.D6/Data/base_estimaciones.rds")
# saveRDS(ENDS,"COL/2015/2.D6m/Data/base_estimaciones.rds")
# saveRDS(ENDS,"COL/2015/3.NI/Data/base_estimaciones.rds")
# saveRDS(ENDS,"COL/2015/4.D7/Data/base_estimaciones.rds")

####

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
###############################################################################
###############################################################################
#####################
#
# Realizando validaciones Generales de los datos 
#
#
#####################################

table(ENDS$unida)
table(ENDS$sex_act)
table(ENDS$edad)

##### Validación Totales

#### Total : informe 38.718 / Datos 38.718
#### Total Unidas: Informe 19.233 / Datos 19.781

tot_enc<-ENDS %>% 
  #filter(unida == "Unida") %>% 
  summarise(n = n())%>% 
  mutate(N = sum(n))
tot_enc

#############################

## total area

#### Total mujeres Unidas 
####
#### Informe  Urbano 14.483  / Datos Urbano 13.882
####          Rural   4.749 /        Rural   5.899

Tot_area<-ENDS %>% 
  filter(unida == "Unida") %>% 
  group_by(area) %>% 
  summarise(n = n()) %>% 
  mutate(N = sum(n))
Tot_area
#######################################

#### Total No unidas y activas sexualmente
#### Informe 5.034 / Datos 4.625

tot_Nounidas<-ENDS %>%
  filter(unida == "No unida", sex_act == "Activa") %>% 
  summarise(n = n())%>% 
  mutate(N = sum(n))
tot_Nounidas

###############################

## total edad

#### Total Unidas por edad
####
####         Informe |   Datos
####  13-14     19   |     24
####  15-19    812   |  1.046 
####  20-24  2.524   |  2.697
####  25-29  3.445   |  3.342  
####  30-34  3.447   |  3.542
####  35-39  3.236   |  3.262
####  40-44  2.900   |  2.997
####  45-49  2.848   |  2.871

####  Total 19.233   | 19.781


Tot_edad<-ENDS %>%
  filter(unida == "Unida") %>% 
  group_by(edad) %>% 
  summarise(n = n())  
Tot_edad


#######################################
## Validaciones Informe nacional     ##
#######################################

##### Validando Grafico 8.1 Informe Nacional Tomo II Pag.52

table(ENDS$usometodo)   

D6_tipometodo = ENDS_Unidas %>% 
          group_by(usometodo)  %>% 
          summarise(n = sum(fexp),
                    n1 = n(),
                    .groups = "drop") %>% 
          mutate(Prop = n1 / sum(n1))
  
D6_tipometodo <- as.data.frame(D6_tipometodo)
D6_tipometodo$Prop <- D6_tipometodo$Prop * 100
D6_tipometodo
D6_tipometodo$Prop <- round((D6_tipometodo$Prop),2)

D6_tipometodo


##### Validadndo Datos Basicos Colombia Informe Nacional Tomo I Pag 46

D6_categ_Metod = ENDS_Unidas %>% 
  group_by(categoriaMetod)  %>% 
  summarise(n = sum(fexp),
            n1 = n(),
            .groups = "drop") %>% 
  mutate(Prop = n1 / sum(n1))

D6_categ_Metod <- as.data.frame(D6_categ_Metod)
D6_categ_Metod$Prop <- D6_categ_Metod$Prop * 100
D6_categ_Metod
D6_categ_Metod$Prop <- round((D6_categ_Metod$Prop),2)

D6_categ_Metod


#######################################
## Indicador D6: Uso Anticonceptivos ##
#######################################

#############################################################################
##                              ESTIMACIONES DIRECTAS                      ##
#############################################################################

####################
## Nivel Nacional ##
####################

# design.base.total
# design.base.unidas


D6_nal = design.base.total %>% #group_by(dominio) %>%
#D6_nal = design.base.unidas %>% #group_by(dominio) %>%  
  summarise(n = unweighted(n()),
            D6 = survey_mean(usametodo,
                             vartype = c("cv")))

D6_nal
D6_nal <- as.data.frame(D6_nal)
D6_nal$D6 <- D6_nal$D6 * 100
D6_nal$D6 <- round(D6_nal$D6, digits = 1)
D6_nal$D6_cv <- D6_nal$D6_cv * 100
D6_nal$D6_cv <- round(D6_nal$D6_cv, digits = 1)
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

openxlsx::write.xlsx(D6_Depto, file = "COL/2015/1.D6/Output/Tablas/D6_Depto.xlsx")


####### Area (Mujeres Unidas) Cuadro 8.4.1.1 Pag 56

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

openxlsx::write.xlsx(D6_area, file = "COL/2015/1.D6/Output/Tablas/D6_area.xlsx")

####### Edad (Graf Informe Nacional 8.3.1 Pag.53)

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

openxlsx::write.xlsx(D6_edad, file = "COL/2015/1.D6/Output/Tablas/D6_edad.xlsx")

####### Años estudio (Mujeres Unidas) Cuadro 8.4.1.1 Pag 56

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

openxlsx::write.xlsx(D6_anoest, file = "COL/2015/1.D6/Output/Tablas/D6_anoest.xlsx")

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

openxlsx::write.xlsx(D6_etnia, file = "COL/2015/1.D6/Output/Tablas/D6_etnia.xlsx")

####### Discapacidad

D6_discap = design.base.unidas %>% group_by(discapacidad) %>%
  summarise(n = unweighted(n()),
            D6 = survey_mean(usametodo,
                             vartype = c("cv")))

D6_discap <- as.data.frame(D6_discap)
D6_discap$D6 <- D6_discap$D6 * 100
D6_discap$D6 <- round(D6_discap$D6, digits = 1)
D6_discap

openxlsx::write.xlsx(D6_discap, file = "COL/2015/1.D6/Output/Tablas/D6_Discp.xlsx")

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

D6_area_anoest = design.base.unidas %>% group_by(area, anoest) %>%
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

openxlsx::write.xlsx(D6_area_anoest, file = "COL/2015/1.D6/Output/Tablas/D6_area_escolaridad.xlsx")


########################### Por Depto y Edad ###############################

## Usa métodos de Planificación D6

D6_depto_edad = design.base.unidas %>% group_by(Depto, edad) %>%
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

openxlsx::write.xlsx(D6_depto_edad, file = "COL/2015/1.D6/Output/Tablas/D6_Depto_edad.xlsx")


########################### Por Depto y Edad ###############################

## Usa métodos de Planificación D6


D6_mpio = design.base.unidas %>% group_by(mpio) %>%
  summarise(n = unweighted(n()),
            D6 = survey_mean(usametodo,
                             vartype = c("cv")))

D6_mpio
D6_mpio$D6 <- D6_mpio$D6 * 100
D6_mpio$D6 <- round(D6_mpio$D6, digits = 1)
D6_mpio$D6_cv <- D6_mpio$D6_cv * 100
D6_mpio$D6_cv <- round(D6_mpio$D6_cv, digits = 1)
D6_mpio    <-D6_mpio[order(-D6_mpio$D6_cv),]
D6_mpio

openxlsx::write.xlsx(D6_mpio, file = "COL/2015/1.D6/Output/Tablas/D6_mpio.xlsx")

