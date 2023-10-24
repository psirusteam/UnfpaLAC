############################################################################################################################################
#############################################################################################################################################
# # 
# # Proceso: Estimación indicadores de planificación
# #              familiar utilizando ENADID 2015
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
library(haven)
######################################################################################################################################################
###                             Limpiando la memoria 
######################################################################################################################################################

rm(list = ls(all=T))
gc()

###########################################################################################################################

######################################################################################################################################################
##                                    Cargando Insumos
######################################################################################################################################################

ENADID<-readRDS("MEX/2018/1.D6/Data/ENADID_MUJER.rds")
Mujer_NI <- read_sav("MEX/2018/3.NI/Data/Mujer_2018.sav") %>%
  rename(llave_per = llave_muj)

ENADID <- left_join(ENADID, Mujer_NI) %>% 
  mutate(Dem14 = ifelse(is.na(Dem14),9,Dem14),
         Dem14.1 = ifelse(is.na(Dem14.1),9,Dem14.1))

######################################################################################################################################################


#### Explorando

table(ENADID$area, useNA = "a")
table(ENADID$edad, useNA = "a")
table(ENADID$etnia, useNA = "a")
table(ENADID$anoest, useNA = "a")
table(ENADID$discapacidad, useNA = "a")
table(ENADID$unida, useNA = "a")

# Creación objetivo diseño de muestreo complejo para unidas y total

################################################################################
## Indicador NI: Necesidad insatisfecha de métodos anticonceptivos de mujeres ##
## en edad fértil sexualmente activas en el momento t                         ##
################################################################################
ENADID_Unidas <- ENADID #%>% filter(unida == "Unida")

ENADID_Unidas %>% distinct(Dem14) %>%
  transmute(value = as_factor(Dem14),
            lab = as_factor(Dem14))

ENADID_Unidas %>% distinct(Dem14.1) %>%
  transmute(value = as_factor(Dem14.1),
            lab = as_factor(Dem14.1))

ENADID_Unidas %>% distinct(Uso) %>%
  transmute(value = as_factor(Uso, levels = "values"),
            lab = as_factor(Uso))

ENADID_Unidas %>% group_by(SexAct,unida, usametodo, Dem14) %>% 
  summarise(
    cont = n(),
    n = sum(fexp )) %>% data.frame()

ENADID_Unidas %>% group_by(usametodo) %>% 
  summarise(
    cont = n(),
    n = sum(fexp ))

ENADID_Unidas %>% group_by(usamoderno) %>% 
  summarise(
    cont = n(),
    n = sum(fexp ))
## Cuadro 4. Porcentaje de mujeres en edad fértil unidas de acuerdo a los indicadores que permiten
## determinar la demanda satisfecha de métodos anticonceptivos modernos por entidad federativa, 2014 y 2018
(759*2+ 1120131)/12249308

ENADID2 <-
  ENADID %>% mutate(NI = ifelse(
      Dem14 %in% c(1, 2, 4) & 
      usametodo == 0 &
      SexAct == 1, 1, 0))

 ENADID2 %>% #filter(unida == "Unida") %>% 
  group_by(usametodo, NI) %>% 
  summarise(
    cont = n(),
    n = sum(fexp ))

sum(ENADID2$NI*ENADID2$fexp)/sum(ENADID2$fexp*(ENADID2$usametodo == 1))

################################ Total mujeres entrevistadas 13-49 años

options(survey.lonely.psu = "adjust")

design.base.total = ENADID2 %>% filter(unida == "Unida",SexAct == 1) %>%
  as_survey_design(
    ids = upm,
    strat = estrato,
    weights = fexp,
    nest = TRUE
  )

###############################################################################
#############################################################################
##                              ESTIMACIONES DIRECTAS                      ##
#############################################################################

####################
## Nivel Nacional ##
####################


NI_nal = design.base.total %>% 
  summarise(n = unweighted(n()),
            NI = survey_ratio(NI,usamoderno,
                             vartype = c("cv")))

NI_nal
NI_nal <- as.data.frame(NI_nal)
NI_nal$NI <- NI_nal$NI * 100
NI_nal$NI <- round(NI_nal$NI, digits = 1)
NI_nal$NI_cv <- NI_nal$NI_cv * 100
NI_nal$NI_cv <- round(NI_nal$NI_cv, digits = 1)

# Gráfica 7.8. República Mexicana. Porcentaje de mujeres en edad fértil unidas 
# con demanda satisfecha de métodos anticonceptivos modernos, prevalencia 
# anticonceptiva y necesidad insatisfecha de métodos anticonceptivos, 2014 y 2018
# (Situacion_de_los_derechos_sexuales_y_reproductivos_2018_RM_030521.pdf)
NI_nal


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

## Usa métodos de Planificación NI

# design.base.total
# design.base.unidas

aux_estimate <- function(dis,num,den, indicador = "NI", group = NULL){
  
  tab <- dis %>% group_by_at(group) %>%
    summarise(n = unweighted(n()),
              Razon = survey_ratio(!!sym(num),!!sym(den),
                                 vartype = c("cv"))) %>% 
    mutate(Razon = round(Razon*100,digits = 1),
           Razon_cv = round(Razon_cv*100,digits = 1)) %>% 
    arrange_at(group)
  
  names(tab) <-  str_replace(names(tab),"Razon",indicador)
  tab
}

estimacion_dir <-
  purrr::map(c(
    "dam",
    "area",
    "edad",
    "unida",
    "anoest",
    "etnia",
    "discapacidad"
  ),
  function(x)
    aux_estimate(design.base.total,"NI", "usametodo","NI", x))

estimacion_dir2 <-
  purrr::map(list(c("area", "anoest"),
                  c("dam", "edad")),
             function(x) {
               aux_estimate(design.base.total,"NI", "usametodo","NI", x)
             })


c(estimacion_dir,estimacion_dir2) %>% 
  openxlsx::write.xlsx(file = "MEX/2018/3.NI/Output/Tablas/Estimacion_dir.xlsx")

ENADID2 %>% 
  saveRDS("MEX/2018/3.NI/Data/encuesta_NI.rds")
