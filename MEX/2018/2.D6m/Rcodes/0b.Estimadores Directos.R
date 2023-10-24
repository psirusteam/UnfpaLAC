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
# # Autor: Stalyn Guerrero y Andrés Gutiérrez
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

###########################################################################################################################

######################################################################################################################################################
##                                    Cargando Insumos
######################################################################################################################################################

ENADID<-readRDS("MEX/2018/2.D6m/Data/encuesta_mrp.rds")

######################################################################################################################################################


#### Explorando

table(ENADID$area, useNA = "a")
table(ENADID$edad, useNA = "a")
table(ENADID$etnia, useNA = "a")
table(ENADID$anoest, useNA = "a")
table(ENADID$discapacidad, useNA = "a")

# Creación objetivo diseño de muestreo complejo para unidas y total


################################ Total mujeres entrevistadas 13-49 años

options(survey.lonely.psu = "adjust")

design.base.total = ENADID %>% as_survey_design(ids = upm,
                                              strat = estrato, 
                                              weights = fexp, nest = TRUE)

################################ Total mujeres Unidas

ENADID_Unidas <- ENADID %>% 
  filter(unida == "1")

options(survey.lonely.psu = "adjust") 

design.base.unidas = ENADID_Unidas %>% as_survey_design(ids = upm,
                                                      strat = estrato, 
                                                      weights = fexp, nest = TRUE)

###############################################################################
#############################################################################
##                              ESTIMACIONES DIRECTAS                      ##
#############################################################################

####################
## Nivel Nacional ##
####################

# design.base.total
# design.base.unidas



D6m_nal = design.base.total %>% #group_by(dominio) %>%
  #D6m_nal = design.base.unidas %>% #group_by(dominio) %>%  
  summarise(n = unweighted(n()),
            total_D6m = survey_total(usamoderno),
            D6m = survey_mean(usamoderno,
                             vartype = c("cv")))

D6m_nal
D6m_nal <- as.data.frame(D6m_nal)
D6m_nal$D6m <- D6m_nal$D6m * 100
D6m_nal$D6m <- round(D6m_nal$D6m, digits = 1)
D6m_nal$D6m_cv <- D6m_nal$D6m_cv * 100
D6m_nal$D6m_cv <- round(D6m_nal$D6m_cv, digits = 1)
# 16.8 millones de mujeres son usuarias de por lo menos un método
# anticonceptivo moderno.
D6m_nal

# Cuadro 7.1. República Mexicana. Porcentaje de mujeres en edad fértil sexualmente activas
# y unidas que usan métodos anticonceptivos modernos por características seleccionadas,
# 2014 y 2018
# Situacion_de_los_derechos_sexuales_y_reproductivos_2018_RM_030521.pdf
D6m_dam <- design.base.total %>% filter(unida == 1) %>%
  summarise(n = unweighted(n()),
            D6m = survey_mean(usamoderno,
                              vartype = c("cv")))
D6m_dam
D6m_dam$D6m <- D6m_dam$D6m * 100
D6m_dam$D6m <- round(D6m_dam$D6m, digits = 1)
D6m_dam$D6m_cv <- D6m_dam$D6m_cv * 100
D6m_dam$D6m_cv <- round(D6m_dam$D6m_cv, digits = 1)
D6m_dam


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

## Usa métodos de Planificación D6m

aux_estimate <- function(dis,indicador, group = NULL){
  
  tab <- dis %>% group_by_at(group) %>%
    summarise(n = unweighted(n()),
              Prom = survey_mean(!!sym(indicador),
                                 vartype = c("cv"))) %>% 
    mutate(Prom = round(Prom*100,digits = 1),
           Prom_cv = round(Prom_cv*100,digits = 1)) %>% 
    arrange_at(group)
  
  names(tab) <-  str_replace(names(tab),"Prom",indicador)
  tab
}

estimacion_dir <-
  purrr::map(c(
    "dam",
    "dam2",
    "area",
    "edad",
    "unida",
    "anoest",
    "etnia",
    "discapacidad"
  ),
  function(x)
    aux_estimate(design.base.total %>% filter(unida == 1), 
                 "usamoderno", x))

estimacion_dir2 <-
  purrr::map(list(c("area", "anoest"),
                  c("dam", "edad")),
             function(x) {
               aux_estimate(design.base.total %>% filter(unida == 1), 
                            "usamoderno", x)
             })


c(estimacion_dir,estimacion_dir2) %>% 
  openxlsx::write.xlsx(file = "MEX/2018/2.D6m/Output/Tablas/Estimacion_dir.xlsx")

