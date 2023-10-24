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

######################################################################################################################################################
###                             Limpiando la memoria 
######################################################################################################################################################

rm(list = ls(all=T))
gc()

###########################################################################################################################

######################################################################################################################################################
##                                    Cargando Insumos
######################################################################################################################################################

ENADID<-readRDS("MEX/2018/4.D7/Data/encuesta_NI.rds")

######################################################################################################################################################

#### Explorando

table(ENADID$area, useNA = "a")
table(ENADID$edad, useNA = "a")
table(ENADID$etnia, useNA = "a")
table(ENADID$anoest, useNA = "a")
table(ENADID$unida, useNA = "a")
table(ENADID$discapacidad, useNA = "a")


# Creación objetivo diseño de muestreo complejo para unidas y total


################################ Total mujeres entrevistadas 13-49 años

options(survey.lonely.psu = "adjust")

design.base.total = ENADID %>% as_survey_design(ids = upm,
                                              strat = estrato, 
                                              weights = fexp, nest = TRUE)


################################ Total mujeres Unidas

ENADID_Unidas <- ENADID %>% 
  filter(unida == "Unida")

options(survey.lonely.psu = "adjust") 

design.base.unidas = ENADID_Unidas %>% as_survey_design(ids = upm,
                                                      strat = estrato, 
                                                      weights = fexp, nest = TRUE)

##########################################################################
##        Indicador D7: Demanda satisfecha por métodos modernos               ##
##########################################################################

#############################################################################
##                              ESTIMACIONES DIRECTAS                      ##
#############################################################################

####################
## Nivel Nacional ##
####################
# Gráfica 7.8. República Mexicana. Porcentaje de mujeres en edad fértil unidas con demanda satisfecha
# de métodos anticonceptivos modernos, prevalencia anticonceptiva y necesidad insatisfecha
# de métodos anticonceptivos, 2014 y 2018
# (Situacion_de_los_derechos_sexuales_y_reproductivos_2018_RM_030521.pdf)

D7_nal = design.base.unidas %>% filter(SexAct == 1) %>% 
  summarise(n = unweighted(n()),
            D7 = survey_ratio(usamoderno, usametodo + NI),
                             vartype = c("cv"))

D7_nal
D7_nal <- as.data.frame(D7_nal)
D7_nal$D7 <- D7_nal$D7 * 100
D7_nal$D7 <- round(D7_nal$D7, digits = 1)
D7_nal$D7_se <- D7_nal$D7_se * 100
D7_nal$D7_se <- round(D7_nal$D7_se, digits = 1)
D7_nal


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

## Usa métodos de Planificación NI

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
    aux_estimate(design.base.unidas %>% 
                   mutate(usametodo2 = usametodo + NI)
                   ,"usamoderno", "usametodo2","D7", x))

estimacion_dir2 <-
  purrr::map(list(c("area", "anoest"),
                  c("dam", "edad")),
             function(x) {
               aux_estimate(design.base.unidas %>% 
                              mutate(usametodo2 = usametodo + NI)
                            ,"usamoderno", "usametodo2","D7", x)
             })


c(estimacion_dir,estimacion_dir2) %>% 
  openxlsx::write.xlsx(file = "MEX/2018/4.D7/Output/Tablas/Estimacion_dir.xlsx")
