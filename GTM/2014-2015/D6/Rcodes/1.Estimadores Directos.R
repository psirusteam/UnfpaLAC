############################################################################################################################################
#############################################################################################################################################
# # 
# # Proceso: Estimación indicadores de planificación
# #              familiar utilizando ENSMI 2014-2015
# # 
# # Historia Archivo:
# #   Creation : 29/06/2022
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

###########################################################################################################################

######################################################################################################################################################
##                                    Cargando Insumos
######################################################################################################################################################

Ensmi = readRDS("GTM/2014-2015/D6/Output/Ensmi.rds")

######################################################################################################################################################

# Ensmi = Ensmi %>% filter(EdadQ!="12-14")
# Ensmi$EdadQ = as.factor(as.character(Ensmi$EdadQ))
# levels(Ensmi$EdadQ)

# Creación objetivo diseño de muestreo complejo

options(survey.lonely.psu = "adjust") 
design.base = Ensmi %>% as_survey_design(ids = V021, strat = V022, 
                                         weights = FEXT, nest = TRUE)


# Flags

nflag = 100
cvflag = 30
nefflag = 68
dfflag = 10
yflag = 100


#######################################
## Indicador D6: Uso Anticonceptivos ##
#######################################

####################
## Nivel Nacional ##
####################

D6_Nacional = design.base %>% #group_by(dominio) %>%
  summarise(n = unweighted(n()),
            D6 = survey_mean(usametodo,
                             vartype = c("cv")))

D6_Nacional

D6_Modernos_Nacional = design.base %>% #group_by(dominio) %>%
           summarise(n = unweighted(n()),
                    D6 = survey_mean(usamoderno,
                             vartype = c("cv")))

D6_Modernos_Nacional

NI_Nacional = design.base %>% #group_by(dominio) %>%
          summarise(n = unweighted(n()),
                  NI = survey_mean(Nec_ins_pf_T %in% c(1,2)))
NI_Nacional


D7_Nacional = design.base %>% filter(ec.casado== 1 | ec.conviviente == 1 ) %>%
         #group_by(dominio) %>%
           summarise(n = unweighted(n()),
                   p = survey_ratio(usamoderno,usametodo+Nec_ins_pf_T,
                                     vartype = c("ci", "cv", "se"), 
                                     proportion = TRUE, 
                                     na.rm = TRUE))

D7_Nacional


##########################################################################

