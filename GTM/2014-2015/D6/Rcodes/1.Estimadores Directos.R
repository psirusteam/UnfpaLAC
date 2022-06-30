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

D6_dom1_ = design.base %>% #group_by(dominio) %>%
  summarise(n = unweighted(n()),
            D6 = survey_mean(usametodo,
                             vartype = c("cv")))

D6_dom1_

D6modernos_dom1 = design.base %>% #group_by(dominio) %>%
  summarise(n = unweighted(n()),
            D6 = survey_mean(usamoderno,
                             vartype = c("cv")))

D6modernos_dom1

NI_dom = design.base %>% #group_by(dominio) %>%
  summarise(n = unweighted(n()),
            NI = survey_mean(Nec_ins_pf_T %in% c(1,2)))
NI_dom


NI_dom1 = design.base %>% #group_by(dominio) %>%
  summarise(n = unweighted(n()),
            NI = survey_mean(Nec_ins_pf_T))

NI_dom1


D7_dom = design.base %>% filter(ec.casado== 1 | ec.conviviente == 1 ) %>%
  #group_by(dominio) %>%
  summarise(n = unweighted(n()),
            p = survey_ratio(usamoderno,usametodo+Nec_ins_pf_T,
                             vartype = c("ci", "cv", "se"), 
                             proportion = TRUE, 
                             na.rm = TRUE))

D7_dom


# Ensmi$d7 = ifelse(Ensmi$usamoderno==1&(Ensmi$Falla_met==1|
#                                                Ensmi$Nec_sat_pf_T==1),1,0)


####################################### Desagregaciones #######################


############################### Por Área ###############################

## Usa métodos de Planificación D6

D6_area = design.base %>% group_by(Area) %>%
  summarise(n = unweighted(n()),
            D6 = survey_mean(usametodo,
                             vartype = c("cv")))

D6_area <- as.data.frame(D6_area)
D6_area


openxlsx::write.xlsx(D6_area, file = "GTM/2014-2015/D6/Output/GTM_D6_area.xlsx")


## Métodos Moderno D6m

D6modernos_area = design.base %>% group_by(Area) %>%
  summarise(n = unweighted(n()),
            D6 = survey_mean(usamoderno,
                             vartype = c("cv")))

D6modernos_area <- as.data.frame(D6modernos_area)
D6modernos_area

openxlsx::write.xlsx(D6modernos_area, file = "GTM/2014-2015/D6/Output/GTM_D6_Modernos_area.xlsx")



## Necesidades Insatisfechas NI

NI_area = design.base %>% group_by(Area) %>%
  summarise(n = unweighted(n()),
            NI = survey_mean(Nec_ins_pf_T),
            vartype = c("cv"))

NI_area <- as.data.frame(NI_area)
NI_area

openxlsx::write.xlsx(NI_area, file = "GTM/2014-2015/D6/Output/GTM_NI_area.xlsx")


## Indicador D7

D7_area = design.base %>% filter(ec.casado== 1 | ec.conviviente == 1 ) %>%
  group_by(Area) %>%
  summarise(n = unweighted(n()),
            p = survey_ratio(usamoderno,usametodo+Nec_ins_pf_T,
                             vartype = c("cv"), 
                             proportion = TRUE, 
                             na.rm = TRUE))

D7_area <- as.data.frame(D7_area)
D7_area

openxlsx::write.xlsx(D7_area, file = "GTM/2014-2015/D6/Output/GTM_D7_area.xlsx")



