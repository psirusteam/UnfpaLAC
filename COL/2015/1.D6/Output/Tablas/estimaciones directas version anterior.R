##################################
# Estimaciones directas de       #
# algunos indicadores de interés #
# junto a criterios de calidad   #
##################################

rm(list=ls())
library(dplyr)
library(survey)
library(srvyr)
library("WriteXLS")

source("10. Otros/directos supr.r")

Xencuesta <- readRDS("1. ConciliarBases/Output/Xencuesta.rds")
encuesta <- readRDS("1. ConciliarBases/Output/ENDS2015_in.rds")

encuesta$idpers <- encuesta$id

encuesta <- inner_join(encuesta, Xencuesta, by = "idpers")  

data_COIR71DT <- readRDS("1. ConciliarBases/Output/COIR71FL.rds")
data_COPR71DT <- readRDS("1. ConciliarBases/Output/COPR71FL.rds")

sum(data_COIR71DT$v005/1000000) #Pesos de muestreo mujer

encuesta$pesos <- 13126296*data_COIR71DT$v005/sum(data_COIR71DT$v005)
sum(encuesta$pesos)

encuesta$departamento <- as.factor(data_COIR71DT$sdepto)

encuesta <- as.data.frame(encuesta)
encuesta$departamento <- as.factor(data_COIR71DT$sdepto)
levels(data_COIR71DT$sdepto)


# Flags
nflag = 100
cvflag = 30
nefflag = 100
dfflag = 8
CVLflag = 20 # proporciones con valores extremos
yflag = 50 # para proporciones

labelled::val_labels(encuesta$departamento)
labelled::val_labels(encuesta$current_method_grou)
labelled::val_labels(encuesta$PA1_GRP_ETNIC_C)
labelled::val_labels(encuesta$P_EDAD_C) # grupo 3: es 13 - 14 y grupo 10: 45 - 49
labelled::val_labels(encuesta$current_method)

table(encuesta$PA1_GRP_ETNIC_C)
table(encuesta$P_EDAD_C)
# Subgrupos:
## Departamento - etnia (1  - 5 - 6 y resto) - grupo de edad 


encuesta$dominio <- paste0(encuesta$departamento, " - ", 
                           ifelse(encuesta$PA1_GRP_ETNIC_C == 1, "Indigena",
                                  ifelse(encuesta$PA1_GRP_ETNIC_C == 5, "Afrodescendiente",
                                         ifelse(encuesta$PA1_GRP_ETNIC_C == 6, "NoConsidera",
                                                "Gitano, raizal, palanquero"))), " - ", 
                           ifelse(encuesta$P_EDAD_C%in%c(3,4), "13 a 20",
                                  ifelse(encuesta$P_EDAD_C%in%c(5,6),"20 a 30", "30 mas")))

encuesta$area = ifelse(encuesta$UA_CLASE_C == 1, "Urbano", "Rural")

##########################
# Mujeres en edad Fértil #
##########################

diseno = encuesta %>% as_survey_design(weights = pesos) 
options(survey.lonely.psu = "adjust")
options( survey.multicore = TRUE )
options(survey.ultimate.cluster = TRUE)


# Por dominios ------------------------------------------------------------

D6_dom = diseno %>% #group_by(dominio) %>%
  summarise(n = unweighted(n()),
            D6 = survey_mean(current_method_grou %in% c(1,2,3),
                             vartype = c("ci")))


D6_dom1_ = diseno %>% #group_by(dominio) %>%
  summarise(n = unweighted(n()),
            D6 = survey_mean(usametodo,
                             vartype = c("cv")))


D6modernos_dom = diseno %>% #group_by(dominio) %>%
  summarise(n = unweighted(n()),
            D6modernos = survey_mean(current_method_grou == 3))


D6modernos_dom1_ = diseno %>% #group_by(dominio) %>%
  summarise(n = unweighted(n()),
            D6modernos = survey_mean(usamoderno,
                                     vartype = c("cv")))

D6modernos_dom1

NI_dom = diseno %>% #group_by(dominio) %>%
  summarise(n = unweighted(n()),
            NI = survey_mean(unmet_need_2 %in% c(1,2)))


NI_dom1 = diseno %>% #group_by(dominio) %>%
  summarise(n = unweighted(n()),
            NI = survey_mean(nec_ins))



D7_dom1 = diseno %>% filter(P_EST_CIVIL_C%in%c(1,2))%>%
  #group_by(dominio) %>%
  summarise(n = unweighted(n()),
            p = survey_ratio(current_method %in% c(3),
                             (current_method_grou %in% c(1,2,3) +
                                unmet_need_2 %in% c(1,2)),
                             vartype = c("cv"), 
                             proportion = TRUE, 
                             na.rm = TRUE))

D7_dom1

LARC_dom1 = design.base %>% #group_by(dominio) %>%
  summarise(n = unweighted(n()),
            LARC = survey_mean(current_method %in% c(2,11)))

LARC_area


####################################### Desagregaciones #######################


############################### Por Área ###############################

## Usa métodos de Planificación D6

D6_area = diseno %>% group_by(area) %>%
  summarise(n = unweighted(n()),
            D6 = survey_mean(current_method_grou %in% c(1,2,3),
                             vartype = c("cv")))

D6_area <- as.data.frame(D6_area)
D6_area

openxlsx::write.xlsx(D6_area, file = "Colombia_D6_area.xlsx")


## Métodos Moderno D6m

D6modernos_area = diseno %>% group_by(area) %>%
  summarise(n = unweighted(n()),
            D6 = survey_mean(usamoderno,
                             vartype = c("cv")))

D6modernos_area <- as.data.frame(D6modernos_area)
D6modernos_area

openxlsx::write.xlsx(D6modernos_area, file = "Colombia_D6_Modernos_area.xlsx")



## Necesidades Insatisfechas NI

NI_area = diseno %>% group_by(area) %>%
  summarise(n = unweighted(n()),
            NI = survey_mean(nec_ins),
            vartype = c("cv"))

NI_area <- as.data.frame(NI_area)
NI_area

openxlsx::write.xlsx(NI_area, file = "Colombia_NI_area.xlsx")


## Indicador D7

D7_area = diseno %>% filter(P_EST_CIVIL_C%in%c(1,2)) %>%
  group_by(area) %>%
  summarise(n = unweighted(n()),
            p = survey_ratio(current_method %in% c(3),
                             (current_method_grou %in% c(1,2,3) +
                                unmet_need_2 %in% c(1,2)),
                             vartype = c("cv"), 
                             proportion = TRUE, 
                             na.rm = TRUE))

D7_area <- as.data.frame(D7_area)
D7_area

openxlsx::write.xlsx(D7_area, file = "Colombia_D7_area.xlsx")



## Indicador LARC

LARC_area = diseno %>% group_by(area) %>%
  summarise(n = unweighted(n()),
            LARC = survey_mean(current_method %in% c(2,11)),
            vartype = c("cv"))

LARC_area <- as.data.frame(LARC_area)
LARC_area

openxlsx::write.xlsx(LARC_area, file = "Colombia_LARC_area.xlsx")



##############################################################

############################### Por Departamento ###############################

## Usa métodos de Planificación D6

D6_depto = diseno %>% group_by(departamento) %>%
  summarise(n = unweighted(n()),
            D6 = survey_mean(current_method_grou %in% c(1,2,3),
                             vartype = c("cv")))

D6_depto <- as.data.frame(D6_depto)
D6_depto

openxlsx::write.xlsx(D6_depto, file = "Colombia_D6_depto.xlsx")


## Métodos Moderno D6m

D6modernos_depto = diseno %>% group_by(departamento) %>%
  summarise(n = unweighted(n()),
            D6 = survey_mean(usamoderno,
                             vartype = c("cv")))

D6modernos_depto <- as.data.frame(D6modernos_depto)
D6modernos_depto

openxlsx::write.xlsx(D6modernos_depto, file = "Colombia_D6modernos_depto.xlsx")



## Necesidades Insatisfechas NI

NI_depto = diseno %>% group_by(departamento) %>%
  summarise(n = unweighted(n()),
            NI = survey_mean(nec_ins),
            vartype = c("cv"))

NI_depto <- as.data.frame(NI_depto)
NI_depto

openxlsx::write.xlsx(NI_depto, file = "Colombia_NI_depto.xlsx")


## Indicador D7

D7_depto = diseno %>% filter(P_EST_CIVIL_C%in%c(1,2)) %>%
  group_by(departamento) %>%
  summarise(n = unweighted(n()),
            p = survey_ratio(current_method %in% c(3),
                             (current_method_grou %in% c(1,2,3) +
                                unmet_need_2 %in% c(1,2)),
                             vartype = c("cv"), 
                             proportion = TRUE, 
                             na.rm = TRUE))

D7_depto <- as.data.frame(D7_depto)
D7_depto

openxlsx::write.xlsx(D7_depto, file = "Colombia_D7_depto.xlsx")



## Indicador LARC

LARC_depto = diseno %>% group_by(departamento) %>%
  summarise(n = unweighted(n()),
            LARC = survey_mean(current_method %in% c(2,11)),
            vartype = c("cv"))

LARC_depto <- as.data.frame(LARC_depto)
LARC_depto

openxlsx::write.xlsx(LARC_depto, file = "Colombia_LARC_depto.xlsx")

##############################################################

############################### Por Grupo Edad ###############################

## Usa métodos de Planificación D6

D6_edad = diseno %>% group_by(edad) %>%
  summarise(n = unweighted(n()),
            D6 = survey_mean(current_method_grou %in% c(1,2,3),
                             vartype = c("cv")))

D6_edad <- as.data.frame(D6_edad)
D6_edad

openxlsx::write.xlsx(D6_edad, file = "Colombia_D6_edad.xlsx")


## Métodos Moderno D6m

D6modernos_edad = diseno %>% group_by(edad) %>%
  summarise(n = unweighted(n()),
            D6 = survey_mean(usamoderno,
                             vartype = c("cv")))

D6modernos_edad <- as.data.frame(D6modernos_edad)
D6modernos_edad

openxlsx::write.xlsx(D6modernos_edad, file = "Colombia_D6modernos_edad.xlsx")



## Necesidades Insatisfechas NI

NI_edad = diseno %>% group_by(edad) %>%
  summarise(n = unweighted(n()),
            NI = survey_mean(nec_ins),
            vartype = c("cv"))

NI_edad <- as.data.frame(NI_edad)
NI_edad

openxlsx::write.xlsx(NI_edad, file = "Colombia_NI_edad.xlsx")


## Indicador D7

D7_edad = diseno %>% filter(P_EST_CIVIL_C%in%c(1,2)) %>%
  group_by(edad) %>%
  summarise(n = unweighted(n()),
            p = survey_ratio(current_method %in% c(3),
                             (current_method_grou %in% c(1,2,3) +
                                unmet_need_2 %in% c(1,2)),
                             vartype = c("cv"), 
                             proportion = TRUE, 
                             na.rm = TRUE))

D7_edad <- as.data.frame(D7_edad)
D7_edad

openxlsx::write.xlsx(D7_edad, file = "Colombia_D7_edad.xlsx")


## Indicador LARC

LARC_edad = diseno %>% group_by(edad) %>%
  summarise(n = unweighted(n()),
            LARC = survey_mean(current_method %in% c(2,11)),
            vartype = c("cv"))

LARC_edad <- as.data.frame(LARC_edad)
LARC_edad

openxlsx::write.xlsx(LARC_edad, file = "Colombia_LARC_edad.xlsx")

##############################################################

############################### Por Etnia ###############################

## Usa métodos de Planificación D6

D6_etnia = diseno %>% group_by(etnia) %>%
  summarise(n = unweighted(n()),
            D6 = survey_mean(current_method_grou %in% c(1,2,3),
                             vartype = c("cv")))

D6_etnia <- as.data.frame(D6_etnia)
D6_etnia

openxlsx::write.xlsx(D6_etnia, file = "Colombia_D6_etnia.xlsx")


## Métodos Moderno D6m

D6modernos_etnia = diseno %>% group_by(etnia) %>%
  summarise(n = unweighted(n()),
            D6 = survey_mean(usamoderno,
                             vartype = c("cv")))

D6modernos_etnia <- as.data.frame(D6modernos_etnia)
D6modernos_etnia

openxlsx::write.xlsx(D6modernos_etnia, file = "Colombia_D6modernos_etnia.xlsx")


## Necesidades Insatisfechas NI

NI_etnia = diseno %>% group_by(etnia) %>%
  summarise(n = unweighted(n()),
            NI = survey_mean(nec_ins),
            vartype = c("cv"))

NI_etnia <- as.data.frame(NI_etnia)
NI_etnia

openxlsx::write.xlsx(NI_etnia, file = "Colombia_NI_etnia.xlsx")


## Indicador D7

D7_etnia = diseno %>% filter(P_EST_CIVIL_C%in%c(1,2)) %>%
  group_by(etnia) %>%
  summarise(n = unweighted(n()),
            p = survey_ratio(current_method %in% c(3),
                             (current_method_grou %in% c(1,2,3) +
                                unmet_need_2 %in% c(1,2)),
                             vartype = c("cv"), 
                             proportion = TRUE, 
                             na.rm = TRUE))

D7_etnia <- as.data.frame(D7_etnia)
D7_etnia

openxlsx::write.xlsx(D7_etnia, file = "Colombia_D7_etnia.xlsx")



## Indicador LARC

LARC_etnia = diseno %>% group_by(etnia) %>%
  summarise(n = unweighted(n()),
            LARC = survey_mean(current_method %in% c(2,11)),
            vartype = c("cv"))

LARC_etnia <- as.data.frame(LARC_etnia)
LARC_etnia

openxlsx::write.xlsx(LARC_etnia, file = "Colombia_LARC_etnia.xlsx")

##############################################################



############################### Por Municipio ###############################

## Usa métodos de Planificación D6

D6_mcpio = diseno %>% group_by(paste(departamento,municipio)) %>%
  summarise(n = unweighted(n()),
            D6 = survey_mean(current_method_grou %in% c(1,2,3),
                             vartype = c("cv")))

D6_mcpio <- as.data.frame(D6_mcpio)
D6_mcpio

openxlsx::write.xlsx(D6_mcpio, file = "Colombia_D6_mcpio.xlsx")


## Métodos Moderno D6m

D6modernos_mcpio = diseno %>% group_by(paste(departamento,municipio)) %>%
  summarise(n = unweighted(n()),
            D6 = survey_mean(usamoderno,
                             vartype = c("cv")))

D6modernos_mcpio <- as.data.frame(D6modernos_mcpio)
D6modernos_mcpio

openxlsx::write.xlsx(D6modernos_mcpio, file = "Colombia_D6modernos_mcpio.xlsx")



## Necesidades Insatisfechas NI

NI_mcpio = diseno %>% group_by(paste(departamento,municipio)) %>%
  summarise(n = unweighted(n()),
            NI = survey_mean(nec_ins),
            vartype = c("cv"))

NI_mcpio <- as.data.frame(NI_mcpio)
NI_mcpio

openxlsx::write.xlsx(NI_mcpio, file = "Colombia_NI_mcpio.xlsx")


## Indicador D7


D7_mcpio = diseno %>% filter(P_EST_CIVIL_C%in%c(1,2)) %>%
  group_by(paste(departamento,municipio)) %>%
  summarise(n = unweighted(n()),
            p = survey_ratio(current_method %in% c(3),
                             (current_method_grou %in% c(1,2,3) +
                                unmet_need_2 %in% c(1,2)),
                             vartype = c("cv"), 
                             proportion = TRUE, 
                             na.rm = TRUE))

D7_mcpio <- as.data.frame(D7_mcpio)
D7_mcpio

openxlsx::write.xlsx(D7_mcpio, file = "Colombia_D7_mcpio.xlsx")



## Indicador LARC

LARC_mcpio = diseno %>% group_by(paste(departamento,municipio)) %>%
  summarise(n = unweighted(n()),
            LARC = survey_mean(current_method %in% c(2,11)),
            vartype = c("cv"))

LARC_mcpio <- as.data.frame(LARC_mcpio)
LARC_mcpio

openxlsx::write.xlsx(LARC_mcpio, file = "Colombia_LARC_mcpio.xlsx")

###############################################################

########################### Por Area y Escolaridad ###############################

## Usa métodos de Planificación D6

D6_area_escol = diseno %>% group_by(area, escolaridad) %>%
  summarise(n = unweighted(n()),
            D6 = survey_mean(current_method_grou %in% c(1,2,3),
                             vartype = c("cv")))

D6_area_escol <- as.data.frame(D6_area_escol)
D6_area_escol

openxlsx::write.xlsx(D6_area_escol, file = "Colombia_D6_area_escol.xlsx")


## Métodos Moderno D6m

D6modernos_area_escol = diseno %>% group_by(area, escolaridad) %>%
  summarise(n = unweighted(n()),
            D6 = survey_mean(usamoderno,
                             vartype = c("cv")))

D6modernos_area_escol <- as.data.frame(D6modernos_area_escol)
D6modernos_area_escol

openxlsx::write.xlsx(D6modernos_area_escol, file = "Colombia_D6modernos_area_escol.xlsx")



## Necesidades Insatisfechas NI

NI_area_escol = diseno %>% group_by(area, escolaridad) %>%
  summarise(n = unweighted(n()),
            NI = survey_mean(nec_ins),
            vartype = c("cv"))

NI_area_escol <- as.data.frame(NI_area_escol)
NI_area_escol

openxlsx::write.xlsx(NI_area_escol, file = "Colombia_NI_area_escol.xlsx")


## Indicador D7

D7_area_escol = diseno %>% filter(P_EST_CIVIL_C%in%c(1,2))%>%
  group_by(area, escolaridad) %>%
  summarise(n = unweighted(n()),
            p = survey_ratio(current_method %in% c(3),
                             (current_method_grou %in% c(1,2,3) +
                                unmet_need_2 %in% c(1,2)),
                             vartype = c("cv"), 
                             proportion = TRUE, 
                             na.rm = TRUE))

D7_area_escol <- as.data.frame(D7_area_escol)
D7_area_escol

openxlsx::write.xlsx(D7_area_escol, file = "Colombia_D7_area_escol.xlsx")



## Indicador LARC

LARC_area_escol = diseno %>% group_by(area, escolaridad) %>%
  summarise(n = unweighted(n()),
            LARC = survey_mean(current_method %in% c(2,11)),
            vartype = c("cv"))

LARC_area_escol <- as.data.frame(LARC_area_escol)
LARC_area_escol

openxlsx::write.xlsx(LARC_area_escol, file = "Colombia_LARC_area_escol.xlsx")

##############################################################


########################### Por departamento y edad ###############################

## Usa métodos de Planificación D6

D6_depto_edad = diseno %>% group_by(departamento, edad) %>%
  summarise(n = unweighted(n()),
            D6 = survey_mean(current_method_grou %in% c(1,2,3),
                             vartype = c("cv")))

D6_depto_edad <- as.data.frame(D6_depto_edad)
D6_depto_edad

openxlsx::write.xlsx(D6_depto_edad, file = "Colombia_D6_depto_edad.xlsx")


## Métodos Moderno D6m

D6modernos_depto_edad = diseno %>% group_by(departamento, edad) %>%
  summarise(n = unweighted(n()),
            D6 = survey_mean(usamoderno,
                             vartype = c("cv")))

D6modernos_depto_edad <- as.data.frame(D6modernos_depto_edad)
D6modernos_depto_edad

openxlsx::write.xlsx(D6modernos_depto_edad, file = "Colombia_D6modernos_depto_edad.xlsx")

## Necesidades Insatisfechas NI

NI_depto_edad = diseno %>% group_by(departamento, edad) %>%
  summarise(n = unweighted(n()),
            NI = survey_mean(nec_ins),
            vartype = c("cv"))

NI_depto_edad <- as.data.frame(NI_depto_edad)
NI_depto_edad

openxlsx::write.xlsx(NI_depto_edad, file = "Colombia_NI_depto_edad.xlsx")


## Indicador D7

D7_depto_edad = diseno %>% filter(P_EST_CIVIL_C%in%c(1,2)) %>%
  group_by(departamento, edad) %>%
  summarise(n = unweighted(n()),
            p = survey_ratio(current_method %in% c(3),
                             (current_method_grou %in% c(1,2,3) +
                                unmet_need_2 %in% c(1,2)),
                             vartype = c("cv"), 
                             proportion = TRUE, 
                             na.rm = TRUE))

D7_depto_edad <- as.data.frame(D7_depto_edad)
D7_depto_edad

openxlsx::write.xlsx(D7_depto_edad, file = "Colombia_D7_depto_edad.xlsx")



## Indicador LARC

LARC_depto_edad = diseno %>% group_by(departamento, edad) %>%
  summarise(n = unweighted(n()),
            LARC = survey_mean(current_method %in% c(2,11)),
            vartype = c("cv"))

LARC_depto_edad <- as.data.frame(LARC_depto_edad)
LARC_depto_edad

openxlsx::write.xlsx(LARC_depto_edad, file = "Colombia_LARC_depto_edad.xlsx")

##############################################################



























##############################################################
##############################################################
##############################################################
##############################################################
##############################################################
##############################################################
##############################################################
##############################################################
##############################################################
##############################################################

# Nacional ----------------------------------------------------------------
# Nacional - Urbano - rural

D6 = diseno %>% group_by(area) %>%
  cascade(n = unweighted(n()),
          D6 = survey_mean(current_method_grou %in% c(1,2,3),
                           vartype = c("se","cv")),
          .fill = "Nacional")

D6

D6modernos = diseno %>% group_by(area) %>%
  cascade(n = unweighted(n()),
          D6modernos = survey_mean(current_method_grou == 3,
                                   vartype = c("se","cv")),
          .fill = "Nacional")

NI = diseno %>% group_by(area) %>%
  cascade(n = unweighted(n()),
          NI = survey_mean(unmet_need_2_2 %in% c(1,2),
                           vartype = c("se","cv")),
          .fill = "Nacional")

labelled::val_labels(encuesta$current_method)

LARC = diseno %>% group_by(area) %>%
  cascade(n = unweighted(n()),
          LARC = survey_mean(current_method %in% c(2,11),
                             vartype = c("se","cv")),
          .fill = "Nacional")

D7 = diseno %>% filter(P_EST_CIVIL_C%in%c(1,2)) %>%
  group_by(area) %>%
  cascade(n = unweighted(n()),
          D7 = survey_ratio(current_method %in% c(3),(current_method_grou %in% c(1,2,3) + unmet_need_2_2 %in% c(1,2)),
                            vartype = c("se","cv")),
          .fill = "Nacional")

# Nacional ----------------------------------------------------------------
# Nacional - departamental

D6_depto = diseno %>% group_by(departamento) %>%
  summarise(n = unweighted(n()),
            D6 = survey_mean(current_method_grou %in% c(1,2,3),
                             vartype = c("se","cv")))

D6modernos_depto = diseno %>% group_by(departamento) %>%
  summarise(n = unweighted(n()),
            D6modernos = survey_mean(current_method_grou == 3,
                                     vartype = c("se","cv")))

NI_depto = diseno %>% group_by(departamento) %>%
  summarise(n = unweighted(n()),
            NI = survey_mean(unmet_need_2_2 %in% c(1,2),
                             vartype = c("se","cv")))

labelled::val_labels(encuesta$current_method)

LARC_depto = diseno %>% group_by(departamento) %>%
  summarise(n = unweighted(n()),
            LARC = survey_mean(current_method %in% c(2,11),
                               vartype = c("se","cv")))

D7_depto = diseno %>% filter(P_EST_CIVIL_C%in%c(1,2)) %>%
  group_by(departamento) %>%
  summarise(n = unweighted(n()),
            D7 = survey_ratio(current_method %in% c(3),(current_method_grou %in% c(1,2,3) + unmet_need_2_2 %in% c(1,2)),
                              vartype = c("se","cv")))

# Nacional ----------------------------------------------------------------
# Etnia

D6_etnia = diseno %>% group_by(PA1_GRP_ETNIC_C) %>%
  summarise(n = unweighted(n()),
            D6 = survey_mean(current_method_grou %in% c(1,2,3),
                             vartype = c("se","cv")))

D6modernos_etnia = diseno %>% group_by(PA1_GRP_ETNIC_C) %>%
  summarise(n = unweighted(n()),
            D6modernos = survey_mean(current_method_grou == 3,
                                     vartype = c("se","cv")))

NI_etnia = diseno %>% group_by(PA1_GRP_ETNIC_C) %>%
  summarise(n = unweighted(n()),
            NI = survey_mean(unmet_need_2_2 %in% c(1,2),
                             vartype = c("se","cv")))

labelled::val_labels(encuesta$current_method)

LARC_etnia = diseno %>% group_by(PA1_GRP_ETNIC_C) %>%
  summarise(n = unweighted(n()),
            LARC = survey_mean(current_method %in% c(2,11),
                               vartype = c("se","cv")))

D7_etnia = diseno %>% filter(P_EST_CIVIL_C%in%c(1,2)) %>%
  group_by(PA1_GRP_ETNIC_C) %>%
  summarise(n = unweighted(n()),
            D7 = survey_ratio(current_method %in% c(3),(current_method_grou %in% c(1,2,3) + unmet_need_2 %in% c(1,2)),
                              vartype = c("se","cv")))

# Nacional ----------------------------------------------------------------
# Edad

D6_edad = diseno %>% group_by(P_EDAD_C) %>%
  summarise(n = unweighted(n()),
            D6 = survey_mean(current_method_grou %in% c(1,2,3),
                             vartype = c("se","cv")))

D6modernos_edad = diseno %>% group_by(P_EDAD_C) %>%
  summarise(n = unweighted(n()),
            D6modernos = survey_mean(current_method_grou == 3,
                                     vartype = c("se","cv")))

NI_edad = diseno %>% group_by(P_EDAD_C) %>%
  summarise(n = unweighted(n()),
            NI = survey_mean(unmet_need_2 %in% c(1,2),
                             vartype = c("se","cv")))

labelled::val_labels(encuesta$current_method)

LARC_edad = diseno %>% group_by(P_EDAD_C) %>%
  summarise(n = unweighted(n()),
            LARC = survey_mean(current_method %in% c(2,11),
                               vartype = c("se","cv")))

D7_edad = diseno %>% filter(P_EST_CIVIL_C%in%c(1,2)) %>%
  group_by(P_EDAD_C) %>%
  summarise(n = unweighted(n()),
            D7 = survey_ratio(current_method %in% c(3),(current_method_grou %in% c(1,2,3) + unmet_need_2 %in% c(1,2)),
                              vartype = c("se","cv")))



