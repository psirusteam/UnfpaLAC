############################################################################################################################################
#############################################################################################################################################
# # 
# # Proceso: Estimación indicadores de planificación
# #              familiar utilizando ENDES 2021
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

EndesMEF = readRDS("PER/2021/D6/Output/EndesMEF.rds")

######################################################################################################################################################

sum(is.na(EndesMEF$DS))
#ENDESMEF = EndesMEF

#### Creando Dummys para cada Indicador de Interés


EndesMEF$D6  <- ifelse(EndesMEF$usametodo    == 1,1,0)
EndesMEF$D6m <- ifelse(EndesMEF$usamoderno   == 1,1,0)
EndesMEF$NI  <- ifelse(EndesMEF$Nec_ins_pf_T == 1,1,0)
EndesMEF$D7  <- ifelse(EndesMEF$usamoderno   == 1 &(EndesMEF$Falla_met==1|
                                                          EndesMEF$Nec_sat_pf_T==1),1,0)
EndesMEF = EndesMEF %>% filter(EdadQ!="12-14")
EndesMEF$EdadQ = as.factor(as.character(EndesMEF$EdadQ))
levels(EndesMEF$EdadQ)

# Creación objetivo diseño de muestreo complejo

options(survey.lonely.psu = "adjust") 
design.base = EndesMEF %>% as_survey_design(ids = V021, strat = V022, 
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


####################################### Desagregaciones #######################


############################### Por Área ###############################

## Usa métodos de Planificación D6

D6_area = design.base %>% group_by(Area) %>%
  summarise(n = unweighted(n()),
            D6 = survey_mean(usametodo,
                             vartype = c("cv")))

D6_area <- as.data.frame(D6_area)
D6_area

openxlsx::write.xlsx(D6_area, file = "PER/2021/D6/Output/Tablas/D6_area.xlsx")


## Métodos Moderno D6m

D6modernos_area = design.base %>% group_by(Area) %>%
  summarise(n = unweighted(n()),
            D6 = survey_mean(usamoderno,
                             vartype = c("cv")))

D6modernos_area <- as.data.frame(D6modernos_area)
D6modernos_area

openxlsx::write.xlsx(D6modernos_area, file = "PER/2021/D6/Output/Tablas/D6_Modernos_area.xlsx")



## Necesidades Insatisfechas NI

NI_area = design.base %>% group_by(Area) %>%
  summarise(n = unweighted(n()),
            NI = survey_mean(Nec_ins_pf_T),
            vartype = c("cv"))

NI_area <- as.data.frame(NI_area)
NI_area

openxlsx::write.xlsx(NI_area, file = "PER/2021/D6/Output/Tablas/NI_area.xlsx")


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

openxlsx::write.xlsx(D7_area, file = "PER/2021/D6/Output/Tablas/D7_area.xlsx")



##############################################################

############################### Por Departamento ###############################

## Usa métodos de Planificación D6

D6_depto = design.base %>% group_by(Departamento) %>%
  summarise(n = unweighted(n()),
            D6 = survey_mean(usametodo,
                             vartype = c("cv")))

D6_depto <- as.data.frame(D6_depto)
D6_depto

openxlsx::write.xlsx(D6_depto, file = "PER/2021/D6/Output/Tablas/D6_depto.xlsx")


## Métodos Moderno D6m

D6modernos_depto = design.base %>% group_by(Departamento) %>%
  summarise(n = unweighted(n()),
            D6 = survey_mean(usamoderno,
                             vartype = c("cv")))

D6modernos_depto <- as.data.frame(D6modernos_depto)
D6modernos_depto

openxlsx::write.xlsx(D6modernos_depto, file = "PER/2021/D6/Output/Tablas/D6_modernos_depto.xlsx")



## Necesidades Insatisfechas NI

NI_depto = design.base %>% group_by(Departamento) %>%
  summarise(n = unweighted(n()),
            NI = survey_mean(Nec_ins_pf_T),
            vartype = c("cv"))

NI_depto <- as.data.frame(NI_depto)
NI_depto

openxlsx::write.xlsx(NI_depto, file = "PER/2021/D6/Output/Tablas/NI_depto.xlsx")


## Indicador D7

D7_depto = design.base %>% filter(ec.casado== 1 | ec.conviviente == 1 ) %>%
  group_by(Departamento) %>%
  summarise(n = unweighted(n()),
            p = survey_ratio(usamoderno,usametodo+Nec_ins_pf_T,
                             vartype = c("cv"), 
                             proportion = TRUE, 
                             na.rm = TRUE))

D7_depto <- as.data.frame(D7_depto)
D7_depto

openxlsx::write.xlsx(D7_depto, file = "PER/2021/D6/Output/Tablas/D7_depto.xlsx")


##############################################################

############################### Por Grupo Edad ###############################

## Usa métodos de Planificación D6

D6_edad = design.base %>% group_by(EdadQ) %>%
  summarise(n = unweighted(n()),
            D6 = survey_mean(usametodo,
                             vartype = c("cv")))

D6_edad <- as.data.frame(D6_edad)
D6_edad

openxlsx::write.xlsx(D6_edad, file = "PER/2021/D6/Output/Tablas/D6_edad.xlsx")


## Métodos Moderno D6m

D6modernos_edad = design.base %>% group_by(EdadQ) %>%
  summarise(n = unweighted(n()),
            D6 = survey_mean(usamoderno,
                             vartype = c("cv")))

D6modernos_edad <- as.data.frame(D6modernos_edad)
D6modernos_edad

openxlsx::write.xlsx(D6modernos_edad, file = "PER/2021/D6/Output/Tablas/D6_modernos_edad.xlsx")

## Necesidades Insatisfechas NI

NI_edad = design.base %>% group_by(EdadQ) %>%
  summarise(n = unweighted(n()),
            NI = survey_mean(Nec_ins_pf_T),
            vartype = c("cv"))

NI_edad <- as.data.frame(NI_edad)
NI_edad

openxlsx::write.xlsx(NI_edad, file = "PER/2021/D6/Output/Tablas/NI_edad.xlsx")


## Indicador D7

D7_edad = design.base %>% filter(ec.casado== 1 | ec.conviviente == 1 ) %>%
  group_by(EdadQ) %>%
  summarise(n = unweighted(n()),
            p = survey_ratio(usamoderno,usametodo+Nec_ins_pf_T,
                             vartype = c("cv"), 
                             proportion = TRUE, 
                             na.rm = TRUE))

D7_edad <- as.data.frame(D7_edad)
D7_edad

openxlsx::write.xlsx(D7_edad, file = "PER/2021/D6/Output/Tablas/D7_edad.xlsx")


##############################################################

############################### Por Etnia ###############################

## Usa métodos de Planificación D6

D6_etnia = design.base %>% group_by(etnia) %>%
  summarise(n = unweighted(n()),
            D6 = survey_mean(usametodo,
                             vartype = c("cv")))

D6_etnia <- as.data.frame(D6_etnia)
D6_etnia

openxlsx::write.xlsx(D6_etnia, file = "PER/2021/D6/Output/Tablas/D6_etnia.xlsx")


## Métodos Moderno D6m

D6modernos_etnia = design.base %>% group_by(etnia) %>%
  summarise(n = unweighted(n()),
            D6 = survey_mean(usamoderno,
                             vartype = c("cv")))

D6modernos_etnia <- as.data.frame(D6modernos_etnia)
D6modernos_etnia

openxlsx::write.xlsx(D6modernos_etnia, file = "PER/2021/D6/Output/Tablas/D6_modernos_etnia.xlsx")

## Necesidades Insatisfechas NI

NI_etnia = design.base %>% group_by(etnia) %>%
  summarise(n = unweighted(n()),
            NI = survey_mean(Nec_ins_pf_T),
            vartype = c("cv"))

NI_etnia <- as.data.frame(NI_etnia)
NI_etnia

openxlsx::write.xlsx(NI_etnia, file = "PER/2021/D6/Output/Tablas/NI_etnia.xlsx")


## Indicador D7

D7_etnia = design.base %>% filter(ec.casado== 1 | ec.conviviente == 1 ) %>%
  group_by(etnia) %>%
  summarise(n = unweighted(n()),
            p = survey_ratio(usamoderno,usametodo+Nec_ins_pf_T,
                             vartype = c("cv"), 
                             proportion = TRUE, 
                             na.rm = TRUE))

D7_etnia <- as.data.frame(D7_etnia)
D7_etnia

openxlsx::write.xlsx(D7_etnia, file = "PER/2021/D6/Output/Tablas/D7_etnia.xlsx")



##############################################################



############################### Por Provincia ###############################

## Usa métodos de Planificación D6

D6_pvcia = design.base %>% group_by(Provincia) %>%
  summarise(n = unweighted(n()),
            D6 = survey_mean(usametodo,
                             vartype = c("cv")))

D6_pvcia <- as.data.frame(D6_pvcia)
D6_pvcia

openxlsx::write.xlsx(D6_pvcia, file = "PER/2021/D6/Output/Tablas/D6_provincia.xlsx")


## Métodos Moderno D6m

D6modernos_pvcia = design.base %>% group_by(Provincia) %>%
  summarise(n = unweighted(n()),
            D6 = survey_mean(usamoderno,
                             vartype = c("cv")))

D6modernos_pvcia <- as.data.frame(D6modernos_pvcia)
D6modernos_pvcia

openxlsx::write.xlsx(D6modernos_pvcia, file = "PER/2021/D6/Output/Tablas/D6_modernos_provincia.xlsx")



## Necesidades Insatisfechas NI

NI_pvcia = design.base %>% group_by(Provincia) %>%
  summarise(n = unweighted(n()),
            NI = survey_mean(Nec_ins_pf_T),
            vartype = c("cv"))

NI_pvcia <- as.data.frame(NI_pvcia)
NI_pvcia

openxlsx::write.xlsx(NI_pvcia, file = "PER/2021/D6/Output/Tablas/NI_provincia.xlsx")


## Indicador D7


D7_pvcia = design.base %>% filter(ec.casado== 1 | ec.conviviente == 1 ) %>%
  group_by(Provincia) %>%
  summarise(n = unweighted(n()),
            p = survey_ratio(usamoderno,usametodo+Nec_ins_pf_T,
                             vartype = c("cv"), 
                             proportion = TRUE, 
                             na.rm = TRUE))

D7_pvcia <- as.data.frame(D7_pvcia)
D7_pvcia

openxlsx::write.xlsx(D7_pvcia, file = "PER/2021/D6/Output/Tablas/D7_provincia.xlsx")


###############################################################

########################### Por Area y Escolaridad ###############################

## Usa métodos de Planificación D6

D6_area_escol = design.base %>% group_by(Area, nivel.educ) %>%
  summarise(n = unweighted(n()),
            D6 = survey_mean(usametodo,
                             vartype = c("cv")))

D6_area_escol <- as.data.frame(D6_area_escol)
D6_area_escol

openxlsx::write.xlsx(D6_area_escol, file = "PER/2021/D6/Output/Tablas/D6_area_escol.xlsx")


## Métodos Moderno D6m

D6modernos_area_escol = design.base %>% group_by(Area, nivel.educ) %>%
  summarise(n = unweighted(n()),
            D6 = survey_mean(usamoderno,
                             vartype = c("cv")))

D6modernos_area_escol <- as.data.frame(D6modernos_area_escol)
D6modernos_area_escol

openxlsx::write.xlsx(D6modernos_area_escol, file = "PER/2021/D6/Output/Tablas/D6_modernos_area_escol.xlsx")



## Necesidades Insatisfechas NI

NI_area_escol = design.base %>% group_by(Area, nivel.educ) %>%
  summarise(n = unweighted(n()),
            NI = survey_mean(Nec_ins_pf_T),
            vartype = c("cv"))

NI_area_escol <- as.data.frame(NI_area_escol)
NI_area_escol

openxlsx::write.xlsx(NI_area_escol, file = "PER/2021/D6/Output/Tablas/NI_area_escol.xlsx")


## Indicador D7

D7_area_escol = design.base %>% filter(ec.casado== 1 | ec.conviviente == 1 )%>%
  group_by(Area, nivel.educ) %>%
  summarise(n = unweighted(n()),
            p = survey_ratio(usamoderno,usametodo+Nec_ins_pf_T,
                             vartype = c("cv"), 
                             proportion = TRUE, 
                             na.rm = TRUE))

D7_area_escol <- as.data.frame(D7_area_escol)
D7_area_escol

openxlsx::write.xlsx(D7_area_escol, file = "PER/2021/D6/Output/Tablas/D7_area_escol.xlsx")

##############################################################


########################### Por departamento y edad ###############################

## Usa métodos de Planificación D6

D6_depto_edad = design.base %>% group_by(Departamento, EdadQ) %>%
  summarise(n = unweighted(n()),
            D6 = survey_mean(usametodo,
                             vartype = c("cv")))

D6_depto_edad <- as.data.frame(D6_depto_edad)
D6_depto_edad

openxlsx::write.xlsx(D6_depto_edad, file = "PER/2021/D6/Output/Tablas/D6_depto_edad.xlsx")


## Métodos Moderno D6m

D6modernos_depto_edad = design.base %>% group_by(Departamento, EdadQ) %>%
  summarise(n = unweighted(n()),
            D6 = survey_mean(usamoderno,
                             vartype = c("cv")))

D6modernos_depto_edad <- as.data.frame(D6modernos_depto_edad)
D6modernos_depto_edad

openxlsx::write.xlsx(D6modernos_depto_edad, file = "PER/2021/D6/Output/Tablas/D6_modernos_depto_edad.xlsx")



## Necesidades Insatisfechas NI

NI_depto_edad = design.base %>% group_by(Departamento, EdadQ) %>%
  summarise(n = unweighted(n()),
            NI = survey_mean(Nec_ins_pf_T),
            vartype = c("cv"))

NI_depto_edad <- as.data.frame(NI_depto_edad)
NI_depto_edad

openxlsx::write.xlsx(NI_depto_edad, file = "PER/2021/D6/Output/Tablas/NI_depto_edad.xlsx")


## Indicador D7

D7_depto_edad = design.base %>% filter(ec.casado== 1 | ec.conviviente == 1 ) %>%
  group_by(Departamento, EdadQ) %>%
  summarise(n = unweighted(n()),
            p = survey_ratio(usamoderno,usametodo+Nec_ins_pf_T,
                             vartype = c("cv"), 
                             proportion = TRUE, 
                             na.rm = TRUE))

D7_depto_edad <- as.data.frame(D7_depto_edad)
D7_depto_edad

openxlsx::write.xlsx(D7_depto_edad, file = "PER/2021/D6/Output/Tablas/D7_depto_edad.xlsx")


########################################################################



#####################################################################
#####################################################################
#####################################################################

## Todas las mujeres

D6nacional = design.base %>%
  group_by(corte.edad = EdadQ) %>% 
  cascade(n = unweighted(n()), 
          p = survey_mean(usametodo,
                          vartype = c("ci", "cv", "se"), 
                          proportion = TRUE, 
                          na.rm = TRUE), 
          defff = survey_mean(usametodo, 
                              na.rm = TRUE, 
                              deff = TRUE),
          y = unweighted(sum(usametodo)),
          .fill = "Total") %>% 
  mutate(LI = p_low, 
         LS = p_upp, 
         CV = 100 * p_cv, 
         deff = defff_deff, 
         n.eff = n/deff,
         CVl = ifelse(p <= 0.5, 
                      100 * p_se / (-p * log(p)), 
                      100 * p_se / (-(1 - p) * log(1 - p)))) %>% 
  select(p, LI, LS, ee = p_se, CV, deff, n.eff, y, CVl) %>% 
  as.data.frame()

Tabla = EndesMEF %>% 
  mutate_at(vars(c(EdadQ)), list(~as.character(.))) %>%
  bind_rows(mutate(., EdadQ = "Total")) %>%
  group_by(EdadQ) %>% 
  summarise(n = n(), 
            estratos = length(unique(V022)), 
            UPMs = length(unique(V021)), 
            grados = UPMs - estratos) %>% 
  mutate(Subpoblacion = EdadQ) %>% 
  select(Subpoblacion, n, grados) 

TablaD6N <- cbind(Tabla, D6nacional) %>% 
  select(Subpoblacion, p, LI, LS,ee, CV, deff, n, n.eff, grados, y, CVl) %>%
  mutate(flag = ifelse(CV > cvflag | n < nflag | n.eff < nefflag | grados < dfflag | y < yflag | CVl > cvflag, 
                       "*", ""))

saveRDS(TablaD6N,"PER/2021/D6/Output/Tablas/D6nacional_Subp.rds")


## Todas las mujeres - modernos

D6nacional.corte = design.base %>%
  group_by(corte.edad = EdadQ) %>% 
  cascade(n = unweighted(n()), 
          p = survey_mean(usamoderno,
                          vartype = c("ci", "cv", "se"), 
                          proportion = TRUE, 
                          na.rm = TRUE), 
          defff = survey_mean(usamoderno, 
                              na.rm = TRUE, 
                              deff = TRUE),
          y = unweighted(sum(usamoderno)),
          .fill = "Total") %>% 
  mutate(LI = p_low, 
         LS = p_upp, 
         CV = 100 * p_cv, 
         deff = defff_deff, 
         n.eff = n/deff,
         CVl = ifelse(p <= 0.5, 
                      100 * p_se / (-p * log(p)), 
                      100 * p_se / (-(1 - p) * log(1 - p)))) %>% 
  select(p, LI, LS,ee = p_se, CV, deff, n.eff, y, CVl) %>% 
  as.data.frame()

Tabla = EndesMEF %>% 
  mutate_at(vars(c(EdadQ)), list(~as.character(.))) %>%
  bind_rows(mutate(., EdadQ = "Total")) %>%
  group_by(EdadQ) %>% 
  summarise(n = n(), 
            estratos = length(unique(V022)), 
            UPMs = length(unique(V021)), 
            grados = UPMs - estratos) %>% 
  mutate(Subpoblacion = EdadQ) %>% 
  select(Subpoblacion, n, grados) 

TablaD6N <- cbind(Tabla, D6nacional.corte) %>% 
  select(Subpoblacion, p, LI, LS,ee, CV, deff, n, n.eff, grados, y, CVl) %>%
  mutate(flag = ifelse(CV > cvflag | n < nflag | n.eff < nefflag | grados < dfflag | y < yflag | CVl > cvflag, 
                       "*", ""))

saveRDS(TablaD6N,"PER/2021/D6/Output/Tablas/D6M_Nacional_Subp.rds")

## Sexualmente activas no unidas - todos los metodos

D6nacional.sa = design.base %>% filter(MSA == "Si",union_1!="Unida") %>%
  group_by(EdadQ) %>% 
  cascade(n = unweighted(n()), 
          p = survey_mean(usametodo,
                          vartype = c("ci", "cv", "se"), 
                          proportion = TRUE, 
                          na.rm = TRUE), 
          defff = survey_mean(usametodo, 
                              na.rm = TRUE, 
                              deff = TRUE),
          y = unweighted(sum(usametodo,na.rm = TRUE)),
          .fill = "Total") %>% 
  mutate(LI = p_low, 
         LS = p_upp, 
         CV = 100 * p_cv, 
         deff = defff_deff, 
         n.eff = n/deff,
         CVl = ifelse(p <= 0.5, 
                      100 * p_se / (-p * log(p)), 
                      100 * p_se / (-(1 - p) * log(1 - p)))) %>% 
  select(p, LI, LS, ee = p_se, CV, deff, n.eff, y, CVl) %>% 
  as.data.frame()

Tabla = EndesMEF %>% filter(MSA == "Si",union_1 != "Unida") %>%
  mutate_at(vars(c(EdadQ)), list(~as.character(.))) %>%
  bind_rows(mutate(., EdadQ = "Total")) %>%
  group_by(EdadQ) %>% 
  summarise(n = n(), 
            estratos = length(unique(V022)), 
            UPMs = length(unique(V021)), 
            grados = UPMs - estratos) %>% 
  mutate(Subpoblacion = EdadQ) %>% 
  select(Subpoblacion, n, grados) 

TablaD6N <- cbind(Tabla, D6nacional.sa) %>% 
  select(Subpoblacion, p, LI, LS, ee, CV, deff, n, n.eff, grados, y, CVl) %>%
  mutate(flag = ifelse(CV > cvflag | n < nflag | n.eff < nefflag | grados < dfflag | y < yflag | CVl > cvflag, 
                       "*", ""))
## se considera mujeres sexualmente activas no unidas
saveRDS(TablaD6N,"PER/2021/D6/Output/Tablas/D6_Nacional_SANU.rds")

## Sexualmente activas no unidas - modernos

D6nacional.sa = design.base %>% filter(MSA == "Si",union_1!="Unida") %>%
  group_by(EdadQ) %>% 
  cascade(n = unweighted(n()), 
          p = survey_mean(usamoderno,
                          vartype = c("ci", "cv", "se"), 
                          proportion = TRUE, 
                          na.rm = TRUE), 
          defff = survey_mean(usamoderno, 
                              na.rm = TRUE, 
                              deff = TRUE),
          y = unweighted(sum(usamoderno)),
          .fill = "Total") %>% 
  mutate(LI = p_low, 
         LS = p_upp, 
         CV = 100 * p_cv, 
         deff = defff_deff, 
         n.eff = n/deff,
         CVl = ifelse(p <= 0.5, 
                      100 * p_se / (-p * log(p)), 
                      100 * p_se / (-(1 - p) * log(1 - p)))) %>% 
  select(p, LI, LS, ee = p_se, CV, deff, n.eff, y, CVl) %>% 
  as.data.frame()

Tabla = EndesMEF %>% filter(MSA == "Si",union_1 != "Unida") %>%
  mutate_at(vars(c(EdadQ)), list(~as.character(.))) %>%
  bind_rows(mutate(., EdadQ = "Total")) %>%
  group_by(EdadQ) %>% 
  summarise(n = n(), 
            estratos = length(unique(V022)), 
            UPMs = length(unique(V021)), 
            grados = UPMs - estratos) %>% 
  mutate(Subpoblacion = EdadQ) %>% 
  select(Subpoblacion, n, grados) 

TablaD6N <- cbind(Tabla, D6nacional.sa) %>% 
  select(Subpoblacion, p, LI, LS, ee, CV, deff, n, n.eff, grados, y, CVl) %>%
  mutate(flag = ifelse(CV > cvflag | n < nflag | n.eff < nefflag | grados < dfflag | y < yflag | CVl > cvflag, 
                       "*", ""))
## se considera mujeres sexualmente activas no unidas
saveRDS(TablaD6N,"PER/2021/D6/Output/Tablas/D6M_nacionalSANU.rds")

## Unidas - todos los metodos

D6nacional.U = design.base %>% filter(union_1=="Unida") %>%
  group_by(EdadQ) %>% 
  cascade(n = unweighted(n()), 
          p = survey_mean(usametodo,
                          vartype = c("ci", "cv", "se"), 
                          proportion = TRUE, 
                          na.rm = TRUE), 
          defff = survey_mean(usametodo, 
                              na.rm = TRUE, 
                              deff = TRUE),
          y = unweighted(sum(usametodo,na.rm = TRUE)),
          .fill = "Total") %>% 
  mutate(LI = p_low, 
         LS = p_upp, 
         CV = 100 * p_cv, 
         deff = defff_deff, 
         n.eff = n/deff,
         CVl = ifelse(p <= 0.5, 
                      100 * p_se / (-p * log(p)), 
                      100 * p_se / (-(1 - p) * log(1 - p)))) %>% 
  select(p, LI, LS, ee = p_se, CV, deff, n.eff, y, CVl) %>% 
  as.data.frame()

Tabla = EndesMEF %>% filter(union_1 == "Unida") %>%
  mutate_at(vars(c(EdadQ)), list(~as.character(.))) %>%
  bind_rows(mutate(., EdadQ = "Total")) %>%
  group_by(EdadQ) %>% 
  summarise(n = n(), 
            estratos = length(unique(V022)), 
            UPMs = length(unique(V021)), 
            grados = UPMs - estratos) %>% 
  mutate(Subpoblacion = EdadQ) %>% 
  select(Subpoblacion, n, grados) 

TablaD6N <- cbind(Tabla, D6nacional.U) %>% 
  select(Subpoblacion, p, LI, LS, ee, CV, deff, n, n.eff, grados, y, CVl) %>%
  mutate(flag = ifelse(CV > cvflag | n < nflag | n.eff < nefflag | grados < dfflag | y < yflag | CVl > cvflag, 
                       "*", ""))
saveRDS(TablaD6N,"PER/2021/D6/Output/Tablas/D6_nacional_U.rds")

## Unidas - modernos

D6nacional.U = design.base %>% filter(union_1=="Unida") %>%
  group_by(EdadQ) %>% 
  cascade(n = unweighted(n()), 
          p = survey_mean(usamoderno,
                          vartype = c("ci", "cv", "se"), 
                          proportion = TRUE, 
                          na.rm = TRUE), 
          defff = survey_mean(usamoderno, 
                              na.rm = TRUE, 
                              deff = TRUE),
          y = unweighted(sum(usamoderno)),
          .fill = "Total") %>% 
  mutate(LI = p_low, 
         LS = p_upp, 
         CV = 100 * p_cv, 
         deff = defff_deff, 
         n.eff = n/deff,
         CVl = ifelse(p <= 0.5, 
                      100 * p_se / (-p * log(p)), 
                      100 * p_se / (-(1 - p) * log(1 - p)))) %>% 
  select(p, LI, LS, ee = p_se, CV, deff, n.eff, y, CVl) %>% 
  as.data.frame()

Tabla = EndesMEF %>% filter(union_1 == "Unida") %>%
  mutate_at(vars(c(EdadQ)), list(~as.character(.))) %>%
  bind_rows(mutate(., EdadQ = "Total")) %>%
  group_by(EdadQ) %>% 
  summarise(n = n(), 
            estratos = length(unique(V022)), 
            UPMs = length(unique(V021)), 
            grados = UPMs - estratos) %>% 
  mutate(Subpoblacion = EdadQ) %>% 
  select(Subpoblacion, n, grados) 

TablaD6N <- cbind(Tabla, D6nacional.U) %>% 
  select(Subpoblacion, p, LI, LS, ee, CV, deff, n, n.eff, grados, y, CVl) %>%
  mutate(flag = ifelse(CV > cvflag | n < nflag | n.eff < nefflag | grados < dfflag | y < yflag | CVl > cvflag, 
                       "*", ""))

saveRDS(TablaD6N,"PER/2021/D6/Output/Tablas/D6Mnacional_U.rds")

## Sexualmente activas - todos los metodos

D6nacional.sa = design.base %>% filter(MSA == "Si") %>%
  group_by(EdadQ) %>% 
  cascade(n = unweighted(n()), 
          p = survey_mean(usametodo,
                          vartype = c("ci", "cv", "se"), 
                          proportion = TRUE, 
                          na.rm = TRUE), 
          defff = survey_mean(usametodo, 
                              na.rm = TRUE, 
                              deff = TRUE),
          y = unweighted(sum(usametodo,na.rm = TRUE)),
          .fill = "Total") %>% 
  mutate(LI = p_low, 
         LS = p_upp, 
         CV = 100 * p_cv, 
         deff = defff_deff, 
         n.eff = n/deff,
         CVl = ifelse(p <= 0.5, 
                      100 * p_se / (-p * log(p)), 
                      100 * p_se / (-(1 - p) * log(1 - p)))) %>% 
  select(p, LI, LS, ee = p_se, CV, deff, n.eff, y, CVl) %>% 
  as.data.frame()

Tabla = EndesMEF %>% filter(MSA == "Si") %>%
  mutate_at(vars(c(EdadQ)), list(~as.character(.))) %>%
  bind_rows(mutate(., EdadQ = "Total")) %>%
  group_by(EdadQ) %>% 
  summarise(n = n(), 
            estratos = length(unique(V022)), 
            UPMs = length(unique(V021)), 
            grados = UPMs - estratos) %>% 
  mutate(Subpoblacion = EdadQ) %>% 
  select(Subpoblacion, n, grados) 

TablaD6N <- cbind(Tabla, D6nacional.sa) %>% 
  select(Subpoblacion, p, LI, LS, ee, CV, deff, n, n.eff, grados, y, CVl) %>%
  mutate(flag = ifelse(CV > cvflag | n < nflag | n.eff < nefflag | grados < dfflag | y < yflag | CVl > cvflag, 
                       "*", ""))

saveRDS(TablaD6N,"PER/2021/D6/Output/Tablas/D6nacional_SA.rds")

## Sexualmente activas  - modernos

D6nacional.sa = design.base %>% filter(MSA == "Si") %>%
  group_by(EdadQ) %>% 
  cascade(n = unweighted(n()), 
          p = survey_mean(usamoderno,
                          vartype = c("ci", "cv", "se"), 
                          proportion = TRUE, 
                          na.rm = TRUE), 
          defff = survey_mean(usamoderno, 
                              na.rm = TRUE, 
                              deff = TRUE),
          y = unweighted(sum(usamoderno)),
          .fill = "Total") %>% 
  mutate(LI = p_low, 
         LS = p_upp, 
         CV = 100 * p_cv, 
         deff = defff_deff, 
         n.eff = n/deff,
         CVl = ifelse(p <= 0.5, 
                      100 * p_se / (-p * log(p)), 
                      100 * p_se / (-(1 - p) * log(1 - p)))) %>% 
  select(p, LI, LS, ee = p_se, CV, deff, n.eff, y, CVl) %>% 
  as.data.frame()

Tabla = EndesMEF %>% filter(MSA == "Si") %>%
  mutate_at(vars(c(EdadQ)), list(~as.character(.))) %>%
  bind_rows(mutate(., EdadQ = "Total")) %>%
  group_by(EdadQ) %>% 
  summarise(n = n(), 
            estratos = length(unique(V022)), 
            UPMs = length(unique(V021)), 
            grados = UPMs - estratos) %>% 
  mutate(Subpoblacion = EdadQ) %>% 
  select(Subpoblacion, n, grados) 

TablaD6N <- cbind(Tabla, D6nacional.sa) %>% 
  select(Subpoblacion, p, LI, LS, ee, CV, deff, n, n.eff, grados, y, CVl) %>%
  mutate(flag = ifelse(CV > cvflag | n < nflag | n.eff < nefflag | grados < dfflag | y < yflag | CVl > cvflag, 
                       "*", ""))

saveRDS(TablaD6N,"PER/2021/D6/Output/Tablas/D6Mnacional_SA.rds")


###################
##  Nivel Area   ##
###################


#### Todas las mujeres - Todos los metodos

D6area = design.base %>%
  group_by(Area) %>% 
  cascade(n = unweighted(n()), 
          p = survey_mean(usametodo,
                          vartype = c("ci", "cv", "se"), 
                          proportion = TRUE, 
                          na.rm = TRUE), 
          defff = survey_mean(usametodo, 
                              na.rm = TRUE, 
                              deff = TRUE),
          y = unweighted(sum(usametodo)),
          .fill = "Total") %>% 
  mutate(LI = p_low, 
         LS = p_upp, 
         CV = 100 * p_cv, 
         deff = defff_deff, 
         n.eff = n/deff,
         CVl = ifelse(p <= 0.5, 
                      100 * p_se / (-p * log(p)), 
                      100 * p_se / (-(1 - p) * log(1 - p)))) %>% 
  select(p, LI, LS, ee = p_se, CV, deff, n.eff, y, CVl) %>% 
  as.data.frame()

Tabla = EndesMEF %>% 
  mutate_at(vars(c(Area)), list(~as.character(.))) %>%
  bind_rows(mutate(., Area = "Total")) %>%
  group_by(Area) %>% 
  summarise(n = n(), 
            estratos = length(unique(V022)), 
            UPMs = length(unique(V021)), 
            grados = UPMs - estratos) %>% 
  mutate(Subpoblacion = Area) %>% 
  select(Subpoblacion, n, grados) %>% slice(3,1,2)

TablaD6N <- cbind(Tabla, D6area) %>% 
  select(Subpoblacion, p, LI, LS, ee, CV, deff, n, n.eff, grados, y, CVl) %>%
  mutate(flag = ifelse(CV > cvflag | n < nflag | n.eff < nefflag | grados < dfflag | y < yflag | CVl > cvflag, 
                       "*", ""))

saveRDS(TablaD6N,"PER/2021/D6/Output/Tablas/D6_area_subp.rds")

#### Todas las mujeres - metodos modernos

D6area = design.base %>%
  group_by(Area) %>% 
  cascade(n = unweighted(n()), 
          p = survey_mean(usamoderno,
                          vartype = c("ci", "cv", "se"), 
                          proportion = TRUE, 
                          na.rm = TRUE), 
          defff = survey_mean(usamoderno, 
                              na.rm = TRUE, 
                              deff = TRUE),
          y = unweighted(sum(usamoderno)),
          .fill = "Total") %>% 
  mutate(LI = p_low, 
         LS = p_upp, 
         CV = 100 * p_cv, 
         deff = defff_deff, 
         n.eff = n/deff,
         CVl = ifelse(p <= 0.5, 
                      100 * p_se / (-p * log(p)), 
                      100 * p_se / (-(1 - p) * log(1 - p)))) %>% 
  select(p, LI, LS, ee = p_se, CV, deff, n.eff, y, CVl) %>% 
  as.data.frame()

Tabla = EndesMEF %>% 
  mutate_at(vars(c(Area)), list(~as.character(.))) %>%
  bind_rows(mutate(., Area = "Total")) %>%
  group_by(Area) %>% 
  summarise(n = n(), 
            estratos = length(unique(V022)), 
            UPMs = length(unique(V021)), 
            grados = UPMs - estratos) %>% 
  mutate(Subpoblacion = Area) %>% 
  select(Subpoblacion, n, grados)  %>% slice(3,1,2)

TablaD6N <- cbind(Tabla, D6area) %>% 
  select(Subpoblacion, p, LI, LS, ee, CV, deff, n, n.eff, grados, y, CVl) %>%
  mutate(flag = ifelse(CV > cvflag | n < nflag | n.eff < nefflag | grados < dfflag | y < yflag | CVl > cvflag, 
                       "*", ""))

saveRDS(TablaD6N,"PER/2021/D6/Output/Tablas/D6M_area_subp.rds")

#### Unidas - Todos los metodos

D6area = design.base %>% filter(union_1=="Unida") %>%
  group_by(Area) %>% 
  cascade(n = unweighted(n()), 
          p = survey_mean(usametodo,
                          vartype = c("ci", "cv", "se"), 
                          proportion = TRUE, 
                          na.rm = TRUE), 
          defff = survey_mean(usametodo, 
                              na.rm = TRUE, 
                              deff = TRUE),
          y = unweighted(sum(usametodo)),
          .fill = "Total") %>% 
  mutate(LI = p_low, 
         LS = p_upp, 
         CV = 100 * p_cv, 
         deff = defff_deff, 
         n.eff = n/deff,
         CVl = ifelse(p <= 0.5, 
                      100 * p_se / (-p * log(p)), 
                      100 * p_se / (-(1 - p) * log(1 - p)))) %>% 
  select(p, LI, LS, ee = p_se, CV, deff, n.eff, y, CVl) %>% 
  as.data.frame()

Tabla = EndesMEF %>% filter(union_1=="Unida") %>%
  mutate_at(vars(c(Area)), list(~as.character(.))) %>%
  bind_rows(mutate(., Area = "Total")) %>%
  group_by(Area) %>% 
  summarise(n = n(), 
            estratos = length(unique(V022)), 
            UPMs = length(unique(V021)), 
            grados = UPMs - estratos) %>% 
  mutate(Subpoblacion = Area) %>% 
  select(Subpoblacion, n, grados)  %>% slice(3,1,2)

TablaD6N <- cbind(Tabla, D6area) %>% 
  select(Subpoblacion, p, LI, LS, ee, CV, deff, n, n.eff, grados, y, CVl) %>%
  mutate(flag = ifelse(CV > cvflag | n < nflag | n.eff < nefflag | grados < dfflag | y < yflag | CVl > cvflag, 
                       "*", ""))

saveRDS(TablaD6N,"PER/2021/D6/Output/Tablas/D6area_U.rds")

#### Unidas - metodo moderno

D6area = design.base %>% filter(union_1=="Unida") %>%
  group_by(Area) %>% 
  cascade(n = unweighted(n()), 
          p = survey_mean(usamoderno,
                          vartype = c("ci", "cv", "se"), 
                          proportion = TRUE, 
                          na.rm = TRUE), 
          defff = survey_mean(usamoderno, 
                              na.rm = TRUE, 
                              deff = TRUE),
          y = unweighted(sum(usamoderno)),
          .fill = "Total") %>% 
  mutate(LI = p_low, 
         LS = p_upp, 
         CV = 100 * p_cv, 
         deff = defff_deff, 
         n.eff = n/deff,
         CVl = ifelse(p <= 0.5, 
                      100 * p_se / (-p * log(p)), 
                      100 * p_se / (-(1 - p) * log(1 - p)))) %>% 
  select(p, LI, LS, ee = p_se, CV, deff, n.eff, y, CVl) %>% 
  as.data.frame()

Tabla = EndesMEF %>% filter(union_1=="Unida") %>%
  mutate_at(vars(c(Area)), list(~as.character(.))) %>%
  bind_rows(mutate(., Area = "Total")) %>%
  group_by(Area) %>% 
  summarise(n = n(), 
            estratos = length(unique(V022)), 
            UPMs = length(unique(V021)), 
            grados = UPMs - estratos) %>% 
  mutate(Subpoblacion = Area) %>% 
  select(Subpoblacion, n, grados) %>% slice(3,1,2)

TablaD6N <- cbind(Tabla, D6area) %>% 
  select(Subpoblacion, p, LI, LS, ee, CV, deff, n, n.eff, grados, y, CVl) %>%
  mutate(flag = ifelse(CV > cvflag | n < nflag | n.eff < nefflag | grados < dfflag | y < yflag | CVl > cvflag, 
                       "*", ""))

saveRDS(TablaD6N,"PER/2021/D6/Output/Tablas/D6M_area_U.rds")

#### Sexualmente activas no unidas - Todos los metodos

D6area = design.base %>% filter(union_1!="Unida",MSA=="Si") %>%
  group_by(Area) %>% 
  cascade(n = unweighted(n()), 
          p = survey_mean(usametodo,
                          vartype = c("ci", "cv", "se"), 
                          proportion = TRUE, 
                          na.rm = TRUE), 
          defff = survey_mean(usametodo, 
                              na.rm = TRUE, 
                              deff = TRUE),
          y = unweighted(sum(usametodo)),
          .fill = "Total") %>% 
  mutate(LI = p_low, 
         LS = p_upp, 
         CV = 100 * p_cv, 
         deff = defff_deff, 
         n.eff = n/deff,
         CVl = ifelse(p <= 0.5, 
                      100 * p_se / (-p * log(p)), 
                      100 * p_se / (-(1 - p) * log(1 - p)))) %>% 
  select(p, LI, LS, ee = p_se, CV, deff, n.eff, y, CVl) %>% 
  as.data.frame()

Tabla = EndesMEF %>% filter(union_1!="Unida",MSA=="Si") %>%
  mutate_at(vars(c(Area)), list(~as.character(.))) %>%
  bind_rows(mutate(., Area = "Total")) %>%
  group_by(Area) %>% 
  summarise(n = n(), 
            estratos = length(unique(V022)), 
            UPMs = length(unique(V021)), 
            grados = UPMs - estratos) %>% 
  mutate(Subpoblacion = Area) %>% 
  select(Subpoblacion, n, grados)  %>% slice(3,1,2)

TablaD6N <- cbind(Tabla, D6area) %>% 
  select(Subpoblacion, p, LI, LS, ee, CV, deff, n, n.eff, grados, y, CVl) %>%
  mutate(flag = ifelse(CV > cvflag | n < nflag | n.eff < nefflag | grados < dfflag | y < yflag | CVl > cvflag, 
                       "*", ""))

saveRDS(TablaD6N,"PER/2021/D6/Output/Tablas/D6_area_SANU.rds")

#### Sexualmente activas no unidas - metodos modernos

D6area = design.base %>% filter(union_1!="Unida",MSA=="Si") %>%
  group_by(Area) %>% 
  cascade(n = unweighted(n()), 
          p = survey_mean(usamoderno,
                          vartype = c("ci", "cv", "se"), 
                          proportion = TRUE, 
                          na.rm = TRUE), 
          defff = survey_mean(usamoderno, 
                              na.rm = TRUE, 
                              deff = TRUE),
          y = unweighted(sum(usamoderno)),
          .fill = "Total") %>% 
  mutate(LI = p_low, 
         LS = p_upp, 
         CV = 100 * p_cv, 
         deff = defff_deff, 
         n.eff = n/deff,
         CVl = ifelse(p <= 0.5, 
                      100 * p_se / (-p * log(p)), 
                      100 * p_se / (-(1 - p) * log(1 - p)))) %>% 
  select(p, LI, LS, ee = p_se, CV, deff, n.eff, y, CVl) %>% 
  as.data.frame()

Tabla = EndesMEF %>% filter(union_1!="Unida",MSA=="Si") %>%
  mutate_at(vars(c(Area)), list(~as.character(.))) %>%
  bind_rows(mutate(., Area = "Total")) %>%
  group_by(Area) %>% 
  summarise(n = n(), 
            estratos = length(unique(V022)), 
            UPMs = length(unique(V021)), 
            grados = UPMs - estratos) %>% 
  mutate(Subpoblacion = Area) %>% 
  select(Subpoblacion, n, grados)  %>% slice(3,1,2)

TablaD6N <- cbind(Tabla, D6area) %>% 
  select(Subpoblacion, p, LI, LS, ee, CV, deff, n, n.eff, grados, y, CVl) %>%
  mutate(flag = ifelse(CV > cvflag | n < nflag | n.eff < nefflag | grados < dfflag | y < yflag | CVl > cvflag, 
                       "*", ""))

saveRDS(TablaD6N,"Tablas/D6MareaSANU.rds")

#### Sexualmente activas - Todos los metodos

D6area = design.base %>% filter(MSA=="Si") %>%
  group_by(Area) %>% 
  cascade(n = unweighted(n()), 
          p = survey_mean(usametodo,
                          vartype = c("ci", "cv", "se"), 
                          proportion = TRUE, 
                          na.rm = TRUE), 
          defff = survey_mean(usametodo, 
                              na.rm = TRUE, 
                              deff = TRUE),
          y = unweighted(sum(usametodo)),
          .fill = "Total") %>% 
  mutate(LI = p_low, 
         LS = p_upp, 
         CV = 100 * p_cv, 
         deff = defff_deff, 
         n.eff = n/deff,
         CVl = ifelse(p <= 0.5, 
                      100 * p_se / (-p * log(p)), 
                      100 * p_se / (-(1 - p) * log(1 - p)))) %>% 
  select(p, LI, LS, ee = p_se, CV, deff, n.eff, y, CVl) %>% 
  as.data.frame()

Tabla = EndesMEF %>% filter(MSA=="Si") %>%
  mutate_at(vars(c(Area)), list(~as.character(.))) %>%
  bind_rows(mutate(., Area = "Total")) %>%
  group_by(Area) %>% 
  summarise(n = n(), 
            estratos = length(unique(V022)), 
            UPMs = length(unique(V021)), 
            grados = UPMs - estratos) %>% 
  mutate(Subpoblacion = Area) %>% 
  select(Subpoblacion, n, grados)  %>% slice(3,1,2)

TablaD6N <- cbind(Tabla, D6area) %>% 
  select(Subpoblacion, p, LI, LS, ee, CV, deff, n, n.eff, grados, y, CVl) %>%
  mutate(flag = ifelse(CV > cvflag | n < nflag | n.eff < nefflag | grados < dfflag | y < yflag | CVl > cvflag, 
                       "*", ""))

saveRDS(TablaD6N,"Tablas/D6areaSA.rds")

#### Sexualmente activas - metodos modernos

D6area = design.base %>% filter(MSA=="Si") %>%
  group_by(Area) %>% 
  cascade(n = unweighted(n()), 
          p = survey_mean(usamoderno,
                          vartype = c("ci", "cv", "se"), 
                          proportion = TRUE, 
                          na.rm = TRUE), 
          defff = survey_mean(usamoderno, 
                              na.rm = TRUE, 
                              deff = TRUE),
          y = unweighted(sum(usamoderno)),
          .fill = "Total") %>% 
  mutate(LI = p_low, 
         LS = p_upp, 
         CV = 100 * p_cv, 
         deff = defff_deff, 
         n.eff = n/deff,
         CVl = ifelse(p <= 0.5, 
                      100 * p_se / (-p * log(p)), 
                      100 * p_se / (-(1 - p) * log(1 - p)))) %>% 
  select(p, LI, LS, ee = p_se, CV, deff, n.eff, y, CVl) %>% 
  as.data.frame()

Tabla = EndesMEF %>% filter(MSA=="Si") %>%
  mutate_at(vars(c(Area)), list(~as.character(.))) %>%
  bind_rows(mutate(., Area = "Total")) %>%
  group_by(Area) %>% 
  summarise(n = n(), 
            estratos = length(unique(V022)), 
            UPMs = length(unique(V021)), 
            grados = UPMs - estratos) %>% 
  mutate(Subpoblacion = Area) %>% 
  select(Subpoblacion, n, grados)  %>% slice(3,1,2)

TablaD6N <- cbind(Tabla, D6area) %>% 
  select(Subpoblacion, p, LI, LS, ee, CV, deff, n, n.eff, grados, y, CVl) %>%
  mutate(flag = ifelse(CV > cvflag | n < nflag | n.eff < nefflag | grados < dfflag | y < yflag | CVl > cvflag, 
                       "*", ""))

saveRDS(TablaD6N,"Tablas/D6MareaSA.rds")


#########################
## Nivel departamento  ##
#########################

#### Todas las mujeres - Todos los metodos

D6depto = design.base %>%
  group_by(Departamento) %>% 
  cascade(n = unweighted(n()), 
          p = survey_mean(usametodo,
                          vartype = c("ci", "cv", "se"), 
                          proportion = TRUE, 
                          na.rm = TRUE), 
          defff = survey_mean(usametodo, 
                              na.rm = TRUE, 
                              deff = TRUE),
          y = unweighted(sum(usametodo)),
          .fill = "Total") %>% 
  mutate(LI = p_low, 
         LS = p_upp, 
         CV = 100 * p_cv, 
         deff = defff_deff, 
         n.eff = n/deff,
         CVl = ifelse(p <= 0.5, 
                      100 * p_se / (-p * log(p)), 
                      100 * p_se / (-(1 - p) * log(1 - p)))) %>% 
  select(p, LI, LS, ee = p_se, CV, deff, n.eff, y, CVl) %>% 
  as.data.frame()


Tabla = EndesMEF %>% 
  mutate_at(vars(c(Departamento)), list(~as.character(.))) %>%
  bind_rows(mutate(., Departamento = "Total")) %>%
  group_by(Departamento) %>% 
  summarise(n = n(), 
            estratos = length(unique(V022)), 
            UPMs = length(unique(V021)), 
            grados = UPMs - estratos) %>% 
  mutate(Subpoblacion = Departamento) %>% 
  select(Subpoblacion, n, grados) %>% slice(1:23,25,26,24)

TablaD6N <- cbind(Tabla, D6depto) %>% 
  select(Subpoblacion, p, LI, LS, ee, CV, deff, n, n.eff, grados, y, CVl) %>%
  mutate(flag = ifelse(CV > cvflag | n < nflag | n.eff < nefflag | grados < dfflag | y < yflag | CVl > cvflag, 
                       "*", ""))

saveRDS(TablaD6N,"Tablas/D6depto.rds")

#### Todas las mujeres - modernos

D6depto = design.base %>%
  group_by(Departamento) %>% 
  cascade(n = unweighted(n()), 
          p = survey_mean(usamoderno,
                          vartype = c("ci", "cv", "se"), 
                          proportion = TRUE, 
                          na.rm = TRUE), 
          defff = survey_mean(usamoderno, 
                              na.rm = TRUE, 
                              deff = TRUE),
          y = unweighted(sum(usamoderno)),
          .fill = "Total") %>% 
  mutate(LI = p_low, 
         LS = p_upp, 
         CV = 100 * p_cv, 
         deff = defff_deff, 
         n.eff = n/deff,
         CVl = ifelse(p <= 0.5, 
                      100 * p_se / (-p * log(p)), 
                      100 * p_se / (-(1 - p) * log(1 - p)))) %>% 
  select(p, LI, LS, ee = p_se, CV, deff, n.eff, y, CVl) %>% 
  as.data.frame()


Tabla = EndesMEF %>% 
  mutate_at(vars(c(Departamento)), list(~as.character(.))) %>%
  bind_rows(mutate(., Departamento = "Total")) %>%
  group_by(Departamento) %>% 
  summarise(n = n(), 
            estratos = length(unique(V022)), 
            UPMs = length(unique(V021)), 
            grados = UPMs - estratos) %>% 
  mutate(Subpoblacion = Departamento) %>% 
  select(Subpoblacion, n, grados) %>% slice(1:23,25,26,24)

TablaD6N <- cbind(Tabla, D6depto) %>% 
  select(Subpoblacion, p, LI, LS, ee, CV, deff, n, n.eff, grados, y, CVl) %>%
  mutate(flag = ifelse(CV > cvflag | n < nflag | n.eff < nefflag | grados < dfflag | y < yflag | CVl > cvflag, 
                       "*", ""))

saveRDS(TablaD6N,"Tablas/D6Mdepto.rds")


#### Mujeres sexualmente activas no unidas - Todos los metodos

D6depto = design.base %>% filter(MSA == "Si", union_1 != "Unida") %>%
  group_by(Departamento) %>% 
  cascade(n = unweighted(n()), 
          p = survey_mean(usametodo,
                          vartype = c("ci", "cv", "se"), 
                          proportion = TRUE, 
                          na.rm = TRUE), 
          defff = survey_mean(usametodo, 
                              na.rm = TRUE, 
                              deff = TRUE),
          y = unweighted(sum(usametodo)),
          .fill = "Total") %>% 
  mutate(LI = p_low, 
         LS = p_upp, 
         CV = 100 * p_cv, 
         deff = defff_deff, 
         n.eff = n/deff,
         CVl = ifelse(p <= 0.5, 
                      100 * p_se / (-p * log(p)), 
                      100 * p_se / (-(1 - p) * log(1 - p)))) %>% 
  select(p, LI, LS, ee = p_se, CV, deff, n.eff, y, CVl) %>% 
  as.data.frame()


Tabla = EndesMEF %>% filter(MSA == "Si", union_1 != "Unida") %>%
  mutate_at(vars(c(Departamento)), list(~as.character(.))) %>%
  bind_rows(mutate(., Departamento = "Total")) %>%
  group_by(Departamento) %>% 
  summarise(n = n(), 
            estratos = length(unique(V022)), 
            UPMs = length(unique(V021)), 
            grados = UPMs - estratos) %>% 
  mutate(Subpoblacion = Departamento) %>% 
  select(Subpoblacion, n, grados) %>% slice(1:23,25,26,24)

TablaD6N <- cbind(Tabla, D6depto) %>% 
  select(Subpoblacion, p, LI, LS, ee, CV, deff, n, n.eff, grados, y, CVl) %>%
  mutate(flag = ifelse(CV > cvflag | n < nflag | n.eff < nefflag | grados < dfflag | y < yflag | CVl > cvflag, 
                       "*", ""))


saveRDS(TablaD6N,"Tablas/D6deptoSANU.rds")

#### Mujeres sexualmente activas no unidas - modernos

D6depto = design.base %>% filter(MSA == "Si", union_1!= "Unida") %>% 
  group_by(Departamento) %>% 
  cascade(n = unweighted(n()), 
          p = survey_mean(usamoderno,
                          vartype = c("ci", "cv", "se"), 
                          proportion = TRUE, 
                          na.rm = TRUE), 
          defff = survey_mean(usamoderno, 
                              na.rm = TRUE, 
                              deff = TRUE),
          y = unweighted(sum(usamoderno)),
          .fill = "Total") %>% 
  mutate(LI = p_low, 
         LS = p_upp, 
         CV = 100 * p_cv, 
         deff = defff_deff, 
         n.eff = n/deff,
         CVl = ifelse(p <= 0.5, 
                      100 * p_se / (-p * log(p)), 
                      100 * p_se / (-(1 - p) * log(1 - p)))) %>% 
  select(p, LI, LS, ee = p_se, CV, deff, n.eff, y, CVl) %>% 
  as.data.frame()


Tabla = EndesMEF %>% filter(MSA == "Si", union_1 != "Unida") %>%
  mutate_at(vars(c(Departamento)), list(~as.character(.))) %>%
  bind_rows(mutate(., Departamento = "Total")) %>%
  group_by(Departamento) %>% 
  summarise(n = n(), 
            estratos = length(unique(V022)), 
            UPMs = length(unique(V021)), 
            grados = UPMs - estratos) %>% 
  mutate(Subpoblacion = Departamento) %>% 
  select(Subpoblacion, n, grados) %>% slice(1:23,25,26,24)

TablaD6N <- cbind(Tabla, D6depto) %>% 
  select(Subpoblacion, p, LI, LS, ee, CV, deff, n, n.eff, grados, y, CVl) %>%
  mutate(flag = ifelse(CV > cvflag | n < nflag | n.eff < nefflag | grados < dfflag | y < yflag | CVl > cvflag, 
                       "*", ""))

saveRDS(TablaD6N,"Tablas/D6MdeptoSANU.rds")

#### Mujeres Unidas - Todos los metodos

D6depto = design.base %>% filter(union_1 == "Unida") %>%
  group_by(Departamento) %>% 
  cascade(n = unweighted(n()), 
          p = survey_mean(usametodo,
                          vartype = c("ci", "cv", "se"), 
                          proportion = TRUE, 
                          na.rm = TRUE), 
          defff = survey_mean(usametodo, 
                              na.rm = TRUE, 
                              deff = TRUE),
          y = unweighted(sum(usametodo)),
          .fill = "Total") %>% 
  mutate(LI = p_low, 
         LS = p_upp, 
         CV = 100 * p_cv, 
         deff = defff_deff, 
         n.eff = n/deff,
         CVl = ifelse(p <= 0.5, 
                      100 * p_se / (-p * log(p)), 
                      100 * p_se / (-(1 - p) * log(1 - p)))) %>% 
  select(p, LI, LS, ee = p_se, CV, deff, n.eff, y, CVl) %>% 
  as.data.frame()


Tabla = EndesMEF %>% filter(union_1 == "Unida") %>%
  mutate_at(vars(c(Departamento)), list(~as.character(.))) %>%
  bind_rows(mutate(., Departamento = "Total")) %>%
  group_by(Departamento) %>% 
  summarise(n = n(), 
            estratos = length(unique(V022)), 
            UPMs = length(unique(V021)), 
            grados = UPMs - estratos) %>% 
  mutate(Subpoblacion = Departamento) %>% 
  select(Subpoblacion, n, grados) %>% slice(1:23,25,26,24)

TablaD6N <- cbind(Tabla, D6depto) %>% 
  select(Subpoblacion, p, LI, LS, ee, CV, deff, n, n.eff, grados, y, CVl) %>%
  mutate(flag = ifelse(CV > cvflag | n < nflag | n.eff < nefflag | grados < dfflag | y < yflag | CVl > cvflag, 
                       "*", ""))


saveRDS(TablaD6N,"Tablas/D6deptoU.rds")

#### Mujeres Unidas - modernos

D6depto = design.base %>% filter(union_1 == "Unida") %>% 
  group_by(Departamento) %>% 
  cascade(n = unweighted(n()), 
          p = survey_mean(usamoderno,
                          vartype = c("ci", "cv", "se"), 
                          proportion = TRUE, 
                          na.rm = TRUE), 
          defff = survey_mean(usamoderno, 
                              na.rm = TRUE, 
                              deff = TRUE),
          y = unweighted(sum(usamoderno)),
          .fill = "Total") %>% 
  mutate(LI = p_low, 
         LS = p_upp, 
         CV = 100 * p_cv, 
         deff = defff_deff, 
         n.eff = n/deff,
         CVl = ifelse(p <= 0.5, 
                      100 * p_se / (-p * log(p)), 
                      100 * p_se / (-(1 - p) * log(1 - p)))) %>% 
  select(p, LI, LS, ee = p_se, CV, deff, n.eff, y, CVl) %>% 
  as.data.frame()


Tabla = EndesMEF %>% filter(union_1 == "Unida") %>%
  mutate_at(vars(c(Departamento)), list(~as.character(.))) %>%
  bind_rows(mutate(., Departamento = "Total")) %>%
  group_by(Departamento) %>% 
  summarise(n = n(), 
            estratos = length(unique(V022)), 
            UPMs = length(unique(V021)), 
            grados = UPMs - estratos) %>% 
  mutate(Subpoblacion = Departamento) %>% 
  select(Subpoblacion, n, grados) %>% slice(1:23,25,26,24)

TablaD6N <- cbind(Tabla, D6depto) %>% 
  select(Subpoblacion, p, LI, LS, ee, CV, deff, n, n.eff, grados, y, CVl) %>%
  mutate(flag = ifelse(CV > cvflag | n < nflag | n.eff < nefflag | grados < dfflag | y < yflag | CVl > cvflag, 
                       "*", ""))

saveRDS(TablaD6N,"Tablas/D6MdeptoU.rds")

#### Mujeres sexualmente activas - Todos los metodos

D6depto = design.base %>% filter(MSA == "Si") %>%
  group_by(Departamento) %>% 
  cascade(n = unweighted(n()), 
          p = survey_mean(usametodo,
                          vartype = c("ci", "cv", "se"), 
                          proportion = TRUE, 
                          na.rm = TRUE), 
          defff = survey_mean(usametodo, 
                              na.rm = TRUE, 
                              deff = TRUE),
          y = unweighted(sum(usametodo)),
          .fill = "Total") %>% 
  mutate(LI = p_low, 
         LS = p_upp, 
         CV = 100 * p_cv, 
         deff = defff_deff, 
         n.eff = n/deff,
         CVl = ifelse(p <= 0.5, 
                      100 * p_se / (-p * log(p)), 
                      100 * p_se / (-(1 - p) * log(1 - p)))) %>% 
  select(p, LI, LS, ee = p_se, CV, deff, n.eff, y, CVl) %>% 
  as.data.frame()


Tabla = EndesMEF %>% filter(MSA == "Si") %>%
  mutate_at(vars(c(Departamento)), list(~as.character(.))) %>%
  bind_rows(mutate(., Departamento = "Total")) %>%
  group_by(Departamento) %>% 
  summarise(n = n(), 
            estratos = length(unique(V022)), 
            UPMs = length(unique(V021)), 
            grados = UPMs - estratos) %>% 
  mutate(Subpoblacion = Departamento) %>% 
  select(Subpoblacion, n, grados) %>% slice(1:23,25,26,24)

TablaD6N <- cbind(Tabla, D6depto) %>% 
  select(Subpoblacion, p, LI, LS, ee, CV, deff, n, n.eff, grados, y, CVl) %>%
  mutate(flag = ifelse(CV > cvflag | n < nflag | n.eff < nefflag | grados < dfflag | y < yflag | CVl > cvflag, 
                       "*", ""))


saveRDS(TablaD6N,"Tablas/D6deptoSA.rds")

#### Mujeres sexualmente activas - modernos

D6depto = design.base %>% filter(MSA == "Si") %>% 
  group_by(Departamento) %>% 
  cascade(n = unweighted(n()), 
          p = survey_mean(usamoderno,
                          vartype = c("ci", "cv", "se"), 
                          proportion = TRUE, 
                          na.rm = TRUE), 
          defff = survey_mean(usamoderno, 
                              na.rm = TRUE, 
                              deff = TRUE),
          y = unweighted(sum(usamoderno)),
          .fill = "Total") %>% 
  mutate(LI = p_low, 
         LS = p_upp, 
         CV = 100 * p_cv, 
         deff = defff_deff, 
         n.eff = n/deff,
         CVl = ifelse(p <= 0.5, 
                      100 * p_se / (-p * log(p)), 
                      100 * p_se / (-(1 - p) * log(1 - p)))) %>% 
  select(p, LI, LS, ee = p_se, CV, deff, n.eff, y, CVl) %>% 
  as.data.frame()


Tabla = EndesMEF %>% filter(MSA == "Si") %>%
  mutate_at(vars(c(Departamento)), list(~as.character(.))) %>%
  bind_rows(mutate(., Departamento = "Total")) %>%
  group_by(Departamento) %>% 
  summarise(n = n(), 
            estratos = length(unique(V022)), 
            UPMs = length(unique(V021)), 
            grados = UPMs - estratos) %>% 
  mutate(Subpoblacion = Departamento) %>% 
  select(Subpoblacion, n, grados) %>% slice(1:23,25,26,24)

TablaD6N <- cbind(Tabla, D6depto) %>% 
  select(Subpoblacion, p, LI, LS, ee, CV, deff, n, n.eff, grados, y, CVl) %>%
  mutate(flag = ifelse(CV > cvflag | n < nflag | n.eff < nefflag | grados < dfflag | y < yflag | CVl > cvflag, 
                       "*", ""))

saveRDS(TablaD6N,"Tablas/D6MdeptoSA.rds")

#####################
## Nivel Provincia ##
#####################

#### Todas las mujeres - Todos los metodos

D6provin = design.base %>%
  group_by(Provincia) %>% 
  cascade(n = unweighted(n()), 
          p = survey_mean(usametodo,
                          vartype = c("ci", "cv", "se"), 
                          proportion = TRUE, 
                          na.rm = TRUE), 
          defff = survey_mean(usametodo, 
                              na.rm = TRUE, 
                              deff = TRUE),
          y = unweighted(sum(usametodo)),
          .fill = "Total") %>% 
  mutate(LI = p_low, 
         LS = p_upp, 
         CV = 100 * p_cv, 
         deff = defff_deff, 
         n.eff = n/deff,
         CVl = ifelse(p <= 0.5, 
                      100 * p_se / (-p * log(p)), 
                      100 * p_se / (-(1 - p) * log(1 - p)))) %>% 
  select(p, LI, LS, ee = p_se, CV, deff, n.eff, y, CVl) %>% 
  as.data.frame()


Tabla = EndesMEF %>% 
  mutate_at(vars(c(Provincia)), list(~as.character(.))) %>%
  bind_rows(mutate(., Provincia = "Total")) %>%
  group_by(Provincia) %>% 
  summarise(n = n(), 
            estratos = length(unique(V022)), 
            UPMs = length(unique(V021)), 
            grados = UPMs - estratos) %>% 
  mutate(Subpoblacion = Provincia) %>% 
  select(Subpoblacion, n, grados) %>% slice(1:180,182:188,181)

TablaD6N <- cbind(Tabla, D6provin) %>% 
  select(Subpoblacion, p, LI, LS, ee, CV, deff, n, n.eff, grados, y, CVl) %>%
  mutate(flag = ifelse(CV > cvflag | n < nflag | n.eff < nefflag | grados < dfflag | y < yflag | CVl > cvflag, 
                       "*", ""))

saveRDS(TablaD6N,"Tablas/D6provin.rds")

#### Todas las mujeres - modernos

D6provin = design.base %>%
  group_by(Provincia) %>% 
  cascade(n = unweighted(n()), 
          p = survey_mean(usamoderno,
                          vartype = c("ci", "cv", "se"), 
                          proportion = TRUE, 
                          na.rm = TRUE), 
          defff = survey_mean(usamoderno, 
                              na.rm = TRUE, 
                              deff = TRUE),
          y = unweighted(sum(usamoderno)),
          .fill = "Total") %>% 
  mutate(LI = p_low, 
         LS = p_upp, 
         CV = 100 * p_cv, 
         deff = defff_deff, 
         n.eff = n/deff,
         CVl = ifelse(p <= 0.5, 
                      100 * p_se / (-p * log(p)), 
                      100 * p_se / (-(1 - p) * log(1 - p)))) %>% 
  select(p, LI, LS, ee = p_se, CV, deff, n.eff, y, CVl) %>% 
  as.data.frame()


Tabla = EndesMEF %>% 
  mutate_at(vars(c(Provincia)), list(~as.character(.))) %>%
  bind_rows(mutate(., Provincia = "Total")) %>%
  group_by(Provincia) %>% 
  summarise(n = n(), 
            estratos = length(unique(V022)), 
            UPMs = length(unique(V021)), 
            grados = UPMs - estratos) %>% 
  mutate(Subpoblacion = Provincia) %>% 
  select(Subpoblacion, n, grados) %>% slice(1:180,182:188,181)

TablaD6N <- cbind(Tabla, D6provin) %>% 
  select(Subpoblacion, p, LI, LS, ee, CV, deff, n, n.eff, grados, y, CVl) %>%
  mutate(flag = ifelse(CV > cvflag | n < nflag | n.eff < nefflag | grados < dfflag | y < yflag | CVl > cvflag, 
                       "*", ""))

saveRDS(TablaD6N,"Tablas/D6Mprovin.rds")


#### Mujeres sexualmente activas no unidas- Todos los metodos

D6provin = design.base %>% filter(MSA == "Si", union_1 != "Unida") %>%
  group_by(Provincia) %>% 
  cascade(n = unweighted(n()), 
          p = survey_mean(usametodo,
                          vartype = c("ci", "cv", "se"), 
                          proportion = TRUE, 
                          na.rm = TRUE), 
          defff = survey_mean(usametodo, 
                              na.rm = TRUE, 
                              deff = TRUE),
          y = unweighted(sum(usametodo)),
          .fill = "Total") %>% 
  mutate(LI = p_low, 
         LS = p_upp, 
         CV = 100 * p_cv, 
         deff = defff_deff, 
         n.eff = n/deff,
         CVl = ifelse(p <= 0.5, 
                      100 * p_se / (-p * log(p)), 
                      100 * p_se / (-(1 - p) * log(1 - p)))) %>% 
  select(p, LI, LS, ee = p_se, CV, deff, n.eff, y, CVl) %>% 
  as.data.frame()


Tabla = EndesMEF %>% filter(MSA == "Si", union_1 != "Unida") %>%
  mutate_at(vars(c(Provincia)), list(~as.character(.))) %>%
  bind_rows(mutate(., Provincia = "Total")) %>%
  group_by(Provincia) %>% 
  summarise(n = n(), 
            estratos = length(unique(V022)), 
            UPMs = length(unique(V021)), 
            grados = UPMs - estratos) %>% 
  mutate(Subpoblacion = Provincia) %>% 
  select(Subpoblacion, n, grados) %>% slice(1:140,142:147,141)

TablaD6N <- cbind(Tabla, D6provin) %>% 
  select(Subpoblacion, p, LI, LS, ee, CV, deff, n, n.eff, grados, y, CVl) %>%
  mutate(flag = ifelse(CV > cvflag | n < nflag | n.eff < nefflag | grados < dfflag | y < yflag | CVl > cvflag, 
                       "*", ""))


saveRDS(TablaD6N,"Tablas/D6provinSANU.rds")

#### Mujeres sexualmente activas no unidas - modernos

D6provin = design.base %>% filter(MSA == "Si", union_1!= "Unida") %>% 
  group_by(Provincia) %>% 
  cascade(n = unweighted(n()), 
          p = survey_mean(usamoderno,
                          vartype = c("ci", "cv", "se"), 
                          proportion = TRUE, 
                          na.rm = TRUE), 
          defff = survey_mean(usamoderno, 
                              na.rm = TRUE, 
                              deff = TRUE),
          y = unweighted(sum(usamoderno)),
          .fill = "Total") %>% 
  mutate(LI = p_low, 
         LS = p_upp, 
         CV = 100 * p_cv, 
         deff = defff_deff, 
         n.eff = n/deff,
         CVl = ifelse(p <= 0.5, 
                      100 * p_se / (-p * log(p)), 
                      100 * p_se / (-(1 - p) * log(1 - p)))) %>% 
  select(p, LI, LS, ee = p_se, CV, deff, n.eff, y, CVl) %>% 
  as.data.frame()


Tabla = EndesMEF %>% filter(MSA == "Si", union_1 != "Unida") %>%
  mutate_at(vars(c(Provincia)), list(~as.character(.))) %>%
  bind_rows(mutate(., Provincia = "Total")) %>%
  group_by(Provincia) %>% 
  summarise(n = n(), 
            estratos = length(unique(V022)), 
            UPMs = length(unique(V021)), 
            grados = UPMs - estratos) %>% 
  mutate(Subpoblacion = Provincia) %>% 
  select(Subpoblacion, n, grados) %>% slice(1:140,142:147,141)

TablaD6N <- cbind(Tabla, D6provin) %>% 
  select(Subpoblacion, p, LI, LS, ee, CV, deff, n, n.eff, grados, y, CVl) %>%
  mutate(flag = ifelse(CV > cvflag | n < nflag | n.eff < nefflag | grados < dfflag | y < yflag | CVl > cvflag, 
                       "*", ""))

saveRDS(TablaD6N,"Tablas/D6MprovinSANU.rds")

#### Mujeres Unidas - Todos los metodos

D6provin = design.base %>% filter(union_1 == "Unida") %>%
  group_by(Provincia) %>% 
  cascade(n = unweighted(n()), 
          p = survey_mean(usametodo,
                          vartype = c("ci", "cv", "se"), 
                          proportion = TRUE, 
                          na.rm = TRUE), 
          defff = survey_mean(usametodo, 
                              na.rm = TRUE, 
                              deff = TRUE),
          y = unweighted(sum(usametodo)),
          .fill = "Total") %>% 
  mutate(LI = p_low, 
         LS = p_upp, 
         CV = 100 * p_cv, 
         deff = defff_deff, 
         n.eff = n/deff,
         CVl = ifelse(p <= 0.5, 
                      100 * p_se / (-p * log(p)), 
                      100 * p_se / (-(1 - p) * log(1 - p)))) %>% 
  select(p, LI, LS, ee = p_se, CV, deff, n.eff, y, CVl) %>% 
  as.data.frame()


Tabla = EndesMEF %>% filter(union_1 == "Unida") %>%
  mutate_at(vars(c(Provincia)), list(~as.character(.))) %>%
  bind_rows(mutate(., Provincia = "Total")) %>%
  group_by(Provincia) %>% 
  summarise(n = n(), 
            estratos = length(unique(V022)), 
            UPMs = length(unique(V021)), 
            grados = UPMs - estratos) %>% 
  mutate(Subpoblacion = Provincia) %>% 
  select(Subpoblacion, n, grados) %>% slice(1:180,182:188,181)

TablaD6N <- cbind(Tabla, D6provin) %>% 
  select(Subpoblacion, p, LI, LS, ee, CV, deff, n, n.eff, grados, y, CVl) %>%
  mutate(flag = ifelse(CV > cvflag | n < nflag | n.eff < nefflag | grados < dfflag | y < yflag | CVl > cvflag, 
                       "*", ""))


saveRDS(TablaD6N,"Tablas/D6provinU.rds")

#### Mujeres Unidas - modernos

D6provin = design.base %>% filter(union_1 == "Unida") %>% 
  group_by(Provincia) %>% 
  cascade(n = unweighted(n()), 
          p = survey_mean(usamoderno,
                          vartype = c("ci", "cv", "se"), 
                          proportion = TRUE, 
                          na.rm = TRUE), 
          defff = survey_mean(usamoderno, 
                              na.rm = TRUE, 
                              deff = TRUE),
          y = unweighted(sum(usamoderno)),
          .fill = "Total") %>% 
  mutate(LI = p_low, 
         LS = p_upp, 
         CV = 100 * p_cv, 
         deff = defff_deff, 
         n.eff = n/deff,
         CVl = ifelse(p <= 0.5, 
                      100 * p_se / (-p * log(p)), 
                      100 * p_se / (-(1 - p) * log(1 - p)))) %>% 
  select(p, LI, LS, ee = p_se, CV, deff, n.eff, y, CVl) %>% 
  as.data.frame()


Tabla = EndesMEF %>% filter(union_1 == "Unida") %>%
  mutate_at(vars(c(Provincia)), list(~as.character(.))) %>%
  bind_rows(mutate(., Provincia = "Total")) %>%
  group_by(Provincia) %>% 
  summarise(n = n(), 
            estratos = length(unique(V022)), 
            UPMs = length(unique(V021)), 
            grados = UPMs - estratos) %>% 
  mutate(Subpoblacion = Provincia) %>% 
  select(Subpoblacion, n, grados) %>% slice(1:180,182:188,181)

TablaD6N <- cbind(Tabla, D6provin) %>% 
  select(Subpoblacion, p, LI, LS, ee, CV, deff, n, n.eff, grados, y, CVl) %>%
  mutate(flag = ifelse(CV > cvflag | n < nflag | n.eff < nefflag | grados < dfflag | y < yflag | CVl > cvflag, 
                       "*", ""))

saveRDS(TablaD6N,"Tablas/D6MprovinU.rds")

#### Mujeres sexualmente activas - Todos los metodos

D6provin = design.base %>% filter(MSA == "Si") %>%
  group_by(Provincia) %>% 
  cascade(n = unweighted(n()), 
          p = survey_mean(usametodo,
                          vartype = c("ci", "cv", "se"), 
                          proportion = TRUE, 
                          na.rm = TRUE), 
          defff = survey_mean(usametodo, 
                              na.rm = TRUE, 
                              deff = TRUE),
          y = unweighted(sum(usametodo)),
          .fill = "Total") %>% 
  mutate(LI = p_low, 
         LS = p_upp, 
         CV = 100 * p_cv, 
         deff = defff_deff, 
         n.eff = n/deff,
         CVl = ifelse(p <= 0.5, 
                      100 * p_se / (-p * log(p)), 
                      100 * p_se / (-(1 - p) * log(1 - p)))) %>% 
  select(p, LI, LS, ee = p_se, CV, deff, n.eff, y, CVl) %>% 
  as.data.frame()


Tabla = EndesMEF %>% filter(MSA == "Si") %>%
  mutate_at(vars(c(Provincia)), list(~as.character(.))) %>%
  bind_rows(mutate(., Provincia = "Total")) %>%
  group_by(Provincia) %>% 
  summarise(n = n(), 
            estratos = length(unique(V022)), 
            UPMs = length(unique(V021)), 
            grados = UPMs - estratos) %>% 
  mutate(Subpoblacion = Provincia) %>% 
  select(Subpoblacion, n, grados) %>% slice(1:180,182:188,181)

TablaD6N <- cbind(Tabla, D6provin) %>% 
  select(Subpoblacion, p, LI, LS, ee, CV, deff, n, n.eff, grados, y, CVl) %>%
  mutate(flag = ifelse(CV > cvflag | n < nflag | n.eff < nefflag | grados < dfflag | y < yflag | CVl > cvflag, 
                       "*", ""))


saveRDS(TablaD6N,"Tablas/D6provinSA.rds")

#### Mujeres sexualmente activas - modernos

D6provin = design.base %>% filter(MSA == "Si") %>% 
  group_by(Provincia) %>% 
  cascade(n = unweighted(n()), 
          p = survey_mean(usamoderno,
                          vartype = c("ci", "cv", "se"), 
                          proportion = TRUE, 
                          na.rm = TRUE), 
          defff = survey_mean(usamoderno, 
                              na.rm = TRUE, 
                              deff = TRUE),
          y = unweighted(sum(usamoderno)),
          .fill = "Total") %>% 
  mutate(LI = p_low, 
         LS = p_upp, 
         CV = 100 * p_cv, 
         deff = defff_deff, 
         n.eff = n/deff,
         CVl = ifelse(p <= 0.5, 
                      100 * p_se / (-p * log(p)), 
                      100 * p_se / (-(1 - p) * log(1 - p)))) %>% 
  select(p, LI, LS, ee = p_se, CV, deff, n.eff, y, CVl) %>% 
  as.data.frame()


Tabla = EndesMEF %>% filter(MSA == "Si") %>%
  mutate_at(vars(c(Provincia)), list(~as.character(.))) %>%
  bind_rows(mutate(., Provincia = "Total")) %>%
  group_by(Provincia) %>% 
  summarise(n = n(), 
            estratos = length(unique(V022)), 
            UPMs = length(unique(V021)), 
            grados = UPMs - estratos) %>% 
  mutate(Subpoblacion = Provincia) %>% 
  select(Subpoblacion, n, grados) %>% slice(1:180,182:188,181)

TablaD6N <- cbind(Tabla, D6provin) %>% 
  select(Subpoblacion, p, LI, LS, ee, CV, deff, n, n.eff, grados, y, CVl) %>%
  mutate(flag = ifelse(CV > cvflag | n < nflag | n.eff < nefflag | grados < dfflag | y < yflag | CVl > cvflag, 
                       "*", ""))

saveRDS(TablaD6N,"Tablas/D6MprovinSA.rds")

#################################
## Nivel departamento - Urbana ##
#################################

#### Todas las mujeres - Todos los metodos

D6depto = design.base %>% filter(Area == "Urbana") %>%
  group_by(Departamento) %>% 
  cascade(n = unweighted(n()), 
          p = survey_mean(usametodo,
                          vartype = c("ci", "cv", "se"), 
                          proportion = TRUE, 
                          na.rm = TRUE), 
          defff = survey_mean(usametodo, 
                              na.rm = TRUE, 
                              deff = TRUE),
          y = unweighted(sum(usametodo)),
          .fill = "Total") %>% 
  mutate(LI = p_low, 
         LS = p_upp, 
         CV = 100 * p_cv, 
         deff = defff_deff, 
         n.eff = n/deff,
         CVl = ifelse(p <= 0.5, 
                      100 * p_se / (-p * log(p)), 
                      100 * p_se / (-(1 - p) * log(1 - p)))) %>% 
  select(p, LI, LS, ee = p_se, CV, deff, n.eff, y, CVl) %>% 
  as.data.frame()


Tabla = EndesMEF %>% filter(Area == "Urbana") %>%
  mutate_at(vars(c(Departamento)), list(~as.character(.))) %>%
  bind_rows(mutate(., Departamento = "Total")) %>%
  group_by(Departamento) %>% 
  summarise(n = n(), 
            estratos = length(unique(V022)), 
            UPMs = length(unique(V021)), 
            grados = UPMs - estratos) %>% 
  mutate(Subpoblacion = Departamento) %>% 
  select(Subpoblacion, n, grados) %>% slice(1:23,25,26,24)

TablaD6N <- cbind(Tabla, D6depto) %>% 
  select(Subpoblacion, p, LI, LS, ee, CV, deff, n, n.eff, grados, y, CVl) %>%
  mutate(flag = ifelse(CV > cvflag | n < nflag | n.eff < nefflag | grados < dfflag | y < yflag | CVl > cvflag, 
                       "*", ""))

saveRDS(TablaD6N,"Tablas/D6deptoUrbana.rds")

#### Todas las mujeres - modernos

D6depto = design.base %>% filter(Area == "Urbana") %>%
  group_by(Departamento) %>% 
  cascade(n = unweighted(n()), 
          p = survey_mean(usamoderno,
                          vartype = c("ci", "cv", "se"), 
                          proportion = TRUE, 
                          na.rm = TRUE), 
          defff = survey_mean(usamoderno, 
                              na.rm = TRUE, 
                              deff = TRUE),
          y = unweighted(sum(usamoderno)),
          .fill = "Total") %>% 
  mutate(LI = p_low, 
         LS = p_upp, 
         CV = 100 * p_cv, 
         deff = defff_deff, 
         n.eff = n/deff,
         CVl = ifelse(p <= 0.5, 
                      100 * p_se / (-p * log(p)), 
                      100 * p_se / (-(1 - p) * log(1 - p)))) %>% 
  select(p, LI, LS, ee = p_se, CV, deff, n.eff, y, CVl) %>% 
  as.data.frame()


Tabla = EndesMEF %>% filter(Area == "Urbana") %>%
  mutate_at(vars(c(Departamento)), list(~as.character(.))) %>%
  bind_rows(mutate(., Departamento = "Total")) %>%
  group_by(Departamento) %>% 
  summarise(n = n(), 
            estratos = length(unique(V022)), 
            UPMs = length(unique(V021)), 
            grados = UPMs - estratos) %>% 
  mutate(Subpoblacion = Departamento) %>% 
  select(Subpoblacion, n, grados) %>% slice(1:23,25,26,24)

TablaD6N <- cbind(Tabla, D6depto) %>% 
  select(Subpoblacion, p, LI, LS, ee, CV, deff, n, n.eff, grados, y, CVl) %>%
  mutate(flag = ifelse(CV > cvflag | n < nflag | n.eff < nefflag | grados < dfflag | y < yflag | CVl > cvflag, 
                       "*", ""))

saveRDS(TablaD6N,"Tablas/D6MdeptoUrbana.rds")


#### Mujeres sexualmente activas no unidas - Todos los metodos

D6depto = design.base %>% filter(Area == "Urbana") %>%
  filter(MSA == "Si", union_1 != "Unida") %>%
  group_by(Departamento) %>% 
  cascade(n = unweighted(n()), 
          p = survey_mean(usametodo,
                          vartype = c("ci", "cv", "se"), 
                          proportion = TRUE, 
                          na.rm = TRUE), 
          defff = survey_mean(usametodo, 
                              na.rm = TRUE, 
                              deff = TRUE),
          y = unweighted(sum(usametodo)),
          .fill = "Total") %>% 
  mutate(LI = p_low, 
         LS = p_upp, 
         CV = 100 * p_cv, 
         deff = defff_deff, 
         n.eff = n/deff,
         CVl = ifelse(p <= 0.5, 
                      100 * p_se / (-p * log(p)), 
                      100 * p_se / (-(1 - p) * log(1 - p)))) %>% 
  select(p, LI, LS, ee = p_se, CV, deff, n.eff, y, CVl) %>% 
  as.data.frame()


Tabla = EndesMEF %>% filter(Area == "Urbana") %>%
  filter(MSA == "Si", union_1 != "Unida") %>%
  mutate_at(vars(c(Departamento)), list(~as.character(.))) %>%
  bind_rows(mutate(., Departamento = "Total")) %>%
  group_by(Departamento) %>% 
  summarise(n = n(), 
            estratos = length(unique(V022)), 
            UPMs = length(unique(V021)), 
            grados = UPMs - estratos) %>% 
  mutate(Subpoblacion = Departamento) %>% 
  select(Subpoblacion, n, grados) %>% slice(1:23,25,26,24)

TablaD6N <- cbind(Tabla, D6depto) %>% 
  select(Subpoblacion, p, LI, LS, ee, CV, deff, n, n.eff, grados, y, CVl) %>%
  mutate(flag = ifelse(CV > cvflag | n < nflag | n.eff < nefflag | grados < dfflag | y < yflag | CVl > cvflag, 
                       "*", ""))


saveRDS(TablaD6N,"Tablas/D6deptoUrbanaSANU.rds")

#### Mujeres sexualmente activas no unidas - modernos

D6depto = design.base %>% filter(Area == "Urbana") %>%
  filter(MSA == "Si", union_1!= "Unida") %>% 
  group_by(Departamento) %>% 
  cascade(n = unweighted(n()), 
          p = survey_mean(usamoderno,
                          vartype = c("ci", "cv", "se"), 
                          proportion = TRUE, 
                          na.rm = TRUE), 
          defff = survey_mean(usamoderno, 
                              na.rm = TRUE, 
                              deff = TRUE),
          y = unweighted(sum(usamoderno)),
          .fill = "Total") %>% 
  mutate(LI = p_low, 
         LS = p_upp, 
         CV = 100 * p_cv, 
         deff = defff_deff, 
         n.eff = n/deff,
         CVl = ifelse(p <= 0.5, 
                      100 * p_se / (-p * log(p)), 
                      100 * p_se / (-(1 - p) * log(1 - p)))) %>% 
  select(p, LI, LS, ee = p_se, CV, deff, n.eff, y, CVl) %>% 
  as.data.frame()


Tabla = EndesMEF %>% filter(Area == "Urbana") %>% 
  filter(MSA == "Si", union_1 != "Unida") %>%
  mutate_at(vars(c(Departamento)), list(~as.character(.))) %>%
  bind_rows(mutate(., Departamento = "Total")) %>%
  group_by(Departamento) %>% 
  summarise(n = n(), 
            estratos = length(unique(V022)), 
            UPMs = length(unique(V021)), 
            grados = UPMs - estratos) %>% 
  mutate(Subpoblacion = Departamento) %>% 
  select(Subpoblacion, n, grados) %>% slice(1:23,25,26,24)

TablaD6N <- cbind(Tabla, D6depto) %>% 
  select(Subpoblacion, p, LI, LS, ee, CV, deff, n, n.eff, grados, y, CVl) %>%
  mutate(flag = ifelse(CV > cvflag | n < nflag | n.eff < nefflag | grados < dfflag | y < yflag | CVl > cvflag, 
                       "*", ""))

saveRDS(TablaD6N,"Tablas/D6MdeptoUrbanaSANU.rds")

#### Mujeres Unidas - Todos los metodos

D6depto = design.base %>% filter(Area == "Urbana") %>%
  filter(union_1 == "Unida") %>%
  group_by(Departamento) %>% 
  cascade(n = unweighted(n()), 
          p = survey_mean(usametodo,
                          vartype = c("ci", "cv", "se"), 
                          proportion = TRUE, 
                          na.rm = TRUE), 
          defff = survey_mean(usametodo, 
                              na.rm = TRUE, 
                              deff = TRUE),
          y = unweighted(sum(usametodo)),
          .fill = "Total") %>% 
  mutate(LI = p_low, 
         LS = p_upp, 
         CV = 100 * p_cv, 
         deff = defff_deff, 
         n.eff = n/deff,
         CVl = ifelse(p <= 0.5, 
                      100 * p_se / (-p * log(p)), 
                      100 * p_se / (-(1 - p) * log(1 - p)))) %>% 
  select(p, LI, LS, ee = p_se, CV, deff, n.eff, y, CVl) %>% 
  as.data.frame()


Tabla = EndesMEF %>% filter(Area == "Urbana") %>% 
  filter(union_1 == "Unida") %>%
  mutate_at(vars(c(Departamento)), list(~as.character(.))) %>%
  bind_rows(mutate(., Departamento = "Total")) %>%
  group_by(Departamento) %>% 
  summarise(n = n(), 
            estratos = length(unique(V022)), 
            UPMs = length(unique(V021)), 
            grados = UPMs - estratos) %>% 
  mutate(Subpoblacion = Departamento) %>% 
  select(Subpoblacion, n, grados) %>% slice(1:23,25,26,24)

TablaD6N <- cbind(Tabla, D6depto) %>% 
  select(Subpoblacion, p, LI, LS, ee, CV, deff, n, n.eff, grados, y, CVl) %>%
  mutate(flag = ifelse(CV > cvflag | n < nflag | n.eff < nefflag | grados < dfflag | y < yflag | CVl > cvflag, 
                       "*", ""))


saveRDS(TablaD6N,"Tablas/D6deptoUrbanaU.rds")

#### Mujeres Unidas - modernos

D6depto = design.base %>% filter(Area == "Urbana") %>%
  filter(union_1 == "Unida") %>% 
  group_by(Departamento) %>% 
  cascade(n = unweighted(n()), 
          p = survey_mean(usamoderno,
                          vartype = c("ci", "cv", "se"), 
                          proportion = TRUE, 
                          na.rm = TRUE), 
          defff = survey_mean(usamoderno, 
                              na.rm = TRUE, 
                              deff = TRUE),
          y = unweighted(sum(usamoderno)),
          .fill = "Total") %>% 
  mutate(LI = p_low, 
         LS = p_upp, 
         CV = 100 * p_cv, 
         deff = defff_deff, 
         n.eff = n/deff,
         CVl = ifelse(p <= 0.5, 
                      100 * p_se / (-p * log(p)), 
                      100 * p_se / (-(1 - p) * log(1 - p)))) %>% 
  select(p, LI, LS, ee = p_se, CV, deff, n.eff, y, CVl) %>% 
  as.data.frame()


Tabla = EndesMEF %>% filter(Area == "Urbana") %>%
  filter(union_1 == "Unida") %>%
  mutate_at(vars(c(Departamento)), list(~as.character(.))) %>%
  bind_rows(mutate(., Departamento = "Total")) %>%
  group_by(Departamento) %>% 
  summarise(n = n(), 
            estratos = length(unique(V022)), 
            UPMs = length(unique(V021)), 
            grados = UPMs - estratos) %>% 
  mutate(Subpoblacion = Departamento) %>% 
  select(Subpoblacion, n, grados) %>% slice(1:23,25,26,24)

TablaD6N <- cbind(Tabla, D6depto) %>% 
  select(Subpoblacion, p, LI, LS, ee, CV, deff, n, n.eff, grados, y, CVl) %>%
  mutate(flag = ifelse(CV > cvflag | n < nflag | n.eff < nefflag | grados < dfflag | y < yflag | CVl > cvflag, 
                       "*", ""))

saveRDS(TablaD6N,"Tablas/D6MdeptoUrbanaU.rds")

#### Mujeres sexualmente activas - Todos los metodos

D6depto = design.base %>% filter(Area == "Urbana") %>%
  filter(MSA == "Si") %>%
  group_by(Departamento) %>% 
  cascade(n = unweighted(n()), 
          p = survey_mean(usametodo,
                          vartype = c("ci", "cv", "se"), 
                          proportion = TRUE, 
                          na.rm = TRUE), 
          defff = survey_mean(usametodo, 
                              na.rm = TRUE, 
                              deff = TRUE),
          y = unweighted(sum(usametodo)),
          .fill = "Total") %>% 
  mutate(LI = p_low, 
         LS = p_upp, 
         CV = 100 * p_cv, 
         deff = defff_deff, 
         n.eff = n/deff,
         CVl = ifelse(p <= 0.5, 
                      100 * p_se / (-p * log(p)), 
                      100 * p_se / (-(1 - p) * log(1 - p)))) %>% 
  select(p, LI, LS, ee = p_se, CV, deff, n.eff, y, CVl) %>% 
  as.data.frame()


Tabla = EndesMEF %>% filter(Area == "Urbana") %>%
  filter(MSA == "Si") %>%
  mutate_at(vars(c(Departamento)), list(~as.character(.))) %>%
  bind_rows(mutate(., Departamento = "Total")) %>%
  group_by(Departamento) %>% 
  summarise(n = n(), 
            estratos = length(unique(V022)), 
            UPMs = length(unique(V021)), 
            grados = UPMs - estratos) %>% 
  mutate(Subpoblacion = Departamento) %>% 
  select(Subpoblacion, n, grados) %>% slice(1:23,25,26,24)

TablaD6N <- cbind(Tabla, D6depto) %>% 
  select(Subpoblacion, p, LI, LS, ee, CV, deff, n, n.eff, grados, y, CVl) %>%
  mutate(flag = ifelse(CV > cvflag | n < nflag | n.eff < nefflag | grados < dfflag | y < yflag | CVl > cvflag, 
                       "*", ""))


saveRDS(TablaD6N,"Tablas/D6deptoUrbanaSA.rds")

#### Mujeres sexualmente activas - modernos

D6depto = design.base %>% filter(Area == "Urbana") %>%
  filter(MSA == "Si") %>% 
  group_by(Departamento) %>% 
  cascade(n = unweighted(n()), 
          p = survey_mean(usamoderno,
                          vartype = c("ci", "cv", "se"), 
                          proportion = TRUE, 
                          na.rm = TRUE), 
          defff = survey_mean(usamoderno, 
                              na.rm = TRUE, 
                              deff = TRUE),
          y = unweighted(sum(usamoderno)),
          .fill = "Total") %>% 
  mutate(LI = p_low, 
         LS = p_upp, 
         CV = 100 * p_cv, 
         deff = defff_deff, 
         n.eff = n/deff,
         CVl = ifelse(p <= 0.5, 
                      100 * p_se / (-p * log(p)), 
                      100 * p_se / (-(1 - p) * log(1 - p)))) %>% 
  select(p, LI, LS, ee = p_se, CV, deff, n.eff, y, CVl) %>% 
  as.data.frame()


Tabla = EndesMEF %>% filter(Area == "Urbana") %>%
  filter(MSA == "Si") %>%
  mutate_at(vars(c(Departamento)), list(~as.character(.))) %>%
  bind_rows(mutate(., Departamento = "Total")) %>%
  group_by(Departamento) %>% 
  summarise(n = n(), 
            estratos = length(unique(V022)), 
            UPMs = length(unique(V021)), 
            grados = UPMs - estratos) %>% 
  mutate(Subpoblacion = Departamento) %>% 
  select(Subpoblacion, n, grados) %>% slice(1:23,25,26,24)

TablaD6N <- cbind(Tabla, D6depto) %>% 
  select(Subpoblacion, p, LI, LS, ee, CV, deff, n, n.eff, grados, y, CVl) %>%
  mutate(flag = ifelse(CV > cvflag | n < nflag | n.eff < nefflag | grados < dfflag | y < yflag | CVl > cvflag, 
                       "*", ""))

saveRDS(TablaD6N,"Tablas/D6MUrbanaSA.rds")

#################################
## Nivel departamento - Rural ##
#################################

#### Todas las mujeres - Todos los metodos

D6depto = design.base %>% filter(Area == "Rural") %>%
  group_by(Departamento) %>% 
  cascade(n = unweighted(n()), 
          p = survey_mean(usametodo,
                          vartype = c("ci", "cv", "se"), 
                          proportion = TRUE, 
                          na.rm = TRUE), 
          defff = survey_mean(usametodo, 
                              na.rm = TRUE, 
                              deff = TRUE),
          y = unweighted(sum(usametodo)),
          .fill = "Total") %>% 
  mutate(LI = p_low, 
         LS = p_upp, 
         CV = 100 * p_cv, 
         deff = defff_deff, 
         n.eff = n/deff,
         CVl = ifelse(p <= 0.5, 
                      100 * p_se / (-p * log(p)), 
                      100 * p_se / (-(1 - p) * log(1 - p)))) %>% 
  select(p, LI, LS, ee = p_se, CV, deff, n.eff, y, CVl) %>% 
  as.data.frame()


Tabla = EndesMEF %>% filter(Area == "Rural") %>%
  mutate_at(vars(c(Departamento)), list(~as.character(.))) %>%
  bind_rows(mutate(., Departamento = "Total")) %>%
  group_by(Departamento) %>% 
  summarise(n = n(), 
            estratos = length(unique(V022)), 
            UPMs = length(unique(V021)), 
            grados = UPMs - estratos) %>% 
  mutate(Subpoblacion = Departamento) %>% 
  select(Subpoblacion, n, grados) %>% slice(1:22,24,25,23)

TablaD6N <- cbind(Tabla, D6depto) %>% 
  select(Subpoblacion, p, LI, LS, ee, CV, deff, n, n.eff, grados, y, CVl) %>%
  mutate(flag = ifelse(CV > cvflag | n < nflag | n.eff < nefflag | grados < dfflag | y < yflag | CVl > cvflag, 
                       "*", ""))

saveRDS(TablaD6N,"Tablas/D6deptoRural.rds")

#### Todas las mujeres - modernos

D6depto = design.base %>% filter(Area == "Rural") %>%
  group_by(Departamento) %>% 
  cascade(n = unweighted(n()), 
          p = survey_mean(usamoderno,
                          vartype = c("ci", "cv", "se"), 
                          proportion = TRUE, 
                          na.rm = TRUE), 
          defff = survey_mean(usamoderno, 
                              na.rm = TRUE, 
                              deff = TRUE),
          y = unweighted(sum(usamoderno)),
          .fill = "Total") %>% 
  mutate(LI = p_low, 
         LS = p_upp, 
         CV = 100 * p_cv, 
         deff = defff_deff, 
         n.eff = n/deff,
         CVl = ifelse(p <= 0.5, 
                      100 * p_se / (-p * log(p)), 
                      100 * p_se / (-(1 - p) * log(1 - p)))) %>% 
  select(p, LI, LS, ee = p_se, CV, deff, n.eff, y, CVl) %>% 
  as.data.frame()


Tabla = EndesMEF %>% filter(Area == "Rural") %>%
  mutate_at(vars(c(Departamento)), list(~as.character(.))) %>%
  bind_rows(mutate(., Departamento = "Total")) %>%
  group_by(Departamento) %>% 
  summarise(n = n(), 
            estratos = length(unique(V022)), 
            UPMs = length(unique(V021)), 
            grados = UPMs - estratos) %>% 
  mutate(Subpoblacion = Departamento) %>% 
  select(Subpoblacion, n, grados) %>% slice(1:22,24,25,23)

TablaD6N <- cbind(Tabla, D6depto) %>% 
  select(Subpoblacion, p, LI, LS, ee, CV, deff, n, n.eff, grados, y, CVl) %>%
  mutate(flag = ifelse(CV > cvflag | n < nflag | n.eff < nefflag | grados < dfflag | y < yflag | CVl > cvflag, 
                       "*", ""))

saveRDS(TablaD6N,"Tablas/D6MdeptoRural.rds")


#### Mujeres sexualmente activas no unidas - Todos los metodos

D6depto = design.base %>% filter(Area == "Rural") %>%
  filter(MSA == "Si", union_1 != "Unida") %>%
  group_by(Departamento) %>% 
  cascade(n = unweighted(n()), 
          p = survey_mean(usametodo,
                          vartype = c("ci", "cv", "se"), 
                          proportion = TRUE, 
                          na.rm = TRUE), 
          defff = survey_mean(usametodo, 
                              na.rm = TRUE, 
                              deff = TRUE),
          y = unweighted(sum(usametodo)),
          .fill = "Total") %>% 
  mutate(LI = p_low, 
         LS = p_upp, 
         CV = 100 * p_cv, 
         deff = defff_deff, 
         n.eff = n/deff,
         CVl = ifelse(p <= 0.5, 
                      100 * p_se / (-p * log(p)), 
                      100 * p_se / (-(1 - p) * log(1 - p)))) %>% 
  select(p, LI, LS, ee = p_se, CV, deff, n.eff, y, CVl) %>% 
  as.data.frame()


Tabla = EndesMEF %>% filter(Area == "Rural") %>%
  filter(MSA == "Si", union_1 != "Unida") %>%
  mutate_at(vars(c(Departamento)), list(~as.character(.))) %>%
  bind_rows(mutate(., Departamento = "Total")) %>%
  group_by(Departamento) %>% 
  summarise(n = n(), 
            estratos = length(unique(V022)), 
            UPMs = length(unique(V021)), 
            grados = UPMs - estratos) %>% 
  mutate(Subpoblacion = Departamento) %>% 
  select(Subpoblacion, n, grados) %>% slice(1:22,24,25,23)

TablaD6N <- cbind(Tabla, D6depto) %>% 
  select(Subpoblacion, p, LI, LS, ee, CV, deff, n, n.eff, grados, y, CVl) %>%
  mutate(flag = ifelse(CV > cvflag | n < nflag | n.eff < nefflag | grados < dfflag | y < yflag | CVl > cvflag, 
                       "*", ""))


saveRDS(TablaD6N,"Tablas/D6deptoRuralSANU.rds")

#### Mujeres sexualmente activas no unidas - modernos

D6depto = design.base %>% filter(Area == "Rural") %>%
  filter(MSA == "Si", union_1!= "Unida") %>% 
  group_by(Departamento) %>% 
  cascade(n = unweighted(n()), 
          p = survey_mean(usamoderno,
                          vartype = c("ci", "cv", "se"), 
                          proportion = TRUE, 
                          na.rm = TRUE), 
          defff = survey_mean(usamoderno, 
                              na.rm = TRUE, 
                              deff = TRUE),
          y = unweighted(sum(usamoderno)),
          .fill = "Total") %>% 
  mutate(LI = p_low, 
         LS = p_upp, 
         CV = 100 * p_cv, 
         deff = defff_deff, 
         n.eff = n/deff,
         CVl = ifelse(p <= 0.5, 
                      100 * p_se / (-p * log(p)), 
                      100 * p_se / (-(1 - p) * log(1 - p)))) %>% 
  select(p, LI, LS, ee = p_se, CV, deff, n.eff, y, CVl) %>% 
  as.data.frame()


Tabla = EndesMEF %>% filter(Area == "Rural") %>% 
  filter(MSA == "Si", union_1 != "Unida") %>%
  mutate_at(vars(c(Departamento)), list(~as.character(.))) %>%
  bind_rows(mutate(., Departamento = "Total")) %>%
  group_by(Departamento) %>% 
  summarise(n = n(), 
            estratos = length(unique(V022)), 
            UPMs = length(unique(V021)), 
            grados = UPMs - estratos) %>% 
  mutate(Subpoblacion = Departamento) %>% 
  select(Subpoblacion, n, grados) %>% slice(1:22,24,25,23)

TablaD6N <- cbind(Tabla, D6depto) %>% 
  select(Subpoblacion, p, LI, LS, ee, CV, deff, n, n.eff, grados, y, CVl) %>%
  mutate(flag = ifelse(CV > cvflag | n < nflag | n.eff < nefflag | grados < dfflag | y < yflag | CVl > cvflag, 
                       "*", ""))

saveRDS(TablaD6N,"Tablas/D6MdeptoRuralSANU.rds")

#### Mujeres Unidas - Todos los metodos

D6depto = design.base %>% filter(Area == "Rural") %>%
  filter(union_1 == "Unida") %>%
  group_by(Departamento) %>% 
  cascade(n = unweighted(n()), 
          p = survey_mean(usametodo,
                          vartype = c("ci", "cv", "se"), 
                          proportion = TRUE, 
                          na.rm = TRUE), 
          defff = survey_mean(usametodo, 
                              na.rm = TRUE, 
                              deff = TRUE),
          y = unweighted(sum(usametodo)),
          .fill = "Total") %>% 
  mutate(LI = p_low, 
         LS = p_upp, 
         CV = 100 * p_cv, 
         deff = defff_deff, 
         n.eff = n/deff,
         CVl = ifelse(p <= 0.5, 
                      100 * p_se / (-p * log(p)), 
                      100 * p_se / (-(1 - p) * log(1 - p)))) %>% 
  select(p, LI, LS, ee = p_se, CV, deff, n.eff, y, CVl) %>% 
  as.data.frame()


Tabla = EndesMEF %>% filter(Area == "Rural") %>% 
  filter(union_1 == "Unida") %>%
  mutate_at(vars(c(Departamento)), list(~as.character(.))) %>%
  bind_rows(mutate(., Departamento = "Total")) %>%
  group_by(Departamento) %>% 
  summarise(n = n(), 
            estratos = length(unique(V022)), 
            UPMs = length(unique(V021)), 
            grados = UPMs - estratos) %>% 
  mutate(Subpoblacion = Departamento) %>% 
  select(Subpoblacion, n, grados) %>% slice(1:22,24,25,23)

TablaD6N <- cbind(Tabla, D6depto) %>% 
  select(Subpoblacion, p, LI, LS, ee, CV, deff, n, n.eff, grados, y, CVl) %>%
  mutate(flag = ifelse(CV > cvflag | n < nflag | n.eff < nefflag | grados < dfflag | y < yflag | CVl > cvflag, 
                       "*", ""))


saveRDS(TablaD6N,"Tablas/D6deptoRuralU.rds")

#### Mujeres Unidas - modernos

D6depto = design.base %>% filter(Area == "Rural") %>%
  filter(union_1 == "Unida") %>% 
  group_by(Departamento) %>% 
  cascade(n = unweighted(n()), 
          p = survey_mean(usamoderno,
                          vartype = c("ci", "cv", "se"), 
                          proportion = TRUE, 
                          na.rm = TRUE), 
          defff = survey_mean(usamoderno, 
                              na.rm = TRUE, 
                              deff = TRUE),
          y = unweighted(sum(usamoderno)),
          .fill = "Total") %>% 
  mutate(LI = p_low, 
         LS = p_upp, 
         CV = 100 * p_cv, 
         deff = defff_deff, 
         n.eff = n/deff,
         CVl = ifelse(p <= 0.5, 
                      100 * p_se / (-p * log(p)), 
                      100 * p_se / (-(1 - p) * log(1 - p)))) %>% 
  select(p, LI, LS, ee = p_se, CV, deff, n.eff, y, CVl) %>% 
  as.data.frame()


Tabla = EndesMEF %>% filter(Area == "Rural") %>%
  filter(union_1 == "Unida") %>%
  mutate_at(vars(c(Departamento)), list(~as.character(.))) %>%
  bind_rows(mutate(., Departamento = "Total")) %>%
  group_by(Departamento) %>% 
  summarise(n = n(), 
            estratos = length(unique(V022)), 
            UPMs = length(unique(V021)), 
            grados = UPMs - estratos) %>% 
  mutate(Subpoblacion = Departamento) %>% 
  select(Subpoblacion, n, grados) %>% slice(1:22,24,25,23)

TablaD6N <- cbind(Tabla, D6depto) %>% 
  select(Subpoblacion, p, LI, LS, ee, CV, deff, n, n.eff, grados, y, CVl) %>%
  mutate(flag = ifelse(CV > cvflag | n < nflag | n.eff < nefflag | grados < dfflag | y < yflag | CVl > cvflag, 
                       "*", ""))

saveRDS(TablaD6N,"Tablas/D6MdeptoRuralU.rds")

#### Mujeres sexualmente activas - Todos los metodos

D6depto = design.base %>% filter(Area == "Rural") %>%
  filter(MSA == "Si") %>%
  group_by(Departamento) %>% 
  cascade(n = unweighted(n()), 
          p = survey_mean(usametodo,
                          vartype = c("ci", "cv", "se"), 
                          proportion = TRUE, 
                          na.rm = TRUE), 
          defff = survey_mean(usametodo, 
                              na.rm = TRUE, 
                              deff = TRUE),
          y = unweighted(sum(usametodo)),
          .fill = "Total") %>% 
  mutate(LI = p_low, 
         LS = p_upp, 
         CV = 100 * p_cv, 
         deff = defff_deff, 
         n.eff = n/deff,
         CVl = ifelse(p <= 0.5, 
                      100 * p_se / (-p * log(p)), 
                      100 * p_se / (-(1 - p) * log(1 - p)))) %>% 
  select(p, LI, LS, ee = p_se, CV, deff, n.eff, y, CVl) %>% 
  as.data.frame()


Tabla = EndesMEF %>% filter(Area == "Rural") %>%
  filter(MSA == "Si") %>%
  mutate_at(vars(c(Departamento)), list(~as.character(.))) %>%
  bind_rows(mutate(., Departamento = "Total")) %>%
  group_by(Departamento) %>% 
  summarise(n = n(), 
            estratos = length(unique(V022)), 
            UPMs = length(unique(V021)), 
            grados = UPMs - estratos) %>% 
  mutate(Subpoblacion = Departamento) %>% 
  select(Subpoblacion, n, grados) %>% slice(1:22,24,25,23)

TablaD6N <- cbind(Tabla, D6depto) %>% 
  select(Subpoblacion, p, LI, LS, ee, CV, deff, n, n.eff, grados, y, CVl) %>%
  mutate(flag = ifelse(CV > cvflag | n < nflag | n.eff < nefflag | grados < dfflag | y < yflag | CVl > cvflag, 
                       "*", ""))


saveRDS(TablaD6N,"Tablas/D6deptoRuralSA.rds")

#### Mujeres sexualmente activas - modernos

D6depto = design.base %>% filter(Area == "Rural") %>%
  filter(MSA == "Si") %>% 
  group_by(Departamento) %>% 
  cascade(n = unweighted(n()), 
          p = survey_mean(usamoderno,
                          vartype = c("ci", "cv", "se"), 
                          proportion = TRUE, 
                          na.rm = TRUE), 
          defff = survey_mean(usamoderno, 
                              na.rm = TRUE, 
                              deff = TRUE),
          y = unweighted(sum(usamoderno)),
          .fill = "Total") %>% 
  mutate(LI = p_low, 
         LS = p_upp, 
         CV = 100 * p_cv, 
         deff = defff_deff, 
         n.eff = n/deff,
         CVl = ifelse(p <= 0.5, 
                      100 * p_se / (-p * log(p)), 
                      100 * p_se / (-(1 - p) * log(1 - p)))) %>% 
  select(p, LI, LS, ee = p_se, CV, deff, n.eff, y, CVl) %>% 
  as.data.frame()


Tabla = EndesMEF %>% filter(Area == "Rural") %>%
  filter(MSA == "Si") %>%
  mutate_at(vars(c(Departamento)), list(~as.character(.))) %>%
  bind_rows(mutate(., Departamento = "Total")) %>%
  group_by(Departamento) %>% 
  summarise(n = n(), 
            estratos = length(unique(V022)), 
            UPMs = length(unique(V021)), 
            grados = UPMs - estratos) %>% 
  mutate(Subpoblacion = Departamento) %>% 
  select(Subpoblacion, n, grados) %>% slice(1:22,24,25,23)

TablaD6N <- cbind(Tabla, D6depto) %>% 
  select(Subpoblacion, p, LI, LS, ee, CV, deff, n, n.eff, grados, y, CVl) %>%
  mutate(flag = ifelse(CV > cvflag | n < nflag | n.eff < nefflag | grados < dfflag | y < yflag | CVl > cvflag, 
                       "*", ""))

saveRDS(TablaD6N,"Tablas/D6MRuralSA.rds")




##########################################
## Indicador D7: % de mujeres que       ##
##               cubren sus necesidades ##
##               con métodos modernos   ##
##########################################

## es necesaria variable
## mujeres que no desean más hijos o desean posponer y usan metodo moderno
## se incluye aquellas mujeres que utilizaban met moderno pero fallo el metodo
EndesMEF$d7 = ifelse(EndesMEF$usamoderno==1&(EndesMEF$Falla_met==1|
                                               EndesMEF$Nec_sat_pf_T==1),1,0)


################
##  Nacional  ##
################


D7_dom = design.base %>% filter(P_EST_CIVIL_C%in%c(1,2)) %>%
  #group_by(dominio) %>%
  summarise(n = unweighted(n()),
            D7 = survey_ratio(current_method %in% c(3),
                              (current_method_grou %in% c(1,2,3) + unmet_need_2 %in% c(1,2))))
D7_dom 

## Todas las mujeres

D7nacional = design.base %>%
  group_by(EdadQ) %>% 
  cascade(n = unweighted(n()), 
          p = survey_mean(d7,
                          vartype = c("ci", "cv", "se"), 
                          proportion = TRUE, 
                          na.rm = TRUE), 
          defff = survey_mean(d7, 
                              na.rm = TRUE, 
                              deff = TRUE),
          y = unweighted(sum(d7)),
          .fill = "Total") %>% 
  mutate(LI = p_low, 
         LS = p_upp, 
         CV = 100 * p_cv, 
         deff = defff_deff, 
         n.eff = n/deff,
         CVl = ifelse(p <= 0.5, 
                      100 * p_se / (-p * log(p)), 
                      100 * p_se / (-(1 - p) * log(1 - p)))) %>% 
  select(p, LI, LS, ee = p_se, CV, deff, n.eff, y, CVl) %>% 
  as.data.frame()


D7nacional


Tabla = EndesMEF %>% 
  mutate_at(vars(c(EdadQ)), list(~as.character(.))) %>%
  bind_rows(mutate(., EdadQ = "Total")) %>%
  group_by(EdadQ) %>% 
  summarise(n = n(), 
            estratos = length(unique(V022)), 
            UPMs = length(unique(V021)), 
            grados = UPMs - estratos) %>% 
  mutate(Subpoblacion = EdadQ) %>% 
  select(Subpoblacion, n, grados) 

TablaD7 <- cbind(Tabla, D7nacional) %>% 
  select(Subpoblacion, p, LI, LS, ee, CV, deff, n, n.eff, grados, y, CVl) %>%
  mutate(flag = ifelse(CV > cvflag | n < nflag | n.eff < nefflag | grados < dfflag | y < yflag | CVl > cvflag, 
                       "*", ""))

saveRDS(TablaD7,"Tablas/D7nacional.rds")


# Mujeres Unidas

D7nacional = design.base %>% filter(union_1=="Unida") %>%
  group_by(EdadQ) %>% 
  cascade(n = unweighted(n()), 
          p = survey_mean(d7,
                          vartype = c("ci", "cv", "se"), 
                          proportion = TRUE, 
                          na.rm = TRUE), 
          defff = survey_mean(d7, 
                              na.rm = TRUE, 
                              deff = TRUE),
          y = unweighted(sum(d7)),
          .fill = "Total") %>% 
  mutate(LI = p_low, 
         LS = p_upp, 
         CV = 100 * p_cv, 
         deff = defff_deff, 
         n.eff = n/deff,
         CVl = ifelse(p <= 0.5, 
                      100 * p_se / (-p * log(p)), 
                      100 * p_se / (-(1 - p) * log(1 - p)))) %>% 
  select(p, LI, LS, ee = p_se, CV, deff, n.eff, y, CVl) %>% 
  as.data.frame()

Tabla = EndesMEF %>% filter(union_1=="Unida") %>%
  mutate_at(vars(c(EdadQ)), list(~as.character(.))) %>%
  bind_rows(mutate(., EdadQ = "Total")) %>%
  group_by(EdadQ) %>% 
  summarise(n = n(), 
            estratos = length(unique(V022)), 
            UPMs = length(unique(V021)), 
            grados = UPMs - estratos) %>% 
  mutate(Subpoblacion = EdadQ) %>% 
  select(Subpoblacion, n, grados) 

TablaD7 <- cbind(Tabla, D7nacional) %>% 
  select(Subpoblacion, p, LI, LS, ee, CV, deff, n, n.eff, grados, y, CVl) %>%
  mutate(flag = ifelse(CV > cvflag | n < nflag | n.eff < nefflag | grados < dfflag | y < yflag | CVl > cvflag, 
                       "*", ""))

saveRDS(TablaD7,"Tablas/D7nacionalU.rds")

## Mujeres sexualmente activas no unidas

D7nacional = design.base %>% filter(MSA == "Si",union_1!="Unida") %>%
  group_by(EdadQ) %>% 
  cascade(n = unweighted(n()), 
          p = survey_mean(d7,
                          vartype = c("ci", "cv", "se"), 
                          proportion = TRUE, 
                          na.rm = TRUE), 
          defff = survey_mean(d7, 
                              na.rm = TRUE, 
                              deff = TRUE),
          y = unweighted(sum(d7)),
          .fill = "Total") %>% 
  mutate(LI = p_low, 
         LS = p_upp, 
         CV = 100 * p_cv, 
         deff = defff_deff, 
         n.eff = n/deff,
         CVl = ifelse(p <= 0.5, 
                      100 * p_se / (-p * log(p)), 
                      100 * p_se / (-(1 - p) * log(1 - p)))) %>% 
  select(p, LI, LS, ee = p_se, CV, deff, n.eff, y, CVl) %>% 
  as.data.frame()

Tabla = EndesMEF %>% filter(MSA == "Si", union_1!="Unida") %>% 
  mutate_at(vars(c(EdadQ)), list(~as.character(.))) %>%
  bind_rows(mutate(., EdadQ = "Total")) %>%
  group_by(EdadQ) %>% 
  summarise(n = n(), 
            estratos = length(unique(V022)), 
            UPMs = length(unique(V021)), 
            grados = UPMs - estratos) %>% 
  mutate(Subpoblacion = EdadQ) %>% 
  select(Subpoblacion, n, grados) 

TablaD7 <- cbind(Tabla, D7nacional) %>% 
  select(Subpoblacion, p, LI, LS, ee, CV, deff, n, n.eff, grados, y, CVl) %>%
  mutate(flag = ifelse(CV > cvflag | n < nflag | n.eff < nefflag | grados < dfflag | y < yflag | CVl > cvflag, 
                       "*", ""))

saveRDS(TablaD7,"Tablas/D7nacionalSANU.rds")

## Mujeres sexualmente activas

D7nacional = design.base %>% filter(MSA == "Si") %>%
  group_by(EdadQ) %>% 
  cascade(n = unweighted(n()), 
          p = survey_mean(d7,
                          vartype = c("ci", "cv", "se"), 
                          proportion = TRUE, 
                          na.rm = TRUE), 
          defff = survey_mean(d7, 
                              na.rm = TRUE, 
                              deff = TRUE),
          y = unweighted(sum(d7)),
          .fill = "Total") %>% 
  mutate(LI = p_low, 
         LS = p_upp, 
         CV = 100 * p_cv, 
         deff = defff_deff, 
         n.eff = n/deff,
         CVl = ifelse(p <= 0.5, 
                      100 * p_se / (-p * log(p)), 
                      100 * p_se / (-(1 - p) * log(1 - p)))) %>% 
  select(p, LI, LS, ee = p_se, CV, deff, n.eff, y, CVl) %>% 
  as.data.frame()

Tabla = EndesMEF %>% filter(MSA == "Si") %>% 
  mutate_at(vars(c(EdadQ)), list(~as.character(.))) %>%
  bind_rows(mutate(., EdadQ = "Total")) %>%
  group_by(EdadQ) %>% 
  summarise(n = n(), 
            estratos = length(unique(V022)), 
            UPMs = length(unique(V021)), 
            grados = UPMs - estratos) %>% 
  mutate(Subpoblacion = EdadQ) %>% 
  select(Subpoblacion, n, grados) 

TablaD7 <- cbind(Tabla, D7nacional) %>% 
  select(Subpoblacion, p, LI, LS, ee, CV, deff, n, n.eff, grados, y, CVl) %>%
  mutate(flag = ifelse(CV > cvflag | n < nflag | n.eff < nefflag | grados < dfflag | y < yflag | CVl > cvflag, 
                       "*", ""))

saveRDS(TablaD7,"Tablas/D7nacionalSA.rds")

##############
##   Area   ##
##############

## todas las mujeres

D7area = design.base %>%
  group_by(Area) %>% 
  cascade(n = unweighted(n()), 
          p = survey_mean(d7,
                          vartype = c("ci", "cv", "se"), 
                          proportion = TRUE, 
                          na.rm = TRUE), 
          defff = survey_mean(d7, 
                              na.rm = TRUE, 
                              deff = TRUE),
          y = unweighted(sum(d7)),
          .fill = "Total") %>% 
  mutate(LI = p_low, 
         LS = p_upp, 
         CV = 100 * p_cv, 
         deff = defff_deff, 
         n.eff = n/deff,
         CVl = ifelse(p <= 0.5, 
                      100 * p_se / (-p * log(p)), 
                      100 * p_se / (-(1 - p) * log(1 - p)))) %>% 
  select(p, LI, LS, ee = p_se, CV, deff, n.eff, y, CVl) %>% 
  as.data.frame()

Tabla = EndesMEF %>% 
  mutate_at(vars(c(Area)), list(~as.character(.))) %>%
  bind_rows(mutate(., Area = "Total")) %>%
  group_by(Area) %>% 
  summarise(n = n(), 
            estratos = length(unique(V022)), 
            UPMs = length(unique(V021)), 
            grados = UPMs - estratos) %>% 
  mutate(Subpoblacion = Area) %>% 
  select(Subpoblacion, n, grados) %>% slice(3,1,2)

TablaD7 <- cbind(Tabla, D7area) %>% 
  select(Subpoblacion, p, LI, LS, ee, CV, deff, n, n.eff, grados, y, CVl) %>%
  mutate(flag = ifelse(CV > cvflag | n < nflag | n.eff < nefflag | grados < dfflag | y < yflag | CVl > cvflag, 
                       "*", ""))

saveRDS(TablaD7,"Tablas/D7area.rds")

## Mujeres unidas

D7area = design.base %>% filter(union_1 == "Unida") %>%
  group_by(Area) %>% 
  cascade(n = unweighted(n()), 
          p = survey_mean(d7,
                          vartype = c("ci", "cv", "se"), 
                          proportion = TRUE, 
                          na.rm = TRUE), 
          defff = survey_mean(d7, 
                              na.rm = TRUE, 
                              deff = TRUE),
          y = unweighted(sum(d7)),
          .fill = "Total") %>% 
  mutate(LI = p_low, 
         LS = p_upp, 
         CV = 100 * p_cv, 
         deff = defff_deff, 
         n.eff = n/deff,
         CVl = ifelse(p <= 0.5, 
                      100 * p_se / (-p * log(p)), 
                      100 * p_se / (-(1 - p) * log(1 - p)))) %>% 
  select(p, LI, LS, ee = p_se, CV, deff, n.eff, y, CVl) %>% 
  as.data.frame()

Tabla = EndesMEF %>% filter(union_1 == "Unida") %>%
  mutate_at(vars(c(Area)), list(~as.character(.))) %>%
  bind_rows(mutate(., Area = "Total")) %>%
  group_by(Area) %>% 
  summarise(n = n(), 
            estratos = length(unique(V022)), 
            UPMs = length(unique(V021)), 
            grados = UPMs - estratos) %>% 
  mutate(Subpoblacion = Area) %>% 
  select(Subpoblacion, n, grados) %>% slice(3,1,2)

TablaD7 <- cbind(Tabla, D7area) %>% 
  select(Subpoblacion, p, LI, LS, ee, CV, deff, n, n.eff, grados, y, CVl) %>%
  mutate(flag = ifelse(CV > cvflag | n < nflag | n.eff < nefflag | grados < dfflag | y < yflag | CVl > cvflag, 
                       "*", ""))

saveRDS(TablaD7,"Tablas/D7areaU.rds")

## Mujeres sexualmente activas no unidas


D7area = design.base %>% filter(MSA == "Si", union_1!="Unida") %>%
  group_by(Area) %>% 
  cascade(n = unweighted(n()), 
          p = survey_mean(d7,
                          vartype = c("ci", "cv", "se"), 
                          proportion = TRUE, 
                          na.rm = TRUE), 
          defff = survey_mean(d7, 
                              na.rm = TRUE, 
                              deff = TRUE),
          y = unweighted(sum(d7)),
          .fill = "Total") %>% 
  mutate(LI = p_low, 
         LS = p_upp, 
         CV = 100 * p_cv, 
         deff = defff_deff, 
         n.eff = n/deff,
         CVl = ifelse(p <= 0.5, 
                      100 * p_se / (-p * log(p)), 
                      100 * p_se / (-(1 - p) * log(1 - p)))) %>% 
  select(p, LI, LS, ee = p_se, CV, deff, n.eff, y, CVl) %>% 
  as.data.frame()

Tabla = EndesMEF %>% filter(MSA == "Si", union_1!="Unida") %>%
  mutate_at(vars(c(Area)), list(~as.character(.))) %>%
  bind_rows(mutate(., Area = "Total")) %>%
  group_by(Area) %>% 
  summarise(n = n(), 
            estratos = length(unique(V022)), 
            UPMs = length(unique(V021)), 
            grados = UPMs - estratos) %>% 
  mutate(Subpoblacion = Area) %>% 
  select(Subpoblacion, n, grados) %>% slice(3,1,2)

TablaD7 <- cbind(Tabla, D7area) %>% 
  select(Subpoblacion, p, LI, LS, ee, CV, deff, n, n.eff, grados, y, CVl) %>%
  mutate(flag = ifelse(CV > cvflag | n < nflag | n.eff < nefflag | grados < dfflag | y < yflag | CVl > cvflag, 
                       "*", ""))

saveRDS(TablaD7,"Tablas/D7areaSANU.rds")

## Mujeres sexualmente activas


D7area = design.base %>% filter(MSA == "Si") %>%
  group_by(Area) %>% 
  cascade(n = unweighted(n()), 
          p = survey_mean(d7,
                          vartype = c("ci", "cv", "se"), 
                          proportion = TRUE, 
                          na.rm = TRUE), 
          defff = survey_mean(d7, 
                              na.rm = TRUE, 
                              deff = TRUE),
          y = unweighted(sum(d7)),
          .fill = "Total") %>% 
  mutate(LI = p_low, 
         LS = p_upp, 
         CV = 100 * p_cv, 
         deff = defff_deff, 
         n.eff = n/deff,
         CVl = ifelse(p <= 0.5, 
                      100 * p_se / (-p * log(p)), 
                      100 * p_se / (-(1 - p) * log(1 - p)))) %>% 
  select(p, LI, LS, ee = p_se, CV, deff, n.eff, y, CVl) %>% 
  as.data.frame()

Tabla = EndesMEF %>% filter(MSA == "Si") %>%
  mutate_at(vars(c(Area)), list(~as.character(.))) %>%
  bind_rows(mutate(., Area = "Total")) %>%
  group_by(Area) %>% 
  summarise(n = n(), 
            estratos = length(unique(V022)), 
            UPMs = length(unique(V021)), 
            grados = UPMs - estratos) %>% 
  mutate(Subpoblacion = Area) %>% 
  select(Subpoblacion, n, grados) %>% slice(3,1,2)

TablaD7 <- cbind(Tabla, D7area) %>% 
  select(Subpoblacion, p, LI, LS, ee, CV, deff, n, n.eff, grados, y, CVl) %>%
  mutate(flag = ifelse(CV > cvflag | n < nflag | n.eff < nefflag | grados < dfflag | y < yflag | CVl > cvflag, 
                       "*", ""))

saveRDS(TablaD7,"Tablas/D7areaSA.rds")

###################
##  Departamento ##
###################

## todas las mujeres

D7depto = design.base %>%
  group_by(Departamento) %>% 
  cascade(n = unweighted(n()), 
          p = survey_mean(d7,
                          vartype = c("ci", "cv", "se"), 
                          proportion = TRUE, 
                          na.rm = TRUE), 
          defff = survey_mean(d7, 
                              na.rm = TRUE, 
                              deff = TRUE),
          y = unweighted(sum(d7)),
          .fill = "Total") %>% 
  mutate(LI = p_low, 
         LS = p_upp, 
         CV = 100 * p_cv, 
         deff = defff_deff, 
         n.eff = n/deff,
         CVl = ifelse(p <= 0.5, 
                      100 * p_se / (-p * log(p)), 
                      100 * p_se / (-(1 - p) * log(1 - p)))) %>% 
  select(p, LI, LS, ee = p_se, CV, deff, n.eff, y, CVl) %>% 
  as.data.frame()

Tabla = EndesMEF %>% 
  mutate_at(vars(c(Departamento)), list(~as.character(.))) %>%
  bind_rows(mutate(., Departamento = "Total")) %>%
  group_by(Departamento) %>% 
  summarise(n = n(), 
            estratos = length(unique(V022)), 
            UPMs = length(unique(V021)), 
            grados = UPMs - estratos) %>% 
  mutate(Subpoblacion = Departamento) %>% 
  select(Subpoblacion, n, grados) %>% slice(1:23,25,26,24)

TablaD7 <- cbind(Tabla, D7depto) %>% 
  select(Subpoblacion, p, LI, LS, ee, CV, deff, n, n.eff, grados, y, CVl) %>%
  mutate(flag = ifelse(CV > cvflag | n < nflag | n.eff < nefflag | grados < dfflag | y < yflag | CVl > cvflag, 
                       "*", ""))

saveRDS(TablaD7,"Tablas/D7depto.rds")

## Mujeres unidas

D7depto = design.base %>% filter(union_1 == "Unida") %>%
  group_by(Departamento) %>% 
  cascade(n = unweighted(n()), 
          p = survey_mean(d7,
                          vartype = c("ci", "cv", "se"), 
                          proportion = TRUE, 
                          na.rm = TRUE), 
          defff = survey_mean(d7, 
                              na.rm = TRUE, 
                              deff = TRUE),
          y = unweighted(sum(d7)),
          .fill = "Total") %>% 
  mutate(LI = p_low, 
         LS = p_upp, 
         CV = 100 * p_cv, 
         deff = defff_deff, 
         n.eff = n/deff,
         CVl = ifelse(p <= 0.5, 
                      100 * p_se / (-p * log(p)), 
                      100 * p_se / (-(1 - p) * log(1 - p)))) %>% 
  select(p, LI, LS, ee = p_se, CV, deff, n.eff, y, CVl) %>% 
  as.data.frame()

Tabla = EndesMEF %>% filter(union_1 == "Unida") %>%
  mutate_at(vars(c(Departamento)), list(~as.character(.))) %>%
  bind_rows(mutate(., Departamento = "Total")) %>%
  group_by(Departamento) %>% 
  summarise(n = n(), 
            estratos = length(unique(V022)), 
            UPMs = length(unique(V021)), 
            grados = UPMs - estratos) %>% 
  mutate(Subpoblacion = Departamento) %>% 
  select(Subpoblacion, n, grados) %>% slice(1:23,25,26,24)

TablaD7 <- cbind(Tabla, D7depto) %>% 
  select(Subpoblacion, p, LI, LS, ee, CV, deff, n, n.eff, grados, y, CVl) %>%
  mutate(flag = ifelse(CV > cvflag | n < nflag | n.eff < nefflag | grados < dfflag | y < yflag | CVl > cvflag, 
                       "*", ""))

saveRDS(TablaD7,"Tablas/D7deptoU.rds")

## Mujeres sexualmente activas no unidas


D7depto = design.base %>% filter(MSA == "Si", union_1!="Unida") %>%
  group_by(Departamento) %>% 
  cascade(n = unweighted(n()), 
          p = survey_mean(d7,
                          vartype = c("ci", "cv", "se"), 
                          proportion = TRUE, 
                          na.rm = TRUE), 
          defff = survey_mean(d7, 
                              na.rm = TRUE, 
                              deff = TRUE),
          y = unweighted(sum(d7)),
          .fill = "Total") %>% 
  mutate(LI = p_low, 
         LS = p_upp, 
         CV = 100 * p_cv, 
         deff = defff_deff, 
         n.eff = n/deff,
         CVl = ifelse(p <= 0.5, 
                      100 * p_se / (-p * log(p)), 
                      100 * p_se / (-(1 - p) * log(1 - p)))) %>% 
  select(p, LI, LS, ee = p_se, CV, deff, n.eff, y, CVl) %>% 
  as.data.frame()

Tabla = EndesMEF %>% filter(MSA == "Si", union_1!="Unida") %>%
  mutate_at(vars(c(Departamento)), list(~as.character(.))) %>%
  bind_rows(mutate(., Departamento = "Total")) %>%
  group_by(Departamento) %>% 
  summarise(n = n(), 
            estratos = length(unique(V022)), 
            UPMs = length(unique(V021)), 
            grados = UPMs - estratos) %>% 
  mutate(Subpoblacion = Departamento) %>% 
  select(Subpoblacion, n, grados) %>% slice(1:23,25,26,24)

TablaD7 <- cbind(Tabla, D7depto) %>% 
  select(Subpoblacion, p, LI, LS, ee, CV, deff, n, n.eff, grados, y, CVl) %>%
  mutate(flag = ifelse(CV > cvflag | n < nflag | n.eff < nefflag | grados < dfflag | y < yflag | CVl > cvflag, 
                       "*", ""))

saveRDS(TablaD7,"Tablas/D7deptoSANU.rds")

## Mujeres sexualmente activas


D7depto = design.base %>% filter(MSA == "Si") %>%
  group_by(Departamento) %>% 
  cascade(n = unweighted(n()), 
          p = survey_mean(d7,
                          vartype = c("ci", "cv", "se"), 
                          proportion = TRUE, 
                          na.rm = TRUE), 
          defff = survey_mean(d7, 
                              na.rm = TRUE, 
                              deff = TRUE),
          y = unweighted(sum(d7)),
          .fill = "Total") %>% 
  mutate(LI = p_low, 
         LS = p_upp, 
         CV = 100 * p_cv, 
         deff = defff_deff, 
         n.eff = n/deff,
         CVl = ifelse(p <= 0.5, 
                      100 * p_se / (-p * log(p)), 
                      100 * p_se / (-(1 - p) * log(1 - p)))) %>% 
  select(p, LI, LS, ee = p_se, CV, deff, n.eff, y, CVl) %>% 
  as.data.frame()

Tabla = EndesMEF %>% filter(MSA == "Si") %>%
  mutate_at(vars(c(Departamento)), list(~as.character(.))) %>%
  bind_rows(mutate(., Departamento = "Total")) %>%
  group_by(Departamento) %>% 
  summarise(n = n(), 
            estratos = length(unique(V022)), 
            UPMs = length(unique(V021)), 
            grados = UPMs - estratos) %>% 
  mutate(Subpoblacion = Departamento) %>% 
  select(Subpoblacion, n, grados) %>% slice(1:23,25,26,24)

TablaD7 <- cbind(Tabla, D7depto) %>% 
  select(Subpoblacion, p, LI, LS, ee, CV, deff, n, n.eff, grados, y, CVl) %>%
  mutate(flag = ifelse(CV > cvflag | n < nflag | n.eff < nefflag | grados < dfflag | y < yflag | CVl > cvflag, 
                       "*", ""))

saveRDS(TablaD7,"Tablas/D7deptoSA.rds")


############################
##  Departamento - Rural ##
############################

## todas las mujeres

D7depto = design.base %>% filter(Area == "Rural") %>%
  group_by(Departamento) %>% 
  cascade(n = unweighted(n()), 
          p = survey_mean(d7,
                          vartype = c("ci", "cv", "se"), 
                          proportion = TRUE, 
                          na.rm = TRUE), 
          defff = survey_mean(d7, 
                              na.rm = TRUE, 
                              deff = TRUE),
          y = unweighted(sum(d7)),
          .fill = "Total") %>% 
  mutate(LI = p_low, 
         LS = p_upp, 
         CV = 100 * p_cv, 
         deff = defff_deff, 
         n.eff = n/deff,
         CVl = ifelse(p <= 0.5, 
                      100 * p_se / (-p * log(p)), 
                      100 * p_se / (-(1 - p) * log(1 - p)))) %>% 
  select(p, LI, LS, ee = p_se, CV, deff, n.eff, y, CVl) %>% 
  as.data.frame()

Tabla = EndesMEF %>% filter(Area == "Rural") %>%
  mutate_at(vars(c(Departamento)), list(~as.character(.))) %>%
  bind_rows(mutate(., Departamento = "Total")) %>%
  group_by(Departamento) %>% 
  summarise(n = n(), 
            estratos = length(unique(V022)), 
            UPMs = length(unique(V021)), 
            grados = UPMs - estratos) %>% 
  mutate(Subpoblacion = Departamento) %>% 
  select(Subpoblacion, n, grados) %>% slice(1:22,24,25,23)

TablaD7 <- cbind(Tabla, D7depto) %>% 
  select(Subpoblacion, p, LI, LS, ee, CV, deff, n, n.eff, grados, y, CVl) %>%
  mutate(flag = ifelse(CV > cvflag | n < nflag | n.eff < nefflag | grados < dfflag | y < yflag | CVl > cvflag, 
                       "*", ""))

saveRDS(TablaD7,"Tablas/D7deptoRural.rds")

## Mujeres unidas

D7depto = design.base %>% filter(Area == "Rural") %>%
  filter(union_1 == "Unida") %>%
  group_by(Departamento) %>% 
  cascade(n = unweighted(n()), 
          p = survey_mean(d7,
                          vartype = c("ci", "cv", "se"), 
                          proportion = TRUE, 
                          na.rm = TRUE), 
          defff = survey_mean(d7, 
                              na.rm = TRUE, 
                              deff = TRUE),
          y = unweighted(sum(d7)),
          .fill = "Total") %>% 
  mutate(LI = p_low, 
         LS = p_upp, 
         CV = 100 * p_cv, 
         deff = defff_deff, 
         n.eff = n/deff,
         CVl = ifelse(p <= 0.5, 
                      100 * p_se / (-p * log(p)), 
                      100 * p_se / (-(1 - p) * log(1 - p)))) %>% 
  select(p, LI, LS, ee = p_se, CV, deff, n.eff, y, CVl) %>% 
  as.data.frame()

Tabla = EndesMEF %>% filter(Area == "Rural") %>%
  filter(union_1 == "Unida") %>%
  mutate_at(vars(c(Departamento)), list(~as.character(.))) %>%
  bind_rows(mutate(., Departamento = "Total")) %>%
  group_by(Departamento) %>% 
  summarise(n = n(), 
            estratos = length(unique(V022)), 
            UPMs = length(unique(V021)), 
            grados = UPMs - estratos) %>% 
  mutate(Subpoblacion = Departamento) %>% 
  select(Subpoblacion, n, grados) %>% slice(1:22,24,25,23)

TablaD7 <- cbind(Tabla, D7depto) %>% 
  select(Subpoblacion, p, LI, LS, ee, CV, deff, n, n.eff, grados, y, CVl) %>%
  mutate(flag = ifelse(CV > cvflag | n < nflag | n.eff < nefflag | grados < dfflag | y < yflag | CVl > cvflag, 
                       "*", ""))

saveRDS(TablaD7,"Tablas/D7deptoRuralU.rds")

## Mujeres sexualmente activas no unidas


D7depto = design.base %>% filter(Area == "Rural") %>%
  filter(MSA == "Si", union_1!="Unida") %>%
  group_by(Departamento) %>% 
  cascade(n = unweighted(n()), 
          p = survey_mean(d7,
                          vartype = c("ci", "cv", "se"), 
                          proportion = TRUE, 
                          na.rm = TRUE), 
          defff = survey_mean(d7, 
                              na.rm = TRUE, 
                              deff = TRUE),
          y = unweighted(sum(d7)),
          .fill = "Total") %>% 
  mutate(LI = p_low, 
         LS = p_upp, 
         CV = 100 * p_cv, 
         deff = defff_deff, 
         n.eff = n/deff,
         CVl = ifelse(p <= 0.5, 
                      100 * p_se / (-p * log(p)), 
                      100 * p_se / (-(1 - p) * log(1 - p)))) %>% 
  select(p, LI, LS, ee = p_se, CV, deff, n.eff, y, CVl) %>% 
  as.data.frame()

Tabla = EndesMEF %>% filter(Area == "Rural") %>%
  filter(MSA == "Si", union_1!="Unida") %>%
  mutate_at(vars(c(Departamento)), list(~as.character(.))) %>%
  bind_rows(mutate(., Departamento = "Total")) %>%
  group_by(Departamento) %>% 
  summarise(n = n(), 
            estratos = length(unique(V022)), 
            UPMs = length(unique(V021)), 
            grados = UPMs - estratos) %>% 
  mutate(Subpoblacion = Departamento) %>% 
  select(Subpoblacion, n, grados) %>% slice(1:22,24,25,23)

TablaD7 <- cbind(Tabla, D7depto) %>% 
  select(Subpoblacion, p, LI, LS, ee, CV, deff, n, n.eff, grados, y, CVl) %>%
  mutate(flag = ifelse(CV > cvflag | n < nflag | n.eff < nefflag | grados < dfflag | y < yflag | CVl > cvflag, 
                       "*", ""))

saveRDS(TablaD7,"Tablas/D7deptoRuralSANU.rds")

## Mujeres sexualmente activas


D7depto = design.base %>% filter(Area == "Rural") %>%
  filter(MSA == "Si") %>%
  group_by(Departamento) %>% 
  cascade(n = unweighted(n()), 
          p = survey_mean(d7,
                          vartype = c("ci", "cv", "se"), 
                          proportion = TRUE, 
                          na.rm = TRUE), 
          defff = survey_mean(d7, 
                              na.rm = TRUE, 
                              deff = TRUE),
          y = unweighted(sum(d7)),
          .fill = "Total") %>% 
  mutate(LI = p_low, 
         LS = p_upp, 
         CV = 100 * p_cv, 
         deff = defff_deff, 
         n.eff = n/deff,
         CVl = ifelse(p <= 0.5, 
                      100 * p_se / (-p * log(p)), 
                      100 * p_se / (-(1 - p) * log(1 - p)))) %>% 
  select(p, LI, LS, ee = p_se, CV, deff, n.eff, y, CVl) %>% 
  as.data.frame()

Tabla = EndesMEF %>% filter(Area == "Rural") %>%
  filter(MSA == "Si") %>%
  mutate_at(vars(c(Departamento)), list(~as.character(.))) %>%
  bind_rows(mutate(., Departamento = "Total")) %>%
  group_by(Departamento) %>% 
  summarise(n = n(), 
            estratos = length(unique(V022)), 
            UPMs = length(unique(V021)), 
            grados = UPMs - estratos) %>% 
  mutate(Subpoblacion = Departamento) %>% 
  select(Subpoblacion, n, grados) %>% slice(1:22,24,25,23)

TablaD7 <- cbind(Tabla, D7depto) %>% 
  select(Subpoblacion, p, LI, LS, ee, CV, deff, n, n.eff, grados, y, CVl) %>%
  mutate(flag = ifelse(CV > cvflag | n < nflag | n.eff < nefflag | grados < dfflag | y < yflag | CVl > cvflag, 
                       "*", ""))

saveRDS(TablaD7,"Tablas/D7deptoRuralSA.rds")

############################
##  Departamento - Urbana ##
############################

## todas las mujeres

D7depto = design.base %>% filter(Area == "Urbana") %>%
  group_by(Departamento) %>% 
  cascade(n = unweighted(n()), 
          p = survey_mean(d7,
                          vartype = c("ci", "cv", "se"), 
                          proportion = TRUE, 
                          na.rm = TRUE), 
          defff = survey_mean(d7, 
                              na.rm = TRUE, 
                              deff = TRUE),
          y = unweighted(sum(d7)),
          .fill = "Total") %>% 
  mutate(LI = p_low, 
         LS = p_upp, 
         CV = 100 * p_cv, 
         deff = defff_deff, 
         n.eff = n/deff,
         CVl = ifelse(p <= 0.5, 
                      100 * p_se / (-p * log(p)), 
                      100 * p_se / (-(1 - p) * log(1 - p)))) %>% 
  select(p, LI, LS, ee = p_se, CV, deff, n.eff, y, CVl) %>% 
  as.data.frame()

Tabla = EndesMEF %>% filter(Area == "Urbana") %>%
  mutate_at(vars(c(Departamento)), list(~as.character(.))) %>%
  bind_rows(mutate(., Departamento = "Total")) %>%
  group_by(Departamento) %>% 
  summarise(n = n(), 
            estratos = length(unique(V022)), 
            UPMs = length(unique(V021)), 
            grados = UPMs - estratos) %>% 
  mutate(Subpoblacion = Departamento) %>% 
  select(Subpoblacion, n, grados) %>% slice(1:23,25,26,24)

TablaD7 <- cbind(Tabla, D7depto) %>% 
  select(Subpoblacion, p, LI, LS, ee, CV, deff, n, n.eff, grados, y, CVl) %>%
  mutate(flag = ifelse(CV > cvflag | n < nflag | n.eff < nefflag | grados < dfflag | y < yflag | CVl > cvflag, 
                       "*", ""))

saveRDS(TablaD7,"Tablas/D7deptoUrbana.rds")

## Mujeres unidas

D7depto = design.base %>% filter(Area == "Urbana") %>%
  filter(union_1 == "Unida") %>%
  group_by(Departamento) %>% 
  cascade(n = unweighted(n()), 
          p = survey_mean(d7,
                          vartype = c("ci", "cv", "se"), 
                          proportion = TRUE, 
                          na.rm = TRUE), 
          defff = survey_mean(d7, 
                              na.rm = TRUE, 
                              deff = TRUE),
          y = unweighted(sum(d7)),
          .fill = "Total") %>% 
  mutate(LI = p_low, 
         LS = p_upp, 
         CV = 100 * p_cv, 
         deff = defff_deff, 
         n.eff = n/deff,
         CVl = ifelse(p <= 0.5, 
                      100 * p_se / (-p * log(p)), 
                      100 * p_se / (-(1 - p) * log(1 - p)))) %>% 
  select(p, LI, LS, ee = p_se, CV, deff, n.eff, y, CVl) %>% 
  as.data.frame()

Tabla = EndesMEF %>% filter(Area == "Urbana") %>%
  filter(union_1 == "Unida") %>%
  mutate_at(vars(c(Departamento)), list(~as.character(.))) %>%
  bind_rows(mutate(., Departamento = "Total")) %>%
  group_by(Departamento) %>% 
  summarise(n = n(), 
            estratos = length(unique(V022)), 
            UPMs = length(unique(V021)), 
            grados = UPMs - estratos) %>% 
  mutate(Subpoblacion = Departamento) %>% 
  select(Subpoblacion, n, grados) %>% slice(1:23,25,26,24)

TablaD7 <- cbind(Tabla, D7depto) %>% 
  select(Subpoblacion, p, LI, LS, ee, CV, deff, n, n.eff, grados, y, CVl) %>%
  mutate(flag = ifelse(CV > cvflag | n < nflag | n.eff < nefflag | grados < dfflag | y < yflag | CVl > cvflag, 
                       "*", ""))

saveRDS(TablaD7,"Tablas/D7deptoUrbanaU.rds")

## Mujeres sexualmente activas no unidas


D7depto = design.base %>% filter(Area == "Urbana") %>%
  filter(MSA == "Si", union_1!="Unida") %>%
  group_by(Departamento) %>% 
  cascade(n = unweighted(n()), 
          p = survey_mean(d7,
                          vartype = c("ci", "cv", "se"), 
                          proportion = TRUE, 
                          na.rm = TRUE), 
          defff = survey_mean(d7, 
                              na.rm = TRUE, 
                              deff = TRUE),
          y = unweighted(sum(d7)),
          .fill = "Total") %>% 
  mutate(LI = p_low, 
         LS = p_upp, 
         CV = 100 * p_cv, 
         deff = defff_deff, 
         n.eff = n/deff,
         CVl = ifelse(p <= 0.5, 
                      100 * p_se / (-p * log(p)), 
                      100 * p_se / (-(1 - p) * log(1 - p)))) %>% 
  select(p, LI, LS, ee = p_se, CV, deff, n.eff, y, CVl) %>% 
  as.data.frame()

Tabla = EndesMEF %>% filter(Area == "Urbana") %>%
  filter(MSA == "Si", union_1!="Unida") %>%
  mutate_at(vars(c(Departamento)), list(~as.character(.))) %>%
  bind_rows(mutate(., Departamento = "Total")) %>%
  group_by(Departamento) %>% 
  summarise(n = n(), 
            estratos = length(unique(V022)), 
            UPMs = length(unique(V021)), 
            grados = UPMs - estratos) %>% 
  mutate(Subpoblacion = Departamento) %>% 
  select(Subpoblacion, n, grados) %>% slice(1:23,25,26,24)

TablaD7 <- cbind(Tabla, D7depto) %>% 
  select(Subpoblacion, p, LI, LS, ee, CV, deff, n, n.eff, grados, y, CVl) %>%
  mutate(flag = ifelse(CV > cvflag | n < nflag | n.eff < nefflag | grados < dfflag | y < yflag | CVl > cvflag, 
                       "*", ""))

saveRDS(TablaD7,"Tablas/D7deptoUrbanaSANU.rds")

## Mujeres sexualmente activas


D7depto = design.base %>% filter(Area == "Urbana") %>%
  filter(MSA == "Si") %>%
  group_by(Departamento) %>% 
  cascade(n = unweighted(n()), 
          p = survey_mean(d7,
                          vartype = c("ci", "cv", "se"), 
                          proportion = TRUE, 
                          na.rm = TRUE), 
          defff = survey_mean(d7, 
                              na.rm = TRUE, 
                              deff = TRUE),
          y = unweighted(sum(d7)),
          .fill = "Total") %>% 
  mutate(LI = p_low, 
         LS = p_upp, 
         CV = 100 * p_cv, 
         deff = defff_deff, 
         n.eff = n/deff,
         CVl = ifelse(p <= 0.5, 
                      100 * p_se / (-p * log(p)), 
                      100 * p_se / (-(1 - p) * log(1 - p)))) %>% 
  select(p, LI, LS, ee = p_se, CV, deff, n.eff, y, CVl) %>% 
  as.data.frame()

Tabla = EndesMEF %>% filter(Area == "Urbana") %>%
  filter(MSA == "Si") %>%
  mutate_at(vars(c(Departamento)), list(~as.character(.))) %>%
  bind_rows(mutate(., Departamento = "Total")) %>%
  group_by(Departamento) %>% 
  summarise(n = n(), 
            estratos = length(unique(V022)), 
            UPMs = length(unique(V021)), 
            grados = UPMs - estratos) %>% 
  mutate(Subpoblacion = Departamento) %>% 
  select(Subpoblacion, n, grados) %>% slice(1:23,25,26,24)

TablaD7 <- cbind(Tabla, D7depto) %>% 
  select(Subpoblacion, p, LI, LS, ee, CV, deff, n, n.eff, grados, y, CVl) %>%
  mutate(flag = ifelse(CV > cvflag | n < nflag | n.eff < nefflag | grados < dfflag | y < yflag | CVl > cvflag, 
                       "*", ""))

saveRDS(TablaD7,"Tablas/D7deptoUrbanaSA.rds")




#################
##  Provincia  ##
#################

## todas las mujeres

D7provin = design.base %>%
  group_by(Provincia) %>% 
  cascade(n = unweighted(n()), 
          p = survey_mean(d7,
                          vartype = c("ci", "cv", "se"), 
                          proportion = TRUE, 
                          na.rm = TRUE), 
          defff = survey_mean(d7, 
                              na.rm = TRUE, 
                              deff = TRUE),
          y = unweighted(sum(d7)),
          .fill = "Total") %>% 
  mutate(LI = p_low, 
         LS = p_upp, 
         CV = 100 * p_cv, 
         deff = defff_deff, 
         n.eff = n/deff,
         CVl = ifelse(p <= 0.5, 
                      100 * p_se / (-p * log(p)), 
                      100 * p_se / (-(1 - p) * log(1 - p)))) %>% 
  select(p, LI, LS, ee = p_se, CV, deff, n.eff, y, CVl) %>% 
  as.data.frame()

Tabla = EndesMEF %>% 
  mutate_at(vars(c(Provincia)), list(~as.character(.))) %>%
  bind_rows(mutate(., Provincia = "Total")) %>%
  group_by(Provincia) %>% 
  summarise(n = n(), 
            estratos = length(unique(V022)), 
            UPMs = length(unique(V021)), 
            grados = UPMs - estratos) %>% 
  mutate(Subpoblacion = Provincia) %>% 
  select(Subpoblacion, n, grados) %>% slice(1:180,182:188,181)

TablaD7 <- cbind(Tabla, D7provin) %>% 
  select(Subpoblacion, p, LI, LS, ee, CV, deff, n, n.eff, grados, y, CVl) %>%
  mutate(flag = ifelse(CV > cvflag | n < nflag | n.eff < nefflag | grados < dfflag | y < yflag | CVl > cvflag, 
                       "*", ""))

saveRDS(TablaD7,"Tablas/D7provin.rds")

## Mujeres unidas

D7provin = design.base %>% filter(union_1 == "Unida") %>%
  group_by(Provincia) %>% 
  cascade(n = unweighted(n()), 
          p = survey_mean(d7,
                          vartype = c("ci", "cv", "se"), 
                          proportion = TRUE, 
                          na.rm = TRUE), 
          defff = survey_mean(d7, 
                              na.rm = TRUE, 
                              deff = TRUE),
          y = unweighted(sum(d7)),
          .fill = "Total") %>% 
  mutate(LI = p_low, 
         LS = p_upp, 
         CV = 100 * p_cv, 
         deff = defff_deff, 
         n.eff = n/deff,
         CVl = ifelse(p <= 0.5, 
                      100 * p_se / (-p * log(p)), 
                      100 * p_se / (-(1 - p) * log(1 - p)))) %>% 
  select(p, LI, LS, ee = p_se, CV, deff, n.eff, y, CVl) %>% 
  as.data.frame()

Tabla = EndesMEF %>% filter(union_1 == "Unida") %>%
  mutate_at(vars(c(Provincia)), list(~as.character(.))) %>%
  bind_rows(mutate(., Provincia = "Total")) %>%
  group_by(Provincia) %>% 
  summarise(n = n(), 
            estratos = length(unique(V022)), 
            UPMs = length(unique(V021)), 
            grados = UPMs - estratos) %>% 
  mutate(Subpoblacion = Provincia) %>% 
  select(Subpoblacion, n, grados) %>% slice(1:180,182:188,181)

TablaD7 <- cbind(Tabla, D7provin) %>% 
  select(Subpoblacion, p, LI, LS, ee, CV, deff, n, n.eff, grados, y, CVl) %>%
  mutate(flag = ifelse(CV > cvflag | n < nflag | n.eff < nefflag | grados < dfflag | y < yflag | CVl > cvflag, 
                       "*", ""))

saveRDS(TablaD7,"Tablas/D7provinU.rds")

## Mujeres sexualmente activas no unidas


D7provin = design.base %>% filter(MSA == "Si", union_1!="Unida") %>%
  group_by(Provincia) %>% 
  cascade(n = unweighted(n()), 
          p = survey_mean(d7,
                          vartype = c("ci", "cv", "se"), 
                          proportion = TRUE, 
                          na.rm = TRUE), 
          defff = survey_mean(d7, 
                              na.rm = TRUE, 
                              deff = TRUE),
          y = unweighted(sum(d7)),
          .fill = "Total") %>% 
  mutate(LI = p_low, 
         LS = p_upp, 
         CV = 100 * p_cv, 
         deff = defff_deff, 
         n.eff = n/deff,
         CVl = ifelse(p <= 0.5, 
                      100 * p_se / (-p * log(p)), 
                      100 * p_se / (-(1 - p) * log(1 - p)))) %>% 
  select(p, LI, LS, ee = p_se, CV, deff, n.eff, y, CVl) %>% 
  as.data.frame()

Tabla = EndesMEF %>% filter(MSA == "Si", union_1!="Unida") %>%
  mutate_at(vars(c(Provincia)), list(~as.character(.))) %>%
  bind_rows(mutate(., Provincia = "Total")) %>%
  group_by(Provincia) %>% 
  summarise(n = n(), 
            estratos = length(unique(V022)), 
            UPMs = length(unique(V021)), 
            grados = UPMs - estratos) %>% 
  mutate(Subpoblacion = Provincia) %>% 
  select(Subpoblacion, n, grados) %>% slice(1:140,142:147,141)

TablaD7 <- cbind(Tabla, D7provin) %>% 
  select(Subpoblacion, p, LI, LS, ee, CV, deff, n, n.eff, grados, y, CVl) %>%
  mutate(flag = ifelse(CV > cvflag | n < nflag | n.eff < nefflag | grados < dfflag | y < yflag | CVl > cvflag, 
                       "*", ""))

saveRDS(TablaD7,"Tablas/D7provinSANU.rds")

## Mujeres sexualmente activas

D7provin = design.base %>% filter(MSA == "Si") %>%
  group_by(Provincia) %>% 
  cascade(n = unweighted(n()), 
          p = survey_mean(d7,
                          vartype = c("ci", "cv", "se"), 
                          proportion = TRUE, 
                          na.rm = TRUE), 
          defff = survey_mean(d7, 
                              na.rm = TRUE, 
                              deff = TRUE),
          y = unweighted(sum(d7)),
          .fill = "Total") %>% 
  mutate(LI = p_low, 
         LS = p_upp, 
         CV = 100 * p_cv, 
         deff = defff_deff, 
         n.eff = n/deff,
         CVl = ifelse(p <= 0.5, 
                      100 * p_se / (-p * log(p)), 
                      100 * p_se / (-(1 - p) * log(1 - p)))) %>% 
  select(p, LI, LS, ee = p_se, CV, deff, n.eff, y, CVl) %>% 
  as.data.frame()

Tabla = EndesMEF %>% filter(MSA == "Si") %>%
  mutate_at(vars(c(Provincia)), list(~as.character(.))) %>%
  bind_rows(mutate(., Provincia = "Total")) %>%
  group_by(Provincia) %>% 
  summarise(n = n(), 
            estratos = length(unique(V022)), 
            UPMs = length(unique(V021)), 
            grados = UPMs - estratos) %>% 
  mutate(Subpoblacion = Provincia) %>% 
  select(Subpoblacion, n, grados) %>% slice(1:180,182:188,181)

TablaD7 <- cbind(Tabla, D7provin) %>% 
  select(Subpoblacion, p, LI, LS, ee, CV, deff, n, n.eff, grados, y, CVl) %>%
  mutate(flag = ifelse(CV > cvflag | n < nflag | n.eff < nefflag | grados < dfflag | y < yflag | CVl > cvflag, 
                       "*", ""))
saveRDS(TablaD7,"Tablas/D7provinSA.rds")