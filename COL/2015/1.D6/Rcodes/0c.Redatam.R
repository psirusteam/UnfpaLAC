############################################################
# Proyecto Unfpa Métodos de planificación Familiar         #
# Mapas de Indicadores de Planificación familiar           #
# Lectura y preparación de las bases de datos              #
# Autor: Stalyn Guerrero,Andrés Gutiérrez & Gabriel Nieto  #
############################################################

### Cleaning R environment ###


rm(list = ls())
library(Rcpp)
library(RcppProgress)
library(redatam)
library(dplyr)
library(tidyverse)
library(haven)
library(DataExplorer)
library(openxlsx)
## Diccionario traido directamente de los repositorios en Celade
colombia <-  redatam.open( "COL/2015/1.D6/Data/cpv2018col-cde.dicX")

redatam.entities(colombia)
redatam.variables(dic = colombia, entName =  "PERSONA")
redatam.variables(colombia, "VIVIENDA")
redatam.variables(colombia, "HOGAR")
redatam.variables(colombia, "MUPIO")
redatam.variables(colombia, "DEPTO")

CONTEOS <- redatam.query(colombia, "freq MUPIO.REDCODEN
                      by CLASE.AREA
                      by PERSONA.P_SEXO
                      by PERSONA.P_EDAD
                      by PERSONA.UnidasR
                      by PERSONA.EDUCA
                      by PERSONA.PBLOPER
                      ", tot.omit = FALSE)

saveRDS(CONTEOS, "COL/2015/1.D6/Data/CONTEOS.RDS")
rm("$table1")
# CONTEOS <- readRDS("COL/2015/1.D6/Data/CONTEOS.RDS")


#   revisando valores unicos.
map(grep(pattern = "_label", x = names(CONTEOS),value = TRUE),
    function(by){
      unique(CONTEOS[[by]])
    })
# Eliminando totales de la tabla
CONTEOS2 <- CONTEOS %>% filter_at(vars(matches("_label")),all_vars(. !=  "__tot__"))
names(CONTEOS2)
#### elimando las edades menores a 12 años 

CONTEOS2<-CONTEOS2 %>% 
      filter(P_EDAD4_value >= 12)

## sumas por variables de agregación, coincidir con el total nacional.
map(grep(pattern = "_value", x = names(CONTEOS2),value = TRUE),
    function(by){
      CONTEOS2 %>% group_by_at(by) %>%
        summarise(n = sum(value)) %>%
        mutate(Prop = n / sum(n), N = sum(n))
    })

map(grep(pattern = "_label", x = names(CONTEOS2),value = TRUE),
    function(by){
      CONTEOS2 %>% group_by_at(by) %>%
        summarise(n = sum(value)) %>%
        mutate(Prop = n / sum(n), N = sum(n))
    })

# group_by(CONTEOS2, EDUCA6_value, EDUCA6_value) %>%
#   summarise(n = sum(value)) %>% data.frame()

censo_mrp <- CONTEOS2 %>%
  transmute(
    mpio = str_pad(
      string = REDCODEN1_value,
      width = 5,
      pad = "0"
    ),
    area = case_when(AREA2_value == 1 ~ "1", # 1 = Urbana
                                 TRUE ~ "0"  # 0 = Rural
    ),   
    sexo = as.character(P_SEXO3_value),
    
    edad = case_when(
      P_EDAD4_value %in% 0:14 ~  "1",       # 5 a 14
      P_EDAD4_value %in% 15:20 ~ "2",      # 15 a 20
      P_EDAD4_value %in% 21:30 ~ "3",      # 21 a 30
      P_EDAD4_value %in% 31:39 ~ "4",      # 31 a 39
      P_EDAD4_value %in% 40:49 ~ "5",      # 40 a 49
                          TRUE ~ "6"                     
       ),     
    
    anoest = case_when( 
      EDUCA6_value == 98  ~ "98", # No aplica
      EDUCA6_value == 99  ~ "99", #NS/NR
      EDUCA6_value == 1   ~ "1",  # Sin educacion
      EDUCA6_value == 2   ~ "2",  # 1-6
      EDUCA6_value == 3   ~ "3",  # 7-12
      EDUCA6_value == 4   ~ "4" , # 12 o mas
                     TRUE ~ "Error"
    ),
    
    etnia = case_when(
      PBLOPER7_value %in% c(1) ~ "1", #indigena
      PBLOPER7_value %in% c(2) ~ "2", #afro negro mulato
                          TRUE ~ "3"  # Otro
    ),
   
    unida = case_when(
      UnidasR5_value == 1 ~ "1",# Unida
                     TRUE ~ "2" # Otro
    ),
    
    value) %>% group_by(mpio, area, sexo, edad, etnia, anoest, 
                        unida) %>%
  summarise(n = sum(value))

# Suma del total nacional
sum(censo_mrp$n)

# agregados por nuevas variables
map(c(
  "mpio",
  "area",
  "etnia",
  "sexo",
  "edad",
  "anoest",
  "unida"
  ),
function(x) {
  censo_mrp %>% group_by_at(x) %>%
    summarise(n = sum(n)) %>%
    mutate(Prop = n / sum(n), N = sum(n))
})


plot_intro(censo_mrp)
plot_missing(censo_mrp)
plot_bar(censo_mrp, with = "n")


saveRDS(censo_mrp, "COL/2015/1.D6/Data/censo_mrp.rds")

######## Ocupación

OCUPACION <-
  redatam.query(colombia, "freq MUPIO.REDCODEN by PERSONA.PET",
                tot.omit = FALSE)

OCUPACION2 <- OCUPACION %>% filter_at(vars(matches("_label")),
                                      all_vars(!. %in%   c(
                                        "__mv__", "__tot__", "No especificado", "__na__"
                                      )))

names(OCUPACION2)

group_by(OCUPACION2,PET2_value, PET2_label) %>% summarise(n = sum(value))

OCUPACION2 <- OCUPACION2 %>% transmute(
  mpio = str_pad(
    string = REDCODEN1_value,
    width = 5,
    pad = "0"
  ),
  ocupados = ifelse(PET2_value %in% c(1), 1, 0),
  desocupados = ifelse(PET2_value %in% c(2), 1, 0),
  value
) %>% group_by(mpio, ocupados, desocupados) %>%
  summarise(value = sum(value))


#write.xlsx(OCUPACION2, "Ocupacion_tabla.xlsx")

tabla <-
  pivot_wider(
    OCUPACION2,
    names_from = c("ocupados", "desocupados"),
    values_from = value,
    names_prefix = c("ocupados")
  )

#write.xlsx(tabla, "Ocupacion_tablai.xlsx")

tasa_desocupacion  <- tabla %>%
  transmute(mpio,
            tasa_desocupacion = ocupados0_1 / sum(ocupados0_1 + ocupados1_0))

sum(complete.cases(tasa_desocupacion))

#write.xlsx(tasa_desocupacion, "tasa_desocup.xlsx")

saveRDS(tasa_desocupacion, "COL/2015/1.D6/Data/tasa_desocupacion.rds")

# 