############################################################
# Proyecto Unfpa Métodos de planificación Familiar         #
# Mapas de Indicadores de Planificación familiar           #
# Lectura y preparación de las bases de datos              #
# Autor: Stalyn Guerrero,Andrés Gutiérrez  #
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
mexico <-  redatam.open( "MEX/2018/1.D6/Data/cpv2020mex-cde.dicX")

redatam.entities(mexico)
redatam.variables(mexico, "VIVIENDA")
redatam.variables(mexico, "MUN") %>% view()
redatam.query(
  mexico,
  "freq  MUN.REDCODE", tot.omit = FALSE)

CONTEOS <- redatam.query(
  mexico,
  "    freq   MUN.REDCODE
       by   VIVIENDA.area
       by   PERSONA.sexo
       by   PERSONA.EDAD6
       by   PERSONA.pbloper
       by   PERSONA.EDUCA
       by   PERSONA.disres
       by   PERSONA.UnidasR
       ",
  tot.omit = FALSE)

saveRDS(CONTEOS, "MEX/2018/1.D6/Data/CONTEOS2.RDS")
rm("$table1")
# CONTEOS <- readRDS("MEX/2018/1.D6/Data/CONTEOS2.RDS")

#   revisando valores unicos.
map(grep(pattern = "_label", x = names(CONTEOS),value = TRUE),
    function(by){
      unique(CONTEOS[[by]])
    })
# Eliminando totales de la tabla
CONTEOS2 <- CONTEOS %>% filter_at(vars(matches("_label")),
                                  all_vars(!. %in%  c("__mv__","__tot__")))
names(CONTEOS2)
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

# group_by(CONTEOS2, EDUCA7_value, EDUCA7_value) %>%
#   summarise(n = sum(value)) %>% data.frame()

censo_mrp <- CONTEOS2 %>% transmute(
  dam2 = str_pad(
    string = REDCODEN1_value,
    width = 5,
    pad = "0"
  ),
  dam = str_sub(string = dam2,start = 1,end = 2 ),
  area = case_when(area2_value == 1 ~ "1", # 1 = Urbana
                   TRUE ~ "0"),    # 0 = Rural
  sexo = as.character(sexo3_value),
  edad = as.character(EDAD64_value),
  anoest = as.character(EDUCA6_value),
  etnia = case_when(
    pbloper5_value == 1 ~ "1",    # Indigena
    pbloper5_value %in% c(5, 2)  ~ "2", # Afro,
    TRUE ~ "3" # Otro
  ),
  discapacidad = case_when(
    disres7_value == 8 ~ "0",    # No discapacitado
    TRUE ~ "1" # Discapacitado
  ),
  unida = as.character(UnidasR8_value),
  value
) %>% group_by(dam,area, etnia, sexo, edad, anoest, discapacidad,unida) %>%
  summarise(n = sum(value), .groups = "drop") %>% 
  filter(sexo == "2")

# Suma del total nacional
sum(censo_mrp$n)

# agregados por nuevas variables
map(c(
  "dam",
  "area",
  "etnia",
  "sexo",
  "edad",
  "anoest",
  "unida",
  "discapacidad"
),
function(x) {
  censo_mrp %>% group_by_at(x) %>%
    summarise(n = sum(n)) %>%
    mutate(Prop = n / sum(n), N = sum(n))
})


plot_intro(censo_mrp)
plot_missing(censo_mrp)
plot_bar(censo_mrp, with = "n")


saveRDS(censo_mrp, "MEX/2018/1.D6/Data/censo_mrp.rds")



