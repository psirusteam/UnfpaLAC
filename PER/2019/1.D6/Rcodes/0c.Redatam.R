############################################################
# Proyecto Unfpa Métodos de planificación Familiar         #
# Mapas de Indicadores de Planificación familiar           #
# Lectura y preparación de las bases de datos              #
# Autor: Stalyn Guerrero,Andrés Gutiérrez & Gabriel Nieto  #
############################################################

### Cleaning R environment ###


rm(list = ls())
gc()
library(Rcpp)
library(RcppProgress)
library(redatam)
library(dplyr)
library(tidyverse)
library(haven)
library(DataExplorer)
library(openxlsx)
## Diccionario traido directamente de los repositorios en Celade
peru <-  redatam.open( "PER/2019/1.D6/Data/cpv2017per-cde.dicx")

redatam.entities(peru)
redatam.variables(dic = peru, entName =  "PERSONA")
redatam.variables(peru, "VIVIENDA")
redatam.variables(peru, "HOGAR")
redatam.variables(peru, "DISTRITO")
redatam.variables(peru, "DEPARTAM")
redatam.variables(peru, "PROVINCI")

CONTEOS <- redatam.query(peru, "freq PROVINCI.REDCODEN   
                                  by VIVIENDA.VAREA  
                                  by PERSONA.C5P041  
                                  by PERSONA.C5P02   
                                  by PERSONA.EDUCA
                                  by PERSONA.UnidasR
                                  by PERSONA.P09DISC
                                  by PERSONA.PBLOPER", tot.omit = FALSE)

saveRDS(CONTEOS, "PER/2019/1.D6/Data/CONTEOS.RDS")
rm("$table1")
#CONTEOS <- readRDS("PER/2019/1.D6/Data/CONTEOS.RDS")

#   revisando valores unicos.
map(grep(pattern = "_value", x = names(CONTEOS),value = TRUE),
    function(by){
      unique(CONTEOS[[by]])
    })
# Eliminando totales de la tabla
CONTEOS2 <- CONTEOS %>% filter_at(vars(matches("_label")),all_vars(. !=  "__tot__"))

sum(CONTEOS2$value)
sum(CONTEOS2$weight_tally)
#CONTEOS2$value <- CONTEOS2$weight_tally

#### elimando las edades menores a 12 años 

CONTEOS2<-CONTEOS2 %>% 
  filter(C5P0413_value >= 12)

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
names(CONTEOS2)

CONTEOS2 %>% group_by(EDUCA5_label, EDUCA5_value) %>%
  summarise(n = sum(value))  %>%
  mutate(N = sum(n)) %>%
  data.frame()

CONTEOS2 %>% group_by(PBLOPER8_label, PBLOPER8_value) %>%
  summarise(n = sum(value))  %>%
  mutate(N = sum(n)) %>%
  data.frame()

names(CONTEOS2)

##### Creando Censo_mrp

censo_mrp <- CONTEOS2 %>%
  transmute(mpio = str_pad(
    string = REDCODEN1_value,
    width = 4,
    pad = "0"
  ),
  area = case_when(VAREA2_value == 1 ~ "1", # 1 = Urbana
                   TRUE ~ "0"),    # 0 = Rural
  sexo = as.character(C5P024_value),
  
  edad = case_when(
    C5P0413_value %in% 0:14 ~  "1",       # 0 a 14
    C5P0413_value %in% 15:20 ~ "2",      # 15 a 20
    C5P0413_value %in% 21:30 ~ "3",      # 21 a 30
    C5P0413_value %in% 31:39 ~ "4",      # 31 a 39
    C5P0413_value %in% 40:49 ~ "5",      # 40 a 49
    TRUE ~ "6"                     
  ),     
  
  anoest = case_when(
    EDUCA5_value == 98  ~ "98", # No aplica
    EDUCA5_value == 99  ~ "99", #NS/NR
    EDUCA5_value == 1   ~ "1",  # Sin educacion
    EDUCA5_value == 2   ~ "2",  # 1-6
    EDUCA5_value == 3   ~ "3",  # 7-12
    EDUCA5_value == 4   ~ "4" ,  # 12 o mas
    TRUE ~ "Error"
  ),
  
  etnia = case_when(
    PBLOPER8_value == 1 ~ "1", # Indigena
    PBLOPER8_value == 2 ~ "2", # Afro
    TRUE ~ "3"), # Otro
  
  discapacidad = case_when(
    P09DISC7_value == 63 ~ "0", # No discapacitado
    TRUE ~ "1"
    ), # Discapacitado
  
  unida = case_when(
    UnidasR6_value == 1 ~ "1",# Unida
    TRUE ~ "2" # Otro
  ),
  
  value) %>%
  group_by(mpio, area, sexo, edad, etnia, discapacidad, anoest, unida) %>%
  summarise(n = sum(value), .groups = "drop")

# Suma del total nacional
sum(censo_mrp$n)

# agregados por nuevas variables
map(c(
  "mpio",
  "area",
  "discapacidad",
  "sexo",
  "edad",
  "etnia",
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
saveRDS(censo_mrp, "PER/2019/1.D6/Data/censo_mrp.rds")


OCUPACION <- redatam.query(peru, "freq PROVINCI.REDCODEN
                           by PERSONA.PET", tot.omit = FALSE)

group_by(OCUPACION, PET2_value, PET2_label) %>% 
  summarise(n = sum(value))

OCUPACION2 <- OCUPACION %>%
  filter(!PET2_label %in% c("__tot__", "No especificado", "__na__"))

group_by(OCUPACION2, PET2_value, PET2_label) %>% summarise(n = sum(value))


OCUPACION2 <- OCUPACION2  %>% transmute(
  mpio = str_pad(
    string = REDCODEN1_value,
    width = 4,
    pad = "0"
  ),
  
  ocupados = ifelse(PET2_value  %in% c(2), 1, 0),
  desocupados = ifelse(PET2_value  %in% c(3, 4), 1, 0),
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

#write.xlsx(tasa_desocupacion, "tasa_desocup.xlsx")

saveRDS(tasa_desocupacion, "PER/2019/1.D6/Data/tasa_desocupacion.rds")


