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
ecuador <-  redatam.open( "ECU/2018/1.D6/Data/CE11.dicX")

redatam.entities(ecuador)
redatam.variables(dic = ecuador, entName =  "PERSONA")
redatam.variables(ecuador, "VIVIENDA")
redatam.variables(ecuador, "HOGAR")
redatam.variables(ecuador, "CANTON")
redatam.variables(ecuador, "PROVIN")

CONTEOS <- redatam.query(ecuador, "freq CANTON.CANTON
                      by VIVIENDA.UR
                      by PERSONA.P01
                      by PERSONA.P03
                      by PERSONA.P16
                      by PERSONA.UnidasR
                      by PERSONA.EDUCA
                      by PERSONA.DISCAP
                      ", tot.omit = FALSE)

saveRDS(CONTEOS, "ECU/2018/1.D6/Data/CONTEOS.RDS")
rm("$table1")
# CONTEOS <- readRDS("ECU/2018/1.D6/Data/CONTEOS.RDS")


#   revisando valores unicos.
map(grep(pattern = "_label", x = names(CONTEOS),value = TRUE),
    function(by){
      unique(CONTEOS[[by]])
    })
# Eliminando totales de la tabla
CONTEOS2 <- CONTEOS %>% filter_at(vars(matches("_label")),all_vars(. !=  "__tot__"))
names(CONTEOS2)
#### elimando las edades menores a 10 años 

CONTEOS2<-CONTEOS2 %>% 
      filter(P034_value >= 10)

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

censo_mrp <- CONTEOS2 %>%
  transmute(
    mpio = str_pad(
      string = CANTON1_value,
      width = 4,
      pad = "0"
    ),
    area = case_when(UR2_value == 1 ~ "1", # 1 = Urbana
                     TRUE ~ "0"),    # 0 = Rural
    sexo = as.character(P013_value),
    
    edad = case_when(
      P034_value %in% 0:14 ~  "1",       # 5 a 14
      P034_value %in% 15:20 ~ "2",      # 15 a 20
      P034_value %in% 21:30 ~ "3",      # 21 a 30
      P034_value %in% 31:39 ~ "4",      # 31 a 39
      P034_value %in% 40:49 ~ "5",      # 40 a 49
      TRUE ~ "6"                     
       ),     
    
    anoest = case_when( 
      EDUCA7_value == 98  ~ "98", # No aplica
      EDUCA7_value == 99  ~ "99", #NS/NR
      EDUCA7_value == 1   ~ "1",  # Sin educacion
      EDUCA7_value == 2   ~ "2",  # 1-6
      EDUCA7_value == 3   ~ "3",  # 7-12
      EDUCA7_value == 4   ~ "4" ,  # 12 o mas
      TRUE ~ "Error"
    ),
    
    etnia = case_when(
      P165_value %in% 1   ~ "1", #indigena
      P165_value %in% 2:4 ~ "2", #afro negro mulato
      TRUE ~ "3" 
    ),# Otro
   
     discapacidad = case_when(
      DISCAP8_value %in% 1:6 ~ "1", # discapacitado
      TRUE ~ "0"
    ), # No discapacitado
    
    unida = case_when(
      UnidasR6_value == 1 ~ "1",# Unida
      TRUE ~ "2" # Otro
    ),
    
    value) %>% group_by(mpio, area, sexo, edad, etnia, anoest, discapacidad,
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


saveRDS(censo_mrp, "ECU/2018/1.D6/Data/censo_mrp.rds")

######## Ocupación

OCUPACION <-
  redatam.query(ecuador, "freq CANTON.CANTON by PERSONA.TIPOACT",
                tot.omit = FALSE)

OCUPACION2 <- OCUPACION %>% filter_at(vars(matches("_label")),
                                      all_vars(!. %in%   c(
                                        "__mv__", "__tot__", "No especificado", "__na__"
                                      )))

group_by(OCUPACION2,TIPOACT2_value, TIPOACT2_label) %>% summarise(n = sum(value))

OCUPACION2 <- OCUPACION2 %>% transmute(
  mpio = str_pad(
    string = CANTON1_value,
    width = 4,
    pad = "0"
  ),
  ocupados = ifelse(TIPOACT2_value %in% c(1:5),1,0),
  desocupados = ifelse(TIPOACT2_value %in% c(6,7),1,0),
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

saveRDS(tasa_desocupacion, "ECU/2018/1.D6/Data/tasa_desocupacion.rds")

# 