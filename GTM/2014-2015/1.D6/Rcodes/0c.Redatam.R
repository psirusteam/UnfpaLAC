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
guatemala <-  redatam.open( "GTM/2014-2015/1.D6/Data/cpv2018gtm-cde.dicx")

redatam.entities(guatemala)
redatam.variables(dic = guatemala, entName =  "PERSONA")
redatam.variables(guatemala, "VIVIENDA")
redatam.variables(guatemala, "HOGAR")
redatam.variables(guatemala, "MUPIO")
redatam.variables(guatemala, "DEPTO")
redatam.variables(guatemala, "MUPIO")

CONTEOS <- redatam.query(guatemala, "freq DEPTO.IDEPTO
                              by VIVIENDA.PLG11
                              by  PERSONA.PCP7
                              by PERSONA.PCP6
                              by PERSONA.ANEDUCA
                              by PERSONA.PCP34
                              by PERSONA.PBLOPER",tot.omit = FALSE)

saveRDS(CONTEOS, "GTM/2014-2015/1.D6/Data/CONTEOS.RDS")
rm("$table1")
# CONTEOS <- readRDS("GTM/2014-2015/1.D6/Data/CONTEOS.RDS")


#   revisando valores unicos.
map(grep(pattern = "_value", x = names(CONTEOS),value = TRUE),
    function(by){
      unique(CONTEOS[[by]])
    })
# Eliminando totales de la tabla
CONTEOS2 <-
  CONTEOS %>% filter_at(vars(matches("_label")), all_vars(. !=  "__tot__"))


#### elimando las edades menores a 10 años 

CONTEOS2<-CONTEOS2 %>% 
  filter(PCP73_value >= 10)

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

##
CONTEOS2 %>% group_by(ANEDUCA5_value, ANEDUCA5_label) %>%
  summarise(n = sum(value))  %>%
  mutate(N = sum(n)) %>% data.frame()


censo_mrp <- CONTEOS2 %>% transmute(
  depto = str_pad(
    string = IDEPTO1_value,
    width = 2,
    pad = "0"
  ),
  area = case_when(PLG112_value == 1 ~ "1", # 1 = Urbana
                   TRUE ~ "0"),
  # 0 = Rural
  sexo = as.character(PCP64_value),
  
  edad = case_when(
    PCP73_value %in% 0:14 ~  "1",       # 0 a 14
    PCP73_value %in% 15:20 ~ "2",      # 15 a 20
    PCP73_value %in% 21:30 ~ "3",      # 21 a 30
    PCP73_value %in% 31:39 ~ "4",      # 31 a 39
    PCP73_value %in% 40:49 ~ "5",      # 40 a 49
    TRUE ~ "6"                         # 50 o mas
  ),     
  
  anoest = case_when(
    
    is.na(ANEDUCA5_value) | PCP73_value < 7 ~ "98",     # No aplica
    ANEDUCA5_value == 99 ~ "99", #NS/NR
    ANEDUCA5_value %in% 0 ~ "1",  # Sin educacion
    ANEDUCA5_value %in% c(1:6) ~ "2",  # 1-6
    ANEDUCA5_value %in% c(7:12) ~ "3",  # 7-12
    ANEDUCA5_value > 12 ~ "4" ,  # 12 o mas
    TRUE ~ "Error"
  ),    
  etnia = case_when(
    PBLOPER7_value %in% c(2:3) ~ "2",    # Afro
    PBLOPER7_value == 1  ~ "1", # Indigena,
    TRUE ~ "3" # Otro
  ),
  
  unida = case_when(
    PCP346_value %in% c(2:3) ~ "1",# Unido
    TRUE ~ "2" # Otro
  ),
  
  value
) %>% group_by(depto, area, etnia, sexo, edad, anoest, unida) %>%
  summarise(n = sum(value))

# Suma del total nacional
sum(censo_mrp$n)

# agregados por nuevas variables
map(c("depto", "area", "etnia", "sexo", "edad", "anoest","unida"),
    function(x) {
      censo_mrp %>% group_by_at(x) %>%
        summarise(n = sum(n)) %>%
        mutate(Prop = n / sum(n), N = sum(n))
    })


plot_intro(censo_mrp)
plot_missing(censo_mrp)
plot_bar(censo_mrp, with = "n")

saveRDS(censo_mrp, "GTM/2014-2015/1.D6/Data/censo_mrp.rds")

## tasa de desocupación
OCUPACION <-
  redatam.query(guatemala, "freq DEPTO.IDEPTO by  PERSONA.PET",
                tot.omit = FALSE)

OCUPACION2 <- OCUPACION %>%
  filter(!PET2_label %in% c("__tot__", "No especificado", "__na__"))

group_by(OCUPACION2, PET2_value, PET2_label) %>% summarise(n = sum(value))

sum(OCUPACION2$value)


OCUPACION2 <- OCUPACION2 %>%
  transmute(
    depto = str_pad(
      string = IDEPTO1_value,
      width = 2,
      pad = "0"
    ),
    ocupados = ifelse(PET2_value  %in% c(1), 1, 0),
    desocupados = ifelse(PET2_value  %in% c(2), 1, 0),
    value
  ) %>% group_by(depto, ocupados, desocupados) %>%
  summarise(value = sum(value))


tabla <-
  pivot_wider(
    OCUPACION2,
    names_from = c("ocupados", "desocupados"),
    values_from = value,
    names_prefix = c("ocupados")
  )


tasa_desocupacion <- tabla %>%
  transmute(depto,
            tasa_desocupacion = ocupados0_1 / sum(ocupados0_1 + ocupados1_0))

#write.xlsx(tasa_desocupacion, "tasa_desocup.xlsx")

saveRDS(tasa_desocupacion, "GTM/2014-2015/1.D6/Data/tasa_desocupacion.rds")


