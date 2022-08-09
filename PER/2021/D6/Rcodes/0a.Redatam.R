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
peru <-  redatam.open( "PER/2021/D6/Data/cpv2017per-cde.dicx")

redatam.entities(peru)
redatam.variables(dic = peru, entName =  "PERSONA")
redatam.variables(peru, "VIVIENDA")
redatam.variables(peru, "HOGAR")
redatam.variables(peru, "DISTRITO")
redatam.variables(peru, "DEPARTAM")

CONTEOS <- redatam.query(peru, "freq DEPARTAM.CCDD   
                                  by VIVIENDA.VAREA  
                                  by PERSONA.C5P041  
                                  by PERSONA.C5P02   
                                  by PERSONA.ANEST
                                  by PERSONA.P09DISC
                                  by PERSONA.PBLOPER
                           TALLY PERSONA.FACTORPOND", tot.omit = FALSE)

saveRDS(CONTEOS, "PER/2021/D6/Data/CONTEOS.RDS")
rm("$table1")
#CONTEOS <- readRDS("URY/2020/1.Ingreso/Data/CONTEOS.RDS")


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


CONTEOS2 %>% group_by(ANEST5_label, ANEST5_value) %>%
  summarise(n = sum(value))  %>%
  mutate(N = sum(n)) %>%
  data.frame()

CONTEOS2 %>% group_by(PBLOPER7_label, PBLOPER7_value) %>%
  summarise(n = sum(value))  %>%
  mutate(N = sum(n)) %>%
  data.frame()


censo_mrp <- CONTEOS2 %>%
  transmute(depto = str_pad(
    string = CCDD1_value,
    width = 2,
    pad = "0"
  ),
  area = case_when(VAREA2_value == 1 ~ "1", # 1 = Urbana
                   TRUE ~ "0"),    # 0 = Rural
  sexo = as.character(C5P024_value),
  
  edad = case_when(
    C5P0413_value  %in% 0:14 ~ "1", # 5 a 14
    C5P0413_value  %in% 15:29 ~ "2", # 15 a 29
    C5P0413_value  %in% 30:44 ~ "3", # 30 a 44
    C5P0413_value  %in% 45:64 ~ "4", # 45 a 64
    TRUE ~ "5"), # 65 o mas
  
  anoest = case_when(
    C5P0413_value < 4| is.na(ANEST5_value) ~ "98",     # No aplica
    ANEST5_value == 99 ~ "99", #NS/NR
    ANEST5_value %in% 0 ~ "1",  # Sin educacion
    ANEST5_value %in% c(1:6) ~ "2",  # 1-6
    ANEST5_value %in% c(7:11) ~ "3",  # 7-12 (caso particular  de perú)
    ANEST5_value > 11 ~ "4" ,  # 12 o mas
    TRUE ~ "Error"
  ),
  etnia = case_when(
    PBLOPER7_value == 1 ~ "1", # Indigena
    PBLOPER7_value == 2 ~ "2", # Afro
    TRUE ~ "3"), # Otro
  
  discapacidad = case_when(
    P09DISC6_value == 63 ~ "0", # No discapacitado
    TRUE ~ "1"), # Discapacitado
  value) %>%
  group_by(depto, area, sexo, edad, etnia, discapacidad, anoest) %>%
  summarise(n = sum(value), .groups = "drop")

# Suma del total nacional
sum(censo_mrp$n)

# agregados por nuevas variables
map(c(
  "depto",
  "area",
  "discapacidad",
  "sexo",
  "edad",
  "etnia",
  "anoest"
),
function(x) {
  censo_mrp %>% group_by_at(x) %>%
    summarise(n = sum(n)) %>%
    mutate(Prop = n / sum(n), N = sum(n))
})

plot_intro(censo_mrp)
plot_missing(censo_mrp)
plot_bar(censo_mrp, with = "n")
saveRDS(censo_mrp, "PER/2021/D6/Data/censo_mrp.rds")



OCUPACION <- redatam.query(peru, "freq DEPARTAM.CCDD
                           by PERSONA.PET
                           TALLY PERSONA.FACTORPOND", tot.omit = FALSE)

group_by(OCUPACION, PET2_value, PET2_label) %>% 
  summarise(n = sum(value))

OCUPACION2 <- OCUPACION %>%
  filter(!PET2_label %in% c("__tot__", "No especificado", "__na__"))

group_by(OCUPACION2, PET2_value, PET2_label) %>% summarise(n = sum(value))


OCUPACION2 <- OCUPACION2  %>% transmute(
  depto = str_pad(
    string = CCDD1_value,
    width = 2,
    pad = "0"
  ),
  
  ocupados = ifelse(PET2_value  %in% c(2), 1, 0),
  desocupados = ifelse(PET2_value  %in% c(3, 4), 1, 0),
  value
) %>% group_by(depto, ocupados, desocupados) %>%
  summarise(value = sum(value))


write.xlsx(OCUPACION2, "Ocupacion_tabla.xlsx")

tabla <-
  pivot_wider(
    OCUPACION2,
    names_from = c("ocupados", "desocupados"),
    values_from = value,
    names_prefix = c("ocupados")
  )

write.xlsx(tabla, "Ocupacion_tablai.xlsx")

tasa_desocupacion  <- tabla %>%
  transmute(depto,
            tasa_desocupacion = ocupados0_1 / sum(ocupados0_1 + ocupados1_0))

write.xlsx(tasa_desocupacion, "tasa_desocup.xlsx")

saveRDS(tasa_desocupacion, "PER/2021/D6/Data/tasa_desocupacion.rds")
saveRDS(tasa_desocupacion, "PER/2021/D6m/Data/tasa_desocupacion.rds")
saveRDS(tasa_desocupacion, "PER/2021/D7/Data/tasa_desocupacion.rds")
saveRDS(tasa_desocupacion, "PER/2021/NI/Data/tasa_desocupacion.rds")


# ## Leer encuesta
# # encuesta <- read_dta("Z:/BG/ury19n/ury19n.dta")
# encuesta <- read_dta("V:/DAT/BADEHOG_N/BC/URY_2020N.dta")
# ## Guardar encuesta
# saveRDS(encuesta, "2020/1.Ingreso/Data/encuestaURY20N.rds")