############################################################
# Proyecto MRP - Leave no one behind                       #
# Mapas de pobreza CEPAL                                   #
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
peru <-  redatam.open( "PER/2021/D6/Data/CPVPER2017D.dicX")

redatam.entities(peru)
redatam.variables(dic = peru, entName =  "PERSONA")
redatam.variables(peru, "VIVIENDA")
redatam.variables(peru, "HOGAR")
redatam.variables(peru, "DISTRITO")

CONTEOS <- redatam.query(
  peru,
  "freq DEPTO.IDDEPTO
  by  VIVIENDA.URBRUR
                      by PERSONA.PH02
                      by PERSONA.NA01
                      by PERSONA.ER02
                      by PERSONA.AESTUDIO", tot.omit = FALSE)

saveRDS(CONTEOS, "2020/1.Ingreso/Data/CONTEOS.RDS")
rm("$table1")
# CONTEOS <- readRDS("URY/2020/1.Ingreso/Data/CONTEOS.RDS")


#   revisando valores unicos.
map(grep(pattern = "_label", x = names(CONTEOS),value = TRUE),
    function(by){
      unique(CONTEOS[[by]])
    })
# Eliminando totales de la tabla
CONTEOS2 <- CONTEOS %>% filter_at(vars(matches("_label")),all_vars(. !=  "__tot__"))


## sumas por variables de agregación, coincidir con el total nacional.
map(grep(pattern = "_value", x = names(CONTEOS2),value = TRUE),
    function(by){
      CONTEOS2 %>% group_by_at(by) %>%
        summarise(n = sum(value)) %>%
        mutate(Prop = n / sum(n), N = sum(n)) %>%
        data.frame()
    })
map(grep(pattern = "_label", x = names(CONTEOS2),value = TRUE),
    function(by){
      CONTEOS2 %>% group_by_at(by) %>%
        summarise(n = sum(value)) %>%
        mutate(Prop = n / sum(n), N = sum(n)) %>%
        data.frame()
    })

group_by(CONTEOS2, AESTUDIO6_value, AESTUDIO6_label) %>%
  summarise(n = sum(value)) %>%
  mutate(Prop = n / sum(n), N = sum(n)) %>%
  data.frame()

censo_mrp <- CONTEOS2 %>%
  transmute(depto = str_pad(
              string = IDDEPTO1_value,
              width = 2,
              pad = "0"
            ),
            area = case_when(URBRUR2_value == 1 ~ "1", # 1 = Urbana
                             TRUE ~ "0"),
            sexo = as.character(PH023_value) ,

            edad = case_when(
              NA014_value  < 15 ~ "1", # 5 a 14
              NA014_value  < 30 ~ "2", # 15 a 29
              NA014_value  < 45 ~ "3", # 30 a 44
              NA014_value  < 65 ~ "4", # 45 a 64
              TRUE ~ "5"), # 65 o mas

            anoest = case_when(
              NA014_value < 4| is.na( AESTUDIO6_value) ~ "98",     # No aplica
               AESTUDIO6_value == 88 ~ "99", #NS/NR
               AESTUDIO6_value %in% 0 ~ "1",  # Sin educacion
               AESTUDIO6_value %in% c(1:6) ~ "2",  # 1-6
               AESTUDIO6_value %in% c(7:12) ~ "3",  # 7-12
               AESTUDIO6_value > 12 ~ "4" ,  # 12 o mas
              TRUE ~ "Error"
            ),

           etnia = case_when(
              ER025_value %in% 4 ~ "1", #indigena
              ER025_value %in% 1 ~ "2", #afro negro mulato
              TRUE ~ "3" ),# Otro

            value) %>%
  group_by(depto, area, sexo, edad, etnia, anoest) %>%
  summarise(n = sum(value), .groups = "drop")

# Suma del total nacional
sum(censo_mrp$n)

# agregados por nuevas variables
map(c("depto", "area", "etnia", "sexo", "edad", "anoest"),
    function(x){
      censo_mrp %>% group_by_at(x) %>%
        summarise(n = sum(n)) %>%
        mutate(Prop = n / sum(n), N = sum(n))
    })

plot_intro(censo_mrp)
plot_missing(censo_mrp)
plot_bar(censo_mrp, with = "n")


saveRDS(censo_mrp, "2020/1.Ingreso/Data/censo_mrp.rds")



OCUPACION <-
  redatam.query(peru, "freq DEPTO.IDDEPTO by PERSONA.POBCOAC",
                tot.omit = FALSE)

group_by(OCUPACION, POBCOAC2_value, POBCOAC2_label) %>% summarise(n = sum(value))

OCUPACION2 <- OCUPACION %>%
  filter(!POBCOAC2_label %in% c("__tot__", "No especificado", "__na__"))

group_by(OCUPACION2, POBCOAC2_value, POBCOAC2_label) %>% summarise(n = sum(value))


OCUPACION2 <- OCUPACION2  %>% transmute(
  depto = str_pad(
    string = IDDEPTO1_value,
    width = 2,
    pad = "0"
  ),

  ocupados = ifelse(POBCOAC2_value  %in% c(2), 1, 0),
  desocupados = ifelse(POBCOAC2_value  %in% c(3, 4), 1, 0),
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

saveRDS(tasa_desocupacion, "2020/1.Ingreso/Data/tasa_desocupacion.rds")
saveRDS(tasa_desocupacion, "2020/2.Pobreza/Data/tasa_desocupacion.rds")
saveRDS(tasa_desocupacion, "2020/3.PobrezaExtrema/Data/tasa_desocupacion.rds")

## Leer encuesta
# encuesta <- read_dta("Z:/BG/ury19n/ury19n.dta")
encuesta <- read_dta("V:/DAT/BADEHOG_N/BC/URY_2020N.dta")
## Guardar encuesta
saveRDS(encuesta, "2020/1.Ingreso/Data/encuestaURY20N.rds")
