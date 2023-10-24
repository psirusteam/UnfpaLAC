#########################################################
# Proyecto MRP - Leave no one behind                    #
# Mapas de pobreza CEPAL                                #
# Lectura y preparación de las bases de datos           #
# Autor: Stalyn Guerrero & Andrés Gutiérrez             #
#########################################################

### Cleaning R environment ###

rm(list = ls())

#################
### Libraries ###
#################
memory.limit(500000)

library(tidyverse)
library(sampling)
library(DataExplorer)

####################################################
### Loading datasets: ENADID and Population census ###
####################################################
encuesta <- readRDS("MEX/2018/3.NI/Data/encuesta_NI.rds")
censo_mrp <- readRDS("MEX/2018/3.NI/Data/censo_mrp.rds") 


##############################
### Exploratory statistics ###
##############################
## El depto esta unido al jefe del hogar

Texp <- encuesta %>%
  group_by(dam) %>%
  summarise(
    "Num_NI"    = sum(NI),
    "Den_usametodo"   = sum(usametodo),
    "NI" = Num_NI/Den_usametodo,
    n = n(),
    Nhat = sum(`fexp`),
    .groups = "drop"
  ) %>%
  mutate(N = sum(n))

Texp


#  ENSANUT: Buscando NA's en las variables de disponibles -----------------

table(encuesta$dam2, useNA = "always")
labelled::generate_dictionary(encuesta %>%
                                select(dam2))

table(encuesta$area, useNA = "always")
labelled::generate_dictionary(encuesta %>%
                                select(area))

table(encuesta$discapacidad, useNA = "always")
labelled::generate_dictionary(encuesta %>%
                                select(discapacidad))

table(encuesta$unida, useNA = "always")
labelled::generate_dictionary(encuesta %>%
                                select(unida))

table(encuesta$edad, useNA = "always")
labelled::generate_dictionary(encuesta %>%
                                select(edad))


table(encuesta$anoest, useNA = "always")
labelled::generate_dictionary(encuesta %>%
                                select(anoest))
unique(encuesta$anoest)

table(encuesta$etnia , useNA = "always")
labelled::generate_dictionary(encuesta %>%
                                select(etnia))

table(encuesta$anoest, encuesta$edad, useNA = "always")

table(encuesta$usametodo, useNA = "always")
labelled::generate_dictionary(encuesta %>%
                                select(usametodo))

table(encuesta$usamoderno, useNA = "always")
labelled::generate_dictionary(encuesta %>%
                                select(usamoderno))


##### Eliminando la variable sexo ya que se tienen solo mujeres

#encuesta$sexo <-NULL


##### Adecuando censo_mrp (eliminando las categorías que
#####                       no se tienen en cuenta en el análisis)

table(censo_mrp$anoest, useNA = "a")
table(censo_mrp$sexo, useNA = "a")
table(censo_mrp$edad, useNA = "a")

### edad,
### Años de estudio,
### Etnia

### No se incluyen las variables estrato y upm  ya que no necesitan ser estandarazidos

encuesta_mrp <- encuesta %>%
  transmute(
   dam,
   dam2,
  usametodo ,
  NI,
  area = ifelse(area == "Rural", "0","1"),
  edad = case_when(  edad == "15-19" ~  "2",       # 5 a 14
                     edad == "20-24" ~  "3",      # 15 a 20
                     edad == "25-29" ~ "3",      # 21 a 30
                     edad == "30-34" ~ "4",      # 31 a 39
                     edad == "35-39" ~ "4",      # 31 a 39
                     edad == "40-44" ~ "5",      # 40 a 49
                     edad == "45-49" ~ "5"      # 40 a 49
                     ),               
    
    etnia = case_when(etnia == "Indigena" ~ "1", # Indigena
                      etnia == "Afrodescendiente" ~ "2", # afro negro mulato
                      TRUE ~ "3"),
    # Otro
    
  anoest = case_when(
    anoest == "NS/NR" ~ "99",
    anoest == "Sin educación"   ~ "1",
    anoest == "1-6 años"   ~ "2",
    anoest == "7-12 años"  ~ "3",
    anoest  ==  "Más de 12 años"        ~ "4"
  ), 
    
    discapacidad = ifelse(discapacidad == "Dispacitado","1","0"),
    
    unida = case_when(unida == "Unida" ~ "1", unida == "No unida" ~ "0"
    ),
    
    upm = upm,
    estrato = estrato,
    fexp = fexp
  )


########

###### Validación variables interes

table(encuesta_mrp$anoest, useNA = "a")
table(encuesta_mrp$etnia, useNA = "a")
table(encuesta_mrp$edad, useNA = "a")
table(encuesta_mrp$area, useNA = "a")
table(encuesta_mrp$unida, useNA = "a")
table(encuesta_mrp$discapacidad, useNA = "a")


###### Eliminando las categorías no comtempladas de edad

encuesta_mrp <- encuesta_mrp %>%
  filter(edad %in% c(2:5))

encuesta_mrp <- encuesta_mrp %>%
  filter(anoest!="99")


table(encuesta_mrp$edad, useNA = "a")

# validación visual

plot_intro(encuesta_mrp)
plot_missing(encuesta_mrp)
plot_histogram(encuesta_mrp)
plot_bar(encuesta_mrp, with = "fexp")

saveRDS(encuesta_mrp, file = "MEX/2018/3.NI/Data/encuesta_mrp.rds")





