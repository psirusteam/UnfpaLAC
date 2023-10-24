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
### Loading datasets: CASEN and Population census ###
####################################################
source(file = "0Funciones/funciones_mrp.R", encoding = "UTF-8")
encuesta <- readRDS("GTM/2014-2015/1.D6/Data/ENSMI15.rds")
censo_mrp <- readRDS("GTM/2014-2015/1.D6/Data/censo_mrp.rds")


##############################
### Exploratory statistics ###
##############################
### EPHC: percentage of people living in poverty ##
## El depto esta unido al jefe del hogar

Texp <- encuesta %>%
  group_by(depto) %>%
  summarise(
    "D6_n"    = mean(D6, na.rm = T),
    "D6m_n"   = mean(D6m, na.rm = T),
    "Ni_n"    = mean(NI , na.rm = T),
    
    n = n(),
    Nhat = sum(`fexp`),
    .groups = "drop"
  ) %>%
  mutate(N = sum(n))

Texp


#  ENSANUT: Buscando NA's en las variables de disponibles -----------------

table(encuesta$depto, useNA = "always")
labelled::generate_dictionary(encuesta %>%
                                select(depto))

table(encuesta$area, useNA = "always")
labelled::generate_dictionary(encuesta %>%
                                select(area))

# table(encuesta$discapacidad, useNA = "always")
# labelled::generate_dictionary(encuesta %>%
#                                 select(discapacidad))

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

table(encuesta$unida , useNA = "always")
labelled::generate_dictionary(encuesta %>%
                                select(unida))


table(encuesta$D6, useNA = "always")
labelled::generate_dictionary(encuesta %>%
                                select(D6))

table(encuesta$D6m, useNA = "always")
labelled::generate_dictionary(encuesta %>%
                                select(D6m))

table(encuesta$NI, useNA = "always")
labelled::generate_dictionary(encuesta %>%
                                select(NI))

##### Eliminando la variable sexo ya que se tienen solo mujeres

#encuesta$sexo <-NULL


##### Adecuando censo_mrp (eliminando las categorías que
#####                       no se tienen en cuenta en el análisis)

table(censo_mrp$anoest, useNA = "a")
table(censo_mrp$sexo, useNA = "a")
table(censo_mrp$edad, useNA = "a")

###### Eliminando sexo y edad y filtrando por unidas

censo_mrp <- censo_mrp %>%
  filter(sexo != 1)

table(censo_mrp$edad, useNA = "a")

censo_mrp <- censo_mrp %>%
  filter(!edad %in% c(1, 6))

table(censo_mrp$sexo, useNA = "a")
table(censo_mrp$edad, useNA = "a")

##### Elimando categoria 98 No aplica

censo_mrp <- censo_mrp %>%
  filter(anoest != 98)

table(censo_mrp$anoest, useNA = "a")


##### filtrando por Unidas 

censo_mrp <- censo_mrp %>%
  filter(unida == 1)

### Ensanut: creando las variables de postestratificación:

### edad,
### Años de estudio,
### Etnia

encuesta_mrp <- encuesta %>%
  transmute(
    depto = str_pad(
      string = depto,
      pad = "0",
      width = 2
    ),
    
    usametodo  = D6,
    usamoderno = D6m,
    necesInst  = NI,
    
    area = case_when(area == 1 ~ "1", 
                     TRUE ~ "0"
    ),
    
    edad = case_when(edad %in% 0:14 ~  "1",       # 5 a 14
                     edad %in% 15:20 ~ "2",      # 15 a 20
                     edad %in% 21:30 ~ "3",      # 21 a 30
                     edad %in% 31:39 ~ "4",      # 31 a 39
                     edad %in% 40:49 ~ "5",      # 40 a 49
                     TRUE ~ "6"
    ),
    
    etnia = case_when(
      etnia %in% c(1,4)   ~ "1",# Indigena
      etnia == 3          ~ "2",# afro negro mulato
      etnia %in% c(2,6,8) ~ "3",# Otro
      TRUE ~ "ERROR"
    ),
    
    unida = case_when(
         unida ==1   ~ "1",# unida
         TRUE ~ "2"        # no unida
    ),
      anoest = case_when(
      anoest %in% c(0,1)   ~ "1",  # Sin educacion
      anoest %in% c(2:6)   ~ "2",  # 1 - 6
      anoest %in% c(7:12)  ~ "3",  # 7 - 12
      anoest  >  12        ~ "4",  # mas de 12
      TRUE ~ "Error"
    ),
    
    upm = upm,
    estrato = estrato,
    fexp = `fexp`
  )

###### Eliminando las categorías no comtempladas de edad

table(encuesta_mrp$edad, useNA = "a")

encuesta_mrp <- encuesta_mrp %>%
  filter(!edad %in% c(1, 6))

table(encuesta_mrp$edad, useNA = "a")

table(encuesta_mrp$unida, useNA = "a")

encuesta_mrp <- encuesta_mrp %>%
  filter(unida == 1)

table(encuesta_mrp$unida, useNA = "a")


###### Validación variables interes

table(encuesta_mrp$anoest, useNA = "a")
table(encuesta_mrp$etnia, useNA = "a")
table(encuesta_mrp$edad, useNA = "a")
table(encuesta_mrp$area, useNA = "a")
#table(encuesta_mrp$discapacidad, useNA = "a")


# validación visual

plot_intro(encuesta_mrp)
plot_missing(encuesta_mrp)
plot_histogram(encuesta_mrp)
plot_bar(encuesta_mrp, with = "fexp")

saveRDS(encuesta_mrp, file = "GTM/2014-2015/3.NI/Data/encuesta_mrp.rds")

# Actualización de tabla censal- IPFP -------------------------------------

names_cov <-
  grep(
    pattern =  "^(n|usametodo|usamoderno|necesInst|
                  upm|estrato|fexp)",
    x = names(censo_mrp),
    invert = TRUE,
    value = TRUE
  )

names_cov <- names_cov[names_cov %in% names(encuesta_mrp)]
names_cov

num_cat_censo <-
  apply(censo_mrp[names_cov], MARGIN  = 2, function(x)
    length(unique(x)))
num_cat_censo

num_cat_sample <-
  apply(encuesta_mrp[names_cov], MARGIN  = 2, function(x)
    length(unique(x)))
num_cat_sample

names_cov <- names_cov[num_cat_censo == num_cat_sample]
names_cov

# Matriz Calibrada creada únicamente para los niveles completos

# IMPORTANTE: Excluir las covariables que tengan niveles incompletos


## etnia

encuesta_mrp %>% group_by(etnia) %>%
  summarise(n = sum(fexp),
            n1 = n(),
            .groups = "drop") %>%
  mutate(prop = n1 / sum(n1))

censo_mrp %>% group_by(etnia) %>%
  summarise(n = sum(n),
            .groups = "drop") %>%
  mutate(prop = n / sum(n))


## Edad

encuesta_mrp %>% group_by(edad) %>%
  summarise(n = sum(fexp),
            n1 = n(),
            .groups = "drop") %>%
  mutate(prop = n1 / sum(n1))

censo_mrp %>% group_by(edad) %>%
  summarise(n = sum(n),
            .groups = "drop") %>%
  mutate(prop = n / sum(n))


## Años estudio 

encuesta_mrp %>% group_by(anoest) %>%
  summarise(n = sum(fexp),
            n1 = n(),
            .groups = "drop") %>%
  mutate(prop = n1 / sum(n1))

censo_mrp %>% group_by(anoest) %>%
  summarise(n = sum(n),
            .groups = "drop") %>%
  mutate(prop = n / sum(n))


## Area

encuesta_mrp %>% group_by(area) %>%
  summarise(n = sum(fexp),
            n1 = n(),
            .groups = "drop") %>%
  mutate(prop = n1 / sum(n1))

censo_mrp %>% group_by(area) %>%
  summarise(n = sum(n),
            .groups = "drop") %>%
  mutate(prop = n / sum(n))


## Departamento 

encuesta_mrp %>% group_by(depto) %>%
  summarise(n = sum(fexp),
            n1 = n(),
            .groups = "drop") %>%
  mutate(prop = n1 / sum(n1))

censo_mrp %>% group_by(depto) %>%
  summarise(n = sum(n),
            .groups = "drop") %>%
  mutate(prop = n / sum(n))


auxSuma <- function(dat, col, ni) {
  dat %>% ungroup() %>% select(all_of(col))  %>%
    fastDummies::dummy_cols(remove_selected_columns = TRUE) %>%
    mutate_all(~ . * ni) %>% colSums()
}

###### Verificando el n de cada una de las categorías
### Para el censo y la encuesta expandida

## encuesta
N.g <- map(names_cov,
           ~ auxSuma(encuesta_mrp, col = .x, ni = encuesta_mrp$fexp)) %>%
  unlist()

N.g

## censo

N_censo.g <- map(names_cov,
                 ~ auxSuma(censo_mrp, col = .x, ni = censo_mrp$n)) %>%
  unlist()

N_censo.g

#### verificando el nombre y orden de las variables comunes en los datos

names_xk <- intersect(names(N.g), names(N_censo.g))
names_xk

N.g <- N.g[names_xk]
N.g
N_censo.g <- N_censo.g[names_xk]
N_censo.g

### Dataframe con las variables comunes en el orden
var_com <- data.frame(N.g, N_censo.g)
var_com

#### Creando indicadoras para cada una de las variables-categorias
Xk <- censo_mrp %>% ungroup() %>% select(all_of(names_cov)) %>%
  fastDummies::dummy_cols(remove_selected_columns = TRUE) %>%
  select(all_of(names_xk))
Xk

#### verificando que los valores sean igual a los originales
colSums(Xk * censo_mrp$n)


# Iniciando el proceso de calibración -------------------------------------

## el peso de las ponderaciones para actualizar o acercar los valores
##                          censales hacia los valores de la encuesta
##      (El más desactualizado hacia el actualizado)

gk <- calib(
  Xs = Xk,
  d = censo_mrp$n,
  total = N.g,
  method = "logit"
) # linear primera opcion

### verificando que el proceso se haya realizado y este convergiendo

checkcalibration(Xs = Xk,
                 d = censo_mrp$n,
                 total = N.g,
                 g = gk)

##### Validación visual

hist(gk)
summary(gk)
length(table(gk))


##### n1 con pesos actualizados
n1 <- ceiling(censo_mrp$n * gk)
summary(n1)
summary(censo_mrp$n)

##### verificación visual

jpeg(filename = "GTM/2014-2015/3.NI/Output/plot_actualizacion_censo1.jpeg",
     width = 2000,
     height = 2000)
plot(censo_mrp$n, n1)
dev.off()

#### Verificando que los valores sean cercanos
sum(round(censo_mrp$n))
sum(n1)
sum(encuesta_mrp$fexp)

jpeg(filename = "GTM/2014-2015/3.NI/Output/plot_actualizacion_censo2.jpeg",
     width = 2000,
     height = 2000)
par(mfrow = c(2, 2))
hist(censo_mrp$n)
boxplot(censo_mrp$n)
hist(n1)
boxplot(n1)
dev.off()

## actualiza el censo 
censo_mrp$n <- n1


saveRDS(censo_mrp, "GTM/2014-2015/3.NI/Data/censo_mrp.rds")
saveRDS(censo_mrp, "GTM/2014-2015/4.D7/Data/censo_mrp.rds")

