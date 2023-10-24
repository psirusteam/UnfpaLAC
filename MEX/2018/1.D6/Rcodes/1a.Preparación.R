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
encuesta <- readRDS("MEX/2018/1.D6/Data/ENADID_MUJER.rds")
censo_mrp <- readRDS("MEX/2018/1.D6/Data/censo_mrp.rds")


##############################
### Exploratory statistics ###
##############################
## El depto esta unido al jefe del hogar

Texp <- encuesta %>%
  group_by(dam) %>%
  summarise(
    "D6_n"    = mean(usametodo, na.rm = T),
    "D6m_n"   = mean(usamoderno, na.rm = T),
    n = n(),
    Nhat = sum(`fexp`),
    .groups = "drop"
  ) %>%
  mutate(N = sum(n))

Texp


#  ENSANUT: Buscando NA's en las variables de disponibles -----------------

table(encuesta$dam2, useNA = "always")
labelled::generate_dictionary(encuesta %>% select(dam2))

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

###### Eliminando sexo y edad

censo_mrp <- censo_mrp %>% filter(sexo != 1)

table(censo_mrp$edad, useNA = "a")

censo_mrp <- censo_mrp %>% filter(edad %in% c(2:5))

table(censo_mrp$sexo, useNA = "a")
table(censo_mrp$edad, useNA = "a")

##### Elimando categoria 98 No aplica

censo_mrp <- censo_mrp %>% filter(anoest != 98)
censo_mrp <- censo_mrp %>% filter(anoest != 99)

table(censo_mrp$anoest, useNA = "a")

### edad,
### Años de estudio,
### Etnia

### No se incluyen las variables estrato y upm  ya que no necesitan ser estandarazidos

encuesta_mrp <- encuesta %>%
  transmute(
   dam,
   dam2,
  usametodo ,
  usamoderno,
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

encuesta_mrp <- encuesta_mrp %>%  filter(edad %in% c(2:5))

encuesta_mrp <- encuesta_mrp %>% filter(anoest!="99")


table(encuesta_mrp$edad, useNA = "a")

# validación visual

plot_intro(encuesta_mrp)
plot_missing(encuesta_mrp)
plot_histogram(encuesta_mrp)
plot_bar(encuesta_mrp, with = "fexp")

saveRDS(encuesta_mrp, file = "MEX/2018/1.D6/Data/encuesta_mrp.rds")
saveRDS(encuesta_mrp, file = "MEX/2018/2.D6m/Data/encuesta_mrp.rds")

# Actualización de tabla censal- IPFP -------------------------------------

names_cov <-
  grep(
    pattern =  "^(n|usametodo|usamoderno|
              |upm|estrato|fexp)",
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

##### Años de estudio

encuesta_mrp %>% group_by(anoest) %>%
  summarise(n = sum(fexp),
            n1 = n(),
            .groups = "drop") %>%
  mutate(prop = n1 / sum(n1))

censo_mrp %>% group_by(anoest) %>%
  summarise(n = sum(n),
            .groups = "drop") %>%
  mutate(prop = n / sum(n))


### Edad

encuesta_mrp %>% group_by(edad) %>%
  summarise(n = sum(fexp),
            n1 = n(),
            .groups = "drop") %>%
  mutate(prop = n1 / sum(n1))

censo_mrp %>% group_by(edad) %>%
  summarise(n = sum(n),
            .groups = "drop") %>%
  mutate(prop = n / sum(n))


### Area

encuesta_mrp %>% group_by(area) %>%
  summarise(n = sum(fexp),
            n1 = n(),
            .groups = "drop") %>%
  mutate(prop = n1 / sum(n1))

censo_mrp %>% group_by(area) %>%
  summarise(n = sum(n),
            .groups = "drop") %>%
  mutate(prop = n / sum(n))


### Etnia

encuesta_mrp %>% group_by(etnia) %>%
  summarise(n = sum(fexp),
            n1 = n(),
            .groups = "drop") %>%
  mutate(prop = n1 / sum(n1))

censo_mrp %>% group_by(etnia) %>%
  summarise(n = sum(n),
            .groups = "drop") %>%
  mutate(prop = n / sum(n))

### Discapacidad

encuesta_mrp %>% group_by(discapacidad) %>%
  summarise(n = sum(fexp),
            n1 = n(),
            .groups = "drop") %>%
  mutate(prop = n1 / sum(n1))

censo_mrp %>% group_by(discapacidad) %>%
  summarise(n = sum(n),
            .groups = "drop") %>%
  mutate(prop = n / sum(n))



### unida

encuesta_mrp %>% group_by(unida) %>%
  summarise(n = sum(fexp),
            n1 = n(),
            .groups = "drop") %>%
  mutate(prop = n1 / sum(n1))

censo_mrp %>% group_by(unida) %>%
  summarise(n = sum(n),
            .groups = "drop") %>%
  mutate(prop = n / sum(n))

###################################



auxSuma <- function(dat, col, ni) {
  dat %>% ungroup() %>% select(all_of(col))  %>%
    fastDummies::dummy_cols(remove_selected_columns = TRUE) %>%
    mutate_all( ~ . * ni) %>% colSums()
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

##### n1 con pesos actualizados
n1 <- ceiling(censo_mrp$n * gk)
summary(n1)
summary(censo_mrp$n)

##### verificación visual

jpeg(filename = "MEX/2018/1.D6/Output/plot_actualizacion_censo1.jpeg",
     width = 2000,
     height = 2000)
plot(censo_mrp$n, n1)
dev.off()

#### Verificando que los valores sean cercanos
sum(round(censo_mrp$n))
sum(n1)
sum(encuesta_mrp$fexp)

jpeg(filename = "MEX/2018/1.D6/Output/plot_actualizacion_censo2.jpeg",
     width = 2000,
     height = 2000)
par(mfrow = c(2, 2))
hist(censo_mrp$n)
boxplot(censo_mrp$n)
hist(n1)
boxplot(n1)
dev.off()

censo_mrp$n <- n1

saveRDS(censo_mrp, "MEX/2018/1.D6/Data/censo_mrp.rds")
saveRDS(censo_mrp, "MEX/2018/2.D6m/Data/censo_mrp.rds")
saveRDS(censo_mrp, "MEX/2018/3.NI/Data/censo_mrp.rds")


