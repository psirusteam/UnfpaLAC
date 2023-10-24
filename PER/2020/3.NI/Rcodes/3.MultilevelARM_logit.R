#########################################################
# Proyecto MRP - Leave no one behind                    #
# Mapas de pobreza CEPAL                                #
# Lectura y preparación de las bases de datos           #
# Autor: Stalyn Guerrero & Andrés Gutiérrez             #
#########################################################

rm(list =ls())

# Loading required libraries ----------------------------------------------

library(lme4)
library(tidyverse)
library(formatR)
library(patchwork)
library(dplyr)
theme_set(bayesplot::theme_default())


# Loading data ------------------------------------------------------------

encuesta_mrp <- readRDS("PER/2020/3.NI/Data/encuesta_mrp.rds") 
#%>% mutate(pobreza = ifelse(ingreso <= lp, 1, 0))
tasa_desocupados <- readRDS("PER/2020/3.NI/Data/tasa_desocupacion.rds")

#### creando la variable pobreza a partir de la variable de interés
#### usametodo

encuesta_mrp$pobreza <- encuesta_mrp$necesInst


#--- Expand state-level predictors to the individual level ---#
statelevel_predictors_df <- tasa_desocupados

byAgrega <-
  grep(
    pattern =  "^(n|usametodo|usamoderno|necesInst|
              |upm|estrato|fexp)",
    x = names(encuesta_mrp),
    invert = TRUE,
    value = TRUE
  )

byAgrega

encuesta_df_agg <-
  encuesta_mrp %>%
  group_by_at(all_of(byAgrega)) %>%
  summarise(n = n(),
            pobres = sum(pobreza),
            nopobres = n - pobres, .groups = "drop") 

encuesta_df_agg

encuesta_df_agg<- encuesta_df_agg %>% 
                  inner_join(statelevel_predictors_df, 
                      by = "mpio") 

#--- Fit ---#
fit <- glmer(
  cbind(pobres, nopobres) ~  (1 | mpio) +
    (1 | edad) +
    (1 | area) +
    (1 | anoest) +
    (1 | etnia) +
    (1 | mpio:area) +
    (1 | mpio:etnia) +
    #(1 | mpio:sexo) +
    (1 | mpio:edad) +
    (1 | mpio:anoest) +
    #(1 | area:etnia) +
    #(1 | area:sexo) +
    (1 | area:edad) +
    (1 | area:anoest) +
    #(1 | etnia:sexo) +
    (1 | etnia:edad) +
    (1 | etnia:anoest) +
    #(1 | sexo:edad) +
    #(1 | sexo:anoest) +
    (1 | edad:anoest) +
    (1 | discapacidad) +
      tasa_desocupacion +
      stable_lights + 
  crops.coverfraction +
  urban.coverfraction +
    unida,
              data = encuesta_df_agg,
  family = binomial(link = "logit")
)

#fit<-readRDS("PER/2020/3.NI/Data/fit_mrp_logit.rds")

sum(predict(fit, type = "response") * encuesta_df_agg$n)
sum(encuesta_df_agg$pobres)

print(fit)
#--- Exporting Bayesian Multilevel Model Results ---#

saveRDS(fit, file = "PER/2020/3.NI/Data/fit_mrp_logit.rds")


