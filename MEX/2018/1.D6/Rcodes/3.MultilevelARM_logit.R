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
library(magrittr)
theme_set(bayesplot::theme_default())


# Loading data ------------------------------------------------------------

encuesta_mrp <- readRDS("MEX/2018/1.D6/Data/encuesta_mrp.rds") %>% 
  select(-dam2) 
statelevel_predictors_df <- 
  readRDS("MEX/2018/1.D6/Data/statelevel_predictors_df.rds") %>% 
  mutate_if(is.numeric, as.vector)
#### creando la variable pobreza a partir de la variable de interés
#### usametodo

encuesta_mrp$pobreza <- encuesta_mrp$usametodo

byAgrega <-
  grep(
    pattern =  "^(n|usametodo|usamoderno|pobreza|
              |upm|estrato|fexp)",
    x = names(encuesta_mrp),
    invert = TRUE,
    value = TRUE
  )

encuesta_df_agg <-
  encuesta_mrp %>%
  group_by_at(all_of(byAgrega)) %>%
  summarise(n = n(),
            pobres = sum(pobreza),
            nopobres = n - pobres, .groups = "drop") 

encuesta_df_agg %<>% inner_join(statelevel_predictors_df) 
names(encuesta_df_agg)
sum(complete.cases(encuesta_df_agg))

#--- Fit ---#
fit <- glmer(
  cbind(pobres, nopobres) ~  (1 | dam) +
    (1 | edad) +
    (1 | area) +
    (1 | anoest) +
    (1 | etnia) +
    (1 | dam:area) +
    (1 | dam:etnia) +
    #(1 | dam:sexo) +
    (1 | dam:edad) +
    (1 | dam:anoest) +
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
    luces_nocturnas + 
    cubrimiento_rural +
    cubrimiento_urbano +
    accesibilidad_hospitales +
    tiene_electricidad+
    rezago_escolar+
    alfabeta+
    material_techo+
    unida,
   data = encuesta_df_agg,
  family = binomial(link = "logit")
)


#### estas sumas deben ser exactas entre si
sum(predict(fit, type = "response") * encuesta_df_agg$n)
sum(encuesta_df_agg$pobres)

print(fit)
#--- Exporting Bayesian Multilevel Model Results ---#

saveRDS(fit, file = "MEX/2018/1.D6/Data/fit_mrp_logit.rds")
fit <- readRDS( file = "MEX/2018/1.D6/Data/fit_mrp_logit.rds")
