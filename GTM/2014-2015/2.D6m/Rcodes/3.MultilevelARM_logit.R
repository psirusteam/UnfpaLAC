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
theme_set(bayesplot::theme_default())


# Loading data ------------------------------------------------------------

encuesta_mrp <- readRDS("GTM/2014-2015/2.D6m/Data/encuesta_mrp.rds") 
#%>% mutate(pobreza = ifelse(ingreso <= lp, 1, 0))
tasa_desocupados <- readRDS("GTM/2014-2015/2.D6m/Data/tasa_desocupacion.rds")

#### creando la variable pobreza a partir de la variable de interés
#### usametodo

encuesta_mrp$pobreza <- encuesta_mrp$usamoderno

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

encuesta_df_agg <- encuesta_mrp %>%
           group_by_at(all_of(byAgrega)) %>%
              summarise(n = n(),
                pobres = sum(pobreza),
                nopobres = n - pobres, .groups = "drop") 

encuesta_df_agg<- encuesta_df_agg %>% 
                    inner_join(statelevel_predictors_df, 
                                by = "depto") 

#--- Fit ---#
fit <- glmer(
  cbind(pobres, nopobres) ~  (1 | depto) +
    (1 | edad) +
    (1 | area) +
    (1 | anoest) +
    (1 | etnia) +
    (1 | depto:area) +
    (1 | depto:etnia) +
   #(1 | depto:sexo) +
    (1 | depto:edad) +
    (1 | depto:anoest) +
    (1 | area:etnia) +
   #(1 | area:sexo) +
    #(1 | area:edad) +
    (1 | area:anoest) +
   #(1 | etnia:sexo) +
    (1 | etnia:edad) +
    (1 | etnia:anoest) +
   #(1 | sexo:edad) +
   #(1 | sexo:anoest) +
    (1 | edad:anoest) +
   #(1 | discapacidad) +
    tasa_desocupacion +
    stable_lights + 
    crops.coverfraction +
    urban.coverfraction ,
  data = encuesta_df_agg,
  control = glmerControl(tolPwrss=1e-3),
  family = binomial(link = "logit")
)
#fit<-readRDS("GTM/2014-2015/2.D6m/Data/fit_mrp_logit.rds")

### estos dos valores deben ser iguales
sum(predict(fit, type = "response") * encuesta_df_agg$n)
sum(encuesta_df_agg$pobres)

print(fit)
#--- Exporting Bayesian Multilevel Model Results ---#

saveRDS(fit, file = "GTM/2014-2015/2.D6m/Data/fit_mrp_logit.rds")




