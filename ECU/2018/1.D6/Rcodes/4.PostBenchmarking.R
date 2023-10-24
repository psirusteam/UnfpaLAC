#########################################################
# Proyecto MRP - Leave no one behind                    #
# Mapas de pobreza CEPAL                                #
# Lectura y preparación de las bases de datos           #
# Autor: Stalyn Guerrero & Andrés Gutiérrez             #
#########################################################

rm(list =ls())
cat("\f")
# Loading required libraries ----------------------------------------------

library(scales)
library(patchwork)
library(srvyr)
library(survey)
library(haven)
library(sampling)
theme_set(bayesplot::theme_default())
library(tidyverse)

source("0Funciones/funciones_mrp.R", encoding = "UTF-8")

# Loading data ------------------------------------------------------------
encuesta_mrp <- readRDS("ECU/2018/1.D6/Data/encuesta_mrp.rds")
censo_mrp <- readRDS("ECU/2018/1.D6/Data/censo_mrp.rds")
tasa_desocupados <- readRDS("ECU/2018/1.D6/Data/tasa_desocupacion.rds")
fit <- readRDS("ECU/2018/1.D6/Data/fit_mrp_logit.rds")


# Poststratification at the National Level --------------------------------

statelevel_predictors_df <- tasa_desocupados


byAgrega <-
  grep(
    pattern =  "^(n|usametodo|usamoderno|necesInst|
              |upm|estrato|fexp)",
    x = names(censo_mrp),
    invert = TRUE,
    value = TRUE
  )

byAgrega

####agregando individuos por caracteristicas similares

poststrat_df <- censo_mrp %>%  
            group_by_at(byAgrega) %>%
            summarise(n = sum(n), .groups = "drop")

poststrat_df

# Expand state level predictors to the individual level

poststrat_df <- left_join(poststrat_df, statelevel_predictors_df,
                          by = "mpio")

poststrat_df

# Posterior_epred returns the posterior estimates for the different subgroups stored in the
# poststrat_df dataframe.


epred_mat <- predict(fit, newdata = poststrat_df, 
                     type = "response", allow.new.levels = TRUE)

sum(is.na(epred_mat))
sum(epred_mat < 0)

poststrat_df$epred_mat <- epred_mat
names(poststrat_df)


# Calculo de uso de métodos anticonceptivos.  -------------------------------------------

## definiendo uso de métodos como pobreza
encuesta_mrp %<>% mutate(pobreza = usametodo)
poststrat_df %<>% mutate(pobreza = epred_mat)

# definiendo diseno muestral

diseno <- encuesta_mrp %>%
  as_survey_design(weights = fexp)

###########################################
###########################################
###           Benchmarking              ###
###     (Gutiérrez - Guerrero, 2022)    ###
###########################################
###########################################

####Validación de nombres y  categorías en censo y encuesta

names_cov <-
  grep(
    pattern =  "^(n|usametodo|usamoderno|necesInst|
              |upm|estrato|fexp)",
    x = names(censo_mrp),
    invert = TRUE,
    value = TRUE
  )

names_cov

names_cov <- names_cov[names_cov %in% names(encuesta_mrp)]
names_cov

num_cat_censo <- apply(poststrat_df[names_cov], MARGIN = 2, function(x)
  length(unique(x)))
num_cat_censo

num_cat_sample <- apply(encuesta_mrp[names_cov], MARGIN = 2, function(x)
  length(unique(x)))
num_cat_sample

names_cov <- names_cov[num_cat_censo==num_cat_sample]
names_cov
names_cov <- c("area", "edad", "etnia")

### creando indicadoras para cada variable-categoría

poststrat_df %<>%
  fastDummies::dummy_cols(select_columns = names_cov,
                          remove_selected_columns = FALSE)
poststrat_df

#### realizando estimaciones 

estimaciones <-
  map(names_cov ,~ poststrat_df %>% 
        group_by_at(all_of(.x)) %>%
        summarise(
          medias = weighted.mean(epred_mat, n),
          Nhat = sum(n),
          t_pobreza = sum(epred_mat *
                            n)
        ))

estimaciones


####### ????
poststrat_df %<>% 
       mutate_at(vars(matches("\\d$")) ,~.*poststrat_df$epred_mat)


### total
totales <- map(names_cov, ~encuesta_mrp %>% group_by_at(all_of(.x)) %>% 
                 summarise(Nhat = sum(fexp),
                           t_pobreza = sum(pobreza*fexp),
                           medias = weighted.mean(pobreza,fexp)))

totales


paso <- sapply(names_cov, function(byi){
    encuesta_mrp %>% 
    group_by_at(all_of(byi)) %>% 
    summarise(Nhat = sum(fexp),
              t_pobreza = sum(pobreza*fexp),
              medias = weighted.mean(pobreza,fexp))
})

unlist(paso["t_pobreza",])


poststrat_df$gk <- calib(Xs = poststrat_df %>% 
                         select(matches("\\d$")), 
                         d = poststrat_df$n,
                         total = unlist(paso["t_pobreza",]),
                         method="logit") 

checkcalibration(Xs = poststrat_df %>% 
                 select(matches("\\d$")), 
                 d = poststrat_df$n,
                 total = unlist(paso["t_pobreza",]),
                 g = poststrat_df$gk)

summary(poststrat_df$gk)
hist(poststrat_df$gk)


map(names_cov ,~ poststrat_df %>% 
      group_by_at(all_of(.x)) %>%
      summarise(
      Nhat = sum(n),
      t_pobreza = sum(epred_mat *n*gk)) %>% 
      mutate(medias = t_pobreza/Nhat))

###
poststrat_df %<>%
  mutate(pobreza2 = epred_mat *gk,
         pobreza2 = ifelse(pobreza2>1, 1, pobreza2),
         pobreza2 = ifelse(pobreza2<0, 0, pobreza2)) 


temp <- map(names_cov ,~ poststrat_df %>% 
              group_by_at(all_of(.x)) %>%
              summarise(
              Nhat = sum(n),
              t_pobreza = sum(n*pobreza2)) %>% 
              mutate(medias = t_pobreza/Nhat)) 




# totales[[5]]
# temp[[5]]
# estimaciones[[5]]
# 
# totales[[4]]
# temp[[4]]
# estimaciones[[4]]
# 
totales[[3]]
temp[[3]]
estimaciones[[3]]

totales[[2]]
temp[[2]]
estimaciones[[2]]

totales[[1]]
temp[[1]]
estimaciones[[1]]

jpeg(file = "ECU/2018/1.D6/Output/Plot_Bench_Pobreza.jpeg")
hist(poststrat_df$gk)
dev.off()

saveRDS(poststrat_df %>%
        select(!matches("_\\d{,2}$")), 
        "ECU/2018/1.D6/Data/poststrat_df.RDS")

saveRDS(encuesta_mrp, "ECU/2018/1.D6/Data/encuesta_mrp.RDS")


