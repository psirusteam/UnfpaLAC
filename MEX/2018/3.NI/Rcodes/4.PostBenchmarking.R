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

encuesta_mrp <-
  readRDS("MEX/2018/3.NI/Data/encuesta_mrp.rds") %>%
  select(-dam2) %>% mutate(area = as.character(area))
censo_mrp <-
  readRDS("MEX/2018/3.NI/Data/censo_mrp.rds") 
statelevel_predictors_df <-
  readRDS("MEX/2018/3.NI/Data/statelevel_predictors_df.rds") %>% 
  mutate_if(is.numeric, as.vector)
fit <- readRDS("MEX/2018/3.NI/Data/fit_mrp_logit.rds")

### Names-----------------------------------------------------------------

names(encuesta_mrp)
names(censo_mrp)
names(encuesta_mrp)

encuesta_mrp[,dplyr::intersect(names(encuesta_mrp),
                 names(censo_mrp))] %>% 
  lapply(unique)

censo_mrp[,dplyr::intersect(names(encuesta_mrp),
                               names(censo_mrp))] %>% 
  lapply(unique)

byAgrega <-
  grep(
    pattern =  "^(n|usametodo|usamoderno|necesInst|sexo|
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
                          by = "dam")

poststrat_df

# Posterior_epred returns the posterior estimates for the different subgroups stored in the
# poststrat_df dataframe.

print(fit)

epred_mat <- predict(fit, newdata = poststrat_df, 
                     type = "response", 
                     allow.new.levels = TRUE)

sum(is.na(epred_mat))
sum(epred_mat < 0)

poststrat_df$epred_mat <- epred_mat
names(poststrat_df)


# Calculo de uso de métodos anticonceptivos.  -------------------------------------------

## definiendo uso de métodos como pobreza
encuesta_mrp %<>% mutate(pobreza = NI)
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
#names_cov <- c("area", "edad", "etnia")

### creando indicadoras para cada variable-categoría

Xk <- poststrat_df %>% select(all_of(names_cov)) %>%
  fastDummies::dummy_cols(select_columns = names_cov,
                          remove_selected_columns = TRUE)
Xk

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
Xk %<>% 
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


poststrat_df$gk <- calib(Xs = Xk, 
                         d = poststrat_df$n,
                         total = unlist(paso["t_pobreza",]),
                         method="logit") 

checkcalibration(Xs = Xk, 
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

jpeg(file = "MEX/2018/3.NI/Output/Plot_Bench_Pobreza.jpeg")
hist(poststrat_df$gk)
dev.off()

saveRDS(poststrat_df %>% select(byAgrega,pobreza,pobreza2, n,gk,epred_mat),
        "MEX/2018/3.NI/Data/poststrat_df.RDS")



