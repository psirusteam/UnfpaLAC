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
source("0Funciones/benchmarking_razon.r", encoding = "UTF-8")

# Loading data ------------------------------------------------------------

encuesta_mrp <-
  readRDS("MEX/2018/5.NI_Razon/Data/encuesta_mrp.rds") %>%
  select(-dam2) %>% mutate(area = as.character(area))
censo_mrp <-
  readRDS("MEX/2018/5.NI_Razon/Data/censo_mrp.rds") 
statelevel_predictors_df <-
  readRDS("MEX/2018/5.NI_Razon/Data/statelevel_predictors_df.rds") %>% 
  mutate_if(is.numeric, as.vector)
fit_NI <- readRDS("MEX/2018/5.NI_Razon/Data/fit_mrp_logit.rds")
fit_D6 <- readRDS("MEX/2018/1.D6/Data/fit_mrp_logit.rds")
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


epred_mat_NI <- predict(fit_NI, newdata = poststrat_df, 
                     type = "response", 
                     allow.new.levels = TRUE)

epred_mat_D6 <- predict(fit_D6, newdata = poststrat_df, 
                     type = "response", 
                     allow.new.levels = TRUE)

epred_mat <- epred_mat_NI/epred_mat_D6
sum(is.na(epred_mat))
sum(epred_mat < 0)

poststrat_df$epred_mat <- epred_mat
poststrat_df$Num <- epred_mat_NI
poststrat_df$Den <- epred_mat_D6
names(poststrat_df)


# Calculo de uso de métodos anticonceptivos.  -------------------------------------------

## definiendo uso de métodos como pobreza
encuesta_mrp %<>% mutate(Num = NI, Den = usametodo)
poststrat_df %<>% mutate(pobreza = epred_mat)

# definiendo diseno muestral

diseno <- encuesta_mrp %>%
  as_survey_design(ids = upm, weights = fexp)

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
#names_cov <- c("dam","area",     "unida", "edad", "anoest")  

gk_calib <- benchmarking_razon(poststrat_df,
                   encuesta_sta = encuesta_mrp,
                   var_bench = names_cov,
                   metodo = "logit")


jpeg(file = "MEX/2018/5.NI_Razon/Output/Plot_Bench_gk.jpeg")
hist(gk_calib$gk_bench,main = "gks")
dev.off()

poststrat_df$gk <- gk_calib$gk_bench

saveRDS(poststrat_df %>% select(byAgrega,pobreza, n,gk,Num,Den),
        "MEX/2018/5.NI_Razon/Data/poststrat_df.RDS")



