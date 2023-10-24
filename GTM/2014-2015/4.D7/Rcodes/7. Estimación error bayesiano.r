#########################################################
# Proyecto MRP - Leave no one behind                    #
# Mapas de pobreza CEPAL                                #
# Lectura y preparación de las bases de datos           #
# Autor: Gabriel Nieto & Andrés Gutiérrez               #
#########################################################

### Cleaning R environment ###

rm(list = ls())

#################
### Libraries ###
#################
memory.limit(500000)

library(tidyverse)
library(openxlsx)
library(foreign)
library(lme4)
library(formatR)
library(rstanarm)
library(patchwork)
library(haven)
library(sampling)
library(DataExplorer)

####################################################
### Loading datasets                             ###
####################################################

##### Cargando encuesta y censo provenientes de NI

source("0Funciones/funciones_mrp.R", encoding = "UTF-8")
encuesta_mrp <- readRDS("GTM/2014-2015/3.NI/Data/encuesta_mrp.rds")
censo_mrp    <- readRDS("GTM/2014-2015/3.NI/Data/censo_mrp.rds")
Estima_D7    <- readRDS("GTM/2014-2015/4.D7/Data/poststrat_df.RDS")
##### Cargando los fit de los tres indicadores D6/D6m y NI
fit_mrp_stan_D6  <- readRDS("GTM/2014-2015/1.D6/Data/fit_bayes.rds")
fit_mrp_stan_D6m <- readRDS("GTM/2014-2015/2.D6m/Data/fit_bayes.rds")
fit_mrp_stan_NI  <- readRDS("GTM/2014-2015/3.NI/Data/fit_bayes.rds")
##### Cargando tasa de desocupación
tasa_desocupados <- readRDS("GTM/2014-2015/1.D6/Data/tasa_desocupacion.rds")

##############################################################################


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
                          by = "depto")

poststrat_df

# Posterior_epred returns the posterior estimates for the different subgroups stored in the
# poststrat_df dataframe.


### Ceando epredmat Para D6


epred_mat_D6 <- posterior_epred(fit_mrp_stan_D6, newdata = poststrat_df, 
                     type = "response", allow.new.levels = TRUE)

sum(is.na(epred_mat_D6))
sum(epred_mat_D6 < 0)

### Ceando epredmat Para D6m


epred_mat_D6m <- posterior_epred(fit_mrp_stan_D6m, newdata = poststrat_df, 
                        type = "response", allow.new.levels = TRUE)

sum(is.na(epred_mat_D6m))
sum(epred_mat_D6m < 0)


### Ceando epredmat Para NI


epred_mat_NI <- posterior_epred(fit_mrp_stan_NI, newdata = poststrat_df, 
                        type = "response", allow.new.levels = TRUE)

sum(is.na(epred_mat_NI))
sum(epred_mat_NI < 0)

######
## Construyendo epred_mat D7

epred_mat_D7<- (epred_mat_D6m /(epred_mat_D6 + epred_mat_NI))

#### Error cuadratico medio

(mrp_estimate_D7 <-
    Aux_Agregado(poststrat = poststrat_df,
                 epredmat = epred_mat_D7,
                 byMap = "depto")
        )

####

tabla_est<-Estima_D7 %>% 
  group_by(depto) %>% 
  summarise(Estimate_D7 = sum(pobreza * n)/ sum(n)) %>% 
  full_join(mrp_estimate_D7, by = "depto") %>% 
  mutate(mrp_cv = mrp_estimate_se/Estimate_D7*100) #%>% 
  #select(- mrp_estimate )



########################


openxlsx::write.xlsx(tabla_est, file = "GTM/2014-2015/4.D7/Output/tablas_ECM.xlsx",
                     overwrite = TRUE)





