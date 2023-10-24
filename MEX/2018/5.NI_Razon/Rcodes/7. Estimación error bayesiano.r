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
source("0Funciones/Agregado_razon.R", encoding = "UTF-8")
encuesta_mrp <- readRDS("MEX/2018/5.NI_Razon/Data/encuesta_mrp.rds")
censo_mrp    <- readRDS("MEX/2018/5.NI_Razon/Data/censo_mrp.rds")
Estima_NI    <- readRDS("MEX/2018/5.NI_Razon/Data/poststrat_df.RDS")
##### Cargando los fit de los tres indicadores D6/D6m y NI
fit_mrp_stan_D6  <- readRDS("MEX/2018/1.D6/Data/fit_bayes.rds")
fit_mrp_stan_NI  <- readRDS("MEX/2018/5.NI_Razon/Data/fit_bayes.rds")
##### Cargando tasa de desocupación
statelevel_predictors_df <- readRDS("MEX/2018/5.NI_Razon/Data/statelevel_predictors_df.rds") %>% 
  mutate_if(is.numeric,as.vector)
##############################################################################
# Expand state level predictors to the individual level

poststrat_df <- left_join(Estima_NI, statelevel_predictors_df,
                          by = "dam")

poststrat_df

# Posterior_epred returns the posterior estimates for the different subgroups stored in the
# poststrat_df dataframe.


### Ceando epredmat Para D6

epred_mat_D6 <- posterior_epred(fit_mrp_stan_D6, newdata = poststrat_df, 
                     type = "response", allow.new.levels = TRUE)

sum(is.na(epred_mat_D6))
sum(epred_mat_D6 < 0)

### Ceando epredmat Para NI


epred_mat_NI <- posterior_epred(fit_mrp_stan_NI, newdata = poststrat_df, 
                        type = "response", allow.new.levels = TRUE)

sum(is.na(epred_mat_NI))
sum(epred_mat_NI < 0)

######
## Construyendo epred_mat D7

epred_mat_NI_Razon<- ((epred_mat_NI/epred_mat_D6))

#### Error cuadratico medio

(mrp_estimate_D7 <-
    Aux_Agregado(poststrat = poststrat_df,
                 epredmat = epred_mat_NI_Razon,
                 byMap = NULL)
        )

Agregado_razon(epredmat_num = epred_mat_NI,
               epredmat_den = epred_mat_D6,
               poststrat = poststrat_df,
               byMap = NULL)

###############################

(mrp_estimate_D7 <-
    Aux_Agregado(poststrat = poststrat_df,
                 epredmat = epred_mat_NI_Razon,
                 byMap = "dam")
)

(mrp_estimate_D7_razon <- 
  Agregado_razon(epredmat_num = epred_mat_NI,
               epredmat_den = epred_mat_D6,
               poststrat = poststrat_df,
               byMap = "dam"))

####

poststrat_df %<>% mutate(yk_bench_num = Num*gk,
                         yk_bench_den = Den*gk)

tabla_est<-poststrat_df %>% 
  group_by(dam) %>% 
  summarise(Estimate_D7 = sum(n * yk_bench_num)/sum(yk_bench_den*n)) %>% 
  full_join(mrp_estimate_D7, by = "dam") %>% 
  mutate(mrp_cv = mrp_estimate_se/Estimate_D7*100) #%>% 
  #select(- mrp_estimate )

tabla_est_razon<-poststrat_df %>% 
  group_by(dam) %>% 
  summarise(Estimate_D7 = sum(n * yk_bench_num)/sum(yk_bench_den*n)) %>% 
  full_join(mrp_estimate_D7_razon, by = "dam") %>% 
  mutate(mrp_cv = mrp_estimate_se/Estimate_D7*100) #%>% 
#select(- mrp_estimate )


########################


openxlsx::write.xlsx(tabla_est, file = "MEX/2018/4.D7/Output/tablas_ECM.xlsx",
                     overwrite = TRUE)

openxlsx::write.xlsx(tabla_est_razon, file = "MEX/2018/4.D7/Output/tablas_ECM_razon.xlsx",
                     overwrite = TRUE)





