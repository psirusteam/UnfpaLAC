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
library(patchwork)
library(haven)
library(sampling)
library(DataExplorer)

####################################################
### Loading datasets                             ###
####################################################

##### Cargando encuesta y censo provenientes de NI
encuesta_mrp <- readRDS("COL/2015/3.NI/Data/encuesta_mrp.rds")
censo_mrp    <- readRDS("COL/2015/3.NI/Data/censo_mrp.rds")
##### Cargando los fit de los tres indicadores D6/D6m y NI
fit_mrp_logit_D6  <- readRDS("COL/2015/1.D6/Data/fit_mrp_logit.rds")
fit_mrp_logit_D6m <- readRDS("COL/2015/2.D6m/Data/fit_mrp_logit.rds")
fit_mrp_logit_NI  <- readRDS("COL/2015/3.NI/Data/fit_mrp_logit.rds")
##### Cargando tasa de desocupación
tasa_desocupados <- readRDS("COL/2015/1.D6/Data/tasa_desocupacion.rds")

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
                          by = "mpio")

poststrat_df

# Posterior_epred returns the posterior estimates for the different subgroups stored in the
# poststrat_df dataframe.


### Creando epredmat Para D6


epred_mat_D6 <- predict(fit_mrp_logit_D6, newdata = poststrat_df, 
                     type = "response", allow.new.levels = TRUE)

sum(is.na(epred_mat_D6))
sum(epred_mat_D6 < 0)

### Creando epredmat Para D6m


epred_mat_D6m <- predict(fit_mrp_logit_D6m, newdata = poststrat_df, 
                        type = "response", allow.new.levels = TRUE)

sum(is.na(epred_mat_D6m))
sum(epred_mat_D6m < 0)


### Creando epredmat Para NI


epred_mat_NI <- predict(fit_mrp_logit_NI, newdata = poststrat_df, 
                        type = "response", allow.new.levels = TRUE)

sum(is.na(epred_mat_NI))
sum(epred_mat_NI < 0)

######
## Construyendo epred_mat D7

epred_mat_D7<- (epred_mat_D6m/(epred_mat_D6 + epred_mat_NI))
summary(epred_mat_D7)

####

poststrat_df$epred_mat <- epred_mat_D7
names(poststrat_df)

poststrat_df %<>% mutate(pobreza = epred_mat_D7)

########################

saveRDS(poststrat_df %>%
          select(!matches("_\\d{,2}$")), 
        "COL/2015/4.D7/Data/poststrat_df.RDS")

saveRDS(encuesta_mrp, "COL/2015/4.D7/Data/encuesta_mrp.RDS")





