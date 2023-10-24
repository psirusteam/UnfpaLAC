#########################################################
# Proyecto MRP - Leave no one behind                    #
# Mapas de pobreza CEPAL                                #
# Lectura y preparación de las bases de datos           #
# Autor: Stalyn Guerrero & Andrés Gutiérrez             #
#########################################################
rm(list =ls())


# Loading required libraries ----------------------------------------------

library(rstan)
library(rstanarm)
library(data.table)
library(dplyr)
library(forcats)
library(tidyr)
library(reshape2)
library(stringr)
library(ggplot2)
library(scales)
library(bayesplot)
library(gridExtra)
library(ggalt)
library(usmap)
library(gridExtra)
library(scales)
library(kableExtra)
library(formatR)
library(patchwork)

theme_set(bayesplot::theme_default())
library(tidyverse)


# Loading data ------------------------------------------------------------
memory.limit(10000000000000)
encuesta_mrp <- readRDS("MEX/2018/1.D6/Data/encuesta_mrp.rds") %>% select(-dam2)
statelevel_predictors_df <- readRDS("MEX/2018/1.D6/Data/statelevel_predictors_df.rds")
fit <- readRDS("MEX/2018/1.D6/Data/fit_mrp_logit.rds")
fit_formula <- fit@call$formula

encuesta_df_agg <- fit@frame
y_res <- encuesta_df_agg$`cbind(pobres, nopobres)`
encuesta_df_agg$`cbind(pobres, nopobres)`<- NULL

encuesta_df_agg <- cbind(y_res, encuesta_df_agg)
# Bayesian Multilevel Modelling -------------------------------------------

#--- Fit in stan_glmer ---#
fit <- stan_glmer(formula = fit_formula,
  family = binomial(link = "logit"),         
                 data = encuesta_df_agg,
                  verbose = TRUE,
                 cores = 7,
                 chains = 4,
                 iter = 1000
  )

#fit<-readRDS("MEX/2018/1.D6/Data/fit_bayes.rds")

print(fit)

saveRDS(fit, file = "MEX/2018/1.D6/Data/fit_bayes.rds")


# Assessment of the model -------------------------------------------------
#shinystan::launch_shinystan(fit)
############################################################
## Comparando los medelos Freq y Bayes
############################################################
fit_freq <- readRDS("MEX/2018/2.D6m/Data/fit_mrp_logit.rds")
fit_Bayes <- readRDS("MEX/2018/2.D6m/Data/fit_bayes.rds")

# Graphical posterior predictive checks -----------------------------------
new_encuesta <- encuesta_mrp %>% inner_join(statelevel_predictors_df, by = "dam")
y_sam<- as.numeric(encuesta_mrp$usamoderno)
y_pred <- predict(fit_freq, type = "response", newdata = new_encuesta)
y_pred_B <- posterior_epred(fit_Bayes, newdata = new_encuesta)

rowsrandom <- sample(nrow(y_pred_B), 100)
y_pred2 <- y_pred_B[rowsrandom, ]

color_scheme_set("brightblue")
#
dim(y_pred2)
length(y_sam)
#ppc_dens_overlay(y = y, y_pred)
ppc_dens_overlay(y = as.numeric(y_sam), y_pred2)

plot1 <- ggplot(data.frame(datos = c(y_sam, y_pred, colMeans(y_pred_B)),
                           repe = gl(3, length(y_sam),
                                     labels = c("Muestra", "Freq", "Bayes"))),
                aes(x = datos, fill = repe, alpha = 0.1)) +
  geom_density()

ggsave(filename = "MEX/2018/1.D6/Output/Compara_modelos.jpeg", plot1, scale = 3)

