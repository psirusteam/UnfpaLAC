#########################################################
# Proyecto MRP - Leave no one behind                    #
# Mapas de pobreza CEPAL                                #
# Lectura y preparación de las bases de datos           #
# Autor: Stalyn Guerrero & Andrés Gutiérrez             #
#########################################################
rm(list =ls())

cat("\f")
# Loading required libraries ----------------------------------------------

library(rstan)
library(rstanarm)
library(data.table)
library(dplyr)
library(magrittr)
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

source("0Funciones/funciones_mrp.R", encoding = "UTF-8")

# Loading data ------------------------------------------------------------

encuesta_mrp <- readRDS("MEX/2018/3.NI/Data/encuesta_mrp.rds")
poststrat_df <- readRDS("MEX/2018/3.NI/Data/poststrat_df.RDS")
statelevel_predictors_df <- readRDS("MEX/2018/3.NI/Data/statelevel_predictors_df.rds") %>% 
  mutate_if(is.numeric, as.vector)
fit <- readRDS("MEX/2018/3.NI/Data/fit_bayes.rds")

# Poststratification at the National Level --------------------------------

byAgrega <-
  grep(
    pattern =  "^(n|X|F|pobreza|tasa_desocupacion|epred_mat|gk|mpio|lp|stable_lights|crops.coverfraction|urban.coverfraction)",
    x = names(poststrat_df),
    invert = TRUE,
    value = TRUE
  )

byAgrega

# Posterior_epred returns the posterior estimates for the different subgroups stored in the
# poststrat_df dataframe.
poststrat_df <- full_join(poststrat_df, statelevel_predictors_df)
epred_mat <- posterior_epred(fit, newdata = poststrat_df, type = "responde")

## validacion de los valores posteriores
summary(rowMeans(epred_mat))
summary(colMeans(epred_mat))

summary(as.numeric(epred_mat))
hist(as.numeric(epred_mat))

length(epred_mat[which(epred_mat == 0)])

# Resultados nacionales ---------------------------------------------------
(mrp_estimate_Ingresolp <-
  Aux_Agregado(poststrat = poststrat_df,
             epredmat = epred_mat,
             byMap = NULL)
)

Aux_Agregado(poststrat = poststrat_df,
                 epredmat = epred_mat,
                 byMap = "unida")

# Poststratification at the State level -----------------------------------

byAgrega <-
  grep(
    pattern = "dam",
    x = byAgrega,
    value = TRUE,
    invert = TRUE
  )

byAgrega <- t(combn(byAgrega, 2))
byAgrega <- rbind(c("dam","dam" ), byAgrega)
# resultados para ingreso medio

mrp_estimate = map(1:nrow(byAgrega),
             ~poststrat_df %>% 
               group_by_at(vars("dam", byAgrega[.x,])) %>%
               summarise(Benchmarking_estimate = sum(n * pobreza2) / sum(n), .groups = "drop") %>% 
               ungroup())


mrp_ECM <- map(1:nrow(byAgrega), function(i) {
     Aux_Agregado(poststrat_df,
                 epredmat = epred_mat,
                 byMap = c("dam", byAgrega[i, ])) 

  })

tablas <- map2(mrp_estimate, mrp_ECM, inner_join) %>% 
  map(~.x %>%
        mutate(mrp_cv = mrp_estimate_se/Benchmarking_estimate*100))

nom_tabs <- c("dam", apply(byAgrega[-1,],MARGIN = 1,paste0, collapse = "_"))
names(tablas) <- nom_tabs


tablas %>% map_df(~.x %>% summarise(max = max(mrp_cv)), .id = "agregado")
tablas %>% map(~.x %>% filter(mrp_cv > 50))


openxlsx::write.xlsx(tablas, file = "MEX/2018/3.NI/Output/tablas_ECM.xlsx", 
                     overwrite = TRUE)
