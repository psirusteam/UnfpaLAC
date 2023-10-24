#########################################################
# Proyecto MRP - Leave no one behind                    #
# Mapas de pobreza CEPAL                                #
# Lectura y preparación de las bases de datos           #
# Autor: Stalyn Guerrero  Andrés Gutiérrez              #
#########################################################

rm(list = ls())

# Loading required libraries ----------------------------------------------

library(tidyverse)
library(reshape2)
library(stringr)
library(ggalt)
library(gridExtra)
library(scales)
library(formatR)
library(patchwork)

theme_set(bayesplot::theme_default())

source(file = "0Funciones/funciones2_mrp.R", encoding = "UTF-8")
# Loading data ------------------------------------------------------------

encuesta_mrp <- readRDS("MEX/2018/3.NI/Data/encuesta_mrp.rds")
censo_mrp <- readRDS("MEX/2018/3.NI/Data/censo_mrp.rds")

#### creando la variable pobreza a partir de la variable de interés
#### usametodo

encuesta_mrp$pobreza <- encuesta_mrp$NI


# Exploratory data analysis -----------------------------------------------

theme_set(theme_bw())

### AGE ###
age_plot <-
  Plot_Compare(dat_censo = censo_mrp,
               dat_encuesta = encuesta_mrp,
               by = "edad")
### etnia ###
etnia_plot <-
  Plot_Compare(dat_censo = censo_mrp,
               dat_encuesta = encuesta_mrp,
               by = "etnia")

### Level of schooling (LoS) ###
escolar_plot <-
  Plot_Compare(dat_censo = censo_mrp,
               dat_encuesta = encuesta_mrp,
               by = "anoest")

### Area ###
area_plot <-
  Plot_Compare(dat_censo = censo_mrp,
               dat_encuesta = encuesta_mrp %>% mutate(area = as.character(area)),
               by = "area")

### States ###
mpio_plot <-
  Plot_Compare(dat_censo = censo_mrp,
               dat_encuesta = encuesta_mrp,
               by = "dam")

#--- Patchwork in action ---#
(age_plot | etnia_plot | escolar_plot | area_plot) / ( mpio_plot)


### States ###
mpio_etnia <-
  Plot_Compare(dat_censo = censo_mrp,
               dat_encuesta = encuesta_mrp,
               by = "etnia")


# Interaction effects  ----------------------------------------------------

theme_set(theme_bw())

### Edad y cruzar con las demás variables


p_edad_etnia <-
  plot_interaction(dat_encuesta = encuesta_mrp,
                   by = "edad",
                   by2 = "etnia")


p_edad_escolar <-
  plot_interaction(dat_encuesta = encuesta_mrp,
                   by = "edad",
                   by2 = "anoest")

p_edad_area <-
  plot_interaction(dat_encuesta = encuesta_mrp,
                   by = "edad",
                   by2 = "area")


p_edad_mpio <-
  plot_interaction(dat_encuesta = encuesta_mrp,
                   by = "edad",
                   by2 = "dam")

#--- Patchwork in action ---#
(p_edad_etnia + p_edad_escolar + p_edad_area ) 

###### Etnia 


p_etnia_escolar <-
  plot_interaction(dat_encuesta = encuesta_mrp,
                   by = "etnia",
                   by2 = "anoest")

p_etnia_area <-
  plot_interaction(dat_encuesta = encuesta_mrp,
                   by = "etnia",
                   by2 = "area")


p_etnia_mpio <-
  plot_interaction(dat_encuesta = encuesta_mrp %>% mutate(mpio = dam),
                   by = "etnia",
                   by2 = "mpio")

#--- Patchwork in action ---#
(p_etnia_escolar + p_etnia_area ) / p_etnia_mpio



###### Escolaridad 


p_escolar_area <-
  plot_interaction(dat_encuesta = encuesta_mrp,
                   by = "anoest",
                   by2 = "area")


p_escolar_mpio <-
  plot_interaction(dat_encuesta = encuesta_mrp %>% mutate(mpio = dam),
                   by = "anoest",
                   by2 = "mpio")

#--- Patchwork in action ---#
(p_escolar_area ) / p_escolar_mpio



###### Area 


p_area_mpio <-
  plot_interaction(dat_encuesta = encuesta_mrp %>% mutate(mpio = dam),
                   by = "area",
                   by2 = "mpio")

p_area_mpio

