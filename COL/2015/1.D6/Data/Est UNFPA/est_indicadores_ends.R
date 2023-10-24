## Estimacion directa indicadores ENDS
## May 5 2021
## Autora: Juliana Guerrero Velasquez
## Consultoria UNFPA
## Calculos estimación directa de indicadores DHS

rm(list=ls())
options(encoding = "UTF-8", scipen=999)

## Paquetes
library(tidyverse)

## directorio
setwd("V:/DAT/SAECEPAL/SAE-unfpa/UnfpaLACgithub/COL/2015/1.D6/Data/Est UNFPA")

## Carga individual records
data_COIR71DT <- readRDS("COIR71FL.rds") # archivo con todas las mujeres 13-49
                                         # filtrar si se quiere 15-49


###############################################################################
## unmet need  - Necesidad Insatisfecha
###############################################################################
# Unmet need for contraception - Volumen total
# for spacing
sum(data_COIR71DT[data_COIR71DT$v626a %in% c(1),'pesos'])
# for limiting
sum(data_COIR71DT[data_COIR71DT$v626a %in% c(2),'pesos'])
# total unmet need 
sum(data_COIR71DT[data_COIR71DT$v626a %in% c(1,2),'pesos'])

## calculo de estimación directa indicador necesidad insatisfecha

# % unmet need for currently married women - NI
data_COIR71DT %>% filter(v502==1) %>% # mujeres unidas
  mutate(unmet=ifelse(v626a %in% c(1,2),v005,0)) %>% 
  summarise(per_unmet=100*sum(unmet)/sum(v005))

# % unmet need for sexually active women - NI
data_COIR71DT %>% filter(v502!=1 & v528 %in% c(0:30)) %>% 
  mutate(unmet=ifelse(v626a %in% c(1,2),v005,0)) %>% 
  summarise(ind_unmet=100*sum(unmet)/sum(v005))


# zona V025 urbano rural
#  % unmet need for currently married women 
data_COIR71DT %>% filter(v502==1) %>%
  mutate(unmet=ifelse(v626a %in% c(1,2),v005,0)) %>% 
  group_by(v025) %>%
  summarise(per_unmet_zona=100*sum(unmet)/sum(v005),n_unmet_zona=sum(unmet))


# dpto sdepto
#  % unmet need for currently married women 
data_COIR71DT %>% filter(v502==1) %>% 
  mutate(unmet=ifelse(v626a %in% c(1,2),v005,0)) %>% 
  group_by(sdepto) %>%
  summarise(per_unmet_dpto=100*sum(unmet)/sum(v005),n_unmet_dpto=sum(unmet))



###############################################################################
# uso metodos D6
###############################################################################
data_COIR71DT %>% filter(v502==1) %>% # mujeres unidas
  mutate(uso=ifelse(v313 %in% c(1,2,3),v005,0)) %>%  # actualmente usa algún método
  summarise(per_uso=100*sum(uso)/sum(v005)) # 80.9

###############################################################################
# uso metodo moderno D6M
###############################################################################
data_COIR71DT %>% filter(v502==1) %>% # mujeres unidas
  mutate(uso=ifelse(v313 %in% c(3),v005,0)) %>%  # actualmente usa algún método moderno
  summarise(per_uso=100*sum(uso)/sum(v005)) # 75.9

###############################################################################
# demanda de metodos anticonceptivos
###############################################################################
data_COIR71DT %>% filter(v502==1) %>% # mujeres unidas
  mutate(uso=ifelse(v626a %in% c(1,2,3,4),v005,0)) %>%  # demanda: unmet & met need
  summarise(per_uso=100*sum(uso)/sum(v005)) # 87.6

###############################################################################
# porcentaje demanda satisfecha por método moderno D7
###############################################################################
data_COIR71DT %>% 
  filter(v502==1 & v626a %in% c(1,2,3,4)) %>% # mujeres unidas y mujeres en demanda
  mutate(uso=ifelse(v313 %in% c(3),v005,0)) %>%  # mujeres método moderno
  summarise(per_uso=100*sum(uso)/sum(v005)) # 86.6



