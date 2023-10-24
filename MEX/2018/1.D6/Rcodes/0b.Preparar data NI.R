################################################################################
################################################################################
# # 
# # Process: Construcción Base ENADID Completa (Todos los modulos)
# #
# # 
# # File history:
# #   Creation : 12/04/2023
# #
# #
# # Creation: Stalyn Guerrero. 
# #  
# # 
# # Modification:   
# # https://www.inegi.org.mx/rnm/index.php/catalog/554/data-dictionary
################################################################################
################################################################################
###                             Limpiando la memoria 
################################################################################

rm(list = ls(all=T))
gc()

################################################################################
##        Verificando librerias e instalando las faltantes  
################################################################################

paquetes <-
  c(
    "readr",
    "readxl",
    "faraway",
    "wesanderson",
    "dplyr",
    "factoextra",
    "corrplot",
    "reshape2",
    "reshape",
    "PerformanceAnalytics",
    "foreign",
    "plotly",
    "FactoMineR",
    "homals",
    "nnet",
    "tidyverse",
    "gapminder"
  )
has <- paquetes %in% rownames(installed.packages())
if(any(!has)) install.packages(paquetes [!has])

################################################################################
##                                    Cargando Librerias
################################################################################
library(readr)
library(readxl)
library(readxl)
library(faraway)
library(openxlsx)
library(foreign)
library(dplyr)
library(survey)
library(srvyr)
library(haven)
library(stringr)
library(purrr)
library(tidyr)
################################################################################
## lectura de las bases de datos .rds
################################################################################
TMujer1 <- read_sav("MEX/2018/metadatos/TMujer1.sav")  
TMujer2 <- read_sav("MEX/2018/metadatos/TMujer2.sav") 
TFecHisEmb <- read_sav("MEX/2018/metadatos/TFecHisEmb.sav") 
TFecHisEmb_temp1 <-
  TFecHisEmb %>% select(
    upm,
    viv_sel,
    hogar,
    n_ren,
    llave_viv,
    llave_hog,
    llave_muj,
    ent,
    tam_loc,
    fac_per,
    edad_1ag,
    c_limdisc,
    p3_14,
    niv,
    gra,
    cond_act,
    p10_1_ag,
    estrato,
    est_dis,
    upm_dis,
    p5_17_1,
    p5_17_2,
    ordenemb,
    resemb
  ) %>% unique() %>%
  filter(p5_17_2 != "9999", p5_17_1 != "99")

TFecHisEmb_temp2 <-
  TFecHisEmb_temp1 %>%
  group_by(llave_muj) %>%
  mutate(row = row_number()) %>%
  pivot_wider(
    names_from = row,
    values_from = c(p5_17_1, p5_17_2, ordenemb, resemb),
    names_sep = "."
  )


TMujer1_2 <- full_join(TMujer1, TMujer2) 

TMujer1_2 %>% filter(llave_muj =="310360509102") %>% View()

TMujer1_emb <- left_join(TMujer1_2, TFecHisEmb_temp2) 

TMujer1_emb %>% filter(llave_muj =="310360509102") %>% View()

write_sav(data = TMujer1_emb, path = "MEX/2018/1.D6/Data/Mujer_2018_R.sav")

## El sigueinte código **0c.Indicador NI.sps** se debe ejecutar en spss 

