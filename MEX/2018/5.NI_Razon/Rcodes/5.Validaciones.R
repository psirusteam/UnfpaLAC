#########################################################
# Proyecto MRP - Leave no one behind                    #
# Mapas de pobreza CEPAL                                #
# Lectura y preparación de las bases de datos           #
# Autor: Stalyn Guerrero & Andrés Gutiérrez             #
#########################################################

rm(list =ls())

# Librerías

library(tidyverse)
library(patchwork)
library(survey)
library(srvyr)

source("0Funciones/plot_compare_razon.R", encoding = "UTF-8")
# Loading data ------------------------------------------------------------
encuesta_mrp <- readRDS("MEX/2018/5.NI_Razon/Data/encuesta_mrp.rds")
poststrat_df <- readRDS("MEX/2018/5.NI_Razon/Data/poststrat_df.RDS")

# Revisión de NAs ---------------------------------------------------------

sum(complete.cases(poststrat_df)) == nrow(poststrat_df)

# Calculo de la pobreza Cepal.  -------------------------------------------

poststrat_df %<>% mutate(yk_lmer_num = Num,
                         yk_lmer_den = Den,
                         yk_bench_num = Num*gk,
                         yk_bench_den = Den*gk)

diseno <- encuesta_mrp %>%
  mutate(yk_num = NI,
         yk_den = usametodo) %>% 
  as_survey_design(weights = fexp)

## validación nacional.
diseno %>% filter(unida == "1") %>%
  summarise(Nacional_dir = survey_ratio(yk_num,yk_den))
poststrat_df%>% filter(unida == "1") %>% summarise(
  Nacional_lmer = sum(n * yk_lmer_num)/sum(yk_lmer_den*n) ,
  Nacional_bench = sum(n * yk_bench_num)/sum(yk_bench_den*n)
)

## validación nacional.
diseno %>% 
  summarise(Nacional_dir = survey_ratio(yk_num,yk_den))
poststrat_df %>% summarise(
  Nacional_lmer = sum(n * yk_lmer_num)/sum(yk_lmer_den*n) ,
  Nacional_bench = sum(n * yk_bench_num)/sum(yk_bench_den*n)
)


###########################################
###########################################
### Validaciones por subgrupo completo  ###
###########################################
###########################################
bynames <-
  grep(
    pattern =  "^(X|F|n|Num|Den|tasa_desocupacion|epred_mat|gk|yk|urban.coverfraction|stable_lights|crops.coverfraction|sexo|mpio)",
    x = names(poststrat_df),
    invert = TRUE,
    value = TRUE
  )

bynames

plot_uni <- map(
  .x = setNames(bynames, bynames),
  ~ plot_compare_razon(
    sample_diseno = diseno,
    poststrat = poststrat_df,
    by1 = .x
  )
)


plot_uni <- (
  plot_uni$dam$Plot$plot1
)/
  (plot_uni$area$Plot$plot1 + plot_uni$anoest$Plot$plot1 +plot_uni$edad$Plot$plot1+
     plot_uni$discapacidad$Plot$plot1+ plot_uni$etnia$Plot$plot1)

ggsave(plot = plot_uni, 
       filename = "MEX/2018/5.NI_Razon/Output/plot_uni.jpeg",scale = 5)

plot_uni

plot_uni <- map(
  .x = setNames(bynames, bynames),
  ~ plot_compare_razon(
    sample_diseno = diseno %>% filter(unida == 1),
    poststrat = poststrat_df %>% filter(unida == 1),
    by1 = .x
  )
)


plot_uni <- (
  plot_uni$dam$Plot$plot1
)/
  (plot_uni$area$Plot$plot1 + plot_uni$anoest$Plot$plot1 +plot_uni$edad$Plot$plot1+
     plot_uni$discapacidad$Plot$plot1+ plot_uni$etnia$Plot$plot1)

ggsave(plot = plot_uni, 
       filename = "MEX/2018/5.NI_Razon/Output/plot_uni_unidas.jpeg",scale = 5)

plot_uni
