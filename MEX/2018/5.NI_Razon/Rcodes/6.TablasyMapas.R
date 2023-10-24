#########################################################
# Proyecto MRP - Leave no one behind                    #
# Mapas de pobreza CEPAL                                #
# Modelo Multinivel Bayesiano                           #
# Autor: Stalyn Guerrero  &  Andrés Gutiérrez           #
#########################################################

rm(list = ls())

# Loading required libraries ----------------------------------------------

library(tidyverse)
library(magrittr)
library(patchwork)
library(sp)
library(sf)
library(tmap)
library(RColorBrewer)
library(maptools)
library(stringi)


source("0Funciones/funciones_mrp.R", encoding = "UTF-8")
# Loading data ------------------------------------------------------------
poststrat_df <- readRDS("MEX/2018/5.NI_Razon/Data/poststrat_df.RDS")

#### Definiendo columna de estimaciones

poststrat_df %<>% mutate(yk_bench_num = Num*gk,
                         yk_bench_den = Den*gk)

bynames <-
  grep(
    pattern =  "^(yk|F|n|Den|Num|tasa_desocupacion|sexo|epred_mat|gk|crops.coverfraction|urban.coverfraction|stable_lights|mpio|lp|.groups)",
    x = names(poststrat_df),
    invert = TRUE,
    value = TRUE
  )
bynames

bynames <-   t(combn(bynames, 2)) 


dat_df = map(1:nrow(bynames),
             ~poststrat_df %>% group_by_at(vars("dam", bynames[.x,])) %>%
               summarise(Benchmarking_estimate =sum(n * yk_bench_num)/sum(yk_bench_den*n), .groups = "drop") %>% 
               ungroup() %>% 
               mutate(Benchmarking_estimate = ifelse(Benchmarking_estimate >= 1, 1,Benchmarking_estimate)))

dat_df %>% map(~.x %>% filter(Benchmarking_estimate >= 1))

bynames <-   as.tibble(bynames) %>% 
  mutate(dat_df = dat_df)


bynames <- mutate(
  bynames,
  outPaht = paste0("MEX/2018/5.NI_Razon/Output/pobreza_", paste0(V1, "_", V2),
                   ".pdf")
)

states_df <- poststrat_df %>% group_by_at("dam") %>%
  summarise(Benchmarking_estimate = sum(n * yk_bench_num)/sum(yk_bench_den*n), .groups = "drop") %>% 
  ungroup() 


## Leer Shape del pais
ShapeSAE <- read_sf("MEX/2018/Shape/MEX.shp") 

## Ajustando codigo de departamento

### cod de departamento en la shape
cod_shape <- ShapeSAE %>% as.data.frame() %>% select(dam, nombre)

### cod de departamento en la encuesta

P1_ingresolp <- tm_shape(ShapeSAE %>%
                           left_join(states_df,  by = "dam"))

brks_lp <- c(0, 0.015, 0.03, 
             0.045, 0.06, 0.075, 0.09, 0.115, 1)
tmap_options(check.and.fix = TRUE)
Mapa_lp <-
  P1_ingresolp + tm_polygons(
    "Benchmarking_estimate",
    breaks = brks_lp,
    title = "NI",
    palette = "YlOrRd"
  ) + tm_layout(asp = 0)


tmap_save(
  Mapa_lp,
  "MEX/2018/5.NI_Razon/Output/Estados.pdf",
  width = 6920,
  height = 4080,
  asp = 0
)

bynames %<>% rename(fnames = V1, cnames = V2)

#########################################################
## Exportando tablas 
cod_shape <- ShapeSAE %>% as.data.frame() %>% select(dam, nombre) %>% 
  mutate(
    nombre= stri_enc_toutf8(nombre, is_unknown_8bit=TRUE, validate=TRUE)
  )

nom_tabs <- c("dam", paste0(bynames$fnames, "_", bynames$cnames))
dat_df <- c(list(states_df), bynames$dat_df)
names(dat_df) <- nom_tabs

dat_df <-
  map(dat_df, ~ full_join(y = .x, cod_shape, by = "dam")) %>%
  map( ~ .x %>% rename(Benchmarking = Benchmarking_estimate))

openxlsx::write.xlsx(dat_df, file = "MEX/2018/5.NI_Razon/Output/tablas.xlsx", 
                     overwrite = TRUE, )

Mapa_lp
