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
poststrat_df <- readRDS("GTM/2014-2015/3.NI/Data/poststrat_df.RDS") %>% 
  filter(anoest != "99")

bynames <-
  grep(
    pattern =  "^(X|F|n|pobreza|ingreso|tasa_desocupacion|sexo|epred_mat|gk|crops.coverfraction|urban.coverfraction|stable_lights|depto|lp|.groups)",
    x = names(poststrat_df),
    invert = TRUE,
    value = TRUE
  )
bynames

bynames <-   t(combn(bynames, 2)) 

 
dat_df = map(1:nrow(bynames),
    ~poststrat_df %>% group_by_at(vars("depto", bynames[.x,])) %>%
  summarise(Benchmarking_estimate = sum(n * pobreza2) / sum(n), .groups = "drop") %>% 
  ungroup() %>% 
    mutate(Benchmarking_estimate = ifelse(Benchmarking_estimate >= 1, 1,Benchmarking_estimate)))

dat_df %>% map(~.x %>% filter(Benchmarking_estimate >= 1))

bynames <-   as.tibble(bynames) %>% 
  mutate(dat_df = dat_df)


bynames <- mutate(
  bynames,
  outPaht = paste0("GTM/2014-2015/3.NI/Output/pobreza_", paste0(V1, "_", V2),
                   ".pdf")
)

states_df <- poststrat_df %>% group_by_at("depto") %>%
  summarise(Benchmarking_estimate = sum(n * pobreza2) / sum(n), .groups = "drop") %>% 
  ungroup() 


## Leer Shape del pais
ShapeSAE <- read_sf("GTM/2014-2015/ShapesGTM/departamentos_gtm.shp")
ShapeSAE %<>% mutate(depto = str_pad(departamen, pad = "0", width = 2), area = NULL)
ShapeSAE %<>% mutate(nombre= nombre)


## Ajustando codigo de departamento

### cod de departamento en la shape
cod_shape <- ShapeSAE %>% 
   as.data.frame() %>% 
     select(depto, nombre)

### cod de departamento en la encuesta

# Guatemala
P1_ingresolp <- tm_shape(ShapeSAE %>%
                           left_join(states_df,  by = "depto"))

brks_lp <- c(0, 0.025, 0.05, 0.075, 0.1, 0.125, 0.15, 0.175, 0.2, 1)
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
  "GTM/2014-2015/3.NI/Output/Estados.pdf",
  width = 6920,
  height = 4080,
  asp = 0
)
 
bynames %<>% rename(fnames = V1, cnames = V2)

#########################################################
## Exportando tablas 
cod_shape <- ShapeSAE %>% as.data.frame() %>% select(depto, nombre) %>% 
  rename(nombre = nombre) %>% 
  mutate(
    nombre= stri_enc_toutf8(nombre, is_unknown_8bit=TRUE, validate=TRUE)
  )

nom_tabs <- c("depto", paste0(bynames$fnames, "_", bynames$cnames))
dat_df <- c(list(states_df), bynames$dat_df)
names(dat_df) <- nom_tabs

dat_df <-
  map(dat_df, ~ full_join(y = .x, cod_shape, by = "depto")) %>%
  map( ~ .x %>% rename(Benchmarking = Benchmarking_estimate))

openxlsx::write.xlsx(dat_df, file = "GTM/2014-2015/3.NI/Output/tablas.xlsx", 
                     overwrite = TRUE, )

Mapa_lp
