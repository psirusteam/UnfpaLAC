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
poststrat_df <- readRDS("PER/2021/1.D6/Data/poststrat_df.RDS") %>% 
  filter(anoest != "99")

bynames <-
  grep(
    pattern =  "^(X|F|n|pobreza|ingreso|tasa_desocupacion|sexo|epred_mat|gk|crops.coverfraction|urban.coverfraction|stable_lights|mpio|lp|.groups)",
    x = names(poststrat_df),
    invert = TRUE,
    value = TRUE
  )
bynames

bynames <-   t(combn(bynames, 2)) 

 
dat_df = map(1:nrow(bynames),
    ~poststrat_df %>% group_by_at(vars("mpio", bynames[.x,])) %>%
  summarise(Benchmarking_estimate = sum(n * pobreza2) / sum(n), .groups = "drop") %>% 
  ungroup() %>% 
    mutate(Benchmarking_estimate = ifelse(Benchmarking_estimate >= 1, 1,Benchmarking_estimate)))

dat_df %>% map(~.x %>% filter(Benchmarking_estimate >= 1))

bynames <-   as.tibble(bynames) %>% 
  mutate(dat_df = dat_df)


bynames <- mutate(
  bynames,
  outPaht = paste0("PER/2021/1.D6/Output/pobreza_", paste0(V1, "_", V2),
                   ".pdf")
)

states_df <- poststrat_df %>% group_by_at("mpio") %>%
  summarise(Benchmarking_estimate = sum(n * pobreza2) / sum(n), .groups = "drop") %>% 
  ungroup() 


## Leer Shape del pais
ShapeSAE <- read_sf("PER/2021/ShapesPER/PROVINCIAS.shp")
ShapeSAE %<>% mutate(mpio = IDPROV,
                     nombre= PROVINCIA
                     )


## Ajustando codigo de departamento

### cod de departamento en la shape
cod_shape <- ShapeSAE %>% as.data.frame() %>% select(mpio, nombre)

### cod de departamento en la encuesta

# Perú
P1_ingresolp <- tm_shape(ShapeSAE %>%
                           left_join(states_df,  by = "mpio"))

brks_lp <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 1)
tmap_options(check.and.fix = TRUE)
Mapa_lp <-
  P1_ingresolp + tm_polygons(
    "Benchmarking_estimate",
    breaks = brks_lp,
    title = "D6",
    palette = "-YlOrRd"
  ) + tm_layout(asp = 0)

tmap_save(
  Mapa_lp,
  "PER/2021/1.D6/Output/Estados.pdf",
  width = 6920,
  height = 4080,
  asp = 0
)
 
bynames %<>% rename(fnames = V1, cnames = V2)

#########################################################
## Exportando tablas 
cod_shape <- ShapeSAE %>% as.data.frame() %>% select(mpio, PROVINCIA) %>% 
  rename(nombre = PROVINCIA) %>% 
  mutate(
    nombre= stri_enc_toutf8(nombre, is_unknown_8bit=TRUE, validate=TRUE)
  )

nom_tabs <- c("mpio", paste0(bynames$fnames, "_", bynames$cnames))
dat_df <- c(list(states_df), bynames$dat_df)
names(dat_df) <- nom_tabs

dat_df <-
  map(dat_df, ~ full_join(y = .x, cod_shape, by = "mpio")) %>%
  map( ~ .x %>% rename(Benchmarking = Benchmarking_estimate))

openxlsx::write.xlsx(dat_df, file = "PER/2021/1.D6/Output/tablas.xlsx", 
                     overwrite = TRUE, )

Mapa_lp