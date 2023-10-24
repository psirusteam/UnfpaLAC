# Mapas para el informe de resultados. 

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
library(sf)
library(tmap)
library(RColorBrewer)
library(maptools)
library(stringi)


source("0Funciones/funciones_mrp.R", encoding = "UTF-8")
# Loading data ------------------------------------------------------------
poststrat_df <- readRDS("MEX/2018/3.NI/Data/poststrat_df.RDS") %>% 
  filter(anoest != "99")

bynames <-
  grep(
    pattern =  "^(X|F|n|pobreza|tasa_desocupacion|sexo|epred_mat|gk|crops.coverfraction|urban.coverfraction|stable_lights|dam|lp|.groups)",
    x = names(poststrat_df),
    invert = TRUE,
    value = TRUE
  )
bynames

bynames <-   t(combn(bynames, 2)) 


dat_df = map(1:nrow(bynames),
             ~poststrat_df %>% group_by_at(vars("dam", bynames[.x,])) %>%
               summarise(Benchmarking_estimate = sum(n * pobreza2) / sum(n), .groups = "drop") %>% 
               ungroup() %>% 
               mutate(Benchmarking_estimate = ifelse(Benchmarking_estimate >= 1, 1,Benchmarking_estimate)))

dat_df %>% map(~.x %>% filter(Benchmarking_estimate >= 1))

bynames <-   as.tibble(bynames) %>% 
  mutate(dat_df = dat_df)


bynames <- mutate(
  bynames,
  outPaht = paste0("MEX/2018/6.Mapas_informe/Output/NI_", paste0(V1, "_", V2),
                   ".pdf")
)

states_df <- poststrat_df %>% group_by_at("dam") %>%
  summarise(Benchmarking_estimate = sum(n * pobreza2) / sum(n),
            .groups = "drop") %>%
  ungroup()


## Leer Shape del pais
ShapeSAE <- read_sf("MEX/2018/Shape/MEX.shp") 

## Ajustando codigo de Municipio

### cod de municipio en la shape
cod_shape <- ShapeSAE %>% 
  as.data.frame() %>% select(dam, nombre)

anti_join(states_df,cod_shape)

full_join(states_df,cod_shape) %>% data.frame()


### cod de municipio en la encuesta

P1_ingresolp <- tm_shape(ShapeSAE %>%
                           left_join(states_df,  by = "dam"))

brks_lp <- c(0,0.04, 0.045, 0.050, 0.055, 0.1)
tmap_options(check.and.fix = TRUE)
Mapa_lp <-
  P1_ingresolp + tm_polygons(
    "Benchmarking_estimate",
    breaks = brks_lp,
    title = "Indicador NI",
    palette = "-RdYlGn",
    contrast = c(0.2, 0.8)
  ) + tm_layout(legend.only = FALSE,
                legend.height = -0.4,
                legend.width = -0.3,
                asp = 1.5,
                legend.text.size = 2,
                legend.title.size = 3)

tmap_save(
  Mapa_lp,
  "MEX/2018/6.Mapas_informe/Output/NI_Estados.pdf",
  width = 4920,
  height = 3080,
  asp = 0
)

tmap_save(
  Mapa_lp,
  "MEX/2018/6.Mapas_informe/Output/NI_Estados.png",
  width = 4920,
  height = 3080,
  asp = 0
)

tmap_save(
  Mapa_lp,
  "MEX/2018/6.Mapas_informe/Output/NI_Estados.jpeg",
  width = 4920,
  height = 3080,
  asp = 0
)


########### Mujeres unidas 

states_df_unida <-
  poststrat_df %>% filter(unida == "1") %>% group_by_at("dam") %>%
  summarise(Benchmarking_estimate = sum(n * pobreza2) / sum(n),
            .groups = "drop") %>%
  ungroup()

full_join(cod_shape, states_df_unida) %>% data.frame()


P1_ingresolp <- tm_shape(ShapeSAE %>%
                           left_join(states_df_unida,  by = "dam"))

brks_lp <- c(0,0.04, 0.050, 0.06, 0.07, 0.1)
tmap_options(check.and.fix = TRUE)
Mapa_lp <-
  P1_ingresolp + tm_polygons(
    "Benchmarking_estimate",
    breaks = brks_lp,
    title = "Indicador NI",
    palette = "-RdYlGn",
    contrast = c(0.2, 0.8)
  ) + tm_layout(legend.only = FALSE,
                legend.height = -0.4,
                legend.width = -0.3,
                asp = 1.5,
                legend.text.size = 2,
                legend.title.size = 3)

tmap_save(
  Mapa_lp,
  "MEX/2018/6.Mapas_informe/Output/NI_Estados_unida.pdf",
  width = 4920,
  height = 3080,
  asp = 0
)

tmap_save(
  Mapa_lp,
  "MEX/2018/6.Mapas_informe/Output/NI_Estados_unida.png",
  width = 4920,
  height = 3080,
  asp = 0
)

tmap_save(
  Mapa_lp,
  "MEX/2018/6.Mapas_informe/Output/NI_Estados_unida.jpeg",
  width = 4920,
  height = 3080,
  asp = 0
)

########### Mujeres unidas etnia 
label_etnia = c(
  "1" = "Indigena",
  "2" = "Afro",
  "3" = "Otro"
)
states_df_unida_etnia <-
  poststrat_df %>% filter(unida == "1") %>% group_by_at(c("dam","etnia")) %>%
  summarise(Benchmarking_estimate = sum(n * pobreza2) / sum(n),
            .groups = "drop") %>%
  ungroup()

states_df_unida_etnia$etnia2 <-
  dplyr::recode(as.character(states_df_unida_etnia$etnia),!!!label_etnia)

full_join(cod_shape, states_df_unida_etnia) %>% data.frame()


Mapa_E1 <- tm_shape(ShapeSAE %>%
                           left_join(states_df_unida_etnia %>% filter(etnia == "1")
                                    , by = "dam"))
Mapa_E2 <- tm_shape(ShapeSAE %>%
                      left_join(states_df_unida_etnia %>% filter(etnia == "2")
                                , by = "dam"))
Mapa_E3 <- tm_shape(ShapeSAE %>%
                      left_join(states_df_unida_etnia %>% filter(etnia == "3")
                                , by = "dam"))


brks_lp <- c(0,0.04, 0.050, 0.06, 0.07, 0.1)
tmap_options(check.and.fix = TRUE)
Mapa1 <-
  Mapa_E1 + tm_polygons(
    "Benchmarking_estimate",
    breaks = brks_lp,
    title = "Indicador NI\n(Indígenas)",
    palette = "-RdYlGn",
    contrast = c(0.2, 0.8)
  ) + tm_layout(legend.only = FALSE,
                legend.height = -0.45,
                legend.width = -0.3,
                asp = 2,
                legend.text.size = 4,
                legend.title.size = 3)
tmap_save(
  Mapa1,
  "MEX/2018/6.Mapas_informe/Output/NI_Estados_unida_etnia1.pdf",
  width = 2000,
  height = 1500,
  asp = 0
)


Mapa2 <-
  Mapa_E2 + tm_polygons(
    "Benchmarking_estimate",
    breaks = brks_lp,
    title = "Indicador NI\n(Afrodescendientes)",
    palette = "-RdYlGn",
    contrast = c(0.2, 0.8)
  )  + tm_layout(legend.only = FALSE,
                 legend.height = -0.45,
                 legend.width = -0.3,
                 asp = 2,
                 legend.text.size = 4,
                 legend.title.size = 3)

Mapa3 <-
  Mapa_E3 + tm_polygons(
    "Benchmarking_estimate",
    breaks = brks_lp,
    title = "Indicador NI\n(otros)",
    palette = "-RdYlGn",
    contrast = c(0.2, 0.8)
  )  + tm_layout(legend.only = FALSE,
                 legend.height = -0.45,
                 legend.width = -0.3,
                 asp = 2,
                 legend.text.size = 4,
                 legend.title.size = 3)



mosaico <-
  tmap_arrange(Mapa1,
               Mapa2,
               Mapa3,
               ncol = 3,
               nrow = 1)


tmap_save(
  mosaico,
  "MEX/2018/6.Mapas_informe/Output/NI_Estados_unida_etnia.pdf",
  width = 8920,
  height = 4080,
  asp = 0
)
tmap_save(
  mosaico,
  "MEX/2018/6.Mapas_informe/Output/NI_Estados_unida_etnia.png",
  width = 8920,
  height = 4080,
  asp = 0
)
tmap_save(
  mosaico,
  "MEX/2018/6.Mapas_informe/Output/NI_Estados_unida_etnia.jpeg",
  width = 8920,
  height = 4080,
  asp = 0
)



#########################################################
## Exportando tablas 
poststrat_df <- readRDS("MEX/2018/3.NI/Data/poststrat_df.RDS") %>% 
  filter(anoest != "99")

bynames <-
  grep(
    pattern =  "^(X|F|n|pobreza|tasa_desocupacion|sexo|epred_mat|gk|crops.coverfraction|urban.coverfraction|stable_lights|dam|lp|.groups|unida)",
    x = names(poststrat_df),
    invert = TRUE,
    value = TRUE
  )
bynames

bynames <-   t(combn(bynames, 2)) 


dat_df = map(1:nrow(bynames),
             ~poststrat_df %>% filter(unida == "1") %>% group_by_at(vars("dam", bynames[.x,])) %>%
               summarise(Benchmarking_estimate = sum(n * pobreza2) / sum(n), .groups = "drop") %>% 
               ungroup() %>% 
               mutate(Benchmarking_estimate = ifelse(Benchmarking_estimate >= 1, 1,Benchmarking_estimate)))

dat_df %>% map(~.x %>% filter(Benchmarking_estimate >= 1))

bynames <-   as.tibble(bynames) %>% 
  mutate(dat_df = dat_df)


bynames <- mutate(
  bynames,
  outPaht = paste0("MEX/2018/6.Mapas_informe/Output/NI_unida_", paste0(V1, "_", V2),
                   ".png")
)

bynames %<>% rename(fnames = V1, cnames = V2)

cod_shape <- ShapeSAE %>% as.data.frame() %>% select(dam, nombre) %>% 
  rename(nombre = nombre) %>% 
  mutate(
    nombre= stri_enc_toutf8(nombre, is_unknown_8bit=TRUE, validate=TRUE)
  )

temp_brks <- bynames$dat_df %>% map(~.x$Benchmarking_estimate) %>% 
  unlist()

brks_lp <- as.numeric(round(c(0,quantile(temp_brks, c(.1,0.2,0.4, 0.60,0.80,1))),3))

bynames$dat_df  <- bynames$dat_df %>% map(~.x %>% rename(depto = "dam"))
bynames$Maps = pmap(bynames , function(...) {
  Aux_Maps(Shape = ShapeSAE%>% rename(depto = "dam"), 
           color_p = "-RdYlGn" , brks = brks_lp,...)
})

temp_data <- bynames$dat_df[[3]]
label_anoest = c(
  "1" = "Sin educacion",
  "2" = "1 - 6 años",
  "3" = "7 - 12 años",
  "4" = "Más de 12",
  "98" = "No aplica",
  "99" = "NS/NR"
)

temp_data %>%
  transmute(dam,
    area = ifelse(area == "0","Rural","Urbana"),
        anoest = dplyr::recode(as.character(anoest),
                             !!!label_anoest),
    NI = Benchmarking_estimate) %>% 
  inner_join(cod_shape) %>% data.frame() %>% 
  slice(201:256)
  
