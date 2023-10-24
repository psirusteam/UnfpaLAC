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
################################################################################
## Cambiando las bases de formato .sav a .rds
################################################################################
# read_sav("MEX/2018/1.D6/Data/THogar.sav") %>% saveRDS("MEX/2018/1.D6/Data/THogar.rds")
# read_sav("MEX/2018/1.D6/Data/TMujer1.sav") %>% saveRDS("MEX/2018/1.D6/Data/TMujer1.rds")
# read_sav("MEX/2018/1.D6/Data/TMujer2.sav") %>% saveRDS("MEX/2018/1.D6/Data/TMujer2.rds")
# read_sav("MEX/2018/1.D6/Data/TVivienda.sav") %>% saveRDS("MEX/2018/1.D6/Data/TVivienda.rds")
# read_sav("MEX/2018/1.D6/Data/TSdem.sav") %>% saveRDS("MEX/2018/1.D6/Data/TSdem.rds")
# read_sav("MEX/2018/1.D6/Data/TFecHisEmb.sav") %>% saveRDS("MEX/2018/1.D6/Data/TFecHisEmb.rds")

################################################################################
## lectura de las bases de datos .rds
################################################################################

### TMujer1

# La tabla TMujer1 incluye las variables que permiten analizar aspectos sobre el
# comportamiento reproductivo de las mujeres de 15 a 54 años. Esta tabla comprende
# la primera parte de dos tablas que incluyen variables referidas a estos temas.

#### Variables de interés : 
# - upm:  
# - llave_viv
# - llave_hog
# - llave_muj
# - p5_2_1: Edad
# - p8_9   : ¿Actualmente usted (o su pareja) están utilizando algún método para evitar el embarazo?
# - p8_10  : ¿Está usted (o su pareja) operada para evitar el embarazo?
# - p8_11  : ¿Qué está utilizando usted (o su pareja) para no tener hijas(os)?
# - p8_11_14c: Otro tipo de método (actual)
# - p8_11a : Método que usa (actual) por posición
# - p8_11a_r: Método que usa (actual) por código
# ¿Cuándo fue la última vez que usted tuvo relaciones sexuales?
# - p8_41_1: Actividad sexual, Dias 
# - p8_41_2: Actividad sexual, Semanas 
# - p8_41_3: Actividad sexual, Meses
# - p8_41_4: Actividad sexual, Años
# - niv    : Nivel de escolaridad
# - p10_1_ag: Involucra la pregunta 10.1 Situación conyugal.
# - p10_1_ag2: Involucra la pregunta 10.1 Situación conyugal.
# - est_dis  : Estrato muestral
# - upm_dis  : UPM_diseño muestral
# - fac_per  : Factor de expansión de la mujer
# - t_loc_ur : Tamaño de localidad, Urbano-Rural
# - mashijos: 
# - p7_2    : ¿En el momento que usted se embarazó…
# - p7_3    : 
# - p7_7    : 
# - p7_11   : 

var_mujer1 <- c(
  "upm" , "llave_viv" , "llave_hog" , "llave_muj" , "p5_2_1" , "p8_9" , 
  "p8_10" , "p8_11" , "p8_11_14c" , "p8_11a" , "p8_11a_r" , "p8_41_1" , 
  "p8_41_2" , "p8_41_3" , "p8_41_4" , "niv" , "p10_1_ag" , "p10_1_ag2" ,
  "est_dis" , "upm_dis" , "fac_per" , "t_loc_ur", "mashijos","p7_2",
  "p7_3", "p7_7","p7_11") 

# Persona1 <- readRDS("MEX/2018/1.D6/Data/TMujer1.rds") %>% 
  # select(all_of(var_mujer1)) %>% 
  # rename(llave_per = llave_muj)

### TSdem 

# La tabla TSDem (características sociodemográficas de las personas) concentra
# información sociodemográfica sobre las personas integrantes del hogar.

#### Variables de interés : 
# - upm       : Unidad Primaria de Muestreo
# - llave_per : Llave de persona
# - llave_hog : Llave de hogar
# - llave_viv : Llave de viviendaNo aplica.
# - ent       : Entidad federativa
# - p3_9      : Condición de Autoadscripción indígena
# - p3_9a     : Condición de Afrodescedencia
# - p3_18     : Condición de alfabetismo
# - p3_21     : Situación conyugal
# - edad_2ag  : Grandes grupos de edad
# - esco_acum : Escolaridad acumulada
# - p3_21_ag  : Situación conyugal agrupada
# - c_limdisc : Condición de limitación o discapacidad
# - est_dis   : Estrato muestral
# - upm_dis   : UPM diseño muestral
var_demog <-c("upm", "llave_per", "llave_hog", "llave_viv", "ent", 
              "p3_9", "p3_9a", "p3_18", "p3_21", "edad_2ag", "esco_acum",
              "p3_21_ag", "c_limdisc", "est_dis", "upm_dis")

# Demografica <- readRDS("MEX/2018/1.D6/Data/TSdem.rds") %>% 
  # select(all_of(var_demog))

### THogar

# La Tabla THogar concentra información relativa a los hogares dentro de las viviendas
# seleccionadas para la aplicación de la encuesta, específicamente en esta tabla se 
# incluyen la contabilización del número de integrantes del hogar y la condición
# de migración internacional durante los últimos cinco años en el hogar.

# Hogar <- readRDS("MEX/2018/1.D6/Data/THogar.rds")

###  TVivienda

# La Tabla TVivienda concentra información sobre las viviendas seleccionadas para
# la aplicación de la encuesta, específicamente sobre las características físicas
# y disponibilidad de servicios en las viviendas, número de hogares en la vivienda
# y número de personas residentes habituales en la vivienda y que integran los hogares
# dentro de la misma.

# Vivienda <- readRDS("MEX/2018/1.D6/Data/TVivienda.rds")

# ENADID <- inner_join(Demografica, Persona1 )
# saveRDS(ENADID, "MEX/2018/1.D6/Data/ENADID_2018.rds")
ENADID <- readRDS("MEX/2018/1.D6/Data/ENADID_2018.rds")
################################################################################
## Validaciones a la base de datos. 
################################################################################

### Mujeres en el rango de edad 15 a 49 años. 

# La base tiene 97.961 mujeres en el rango de edad

ENADID %>% summarise(
  n = n(),
  edad_si = sum(p5_2_1>=15 & p5_2_1 <= 49),
  edad_no = sum(!(p5_2_1>=15 & p5_2_1 <= 49)), 
  NAs = sum(is.na(p5_2_1)))

mujer_15_49 <- filter(ENADID,between(as.numeric(p5_2_1),15,49))
dim(mujer_15_49)
### Uso de métodos
tabla_frecuencia <- function(data){
  label <- attributes(data)$label
  Freq <- xtabs(~as_factor(data), sparse = FALSE)
  Porcentaje <- data.frame(prop.table(Freq)*100)%>% 
    mutate(Pregunta = label )
  # Porcentaje <- reshape2::dcast(Porcentaje,.~as_factor.data.) %>% 
  # mutate(Pregunta = label )
  resultado <-  Porcentaje
  return(resultado)
}

mujer_15_49 %>% select(starts_with("p8_")) %>% 
  purrr::map(~.x %>% tabla_frecuencia())
mujer_15_49 %>% distinct(ent,upm,upm_dis) %>% arrange(ent)
######### Definiendo codigo UPM para obtener el dam, dam2, area
mujer_15_49 <- mujer_15_49 %>% 
  mutate(dam = ent,
         dam2 = str_sub(upm,1,5),
         area = ifelse(t_loc_ur== "1","Urbana","Rural"),
         dam_nombre = case_when(dam == "01" ~ "Aguascalientes" ,
                                dam == "02" ~ "Baja California" ,
                                dam == "03" ~ "Baja California Sur" ,
                                dam == "04" ~ "Campeche" ,
                                dam == "05" ~ "Coahuila de Zaragoza" ,
                                dam == "06" ~ "Colima" ,
                                dam == "07" ~ "Chiapas" ,
                                dam == "08" ~ "Chihuahua" ,
                                dam == "09" ~ "Ciudad de México" ,
                                dam == "10" ~ "Durango" ,
                                dam == "11" ~ "Guanajuato" ,
                                dam == "12" ~ "Guerrero" ,
                                dam == "13" ~ "Hidalgo" ,
                                dam == "14" ~ "Jalisco" ,
                                dam == "15" ~ "México" ,
                                dam == "16" ~ "Michoacán de Ocampo" ,
                                dam == "17" ~ "Morelos" ,
                                dam == "18" ~ "Nayarit" ,
                                dam == "19" ~ "Nuevo León" ,
                                dam == "20" ~ "Oaxaca" ,
                                dam == "21" ~ "Puebla" ,
                                dam == "22" ~ "Querétaro" ,
                                dam == "23" ~ "Quintana Roo" ,
                                dam == "24" ~ "San Luis Potosí" ,
                                dam == "25" ~ "Sinaloa" ,
                                dam == "26" ~ "Sonora" ,
                                dam == "27" ~ "Tabasco" ,
                                dam == "28" ~ "Tamaulipas" ,
                                dam == "29" ~ "Tlaxcala" ,
                                dam == "30" ~ "Veracruz de Ignacio de la Llave" ,
                                dam == "31" ~ "Yucatán" ,
                                dam == "32" ~ "Zacatecas", 
                                TRUE ~ NA_character_)) 
## Mujeres Sexualmente activa 
## Semana 
mujer_15_49 %>% distinct(p8_41_1, #día
                         p8_41_2, #semana
                         p8_41_3  #mes 
) %>% 
  arrange(p8_41_1,p8_41_2,p8_41_3) %>% data.frame()

mujer_15_49 <- mujer_15_49 %>% 
  mutate(p8_41_1,p8_41_2,p8_41_3,
         sexactiv_dias = ifelse(p8_41_1 %in% str_pad(0:30,width = 2,pad = "0"),1,0),
         sexactiv_semana = ifelse(p8_41_2 %in% paste0("0",1:4),1,0),
         sexactiv_mes = ifelse(p8_41_3 %in% "01",1,0),
         sexactiv = ifelse(sexactiv_dias + sexactiv_semana + sexactiv_mes >= 1,1,0)
  ) 

mujer_15_49 %>% distinct(p8_41_1, #día
                         p8_41_2, #semana
                         p8_41_3,  #mes 
                         sexactiv) %>% 
  arrange(p8_41_1,p8_41_2,p8_41_3) %>% data.frame()
table(mujer_15_49$sexactiv)

#### Unida
mujer_15_49 %>% distinct(p10_1_ag, p10_1_ag2)
mujer_15_49 <- mujer_15_49 %>% 
  mutate(unida = ifelse(p10_1_ag == "1",1,0)) 

mujer_15_49 %>% distinct(p10_1_ag, p10_1_ag2,unida)
table(mujer_15_49$unida)

#### Construcción Activa Sexual y Unida

table(mujer_15_49$unida,mujer_15_49$sexactiv)

mujer_15_49 %>% distinct(unida,sexactiv)

mujer_15_49 <- mujer_15_49 %>%
  mutate(ActivSexUnida = ifelse(unida + sexactiv == 2,1,0))

mujer_15_49 %>% distinct(unida,sexactiv,ActivSexUnida)
table(mujer_15_49$ActivSexUnida)

#### Definiendo variable edad
table(mujer_15_49$p5_2_1)

mujer_15_49 <- mujer_15_49 %>%
  mutate(
    Grupo_edad = case_when(
      p5_2_1 %in% 0:14 ~ "1",       # 5 a 14
      p5_2_1 %in% 15:29 ~ "2",      # 15 a 29
      p5_2_1 %in% 30:44 ~ "3",      # 30 a 44
      p5_2_1 %in% 45:64 ~ "4",      # 45 a 64
      TRUE ~ "5"
    ) 
  )     

table(mujer_15_49$Grupo_edad)
table(mujer_15_49$p5_2_1, mujer_15_49$Grupo_edad)

## Definir indicadores de interés. 
mujer_15_49 %>% distinct(p8_9) 
attributes(mujer_15_49$p8_9)
table(mujer_15_49$p8_9)
table(mujer_15_49$p8_10)

mujer_15_49 %>% distinct(p8_11a)%>% 
  arrange()
ftable(as_factor(mujer_15_49$p8_10),
       as_factor(mujer_15_49$p8_11a),
       as_factor(mujer_15_49$p8_9,levels = "labels"))

mujer_15_49 %>% distinct(p8_11_14c)
attributes(mujer_15_49$p8_11a)$label
table(mujer_15_49$p8_11a)

mujer_15_49 <- mujer_15_49 %>% mutate(
  usametodo = ifelse( p8_11a %in% c("01", "02", "05", "04", "07","03",
                                    "09", "08", "13", "11", "12","14",
                                    "06","10"),1,0),
  usamoderno = ifelse( p8_11a %in% c("01", "02", "05", "04","07","03",
                                     "09", "08", "13", "06","10"),1,0))

mujer_15_49 %>% distinct(p8_9,p8_11a,usametodo,usamoderno) %>% 
  data.frame() %>% arrange(p8_9)

# En 2018, el porcentaje de mujeres usuarias de métodos
# anticonceptivos es de 53.4% de las mujeres en edad fértil.
weighted.mean(mujer_15_49$usametodo,mujer_15_49$fac_per)#ok

# Para el caso de mujeres casadas o unidas de 15 a 49, 
# el porcentaje de usuarias pasó de 72.3% a 73.1 por ciento.

weighted.mean(mujer_15_49$usametodo[mujer_15_49$unida==1],
              mujer_15_49$fac_per[mujer_15_49$unida==1])#ok
# 16.8 millones de mujeres son usuarias de por lo menos un método
# anticonceptivo moderno 
# (http://estadistica.inmujeres.gob.mx/formas/tarjetas/Uso_anticonceptivos.pdf)
sum(mujer_15_49$usamoderno*mujer_15_49$fac_per)#ok

################## Seleccionado Covariables #####################
mujer_15_49_mrp <- mujer_15_49 %>%
  transmute(
    llave_viv, llave_hog, llave_per,
    dam, dam2 ,
    usametodo,  
    usamoderno,
    area, 
    
    edad = case_when(p5_2_1 %in% 0:14 ~  "12-14",      # 12 a 14
                     p5_2_1 %in% 15:19 ~ "15-19",      # 15 a 19
                     p5_2_1 %in% 20:24 ~ "20-24",      # 20 a 24
                     p5_2_1 %in% 25:29 ~ "25-29",      # 25 a 29
                     p5_2_1 %in% 30:34 ~ "30-34",      # 30 a 34
                     p5_2_1 %in% 35:39 ~ "35-39",      # 35 a 39
                     p5_2_1 %in% 40:44 ~ "40-44",      # 40 a 44
                     p5_2_1 %in% 40:49 ~ "45-49",      # 45 a 49
                     TRUE ~ "Error"
    ),               
    
    etnia = case_when(p3_9 == 1 ~  "Indigena",
                      p3_9a == 1 ~  "Afrodescendiente",
                      TRUE ~  "Otro"
    ),
    
    anoest = case_when( esco_acum == "99" ~ "NS/NR",# NS/NR
                        esco_acum %in% "00"       ~ "Sin educación", # Sin educacion
                        esco_acum %in% str_pad(1:6,width = 2,pad = "0")  ~ "1-6 años", # 1 - 6
                        esco_acum %in% str_pad(7:12,width = 2,pad = "0") ~ "7-12 años", # 7 - 12
                        !esco_acum  %in% str_pad(c(0:12,99),width = 2,pad = "0")  ~ "Más de 12 años", # mas de 12
                        TRUE ~ "Error"
    ),
    
    discapacidad = case_when(
      c_limdisc == "1" ~ "Dispacitado",
      TRUE ~ "No Discapacitado"
      
    ),
    
    unida = case_when(unida == 1 ~ "Unida", 
                      unida == 0 ~ "No unida"
    ),
    
    upm = upm,
    estrato = est_dis,
    fexp = fac_per
  )

saveRDS(mujer_15_49_mrp, file = "MEX/2018/1.D6/Data/ENADID_MUJER.rds")
