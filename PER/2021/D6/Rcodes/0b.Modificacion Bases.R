###############################
## Creacion variables/dummys ##
###############################


rm(list = ls())
setwd("C:/Users/fmolina/Documents/SAE PERU")
dir()
memory.limit(size=18000000)
library(dplyr)
library(TeachingSampling)

#CensoMEF = readRDS("CensoMEFHV.rds")
#EndesMEF = readRDS("ENDESMEF.rds")

#-----
# Estandarización variables Censo-ENDES

## Departamento

levels(EndesMEF$V023) # Con acento
levels(EndesMEF$V024) 
levels(EndesMEF$HV023) # Sin acento
levels(EndesMEF$HV024)

levels(CensoMEF$CCDD) # Numero  

EndesMEF$Departamento = EndesMEF$V024
EndesMEF = EndesMEF %>% select(-V024,-HV024,-V023,-HV023)

CensoMEF$Departamento = CensoMEF$CCDD
levels(CensoMEF$Departamento) = levels(EndesMEF$Departamento)
CensoMEF = CensoMEF %>% select(-CCDD,)

## Provincia


EndesMEF$SHPROVIN # NA's

EndesMEF$SPROVIN
table(EndesMEF$SPROVIN)

EndesMEF$SPROVIN = ifelse(EndesMEF$SPROVIN<10,paste("0",EndesMEF$SPROVIN,sep = ""),EndesMEF$SPROVIN)
levels(CensoMEF$CCPP) = strtrim(levels(CensoMEF$CCPP),2)

CensoMEF$Provincia = as.factor(paste(CensoMEF$Departamento,CensoMEF$CCPP,sep = "-"))
EndesMEF$Provincia = as.factor(paste(EndesMEF$Departamento,EndesMEF$SPROVIN, sep = "-"))

CensoMEF = CensoMEF %>% select(-CCPP)
EndesMEF = EndesMEF %>% select(-SPROVIN,-SHPROVIN)

levels(CensoMEF$Provincia)[which(levels(CensoMEF$Provincia)%in%levels(EndesMEF$Provincia)==FALSE)]

## Distrito 

#### No es necesario modificar variables CCDI en Censo
#### y SDISTRI en ENDES, no se utilizaran.

## Area

CensoMEF$Area = CensoMEF$AREA
EndesMEF$Area = EndesMEF$V025

CensoMEF = CensoMEF %>% select(-AREA)
EndesMEF = EndesMEF %>% select(-HV025,-V025)

## Total de Hogares

EndesMEF$THOGAR = EndesMEF$SHTOTH
EndesMEF$THOGAR = ifelse(is.na(EndesMEF$THOGAR), "99",EndesMEF$THOGAR)
CensoMEF$THOGAR
EndesMEF = EndesMEF %>% select(-SHTOTH)
## Material Pared
EndesMEF$HV214
EndesMEF = EndesMEF %>% rename(MaterialPA = HV214)
CensoMEF = CensoMEF %>% rename(MaterialPA = C2_P3)

levels(EndesMEF$MaterialPA)
levels(CensoMEF$MaterialPA)

## Material Techo

EndesMEF = EndesMEF %>% rename(MaterialTE = HV215)
CensoMEF = CensoMEF %>% rename(MaterialTE = C2_P4)

levels(EndesMEF$MaterialTE)
levels(CensoMEF$MaterialTE)

## Material Techo

EndesMEF = EndesMEF %>% rename(MaterialPI = HV213)
CensoMEF = CensoMEF %>% rename(MaterialPI = C2_P5)

levels(EndesMEF$MaterialTE)
levels(CensoMEF$MaterialTE)

## Abastecimiento de agua
EndesMEF = EndesMEF %>% select(-HV202)

EndesMEF = EndesMEF %>% rename(Ab.agua = V113)
CensoMEF = CensoMEF %>% rename(Ab.agua = C2_P6)
EndesMEF$Ab.agua = as.factor(ifelse(EndesMEF$Ab.agua=="No residente de jure",NA,
                                    as.character(EndesMEF$Ab.agua)))
levels(EndesMEF$Ab.agua)
levels(CensoMEF$Ab.agua)

## servicio de agua

EndesMEF = EndesMEF %>% rename(Ser.agua = SH42)
CensoMEF = CensoMEF %>% rename(Ser.agua = C2_P7)

levels(EndesMEF$Ser.agua)
levels(CensoMEF$Ser.agua)

EndesMEF$Ser.agua = ifelse(EndesMEF$Ser.agua == "Si",1,0)
CensoMEF$Ser.agua = ifelse(startsWith(as.character(CensoMEF$Ser.agua),"Si"),1,0)

## pagan por agua

EndesMEF = EndesMEF %>% rename(pago.agua = SH51)
CensoMEF = CensoMEF %>% rename(pago.agua = C2_P8)

levels(EndesMEF$pago.agua) ## solo NA's
levels(CensoMEF$pago.agua)

## cobro por agua

EndesMEF = EndesMEF %>% rename(cobro.agua = SH52)
CensoMEF = CensoMEF %>% rename(cobro.agua = C2_P9)

levels(EndesMEF$cobro.agua) ## solo NA's
levels(CensoMEF$cobro.agua)

## conexion servicio higienico

EndesMEF = EndesMEF %>% rename(serv.hig = HV205)
CensoMEF = CensoMEF %>% rename(serv.hig = C2_P10)

levels(EndesMEF$serv.hig)
levels(CensoMEF$serv.hig)

## alumbrado publico
EndesMEF$V119
EndesMEF = EndesMEF %>% rename(electricidad = V119)
CensoMEF = CensoMEF %>% rename(electricidad = C2_P11)

EndesMEF$electricidad = ifelse(EndesMEF$electricidad == "No residente de jure",NA,
                               ifelse(EndesMEF$electricidad=="Si",1,0))
CensoMEF$electricidad = ifelse(CensoMEF$electricidad=="Si tiene alumbrado eléctrico",1,0)


## numero de habitaciones

EndesMEF = EndesMEF %>% rename(nro.hab = SH72)
CensoMEF = CensoMEF %>% rename(nro.hab = C2_P12)
CensoMEF$nro.hab = as.numeric(CensoMEF$nro.hab)

## Combustible para cocinar

levels(EndesMEF$V161)

# con electricidad

EndesMEF$V161 == "Electricidad"
CensoMEF$C3_P1_1


EndesMEF$c.electr = ifelse(EndesMEF$V161 == "No residente de jure",NA,
  ifelse(EndesMEF$V161 == "Electricidad",1,0))
CensoMEF = CensoMEF %>% rename(c.eletr = C3_P1_1)
CensoMEF$c.eletr = ifelse(CensoMEF$c.eletr == "Si usa electricidad",1,0)


# Con Gas glp

EndesMEF$c.glp = ifelse(EndesMEF$V161 == "No residente de jure",NA,
                           ifelse(EndesMEF$V161 == "GLP",1,0))
CensoMEF = CensoMEF %>% rename(c.glp = C3_P1_2)
CensoMEF$c.glp = ifelse(CensoMEF$c.glp == "Si usa gas (balón GLP)",1,0)

# Con Gas natural

EndesMEF$c.gasn = ifelse(EndesMEF$V161 == "No residente de jure",NA,
                        ifelse(EndesMEF$V161 == "Gas Natural",1,0))
CensoMEF = CensoMEF %>% rename(c.gasn = C3_P1_3)
CensoMEF$c.gasn = ifelse(CensoMEF$c.gasn == "Si usa gas natural",1,0)

# Carbón

EndesMEF$c.carbon = ifelse(EndesMEF$V161 == "No residente de jure",NA,
                         ifelse(EndesMEF$V161 == "Carbón, lignito"|EndesMEF$V161 == "Carbón",1,0))
CensoMEF = CensoMEF %>% rename(c.carbon = C3_P1_4)
CensoMEF$c.carbon = ifelse(CensoMEF$c.carbon == "Si usa carbón",1,0)

# Leña 

EndesMEF$c.lena = ifelse(EndesMEF$V161 == "No residente de jure",NA,
                           ifelse(EndesMEF$V161 == "Madera",1,0))
CensoMEF = CensoMEF %>% rename(c.lena = C3_P1_5)
CensoMEF$c.lena = ifelse(CensoMEF$c.lena == "Si usa leña",1,0)

levels(EndesMEF$V161)
levels(CensoMEF$C3_P1_5)

# Bosta 

EndesMEF$c.bosta = ifelse(EndesMEF$V161 == "No residente de jure",NA,
                         ifelse(EndesMEF$V161 == "Estiércol de animales",1,0))
CensoMEF = CensoMEF %>% rename(c.bosta = C3_P1_6)
CensoMEF$c.bosta = ifelse(CensoMEF$c.bosta == "Si usa bosta, estiércol",1,0)

levels(EndesMEF$V161)
levels(CensoMEF$C3_P1_6)

# otro 

EndesMEF$c.otro = ifelse(EndesMEF$V161 == "No residente de jure",NA,
                          ifelse(EndesMEF$V161 == "Otro"|
                                   EndesMEF$V161 == "Cultivos agrícolas"|
                                   EndesMEF$V161 == "Kerosene"|
                                   EndesMEF$V161 == "Paja / arbustos / hierba",1,0))
CensoMEF = CensoMEF %>% rename(c.otro = C3_P1_7)
CensoMEF$c.otro = ifelse(CensoMEF$c.otro == "Si usa otros (residuos..)",1,0)

levels(EndesMEF$V161)
levels(CensoMEF$C3_P1_7)

# otro 

EndesMEF$c.nococina = ifelse(EndesMEF$V161 == "No residente de jure",NA,
                         ifelse(EndesMEF$V161 == "Alimento no cocinado en HH",1,0))
CensoMEF = CensoMEF %>% rename(c.nococina = C3_P1_8)
CensoMEF$c.nococina = ifelse(CensoMEF$c.nococina == "No cocinan",1,0)

levels(EndesMEF$V161)
levels(CensoMEF$C3_P1_8)

## Hogar tiene


# equipo de sonido
# Television
# Cocina a gas
# Refrigerador
# Lavadora
# Microondas
# Licuadora
# Computadora
# Celular
# Tv cable
# Conexion a internet
# Automovil, camioneta
# Motocicleta
# Lancha, bote a motor

EndesMEF = EndesMEF %>% rename(
  t.sonido = V120, t.tv = V121, t.cocgas = SH61L, t.refri = V122,
  t.lavadora = SH61O, t.microondas = SH61N, t.licuadora = SH61K,
  t.pc = SH61P, t.celular = HV243A, t.tvcable = SH61J, t.autocam = V125,
  t.moto = V124, t.lancha = HV243D, t.internet = SH61Q)
EndesMEF = EndesMEF %>% 
  mutate(
  t.sonido = ifelse(EndesMEF$t.sonido == "No residente de jure",NA,
         ifelse(EndesMEF$t.sonido == "Si",1,0)),
  t.tv = ifelse(EndesMEF$t.tv == "No residente de jure",NA,
                    ifelse(EndesMEF$t.tv == "Si",1,0)),
  t.refri = ifelse(EndesMEF$t.refri == "No residente de jure",NA,
                    ifelse(EndesMEF$t.refri == "Si",1,0)),
  t.autocam = ifelse(EndesMEF$t.sonido == "No residente de jure",NA,
                    ifelse(EndesMEF$t.autocam == "Si",1,0)),
  t.moto = ifelse(EndesMEF$t.sonido == "No residente de jure",NA,
                    ifelse(EndesMEF$t.moto == "Si",1,0)),
  t.cocgas = ifelse(EndesMEF$t.cocgas == "Si",1,0),
  t.lavadora = ifelse(EndesMEF$t.lavadora == "Si",1,0),
  t.microondas = ifelse(EndesMEF$t.microondas == "Si",1,0),
  t.licuadora = ifelse(EndesMEF$t.licuadora == "Si",1,0),
  t.pc = ifelse(EndesMEF$t.pc == "Si",1,0),
  t.celular = ifelse(EndesMEF$t.celular == "Si",1,0),
  t.tvcable = ifelse(EndesMEF$t.tvcable == "Si",1,0),
  t.lancha = ifelse(EndesMEF$t.lancha == "Si",1,0),
  t.internet = ifelse(EndesMEF$t.internet == "Si",1,0))

CensoMEF = CensoMEF %>% rename(
  t.sonido = C3_P2_1, t.tv = C3_P2_2, t.cocgas = C3_P2_3, t.refri = C3_P2_4,
  t.lavadora = C3_P2_5, t.microondas = C3_P2_6, t.licuadora = C3_P2_7,
  t.pc = C3_P2_9, t.celular = C3_P2_10, t.tvcable = C3_P2_12, t.autocam = C3_P2_14,
  t.moto = C3_P2_15, t.lancha = C3_P2_16, t.internet = C3_P2_13)

CensoMEF = CensoMEF %>% mutate(
  t.sonido = ifelse(startsWith(as.character(CensoMEF$t.sonido),"Si"),1,0),
  t.tv = ifelse(startsWith(as.character(CensoMEF$t.tv),"Si"),1,0) ,
  t.refri = ifelse(startsWith(as.character(CensoMEF$t.refri),"Si"),1,0),
  t.autocam = ifelse(startsWith(as.character(CensoMEF$t.autocam),"Si"),1,0),
  t.moto = ifelse(startsWith(as.character(CensoMEF$t.moto),"Si"),1,0),
  t.cocgas = ifelse(startsWith(as.character(CensoMEF$t.cocgas),"Si"),1,0) ,
  t.lavadora = ifelse(startsWith(as.character(CensoMEF$t.lavadora),"Si"),1,0),
  t.microondas = ifelse(startsWith(as.character(CensoMEF$t.microondas),"Si"),1,0),
  t.licuadora = ifelse(startsWith(as.character(CensoMEF$t.licuadora),"Si"),1,0),
  t.pc = ifelse(startsWith(as.character(CensoMEF$t.pc),"Si"),1,0),
  t.celular = ifelse(startsWith(as.character(CensoMEF$t.celular),"Si"),1,0),
  t.tvcable = ifelse(startsWith(as.character(CensoMEF$t.tvcable),"Si"),1,0),
  t.lancha = ifelse(startsWith(as.character(CensoMEF$t.lancha),"Si"),1,0),
  t.internet = ifelse(startsWith(as.character(CensoMEF$t.internet),"Si"),1,0))

## Relacion de parentesco

levels(CensoMEF$C5_P1)
levels(EndesMEF$V150)

CensoMEF$parentco = as.character(CensoMEF$C5_P1)
CensoMEF$parentco[which(CensoMEF$C5_P1=="Esposo(a) / compaÃ±ero(a)")] = 
  "Esposo(a) / compañero(a)"

CensoMEF$parentco = as.factor(CensoMEF$parentco)
levels(CensoMEF$parentco)


EndesMEF$parentco = as.factor(
  ifelse(EndesMEF$V150 == "Jefe", "Jefe o jefa del hogar",
         ifelse(EndesMEF$V150 == "Esposa"|EndesMEF$V150 == "Conviviente",  "Esposo(a) / compañero(a)",
           ifelse(EndesMEF$V150 == "Hija"|EndesMEF$V150 == "Niño Adoptado", "Hijo(a) / hijastro(a)",
             ifelse(EndesMEF$V150 == "Nuera","Yerno / nuera",
               ifelse(EndesMEF$V150 == "Nieta", "Nieto(a)",
                ifelse(EndesMEF$V150 == "Madre"|EndesMEF$V150 == "Suegra", "Padre / madre / suegro(a)",
                  ifelse(EndesMEF$V150 == "Hermana", "Hermano(a)",
                    ifelse(EndesMEF$V150 == "Otro pariente", "Otro(a) pariente",
                      ifelse(EndesMEF$V150 == "No pariente", "Otro(a) no pariente",
                        ifelse(EndesMEF$V150 == "Trabajador del hogar", "Trabajador(a) del hogar", NA
                               )))))))))) 
)
# Sexo jefe hogar
sjc = readRDS("SexoJefeCenso.rds")
sfe = foreign::read.spss("2018 ENDES Peru-20190701T133902Z-001/2018 ENDES Peru/638-Modulo66/638-Modulo66/REC0111_MODULO66.SAV.SAV",to.data.frame = TRUE)
sfe = sfe %>% select(CASEID,V151)
CensoMEF = CensoMEF %>% left_join(sjc,by = "ID_HOG_IMP_F")
EndesMEF = EndesMEF %>% left_join(sfe, by = "CASEID")

levels(CensoMEF$sexoJefe)
levels(EndesMEF$V151)

CensoMEF = CensoMEF %>% mutate(jefe.M = ifelse(CensoMEF$sexoJefe=="Hombre",0,1)) %>% select(-sexoJefe)
EndesMEF = EndesMEF %>% mutate(jefe.M = ifelse(EndesMEF$V151 == "Hombre",0,1)) %>% select(-V151)
sum(is.na(CensoMEF$jefe.M)) # 227133 NA's :(

IDVIV = CensoMEF$ID_VIV_IMP_F.x[which(is.na(CensoMEF$sexoJefe))]

NASViv = ViviendasCenso[which(IDVIV%in%ViviendasCenso$ID_VIV_IMP_F),]
knitr::kable(table(NASViv$C2_P1))
## Edad quinquenal

EndesMEF$EdadQ = EndesMEF$V013
CensoMEF$EdadQ = cut(CensoMEF$edad, c(14,19,24,29,34,39,44,49))
levels(CensoMEF$EdadQ) = levels(EndesMEF$V013)[-1]


## Edad
CensoMEF$edad
EndesMEF$edad = EndesMEF$V012

## Vive permanentemente en este distrito
EndesMEF = EndesMEF %>% rename(distrpermanente = HV102)
EndesMEF = EndesMEF %>% mutate(distrpermanente = ifelse(EndesMEF$distrpermanente == "Si",1,0))
CensoMEF = CensoMEF %>% mutate(distrpermanente = ifelse(startsWith(as.character(CensoMEF$C5_P5),"Si"),1,0)) %>%
  select(-C5_P5)

## se encuentra afiliado a

# Seguro integral de salud SIS
# ESSALUD
# fuerza armada
# seguro privado
# otro seguro (compañia de seguro??)
# ninguno

EndesMEF = EndesMEF %>% rename(
  afil.sis = SH11C, afil.essalud = SH11A, afil.militar = SH11B,
  afil.privado = SH11D, afil.ninguno = SH11Z)


EndesMEF = EndesMEF %>% mutate(
 afil.sis = ifelse(EndesMEF$afil.sis== "No sabe", NA,
                   ifelse(EndesMEF$afil.sis == "Si",1,0)),
 afil.essalud = ifelse(EndesMEF$afil.essalud == "No sabe", NA,
                   ifelse(EndesMEF$afil.essalud == "Si",1,0)),
 afil.militar = ifelse(EndesMEF$afil.militar == "No sabe", NA,
                   ifelse(EndesMEF$afil.militar == "Si",1,0)),
 afil.privado = ifelse(EndesMEF$afil.privado == "No sabe", NA,
                   ifelse(EndesMEF$afil.privado == "Si",1,0)),
 afil.ninguno = ifelse(EndesMEF$afil.ninguno == "No sabe", NA,
                   ifelse(EndesMEF$afil.ninguno == "Si",1,0))
 
)

CensoMEF = CensoMEF %>% rename(
  afil.sis = C5_P8_1, afil.essalud = C5_P8_2, afil.militar = C5_P8_3,
  afil.privado = C5_P8_4, afil.ninguno = C5_P8_6
)

CensoMEF = CensoMEF %>% mutate(
  afil.sis = ifelse(startsWith(as.character(CensoMEF$afil.sis),"Si"),1,0),
  afil.essalud = ifelse(startsWith(as.character(CensoMEF$afil.essalud),"Si"),1,0),
  afil.militar = ifelse(startsWith(as.character(CensoMEF$afil.militar),"Si"),1,0),
  afil.privado = ifelse(startsWith(as.character(CensoMEF$afil.privado),"Si"),1,0),
  afil.ninguno = ifelse(startsWith(as.character(CensoMEF$afil.ninguno),"No"),1,0),
)

## si esta afiliada

EndesMEF$afiliada = ifelse(EndesMEF$afil.ninguno==0,1,0)
CensoMEF$afilidad = ifelse(CensoMEF$afil.ninguno==0,1,0)

## Discapacidad

EndesMEF = EndesMEF %>% mutate(
  dis.ver = ifelse(EndesMEF$QD333_1 == "Si",1,0),
  dis.oir = ifelse(EndesMEF$QD333_2 == "Si",1,0),
  dis.hablar = ifelse(EndesMEF$QD333_3 == "Si",1,0),
  dis.moverse = ifelse(EndesMEF$QD333_4 == "Si",1,0),
  dis.entender = ifelse(EndesMEF$QD333_5 == "Si",1,0),
  dis.relacionarse = ifelse(EndesMEF$QD333_6 == "Si",1,0),
) %>% select(-QD333_1,-QD333_2,-QD333_3,-QD333_4,-QD333_5,-QD333_6)

CensoMEF = CensoMEF %>% mutate(
  dis.ver = ifelse(startsWith(as.character(CensoMEF$C5_P9_1),"Si"),1,0),
  dis.oir = ifelse(startsWith(as.character(CensoMEF$C5_P9_2),"Si"),1,0),
  dis.hablar = ifelse(startsWith(as.character(CensoMEF$C5_P9_3),"Si"),1,0),
  dis.moverse = ifelse(startsWith(as.character(CensoMEF$C5_P9_4),"Si"),1,0),
  dis.entender = ifelse(startsWith(as.character(CensoMEF$C5_P9_5),"Si"),1,0),
  dis.relacionarse = ifelse(startsWith(as.character(CensoMEF$C5_P9_6),"Si"),1,0)) %>%
    select(-C5_P9_1,-C5_P9_2,-C5_P9_3,-C5_P9_4,-C5_P9_5,-C5_P9_6)



## Estado Civil/Conyugal
levels(EndesMEF$HV115)
levels(CensoMEF$C5_P24)

# casado
EndesMEF$ec.casado = ifelse(EndesMEF$HV115=="Casado (a)",1,0)
CensoMEF$ec.casado = ifelse(CensoMEF$C5_P24 == "Casado/a",1,0)


# Conviviente
EndesMEF$ec.conviviente = ifelse(EndesMEF$HV115=="Conviviente",1,0)
CensoMEF$ec.conviviente = ifelse(CensoMEF$C5_P24 == "Conviviente",1,0)

# casado
EndesMEF$ec.separada = ifelse(EndesMEF$HV115=="Separado(a)",1,0)
CensoMEF$ec.separada = ifelse(CensoMEF$C5_P24 == "Separado/a",1,0)
# casado
EndesMEF$ec.viuda = ifelse(EndesMEF$HV115=="Viudo (a)",1,0)
CensoMEF$ec.viuda = ifelse(CensoMEF$C5_P24 == "Viudo/a",1,0)
# casado
EndesMEF$ec.divorciada = ifelse(EndesMEF$HV115=="Divorciad (a)",1,0)
CensoMEF$ec.divorciada = ifelse(CensoMEF$C5_P24 == "Divorciado/a",1,0)
# casado
EndesMEF$ec.soltera = ifelse(EndesMEF$HV115=="Nunca casado (a)",1,0)
CensoMEF$ec.soltera = ifelse(CensoMEF$C5_P24 == "Soltero/a",1,0)

EndesMEF = EndesMEF %>% select(-HV115)
CensoMEF = CensoMEF %>% select(-C5_P24)

Censo$union_1 = ifelse(Censo$ec.conviviente==1|Censo$ec.casado==1,"Unida","No unida")



# total nacimientos e hijos vivos

EndesMEF = EndesMEF %>% rename(tot.naci = V201) %>% 
  mutate(hijos.vivos = tot.naci - (V206+V207))

CensoMEF$tot.naci = ifelse(CensoMEF$C5_P27 == "99",NA,as.numeric(CensoMEF$C5_P27))
CensoMEF$hijos.vivos =  ifelse(CensoMEF$C5_P28 == "99",NA,as.numeric(CensoMEF$C5_P28))

sum(is.na(CensoMEF$jefe.M))
table(CensoMEF$parentco)

## Nivel de estudio

CensoMEF$C5_P13_ANIO_ESP
CensoMEF$C5_P13_ANIO_PRI
CensoMEF$C5_P13_ANIO_SEC
CensoMEF$C5_P13_GRA
levels(CensoMEF$C5_P13_NIV)
levels(EndesMEF$S108N)
levels(EndesMEF$S108G)
levels(EndesMEF$S108Y)

# usar HV109-S108N y C5_P13_NIV ?

levels(CensoMEF$C5_P13_NIV)

table(CensoMEF$C5_P13_NIV)

levels(EndesMEF$HV109)
levels(EndesMEF$S108N)


sum(is.na(CensoMEF$C5_P13_NIV))

CensoMEF = CensoMEF %>% mutate(
  nivel.educ = ifelse(CensoMEF$C5_P13_NIV%in%c("Sin Nivel", "Inicial"), "Sin nivel o Inicial",
                ifelse(CensoMEF$C5_P13_NIV%in%c("Primaria", "BÃ¡sica especial"),"Primaria",
                 ifelse(CensoMEF$C5_P13_NIV == "Secundaria","Secundaria",
                  ifelse(CensoMEF$C5_P13_NIV%in%c("Superior no universitaria incompleta", "Superior no universitaria completa"),"Superior no universitaria",
                   ifelse(CensoMEF$C5_P13_NIV%in%c("Superior universitaria incompleta", "Superior universitaria completa"), "Superior Universitaria","Postgrado")))))
) %>% select(-C5_P13_NIV,-C5_P13_ANIO_ESP,-C5_P13_ANIO_PRI,-C5_P13_ANIO_SEC,-C5_P13_GRA)
CensoMEF$nivel.educ = as.factor(CensoMEF$nivel.educ)
levels(CensoMEF$nivel.educ)

EndesMEF = EndesMEF %>% rename(nivel.educ = S108N) 
EndesMEF$nivel.educ = ifelse()

## etnia se considera

levels(EndesMEF$S119D)
levels(CensoMEF$C5_P25_I_MC)
table(CensoMEF$C5_P25_I_MC)
table(EndesMEF$S119D)

EndesMEF$etnia = EndesMEF$S119D

CensoMEF$etnianum = as.numeric(CensoMEF$C5_P25_I_OLD)
CensoMEF$etnia = as.factor(
  ifelse(CensoMEF$etnianum%in%c(8,14,15),"OTRO",
   ifelse(CensoMEF$etnianum==1,"QUECHUA",
    ifelse(CensoMEF$etnianum==2,"AYMARA",
     ifelse(CensoMEF$etnianum%in%c(3,10,11,13,12),"NATIVO O INDIGENA DE LA AMAZONIA",
      ifelse(CensoMEF$etnianum==4,"PERTENECIENTE O PARTE DE OTRO PUEBLO INDIGENA",
       ifelse(CensoMEF$etnianum==5,"NEGRO /MORENO/ ZAMBO/MULATO/PUEBLO AFROPERUANO",
        ifelse(CensoMEF$etnianum==6,"BLANCO",
         ifelse(CensoMEF$etnianum==7,"MESTIZO",NA))))))))
)

##############################################
## Variables para el calculo de indicadores ##
##############################################

## Ajuste ponderador

# considerando que mujeres entre 12-14 años tienen ponderador
EndesMEF$FEXT = EndesMEF$V005*8620020/sum(EndesMEF$V005)

# Considerando solo a las mujeres de 15-49 pero incluyendo a las demas pues poseen el ponderador V005
EndesMEF$FEXM = EndesMEF$V005*7845787/sum(EndesMEF$V005)
levels(EndesMEF$V501)
levels(EndesMEF$V502)


## Mujeres en UNION

EndesMEF$union_1 = ifelse(EndesMEF$V501%in%c("Casada","Conviviente"),"Unida","No unida")
EndesMEF$union_2 = ifelse(EndesMEF$V502=="Actualmente casada","Unida","No unida")

## Uso metodo anticonceptivos

levels(EndesMEF$V313)

EndesMEF$usametodo = ifelse(EndesMEF$V313%in%c("Método moderno","Método tradicional","Método Folclórico"),1,0)
EndesMEF$usamoderno = ifelse(EndesMEF$V313=="Método moderno",1,0)
EndesMEF$metodo = EndesMEF$V313
## Mujeres sexualmente activas

EndesMEF$MSA = ifelse(EndesMEF$V536 == "Actividad ult. 4 semanas","Si","No")
EndesMEF$act.sex = EndesMEF$V536
### 

levels(EndesMEF$V626)

## Necesidad insatisfecha de planificacion familiar

EndesMEF$Nec_ins_pf_E = ifelse(EndesMEF$V626=="Necesidad no satisfecha de espacio",1,0)
EndesMEF$Nec_ins_pf_L = ifelse(EndesMEF$V626 == "Necesidad no satisfecha de límite",1,0)
EndesMEF$Nec_ins_pf_T = EndesMEF$Nec_ins_pf_E + EndesMEF$Nec_ins_pf_L

## Necesidad satisfecha de planificacion familiar

EndesMEF$Nec_sat_pf_E = ifelse(EndesMEF$V626=="Uso de espacio",1,0)
EndesMEF$Nec_sat_pf_L = ifelse(EndesMEF$V626 == "Uso de límite",1,0)
EndesMEF$Nec_sat_pf_T = EndesMEF$Nec_sat_pf_E + EndesMEF$Nec_sat_pf_L

## Demanda total de planificacion familiar

EndesMEF$Dem_tot_pf_E = ifelse(EndesMEF$V626%in%c("Necesidad no satisfecha de espacio","Uso de espacio","Falla de espacio"),1,0)
EndesMEF$Dem_tot_pf_L = ifelse(EndesMEF$V626%in%c("Necesidad no satisfecha de límite","Uso de límite","Falla de límite"),1,0)
EndesMEF$Dem_tot_pf_T = EndesMEF$Dem_tot_pf_E + EndesMEF$Dem_tot_pf_L

## Falla del método

EndesMEF$Falla_met = ifelse(EndesMEF$V626%in%c("Falla de espacio","Falla de límite"),1,0)

## Demanda satisfecha (de uso de métodos anticonceptivos)
levels(EndesMEF$V626)
table(EndesMEF$V626)
table(as.numeric(EndesMEF$V626))

EndesMEF$DS = ifelse(as.numeric(EndesMEF$V626)%in%c(4,5,6,7),1,
               ifelse(as.numeric(EndesMEF$V626)%in%c(2,3),0,NA))


### modificacion niveles 

EndesMEF = EndesMEF %>% filter(EdadQ!="12-14")
EndesMEF$EdadQ = as.factor(as.character(EndesMEF$EdadQ))

# CensoMEF = filter(CensoMEF,!is.na(CensoMEF$parentco))

## CONDICION DE OCUPACION
Endes
# CENSO

levels(CensoMEF$C5_P16)
levels(CensoMEF$C5_P17)
levels(CensoMEF$C5_P18)
levels(CensoMEF$C5_P21)

CensoMEF$condact = as.factor(
  ifelse(as.numeric(CensoMEF$C5_P16)==1|
         as.numeric(CensoMEF$C5_P17)%in%c(1,2,3,4,5),"Ocupada",
   ifelse(as.numeric(CensoMEF$C5_P16)==2&
          as.numeric(CensoMEF$C5_P18)==1,"Desocupada",
     ifelse(as.numeric(CensoMEF$C5_P18)==2,"Inactiva",NA))
))

table(CensoMEF$condact)

1-sum(as.numeric(CensoMEF$condact)==3)/(sum(as.numeric(CensoMEF$condact)==3)+sum(as.numeric(CensoMEF$condact)==1))

summary(CensoMEF$C5_P18)
# ENDES
levels(EndesMEF$SH13)
table(EndesMEF$SH13)

EndesMEF$condact = as.factor(
  ifelse(as.numeric(EndesMEF$SH13)%in%c(1,2,3,4,7),"Ocupada",
   ifelse(as.numeric(EndesMEF$SH13)==5,"Desocupada","Inactiva")
))
table(EndesMEF$condact)

#### Creacion variable de hacinamiento


haci.censo = readRDS("Rdata/IndiceHacinamientoCenso.rds")
haci.censo = select(haci.censo,ID_VIV_IMP_F, nro.personas = nro.pero,
                    ind.hacinamiento = indice.haci)

CensoMEF = CensoMEF %>% rename(ID_VIV_IMP_F = ID_VIV_IMP_F.x) %>%
  left_join(haci.censo, by = "ID_VIV_IMP_F")

haci.endes = readRDS("IndiceHacinamientoEndes.rds")
haci.endes = haci.endes %>% select(-SH72,HHID,nro.personas = HV009,ind.hacinamiento)
EndesMEF = EndesMEF %>% left_join(haci.endes, by = "HHID")
summary(EndesMEF$ind.hacinamiento)


Censo$etniamap = as.factor(ifelse(Censo$etnia%in%c("QUECHUA","AYMARA","NATIVO O INDIGENA DE LA AMAZONIA","PERTENECIENTE O PARTE DE OTRO PUEBLO INDIGENA"),"Origen nativo",
                                 ifelse(Censo$etnia%in%c("OTRO","NO SABE"),"OTRO/NO SABE",as.character(Censo$etnia))))


Endes$etniamap = as.factor(ifelse(Endes$etnia%in%c("QUECHUA","AYMARA","NATIVO O INDIGENA DE LA AMAZONIA","PERTENECIENTE O PARTE DE OTRO PUEBLO INDIGENA"),"Origen nativo",
                                 ifelse(Endes$etnia%in%c("OTRO","NO SABE"),"OTRO/NO SABE",as.character(Endes$etnia))))

Censo$nivel.educmap = as.factor(ifelse(Censo$nivel.educ%in%c("Superior Universitaria","Postgrado"),"Superior",
                                      as.character(Censo$nivel.educ)))                          

Endes$nivel.educmap = as.factor(ifelse(Endes$nivel.educ%in%c("Superior universitario","Postgrado"),"Superior",
                                      as.character(Endes$nivel.educ)))








