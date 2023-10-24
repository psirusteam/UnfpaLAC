table(ENSANUT18$f2_s2_202_1)
### >12 meses
ENSANUT18$f2_s2_202_1[is.na(ENSANUT18$f2_s2_202_1)] <- 0

ENSANUT18$tiempo_espera_EAM= ifelse(ENSANUT18$f2_s2_202_1  >= 1, 1,0)

#######

table(ENSANUT18$f2_s2_202_2)
### >1 año
ENSANUT18$f2_s2_202_2[is.na(ENSANUT18$f2_s2_202_2)] <- 0

ENSANUT18$tiempo_espera_EAA= ifelse(ENSANUT18$f2_s2_202_2  >= 1, 1,0)

#######

table(ENSANUT18$f2_s6_613)
### la pareja se opone?
### no conoce/no tiene información sobre métodos anticonceptivos?
### por motivos económicos
### por pena o vergüenza 

#######
ENSANUT18$f2_s6_613[is.na(ENSANUT18$f2_s6_613)] <- 0

ENSANUT18$Razon_no_uso_met= ifelse(ENSANUT18$f2_s6_613  =="la pareja se opone?", 1,
                                   ifelse(ENSANUT18$f2_s6_613  =="no conoce/no tiene información sobre métodos anticonceptivos?",1,
                                          ifelse(ENSANUT18$f2_s6_613  =="por motivos económicos",1,
                                                 ifelse(ENSANUT18$f2_s6_613  =="por pena o vergüenza",1,0))))

#######

table(ENSANUT18$f2_s6_614)
### Opción si
ENSANUT18$f2_s6_614[is.na(ENSANUT18$f2_s6_614)] <- 0

ENSANUT18$Desea_MA= ifelse(ENSANUT18$f2_s6_614 == "si", 1,0)

######


table(ENSANUT18$f2_s6_617) #*****
### Opción no
ENSANUT18$f2_s6_617[is.na(ENSANUT18$f2_s6_617)] <- 0

ENSANUT18$Obten_MA= ifelse(ENSANUT18$f2_s6_617 == "no", 1,0)

#######

table(ENSANUT18$f2_s6_618)
### junta de beneficencia
### Partera
### fundación / ong
### hospital/clínica privada 
### consultorio particular ***
### farmacia o botica
### no sabe/ no responde
ENSANUT18$f2_s6_618[is.na(ENSANUT18$f2_s6_618)] <- 0

ENSANUT18$Donde_Obt_Metod= ifelse(ENSANUT18$f2_s6_618  =="junta de beneficencia*", 1,
                                  ifelse(ENSANUT18$f2_s6_618  =="partera",1,
                                         ifelse(ENSANUT18$f2_s6_618  =="fundación / ong**",1,
                                                ifelse(ENSANUT18$f2_s6_618  =="hospital/clínica privada",1,
                                                       ifelse(ENSANUT18$f2_s6_618  =="consultorio particular",1,
                                                              ifelse(ENSANUT18$f2_s6_618  =="farmacia o botica",1,
                                                                     ifelse(ENSANUT18$f2_s6_618  =="no sabe/ no responde",1,0)))))))



########

table(ENSANUT18$f2_s6_619)
### >120
ENSANUT18$f2_s6_619[is.na(ENSANUT18$f2_s6_619)] <- 0

ENSANUT18$tiempoMin_obten_MA= ifelse(ENSANUT18$f2_s6_619  >= 60, 1,0)

########

table(ENSANUT18$f2_s6_619_1)
### >2 horas
ENSANUT18$f2_s6_619_1[is.na(ENSANUT18$f2_s6_619_1)] <- 0

ENSANUT18$tiempoHor_obten_MA= ifelse(ENSANUT18$f2_s6_619_1  >= 1, 1,0)

########

table(ENSANUT18$f2_s6_623)
### su esposo/pareja?
### familiares?
ENSANUT18$f2_s6_623[is.na(ENSANUT18$f2_s6_623)] <- 0

ENSANUT18$Decid_lig= ifelse(ENSANUT18$f2_s6_623  =="esposo/pareja?", 1,
                            ifelse(ENSANUT18$f2_s6_623  =="familiares?",1,0))

########


table(ENSANUT18$f2_s6_624)
### Opción no
ENSANUT18$f2_s6_624[is.na(ENSANUT18$f2_s6_624)] <- 0

ENSANUT18$Tomar_misma_dec= ifelse(ENSANUT18$f2_s6_624  == "no", 1, 
                                  ifelse(ENSANUT18$f2_s6_624  == "no sabe/ no responde", 1,0))
#######


table(ENSANUT18$f2_s6_625)
### junta de beneficencia
### Partera
### fundación / ong
### consultorio particular ***
ENSANUT18$f2_s6_625[is.na(ENSANUT18$f2_s6_625)] <- 0

ENSANUT18$Lugar_obten_met= ifelse(ENSANUT18$f2_s6_625  =="junta de beneficencia*", 1,
                                  ifelse(ENSANUT18$f2_s6_625  =="partera",1,
                                         ifelse(ENSANUT18$f2_s6_625  =="fundación / ong**",1,
                                                ifelse(ENSANUT18$f2_s6_625  =="hospital/clínica privada",1,
                                                       ifelse(ENSANUT18$f2_s6_625  =="consultorio particular",1,
                                                              ifelse(ENSANUT18$f2_s6_625  =="farmacia o botica",1,
                                                                     ifelse(ENSANUT18$f2_s6_625  =="no sabe/ no responde",1,0)))))))






########

table(ENSANUT18$f2_s6_626)
### >60
ENSANUT18$f2_s6_626[is.na(ENSANUT18$f2_s6_626)] <- 0

ENSANUT18$Met_obt_min= ifelse(ENSANUT18$f2_s6_626  >= 60, 1,0)

#######

table(ENSANUT18$f2_s6_626_1)
### >1
ENSANUT18$f2_s6_626_1[is.na(ENSANUT18$f2_s6_626_1)] <- 0

ENSANUT18$Met_obt_hor= ifelse(ENSANUT18$f2_s6_626_1  >= 1, 1,0)

#######

table(ENSANUT18$f2_s6_628)
### sólo su pareja?
### otra persona? 
ENSANUT18$f2_s6_628[is.na(ENSANUT18$f2_s6_628)] <- 0

ENSANUT18$Q_toma_decis= ifelse(ENSANUT18$f2_s6_628  == "sólo su pareja?", 1, 
                               ifelse(ENSANUT18$f2_s6_628  == "otra persona?", 1,0))


######

table(ENSANUT18$f2_s7_701)
### no quiere hijos/ más hijos/as
ENSANUT18$f2_s7_701[is.na(ENSANUT18$f2_s7_701)] <- 0

ENSANUT18$Mas_hijos= ifelse(ENSANUT18$f2_s7_701  == "no quiere hijos/ más hijos/as", 1, 
                            ifelse(ENSANUT18$f2_s7_701  == "indecisa", 1,0))


#######

##### Calculo de Número de hijos por mujer 

table(ENSANUT18$f2_s2_217_1) ### Total Hijos en Casa 
table(ENSANUT18$f2_s2_217_2) ### Total Hijos  fuera de casa


ENSANUT <- ENSANUT %>%
  dplyr::rowwise() %>%
  dplyr::mutate(Total_hijos_M = sum(f2_s2_217_1, f2_s2_217_2,
                                    na.rm = TRUE))
###

table(ENSANUT18$f2_s7_703)
### Hacer la diferencia respecto del número de hijos

ENSANUT <- ENSANUT %>%
  dplyr::rowwise() %>%
  dplyr::mutate(Diff_hijos = ((f2_s7_703)-(Total_hijos_M)))


ENSANUT18$Hijos_deseo = ifelse(ENSANUT18$Diff_hijos < 0, 1,0)
ENSANUT18$Hijos_deseo[is.na(ENSANUT18$Hijos_deseo)] <- 0                          

#######

table(ENSANUT18$f2_s8_800f)
###  no 
###  no sabe
ENSANUT18$f2_s8_800f[is.na(ENSANUT18$f2_s8_800f)] <- 0

ENSANUT18$Info_metod= ifelse(ENSANUT18$f2_s8_800f  == "no", 1, 
                             ifelse(ENSANUT18$f2_s8_800f  == "no sabe/ no responde", 1,0))


######

table(ENSANUT18$f2_s8_837)
### se molestaría?
### no aceptaría?
ENSANUT18$f2_s8_837[is.na(ENSANUT18$f2_s8_837)] <- 0

ENSANUT18$UsoCondon_Par= ifelse(ENSANUT18$f2_s8_837  == "se molestaría?", 1, 
                                ifelse(ENSANUT18$f2_s8_837  == "no aceptaría?", 1,0))



table(ENSANUT18$f2_s6_618)
### centro clínico quirúrgico ambulatorio iess
### dispensario seguro campesino
### establecimientos de salud del msp
### hospital/policlínico - ffaa/policía
### consejo provincial / unidad municipal de salud

ENSANUT18$Donde_Obt_Metod_ns= ifelse(ENSANUT18$f2_s6_618  =="centro clínico quirúrgico ambulatorio iess", 1,
                                     ifelse(ENSANUT18$f2_s6_618  =="dispensario seguro campesino",1,
                                            ifelse(ENSANUT18$f2_s6_618  =="establecimientos de salud del msp",1,
                                                   ifelse(ENSANUT18$f2_s6_618  =="hospital/policlínico - ffaa/policía",1,
                                                          ifelse(ENSANUT18$f2_s6_618  =="consejo provincial / unidad municipal de salud",1,
                                                                 ifelse(ENSANUT18$f2_s6_618  =="hospital de especialidades del iess",1,0))))))



########


table(ENSANUT18$f2_s6_623)
### usted y su esposo/pareja?
### usted, esposo/pareja y médico?
### usted?

ENSANUT18$Decid_lig_ns= ifelse(ENSANUT18$f2_s6_623  =="usted y su esposo/pareja?", 1,
                               ifelse(ENSANUT18$f2_s6_623  =="usted?",1,
                                      ifelse(ENSANUT18$f2_s6_623  =="usted, esposo/pareja y médico?",1,
                                             ifelse(ENSANUT18$f2_s6_623  =="médico?",1,0))))
######

table(ENSANUT18$f2_s6_624)
### Opción si
ENSANUT18$Tomar_misma_dec_ns= ifelse(ENSANUT18$f2_s6_624  == "si", 1,0) 



table(ENSANUT18$f2_s6_627)
### para espaciar o posponer los embarazos?
### no quiere tener hijos/as?
### ya no quiere tener más hijos/as?
ENSANUT18$f2_s6_627[is.na(ENSANUT18$f2_s6_627)] <- 0

ENSANUT18$Raz_Uso_Metod_ns= ifelse(ENSANUT18$f2_s6_627  == "para espaciar o posponer los embarazos?", 1,
                                   ifelse(ENSANUT18$f2_s6_627  == "no quiere tener hijos/as?", 1,
                                          ifelse(ENSANUT18$f2_s6_627  == "ya no quiere tener más hijos/as?", 1,0))) 

########

table(ENSANUT18$f2_s6_628)
### ambos (usted y su pareja)? 
### sólo usted?

ENSANUT18$Q_toma_decis_ns= ifelse(ENSANUT18$f2_s6_628  == "ambos (usted y su pareja)?", 1, 
                                  ifelse(ENSANUT18$f2_s6_628  == "sólo usted?", 1,0))

########

table(ENSANUT18$f2_s8_800f)
###  si

ENSANUT18$Info_metod_ns= ifelse(ENSANUT18$f2_s8_800f  == "si", 1,0) 


#######

table(ENSANUT18$Diff_hijos)

###### Diferencia para necesidades satisfechas

ENSANUT18$Hijos_deseo_ns = ifelse(ENSANUT18$Diff_hijos >= 1, 1,0)
ENSANUT18$Hijos_deseo_ns[is.na(ENSANUT18$Hijos_deseo_ns)] <- 0                          


########## Uniendo las columnas que se requieren para definir NS


ENSANUT <- ENSANUT %>%
  dplyr::rowwise() %>%
  dplyr::mutate(Var_Necsat = max(Obten_MA_ns,        Donde_Obt_Metod_ns,  Decid_lig_ns,
                                 Tomar_misma_dec_ns, Raz_Uso_Metod_ns,    Q_toma_decis_ns,
                                 Info_metod_ns,      Hijos_deseo_ns,
                                 na.rm = TRUE))
######

table(ENSANUT18$Var_Necsat)

####### Variable Necesidades Insatisfechas

ENSANUT18$Nec_satisf= ifelse(ENSANUT18$Var_Necsat == 1, 1,0)
table(ENSANUT18$Nec_satisf)


########### Falla del método

table(ENSANUT18$f2_s2_201)
### ¿ no quería embarazarse?     
### ¿quería esperar más tiempo? Espacio Tiempo

ENSANUT18$Var_fallamet= ifelse(ENSANUT18$f2_s2_201  =="¿ no quería embarazarse?", 1,
                               ifelse(ENSANUT18$f2_s2_201  =="¿quería esperar más tiempo?",1,0))

table(ENSANUT18$Var_fallamet)

##########

###### Grupos de Edad

ENSANUT18$EdadQ<- ifelse(ENSANUT18$f1_s2_3_1 <= 11, "< a 11",
                         ifelse(ENSANUT18$f1_s2_3_1 <= 14, "12-14",
                                ifelse(ENSANUT18$f1_s2_3_1 >= 15, ">= 15",0)))


table(ENSANUT18$EdadQ)


####### Casada o Conviviente
table(ENSANUT18$f2_s9_900)

ENSANUT18$ec.casado      = ifelse(ENSANUT18$f2_s9_900 == "casado?",1,0)
ENSANUT18$ec.conviviente = ifelse(ENSANUT18$f2_s9_900 == "unión de hecho?",1,
                                  ifelse(ENSANUT18$f2_s9_900 == "unión libre?",1,0))

