*==============================================================================*
* TÍTULO DE LA SINTAXIS:													   *
* Planificación Familiar													   *
* OPERACIÓN ESTADÍSTICA:													   *
* Encuesta Nacional de Salud y Nutrición (ENSANUT - 2018)					   *
*==============================================================================*
* Fecha de elaboración:        3 de Enero del 2018						       *
* Fecha última modificación:  19 de Diciembre del 2019				   	   	   *
*==============================================================================*
* Elaborado por:                                                               *
* Instituto Nacional de Estadística y Censos (INEC)							   *
* Link:                                                                        *
* www.ecuadorencifras.gob.ec 												   *
*==============================================================================*

clear all
set more off

cd "C:/Users/Gabriel Nieto/Documents/CEPAL/ECUADOR/BDD_ENSANUT_2018_STATA_2"

use 4_BDD_ENS2018_f2_mef, clear 

svyset upm [iw=fexp], strata(estrato) vce(linearized) singleunit(certainty) 

*==============================================================================*
* T14_i3. Porcentaje de mujeres en edad fértil con necesidad insatisfecha 
* 		  de planificación familiar 
*==============================================================================*

keep if f2_s1_101>=15

***Se modifica el Estado Civil***
gen civil=.
replace civil=1 if f2_s9_900>=1 & f2_s9_900<=3 
replace civil=2 if f2_s9_900==7 
replace civil=2 if f2_s9_900>=4 & f2_s9_900<=6
replace civil=3 if f2_s9_900==7 & f2_s9_901==2

*** Mujeres Activas Sexualmente ***
gen activas = f2_s8_832_dias + f2_s8_832_semanas*7 + f2_s8_832_meses*30 + f2_s8_832_anios*365 if f2_s8_832_anios<55
tab activas if activas<=30

** 1. Mujeres Casadas/Unidas o Sexualemnte Activas 
*gen unidas=.
*replace unidas=1 if civil==2
*replace unidas=2 if activas<=30 & civil>1
*label def unidas 1"Casadas o Unidas" 2"Activas Sexualmente"
*label values unidas unidas
*keep if unidas==1 & f2_s1_101>=15
*tab unidas
*tab unidas [iw=fexp]

** 1.1. Usuarias 
gen usuarias=.
replace usuarias=1 if f2_s6_604==1 & f2_s6_620<=9
tab usuarias 
tab usuarias [iw=fexp]

**1.2. No Usuarias 
gen no_usuarias=.
replace no_usuarias=1 if f2_s6_604>=2
tab no_usuarias
tab no_usuarias [iw=fexp]

**1.2.1. Embrazadas y Amenorreicas
*** Embarazadas y Amenorreicas (Nunca han usado o no usan actualmente)
gen embarazo=.
replace embarazo=1 if no_usuarias==1 & f2_s2_200==1 
replace embarazo=2 if no_usuarias==1 & f2_s2_200>=2 & f2_s8_843_dias==77 
tab embarazo 

rename embarazo emm
tab emm 
count if missing(emm)
tab emm [iw=fexp]

*** NS/NR ***

****1.2.1.1. Planeado
*** Tiempo espera para embarazo
gen espera01=f2_s2_202_1 + f2_s2_202_2*12 if f2_s2_202_1<88
gen espera02= f2_s7_702 + f2_s7_702_1*12 if f2_s7_702<30
*NOTA:cambiar a 33 después de validar  

tab f2_s7_702 
   *1 obs por revisar 
tab f2_s7_702_1

***
gen planeado=.
replace planeado=1 if no_usuarias==1 & emm==1 & f2_s2_201==1
replace planeado=1 if no_usuarias==1 & emm==1 & f2_s2_201==2 & espera01<=24  //(argumentar esta situación)
replace planeado=1 if no_usuarias==1 & emm==2 & f2_s7_701==1 & espera02<=24
tab planeado 
tab planeado [iw=fexp]

****1.2.1.2. Inoportunos
gen inoportuno=.
replace inoportuno=1 if no_usuarias==1 & emm==1 & f2_s2_201==2 & espera01>24 
replace inoportuno=1 if no_usuarias==1 & emm==2 & f2_s7_701==1 & espera02>24 
*replace inoportuno=1 if no_usuarias==1 & emm==2 & f2501>=5
tab inoportuno 
tab inoportuno [iw=fexp]

****1.2.1.3. No Deseado
gen no_deseado=.
replace no_deseado=1 if no_usuarias==1 & emm==1 & f2_s2_201==3
replace no_deseado=1 if no_usuarias==1 & emm==2 & f2_s7_701==2  
replace no_deseado=1 if no_usuarias==1 & emm==2 & f2_s7_701==4                                                   
tab no_deseado
tab no_deseado [iw=fexp]

**** 1.2.1.3. No sabe/No responde
gen nsnr=.
replace nsnr=1 if emm<=2 & planeado!=1 & inoportuno!=1 & no_deseado!=1
tab nsnr
tab nsnr [iw=fexp]

***1.2.2. No Embarazadas Ni Amenorreicas
tab f2_s2_200
tab no_usuarias
sum f2_s8_843_dias

gen prueba =.
replace prueba = 1 if f2_s8_843_dias!=77
tab prueba


gen no_emm=.
replace no_emm=1 if no_usuarias==1 & f2_s2_200>=2 & f2_s8_843_dias!=77
tab no_emm 
tab no_emm [iw=fexp]
 
****1.2.2.1. Mujeres Infertiles

**** Prueba 
gen t_reg_prueb=int((f2_s8_843_dias + f2_s8_843_semanas*7 + f2_s8_843_meses*30 + f2_s8_843_anios*365)/30) 
tab t_reg_prueb

** Tiempo de retraso de la regla **
gen tiempo_regla=int((f2_s8_843_dias + f2_s8_843_semanas*7 + f2_s8_843_meses*30 + f2_s8_843_anios*365)/30) if f2_s8_843_dias<77
sum tiempo_regla 
tab tiempo_regla	
***
gen retraso=.
replace retraso=1 if no_usuarias==1 & no_emm==1 & tiempo_regla>=3
tab retraso 
tab retraso [iw=fexp]

tab f2_s7_701
tab f2_s6_612
tab retraso

gen prueba_menop =.
replace prueba_menop =1 if no_usuarias==1 & no_emm==1 & (f2_s7_701==3 | f2_s6_612==2)
tab prueba_menop

gen menopausia=.
replace menopausia=1 if no_usuarias==1 & no_emm==1 &  retraso!=1 & (f2_s7_701==3 | f2_s6_612==2)
tab menopausia
tab menopausia [iw=fexp]

*----* Unidas o sexualmente activas Infertiles*----*
	*-*Fecha de la entrevista
	clonevar mes_entrevista=fecha_mes
	clonevar anio_entrevista=fecha_anio
	*-*Fecha del ultimo nacimiento o perdida del emm
	**Hijos nacidos muertos
	gen hijo_muerto=.
	replace hijo_muerto=(f2_s2_213_2*100)+f2_s2_213_1 // Diferente a ENSANUT 2012
	replace hijo_muerto=0 if f2_s2_213_2==8888 | f2_s2_213_1==88
	replace hijo_muerto=0 if f2_s2_213_2==7777 | f2_s2_213_1==77
	replace hijo_muerto=0 if hijo_muerto==.
	tab hijo_muerto
	**Hijos nacidos vivos
	gen hijo_vivo=.
	replace hijo_vivo=(f2_s2_218_1_b3*100)+f2_s2_218_1_b2 // Diferente a ENSANUT 2012 
	replace hijo_vivo=0 if f2_s2_218_1_b2==88 | f2_s2_218_1_b3==8888
	replace hijo_vivo=0 if hijo_vivo==.
	tab hijo_vivo
	
	**ultimo embarazo
	gen ultimo=.
	replace ultimo=1 if hijo_vivo>hijo_muerto
	replace ultimo=2 if hijo_muerto>hijo_vivo
	tab ultimo
	
	**fecha ultimo embarazo
	gen mes_ultimo=.
	replace mes_ultimo=f2_s2_213_1 if ultimo==2
	replace mes_ultimo=f2_s2_218_1_b2 if ultimo==1
	tab mes_ultimo 
	
	gen anio_ultimo=.
	replace anio_ultimo=f2_s2_213_2 if ultimo==2
	replace anio_ultimo=f2_s2_218_1_b3 if ultimo==1
	tab anio_ultimo
	
	*-*fecha de union la pareja (casadas o unidas)
	gen mes_union=.
	replace mes_union=f2_s9_904_2 if f2_s9_900<=3 & f2_s9_904_2!=77
	
	gen anio_union=.
	replace anio_union=f2_s9_904_3 if f2_s9_900<=3 & f2_s9_904_3!=7777
	tab anio_union
	
	*-*No uso del metodo anticonceptivo
	gen no_uso=.
	replace no_uso=1 if f2_s6_604==2
	tab no_uso
	
	gen a=.
	replace a=mes_entrevista+(anio_entrevista*12)
	tab a
	sum a
	count if missing(a)
	
	gen b=.
	replace b=mes_ultimo+(anio_ultimo*12)
	tab b
	count if missing(b)
	sum b
	
	gen c=no_uso
	tab c
	count if missing(c)
	
	gen d=.
	replace d=mes_union+(anio_union*12)
	tab d
	count if missing(d)
	sum d
	
	***** prueba a_d
	
	gen Prov_a_d=(a-d)/12 
	tab Prov_a_d
	count if missing(Prov_a_d)
	sum Prov_a_d
	
	gen a_d=(a-d)/12 if emm!=1
	sum a_d
	tab a_d
	
	***** prueba a_b
	
	gen Prov_a_b=(a-b)/12 
	tab Prov_a_b
	sum Prov_a_b
	
	gen a_b=(a-b)/12 if emm!=1
	tab a_b
	sum a_b
	
    tab a_d
	tab a_b
	tab c
	tab emm
	tab f2_s6_604
	tab retraso
	tab menopausia
	
	*** Prueba para infertil
	
	*** tablas
	tab d
	
	*** prueb 1
	
	gen prub1=.
	replace prub1=1 if a_d>=5
	tab prub1
	
	gen prub_infer=.
	replace prub_infer=1 if emm!=1 & f2_s6_604==2 & retraso!=1 & menopausia!=1 & a_d>=5 & a_b>=5
	tab prub_infer
	
	
	gen infertil_01=.
	replace infertil_01=1 if a_d>=5 & a_b>=5 & c==1 & emm!=1 & f2_s6_604==2 & retraso!=1 & menopausia!=1   
	tab infertil_01 
	tab infertil_01 [iw=fexp]
	
	***** verificacion de composición de las variabñles
	
	tab no_usuarias
	count if missing(no_usuarias)
	tab no_emm
	count if missing(no_emm)
	tab retraso
	count if missing(retraso)
	tab infertil_01
	count if missing(infertil_01)
	tab menopausia
	count if missing(menopausia)
	tab f2_s7_701
	count if missing(f2_s7_701)
	tab f2_s7_702
	count if missing(f2_s7_702)
	tab f2_s6_612
	count if missing(f2_s6_612)

	
****** prueba

gen pr2=.
replace pr2=1 if no_usuarias==1 & no_emm==1 & retraso==1
replace pr2=2 if no_usuarias==1 & no_emm==1 & (menopausia==1 | f2_s7_702==44) & infertil!=1
replace pr2=3 if no_usuarias==1 & no_emm==1 & infertil_01==1 & (infertil!=1 & infertil!=2)
tab pr2
	
	
** Total de mujeres inf곴iles **
gen infertil=.
replace infertil=1 if no_usuarias==1 & no_emm==1 & retraso==1
replace infertil=2 if no_usuarias==1 & no_emm==1 & (menopausia==1 | f2_s7_702==44) & infertil!=1
replace infertil=3 if no_usuarias==1 & no_emm==1 & infertil_01==1 & (infertil!=1 & infertil!=2)
replace infertil=4 if no_usuarias==1 & no_emm==1 & f2_s7_701==4 & (infertil!=1 & infertil!=2 & infertil!=3)
replace infertil=5 if no_usuarias==1 & no_emm==1 & f2_s6_612<=4 & (infertil!=1 & infertil!=2 & infertil!=3 & infertil!=4) // Diferente a ENSANUT 2012
tab infertil
count if missing(infertil)
tab infertil [iw=fexp]

****1.2.2.2. Mujeres Fertiles 
gen fertil=.
replace fertil=1 if no_usuarias==1 & no_emm==1 & infertil>5 
tab fertil 
sum fertil
tab fertil [iw=fexp]

*****1.2.2.2.1. Desea Despues
gen desea_despues=.
replace desea_despues=1 if no_usuarias==1 & no_emm==1 & fertil==1 & f2_s7_701==1 & (f2_s7_702_1>2 & f2_s7_702!=33)
tab desea_despues
tab desea_despues [iw=fexp]

*****1.2.2.2.1. No Desea

*** prueba 

gen Pnd0=. 
replace Pnd0=1 if (f2_s7_701>=2)
tab Pnd0
count if missing(f2_s7_701)
count if (f2_s7_701>=2)

gen Pnd=. 
replace Pnd=1 if no_usuarias==1 & no_emm==1 & fertil==1
count if missing(Pnd)
tab Pnd

gen no_desea=. 
replace no_desea=1 if no_usuarias==1 & no_emm==1 & fertil==1 & (f2_s7_701>=2)
tab no_desea, 
tab no_desea [iw=fexp]

*****1.2.2.2.1. Desea Pronto
gen pronto=.
replace pronto=1 if no_usuarias==1 & no_emm==1 & fertil==1 & f2_s7_701==1 & (f2_s7_702_1<=2 | f2_s7_702==33)
tab pronto 
tab pronto [iw=fexp]

*****NIA****
***Nececidad para espaciar
gen espaciar=0
replace espaciar=1 if inoportuno==1 | desea_despues==1
tab espaciar 
tab espaciar [iw=fexp]

***Nececidad para limitar
gen limitar=0
replace limitar=1 if no_deseado==1 | no_desea==1
tab limitar
tab limitar [iw=fexp]

***NIA***
gen nia=.
replace nia=100 if espaciar==1 | limitar==1
replace nia=0   if nia==.
tab nia
tab nia [iw=fexp]

***Planificación familiar
gen muj=0 if usuarias==1 | nia==100
replace muj=100 if usuarias==1

*** Resultados ***
svy: mean nia
svy: mean muj

rename nia T14_i3


*==============================================================================*
* T14_i10. Porcentaje de mujeres que tomas sus propias decisiones informadas
*===============================================================================

cap drop unidas
gen unidas=.
replace unidas=1 if (f2_s9_900>=1 & f2_s9_900<=3) & f2_s6_604==1 & (f2_s1_101>=15 & f2_s1_101<=49)

cap drop usa_met
gen usa_met=.
replace usa_met=1 if unidas==1 & ((f2_s6_628==1 | f2_s6_628==2 | f2_s6_628==4) & (f2_s6_620>=3 & f2_s6_620<=13)) & ///
(f2_s6_633==1 | f2_s6_633==2) & f2_s8_839==1 
replace usa_met=1 if  unidas==1 & (f2_s6_620>=1 & f2_s6_620<=2) & (f2_s6_623!=. & f2_s6_623!=2 & f2_s6_623!=88) & ///
(f2_s6_633==1 | f2_s6_633==2) & f2_s8_839==1  
cap drop T14_i10
gen T14_i10=0 if unidas==1 
replace T14_i10=100 if usa_met==1

tab T14_i10
tab T14_i10 [iw=fexp]

****************************** FIN *********************************************

