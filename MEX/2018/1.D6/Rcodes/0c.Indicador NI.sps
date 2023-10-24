* Encoding: UTF-8.
CD 
'D:\CEPAL\Mexico\UNFPA\base_datos_enadid18_sav'.

New file. 
GET FILE="Mujer_2018_R.sav".
EXECUTE.
 **** Filtro: Mujeres de 15-49 años ****. 
ALTER TYPE p5_2_1 p10_1 p8_41_1 p8_41_2 p8_41_3 p8_41_4 p8_21 p8_38  (F8.2).
Select if (p5_2_1 <= 49). 

 **** Mujeres unidas ****. 
 
 Recode p10_1 (1 7=1) (2 thru 6=2) (8=3) Into Unida. 
 Recode p10_1 (1 7=1) (2 thru 6=2) (8=2) Into Unida1. 

 **** Mujeres sexualmente activas ****. 
 If (00 <= p8_41_1 & p8_41_1 <= 30) SexAct=1. 
 If (00 <= p8_41_2 & p8_41_2 <= 04) SexAct=1. 
 If (00 <= p8_41_3 & p8_41_3 <= 01) SexAct=1. 
 If (02 <= p8_41_3 & p8_41_3 <= 11) SexAct=2. 
 If (01 <= p8_41_4 & p8_41_4 <= 40) SexAct=2. 
 If (p8_21 = 04) SexAct=3. If (p8_38 = 88) SexAct=3. 
If (p8_38 = 99) SexAct=9. If (p8_41_1 = 99) SexAct=9. 
 *********************************************. 

 **** Filtro: Mujeres Sexualmente activas ****. 
 Select if (SexAct = 1). 
 *********************************************.

 **** Conocimiento de métodos ****. 
ALTER TYPE p8_1_01 p8_1_02 p8_1_03 p8_1_04 p8_1_05   p8_1_06 p8_1_07 p8_1_08 p8_1_09 p8_1_10 p8_1_11 p8_1_12 p8_1_13 p8_1_14 (F8.2).
 Count x=p8_1_01 p8_1_02 p8_1_03 p8_1_04 p8_1_05   p8_1_06 p8_1_07 p8_1_08 p8_1_09 p8_1_10 p8_1_11 p8_1_12 p8_1_13 p8_1_14 (1 2). 

 **** Uso de métodos ****. 
ALTER TYPE p8_4_01 p8_4_02 p8_4_03 p8_4_04 p8_4_05 p8_4_06 p8_4_07 p8_4_08 p8_4_09 p8_4_10 p8_4_11 p8_4_12 p8_4_13 p8_4_14 (F8.2).

  Do if (p8_4_01 <> 1). 
  Count x1=p8_4_01 p8_4_02 p8_4_03 p8_4_04 p8_4_05 p8_4_06 p8_4_07 p8_4_08 p8_4_09 p8_4_10 p8_4_11 p8_4_12 p8_4_13 p8_4_14 (1).
  End if. 

 *p8_4_01.

ALTER TYPE p8_10  (F8.2).


 Do if (x = 0).   
    Compute Uso=3.   
    Do if (p8_10 <= 2).      
        Compute Uso=1.   
    Else.
     Do if (p8_10 = 3).
      Compute Uso=3.
     Else.
       Do if (p8_10 = 9).
        Compute Uso=9.
       Else.
        Compute Uso=98.
       End if.
     End if.
    End if.
 End if.

ALTER TYPE p8_3  (F8.2).

 Do if (sys(Uso) & p8_3 = 2).   
  Do if (p8_10 <= 2).
   Compute Uso=1.
  Else.
   Do if (p8_10 = 3).
    Compute Uso=3.
   Else.
    If (p8_10 = 9) Uso=9.
   End if.
  End if.
End if. 

ALTER TYPE p8_4_01 p8_9 p8_10 (F8.2).

If (sys(Uso) & p8_4_01 = 1) Uso=1.
 DO if (sys(Uso)).   
  Do if (p8_9 = 1).
   Compute Uso=1.
  Else.
   Do if (p8_9 = 9).
     Compute Uso=9.
   Else.
    Do if (p8_10 <= 2).
     Compute Uso=1.
    Else.
     Do if (p8_10 = 3).
      Compute Uso=2.
     Else.
      Compute Uso=9.
     End if.
    End if.
   End if.
  End if.
End if.


ALTER TYPE p7_7 p7_11 p7_9 p7_13  p8_11a (F8.2).

**** Usuarias de métodos Anticonceptivos GRUPO 1 ****. 
Do if (Uso = 1). 
**** Caja 2 ****.
  Compute ULimEsp=2. 
**** Caja 1 ****.   
If (p7_7 = 3) ULimEsp=1.
If (p7_11 = 3) ULimEsp=1.
If (Any(p7_9,2,4,5)) ULimEsp=1.
If (Any(p7_13,2,4,5)) ULimEsp=1.
If (p8_11a <= 2) ULimEsp=1. 
End if.

**** No Usuarias de métodos ****.
ALTER TYPE p8_43_1_1 p8_43_1_2 p8_43_1_3 p8_43_1_4 (F8.2).

**** Tiempo a la última menstruación en meses ****.
If (0 <= p8_43_1_1 & p8_43_1_1 <= 30) TUMes=0. 
If (0 <= p8_43_1_2 & p8_43_1_2 <= 3) TUMes=0. 
If (0 <= p8_43_1_3 & p8_43_1_3 <= 11) TUMes=p8_43_1_3*1.
If (0 <= p8_43_1_4 & p8_43_1_4 <= 99) TUMes=p8_43_1_4*12. 

**** Tiempo al Ultimo HNV *****. 
ALTER TYPE p5_17_1.1 TO p5_17_1.17 p5_17_2.1 TO p5_17_2.17 ordenemb.1 TO ordenemb.17  (F8.2).

Vector 
REmb = ordenemb.1 TO ordenemb.17 
/MNNii=p5_17_1.1 to p5_17_1.17 
/ANNii=p5_17_2.1 to p5_17_2.17. 

Compute Ban=0.
Compute i=17.
Loop If (Ban = 0 & 0 < i).
   Do if (REmb(i) <= 2).
     Compute MNNi1=MNNii(i).
     Compute ANNi1=ANNii(i).
     If (MNNi1 = 99) MNNi1=6.
     Compute OrdNac=i.
     Compute Ban=1.
   End if.
     Compute i=i-1.
End loop.

If (ANNi1 <> 9999) TNNi=(2018-ANNi1)*12+(Mes_Lev-MNNi1).

Do If (Uso <> 1).   
    Compute EmbAmeno=3.   
    If (p7_1 = 1) EmbAmeno=1.   
    If (EmbAmeno = 3 & TNNi < 24 & TNNi < TUMes) EmbAmeno=2. 
End if.

* GRUPO 2 *. 
Do if (EmbAmeno <= 2). 
* Embarazadas *.   
  Do if (EmbAmeno = 1).      
  If (p7_2 = 3) Deseo=1.      
  If (p7_2 = 2) Deseo=2.      
  If (p7_2 = 1) Deseo=3.      
  If (p7_2 = 9) Deseo=9.   
End if. 

* Amenorreicas *.   
  Do if (embAmeno = 2).      
   If (p9_32 = 3) Deseo=1.      
   If (p9_32 = 2) Deseo=2.      
   If (p9_32 = 1) Deseo=3.      
   If (p9_32 = 9) Deseo=9.  
  End if.
  If (sys(Deseo)) Deseo=9. 
  End if.

**** GRUPO 3 ****. 
Do if (~sys(p10_6_1) & ~sys(p10_6_2)).  
 Compute MMat=p10_6_1.   
 Compute AMat=p10_6_2. 
Else.   
 Compute MMat=p10_3_1.   
 Compute AMat=p10_3_2. 
End if. 
If (MMat = 99) MMat=6. 
If (AMat <> 9999) TMat=(2018-AMat)*12+(Mes_Lev-MMat).

Count x1=ResEmb.1 to ResEmb.17 (1 2 3 4). 
Do If (X1 > 0).   
 Compute MNNi=p5_17_1.1.   
 Compute ANNi=p5_17_2.1.   
 if (MNNi = 99) MNNi=6.   
 If (ANNi <> 9999) TNac=((2018-ANNi)*12+(Mes_Lev-MNNi)).
End if.


Do If (EmbAmeno = 3).
   Compute Infecun=2.   
   If (Unida = 1 & TMat >= 60 & TNNi >= 60 & Uso = 3) Infecun=1.   
   If (Unida = 1 & TMat >= 60 & X1 = 0 & Uso = 3) Infecun=1.   
   If (p7_7 = 2) Infecun=1.   
   If (p7_11 = 2) Infecun=1.   
   If (p8_21 = 5) Infecun=1.   
   If (TNNi >= 60 & TUMes >= 6) Infecun=1.   
   If (p8_43_1_1 = 88) Infecun=1.
   If (Any(p8_43_2,1,2)) Infecun=1.
   If (TNNi > 60 & TUMes > TNNi) Infecun=1. 
   **** Para cuadro ****.   
   Compute Infe13=7.   
   If (Unida = 1 & TMat >= 60 & TNNi >= 60 & Uso = 3) Infe13=1.   
   If (Unida = 1 & TMat >= 60 & X1 = 0 & Uso = 3) Infe13=1.   
   If (p7_7 = 2) Infe13=2.   
   If (p7_11 = 2) Infe13=2.   
   If (p8_21 = 5) Infe13=3.   
   If (TNNi >= 60 & TUMes >= 6) Infe13=4.   
   If (p8_43_1_1 = 88) Infe13=5.   
   If (Any(p8_43_2,1,2)) Inf13=5.   
   If (TNNi > 60 & TUMes > TNNi) Infe13=6. 
End if.

**** GRUPO 4 ****. 
Do if (Infecun = 2). 
* Sin hijos *.   
 If (p7_7 = 1 & p7_8 < 2) GTH=3.   
 If (p7_11 = 1  & p7_12 < 2) GTH=3.   
 If (p7_7 = 3) GTH=1.   
 If (p7_11 = 3) GTH=1.   
 If (p7_7 = 1 & 2 <= p7_8 & p7_8 < 99) GTH=2.   
 If (p7_11 = 1 & 2 <= p7_12 & p7_12 < 99) GTH=2.   
 If (p7_7 = 9) GTH=9.   
 If (p7_11 = 9) GTH=9.   
 If (sys(GTH)) GTH=4. 
 ** Para cuadro ***.      
 If (p7_7 = 1 & p7_8 < 2) GTH13=1.   
 If (p7_11 = 1  & p7_12 < 2) GTH13=1.   
 If (p7_7 = 3) GTH13=2.   
 If (p7_11 = 3) GTH13=2.   
 If (p7_7 = 1 & 2 <= p7_8 & p7_8 < 99) GTH13=3.   
 If (p7_11 = 1 & 2 <= p7_12 & p7_12 < 99) GTH13=3.   
 If (p7_7 = 9) GTH13=4.   
 If (p7_11 = 9) GTH13=4.   
 If (sys(GTH)) GTH13=5. 
End if

Compute Dem14=8. 
If (ULimEsp = 1) Dem14=1. 
If (ULimEsp = 2) Dem14=2. 
If (Deseo = 2) Dem14=3. 
If (Deseo = 1) Dem14=4. 
If (Deseo = 3) Dem14=5. 
If (Deseo = 9) Dem14=9. 
If (GTH = 2) Dem14=3.
If (GTH = 1) Dem14=4. 
If (GTH = 3) Dem14=5. 
If (Infecun = 1) Dem14=6. 
If (Deseo = 4) Dem14=7. 
If (GTH = 4) Dem14=9. 
If (GTH = 9) Dem14=9. 
Compute Dem14.1=Dem14. 
If (Dem14 = 4) Dem14.1=3.

*********************************************************.
******************Variable para cruces ******************. 
**** Grupos de edad ****. 
Compute Edadq=TRUNC(p5_2_1/5)-2. 
*****************************************************************************. 
Variable labels 
Uso ‘Condición de uso’ 
/ULimEsp ‘Uso para espaciar o limitar’ 
/TUMes ‘Tiempo a la última menstruación’ 
/TNNi ‘Tiempo al nacimiento del último HNV’ 
/Deseo ‘Deseo del embarazo o último HNV’ 
/Dem14 ‘NIA 2018’ 
/Dem14.1 ‘NIA 2018’ 
.

Value labels 
Uso   
 1 ‘Usuaria’   
 2 ‘Exusuaria’   
 3 ‘Nunca usuaria’ 
/ULimEsp   
 1 ‘Limitar’   
 2 ‘Espaciar’ 
/Deseo   
 1 ‘Espaciar’   
 2 ‘Limitar’   
 3 ‘No necesita’   
 4 ‘Sin clasificar’   
 9 ‘Dato perdido’
/Infecun   
 1 ‘Infecundas’   
 2 ‘Fecundas’ 
/Dem14   
 1 ‘Uso limitar’   
 2 ‘Uso espaciar’   
 3 ‘Demanda espaciar’   
 4 ‘Demanda limitar’   
 5 ‘No necesita’
 6 ‘Infecundas’   
 9 ‘N.E.’ 
/Dem14.1   
 1 ‘Uso limitar’   
 2 ‘Uso espaciar’   
 3 ‘Demanda total’   
 5 ‘No necesita’   
 6 ‘Infecundas’   
 9 ‘N.E.’ 
/Edadq   
 1 ‘15-19’   
 2 ‘20-24’   
 3 ‘25-29’   
 4 ‘30-34’   
 5 ‘35-39’   
 6 ‘40-44’   
 7 ‘45-49’
 .

 WEIGHT BY fac_per. 

 * Tablas personalizadas. 
 CTABLES  
  /VLABELS VARIABLES=Edadq Dem14 DISPLAY=DEFAULT  
  /TABLE Edadq [C][UCOUNT F40.0, COUNT F40.0, ROWPCT.COUNT PCT40.1] BY Dem14 [C] 
  /CATEGORIES VARIABLES=Edadq ORDER=A KEY=VALUE EMPTY=INCLUDE TOTAL=YES POSITION=BEFORE  
  /CATEGORIES VARIABLES=Dem14 ORDER=A KEY=VALUE EMPTY=INCLUDE. 
 CTABLES  
  /VLABELS VARIABLES=Edadq Dem14.1 DISPLAY=DEFAULT  
  /TABLE Edadq [C][UCOUNT F40.0, COUNT F40.0, ROWPCT.COUNT PCT40.1] BY Dem14.1 [C] 
  /CATEGORIES VARIABLES=Edadq ORDER=A KEY=VALUE EMPTY=INCLUDE TOTAL=YES POSITION=BEFORE  
  /CATEGORIES VARIABLES=Dem14.1 ORDER=A KEY=VALUE EMPTY=INCLUDE.