DEFINE PERSONA.EDAD6 
	AS RECODE PERSONA.EDAD_OG
	(0 : 14 = 1)
	(15 : 20 = 2)
	(21 : 30 = 3)
	(31 : 39 = 4)
	(40 : 49 = 5)
    (50 : 200 = 6)
	TYPE INTEGER
 RANGE 1:6
SAVE "V:\DAT\SAECEPAL\SAE-unfpa\UnfpaLACgithub\MEX\2018\1.D6\Rcodes\redatam\EDAD6.rbf" OVERWRITE

FREQ PERSONA.EDAD6

DEFINE PERSONA.UnidasR 
	AS RECODE   PERSONA.SITUA_CONYUGAL
	(1  = 1)
    (2 : 4 = 0)
    (5 :7  = 1)
    (8 :9  = 0)
	TYPE INTEGER
 RANGE 0:1 
 
SAVE "V:\DAT\SAECEPAL\SAE-unfpa\UnfpaLACgithub\MEX\2018\1.D6\Rcodes\redatam\UnidasR.rbf" OVERWRITE

FREQ PERSONA.UnidasR