DEFINE PERSONA.UnidasR 
	AS RECODE   PERSONA.ecivil
	(1 = 0)
    (2 : 3 = 1)
	(4 : 6 = 0)
	TYPE INTEGER
 RANGE 0:1 
 
SAVE "V:\DAT\SAECEPAL\SAE-unfpa\UnfpaLACgithub\PER\2021\1.D6\Rcodes\Redatam\UnidasR.rbf" OVERWRITE

  FREQ PERSONA.UnidasR by PERSONA.ecivil