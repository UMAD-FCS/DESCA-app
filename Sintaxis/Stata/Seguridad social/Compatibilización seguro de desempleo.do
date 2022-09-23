	foreach anio of numlist 6/19  {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear
	ta bc_anio
	lookfor   f113 g148_3
	}
    *
   
** SEGURO DE PARO **   
   
   *2006
     use "$bases\Fusionada_personasyhogares_6.dta", clear
		g bd_f117 = f112
     save "$bases\Fusionada_personasyhogares_6.dta", replace

    *2007 - 2008
		foreach anio of numlist 7/8  {

	 use "$bases\Fusionada_personasyhogares_`anio'.dta", clear
		g bd_f117 = f120
     save "$bases\Fusionada_personasyhogares_`anio'.dta", replace
	}
    *
	
	
    *2009 - 2019
		foreach anio of numlist 9/19  {

	 use "$bases\Fusionada_personasyhogares_`anio'.dta", clear
		g bd_f117 = f117
     save "$bases\Fusionada_personasyhogares_`anio'.dta", replace
	}
    *
	
** TIEMPO QUE BUSCA TRABAJO **

   *2006
     use "$bases\Fusionada_personasyhogares_6.dta", clear
		g bd_f113  = f112
     save "$bases\Fusionada_personasyhogares_6.dta", replace

    *2007 - 2008
		foreach anio of numlist 7/8  {

	 use "$bases\Fusionada_personasyhogares_`anio'.dta", clear
		g bd_f117 = f120
     save "$bases\Fusionada_personasyhogares_`anio'.dta", replace
	}
    *
	
	
    *2009 - 2019
		foreach anio of numlist 9/19  {

	 use "$bases\Fusionada_personasyhogares_`anio'.dta", clear
		g bd_f117 = f117
     save "$bases\Fusionada_personasyhogares_`anio'.dta", replace
	}
    *
