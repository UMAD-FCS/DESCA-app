*==================================================================================*
*                       **   OBSERVATORIO DDHH  **
*                          INDICADORES VIVIENDA 
*
*
* Creación:      23/11/2020
* Institución:   UMAD - INDDHH
* Responsable:   Jimena Pandolfi
*
*
*======================================================================================*

*======================================================================================*
* Directorio de trabajo
*======================================================================================*

	cd "C:\Users\jimen\OneDrive\Escritorio\UMAD\"
	global bases "C:\Users\jimen\OneDrive\Escritorio\UMAD\En uso"
	global tabulados "C:\Users\jimen\OneDrive\Escritorio\UMAD\SS INDDHH"

*======================================================================================*
* % de desempleados (de 1 a 6 meses)  que cobran seguro de desempleo por debajo del SMN
*======================================================================================*

	matrix def       MATR= J(11,4,.)
	matrix colnames  MATR= INDICADOR VARIABLE CATEGORIA VALOR 
	matrix rownames  MATR= 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 


local i=1

	foreach anio of numlist 9/19  {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear

	 
	
	g desemp1a6 =0
	replace desemp1a6=1 if f113 >= 4 & f113<=24
	
	g desempinsuf = 0
	replace desempinsuf = 1 if g148_3 < 15000
	
	qui: mean desempinsuf [aw=bc_peso] if desemp1a6==1
	matrix MATR  [`i',4]=  e(b)
	local i=`i'+1

	}
    *

**
xml_tab MATR, save("$tabulados\SS_desemp_insuf_smn.xls") sh("T") replace
**

/*
** Quintiles

	foreach quintil of numlist 1/5  {

local i=1

	foreach anio of numlist 6/19  {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear
	
	g pensiones = bc_pg912 + bc_pg922
	sum pensiones [fw=bc_peso] if pensiones>0, detail
	
	g pensiones_insuf = 0
	replace pensiones_insuf=1 if pensiones<(r(p50)/2)
	
	qui: mean pensiones_insuf [aw=bc_peso] if pensiones>0
	matrix MATR  [`i',4]=  e(b)
	local i=`i'+1

	}
xml_tab MATR, save("$tabulados\SS_pensiones insfu_mediana.xls") sh("Q`quintil'") append
	}
    *

** Sexo
	
	foreach sexo of numlist 1/2  {

local i=1

	foreach anio of numlist 6/19  {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear
	
	g pensiones = bc_pg912 + bc_pg922
	sum pensiones [fw=bc_peso] if pensiones>0, detail
	
	g pensiones_insuf = 0
	replace pensiones_insuf=1 if pensiones<(r(p50)/2)
	
	qui: mean pensiones_insuf [aw=bc_peso] if pensiones>0
	matrix MATR  [`i',4]=  e(b)
	local i=`i'+1

	}
xml_tab MATR, save("$tabulados\SS_pensiones insfu_mediana.xls") sh("S`sexo'") append
	}
    *

	
	
** Departamento

	
	foreach dpto of numlist 1/19  {

local i=1

	foreach anio of numlist 6/19  {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear
	
	g pensiones = bc_pg912 + bc_pg922
	sum pensiones [fw=bc_peso] if pensiones>0, detail
	
	g pensiones_insuf = 0
	replace pensiones_insuf=1 if pensiones<(r(p50)/2)
	
	qui: mean pensiones_insuf [aw=bc_peso] if pensiones>0
	matrix MATR  [`i',4]=  e(b)
	local i=`i'+1

	}
    *
xml_tab MATR, save("$tabulados\SS_pensiones insfu_mediana.xls") sh("D`dpto'") append

	}
    *
	
** Ascendencia

	
	foreach asc of numlist 1/2  {

local i=1

	foreach anio of numlist 6/19  {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear
	
	g pensiones = bc_pg912 + bc_pg922
	sum pensiones [fw=bc_peso] if pensiones>0, detail
	
	g pensiones_insuf = 0
	replace pensiones_insuf=1 if pensiones<(r(p50)/2)
	
	qui: mean pensiones_insuf [aw=bc_peso] if pensiones>0
	matrix MATR  [`i',4]=  e(b)
	local i=`i'+1

	}
xml_tab MATR, save("$tabulados\SS_pensiones insfu_mediana.xls") sh("A`asc'") append
	}
    *


