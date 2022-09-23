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
* % de personas residiendo en hogares por debajo de la línea de pobreza que no perciben TUS ni AFAM
*======================================================================================*

	matrix def       MATR= J(5,4,.)
	matrix colnames  MATR= INDICADOR VARIABLE CATEGORIA VALOR 
	matrix rownames  MATR= 2015 2016 2017 2018 2019 


local i=1

	foreach anio of numlist 15/19  {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear
	
	g y_tus_mides= e560_1_1
	g y_tus_inda = e560_2_1
	g y_afam= g257

	g y_transf = y_tus_mides + y_tus_inda + y_afam

	egen h_y_transf = sum (y_transf), by (bc_correlat)

	g hpc_y_transf = h_y_transf/ht19
	
	g transinsuf = 0
	replace transinsuf = 1 if hpc_y_transf<cba
	
	qui: mean transinsuf [aw=bc_peso]
	matrix MATR  [`i',4]=  e(b)
	local i=`i'+1

	}
    *

**
xml_tab MATR, save("$tabulados\SS_transferencias_suf_cba.xls") sh("T") replace
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


