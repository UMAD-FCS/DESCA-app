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

	g p_afam=0
	replace p_afam=1 if g150 == 1
	egen h_afam = max (p_afam), by (bc_correlat)
	
	g p_tarjeta=0
	replace p_tarjeta=1 if e560 == 1
	egen h_tarjeta = max (p_tarjeta), by (bc_correlat)
	
	g nopercibe=0
	replace nopercibe=1 if h_tarjeta == 0 & h_afam==0
	
	qui: mean nopercibe [aw=bc_peso] if pobre06==1
	matrix MATR  [`i',4]=  e(b)
	local i=`i'+1

	}
    *

**
xml_tab MATR, save("$tabulados\SS_transferencias_cobertura.xls") sh("T") replace
**



** Sexo
	
	foreach sexo of numlist 1/2  {

local i=1

	foreach anio of numlist 15/19  {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear
	
	g p_afam=0
	replace p_afam=1 if g150 == 1
	egen h_afam = max (p_afam), by (bc_correlat)
	
	g p_tarjeta=0
	replace p_tarjeta=1 if e560 == 1
	egen h_tarjeta = max (p_tarjeta), by (bc_correlat)
	
	g nopercibe=0
	replace nopercibe=1 if h_tarjeta == 0 & h_afam==0
	
	qui: mean nopercibe [aw=bc_peso] if pobre06==1 & bc_pe2==`sexo'
	matrix MATR  [`i',4]=  e(b)
	local i=`i'+1

	}
xml_tab MATR, save("$tabulados\SS_transferencias_cobertura.xls") sh("S`sexo'") append
	}
    *

	

	
** Ascendencia

	
	foreach asc of numlist 1/2  {

local i=1

	foreach anio of numlist 15/19  {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear
	
	g p_afam=0
	replace p_afam=1 if g150 == 1
	egen h_afam = max (p_afam), by (bc_correlat)
	
	g p_tarjeta=0
	replace p_tarjeta=1 if e560 == 1
	egen h_tarjeta = max (p_tarjeta), by (bc_correlat)
	
	g nopercibe=0
	replace nopercibe=1 if h_tarjeta == 0 & h_afam==0
	
	qui: mean nopercibe [aw=bc_peso] if pobre06==1 & bd_e29_1==`asc'
	matrix MATR  [`i',4]=  e(b)
	local i=`i'+1

	}
xml_tab MATR, save("$tabulados\SS_transferencias_cobertura.xls") sh("A`asc'") append
	}
    *


