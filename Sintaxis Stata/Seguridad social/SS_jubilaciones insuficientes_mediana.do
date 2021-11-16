*==================================================================================*
*                       **   OBSERVATORIO DDHH  **
*                          INDICADORES VIVIENDA 
*
*
* Creación:      9/10/2020
* Institución:   UMAD - INDDHH
* Responsable:   Jimena Pandolfi
*
*
*======================================================================================*

*======================================================================================*
* Directorio de trabajo
*======================================================================================*

	cd "C:\Users\Usuario\Dropbox\1. Unidad de Métodos y Acceso a Datos"
	global bases "C:\Users\Usuario\Dropbox\1. Unidad de Métodos y Acceso a Datos\1. Observatorio UMAD\PISO I_MICRODATOS\ECH\Microdatos\Compatibilizadas Iecon\En uso"
	global tabulados "C:\Users\Usuario\Dropbox\1. Unidad de Métodos y Acceso a Datos\Convenio INDDHH\Tabulados"

*======================================================================================*
* % de jubilaciones por debajo de la mitad de la mediana de jubilaciones
*======================================================================================*

	matrix def       MATR= J(14,4,.)
	matrix colnames  MATR= INDICADOR VARIABLE CATEGORIA VALOR 
	matrix rownames  MATR= 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 

	
	local i=1

	foreach anio of numlist 6/19  {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear

	g jubilaciones = bc_pg911 + bc_pg921
	
	sum bc_pt4 [fw=bc_peso] if bc_pt4>0, detail
	
	g jubilaciones_insuf = 0
	replace jubilaciones_insuf=1 if jubilaciones<(r(p50)/2)
	
	qui: mean jubilaciones_insuf [aw=bc_peso] if jubilaciones>0
	matrix MATR  [`i',4]=  e(b)
	local i=`i'+1

	}
    *

**
xml_tab MATR, save("$tabulados\SS_jubilaciones insfu_mediana.xls") sh("T") replace
**


** Quintiles

	foreach quintil of numlist 1/5  {

local i=1

	foreach anio of numlist 6/19  {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear
	
	g jubilaciones = bc_pg911 + bc_pg921
	
	sum bc_pt4 [fw=bc_peso] if bc_pt4>0, detail
	
	g jubilaciones_insuf = 0
	replace jubilaciones_insuf=1 if jubilaciones<(r(p50)/2)
	
	qui: mean jubilaciones_insuf [aw=bc_peso] if jubilaciones>0 & bd_quintilesy==`quintil'

	matrix MATR  [`i',4]=  e(b)
	local i=`i'+1

	}
xml_tab MATR, save("$tabulados\SS_jubilaciones insfu_mediana.xls") sh("Q`quintil'") append
	}
    *

** Sexo
	
	foreach sexo of numlist 1/2  {

local i=1

	foreach anio of numlist 6/19  {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear
	
	g jubilaciones = bc_pg911 + bc_pg921
	
	sum bc_pt4 [fw=bc_peso] if bc_pt4>0, detail
	
	g jubilaciones_insuf = 0
	replace jubilaciones_insuf=1 if jubilaciones<(r(p50)/2)
	
	qui: mean jubilaciones_insuf [aw=bc_peso] if jubilaciones>0 & bc_pe2==`sexo'

	matrix MATR  [`i',4]=  e(b)
	local i=`i'+1

	}
xml_tab MATR, save("$tabulados\SS_jubilaciones insfu_mediana.xls") sh("S`sexo'") append
	}
    *

	
	
** Ascendencia

	
	foreach asc of numlist 1/2  {

local i=1

	foreach anio of numlist 6/19  {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear
	
	g jubilaciones = bc_pg911 + bc_pg921
	
	sum bc_pt4 [fw=bc_peso] if bc_pt4>0, detail
	
	g jubilaciones_insuf = 0
	replace jubilaciones_insuf=1 if jubilaciones<(r(p50)/2)
	
	qui: mean jubilaciones_insuf [aw=bc_peso] if jubilaciones>0 & bd_e29_1==`asc'

	matrix MATR  [`i',4]=  e(b)
	local i=`i'+1

	}
xml_tab MATR, save("$tabulados\SS_jubilaciones insfu_mediana.xls") sh("A`asc'") append
	}
    *


