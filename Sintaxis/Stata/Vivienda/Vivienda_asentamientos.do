*==================================================================================*
*                       **   OBSERVATORIO DDHH  **
*                          INDICADORES VIVIENDA 
*
*
* Creación:      29/09/2020
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
* Culminación de ciclo educativo. 	% de mayores de 20 años sin educación obligatoria 
* completo	2006-2019	ECH	Parámetros: 20 años/  21-23 años / mayores a 24 años
*======================================================================================*

	matrix def       MATR= J(13,4,.)
	matrix colnames  MATR= INDICADOR VARIABLE CATEGORIA VALOR 
	matrix rownames  MATR= 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 

local i=1

	foreach anio of numlist 7/19  {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear

	g asent = 0
	replace asent = 1 if d8_4==1

	qui: mean asent [aw=bc_peso]
	matrix MATR  [`i',4]=  e(b)
	local i=`i'+1

	}
    *

**
xml_tab MATR, save("$tabulados\Asentamientos.xls") sh("T") replace
**


** Quintiles

	foreach quintil of numlist 1/5  {

local i=1

	foreach anio of numlist 7/19  {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear
	
	g asent = 0
	replace asent = 1 if d8_4==1

	qui: mean asent [aw=bc_peso] if bd_quintilesy==`quintil'
	matrix MATR  [`i',4]=  e(b)
	local i=`i'+1

	}
xml_tab MATR, save("$tabulados\Asentamientos.xls") sh("Q`quintil'") append
	}
    *

** Sexo
	
	foreach sexo of numlist 1/2  {

local i=1

	foreach anio of numlist 7/19  {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear
	
	g asent = 0
	replace asent = 1 if d8_4==1

	
	qui: mean asent [aw=bc_peso] if bc_pe2==`sexo'
	matrix MATR  [`i',4]=  e(b)
	local i=`i'+1

	}
xml_tab MATR, save("$tabulados\Asentamientos.xls") sh("S`sexo'") append
	}
    *

** Edad
	

	foreach edad of numlist 1/6  {


local i=1

	foreach anio of numlist 7/19  {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear
	
	g asent = 0
	replace asent = 1 if d8_4==1
	
	g tramo=.
	replace tramo=1 if bc_pe3>=0 & bc_pe3<=5
	replace tramo=2 if bc_pe3>=6 & bc_pe3<=12
	replace tramo=3 if bc_pe3>=13 & bc_pe3<=17
	replace tramo=4 if bc_pe3>=18 & bc_pe3<=29
	replace tramo=5 if bc_pe3>=30 & bc_pe3<=64
	replace tramo=6 if bc_pe3>=65

	qui: mean asent [aw=bc_peso] if tramo==`edad'
	matrix MATR  [`i',4]=  e(b)
	local i=`i'+1

	}
xml_tab MATR, save("$tabulados\Asentamientos.xls") sh("E`edad'") append
	}
    *

	
	
** Departamento

	
	foreach dpto of numlist 1/19  {

local i=1

	foreach anio of numlist 7/19  {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear
	
	g asent = 0
	replace asent = 1 if d8_4==1
	
	qui: mean asent [aw=bc_peso] if bc_dpto==`dpto'
	matrix MATR  [`i',4]=  e(b)
	local i=`i'+1

	}
    *
xml_tab MATR, save("$tabulados\Asentamientos.xls") sh("D`dpto'") append

	}
    *
	
** Ascendencia

	
	foreach asc of numlist 1/2  {

local i=1

	foreach anio of numlist 7/19  {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear
	
	g asent = 0
	replace asent = 1 if d8_4==1
	
	qui: mean asent [aw=bc_peso] if bd_e29_1==`asc'
	matrix MATR  [`i',4]=  e(b)
	local i=`i'+1

	}
xml_tab MATR, save("$tabulados\Asentamientos.xls") sh("A`asc'") append
	}
    *


