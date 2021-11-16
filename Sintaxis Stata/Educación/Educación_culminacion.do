*==================================================================================*
*                       **   OBSERVATORIO DDHH  **
*                          INDICADORES EDUCACIÓN 
*
*
* Creación:      31/08/2020
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

	matrix def       MATR= J(9,4,.)
	matrix colnames  MATR= INDICADOR VARIABLE CATEGORIA VALOR 
	matrix rownames  MATR= 2011 2012 2013 2014 2015 2016 2017 2018 2019 

local i=1

	foreach anio of numlist 11/19  {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear

	g expulsiontemp = 0
	replace expulsiontemp = 1 if bc_edu_1<12

	qui: mean expulsiontemp [aw=bc_peso] if bc_pe3>=20
	matrix MATR  [`i',4]=  e(b)
	local i=`i'+1

	}
    *

**
xml_tab MATR, save("$tabulados\Culminacion_mas20.xls") sh("T") replace
**


** Quintiles

	foreach quintil of numlist 1/5  {

local i=1

	foreach anio of numlist 11/19  {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear
	
	g expulsiontemp = 0
	replace expulsiontemp = 1 if bc_edu_1<12

	qui: mean expulsiontemp [aw=bc_peso] if bc_pe3>=20 & bd_quintilesy==`quintil'
	matrix MATR  [`i',4]=  e(b)
	local i=`i'+1

	}
xml_tab MATR, save("$tabulados\Culminacion_mas20.xls") sh("Q`quintil'") append
	}
    *

** Sexo
	
	foreach sexo of numlist 1/2  {

local i=1

	foreach anio of numlist 11/19  {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear
	
	g expulsiontemp = 0
	replace expulsiontemp = 1 if bc_edu_1<12
	
	qui: mean expulsiontemp [aw=bc_peso] if bc_pe3>=20 & bc_pe2==`sexo'
	matrix MATR  [`i',4]=  e(b)
	local i=`i'+1

	}
xml_tab MATR, save("$tabulados\Culminacion_mas20.xls") sh("S`sexo'") append
	}
    *

** Edad
	

	foreach edad of numlist 1/3  {


local i=1

	foreach anio of numlist 11/19  {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear
	
	g expulsiontemp = 0
	replace expulsiontemp = 1 if bc_edu_1<12
	
	g tramo=.
	replace tramo=1 if bc_pe3==20
	replace tramo=2 if bc_pe3>=21 & bc_pe3<=23
	replace tramo=3 if bc_pe3>=24
	
	qui: mean expulsiontemp [aw=bc_peso] if bc_pe3>=20 & tramo==`edad'
	matrix MATR  [`i',4]=  e(b)
	local i=`i'+1

	}
xml_tab MATR, save("$tabulados\Culminacion_mas20.xls") sh("E`edad'") append
	}
    *

	
	
** Departamento

	
	foreach dpto of numlist 1/19  {

local i=1

	foreach anio of numlist 11/18  {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear
	
	g expulsiontemp = 0
	replace expulsiontemp = 1 if bc_edu_1<12
	
	qui: mean expulsiontemp [aw=bc_peso] if bc_pe3>=20 & bc_dpto==`dpto'
	matrix MATR  [`i',4]=  e(b)
	local i=`i'+1

	}
    *
xml_tab MATR, save("$tabulados\Culminacion_mas20.xls") sh("D`dpto'") append

	}
    *
	
** Ascendencia

	
	foreach asc of numlist 1/2  {

local i=1

	foreach anio of numlist 11/19  {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear
	
	g expulsiontemp = 0
	replace expulsiontemp = 1 if bc_edu_1<12
	
	qui: mean expulsiontemp [aw=bc_peso] if bc_pe3>=20 & bd_e29_1==`asc'
	matrix MATR  [`i',4]=  e(b)
	local i=`i'+1

	}
xml_tab MATR, save("$tabulados\Culminacion_mas20.xls") sh("A`asc'") append
	}
    *


