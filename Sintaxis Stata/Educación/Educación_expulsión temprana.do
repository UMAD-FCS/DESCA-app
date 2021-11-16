
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
* Explulsión  temprana 	% 18-19 años sin EMB / EMS completa que no asisten	2006-2019		
*======================================================================================*


	matrix def       MATR= J(9,4,.)
	matrix colnames  MATR= INDICADOR VARIABLE CATEGORIA VALOR 
	matrix rownames  MATR= 2011 2012 2013 2014 2015 2016 2017 2018 2019 

local i=1

	foreach anio of numlist 11/19  {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear

	g expulsiontemp = 0
	replace expulsiontemp = 1 if bc_edu_1<12 & bc_pe11==2

	qui: mean expulsiontemp [aw=bc_peso] if bc_pe3>=18 & bc_pe3<=19
	matrix MATR  [`i',4]=  e(b)
	local i=`i'+1

	}
    *
**
xml_tab MATR, save("$tabulados\Expulsion temprana.xls") sh("T") replace
**


** Quintiles

	foreach quintil of numlist 1/5  {

local i=1

	foreach anio of numlist 11/19  {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear
	
	g expulsiontemp = 0
	replace expulsiontemp = 1 if bc_edu_1<12 & bc_pe11==2

	qui: mean expulsiontemp [aw=bc_peso] if bc_pe3>=18 & bc_pe3<=19 & bd_quintilesy==`quintil'
	matrix MATR  [`i',4]=  e(b)
	local i=`i'+1

	}
xml_tab MATR, save("$tabulados\Expulsion temprana.xls") sh("Q`quintil'") append
	}
    *

** Sexo
	
	foreach sexo of numlist 1/2  {

local i=1

	foreach anio of numlist 11/19  {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear
	
	g expulsiontemp = 0
	replace expulsiontemp = 1 if bc_edu_1<12 & bc_pe11==2
	
	qui: mean expulsiontemp [aw=bc_peso] if bc_pe3>=18 & bc_pe3<=19 & bc_pe2==`sexo'
	matrix MATR  [`i',4]=  e(b)
	local i=`i'+1

	}
xml_tab MATR, save("$tabulados\Expulsion temprana.xls") sh("S`sexo'") append
	}
    *

** Edad
	

	foreach edad of numlist 18/19  {


local i=1

	foreach anio of numlist 11/19  {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear
	
	g expulsiontemp = 0
	replace expulsiontemp = 1 if bc_edu_1<12 & bc_pe11==2
	
	qui: mean expulsiontemp [aw=bc_peso] if bc_pe3>=18 & bc_pe3<=19 & bc_pe3==`edad'
	matrix MATR  [`i',4]=  e(b)
	local i=`i'+1

	}
xml_tab MATR, save("$tabulados\Expulsion temprana.xls") sh("E`edad'") append
	}
    *

	
	
** Departamento

	
	foreach dpto of numlist 1/19  {

local i=1

	foreach anio of numlist 11/19  {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear
	
	g expulsiontemp = 0
	replace expulsiontemp = 1 if bc_edu_1<12 & bc_pe11==2
	
	qui: mean expulsiontemp [aw=bc_peso] if bc_pe3>=18 & bc_pe3<=19 & bc_dpto==`dpto'
	matrix MATR  [`i',4]=  e(b)
	local i=`i'+1

	}
    *
xml_tab MATR, save("$tabulados\Expulsion temprana.xls") sh("D`dpto'") append

	}
    *
	
** Ascendencia

	
	foreach asc of numlist 1/2  {

local i=1

	foreach anio of numlist 11/19  {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear
	
	g expulsiontemp = 0
	replace expulsiontemp = 1 if bc_edu_1<12 & bc_pe11==2
	
	qui: mean expulsiontemp [aw=bc_peso] if bc_pe3>=18 & bc_pe3<=19 & bd_e29_1==`asc'
	matrix MATR  [`i',4]=  e(b)
	local i=`i'+1

	}
xml_tab MATR, save("$tabulados\Expulsion temprana.xls") sh("A`asc'") append
	}
    *


