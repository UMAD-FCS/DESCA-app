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

	cd "C:\Users\Usuario\Dropbox\1. Unidad de Métodos y Acceso a Datos"
	global bases "C:\Users\Usuario\Dropbox\1. Unidad de Métodos y Acceso a Datos\1. Observatorio UMAD\PISO I_MICRODATOS\ECH\Microdatos\Compatibilizadas Iecon\En uso"
	global tabulados "C:\Users\Usuario\Dropbox\1. Unidad de Métodos y Acceso a Datos\Convenio INDDHH\Tabulados"

*======================================================================================*
* % de pea que no aporta a la seguridad social
*======================================================================================*

	matrix def       MATR= J(13,4,.)
	matrix colnames  MATR= INDICADOR VARIABLE CATEGORIA VALOR 
	matrix rownames  MATR= 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 
/*
	foreach anio of numlist 7/8  {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear

	g umad_f84 = f87
    save "$bases\Fusionada_personasyhogares_`anio'.dta", replace

	}
    *
	
	foreach anio of numlist 9/19  {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear

	g umad_f84 = f84
    save "$bases\Fusionada_personasyhogares_`anio'.dta", replace

	}
    *
	*/
local i=1

	foreach anio of numlist 7/19  {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear

	g noaporta=0
	replace noaporta=1 if umad_f84 == 2
	
	qui: mean noaporta [aw=bc_peso] if bc_pobp==2
	matrix MATR  [`i',4]=  e(b)
	local i=`i'+1

	}
    *

**
xml_tab MATR, save("$tabulados\SS_activos_ocupadosnoaportatotalidad.xls") sh("T") replace
**


** Quintiles

	foreach quintil of numlist 1/5  {

local i=1

	foreach anio of numlist 7/19  {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear
	
	g noaporta=0
	replace noaporta=1 if umad_f84 == 2
	
	qui: mean noaporta [aw=bc_peso] if bc_pobp==2  & bd_quintilesy==`quintil'

	matrix MATR  [`i',4]=  e(b)
	local i=`i'+1

	}
xml_tab MATR, save("$tabulados\SS_activos_ocupadosnoaportatotalidad.xls") sh("Q`quintil'") append
	}
    *

** Sexo
	
	foreach sexo of numlist 1/2  {

local i=1

	foreach anio of numlist 7/19  {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear
	
	g noaporta=0
	replace noaporta=1 if umad_f84 == 2
	
	qui: mean noaporta [aw=bc_peso] if bc_pobp==2 & bc_pe2==`sexo'	

	matrix MATR  [`i',4]=  e(b)
	local i=`i'+1

	}
xml_tab MATR, save("$tabulados\SS_activos_ocupadosnoaportatotalidad.xls") sh("S`sexo'") append
	}
    *

	
	
** Departamento

	
	foreach dpto of numlist 1/19  {

local i=1

	foreach anio of numlist 7/19  {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear
	
	g noaporta=0
	replace noaporta=1 if umad_f84 == 2
	
	qui: mean noaporta [aw=bc_peso] if bc_pobp==2 & bc_dpto==`dpto'

	matrix MATR  [`i',4]=  e(b)
	local i=`i'+1

	}
    *
xml_tab MATR, save("$tabulados\SS_activos_ocupadosnoaportatotalidad.xls") sh("D`dpto'") append

	}
    *
	
** Ascendencia

	
	foreach asc of numlist 1/2  {

local i=1

	foreach anio of numlist 7/19  {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear
	
	g noaporta=0
	replace noaporta=1 if umad_f84 == 2
	
	qui: mean noaporta [aw=bc_peso] if bc_pobp==2 & bd_e29_1==`asc'

	matrix MATR  [`i',4]=  e(b)
	local i=`i'+1

	}
xml_tab MATR, save("$tabulados\SS_activos_ocupadosnoaportatotalidad.xls") sh("A`asc'") append
	}
    *


