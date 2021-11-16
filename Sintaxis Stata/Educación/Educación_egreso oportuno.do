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
* Egreso oportuno	% 15-16 sin EMB, % 18-19 años sin EMS completa		2006-2019	ECH	
*======================================================================================*

*% 15-16 sin EMB

	matrix def       MATR= J(9,4,.)
	matrix colnames  MATR= INDICADOR VARIABLE CATEGORIA VALOR 
	matrix rownames  MATR= 2011 2012 2013 2014 2015 2016 2017 2018 2019 

local i=1

	foreach anio of numlist 11/18  {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear

	g expulsiontemp = 0
	replace expulsiontemp = 1 if bc_edu_1<9

	qui: mean expulsiontemp [aw=bc_peso] if bc_pe3>=15 & bc_pe3<=16
	matrix MATR  [`i',4]=  e(b)
	local i=`i'+1

	}
    *
**
xml_tab MATR, save("$tabulados\Egresooportuno_15a16.xls") sh("T") replace
**

*% 18-19 años sin EMS completa

	matrix def       MATR= J(9,4,.)
	matrix colnames  MATR= INDICADOR VARIABLE CATEGORIA VALOR 
	matrix rownames  MATR= 2011 2012 2013 2014 2015 2016 2017 2018 2019 

local i=1

	foreach anio of numlist 11/18  {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear

	g expulsiontemp = 0
	replace expulsiontemp = 1 if bc_edu_1<12

	qui: mean expulsiontemp [aw=bc_peso] if bc_pe3>=18 & bc_pe3<=19
	matrix MATR  [`i',4]=  e(b)
	local i=`i'+1

	}
    *
**
xml_tab MATR, save("$tabulados\Egresooportuno_18a19.xls") sh("T") replace
**
