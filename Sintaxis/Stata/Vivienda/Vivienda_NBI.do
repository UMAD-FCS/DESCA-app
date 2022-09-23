***==================================================================================*
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
* Porcentaje de personas residiendo en hogares con NBI
*======================================================================================*

	matrix def       MATR= J(13,4,.)
	matrix colnames  MATR= INDICADOR VARIABLE CATEGORIA VALOR 
	matrix rownames  MATR= 2006 2011 2014 2015 2016 2017 2018 2019 

	xml_tab MATR, save("$tabulados\NBI.xls") replace

local i=1

	foreach dpto of numlist 1/19  {

local i=1

	foreach anio of numlist 6 11 14/19 {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear
	
	qui: mean NBI_2011 [aw=bc_peso] if bc_dpto==`dpto'
	matrix MATR  [`i',4]=  e(b)
	local i=`i'+1

	}
    *
xml_tab MATR, save("$tabulados\NBI.xls") sh("D`dpto'") append

	}
    *
	