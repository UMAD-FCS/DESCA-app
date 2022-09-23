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
* Personas en hogares sin servicio higiénico de calidad
*======================================================================================*

	matrix def       MATR= J(14,4,.)
	matrix colnames  MATR= INDICADOR VARIABLE CATEGORIA VALOR 
	matrix rownames  MATR= 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 

	xml_tab MATR, save("$tabulados\Servicio higiénico.xls") replace

local i=1

	foreach dpto of numlist 1/19  {

local i=1

	foreach anio of numlist 6/19 {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear
	
	qui: mean NBI_servhigien11 [aw=bc_peso] if bc_dpto==`dpto'
	matrix MATR  [`i',4]=  e(b)
	local i=`i'+1

	}
    *
xml_tab MATR, save("$tabulados\Servicio higiénico.xls") sh("D`dpto'") append

	}
    *
