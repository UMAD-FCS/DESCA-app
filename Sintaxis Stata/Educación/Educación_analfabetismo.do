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
* Analfabetismo	% analfabetismo 		2006-2019	ECH	Parámetro: 15-24 años (UNESCO)
*======================================================================================*


	matrix def       MATR= J(14,4,.)
	matrix colnames  MATR= INDICADOR VARIABLE CATEGORIA VALOR 
	matrix rownames  MATR= 2006 2007 2008 2009 /*
	*/					   2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 

local i=1

	foreach anio of numlist 6/19  {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear

	qui: ta bd_e47 if bd_e47!=-9 & bd_e47!=-13 & bd_e47!=-15 & bd_e47!=-0, g (analf)
	
	qui: mean analf2 [aw=bc_peso] if bc_pe3>=15 & bc_pe3<=24
	matrix MATR  [`i',4]=  e(b)
	local i=`i'+1

	}
    *
**
xml_tab MATR, save("$tabulados\Analfabetismo.xls") sh("T") replace
**
