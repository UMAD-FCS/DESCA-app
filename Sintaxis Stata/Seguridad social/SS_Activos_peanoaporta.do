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

	matrix def       MATR= J(14,4,.)
	matrix colnames  MATR= INDICADOR VARIABLE CATEGORIA VALOR 
	matrix rownames  MATR= 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 


local i=1

	foreach anio of numlist 6/19  {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear

	g pea = 0
	replace pea=1 if bc_pobp>=2 & bc_pobp<=5
	
	g noaporta=0
	replace noaporta=1 if bc_register2 == 2
	
	qui: mean noaporta [aw=bc_peso] if pea==1
	matrix MATR  [`i',4]=  e(b)
	local i=`i'+1

	}
    *

**
xml_tab MATR, save("$tabulados\SS_activos_peanoaporta.xls") sh("T") replace
**


** Quintiles

	foreach quintil of numlist 1/5  {

local i=1

	foreach anio of numlist 6/19  {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear
	
	g pea = 0
	replace pea=1 if bc_pobp>=2 & bc_pobp<=5
	
	g noaporta=0
	replace noaporta=1 if bc_register2 == 2
	
	qui: mean noaporta [aw=bc_peso] if pea==1 & bd_quintilesy==`quintil'

	matrix MATR  [`i',4]=  e(b)
	local i=`i'+1

	}
xml_tab MATR, save("$tabulados\SS_activos_peanoaporta.xls") sh("Q`quintil'") append
	}
    *

** Sexo
	
	foreach sexo of numlist 1/2  {

local i=1

	foreach anio of numlist 6/19  {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear
	
	g pea = 0
	replace pea=1 if bc_pobp>=2 & bc_pobp<=5
	
	g noaporta=0
	replace noaporta=1 if bc_register2 == 2
	
	qui: mean noaporta [aw=bc_peso] if pea==1 & bc_pe2==`sexo'	
	matrix MATR  [`i',4]=  e(b)
	local i=`i'+1

	}
xml_tab MATR, save("$tabulados\SS_activos_peanoaporta.xls") sh("S`sexo'") append
	}
    *

	
	
** Departamento

	
	foreach dpto of numlist 1/19  {

local i=1

	foreach anio of numlist 6/19  {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear
	
	g pea = 0
	replace pea=1 if bc_pobp>=2 & bc_pobp<=5
	
	g noaporta=0
	replace noaporta=1 if bc_register2 == 2
	
	qui: mean noaporta [aw=bc_peso] if pea==1  & bc_dpto==`dpto'
	matrix MATR  [`i',4]=  e(b)
	local i=`i'+1

	}
    *
xml_tab MATR, save("$tabulados\SS_activos_peanoaporta.xls") sh("D`dpto'") append

	}
    *
	
** Ascendencia

	
	foreach asc of numlist 1/2  {

local i=1

	foreach anio of numlist 6/19  {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear
	
	g pea = 0
	replace pea=1 if bc_pobp>=2 & bc_pobp<=5
	
	g noaporta=0
	replace noaporta=1 if bc_register2 == 2
	
	qui: mean noaporta [aw=bc_peso] if pea==1 & bd_e29_1==`asc'
	matrix MATR  [`i',4]=  e(b)
	local i=`i'+1

	}
xml_tab MATR, save("$tabulados\SS_activos_peanoaporta.xls") sh("A`asc'") append
	}
    *


