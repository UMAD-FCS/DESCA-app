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
* Asistencia	% de no asistencia	Quintil de ingresos, sexo, edades, afrodescendientes,
* departamento	2006-2019	ECH	Párametros: 4 a 17 años (Educación obligatoria)  
* / 0 a 3 años / 18 a 24 años / 0 a 24 años 
*======================================================================================*

	matrix def       MATR= J(14,4,.)
	matrix colnames  MATR= INDICADOR VARIABLE CATEGORIA VALOR 
	matrix rownames  MATR= 2006 2007 2008 2009 /*
	*/					   2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 


*~~~~~~~~~~~~~*
** 0 a 3 años
*~~~~~~~~~~~~~*

** Total

local i=1

	foreach anio of numlist 12/19  {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear

	g asiste1=0
	replace asiste1=1 if e238==1 | e193==1 | e197==1
	
	qui: mean asiste1 [aw=bc_peso] if bc_pe3>=0 & bc_pe3<=3
	matrix MATR  [`i',4]=  e(b)
	local i=`i'+1

	}
    *
**
xml_tab MATR, save("$tabulados\Asistencia_0a3.xls") sh("T") replace
**

** Quintiles

	foreach quintil of numlist 1/5  {

local i=1

	foreach anio of numlist 12/19  {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear
	
	g asiste1=0
	replace asiste1=1 if e238==1 | e193==1 | e197==1
	
	qui: mean asiste1 [aw=bc_peso] if bc_pe3>=0 & bc_pe3<=3 & bd_quintilesy==`quintil'
	matrix MATR  [`i',4]=  e(b)
	local i=`i'+1

	}
xml_tab MATR, save("$tabulados\Asistencia_0a3.xls") sh("Q`quintil'") append
	}
    *

** Sexo
	
	foreach sexo of numlist 1/2  {

local i=1

	foreach anio of numlist 12/19  {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear
	
	g asiste1=0
	replace asiste1=1 if e238==1 | e193==1 | e197==1
	
	qui: mean asiste1 [aw=bc_peso] if bc_pe3>=0 & bc_pe3<=3 & bc_pe2==`sexo'
	matrix MATR  [`i',4]=  e(b)
	local i=`i'+1

	}
xml_tab MATR, save("$tabulados\Asistencia_0a3.xls") sh("S`sexo'") append
	}
    *

** Edad
	

	foreach edad of numlist 0/3  {


local i=1

	foreach anio of numlist 12/19  {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear
	
	g asiste1=0
	replace asiste1=1 if e238==1 | e193==1 | e197==1
	
	qui: mean asiste1 [aw=bc_peso] if bc_pe3>=0 & bc_pe3<=3 & bc_pe3==`edad'
	matrix MATR  [`i',4]=  e(b)
	local i=`i'+1

	}
xml_tab MATR, save("$tabulados\Asistencia_0a3.xls") sh("E`edad'") append
	}
    *

	
	
** Departamento

	
	foreach dpto of numlist 1/19  {

local i=1

	foreach anio of numlist 12/19 {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear
	
	g asiste1=0
	replace asiste1=1 if e238==1 | e193==1 | e197==1
	
	qui: mean asiste1 [aw=bc_peso] if bc_pe3>=0 & bc_pe3<=3 & bc_dpto==`dpto'
	matrix MATR  [`i',4]=  e(b)
	local i=`i'+1

	}
    *
xml_tab MATR, save("$tabulados\Asistencia_0a3.xls") sh("D`dpto'") append

	}
    *
	
** Ascendencia

	
	foreach asc of numlist 1/2  {

local i=1

	foreach anio of numlist 12/19  {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear
	
	g asiste1=0
	replace asiste1=1 if e238==1 | e193==1 | e197==1
	
	qui: mean asiste1 [aw=bc_peso] if bc_pe3>=0 & bc_pe3<=3 & bd_e29_1==`asc'
	matrix MATR  [`i',4]=  e(b)
	local i=`i'+1

	}
xml_tab MATR, save("$tabulados\Asistencia_0a3.xls") sh("A`asc'") append
	}
    *

*~~~~~~~~~~~~~*
** 4 a 17 años
*~~~~~~~~~~~~~*

** Total

local i=1

	foreach anio of numlist 6/19  {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear

	qui: ta bc_pe11 if bc_pe11!=-9 & bc_pe11!=-13 & bc_pe11!=-15, g (asiste)
	
	qui: mean asiste1 [aw=bc_peso] if bc_pe3>=4 & bc_pe3<=17
	matrix MATR  [`i',4]=  e(b)
	local i=`i'+1

	}
    *
**
xml_tab MATR, save("$tabulados\Asistencia_4a17.xls") sh("T") replace
**

** Quintiles

	foreach quintil of numlist 1/5  {

local i=1

	foreach anio of numlist 6/19  {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear
	
	qui: ta bc_pe11 if bc_pe11!=-9 & bc_pe11!=-13 & bc_pe11!=-15, g (asiste)
	
	qui: mean asiste1 [aw=bc_peso] if bc_pe3>=4 & bc_pe3<=17 & bd_quintilesy==`quintil'
	matrix MATR  [`i',4]=  e(b)
	local i=`i'+1

	}
xml_tab MATR, save("$tabulados\Asistencia_4a17.xls") sh("Q`quintil'") append
	}
    *

** Sexo
	
	foreach sexo of numlist 1/2  {

local i=1

	foreach anio of numlist 6/19  {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear
	
	qui: ta bc_pe11 if bc_pe11!=-9 & bc_pe11!=-13 & bc_pe11!=-15, g (asiste)
	
	qui: mean asiste1 [aw=bc_peso] if bc_pe3>=4 & bc_pe3<=17 & bc_pe2==`sexo'
	matrix MATR  [`i',4]=  e(b)
	local i=`i'+1

	}
xml_tab MATR, save("$tabulados\Asistencia_4a17.xls") sh("S`sexo'") append
	}
    *

** Edad
	

	foreach edad of numlist 4/17  {


local i=1

	foreach anio of numlist 6/19  {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear
	
	qui: ta bc_pe11 if bc_pe11!=-9 & bc_pe11!=-13 & bc_pe11!=-15, g (asiste)
	
	qui: mean asiste1 [aw=bc_peso] if bc_pe3>=4 & bc_pe3<=17 & bc_pe3==`edad'
	matrix MATR  [`i',4]=  e(b)
	local i=`i'+1

	}
xml_tab MATR, save("$tabulados\Asistencia_4a17.xls") sh("E`edad'") append
	}
    *

	
	
** Departamento

	
	foreach dpto of numlist 1/19  {

local i=1

	foreach anio of numlist 6/19 {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear
	
	qui: ta bc_pe11 if bc_pe11!=-9 & bc_pe11!=-13 & bc_pe11!=-15, g (asiste)
	
	qui: mean asiste1 [aw=bc_peso] if bc_pe3>=4 & bc_pe3<=17 & bc_dpto==`dpto'
	matrix MATR  [`i',4]=  e(b)
	local i=`i'+1

	}
    *
xml_tab MATR, save("$tabulados\Asistencia_4a17.xls") sh("D`dpto'") append

	}
    *
	
** Ascendencia

	
	foreach asc of numlist 1/2  {

local i=1

	foreach anio of numlist 6/19  {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear
	
	qui: ta bc_pe11 if bc_pe11!=-9 & bc_pe11!=-13 & bc_pe11!=-15, g (asiste)
	
	qui: mean asiste1 [aw=bc_peso] if bc_pe3>=4 & bc_pe3<=17 & bd_e29_1==`asc'
	matrix MATR  [`i',4]=  e(b)
	local i=`i'+1

	}
xml_tab MATR, save("$tabulados\Asistencia_4a17.xls") sh("A`asc'") append
	}
    *

*~~~~~~~~~~~~~*
** 18 a 24 años
*~~~~~~~~~~~~~*

** Total

local i=1

	foreach anio of numlist 6/19  {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear

	qui: ta bc_pe11 if bc_pe11!=-9 & bc_pe11!=-13 & bc_pe11!=-15, g (asiste)
	
	qui: mean asiste1 [aw=bc_peso] if bc_pe3>=18 & bc_pe3<=24
	matrix MATR  [`i',4]=  e(b)
	local i=`i'+1

	}
    *
**
xml_tab MATR, save("$tabulados\Asistencia_18a24.xls") sh("T") replace
**

** Quintiles

	foreach quintil of numlist 1/5  {

local i=1

	foreach anio of numlist 6/19  {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear
	
	qui: ta bc_pe11 if bc_pe11!=-9 & bc_pe11!=-13 & bc_pe11!=-15, g (asiste)
	
	qui: mean asiste1 [aw=bc_peso] if bc_pe3>=18 & bc_pe3<=24 & bd_quintilesy==`quintil'
	matrix MATR  [`i',4]=  e(b)
	local i=`i'+1

	}
xml_tab MATR, save("$tabulados\Asistencia_18a24.xls") sh("Q`quintil'") append
	}
    *

** Sexo
	
	foreach sexo of numlist 1/2  {

local i=1

	foreach anio of numlist 6/19  {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear
	
	qui: ta bc_pe11 if bc_pe11!=-9 & bc_pe11!=-13 & bc_pe11!=-15, g (asiste)
	
	qui: mean asiste1 [aw=bc_peso] if bc_pe3>=18 & bc_pe3<=24 & bc_pe2==`sexo'
	matrix MATR  [`i',4]=  e(b)
	local i=`i'+1

	}
xml_tab MATR, save("$tabulados\Asistencia_18a24.xls") sh("S`sexo'") append
	}
    *

** Edad
	

	foreach edad of numlist 18/24  {


local i=1

	foreach anio of numlist 6/19  {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear
	
	qui: ta bc_pe11 if bc_pe11!=-9 & bc_pe11!=-13 & bc_pe11!=-15, g (asiste)
	
	qui: mean asiste1 [aw=bc_peso] if bc_pe3>=18 & bc_pe3<=24 & bc_pe3==`edad'
	matrix MATR  [`i',4]=  e(b)
	local i=`i'+1

	}
xml_tab MATR, save("$tabulados\Asistencia_18a24.xls") sh("E`edad'") append
	}
    *

	
	
** Departamento

	
	foreach dpto of numlist 1/19  {

local i=1

	foreach anio of numlist 6/19 {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear
	
	qui: ta bc_pe11 if bc_pe11!=-9 & bc_pe11!=-13 & bc_pe11!=-15, g (asiste)
	
	qui: mean asiste1 [aw=bc_peso] if bc_pe3>=18 & bc_pe3<=24 & bc_dpto==`dpto'
	matrix MATR  [`i',4]=  e(b)
	local i=`i'+1

	}
    *
xml_tab MATR, save("$tabulados\Asistencia_18a24.xls") sh("D`dpto'") append

	}
    *
	
** Ascendencia

	
	foreach asc of numlist 1/2  {

local i=1

	foreach anio of numlist 6/19  {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear
	
	qui: ta bc_pe11 if bc_pe11!=-9 & bc_pe11!=-13 & bc_pe11!=-15, g (asiste)
	
	qui: mean asiste1 [aw=bc_peso] if bc_pe3>=18 & bc_pe3<=24 & bd_e29_1==`asc'
	matrix MATR  [`i',4]=  e(b)
	local i=`i'+1

	}
xml_tab MATR, save("$tabulados\Asistencia_18a24.xls") sh("A`asc'") append
	}
    *


*~~~~~~~~~~~~~*
** 0 a 24 años
*~~~~~~~~~~~~~*


** Total

local i=1

	foreach anio of numlist 12/19  {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear

	g asiste1=0
	replace asiste1=1 if e238==1 | e193==1 | e197==1
	replace asiste1=1 if bc_pe11==1
	
	qui: mean asiste1 [aw=bc_peso] if  bc_pe3>=0 & bc_pe3<=24
	matrix MATR  [`i',4]=  e(b)
	local i=`i'+1

	}
    *
**
xml_tab MATR, save("$tabulados\Asistencia_0a24.xls") sh("T") replace
**

** Quintiles

	foreach quintil of numlist 1/5  {

local i=1

	foreach anio of numlist 12/19  {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear
	
	g asiste1=0
	replace asiste1=1 if e238==1 | e193==1 | e197==1
	replace asiste1=1 if bc_pe11==1

	qui: mean asiste1 [aw=bc_peso] if  bc_pe3>=0 & bc_pe3<=24 & bd_quintilesy==`quintil'
	matrix MATR  [`i',4]=  e(b)
	local i=`i'+1

	}
xml_tab MATR, save("$tabulados\Asistencia_0a24.xls") sh("Q`quintil'") append
	}
    *

** Sexo
	
	foreach sexo of numlist 1/2  {

local i=1

	foreach anio of numlist 12/19  {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear
	
	g asiste1=0
	replace asiste1=1 if e238==1 | e193==1 | e197==1
	replace asiste1=1 if bc_pe11==1

	qui: mean asiste1 [aw=bc_peso] if  bc_pe3>=0 & bc_pe3<=24 & bc_pe2==`sexo'
	matrix MATR  [`i',4]=  e(b)
	local i=`i'+1

	}
xml_tab MATR, save("$tabulados\Asistencia_0a24.xls") sh("S`sexo'") append
	}
    *

** Edad
	

	foreach edad of numlist 0/24  {


local i=1

	foreach anio of numlist 12/19  {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear
	
	g asiste1=0
	replace asiste1=1 if e238==1 | e193==1 | e197==1
	replace asiste1=1 if bc_pe11==1

	qui: mean asiste1 [aw=bc_peso] if  bc_pe3>=0 & bc_pe3<=24 & bc_pe3==`edad'
	matrix MATR  [`i',4]=  e(b)
	local i=`i'+1

	}
xml_tab MATR, save("$tabulados\Asistencia_0a24.xls") sh("E`edad'") append
	}
    *

	
	
** Departamento

	
	foreach dpto of numlist 1/19  {

local i=1

	foreach anio of numlist 12/19 {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear
	
	g asiste1=0
	replace asiste1=1 if e238==1 | e193==1 | e197==1
	replace asiste1=1 if bc_pe11==1
	
	qui: mean asiste1 [aw=bc_peso] if  bc_pe3>=0 & bc_pe3<=24 & bc_dpto==`dpto'
	matrix MATR  [`i',4]=  e(b)
	local i=`i'+1

	}
    *
xml_tab MATR, save("$tabulados\Asistencia_0a24.xls") sh("D`dpto'") append

	}
    *
	
** Ascendencia

	
	foreach asc of numlist 1/2  {

local i=1

	foreach anio of numlist 12/19  {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear
	
	g asiste1=0
	replace asiste1=1 if e238==1 | e193==1 | e197==1
	replace asiste1=1 if bc_pe11==1
	
	qui: mean asiste1 [aw=bc_peso] if bc_pe3>=0 & bc_pe3<=24 & bd_e29_1==`asc'
	matrix MATR  [`i',4]=  e(b)
	local i=`i'+1

	}
xml_tab MATR, save("$tabulados\Asistencia_0a24.xls") sh("A`asc'") append
	}
    *
