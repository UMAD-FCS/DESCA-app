*==================================================================================*
*                       **   OBSERVATORIO DDHH  **
*                          INDICADORES VIVIENDA 
*
*
* Creación:      9/10/2020
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
* % personas que no perciben prestaciones 
*======================================================================================*

ta pobpcoac [fw=bc_pesoan] if bc_pe3>=65, m

g jubbps = 0
replace jubbps = 1 if /*
*/g148_1_1>0 |/*
*/g148_1_2>0 |/*
*/g148_1_3>0 

g pensbps = 0
replace pensbps = 1 if /*
*/g148_2_1>0 |/*
*/g148_2_2>0 |/*
*/g148_2_3>0 

g pasbps = 0
replace pasbps = 1 if pensbps==1 | jubbps ==1

ta jubbps [fw=bc_pesoan]  if bc_pe3>=65, m
ta pensbps [fw=bc_pesoan] if bc_pe3>=65, m
ta pasbps [fw=bc_pesoan]  if bc_pe3>=65, m

g mayores = 0 
replace mayores = 1 if bc_pe3>=65

**JUBILCIONES
/*UNIÓN POSTAL*/ta mayores [fw=bc_pesoan]  if g148_1_4  > 0
/*POLICIAL*/	ta mayores [fw=bc_pesoan]  if g148_1_5  > 0
/*MILITAR*/		ta mayores [fw=bc_pesoan]  if g148_1_6  > 0
/*PROFESIONAL*/	ta mayores [fw=bc_pesoan]  if g148_1_7  > 0
/*NOTARIAL*/	ta mayores [fw=bc_pesoan]  if g148_1_8  > 0
/*BANCARIA*/	ta mayores [fw=bc_pesoan]  if g148_1_9  > 0
/*AFAP*/ 		ta mayores [fw=bc_pesoan]  if g148_1_12 > 0
/*OTRA*/ 		ta mayores [fw=bc_pesoan]  if g148_1_10 > 0
/*OTRO PAÍS*/	ta mayores [fw=bc_pesoan]  if g148_1_11 > 0


g jub=0
replace jub=1 if /*
*/g148_1_1 > 0  |  /*
*/g148_1_2 > 0  | /*
*/g148_1_3 > 0  | /*
*/g148_1_4 > 0  | /*
*/g148_1_5 > 0  | /*
*/g148_1_6 > 0  | /*
*/g148_1_7 > 0  | /*
*/g148_1_8 > 0  | /*
*/g148_1_9 > 0  | /*
*/g148_1_12 > 0  | /*
*/g148_1_10 > 0  | /*
*/g148_1_11 > 0 

ta jub [fw=bc_pesoan]  if bc_pe3>=65, m


**PENSIONES
/*UNIÓN POSTAL*/ ta mayores [fw=bc_pesoan]  if g148_2_4 > 0
/*POLICIAL*/	 ta mayores [fw=bc_pesoan]  if g148_2_5 > 0
/*MILITAR*/		 ta mayores [fw=bc_pesoan]  if g148_2_6 > 0
/*PROFESIONAL*/  ta mayores [fw=bc_pesoan]  if g148_2_7 > 0
/*NOTARIAL*/	 ta mayores [fw=bc_pesoan]  if g148_2_8 > 0
/*BANCARIA*/	 ta mayores [fw=bc_pesoan]  if g148_2_9 > 0
/*AFAP*/ 		 ta mayores [fw=bc_pesoan]  if g148_2_12 > 0
/*OTRA*/ 		 ta mayores [fw=bc_pesoan]  if g148_2_10 > 0
/*OTRO PAÍS*/	 ta mayores [fw=bc_pesoan]  if g148_2_11 > 0
	

g pens=0
replace pens=1 if /*
*/g148_2_1 > 0 |/*
*/g148_2_2 > 0 |/*
*/g148_2_3 > 0 |/*
*/g148_2_4 > 0 |/*
*/g148_2_5 > 0 |/*
*/g148_2_6 > 0 |/*
*/g148_2_7 > 0 |/*
*/g148_2_8 > 0 |/*
*/g148_2_9 > 0 |/*
*/g148_2_12 > 0 |/*
*/g148_2_10 > 0 |/*
*/g148_2_11 > 0

ta pens [fw=bc_pesoan]  if bc_pe3>=65, m


**OTROS
/*SEGURO DE DESEMPLEO*/	ta mayores [fw=bc_pesoan]  if g148_3 > 0
/*COMPENSACIONES POR ACCIDENTE, MATERNIDAD O ENFERMEDAD*/ ta mayores [fw=bc_pesoan] if g148_4 > 0
/*BECAS, SUBSIDIOS, DONACIONES*/ ta mayores [fw=bc_pesoan] if g148_5_1 > 0  |  g148_5_2 > 0
/*RECIBE PENSIÓN ALIMENTICIA O ALGUNA CONTRIBUCIÓN POR DIVORCIO O SEPARACIÓN*/ta mayores [fw=bc_pesoan] if  g153_1> 0 | g153_2 > 0

g pasiv=0
replace pasiv = 1 if jub == 1 | pens == 1

ta pasiv [fw=bc_pesoan]  if bc_pe3>=65, m


**********************************************************************************
	
	matrix def       MATR= J(14,4,.)
	matrix colnames  MATR= INDICADOR VARIABLE CATEGORIA VALOR 
	matrix rownames  MATR= 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 

local i=1

	foreach anio of numlist 6/19  {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear

	g nopercibe = 0
	replace nopercibe = 1 if bc_pg91 == 0 & /*
					*/   bc_pg92 == 0 & /*
					*/   bc_pg911 == 0 & /*
					*/   bc_pg912 == 0 & /*
					*/   bc_pg921 == 0 & /*
					*/   bc_pg922 == 0 

	qui: mean nopercibe [aw=bc_peso] if bc_pe3>=65 & bc_pobp!=2
	matrix MATR  [`i',4]=  e(b)
	local i=`i'+1

	}
    *

**
xml_tab MATR, save("$tabulados\SS_perciben prestaciones.xls") sh("T") replace
**


** Quintiles

	foreach quintil of numlist 1/5  {

local i=1

	foreach anio of numlist 6/19  {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear
	
	g nopercibe = 0
	replace nopercibe = 1 if bc_pg91 == 0 & /*
					*/   bc_pg92 == 0 & /*
					*/   bc_pg911 == 0 & /*
					*/   bc_pg912 == 0 & /*
					*/   bc_pg921 == 0 & /*
					*/   bc_pg922 == 0 

	qui: mean nopercibe [aw=bc_peso] if bc_pe3>=65 & bd_quintilesy==`quintil' & bc_pobp!=2

    matrix MATR  [`i',4]=  e(b)
	local i=`i'+1

	}
xml_tab MATR, save("$tabulados\SS_perciben prestaciones.xls") sh("Q`quintil'") append
	}
    *

** Sexo
	
	foreach sexo of numlist 1/2  {

local i=1

	foreach anio of numlist 6/19  {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear
	
	g nopercibe = 0
	replace nopercibe = 1 if bc_pg91 == 0 & /*
					*/   bc_pg92 == 0 & /*
					*/   bc_pg911 == 0 & /*
					*/   bc_pg912 == 0 & /*
					*/   bc_pg921 == 0 & /*
					*/   bc_pg922 == 0 

	qui: mean nopercibe [aw=bc_peso] if bc_pe3>=65 & bc_pe2==`sexo'	& bc_pobp!=2
	matrix MATR  [`i',4]=  e(b)
	local i=`i'+1

	}
xml_tab MATR, save("$tabulados\SS_perciben prestaciones.xls") sh("S`sexo'") append
	}
    *

	
	
** Departamento

	
	foreach dpto of numlist 1/19  {

local i=1

	foreach anio of numlist 6/19  {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear
	
	g nopercibe = 0
	replace nopercibe = 1 if bc_pg91 == 0 & /*
					*/   bc_pg92 == 0 & /*
					*/   bc_pg911 == 0 & /*
					*/   bc_pg912 == 0 & /*
					*/   bc_pg921 == 0 & /*
					*/   bc_pg922 == 0 

	qui: mean nopercibe [aw=bc_peso] if bc_pe3>=65 & bc_dpto==`dpto'& bc_pobp!=2					
	matrix MATR  [`i',4]=  e(b)
	local i=`i'+1

	}
    *
xml_tab MATR, save("$tabulados\SS_perciben prestaciones.xls") sh("D`dpto'") append

	}
    *
	
** Ascendencia

	
	foreach asc of numlist 1/2  {

local i=1

	foreach anio of numlist 6/19  {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear
	
	g nopercibe = 0
	replace nopercibe = 1 if bc_pg91 == 0 & /*
					*/   bc_pg92 == 0 & /*
					*/   bc_pg911 == 0 & /*
					*/   bc_pg912 == 0 & /*
					*/   bc_pg921 == 0 & /*
					*/   bc_pg922 == 0 
	
	qui: mean nopercibe [aw=bc_peso] if bc_pe3>=65 & bd_e29_1==`asc' & bc_pobp!=2
	matrix MATR  [`i',4]=  e(b)
	local i=`i'+1

	}
xml_tab MATR, save("$tabulados\SS_perciben prestaciones.xls") sh("A`asc'") append
	}
    *


