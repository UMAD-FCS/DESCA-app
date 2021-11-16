
*=============================================================================*
*                     **COMPATIBILIZACIÓN DE VARIABLES**
*
* Creación: 11/11/2020
* Institución: UMAD, FCS-UdelaR
* Responsable: Jimena Pandolfi
* Descripción: Generación de varaible Línea de pobreza individual 2006 a 2019
*
*
*======================================================================================*

*======================================================================================*
*Ubucación de archivos:
	cd "C:\Users\Usuario\Dropbox\1. Unidad de Métodos y Acceso a Datos"
	global bases "C:\Users\Usuario\Dropbox\1. Unidad de Métodos y Acceso a Datos\1. Observatorio UMAD\PISO I_MICRODATOS\ECH\Microdatos\Compatibilizadas Iecon\En uso"
*======================================================================================*


	foreach anio of numlist 6/19  {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear

g regionlp = 0
replace regionlp = 1 if region_4 == 1
replace regionlp = 2 if region_4 == 2 | region_4 == 3
replace regionlp = 3 if region_4 == 4


g grupolp=0

local i = 1
foreach region of numlist 1/3  {
foreach mes of numlist 1/12  {

replace grupolp = `i' if bc_mes== `mes' & regionlp==`region'

	local i=`i'+1
	}
	}
*

g lp_unipersonales=0
replace lp_unipersonales = lp_06 if ht19==1

egen lp06_individual = max (lp_unipersonales), by (grupolp)

lab var lp06_individual "Línea de pobreza individual"
drop regionlp grupolp lp_unipersonales
   
   
   save "$bases\Fusionada_personasyhogares_`anio'.dta", replace


	}
    *
