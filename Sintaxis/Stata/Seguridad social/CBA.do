
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
	cd "C:\Users\jimen\OneDrive\Escritorio\UMAD\"
	global bases "C:\Users\jimen\OneDrive\Escritorio\UMAD\En uso"
	global tabulados "C:\Users\jimen\OneDrive\Escritorio\UMAD\SS INDDHH"
*======================================================================================*


	foreach anio of numlist 15/19  {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear

g regioncba = 0
replace regioncba = 1 if region_4 == 1
replace regioncba = 2 if region_4 == 2 | region_4 == 3
replace regioncba = 3 if region_4 == 4

g cba=.
replace cba=	2584	if bc_anio==	2015	 & bc_mes==	1	 & regioncba==	1
replace cba=	2605	if bc_anio==	2015	 & bc_mes==	2	 & regioncba==	1
replace cba=	2626	if bc_anio==	2015	 & bc_mes==	3	 & regioncba==	1
replace cba=	2632	if bc_anio==	2015	 & bc_mes==	4	 & regioncba==	1
replace cba=	2651	if bc_anio==	2015	 & bc_mes==	5	 & regioncba==	1
replace cba=	2656	if bc_anio==	2015	 & bc_mes==	6	 & regioncba==	1
replace cba=	2705	if bc_anio==	2015	 & bc_mes==	7	 & regioncba==	1
replace cba=	2739	if bc_anio==	2015	 & bc_mes==	8	 & regioncba==	1
replace cba=	2769	if bc_anio==	2015	 & bc_mes==	9	 & regioncba==	1
replace cba=	2777	if bc_anio==	2015	 & bc_mes==	10	 & regioncba==	1
replace cba=	2784	if bc_anio==	2015	 & bc_mes==	11	 & regioncba==	1
replace cba=	2786	if bc_anio==	2015	 & bc_mes==	12	 & regioncba==	1
replace cba=	2805	if bc_anio==	2016	 & bc_mes==	1	 & regioncba==	1
replace cba=	2843	if bc_anio==	2016	 & bc_mes==	2	 & regioncba==	1
replace cba=	2890	if bc_anio==	2016	 & bc_mes==	3	 & regioncba==	1
replace cba=	2904	if bc_anio==	2016	 & bc_mes==	4	 & regioncba==	1
replace cba=	2961	if bc_anio==	2016	 & bc_mes==	5	 & regioncba==	1
replace cba=	2991	if bc_anio==	2016	 & bc_mes==	6	 & regioncba==	1
replace cba=	3005	if bc_anio==	2016	 & bc_mes==	7	 & regioncba==	1
replace cba=	3021	if bc_anio==	2016	 & bc_mes==	8	 & regioncba==	1
replace cba=	3014	if bc_anio==	2016	 & bc_mes==	9	 & regioncba==	1
replace cba=	3015	if bc_anio==	2016	 & bc_mes==	10	 & regioncba==	1
replace cba=	2988	if bc_anio==	2016	 & bc_mes==	11	 & regioncba==	1
replace cba=	2979	if bc_anio==	2016	 & bc_mes==	12	 & regioncba==	1
replace cba=	2981	if bc_anio==	2017	 & bc_mes==	1	 & regioncba==	1
replace cba=	2987	if bc_anio==	2017	 & bc_mes==	2	 & regioncba==	1
replace cba=	3024	if bc_anio==	2017	 & bc_mes==	3	 & regioncba==	1
replace cba=	3028	if bc_anio==	2017	 & bc_mes==	4	 & regioncba==	1
replace cba=	3017	if bc_anio==	2017	 & bc_mes==	5	 & regioncba==	1
replace cba=	3024	if bc_anio==	2017	 & bc_mes==	6	 & regioncba==	1
replace cba=	3040	if bc_anio==	2017	 & bc_mes==	7	 & regioncba==	1
replace cba=	3080	if bc_anio==	2017	 & bc_mes==	8	 & regioncba==	1
replace cba=	3116	if bc_anio==	2017	 & bc_mes==	9	 & regioncba==	1
replace cba=	3132	if bc_anio==	2017	 & bc_mes==	10	 & regioncba==	1
replace cba=	3138	if bc_anio==	2017	 & bc_mes==	11	 & regioncba==	1
replace cba=	3160	if bc_anio==	2017	 & bc_mes==	12	 & regioncba==	1
replace cba=	3200	if bc_anio==	2018	 & bc_mes==	1	 & regioncba==	1
replace cba=	3225	if bc_anio==	2018	 & bc_mes==	2	 & regioncba==	1
replace cba=	3224	if bc_anio==	2018	 & bc_mes==	3	 & regioncba==	1
replace cba=	3214	if bc_anio==	2018	 & bc_mes==	4	 & regioncba==	1
replace cba=	3264	if bc_anio==	2018	 & bc_mes==	5	 & regioncba==	1
replace cba=	3334	if bc_anio==	2018	 & bc_mes==	6	 & regioncba==	1
replace cba=	3375	if bc_anio==	2018	 & bc_mes==	7	 & regioncba==	1
replace cba=	3414	if bc_anio==	2018	 & bc_mes==	8	 & regioncba==	1
replace cba=	3403	if bc_anio==	2018	 & bc_mes==	9	 & regioncba==	1
replace cba=	3390	if bc_anio==	2018	 & bc_mes==	10	 & regioncba==	1
replace cba=	3377	if bc_anio==	2018	 & bc_mes==	11	 & regioncba==	1
replace cba=	3394	if bc_anio==	2018	 & bc_mes==	12	 & regioncba==	1
replace cba=	3427	if bc_anio==	2019	 & bc_mes==	1	 & regioncba==	1
replace cba=	3496	if bc_anio==	2019	 & bc_mes==	2	 & regioncba==	1
replace cba=	3508	if bc_anio==	2019	 & bc_mes==	3	 & regioncba==	1
replace cba=	3495	if bc_anio==	2019	 & bc_mes==	4	 & regioncba==	1
replace cba=	3511	if bc_anio==	2019	 & bc_mes==	5	 & regioncba==	1
replace cba=	3559	if bc_anio==	2019	 & bc_mes==	6	 & regioncba==	1
replace cba=	3631	if bc_anio==	2019	 & bc_mes==	7	 & regioncba==	1
replace cba=	3688	if bc_anio==	2019	 & bc_mes==	8	 & regioncba==	1
replace cba=	3708	if bc_anio==	2019	 & bc_mes==	9	 & regioncba==	1
replace cba=	3758	if bc_anio==	2019	 & bc_mes==	10	 & regioncba==	1
replace cba=	3805	if bc_anio==	2019	 & bc_mes==	11	 & regioncba==	1
replace cba=	3855	if bc_anio==	2019	 & bc_mes==	12	 & regioncba==	1
replace cba=	2418	if bc_anio==	2015	 & bc_mes==	1	 & regioncba==	2
replace cba=	2437	if bc_anio==	2015	 & bc_mes==	2	 & regioncba==	2
replace cba=	2454	if bc_anio==	2015	 & bc_mes==	3	 & regioncba==	2
replace cba=	2464	if bc_anio==	2015	 & bc_mes==	4	 & regioncba==	2
replace cba=	2476	if bc_anio==	2015	 & bc_mes==	5	 & regioncba==	2
replace cba=	2475	if bc_anio==	2015	 & bc_mes==	6	 & regioncba==	2
replace cba=	2513	if bc_anio==	2015	 & bc_mes==	7	 & regioncba==	2
replace cba=	2561	if bc_anio==	2015	 & bc_mes==	8	 & regioncba==	2
replace cba=	2581	if bc_anio==	2015	 & bc_mes==	9	 & regioncba==	2
replace cba=	2573	if bc_anio==	2015	 & bc_mes==	10	 & regioncba==	2
replace cba=	2586	if bc_anio==	2015	 & bc_mes==	11	 & regioncba==	2
replace cba=	2587	if bc_anio==	2015	 & bc_mes==	12	 & regioncba==	2
replace cba=	2606	if bc_anio==	2016	 & bc_mes==	1	 & regioncba==	2
replace cba=	2630	if bc_anio==	2016	 & bc_mes==	2	 & regioncba==	2
replace cba=	2666	if bc_anio==	2016	 & bc_mes==	3	 & regioncba==	2
replace cba=	2686	if bc_anio==	2016	 & bc_mes==	4	 & regioncba==	2
replace cba=	2756	if bc_anio==	2016	 & bc_mes==	5	 & regioncba==	2
replace cba=	2787	if bc_anio==	2016	 & bc_mes==	6	 & regioncba==	2
replace cba=	2794	if bc_anio==	2016	 & bc_mes==	7	 & regioncba==	2
replace cba=	2809	if bc_anio==	2016	 & bc_mes==	8	 & regioncba==	2
replace cba=	2806	if bc_anio==	2016	 & bc_mes==	9	 & regioncba==	2
replace cba=	2807	if bc_anio==	2016	 & bc_mes==	10	 & regioncba==	2
replace cba=	2786	if bc_anio==	2016	 & bc_mes==	11	 & regioncba==	2
replace cba=	2776	if bc_anio==	2016	 & bc_mes==	12	 & regioncba==	2
replace cba=	2778	if bc_anio==	2017	 & bc_mes==	1	 & regioncba==	2
replace cba=	2769	if bc_anio==	2017	 & bc_mes==	2	 & regioncba==	2
replace cba=	2802	if bc_anio==	2017	 & bc_mes==	3	 & regioncba==	2
replace cba=	2804	if bc_anio==	2017	 & bc_mes==	4	 & regioncba==	2
replace cba=	2802	if bc_anio==	2017	 & bc_mes==	5	 & regioncba==	2
replace cba=	2793	if bc_anio==	2017	 & bc_mes==	6	 & regioncba==	2
replace cba=	2805	if bc_anio==	2017	 & bc_mes==	7	 & regioncba==	2
replace cba=	2840	if bc_anio==	2017	 & bc_mes==	8	 & regioncba==	2
replace cba=	2876	if bc_anio==	2017	 & bc_mes==	9	 & regioncba==	2
replace cba=	2893	if bc_anio==	2017	 & bc_mes==	10	 & regioncba==	2
replace cba=	2903	if bc_anio==	2017	 & bc_mes==	11	 & regioncba==	2
replace cba=	2921	if bc_anio==	2017	 & bc_mes==	12	 & regioncba==	2
replace cba=	2972	if bc_anio==	2018	 & bc_mes==	1	 & regioncba==	2
replace cba=	2997	if bc_anio==	2018	 & bc_mes==	2	 & regioncba==	2
replace cba=	3002	if bc_anio==	2018	 & bc_mes==	3	 & regioncba==	2
replace cba=	2987	if bc_anio==	2018	 & bc_mes==	4	 & regioncba==	2
replace cba=	3009	if bc_anio==	2018	 & bc_mes==	5	 & regioncba==	2
replace cba=	3081	if bc_anio==	2018	 & bc_mes==	6	 & regioncba==	2
replace cba=	3096	if bc_anio==	2018	 & bc_mes==	7	 & regioncba==	2
replace cba=	3129	if bc_anio==	2018	 & bc_mes==	8	 & regioncba==	2
replace cba=	3136	if bc_anio==	2018	 & bc_mes==	9	 & regioncba==	2
replace cba=	3125	if bc_anio==	2018	 & bc_mes==	10	 & regioncba==	2
replace cba=	3091	if bc_anio==	2018	 & bc_mes==	11	 & regioncba==	2
replace cba=	3107	if bc_anio==	2018	 & bc_mes==	12	 & regioncba==	2
replace cba=	3152	if bc_anio==	2019	 & bc_mes==	1	 & regioncba==	2
replace cba=	3213	if bc_anio==	2019	 & bc_mes==	2	 & regioncba==	2
replace cba=	3233	if bc_anio==	2019	 & bc_mes==	3	 & regioncba==	2
replace cba=	3212	if bc_anio==	2019	 & bc_mes==	4	 & regioncba==	2
replace cba=	3237	if bc_anio==	2019	 & bc_mes==	5	 & regioncba==	2
replace cba=	3282	if bc_anio==	2019	 & bc_mes==	6	 & regioncba==	2
replace cba=	3328	if bc_anio==	2019	 & bc_mes==	7	 & regioncba==	2
replace cba=	3393	if bc_anio==	2019	 & bc_mes==	8	 & regioncba==	2
replace cba=	3416	if bc_anio==	2019	 & bc_mes==	9	 & regioncba==	2
replace cba=	3459	if bc_anio==	2019	 & bc_mes==	10	 & regioncba==	2
replace cba=	3507	if bc_anio==	2019	 & bc_mes==	11	 & regioncba==	2
replace cba=	3552	if bc_anio==	2019	 & bc_mes==	12	 & regioncba==	2
replace cba=	2187	if bc_anio==	2015	 & bc_mes==	1	 & regioncba==	3
replace cba=	2206	if bc_anio==	2015	 & bc_mes==	2	 & regioncba==	3
replace cba=	2222	if bc_anio==	2015	 & bc_mes==	3	 & regioncba==	3
replace cba=	2231	if bc_anio==	2015	 & bc_mes==	4	 & regioncba==	3
replace cba=	2241	if bc_anio==	2015	 & bc_mes==	5	 & regioncba==	3
replace cba=	2239	if bc_anio==	2015	 & bc_mes==	6	 & regioncba==	3
replace cba=	2274	if bc_anio==	2015	 & bc_mes==	7	 & regioncba==	3
replace cba=	2317	if bc_anio==	2015	 & bc_mes==	8	 & regioncba==	3
replace cba=	2336	if bc_anio==	2015	 & bc_mes==	9	 & regioncba==	3
replace cba=	2327	if bc_anio==	2015	 & bc_mes==	10	 & regioncba==	3
replace cba=	2338	if bc_anio==	2015	 & bc_mes==	11	 & regioncba==	3
replace cba=	2337	if bc_anio==	2015	 & bc_mes==	12	 & regioncba==	3
replace cba=	2353	if bc_anio==	2016	 & bc_mes==	1	 & regioncba==	3
replace cba=	2374	if bc_anio==	2016	 & bc_mes==	2	 & regioncba==	3
replace cba=	2409	if bc_anio==	2016	 & bc_mes==	3	 & regioncba==	3
replace cba=	2427	if bc_anio==	2016	 & bc_mes==	4	 & regioncba==	3
replace cba=	2497	if bc_anio==	2016	 & bc_mes==	5	 & regioncba==	3
replace cba=	2526	if bc_anio==	2016	 & bc_mes==	6	 & regioncba==	3
replace cba=	2531	if bc_anio==	2016	 & bc_mes==	7	 & regioncba==	3
replace cba=	2543	if bc_anio==	2016	 & bc_mes==	8	 & regioncba==	3
replace cba=	2541	if bc_anio==	2016	 & bc_mes==	9	 & regioncba==	3
replace cba=	2541	if bc_anio==	2016	 & bc_mes==	10	 & regioncba==	3
replace cba=	2520	if bc_anio==	2016	 & bc_mes==	11	 & regioncba==	3
replace cba=	2510	if bc_anio==	2016	 & bc_mes==	12	 & regioncba==	3
replace cba=	2512	if bc_anio==	2017	 & bc_mes==	1	 & regioncba==	3
replace cba=	2506	if bc_anio==	2017	 & bc_mes==	2	 & regioncba==	3
replace cba=	2538	if bc_anio==	2017	 & bc_mes==	3	 & regioncba==	3
replace cba=	2539	if bc_anio==	2017	 & bc_mes==	4	 & regioncba==	3
replace cba=	2539	if bc_anio==	2017	 & bc_mes==	5	 & regioncba==	3
replace cba=	2529	if bc_anio==	2017	 & bc_mes==	6	 & regioncba==	3
replace cba=	2539	if bc_anio==	2017	 & bc_mes==	7	 & regioncba==	3
replace cba=	2571	if bc_anio==	2017	 & bc_mes==	8	 & regioncba==	3
replace cba=	2604	if bc_anio==	2017	 & bc_mes==	9	 & regioncba==	3
replace cba=	2618	if bc_anio==	2017	 & bc_mes==	10	 & regioncba==	3
replace cba=	2626	if bc_anio==	2017	 & bc_mes==	11	 & regioncba==	3
replace cba=	2642	if bc_anio==	2017	 & bc_mes==	12	 & regioncba==	3
replace cba=	2689	if bc_anio==	2018	 & bc_mes==	1	 & regioncba==	3
replace cba=	2714	if bc_anio==	2018	 & bc_mes==	2	 & regioncba==	3
replace cba=	2717	if bc_anio==	2018	 & bc_mes==	3	 & regioncba==	3
replace cba=	2702	if bc_anio==	2018	 & bc_mes==	4	 & regioncba==	3
replace cba=	2720	if bc_anio==	2018	 & bc_mes==	5	 & regioncba==	3
replace cba=	2787	if bc_anio==	2018	 & bc_mes==	6	 & regioncba==	3
replace cba=	2801	if bc_anio==	2018	 & bc_mes==	7	 & regioncba==	3
replace cba=	2832	if bc_anio==	2018	 & bc_mes==	8	 & regioncba==	3
replace cba=	2837	if bc_anio==	2018	 & bc_mes==	9	 & regioncba==	3
replace cba=	2825	if bc_anio==	2018	 & bc_mes==	10	 & regioncba==	3
replace cba=	2793	if bc_anio==	2018	 & bc_mes==	11	 & regioncba==	3
replace cba=	2806	if bc_anio==	2018	 & bc_mes==	12	 & regioncba==	3
replace cba=	2847	if bc_anio==	2019	 & bc_mes==	1	 & regioncba==	3
replace cba=	2908	if bc_anio==	2019	 & bc_mes==	2	 & regioncba==	3
replace cba=	2925	if bc_anio==	2019	 & bc_mes==	3	 & regioncba==	3
replace cba=	2904	if bc_anio==	2019	 & bc_mes==	4	 & regioncba==	3
replace cba=	2924	if bc_anio==	2019	 & bc_mes==	5	 & regioncba==	3
replace cba=	2960	if bc_anio==	2019	 & bc_mes==	6	 & regioncba==	3
replace cba=	3002	if bc_anio==	2019	 & bc_mes==	7	 & regioncba==	3
replace cba=	3062	if bc_anio==	2019	 & bc_mes==	8	 & regioncba==	3
replace cba=	3082	if bc_anio==	2019	 & bc_mes==	9	 & regioncba==	3
replace cba=	3116	if bc_anio==	2019	 & bc_mes==	10	 & regioncba==	3
replace cba=	3152	if bc_anio==	2019	 & bc_mes==	11	 & regioncba==	3
replace cba=	3189	if bc_anio==	2019	 & bc_mes==	12	 & regioncba==	3


lab var cba "Canasta básica alimentaria"
drop regioncba 
   
   
   save "$bases\Fusionada_personasyhogares_`anio'.dta", replace


	}
    *
