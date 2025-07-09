### Mirador DESCA
### Derecho a la Vivienda
### Unidad de Métodos y Acceso a datos


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

library(rio)
library(survey)
library(srvyr)
library(tidyverse)
library(laeken)
library(statar)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

### Referencias generales para la base motor ###

DERECHO      <- "Vivienda"
TIPOIND      <- "Resultados"
AÑO	         <- 2024
FUENTE	     <- "Encuesta Continua de Hogares 2024, INE"
RESPONSABLE	 <- "J. Pandolfi"
JERARQUIA_CAT <- 1

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

### Carga de bases ###

base          <- rio::import("data_ech/ECH_2024.csv")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

### Generación de nuevas variables ###

#IPC

base <- base %>% dplyr::mutate(bc_ipc_tot = case_when(
    mes == 1  ~ 0.273034511,
    mes == 2  ~ 0.268922991,
    mes == 3  ~ 0.267205999,
    mes == 4  ~ 0.267151765,
    mes == 5  ~ 0.265483176,
    mes == 6  ~ 0.264430628,
    mes == 7  ~ 0.263492068,
    mes == 8  ~ 0.263211705,
    mes == 9  ~ 0.262461609,
    mes == 10 ~ 0.261481803,
    mes == 11 ~ 0.26063237,
    mes == 12 ~ 0.259696067))
  
#base <- base %>% dplyr::mutate(bc_ipc = case_when(   
#  mes == 1 ~  0.30530437,
#  mes == 2 ~  0.30024958,
#  mes == 3 ~  0.296416607,
#  mes == 4 ~  0.293521663,
#  mes == 5 ~  0.291695193,
#  mes == 6 ~  0.289981542,
#  mes == 7 ~  0.288299059,
#  mes == 8 ~  0.285976119,
#  mes == 9 ~  0.283625541,
#  mes == 10 ~ 0.281727985,
#  mes == 11 ~ 0.281058697,
#  mes == 12 ~ 0.281772319))


#base <- base %>% dplyr::mutate(ipc_ene2022 = case_when(
#  mes == 1 ~  1,
#  mes == 2 ~  1.017847463,
#  mes == 3 ~  1.032817647,
#  mes == 4 ~  1.044243359,
#  mes == 5 ~  1.04937242,
#  mes == 6 ~  1.054251282,
#  mes == 7 ~  1.060422835,
#  mes == 8 ~  1.068595972,
#  mes == 9 ~  1.077436304,
#  mes == 10 ~  1.086485134,
#  mes == 11 ~  1.088778616,
#  mes == 12 ~  1.085729808))



# Ingresos

base <- base %>% dplyr::mutate(y_pc       =  HT11 / HT19 )                      #Ingreso per-cápita
base <- base %>% dplyr::mutate(y_pc_svl   =  (HT11 - HT13) / HT19)              #Ingreso per-cápita sin valor locativo
base <- base %>% dplyr::mutate(y_pc_d     =  HT11 / HT19 / bc_ipc_tot)          #Ingreso per-cápita deflactado
base <- base %>% dplyr::mutate(y_pc_svl_d =  (HT11 - HT13) / HT19 / bc_ipc_tot) #Ingreso per-cápita sin valor locativo deflactado


# Quintil de ingresos

base_h <- base %>% distinct(ID, .keep_all = TRUE)
base_h <- base_h %>% dplyr::mutate(quintilesy = statar::xtile(y_pc, n=5, wt = W_ANO))
base_h <- base_h[,c("ID","quintilesy")]
base <- merge(base, base_h, by = "ID")


# Sexo

base <- base %>% dplyr::mutate(bc_pe2 = e26)


# Ascendencia afro

base <- base %>% dplyr::mutate(bd_e29_1 = e29_1)


# Tramo de edad

base <- base %>% dplyr::mutate(tramoed = case_when(e27 >= 0  & e27 <= 5  ~ 1,
                                                   e27 >= 6  & e27 <= 12 ~ 2,
                                                   e27 >= 13 & e27 <= 17 ~ 3,
                                                   e27 >= 18 & e27 <= 29 ~ 4,
                                                   e27 >= 30 & e27 <= 64 ~ 5,
                                                   e27 >= 65             ~ 6))

base <- base %>% dplyr::mutate(tramoed2 = case_when(e27 >= 0  & e27 <= 5  ~ 1,
                                                    e27 >= 6  & e27 <= 12 ~ 2,
                                                    e27 >= 13 & e27 <= 18 ~ 3,
                                                    e27 >= 19 & e27 <= 24 ~ 4,
                                                    e27 >= 25 & e27 <= 29 ~ 5,
                                                    e27 >= 30 & e27 <= 64 ~ 6,
                                                    e27 >= 65             ~ 7))

# Región

base <- base %>% dplyr::mutate(bd_region = case_when(REGION_4 == 1 | REGION_4 == 2  ~ 1,
                                                     REGION_4 == 3  ~ 2,
                                                     REGION_4 == 4  ~ 3))

# Montevideo-interior

base <- base %>% dplyr::mutate(region_dic = case_when(REGION_4 == 1 ~ 1,
                                                      REGION_4 != 1 ~ 2))

# Pobreza

base <- base %>% dplyr::mutate(pobre06 = case_when(pobre06 == 1 ~ 1,
                                                   pobre06 == 0 ~ 2))


# Tenencia insegura de la vivienda

base <- base %>% dplyr::mutate(bd_tenenciainsegura = ifelse(d8_1 == 3 | d8_1 == 4 | 
                                                            d8_1 == 6 | d8_1 == 8 |
                                                            d8_1 == 9, 1, 0))
       
                                 
# No acceso a agua potable

base <- base %>% dplyr::mutate(bd_d11 = case_when(d11 == 1 ~ 1,
                                                  d11 == 2 | d11 == 3 ~ 2,
                                                  d11 >= 4 & d11 <= 6 ~ 3))

base <- base %>% dplyr::mutate(bd_d12 = case_when(d12 == 1 ~ 1,
                                                  d12 == 2 | d11 == 3 ~ 2,
                                                  d12 == 4 ~ 3))

base <- base %>% dplyr::mutate(NBI_agua11 = case_when((bd_d12==2 | bd_d12==3) | bd_d11==3 ~ 1,
                                                       bd_d12==1 & (bd_d11==1 | bd_d11==2) ~ 0))
                                                 

# Ausencia de servicio higiénico de calidad

base <- base %>% dplyr::mutate(bd_d14 = d14)


base <- base %>% dplyr::mutate(bd_d15 = case_when(d15 == 0 ~ 99,
                                                  d15 == 1 ~ 1,
                                                  d15 == 2 ~ 2))

base <- base %>% dplyr::mutate(bd_d16 = case_when(d16 == 0 ~ 99,
                                                  d16 == 4 ~ 3,
                                                  d16 == 1 ~ 1,
                                                  d16 == 2 ~ 2,
                                                  d16 == 3 ~ 3))

base <- base %>% dplyr::mutate(NBI_servhigien11 = case_when(bd_d14==0 | bd_d15==2 |  bd_d16==3 ~ 1,
                                                            bd_d14>0  & bd_d15==1 & (bd_d16==1 | bd_d16==2) ~ 0))


# No acceso a energía eléctrica

base <- base %>% dplyr::mutate(bd_d18 = d18)

base <- base %>% dplyr::mutate(NBI_energiaelectr = case_when(bd_d18>2 ~ 1,
                                                            bd_d18 == 1 |  bd_d18 == 2 ~ 0))


# No acceso a artefactos básicos de confort

base <- base %>% dplyr::mutate(bd_d260 = case_when(d260 == 6 ~ 2,
                                                   d260 == 1 ~ 1,
                                                   d260 == 2 ~ 1,
                                                   d260 == 3 ~ 1,
                                                   d260 == 4 ~ 1,
                                                   d260 == 5 ~ 1,
                                                   d260 == 7 ~ 1))
base <- base %>% dplyr::mutate(bd_d21_1 = d21_3)

base <- base %>% dplyr::mutate(bd_d21_2 = case_when( d21_1 == 1 | d21_2 == 1 ~ 1,
                                                     d21_1 == 2 & d21_2 == 2 ~ 2))

base <- base %>% dplyr::mutate(NBI_artefactos  = case_when(bd_d260==2 | bd_d21_1==2 | bd_d21_2==2 ~ 1,
                                                           bd_d260==1 & bd_d21_1==1 & bd_d21_2==1 ~ 0))


# Gasto excesivo en vivienda

#1. IPC ajusta toda la canasta, no distingo por rubros
#2. Considero promedio del porcentaje del gasto en vivienda según cantidad de integrantes del hogar (para cada región) igual para todo el período
#3. Los gastos de vivienda (valor locativo, alquiler, luz, teléfono, agua)

base <- base %>% dplyr::mutate(inquilino = case_when(d8_1 == 5 ~ 1,
                                                     d8_1 != 5 ~ 0))

base <- base %>% dplyr::mutate(compra = case_when(d8_1 == 1 | d8_1 == 3 ~ 1,
                                                  d8_1 != 1 & d8_1 != 3 ~ 0))


base <- base %>% dplyr::mutate(bd_alquiler = d8_3)
base$bd_alquiler[is.na(base$bd_alquiler)] = 0

base$d8_2[is.na(base$d8_2)] = 0
base <- base %>% dplyr::mutate(bd_gastoviv = bd_alquiler + d8_2)

base <- base %>% dplyr::mutate(lp_06_ajust = case_when(region == 1 ~ lp_06*(1-0.284),
                                                       region == 2 ~ lp_06*(1-0.261),
                                                       region == 3 ~ lp_06*(1-0.119)))


base <- base %>% dplyr::mutate(ing_sinviv = HT11 - bd_gastoviv)

base <- base %>% dplyr::mutate(ing_sinviv = case_when(d8_1 == 1 | d8_1 == 3 | d8_1 == 5 ~ ing_sinviv,
                                                      d8_1 != 1 & d8_1 != 3 & d8_1 != 5 ~ NA_real_))

base <- base %>% dplyr::mutate(bd_lpvivienda = case_when(ing_sinviv <  lp_06_ajust ~ 1,
                                                         ing_sinviv >= lp_06_ajust ~ 0,
                                                         is.na(ing_sinviv) == T ~ 0))


base <- base %>% dplyr::mutate(aux1 = (HT11 - bd_gastoviv) / HT19)

base <- base %>% dplyr::mutate(bd_livivienda = case_when(aux1 <  li_06 & ing_sinviv!= NA_real_ ~ 1,
                                                         aux1 >= li_06 & ing_sinviv!= NA_real_ ~ 0,
                                                         ing_sinviv == NA_real_ ~ 0))  

base <- base %>% dplyr::mutate(aux2 = bd_gastoviv / HT11)

base <- base %>% dplyr::mutate(gto_nosoportable = case_when(aux2 >  0.25 & (inquilino==1|compra==1) ~ 1,
                                                            aux2 <= 0.25 | (inquilino!=1&compra!=1) ~ 0,
                                                            bd_gastoviv == NA_real_ ~ 0))

base <- base %>% dplyr::mutate(aux3 = bd_alquiler / HT11)

base <- base %>% dplyr::mutate(gto_nosoportable_inq = case_when(aux3 >  0.25 & inquilino==1 ~ 1,
                                                                aux3 <= 0.25 & inquilino==1 ~ 0))

base <- base %>% dplyr::mutate(aux4 = d8_2 / HT11)

base <- base %>% dplyr::mutate(gto_nosoportable_comp = case_when(aux4 >  0.25 & compra==1 ~ 1,
                                                                 aux4 <= 0.25 & compra==1 ~ 0))


base <- base %>% dplyr::mutate(bc_ht11 = HT11)

# Hacinamiento

base <- base %>% dplyr::mutate(aux = HT19/d9)
base <- base %>% dplyr::mutate(hacinamiento_bd = case_when(aux >  2 ~ 1,
                                                           aux <= 2 ~ 0))
  

# Materiales de construcción de la vivienda inadecuados


base <- base %>% dplyr::mutate(NBI_Materialidad = case_when(c2 == 6 | c3 == 6 | c4 == 5 ~ 1,
                                                            (c2 >= 1 & c2 <= 5) &
                                                            (c3 >= 1 & c3 <= 5) &
                                                            (c4 >= 1 & c4 <= 4) ~ 0))


# Asentamientos

base <- base %>% dplyr::mutate(asentamiento = case_when(d8_4 ==  2 ~ 0,
                                                        d8_4 ==  1 ~ 1))




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

### Información de muestreo ### 

base_svy <- srvyr::as_survey_design(base, ids = ID, weights = W_ANO)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
### Procesamiento de indicadores ###
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Seguridad jurídica de la tenencia
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


# Total

c_ano <- base_svy %>%
  srvyr::summarise(colname = srvyr::survey_mean(bd_tenenciainsegura, na.rm=T))

c_ano <- c_ano$colname
c_ano <- mean(c_ano)


# Ascendencia étnico racial

c_afro <- function(x) {
  x <- base_svy %>%
    filter(bd_e29_1 == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bd_tenenciainsegura, na.rm=T))
  x <- mean(x$colname)
}       

c_e_afro <- numeric()

for(i in 1:2){
  c_e_afro[i] <- c_afro(x = i)
}     



# Departamento

c_dpto <- function(x) {
  x <- base_svy %>%
    filter(dpto == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bd_tenenciainsegura, na.rm=T))
  x <- mean(x$colname)
}       

c_e_dpto <- numeric()

for(i in 1:19){
  c_e_dpto[i] <- c_dpto(x = i)
}     

# Quintil de ingresos

c_quintil <- function(x) {
  x <- base_svy %>%
    filter(quintilesy == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bd_tenenciainsegura, na.rm=T))
  x <- mean(x$colname)
}       

c_e_quintil <- numeric()

for(i in 1:5){
  c_e_quintil[i] <- c_quintil(x = i)
}     


# Sexo

c_sexo <- function(x) {
  x <- base_svy %>%
    filter(bc_pe2 == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bd_tenenciainsegura, na.rm=T))
  x <- mean(x$colname)
}       

c_e_sexo <- numeric()

for(i in 1:2){
  c_e_sexo[i] <- c_sexo(x = i)
}     

# Edad

c_edad <- function(x) {
  x <- base_svy %>%
    filter(tramoed == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bd_tenenciainsegura, na.rm=T))
  x <- mean(x$colname)
}       

c_e_edad <- numeric()

for(i in 1:6){
  c_e_edad[i] <- c_edad(x = i)
}   



CODIND       <- 230101
DIMENSIÓN    <- "Seguridad jurídica de la tenencia"
CODINDICADOR <- "Tenencia insegura de la vivienda"
NOMINDICADOR <- "Porcentaje de personas en hogares con tenencia insegura"
VALOR	       <- c(c_ano, c_e_afro, c_e_dpto, c_e_quintil, c_e_sexo, c_e_edad)*100
JERARQUIA	   <- c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
CORTE        <- c("Total", "Ascendencia étnico-racial", "Ascendencia étnico-racial", "Departamento", "Departamento", "Departamento", "Departamento",
                  "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento",
                  "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Quintil de ingresos",
                  "Quintil de ingresos", "Quintil de ingresos", "Quintil de ingresos", "Quintil de ingresos", "Sexo", "Sexo", "Edad", "Edad", "Edad", "Edad", "Edad", "Edad")

DEPARTAMENTO <- c("","","", "Montevideo", "Artigas", "Canelones", "Cerro Largo", "Colonia", "Durazno", "Flores", "Florida", "Lavalleja", "Maldonado",
                  "Paysandú", "Río Negro", "Rivera", "Rocha", "Salto", "San José", "Soriano", "Tacuarembó", "Treinta y Tres", "", "", "", "", "", "", "", "", "", "", "", "","")

QUINTIL	      <- c("","","","","","","","","","","","","","","","","","","","","","","Quintil 1", "Quintil 2", "Quintil 3", "Quintil 4", "Quintil 5","","", "", "", "", "", "","")

SEXO		      <- c("","","","","","","","","","","","","","","","","","","","","","","", "", "", "", "","Varones","Mujeres", "", "", "", "", "","")

ASCENDENCIA   <- c("","Afrodescendiente","No afrodescendiente","","","","","","","","","","","","","","","","","","","","", "", "", "", "","","", "", "", "", "", "","") 

EDAD		      <- c("","","","","","","","","","","","","","","","","","","","","","","", "", "", "", "","","", "0 a 5 años", "6 a 12 años", "13 a 17 años", "18 a 29 años", "30 a 64 años", "65 años o más")

POBREZA       <- ""

URBANORURALUY	<- ""

REGIÓN        <- ""


NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. Las estimaciones desde 2022 se calculan a partir de la muestra de implantación."

v_01 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, 
              JERARQUIA_CAT, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA, EDAD, POBREZA, URBANORURALUY, REGIÓN, NOTA_INDICADOR)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# No acceso a agua potable
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


# Total

c_ano <- base_svy %>%
  srvyr::summarise(colname = srvyr::survey_mean(NBI_agua11, na.rm=T))

c_ano <- c_ano$colname
c_ano <- mean(c_ano)


# Ascendencia étnico racial

c_afro <- function(x) {
  x <- base_svy %>%
    filter(bd_e29_1 == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(NBI_agua11, na.rm=T))
  x <- mean(x$colname)
}       

c_e_afro <- numeric()

for(i in 1:2){
  c_e_afro[i] <- c_afro(x = i)
}     

# Departamento

c_dpto <- function(x) {
  x <- base_svy %>%
    filter(dpto == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bd_tenenciainsegura, na.rm=T))
  x <- mean(x$colname)
}       

c_e_dpto <- numeric()

for(i in 1:19){
  c_e_dpto[i] <- c_dpto(x = i)
}     

# Quintil de ingresos

c_quintil <- function(x) {
  x <- base_svy %>%
    filter(quintilesy == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(NBI_agua11, na.rm=T))
  x <- mean(x$colname)
}       

c_e_quintil <- numeric()

for(i in 1:5){
  c_e_quintil[i] <- c_quintil(x = i)
}     


# Sexo

c_sexo <- function(x) {
  x <- base_svy %>%
    filter(bc_pe2 == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(NBI_agua11, na.rm=T))
  x <- mean(x$colname)
}       

c_e_sexo <- numeric()

for(i in 1:2){
  c_e_sexo[i] <- c_sexo(x = i)
}     

# Edad

c_edad <- function(x) {
  x <- base_svy %>%
    filter(tramoed2 == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(NBI_agua11, na.rm=T))
  x <- mean(x$colname)
}       

c_e_edad <- numeric()

for(i in 1:7){
  c_e_edad[i] <- c_edad(x = i)
}   


# Pobreza

c_pobre <- function(x) {
  x <- base_svy %>%
    filter(pobre06 == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(NBI_agua11, na.rm=T))
  x <- mean(x$colname)
}       

c_e_pobre <- numeric()

for(i in 1:2){
  c_e_pobre[i] <- c_pobre(x = i)
}     


# Región

c_region <- function(x) {
  x <- base_svy %>%
    filter(bd_region == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(NBI_agua11, na.rm=T))
  x <- mean(x$colname)
}       

c_e_region <- numeric()

for(i in 1:3){
  c_e_region[i] <- c_region(x = i)
} 


CODIND       <- 230201
DIMENSIÓN    <- "Acceso a servicios e infraestructura"
CODINDICADOR <- "No acceso a agua potable"
NOMINDICADOR <- "Porcentaje de personas que residen en viviendas sin agua potable"
VALOR	       <- c(c_ano, c_e_afro, c_e_dpto, c_e_quintil, c_e_sexo, c_e_edad, c_e_pobre, c_e_region)*100
JERARQUIA	   <- c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0, 0, 0, 0, 0, 0, 0)
CORTE        <- c("Total", "Ascendencia étnico-racial", "Ascendencia étnico-racial", "Departamento", "Departamento", "Departamento", "Departamento",
                  "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento",
                  "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Quintil de ingresos",
                  "Quintil de ingresos", "Quintil de ingresos", "Quintil de ingresos", "Quintil de ingresos", "Sexo", "Sexo", "Edad", "Edad","Edad", "Edad", "Edad", "Edad", "Edad", "Pobreza", "Pobreza", 
                  "Urbano rural", "Urbano rural", "Urbano rural")
DEPARTAMENTO <- c("","","", "Montevideo", "Artigas", "Canelones", "Cerro Largo", "Colonia", "Durazno", "Flores", "Florida", "Lavalleja", "Maldonado",
                  "Paysandú", "Río Negro", "Rivera", "Rocha", "Salto", "San José", "Soriano", "Tacuarembó", "Treinta y Tres","", "", "", "", "", "", "", "", "", "", "", "", "","", "", "", "", "", "")

QUINTIL	      <- c("","","","","","","","","","","","","","","","","","","","","","","Quintil 1", "Quintil 2", "Quintil 3", "Quintil 4", "Quintil 5","","","", "", "", "", "", "","", "", "", "", "", "")

SEXO		      <- c("","","","","","","","","","","","","","","","","","","","","","","", "", "", "", "","Varones","Mujeres", "", "", "", "","","","", "", "", "", "", "")

ASCENDENCIA   <- c("","Afrodescendiente","No afrodescendiente","","","","","","","","","","","","","","","","","","","","", "", "", "", "","","", "","", "", "", "", "","", "", "", "", "", "") 

EDAD		      <- c("","","","","","","","","","","","","","","","","","","","","","","", "", "", "", "","","", "0 a 5 años", "6 a 12 años", "13 a 18 años", "19 a 24 años", "25 a 29 años", "30 a 64 años", "65 años o más", "", "", "", "", "")

POBREZA       <- c("","","","","","","","","","","","","","","","","","","","","","","", "", "", "", "","","","", "", "", "", "", "", "", "Pobres", "No pobres", "", "", "")

URBANORURALUY	<- c("","","","","","","","","","","","","","","","","","","","","","","", "", "", "", "","","", "", "","", "", "", "", "", "", "", "Urbano (más de 5.000 habitantes)", "Urbano (menos de 5.000 habitantes)", "Rural disperso")

REGIÓN        <- ""


NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. Las estimaciones desde 2022 se calculan a partir de la muestra de implantación."

v_02 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, 
              JERARQUIA_CAT, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA, EDAD, POBREZA, URBANORURALUY, REGIÓN, NOTA_INDICADOR)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de personas que residen en viviendas sin servicio higiénico de calidad
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


# Total

c_ano <- base_svy %>%
  srvyr::summarise(colname = srvyr::survey_mean(NBI_servhigien11, na.rm=T))

c_ano <- c_ano$colname
c_ano <- mean(c_ano)


# Ascendencia étnico racial

c_afro <- function(x) {
  x <- base_svy %>%
    filter(bd_e29_1 == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(NBI_servhigien11, na.rm=T))
  x <- mean(x$colname)
}       

c_e_afro <- numeric()

for(i in 1:2){
  c_e_afro[i] <- c_afro(x = i)
}     



# Departamento

c_dpto <- function(x) {
  x <- base_svy %>%
    filter(dpto == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(NBI_servhigien11, na.rm=T))
  x <- mean(x$colname)
}       

c_e_dpto <- numeric()

for(i in 1:19){
  c_e_dpto[i] <- c_dpto(x = i)
}     

# Quintil de ingresos

c_quintil <- function(x) {
  x <- base_svy %>%
    filter(quintilesy == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(NBI_servhigien11, na.rm=T))
  x <- mean(x$colname)
}       

c_e_quintil <- numeric()

for(i in 1:5){
  c_e_quintil[i] <- c_quintil(x = i)
}     


# Sexo

c_sexo <- function(x) {
  x <- base_svy %>%
    filter(bc_pe2 == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(NBI_servhigien11, na.rm=T))
  x <- mean(x$colname)
}       

c_e_sexo <- numeric()

for(i in 1:2){
  c_e_sexo[i] <- c_sexo(x = i)
}     

# Edad

c_edad <- function(x) {
  x <- base_svy %>%
    filter(tramoed2 == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(NBI_servhigien11, na.rm=T))
  x <- mean(x$colname)
}       

c_e_edad <- numeric()

for(i in 1:7){
  c_e_edad[i] <- c_edad(x = i)
} 

# Pobreza

c_pobre <- function(x) {
  x <- base_svy %>%
    filter(pobre06 == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(NBI_servhigien11, na.rm=T))
  x <- mean(x$colname)
}       

c_e_pobre <- numeric()

for(i in 1:2){
  c_e_pobre[i] <- c_pobre(x = i)
}     


# Región

c_region <- function(x) {
  x <- base_svy %>%
    filter(bd_region == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(NBI_servhigien11, na.rm=T))
  x <- mean(x$colname)
}       

c_e_region <- numeric()

for(i in 1:3){
  c_e_region[i] <- c_region(x = i)
} 


CODIND       <- 230202
DIMENSIÓN    <- "Acceso a servicios e infraestructura"
CODINDICADOR <- "Ausencia de servicio higiénico de calidad"
NOMINDICADOR <- "Porcentaje de personas que residen en viviendas sin servicio higiénico de calidad"
VALOR	       <- c(c_ano, c_e_afro, c_e_dpto, c_e_quintil, c_e_sexo, c_e_edad, c_e_pobre, c_e_region)*100
JERARQUIA	   <- c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0, 0, 0, 0, 0, 0,0)
CORTE        <- c("Total", "Ascendencia étnico-racial", "Ascendencia étnico-racial", "Departamento", "Departamento", "Departamento", "Departamento",
                  "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento",
                  "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Quintil de ingresos",
                  "Quintil de ingresos", "Quintil de ingresos", "Quintil de ingresos", "Quintil de ingresos", "Sexo", "Sexo", "Edad","Edad","Edad", "Edad", "Edad", "Edad", "Edad", "Pobreza", "Pobreza", 
                  "Urbano rural", "Urbano rural", "Urbano rural")
DEPARTAMENTO <- c("","","", "Montevideo", "Artigas", "Canelones", "Cerro Largo", "Colonia", "Durazno", "Flores", "Florida", "Lavalleja", "Maldonado",
                  "Paysandú", "Río Negro", "Rivera", "Rocha", "Salto", "San José", "Soriano", "Tacuarembó", "Treinta y Tres", "", "", "", "", "", "", "", "", "", "", "", "", "","", "", "", "", "", "")

QUINTIL	      <- c("","","","","","","","","","","","","","","","","","","","","","","Quintil 1", "Quintil 2", "Quintil 3", "Quintil 4", "Quintil 5", "","","", "", "", "", "", "","", "", "", "", "", "")

SEXO		      <- c("","","","","","","","","","","","","","","","","","","","","","","", "", "", "", "","Varones","Mujeres", "", "", "", "", "", "","", "", "", "", "", "")

ASCENDENCIA   <- c("","Afrodescendiente","No afrodescendiente","","","","","","","","","","","","","","","","","","","","", "", "", "", "", "","","", "", "", "", "", "","", "", "", "", "", "") 

EDAD		      <- c("","","","","","","","","","","","","","","","","","","","","","","", "", "", "", "","","", "0 a 5 años", "6 a 12 años", "13 a 18 años", "19 a 24 años", "25 a 29 años", "30 a 64 años", "65 años o más", "", "", "", "", "")

POBREZA       <- c("","","","","","","","","","","","","","","","","","","","","","","", "", "", "", "","","","", "", "", "", "", "", "", "Pobres", "No pobres", "", "", "")

URBANORURALUY	<- c("","","","","","","","","","","","","","","","","","","","","","","", "", "", "", "","","", "", "","", "", "", "", "", "", "", "Urbano (más de 5.000 habitantes)", "Urbano (menos de 5.000 habitantes)", "Rural disperso")

REGIÓN        <- ""


NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. La estimaciones desde 2022 se calculan a partir de la muestra de implantación."

v_03 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, 
              JERARQUIA_CAT, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA, EDAD, POBREZA, URBANORURALUY, REGIÓN, NOTA_INDICADOR)




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# No acceso a energía eléctrica
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


# Total

c_ano <- base_svy %>%
  srvyr::summarise(colname = srvyr::survey_mean(NBI_energiaelectr, na.rm=T))

c_ano <- c_ano$colname
c_ano <- mean(c_ano)


# Ascendencia étnico racial

c_afro <- function(x) {
  x <- base_svy %>%
    filter(bd_e29_1 == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(NBI_energiaelectr, na.rm=T))
  x <- mean(x$colname)
}       

c_e_afro <- numeric()

for(i in 1:2){
  c_e_afro[i] <- c_afro(x = i)
}     



# Departamento

c_dpto <- function(x) {
  x <- base_svy %>%
    filter(dpto == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(NBI_energiaelectr, na.rm=T))
  x <- mean(x$colname)
}       

c_e_dpto <- numeric()

for(i in 1:19){
  c_e_dpto[i] <- c_dpto(x = i)
}     

# Quintil de ingresos

c_quintil <- function(x) {
  x <- base_svy %>%
    filter(quintilesy == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(NBI_energiaelectr, na.rm=T))
  x <- mean(x$colname)
}       

c_e_quintil <- numeric()

for(i in 1:5){
  c_e_quintil[i] <- c_quintil(x = i)
}     


# Sexo

c_sexo <- function(x) {
  x <- base_svy %>%
    filter(bc_pe2 == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(NBI_energiaelectr, na.rm=T))
  x <- mean(x$colname)
}       

c_e_sexo <- numeric()

for(i in 1:2){
  c_e_sexo[i] <- c_sexo(x = i)
}     



CODIND       <- 230203
DIMENSIÓN    <- "Acceso a servicios e infraestructura"
CODINDICADOR <- "No acceso a energía eléctrica"
NOMINDICADOR <- "Porcentaje de personas que residen en viviendas sin acceso a la energía eléctrica"
VALOR	       <- c(c_ano, c_e_afro, c_e_dpto, c_e_quintil, c_e_sexo)*100
JERARQUIA	   <- c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
CORTE        <- c("Total", "Ascendencia étnico-racial", "Ascendencia étnico-racial", "Departamento", "Departamento", "Departamento", "Departamento",
                  "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento",
                  "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Quintil de ingresos",
                  "Quintil de ingresos", "Quintil de ingresos", "Quintil de ingresos", "Quintil de ingresos", "Sexo", "Sexo")
DEPARTAMENTO <- c("","","", "Montevideo", "Artigas", "Canelones", "Cerro Largo", "Colonia", "Durazno", "Flores", "Florida", "Lavalleja", "Maldonado",
                  "Paysandú", "Río Negro", "Rivera", "Rocha", "Salto", "San José", "Soriano", "Tacuarembó", "Treinta y Tres", "", "", "", "", "", "", "")

QUINTIL	      <- c("","","","","","","","","","","","","","","","","","","","","","","Quintil 1", "Quintil 2", "Quintil 3", "Quintil 4", "Quintil 5","","")

SEXO		      <- c("","","","","","","","","","","","","","","","","","","","","","","", "", "", "", "","Varones","Mujeres")

ASCENDENCIA   <- c("","Afrodescendiente","No afrodescendiente","","","","","","","","","","","","","","","","","","","","", "", "", "", "","","") 

EDAD		      <- ""

POBREZA       <- ""

URBANORURALUY	<- ""

REGIÓN        <- ""


NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. La estimaciones desde 2022 se calculan a partir de la muestra de implantación."

v_04 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, 
              JERARQUIA_CAT, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA, EDAD, POBREZA, URBANORURALUY, REGIÓN, NOTA_INDICADOR)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# No acceso a artefactos básicos de confort
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


# Total

c_ano <- base_svy %>%
  srvyr::summarise(colname = srvyr::survey_mean(NBI_artefactos, na.rm=T))

c_ano <- c_ano$colname
c_ano <- mean(c_ano)


# Ascendencia étnico racial

c_afro <- function(x) {
  x <- base_svy %>%
    filter(bd_e29_1 == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(NBI_artefactos, na.rm=T))
  x <- mean(x$colname)
}       

c_e_afro <- numeric()

for(i in 1:2){
  c_e_afro[i] <- c_afro(x = i)
}     



# Departamento

c_dpto <- function(x) {
  x <- base_svy %>%
    filter(dpto == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(NBI_artefactos, na.rm=T))
  x <- mean(x$colname)
}       

c_e_dpto <- numeric()

for(i in 1:19){
  c_e_dpto[i] <- c_dpto(x = i)
}     

# Quintil de ingresos

c_quintil <- function(x) {
  x <- base_svy %>%
    filter(quintilesy == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(NBI_artefactos, na.rm=T))
  x <- mean(x$colname)
}       

c_e_quintil <- numeric()

for(i in 1:5){
  c_e_quintil[i] <- c_quintil(x = i)
}     


# Sexo

c_sexo <- function(x) {
  x <- base_svy %>%
    filter(bc_pe2 == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(NBI_artefactos, na.rm=T))
  x <- mean(x$colname)
}       

c_e_sexo <- numeric()

for(i in 1:2){
  c_e_sexo[i] <- c_sexo(x = i)
}     

# Edad

c_edad <- function(x) {
  x <- base_svy %>%
    filter(tramoed2 == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(NBI_artefactos, na.rm=T))
  x <- mean(x$colname)
}       

c_e_edad <- numeric()

for(i in 1:7){
  c_e_edad[i] <- c_edad(x = i)
} 

# Pobreza

c_pobre <- function(x) {
  x <- base_svy %>%
    filter(pobre06 == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(NBI_artefactos, na.rm=T))
  x <- mean(x$colname)
}       

c_e_pobre <- numeric()

for(i in 1:2){
  c_e_pobre[i] <- c_pobre(x = i)
}     


# Región

c_region <- function(x) {
  x <- base_svy %>%
    filter(bd_region == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(NBI_artefactos, na.rm=T))
  x <- mean(x$colname)
}       

c_e_region <- numeric()

for(i in 1:3){
  c_e_region[i] <- c_region(x = i)
} 

CODIND       <- 230204
DIMENSIÓN    <- "Acceso a servicios e infraestructura"
CODINDICADOR <- "No acceso a artefactos básicos de confort"
NOMINDICADOR <- "Porcentaje de personas que residen en viviendas sin artefactos básicos de confort"
VALOR	       <- c(c_ano, c_e_afro, c_e_dpto, c_e_quintil, c_e_sexo, c_e_edad, c_e_pobre, c_e_region)*100
JERARQUIA	   <- c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0, 0, 0, 0, 0, 0,0)
CORTE        <- c("Total", "Ascendencia étnico-racial", "Ascendencia étnico-racial", "Departamento", "Departamento", "Departamento", "Departamento",
                  "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento",
                  "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Quintil de ingresos",
                  "Quintil de ingresos", "Quintil de ingresos", "Quintil de ingresos", "Quintil de ingresos", "Sexo", "Sexo", "Edad","Edad","Edad", "Edad", "Edad", "Edad", "Edad", "Pobreza", "Pobreza", 
                  "Urbano rural", "Urbano rural", "Urbano rural")
DEPARTAMENTO <- c("","","", "Montevideo", "Artigas", "Canelones", "Cerro Largo", "Colonia", "Durazno", "Flores", "Florida", "Lavalleja", "Maldonado",
                  "Paysandú", "Río Negro", "Rivera", "Rocha", "Salto", "San José", "Soriano", "Tacuarembó", "Treinta y Tres","", "", "", "", "", "", "", "", "", "", "", "", "","", "", "", "", "", "")

QUINTIL	      <- c("","","","","","","","","","","","","","","","","","","","","","","Quintil 1", "Quintil 2", "Quintil 3", "Quintil 4", "Quintil 5","","","", "", "", "", "", "","", "", "", "", "", "")

SEXO		      <- c("","","","","","","","","","","","","","","","","","","","","","","", "", "", "", "","Varones","Mujeres", "", "", "", "","", "","", "", "", "", "", "")

ASCENDENCIA   <- c("","Afrodescendiente","No afrodescendiente","","","","","","","","","","","","","","","","","","","","", "", "", "","", "","","", "", "", "", "", "","", "", "", "", "", "") 

EDAD		      <- c("","","","","","","","","","","","","","","","","","","","","","","", "", "", "", "","","", "0 a 5 años", "6 a 12 años", "13 a 18 años", "19 a 24 años", "25 a 29 años", "30 a 64 años", "65 años o más", "", "", "", "", "")

POBREZA       <- c("","","","","","","","","","","","","","","","","","","","","","","", "", "", "", "","","","", "", "", "", "","", "", "Pobres", "No pobres", "", "", "")

URBANORURALUY	<- c("","","","","","","","","","","","","","","","","","","","","","","", "", "", "", "","","", "", "","", "", "","", "", "", "", "Urbano (más de 5.000 habitantes)", "Urbano (menos de 5.000 habitantes)", "Rural disperso")

REGIÓN        <- ""


NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. La estimaciones desde 2022 se calculan a partir de la muestra de implantación."

v_05 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, 
              JERARQUIA_CAT, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA, EDAD, POBREZA, URBANORURALUY, REGIÓN, NOTA_INDICADOR)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Gasto excesivo en vivienda en relación a la línea de pobreza
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


# Total

c_ano <- base_svy %>%
  srvyr::summarise(colname = srvyr::survey_mean(bd_lpvivienda, na.rm=T))

c_ano <- c_ano$colname
c_ano <- mean(c_ano)


# Ascendencia étnico racial

c_afro <- function(x) {
  x <- base_svy %>%
    filter(bd_e29_1 == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bd_lpvivienda, na.rm=T))
  x <- mean(x$colname)
}       

c_e_afro <- numeric()

for(i in 1:2){
  c_e_afro[i] <- c_afro(x = i)
}     



# Departamento

c_dpto <- function(x) {
  x <- base_svy %>%
    filter(dpto == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bd_lpvivienda, na.rm=T))
  x <- mean(x$colname)
}       

c_e_dpto <- numeric()

for(i in 1:19){
  c_e_dpto[i] <- c_dpto(x = i)
}     

# Quintil de ingresos

c_quintil <- function(x) {
  x <- base_svy %>%
    filter(quintilesy == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bd_lpvivienda, na.rm=T))
  x <- mean(x$colname)
}       

c_e_quintil <- numeric()

for(i in 1:5){
  c_e_quintil[i] <- c_quintil(x = i)
}     


# Sexo

c_sexo <- function(x) {
  x <- base_svy %>%
    filter(bc_pe2 == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bd_lpvivienda, na.rm=T))
  x <- mean(x$colname)
}       

c_e_sexo <- numeric()

for(i in 1:2){
  c_e_sexo[i] <- c_sexo(x = i)
}     


# Edad

c_edad <- function(x) {
  x <- base_svy %>%
    filter(tramoed == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bd_lpvivienda, na.rm=T))
  x <- mean(x$colname)
}       

c_e_edad <- numeric()

for(i in 1:6){
  c_e_edad[i] <- c_edad(x = i)
} 

CODIND       <- 230301
DIMENSIÓN    <- "Gastos soportables"
CODINDICADOR <- "Gasto excesivo en vivienda en relación a la línea de pobreza"
NOMINDICADOR <- "Porcentaje de personas que residen en hogares con ingreso bajo la línea de pobreza luego de pagar los gastos de vivienda (alquiler o cuota de compra)"
VALOR	       <- c(c_ano, c_e_afro, c_e_dpto, c_e_quintil, c_e_sexo, c_e_edad)*100
JERARQUIA	   <- c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
CORTE        <- c("Total", "Ascendencia étnico-racial", "Ascendencia étnico-racial", "Departamento", "Departamento", "Departamento", "Departamento",
                  "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento",
                  "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Quintil de ingresos",
                  "Quintil de ingresos", "Quintil de ingresos", "Quintil de ingresos", "Quintil de ingresos", "Sexo", "Sexo", "Edad", "Edad", "Edad", "Edad", "Edad", "Edad")
DEPARTAMENTO <- c("","","", "Montevideo", "Artigas", "Canelones", "Cerro Largo", "Colonia", "Durazno", "Flores", "Florida", "Lavalleja", "Maldonado",
                  "Paysandú", "Río Negro", "Rivera", "Rocha", "Salto", "San José", "Soriano", "Tacuarembó", "Treinta y Tres", "", "", "", "", "", "", "", "", "", "", "","","")

QUINTIL	      <- c("","","","","","","","","","","","","","","","","","","","","","","Quintil 1", "Quintil 2", "Quintil 3", "Quintil 4", "Quintil 5","","", "", "", "", "","","")

SEXO		      <- c("","","","","","","","","","","","","","","","","","","","","","","", "", "", "", "","Varones","Mujeres", "", "", "", "","","")

ASCENDENCIA   <- c("","Afrodescendiente","No afrodescendiente","","","","","","","","","","","","","","","","","","","","", "", "", "", "","","", "", "", "", "","","") 

EDAD		      <- c("","","","","","","","","","","","","","","","","","","","","","","", "", "", "", "","","", "0 a 5 años", "6 a 12 años", "13 a 17 años", "18 a 29 años", "30 a 64 años", "65 años o más")

POBREZA       <- ""

URBANORURALUY	<- ""

REGIÓN        <- ""

NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. La estimaciones desde 2022 se calculan a partir de la muestra de implantación."

v_06 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, 
              JERARQUIA_CAT, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA, EDAD, POBREZA, URBANORURALUY, REGIÓN, NOTA_INDICADOR)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Gasto excesivo en vivienda
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


# Total

c_ano <- base_svy %>%
  srvyr::summarise(colname = srvyr::survey_mean(gto_nosoportable, na.rm=T))

c_ano <- c_ano$colname
c_ano <- mean(c_ano)


# Ascendencia étnico racial

c_afro <- function(x) {
  x <- base_svy %>%
    filter(bd_e29_1 == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(gto_nosoportable, na.rm=T))
  x <- mean(x$colname)
}       

c_e_afro <- numeric()

for(i in 1:2){
  c_e_afro[i] <- c_afro(x = i)
}     



# Departamento

c_dpto <- function(x) {
  x <- base_svy %>%
    filter(dpto == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(gto_nosoportable, na.rm=T))
  x <- mean(x$colname)
}       

c_e_dpto <- numeric()

for(i in 1:19){
  c_e_dpto[i] <- c_dpto(x = i)
}     

# Quintil de ingresos

c_quintil <- function(x) {
  x <- base_svy %>%
    filter(quintilesy == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(gto_nosoportable, na.rm=T))
  x <- mean(x$colname)
}       

c_e_quintil <- numeric()

for(i in 1:5){
  c_e_quintil[i] <- c_quintil(x = i)
}     


# Sexo

c_sexo <- function(x) {
  x <- base_svy %>%
    filter(bc_pe2 == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(gto_nosoportable, na.rm=T))
  x <- mean(x$colname)
}       

c_e_sexo <- numeric()

for(i in 1:2){
  c_e_sexo[i] <- c_sexo(x = i)
}     

# Edad

c_edad <- function(x) {
  x <- base_svy %>%
    filter(tramoed == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(gto_nosoportable, na.rm=T))
  x <- mean(x$colname)
}       

c_e_edad <- numeric()

for(i in 1:6){
  c_e_edad[i] <- c_edad(x = i)
} 



# Montevideo - Interior

c_regiondic <- function(x) {
  x <- base_svy %>%
    filter(region_dic == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(gto_nosoportable, na.rm=T))
  x <- mean(x$colname)
}       

c_e_regiondic <- numeric()

for(i in 1:2){
  c_e_regiondic[i] <- c_regiondic(x = i)
}

CODIND       <- 230302
DIMENSIÓN    <- "Gastos soportables"
CODINDICADOR <- "Gasto excesivo en vivienda"
NOMINDICADOR <- "Porcentaje de personas en cuyos hogares se destina más del 25% de los ingresos a la cuota de compra o alquiler de la vivienda"
VALOR	       <- c(c_ano, c_e_afro, c_e_dpto, c_e_quintil, c_e_sexo, c_e_edad, c_e_regiondic)*100
JERARQUIA	   <- c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
CORTE        <- c("Total", "Ascendencia étnico-racial", "Ascendencia étnico-racial", "Departamento", "Departamento", "Departamento", "Departamento",
                  "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento",
                  "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Quintil de ingresos",
                  "Quintil de ingresos", "Quintil de ingresos", "Quintil de ingresos", "Quintil de ingresos", "Sexo", "Sexo", "Edad", "Edad", "Edad", "Edad", "Edad", "Edad", "Región", "Región")
DEPARTAMENTO <- c("","","", "Montevideo", "Artigas", "Canelones", "Cerro Largo", "Colonia", "Durazno", "Flores", "Florida", "Lavalleja", "Maldonado",
                  "Paysandú", "Río Negro", "Rivera", "Rocha", "Salto", "San José", "Soriano", "Tacuarembó", "Treinta y Tres", "", "", "", "", "", "", "", "", "", "", "","","","","")

QUINTIL	      <- c("","","","","","","","","","","","","","","","","","","","","","","Quintil 1", "Quintil 2", "Quintil 3", "Quintil 4", "Quintil 5","","", "", "", "", "","","","","")

SEXO		      <- c("","","","","","","","","","","","","","","","","","","","","","","", "", "", "", "","Varones","Mujeres", "", "", "", "","","","","")

ASCENDENCIA   <- c("","Afrodescendiente","No afrodescendiente","","","","","","","","","","","","","","","","","","","","", "", "", "", "","","", "", "", "", "","","","","") 

EDAD		      <- c("","","","","","","","","","","","","","","","","","","","","","","", "", "", "", "","","", "0 a 5 años", "6 a 12 años", "13 a 17 años", "18 a 29 años", "30 a 64 años", "65 años o más","","")

POBREZA       <- ""

URBANORURALUY <- ""

REGIÓN	<- c("","","","","","","","","","","","","","","","","","","","","","","", "", "", "", "","","", "", "", "", "","","","Montevideo","Interior")



NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. La estimaciones desde 2022 se calculan a partir de la muestra de implantación."

v_07 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, 
              JERARQUIA_CAT, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA, EDAD, POBREZA, URBANORURALUY, REGIÓN, NOTA_INDICADOR)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Gasto excesivo en alquiler de vivienda
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


# Total

c_ano <- base_svy %>%
  filter(inquilino == 1) %>%
  srvyr::summarise(colname = srvyr::survey_mean(gto_nosoportable_inq, na.rm=T))

c_ano <- c_ano$colname
c_ano <- mean(c_ano)


# Ascendencia étnico racial

c_afro <- function(x) {
  x <- base_svy %>%
    filter(bd_e29_1 == x & inquilino == 1) %>%
    srvyr::summarise(colname = srvyr::survey_mean(gto_nosoportable_inq, na.rm=T))
  x <- mean(x$colname)
}       

c_e_afro <- numeric()

for(i in 1:2){
  c_e_afro[i] <- c_afro(x = i)
}     



# Departamento

c_dpto <- function(x) {
  x <- base_svy %>%
    filter(dpto == x & inquilino == 1) %>%
    srvyr::summarise(colname = srvyr::survey_mean(gto_nosoportable_inq, na.rm=T))
  x <- mean(x$colname)
}       

c_e_dpto <- numeric()

for(i in 1:19){
  c_e_dpto[i] <- c_dpto(x = i)
}     

# Quintil de ingresos

c_quintil <- function(x) {
  x <- base_svy %>%
    filter(quintilesy == x & inquilino == 1) %>%
    srvyr::summarise(colname = srvyr::survey_mean(gto_nosoportable_inq, na.rm=T))
  x <- mean(x$colname)
}       

c_e_quintil <- numeric()

for(i in 1:5){
  c_e_quintil[i] <- c_quintil(x = i)
}     


# Sexo

c_sexo <- function(x) {
  x <- base_svy %>%
    filter(bc_pe2 == x & inquilino == 1) %>%
    srvyr::summarise(colname = srvyr::survey_mean(gto_nosoportable_inq, na.rm=T))
  x <- mean(x$colname)
}       

c_e_sexo <- numeric()

for(i in 1:2){
  c_e_sexo[i] <- c_sexo(x = i)
}     

# Edad

c_edad <- function(x) {
  x <- base_svy %>%
    filter(tramoed == x & inquilino == 1) %>%
    srvyr::summarise(colname = srvyr::survey_mean(gto_nosoportable_inq, na.rm=T))
  x <- mean(x$colname)
}       

c_e_edad <- numeric()

for(i in 1:6){
  c_e_edad[i] <- c_edad(x = i)
} 



# Montevideo - Interior

c_regiondic <- function(x) {
  x <- base_svy %>%
    filter(region_dic == x & inquilino == 1) %>%
    srvyr::summarise(colname = srvyr::survey_mean(gto_nosoportable_inq, na.rm=T))
  x <- mean(x$colname)
}       

c_e_regiondic <- numeric()

for(i in 1:2){
  c_e_regiondic[i] <- c_regiondic(x = i)
}


CODIND       <- 230303
DIMENSIÓN    <- "Gastos soportables"
CODINDICADOR <- "Gasto excesivo en alquiler de vivienda"
NOMINDICADOR <- "Porcentaje de personas que residen en hogares inquilinos que destinan más del 25% de sus ingresos al pago de alquiler de la vivienda"
VALOR	       <- c(c_ano, c_e_afro, c_e_dpto, c_e_quintil, c_e_sexo, c_e_edad, c_e_regiondic)*100
JERARQUIA	   <- c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
CORTE        <- c("Total", "Ascendencia étnico-racial", "Ascendencia étnico-racial", "Departamento", "Departamento", "Departamento", "Departamento",
                  "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento",
                  "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Quintil de ingresos",
                  "Quintil de ingresos", "Quintil de ingresos", "Quintil de ingresos", "Quintil de ingresos", "Sexo", "Sexo", "Edad", "Edad", "Edad", "Edad", "Edad", "Edad", "Región", "Región")
DEPARTAMENTO <- c("","","", "Montevideo", "Artigas", "Canelones", "Cerro Largo", "Colonia", "Durazno", "Flores", "Florida", "Lavalleja", "Maldonado",
                  "Paysandú", "Río Negro", "Rivera", "Rocha", "Salto", "San José", "Soriano", "Tacuarembó", "Treinta y Tres", "", "", "", "", "", "", "", "", "", "", "","","","","")

QUINTIL	      <- c("","","","","","","","","","","","","","","","","","","","","","","Quintil 1", "Quintil 2", "Quintil 3", "Quintil 4", "Quintil 5","","", "", "", "", "","","","","")

SEXO		      <- c("","","","","","","","","","","","","","","","","","","","","","","", "", "", "", "","Varones","Mujeres", "", "", "", "","","","","")

ASCENDENCIA   <- c("","Afrodescendiente","No afrodescendiente","","","","","","","","","","","","","","","","","","","","", "", "", "", "","","", "", "", "", "","","","","") 

EDAD		      <- c("","","","","","","","","","","","","","","","","","","","","","","", "", "", "", "","","", "0 a 5 años", "6 a 12 años", "13 a 17 años", "18 a 29 años", "30 a 64 años", "65 años o más","","")

POBREZA       <- ""

URBANORURALUY <- ""

REGIÓN	<- c("","","","","","","","","","","","","","","","","","","","","","","", "", "", "", "","","", "", "", "", "","","","Montevideo","Interior")



NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. La estimaciones desde 2022 se calculan a partir de la muestra de implantación."

v_08 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, 
              JERARQUIA_CAT, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA, EDAD, POBREZA, URBANORURALUY, REGIÓN, NOTA_INDICADOR)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Gasto excesivo en compra de vivienda
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


# Total

c_ano <- base_svy %>%
  filter(compra == 1) %>%
  srvyr::summarise(colname = srvyr::survey_mean(gto_nosoportable_comp, na.rm=T))

c_ano <- c_ano$colname
c_ano <- mean(c_ano)


# Ascendencia étnico racial

c_afro <- function(x) {
  x <- base_svy %>%
    filter(bd_e29_1 == x & compra == 1) %>%
    srvyr::summarise(colname = srvyr::survey_mean(gto_nosoportable_comp, na.rm=T))
  x <- mean(x$colname)
}       

c_e_afro <- numeric()

for(i in 1:2){
  c_e_afro[i] <- c_afro(x = i)
}     



# Departamento

c_dpto <- function(x) {
  x <- base_svy %>%
    filter(dpto == x & compra == 1) %>%
    srvyr::summarise(colname = srvyr::survey_mean(gto_nosoportable_comp, na.rm=T))
  x <- mean(x$colname)
}       

c_e_dpto <- numeric()

for(i in 1:19){
  c_e_dpto[i] <- c_dpto(x = i)
}     

# Quintil de ingresos

c_quintil <- function(x) {
  x <- base_svy %>%
    filter(quintilesy == x & compra == 1) %>%
    srvyr::summarise(colname = srvyr::survey_mean(gto_nosoportable_comp, na.rm=T))
  x <- mean(x$colname)
}       

c_e_quintil <- numeric()

for(i in 1:5){
  c_e_quintil[i] <- c_quintil(x = i)
}     


# Sexo

c_sexo <- function(x) {
  x <- base_svy %>%
    filter(bc_pe2 == x & compra == 1) %>%
    srvyr::summarise(colname = srvyr::survey_mean(gto_nosoportable_comp, na.rm=T))
  x <- mean(x$colname)
}       

c_e_sexo <- numeric()

for(i in 1:2){
  c_e_sexo[i] <- c_sexo(x = i)
}     


# Montevideo - Interior

c_regiondic <- function(x) {
  x <- base_svy %>%
    filter(region_dic == x & compra == 1) %>%
    srvyr::summarise(colname = srvyr::survey_mean(gto_nosoportable_comp, na.rm=T))
  x <- mean(x$colname)
}       

c_e_regiondic <- numeric()

for(i in 1:2){
  c_e_regiondic[i] <- c_regiondic(x = i)
}

CODIND       <- 230304
DIMENSIÓN    <- "Gastos soportables"
CODINDICADOR <- "Gasto excesivo en compra de vivienda"
NOMINDICADOR <- "Porcentaje de personas que residen en hogares en proceso de compra de vivienda que destinan más del 25% de sus ingresos al pago de la cuota de compra"
VALOR	       <- c(c_ano, c_e_afro, c_e_dpto, c_e_quintil, c_e_sexo, c_e_regiondic)*100
JERARQUIA	   <- c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
CORTE        <- c("Total", "Ascendencia étnico-racial", "Ascendencia étnico-racial", "Departamento", "Departamento", "Departamento", "Departamento",
                  "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento",
                  "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Quintil de ingresos",
                  "Quintil de ingresos", "Quintil de ingresos", "Quintil de ingresos", "Quintil de ingresos", "Sexo", "Sexo", "Región", "Región")
DEPARTAMENTO <- c("","","", "Montevideo", "Artigas", "Canelones", "Cerro Largo", "Colonia", "Durazno", "Flores", "Florida", "Lavalleja", "Maldonado",
                  "Paysandú", "Río Negro", "Rivera", "Rocha", "Salto", "San José", "Soriano", "Tacuarembó", "Treinta y Tres", "", "", "", "", "", "", "","","")

QUINTIL	      <- c("","","","","","","","","","","","","","","","","","","","","","","Quintil 1", "Quintil 2", "Quintil 3", "Quintil 4", "Quintil 5","","","","")

SEXO		      <- c("","","","","","","","","","","","","","","","","","","","","","","", "", "", "", "","Varones","Mujeres", "","")

ASCENDENCIA   <- c("","Afrodescendiente","No afrodescendiente","","","","","","","","","","","","","","","","","","","","", "", "", "", "","","", "", "") 

EDAD		      <- ""

POBREZA       <- ""

URBANORURALUY <- ""

REGIÓN	<- c("","","","","","","","","","","","","","","","","","","","","","","", "", "", "", "","","","Montevideo","Interior")



NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. La estimaciones desde 2022 se calculan a partir de la muestra de implantación."

v_09 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, 
              JERARQUIA_CAT, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA, EDAD, POBREZA, URBANORURALUY, REGIÓN, NOTA_INDICADOR)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Hacinamiento
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


# Total

c_ano <- base_svy %>%
  srvyr::summarise(colname = srvyr::survey_mean(hacinamiento_bd, na.rm=T))

c_ano <- c_ano$colname
c_ano <- mean(c_ano)


# Ascendencia étnico racial

c_afro <- function(x) {
  x <- base_svy %>%
    filter(bd_e29_1 == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(hacinamiento_bd, na.rm=T))
  x <- mean(x$colname)
}       

c_e_afro <- numeric()

for(i in 1:2){
  c_e_afro[i] <- c_afro(x = i)
}     



# Departamento

c_dpto <- function(x) {
  x <- base_svy %>%
    filter(dpto == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(hacinamiento_bd, na.rm=T))
  x <- mean(x$colname)
}       

c_e_dpto <- numeric()

for(i in 1:19){
  c_e_dpto[i] <- c_dpto(x = i)
}     

# Quintil de ingresos

c_quintil <- function(x) {
  x <- base_svy %>%
    filter(quintilesy == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(hacinamiento_bd, na.rm=T))
  x <- mean(x$colname)
}       

c_e_quintil <- numeric()

for(i in 1:5){
  c_e_quintil[i] <- c_quintil(x = i)
}     


# Sexo

c_sexo <- function(x) {
  x <- base_svy %>%
    filter(bc_pe2 == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(hacinamiento_bd, na.rm=T))
  x <- mean(x$colname)
}       

c_e_sexo <- numeric()

for(i in 1:2){
  c_e_sexo[i] <- c_sexo(x = i)
}     

# Edad

c_edad <- function(x) {
  x <- base_svy %>%
    filter(tramoed2 == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(hacinamiento_bd, na.rm=T))
  x <- mean(x$colname)
}       

c_e_edad <- numeric()

for(i in 1:7){
  c_e_edad[i] <- c_edad(x = i)
}   


# Pobreza

c_pobre <- function(x) {
  x <- base_svy %>%
    filter(pobre06 == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(hacinamiento_bd, na.rm=T))
  x <- mean(x$colname)
}       

c_e_pobre <- numeric()

for(i in 1:2){
  c_e_pobre[i] <- c_pobre(x = i)
}     


# Región

c_region <- function(x) {
  x <- base_svy %>%
    filter(bd_region == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(hacinamiento_bd, na.rm=T))
  x <- mean(x$colname)
}       

c_e_region <- numeric()

for(i in 1:3){
  c_e_region[i] <- c_region(x = i)
} 


CODIND       <- 230401
DIMENSIÓN    <- "Habitabilidad"
CODINDICADOR <- "Hacinamiento"
NOMINDICADOR <- "Porcentaje de personas que residen en viviendas con hacinamiento según definición del INE"
VALOR	       <- c(c_ano, c_e_afro, c_e_dpto, c_e_quintil, c_e_sexo, c_e_edad, c_e_pobre, c_e_region)*100
JERARQUIA	   <- c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0, 0, 0, 0, 0, 0,0)
CORTE        <- c("Total", "Ascendencia étnico-racial", "Ascendencia étnico-racial", "Departamento", "Departamento", "Departamento", "Departamento",
                  "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento",
                  "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Quintil de ingresos",
                  "Quintil de ingresos", "Quintil de ingresos", "Quintil de ingresos", "Quintil de ingresos", "Sexo", "Sexo", "Edad","Edad","Edad", "Edad", "Edad", "Edad", "Edad", "Pobreza", "Pobreza", 
                  "Urbano rural", "Urbano rural", "Urbano rural")
DEPARTAMENTO <- c("","","", "Montevideo", "Artigas", "Canelones", "Cerro Largo", "Colonia", "Durazno", "Flores", "Florida", "Lavalleja", "Maldonado",
                  "Paysandú", "Río Negro", "Rivera", "Rocha", "Salto", "San José", "Soriano", "Tacuarembó", "Treinta y Tres", "","", "", "", "", "", "", "", "", "", "", "", "","", "", "", "", "", "")

QUINTIL	      <- c("","","","","","","","","","","","","","","","","","","","","","","Quintil 1", "Quintil 2", "Quintil 3", "Quintil 4", "Quintil 5","","","", "", "", "", "", "","", "", "", "", "", "")

SEXO		      <- c("","","","","","","","","","","","","","","","","","","","","","","", "", "", "", "","Varones","Mujeres", "","", "", "", "", "","", "", "", "", "", "")

ASCENDENCIA   <- c("","Afrodescendiente","No afrodescendiente","","","","","","","","","","","","","","","","","","","","", "","", "", "", "","","", "", "", "", "", "","", "", "", "", "", "") 

EDAD		      <- c("","","","","","","","","","","","","","","","","","","","","","","", "", "", "", "","","", "0 a 5 años", "6 a 12 años", "13 a 18 años", "19 a 24 años", "25 a 29 años", "30 a 64 años", "65 años o más", "", "", "", "", "")

POBREZA       <- c("","","","","","","","","","","","","","","","","","","","","","","", "", "", "", "","","","","", "", "", "", "", "", "Pobres", "No pobres", "", "", "")

URBANORURALUY	<- c("","","","","","","","","","","","","","","","","","","","","","","", "", "", "", "","","", "","", "","", "", "", "", "", "", "Urbano (más de 5.000 habitantes)", "Urbano (menos de 5.000 habitantes)", "Rural disperso")

REGIÓN        <- ""


NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. La estimaciones desde 2022 se calculan a partir de la muestra de implantación."

v_10 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, 
              JERARQUIA_CAT, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA, EDAD, POBREZA, URBANORURALUY, REGIÓN, NOTA_INDICADOR)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Materiales de construcción de la vivienda inadecuados
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


# Total

c_ano <- base_svy %>%
  srvyr::summarise(colname = srvyr::survey_mean(NBI_Materialidad, na.rm=T))

c_ano <- c_ano$colname
c_ano <- mean(c_ano)


# Ascendencia étnico racial

c_afro <- function(x) {
  x <- base_svy %>%
    filter(bd_e29_1 == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(NBI_Materialidad, na.rm=T))
  x <- mean(x$colname)
}       

c_e_afro <- numeric()

for(i in 1:2){
  c_e_afro[i] <- c_afro(x = i)
}     



# Departamento

c_dpto <- function(x) {
  x <- base_svy %>%
    filter(dpto == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(NBI_Materialidad, na.rm=T))
  x <- mean(x$colname)
}       

c_e_dpto <- numeric()

for(i in 1:19){
  c_e_dpto[i] <- c_dpto(x = i)
}     

# Quintil de ingresos

c_quintil <- function(x) {
  x <- base_svy %>%
    filter(quintilesy == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(NBI_Materialidad, na.rm=T))
  x <- mean(x$colname)
}       

c_e_quintil <- numeric()

for(i in 1:5){
  c_e_quintil[i] <- c_quintil(x = i)
}     


# Sexo

c_sexo <- function(x) {
  x <- base_svy %>%
    filter(bc_pe2 == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(NBI_Materialidad, na.rm=T))
  x <- mean(x$colname)
}       

c_e_sexo <- numeric()

for(i in 1:2){
  c_e_sexo[i] <- c_sexo(x = i)
}     

# Edad

c_edad <- function(x) {
  x <- base_svy %>%
    filter(tramoed2 == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(NBI_Materialidad, na.rm=T))
  x <- mean(x$colname)
}       

c_e_edad <- numeric()

for(i in 1:7){
  c_e_edad[i] <- c_edad(x = i)
}   


# Pobreza

c_pobre <- function(x) {
  x <- base_svy %>%
    filter(pobre06 == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(NBI_Materialidad, na.rm=T))
  x <- mean(x$colname)
}       

c_e_pobre <- numeric()

for(i in 1:2){
  c_e_pobre[i] <- c_pobre(x = i)
}     


# Región

c_region <- function(x) {
  x <- base_svy %>%
    filter(bd_region == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(NBI_Materialidad, na.rm=T))
  x <- mean(x$colname)
}       

c_e_region <- numeric()

for(i in 1:3){
  c_e_region[i] <- c_region(x = i)
}


CODIND       <- 230402
DIMENSIÓN    <- "Habitabilidad"
CODINDICADOR <- "Materiales de construcción de la vivienda inadecuados"
NOMINDICADOR <- "Porcentaje de personas que residen en viviendas con paredes o techos de desecho o piso de tierra"
VALOR	       <- c(c_ano, c_e_afro, c_e_dpto, c_e_quintil, c_e_sexo, c_e_edad, c_e_pobre, c_e_region)*100
JERARQUIA	   <- c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0, 0, 0, 0, 0, 0)
CORTE        <- c("Total", "Ascendencia étnico-racial", "Ascendencia étnico-racial", "Departamento", "Departamento", "Departamento", "Departamento",
                  "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento",
                  "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Quintil de ingresos",
                  "Quintil de ingresos", "Quintil de ingresos", "Quintil de ingresos", "Quintil de ingresos", "Sexo", "Sexo", "Edad","Edad","Edad", "Edad", "Edad", "Edad", "Edad", "Pobreza", "Pobreza", 
                  "Urbano rural", "Urbano rural", "Urbano rural")
DEPARTAMENTO <- c("","","", "Montevideo", "Artigas", "Canelones", "Cerro Largo", "Colonia", "Durazno", "Flores", "Florida", "Lavalleja", "Maldonado",
                  "Paysandú", "Río Negro", "Rivera", "Rocha", "Salto", "San José", "Soriano", "Tacuarembó", "Treinta y Tres","", "", "", "", "", "", "", "", "", "", "", "", "","", "", "", "", "", "")

QUINTIL	      <- c("","","","","","","","","","","","","","","","","","","","","","","Quintil 1", "Quintil 2", "Quintil 3", "Quintil 4", "Quintil 5","","","", "", "", "", "", "","", "", "", "", "", "")

SEXO		      <- c("","","","","","","","","","","","","","","","","","","","","","","", "", "", "", "","Varones","Mujeres","", "", "", "", "", "","", "", "", "", "", "")

ASCENDENCIA   <- c("","Afrodescendiente","No afrodescendiente","","","","","","","","","","","","","","","","","","","","", "","", "", "", "","","", "", "", "", "", "","", "", "", "", "", "") 

EDAD		      <- c("","","","","","","","","","","","","","","","","","","","","","","", "", "", "", "","","", "0 a 5 años", "6 a 12 años", "13 a 18 años", "19 a 24 años", "25 a 29 años", "30 a 64 años", "65 años o más", "", "", "", "", "")

POBREZA       <- c("","","","","","","","","","","","","","","","","","","","","","","", "", "", "", "","","","", "","", "", "", "", "", "Pobres", "No pobres", "", "", "")

URBANORURALUY	<- c("","","","","","","","","","","","","","","","","","","","","","","", "", "", "", "","","", "", "","","", "", "", "", "", "", "Urbano (más de 5.000 habitantes)", "Urbano (menos de 5.000 habitantes)", "Rural disperso")

REGIÓN        <- ""


NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. La estimaciones desde 2022 se calculan a partir de la muestra de implantación."

v_11 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, 
              JERARQUIA_CAT, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA, EDAD, POBREZA, URBANORURALUY, REGIÓN, NOTA_INDICADOR)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Asentamientos
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


# Total

c_ano <- base_svy %>%
  srvyr::summarise(colname = srvyr::survey_mean(asentamiento, na.rm=T))

c_ano <- c_ano$colname
c_ano <- mean(c_ano)


# Ascendencia étnico racial

c_afro <- function(x) {
  x <- base_svy %>%
    filter(bd_e29_1 == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(asentamiento, na.rm=T))
  x <- mean(x$colname)
}       

c_e_afro <- numeric()

for(i in 1:2){
  c_e_afro[i] <- c_afro(x = i)
}     



# Departamento

c_dpto <- function(x) {
  x <- base_svy %>%
    filter(dpto == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(asentamiento, na.rm=T))
  x <- mean(x$colname)
}       

c_e_dpto <- numeric()

for(i in 1:19){
  c_e_dpto[i] <- c_dpto(x = i)
}     

# Quintil de ingresos

c_quintil <- function(x) {
  x <- base_svy %>%
    filter(quintilesy == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(asentamiento, na.rm=T))
  x <- mean(x$colname)
}       

c_e_quintil <- numeric()

for(i in 1:5){
  c_e_quintil[i] <- c_quintil(x = i)
}     


# Sexo

c_sexo <- function(x) {
  x <- base_svy %>%
    filter(bc_pe2 == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(asentamiento, na.rm=T))
  x <- mean(x$colname)
}       

c_e_sexo <- numeric()

for(i in 1:2){
  c_e_sexo[i] <- c_sexo(x = i)
}     

# Edad

c_edad <- function(x) {
  x <- base_svy %>%
    filter(tramoed == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(asentamiento, na.rm=T))
  x <- mean(x$colname)
}       

c_e_edad <- numeric()

for(i in 1:6){
  c_e_edad[i] <- c_edad(x = i)
}   


CODIND       <- 230501
DIMENSIÓN    <- "Ubicación"
CODINDICADOR <- "Asentamientos"
NOMINDICADOR <- "Porcentaje de personas que viven en asentamientos"
VALOR	       <- c(c_ano, c_e_afro, c_e_dpto, c_e_quintil, c_e_sexo, c_e_edad)*100
JERARQUIA	   <- c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
CORTE        <- c("Total", "Ascendencia étnico-racial", "Ascendencia étnico-racial", "Departamento", "Departamento", "Departamento", "Departamento",
                  "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento",
                  "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Quintil de ingresos",
                  "Quintil de ingresos", "Quintil de ingresos", "Quintil de ingresos", "Quintil de ingresos", "Sexo", "Sexo", "Edad", "Edad", "Edad", "Edad", "Edad", "Edad")
DEPARTAMENTO <- c("","","", "Montevideo", "Artigas", "Canelones", "Cerro Largo", "Colonia", "Durazno", "Flores", "Florida", "Lavalleja", "Maldonado",
                  "Paysandú", "Río Negro", "Rivera", "Rocha", "Salto", "San José", "Soriano", "Tacuarembó", "Treinta y Tres", "", "", "", "", "", "", "","","","","","", "")

QUINTIL	      <- c("","","","","","","","","","","","","","","","","","","","","","","Quintil 1", "Quintil 2", "Quintil 3", "Quintil 4", "Quintil 5","","","","","","","", "")

SEXO		      <- c("","","","","","","","","","","","","","","","","","","","","","","", "", "", "", "","Varones","Mujeres","","","","","", "")

ASCENDENCIA   <- c("","Afrodescendiente","No afrodescendiente","","","","","","","","","","","","","","","","","","","","", "","","","","","", "","","","","", "") 

EDAD		      <- c("","","","","","","","","","","","","","","","","","","","","","","", "", "", "", "","","", "0 a 5 años", "6 a 12 años", "13 a 17 años", "18 a 29 años", "30 a 64 años", "65 años o más")

POBREZA       <- ""

URBANORURALUY	<- ""

REGIÓN        <- ""

NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. La estimaciones desde 2022 se calculan a partir de la muestra de implantación."

v_12 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, 
              JERARQUIA_CAT, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA, EDAD, POBREZA, URBANORURALUY, REGIÓN, NOTA_INDICADOR)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
### Generación de Base motor ###
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #



v_2024 <- rbind(v_01, v_02, v_03, v_04, v_05, v_06, v_07, v_08, v_09, v_10, v_11, v_12)

v_2024 <- as.data.frame(v_2024)


v_2024 <- v_2024 %>% mutate(filter = ifelse((CORTE == "Departamento" & 
                                              (CODIND == "230201" | 
                                                 CODIND == "230202" | 
                                                 CODIND == "230203" | 
                                                 CODIND == "230302" | 
                                                 CODIND == "230303" |
                                                 CODIND == "230401" | 
                                                 CODIND == "230402" |
                                                 CODIND == "230204" |
                                                 CODIND == "230301" |
                                                 CODIND == "230304" )) |
                                             (CORTE == "Quintil de ingresos" &
                                               (CODIND == "230203" |
                                                  CODIND == "230301" | 
                                                  CODIND == "230304")) |
                                              (CORTE == "Ascendencia étnico-racial" &
                                                 CODIND == "230304"), 1, 0)) # Se eliminan cortes por cantidad de casos


table(v_2024$filter)
v_2024 <- filter(v_2024, filter == 0)

v_2024$filter <- NULL


rio::export(v_2024, "Tabulados/Tabulados 2024/v_2024.xlsx" )


