### Mirador DESCA
### Derecho a ambiente sano
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

DERECHO      <- "Ambiente"
TIPOIND      <- "Resultados"
AÑO	         <- 2022
FUENTE	     <- "Encuesta Continua de Hogares 2022, INE"
RESPONSABLE	 <- "J. Pandolfi"
JERARQUIA_CAT <- 1

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

### Carga de bases ###

base          <- rio::import("data_ech/Base anual 2022.Rdata")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

### Generación de nuevas variables ###

#IPC

base <- base %>% dplyr::mutate(bc_ipc_tot = case_when(
  mes == 1  ~ 0.310804203,
  mes == 2  ~ 0.305354401,
  mes == 3  ~ 0.300928439,
  mes == 4  ~ 0.297635795,
  mes == 5  ~ 0.296181029,
  mes == 6  ~ 0.294810362,
  mes == 7  ~ 0.293094596,
  mes == 8  ~ 0.290852868,
  mes == 9  ~ 0.288466429,
  mes == 10 ~ 0.286063926,
  mes == 11 ~ 0.28546134,
  mes == 12 ~ 0.286262936))

  
# Ingresos

base <- base %>% dplyr::mutate(y_pc       =  ht11 / HT19 )                      #Ingreso per-cápita
base <- base %>% dplyr::mutate(y_pc_svl   =  (ht11 - ht13) / HT19)              #Ingreso per-cápita sin valor locativo
base <- base %>% dplyr::mutate(y_pc_d     =  ht11 / HT19 / bc_ipc_tot)          #Ingreso per-cápita deflactado
base <- base %>% dplyr::mutate(y_pc_svl_d =  (ht11 - ht13) / HT19 / bc_ipc_tot) #Ingreso per-cápita sin valor locativo deflactado


# Quintil de ingresos

base_h <- base %>% distinct(ID, .keep_all = TRUE)
base_h <- base_h %>% dplyr::mutate(quintilesy = statar::xtile(y_pc, n=5, wt = w_ano))
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

base <- base %>% dplyr::mutate(bd_region = case_when(region_4 == 1 | region_4 == 2  ~ 1,
                                                     region_4 == 3  ~ 2,
                                                     region_4 == 4  ~ 3))

# Pobreza

base <- base %>% dplyr::mutate(pobre06 = case_when(pobre == 1 ~ 1,
                                                   pobre == 0 ~ 2))



# No acceso a agua potable
# El origen del agua no es por red general ni pozo surgente y la llegada del agua no es por cañería dentro de la vivienda 
# En estas viviendas el agua tiene como origen aljibes, arroyo, río, cachimba, etc., y llega por cañería exterior u otros medios.

base <- base %>% dplyr::mutate(bd_d11 = case_when(d11 == 1 ~ 1,
                                                  d11 == 2 | d11 == 3 ~ 2,
                                                  d11 >= 4 & d11 <= 6 ~ 3))

base <- base %>% dplyr::mutate(bd_d12 = case_when(d12 == 1 ~ 1,
                                                  d12 == 2 | d11 == 3 ~ 2,
                                                  d12 == 4 ~ 3))

base <- base %>% dplyr::mutate(NBI_agua11 = case_when((bd_d12==2 | bd_d12==3) | bd_d11==3 ~ 1,
                                                       bd_d12==1 & (bd_d11==1 | bd_d11==2) ~ 0))
                                                 

# No acceso a red general de saneamiento 
base <- base %>% dplyr::mutate(bd_d16 = case_when(d16 == 0 ~ 99,
                                                  d16 == 4 ~ 3,
                                                  d16 == 1 ~ 1,
                                                  d16 == 2 ~ 2,
                                                  d16 == 3 ~ 3))

# sin conexión a red general de saneamiento.

base <- base %>% dplyr::mutate(saneam = case_when(bd_d16 > 1 & bd_d16<5 ~ 1,
                                                  bd_d16  ==1 ~ 0))
                                                  

# No acceso a saneamiento de calidad 
# sin conexión a red general de saneamiento, fosa séptica o pozo negro, y sin servicio sanitario de uso exclusivo del hogar.

base <- base %>% dplyr::mutate(serv_san = case_when((d13==3|d15==2)  ~ 1,
                                                    (d13==1|d13==2|d15==1) ~ 0))



base <- base %>% dplyr::mutate(saneam_cal = case_when(saneam==1&serv_san==1  ~ 1,
                                                      saneam==0|serv_san==0  ~ 0))




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

### Información de muestreo ### 

base_svy <- srvyr::as_survey_design(base, ids = ID, weights = w_ano)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
### Procesamiento de indicadores ###
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


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


CODIND       <- 630103
DIMENSIÓN    <- "Accesibilidad"
CODINDICADOR <- "No acceso a agua potable"
NOMINDICADOR <- "Porcentaje de personas que residen en viviendas sin agua potable"
VALOR	       <- c(c_ano, c_e_afro, c_e_quintil, c_e_sexo, c_e_edad, c_e_pobre, c_e_region)*100
JERARQUIA	   <- c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0, 0, 0, 0, 0, 0, 0)
CORTE        <- c("Total", "Ascendencia étnico-racial", "Ascendencia étnico-racial", "Quintil de ingresos",
                  "Quintil de ingresos", "Quintil de ingresos", "Quintil de ingresos", "Quintil de ingresos", "Sexo", "Sexo", "Edad", "Edad","Edad", "Edad", "Edad", "Edad", "Edad", "Pobreza", "Pobreza", 
                  "Urbano rural", "Urbano rural", "Urbano rural")
DEPARTAMENTO <- ""

QUINTIL	      <- c("","","","Quintil 1", "Quintil 2", "Quintil 3", "Quintil 4", "Quintil 5","","","", "", "", "", "", "","", "", "", "", "", "")

SEXO		      <- c("","","","","","","","","Varones","Mujeres", "", "", "", "","","","", "", "", "", "", "")

ASCENDENCIA   <- c("","Afrodescendiente","No afrodescendiente","", "", "", "", "","","", "","", "", "", "", "","", "", "", "", "", "") 

EDAD		      <- c("","","","", "", "", "", "","","", "0 a 5 años", "6 a 12 años", "13 a 18 años", "19 a 24 años", "25 a 29 años", "30 a 64 años", "65 años o más", "", "", "", "", "")

POBREZA       <- c("","","","", "", "", "", "","","","", "", "", "", "", "", "", "Pobres", "No pobres", "", "", "")

URBANORURALUY	<- c("","","","", "", "", "", "","","", "", "","", "", "", "", "", "", "", "Urbano (más de 5.000 habitantes)", "Urbano (menos de 5.000 habitantes)", "Rural disperso")

REGIÓN        <- ""


NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. Las estimaciones desde 2022 se calculan a partir de la muestra de implantación."

a_01 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, 
              JERARQUIA_CAT, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA, EDAD, POBREZA, URBANORURALUY, REGIÓN, NOTA_INDICADOR)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de personas en hogares sin acceso a red general de saneamiento 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


# Total

c_ano <- base_svy %>%
  srvyr::summarise(colname = srvyr::survey_mean(saneam, na.rm=T))

c_ano <- c_ano$colname
c_ano <- mean(c_ano)


# Ascendencia étnico racial

c_afro <- function(x) {
  x <- base_svy %>%
    filter(bd_e29_1 == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(saneam, na.rm=T))
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
    srvyr::summarise(colname = srvyr::survey_mean(saneam, na.rm=T))
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
    srvyr::summarise(colname = srvyr::survey_mean(saneam, na.rm=T))
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
    srvyr::summarise(colname = srvyr::survey_mean(saneam, na.rm=T))
  x <- mean(x$colname)
}       

c_e_sexo <- numeric()

for(i in 1:2){
  c_e_sexo[i] <- c_sexo(x = i)
}     

CODIND       <- 630101
DIMENSIÓN    <- "Accesibilidad"
CODINDICADOR <- "No acceso a red general de saneamiento"
NOMINDICADOR <- "Porcentaje de personas en hogares sin acceso a red general de saneamiento"
VALOR	       <- c(c_ano, c_e_afro, c_e_dpto, c_e_quintil, c_e_sexo)*100
JERARQUIA	   <- c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
CORTE        <- c("Total", "Ascendencia étnico-racial", "Ascendencia étnico-racial", "Departamento", "Departamento", "Departamento", "Departamento",
                  "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento",
                  "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Quintil de ingresos",
                  "Quintil de ingresos", "Quintil de ingresos", "Quintil de ingresos", "Quintil de ingresos", "Sexo", "Sexo")
DEPARTAMENTO <- c("","","", "Montevideo", "Artigas", "Canelones", "Cerro Largo", "Colonia", "Durazno", "Flores", "Florida", "Lavalleja", "Maldonado",
                  "Paysandú", "Río Negro", "Rivera", "Rocha", "Salto", "San José", "Soriano", "Tacuarembó", "Treinta y Tres", "", "", "", "", "", "", "")

QUINTIL	      <- c("","","","","","","","","","","","","","","","","","","","","","","Quintil 1", "Quintil 2", "Quintil 3", "Quintil 4", "Quintil 5", "","")

SEXO		      <- c("","","","","","","","","","","","","","","","","","","","","","","", "", "", "", "","Varones","Mujeres")

ASCENDENCIA   <- c("","Afrodescendiente","No afrodescendiente","","","","","","","","","","","","","","","","","","","","", "", "", "", "", "","") 

EDAD		      <- ""
POBREZA       <- ""
URBANORURALUY	<- ""
REGIÓN        <- ""

NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. La estimaciones desde 2022 se calculan a partir de la muestra de implantación."

a_02 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, 
              JERARQUIA_CAT, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA, EDAD, POBREZA, URBANORURALUY, REGIÓN, NOTA_INDICADOR)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de personas en hogares sin acceso a saneamiento de calidad 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


# Total

c_ano <- base_svy %>%
  srvyr::summarise(colname = srvyr::survey_mean(saneam_cal, na.rm=T))

c_ano <- c_ano$colname
c_ano <- mean(c_ano)


# Ascendencia étnico racial

c_afro <- function(x) {
  x <- base_svy %>%
    filter(bd_e29_1 == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(saneam_cal, na.rm=T))
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
    srvyr::summarise(colname = srvyr::survey_mean(saneam_cal, na.rm=T))
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
    srvyr::summarise(colname = srvyr::survey_mean(saneam_cal, na.rm=T))
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
    srvyr::summarise(colname = srvyr::survey_mean(saneam_cal, na.rm=T))
  x <- mean(x$colname)
}       

c_e_sexo <- numeric()

for(i in 1:2){
  c_e_sexo[i] <- c_sexo(x = i)
}     

CODIND       <- 630102
DIMENSIÓN    <- "Accesibilidad"
CODINDICADOR <- "No acceso a saneamiento de calidad"
NOMINDICADOR <- "Porcentaje de personas en hogares sin acceso a saneamiento de calidad"
VALOR	       <- c(c_ano, c_e_afro, c_e_dpto, c_e_quintil, c_e_sexo)*100
JERARQUIA	   <- c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
CORTE        <- c("Total", "Ascendencia étnico-racial", "Ascendencia étnico-racial", "Departamento", "Departamento", "Departamento", "Departamento",
                  "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento",
                  "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Quintil de ingresos",
                  "Quintil de ingresos", "Quintil de ingresos", "Quintil de ingresos", "Quintil de ingresos", "Sexo", "Sexo")
DEPARTAMENTO <- c("","","", "Montevideo", "Artigas", "Canelones", "Cerro Largo", "Colonia", "Durazno", "Flores", "Florida", "Lavalleja", "Maldonado",
                  "Paysandú", "Río Negro", "Rivera", "Rocha", "Salto", "San José", "Soriano", "Tacuarembó", "Treinta y Tres", "", "", "", "", "", "", "")

QUINTIL	      <- c("","","","","","","","","","","","","","","","","","","","","","","Quintil 1", "Quintil 2", "Quintil 3", "Quintil 4", "Quintil 5", "","")

SEXO		      <- c("","","","","","","","","","","","","","","","","","","","","","","", "", "", "", "","Varones","Mujeres")

ASCENDENCIA   <- c("","Afrodescendiente","No afrodescendiente","","","","","","","","","","","","","","","","","","","","", "", "", "", "", "","") 

EDAD		      <- ""
POBREZA       <- ""
URBANORURALUY	<- ""
REGIÓN        <- ""

NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. La estimaciones desde 2022 se calculan a partir de la muestra de implantación."

a_03 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, 
              JERARQUIA_CAT, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA, EDAD, POBREZA, URBANORURALUY, REGIÓN, NOTA_INDICADOR)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
### Generación de Base motor ###
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #



amb_2022 <- rbind(a_01, a_02, a_03)

amb_2022 <- as.data.frame(amb_2022)

rio::export(amb_2022, "Tabulados/Tabulados 2023/amb_2022.xlsx" )


