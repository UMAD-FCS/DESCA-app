### Mirador DESCA
### Derecho al Trabajo
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

DERECHO      <- "Trabajo"
TIPOIND      <- "Resultados"
AÑO	         <- 2024
FUENTE	     <- "Encuesta Continua de Hogares 2024, INE"
RESPONSABLE	 <- "J. Pandolfi"
JERARQUIA_CAT <- 1

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

### Carga de bases ###

base          <- rio::import("data_ech/ECH_2024.csv")
bases_men       <- rio::import("data_ech/ECH_men_2024.RData")

varsimplant <- select(base, ID, HT11, HT19, HT13)
varsimplant <- varsimplant %>% distinct(ID, .keep_all = TRUE)

bases_men       <- base::merge(bases_men, varsimplant, by = "ID", all.x = TRUE, all.y = FALSE)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

### Generación de nuevas variables ###

# Salario mínimo nacional

smn <- 22268
smn_h = smn/200

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
  
bases_men <- bases_men %>% dplyr::mutate(bc_ipc_tot = case_when(
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
  


base <- base %>% dplyr::mutate(ipc_ene2024 = case_when(
  mes == 1 ~  1,
  mes == 2 ~  1.015288838,
  mes == 3 ~  1.021812804,
  mes == 4 ~  1.02202024,
  mes == 5 ~  1.028443742,
  mes == 6 ~  1.032537392,
  mes == 7 ~  1.036215295,
  mes == 8 ~  1.037319032,
  mes == 9 ~  1.040283613,
  mes == 10 ~  1.04418169,
  mes == 11 ~  1.047584807,
  mes == 12 ~  1.051361748))



bases_men <- bases_men %>% dplyr::mutate(ipc_ene2024 = case_when(
  mes == 1 ~  1,
  mes == 2 ~  1.015288838,
  mes == 3 ~  1.021812804,
  mes == 4 ~  1.02202024,
  mes == 5 ~  1.028443742,
  mes == 6 ~  1.032537392,
  mes == 7 ~  1.036215295,
  mes == 8 ~  1.037319032,
  mes == 9 ~  1.040283613,
  mes == 10 ~  1.04418169,
  mes == 11 ~  1.047584807,
  mes == 12 ~  1.051361748))



# Ingresos

bases_men <- bases_men %>% dplyr::mutate(y_pc       =  HT11 / HT19 )                      #Ingreso per-cápita
bases_men <- bases_men %>% dplyr::mutate(y_pc_svl   =  (HT11 - HT13) / HT19)              #Ingreso per-cápita sin valor locativo
bases_men <- bases_men %>% dplyr::mutate(y_pc_d     =  HT11 / HT19 / bc_ipc_tot)          #Ingreso per-cápita deflactado
bases_men <- bases_men %>% dplyr::mutate(y_pc_svl_d =  (HT11 - HT13) / HT19 / bc_ipc_tot) #Ingreso per-cápita sin valor locativo deflactado

base <- base %>% dplyr::mutate(y_pc       =  HT11 / HT19 )                      #Ingreso per-cápita
base <- base %>% dplyr::mutate(y_pc_svl   =  (HT11 - HT13) / HT19)              #Ingreso per-cápita sin valor locativo
base <- base %>% dplyr::mutate(y_pc_d     =  HT11 / HT19 / bc_ipc_tot)          #Ingreso per-cápita deflactado
base <- base %>% dplyr::mutate(y_pc_svl_d =  (HT11 - HT13) / HT19 / bc_ipc_tot) #Ingreso per-cápita sin valor locativo deflactado


# Quintil de ingresos

base_h <- bases_men %>% distinct(ID, .keep_all = TRUE)
base_h <- base_h %>% dplyr::mutate(quintilesy = statar::xtile(y_pc, n=5, wt = W))
base_h <- base_h[,c("ID","quintilesy")]
bases_men <- merge(bases_men, base_h, by = "ID")

base_h_a <- base %>% distinct(ID, .keep_all = TRUE)
base_h_a <- base_h_a %>% dplyr::mutate(quintilesy = statar::xtile(y_pc, n=5, wt = W_ANO))
base_h_a <- base_h_a[,c("ID","quintilesy")]
base <- merge(base, base_h_a, by = "ID")

# Sexo

bases_men <- bases_men %>% dplyr::mutate(bc_pe2 = e26)
base <- base %>% dplyr::mutate(bc_pe2 = e26)


# Ascendencia afro

bases_men <- bases_men %>% dplyr::mutate(bd_e29_1 = e29_1)
base <- base %>% dplyr::mutate(bd_e29_1 = e29_1)


# Población económicamente activa

bases_men <- bases_men %>% dplyr::mutate(pea = ifelse(POBPCOAC==2 | POBPCOAC==3 | POBPCOAC==4 | POBPCOAC==5, 1, 0))
base <- base %>% dplyr::mutate(pea = ifelse(POBPCOAC==2 | POBPCOAC==3 | POBPCOAC==4 | POBPCOAC==5, 1, 0))


# Ocupados

bases_men <- bases_men %>% dplyr::mutate(ocup = ifelse(POBPCOAC==2, 1, 0))
base <- base %>% dplyr::mutate(ocup = ifelse(POBPCOAC==2, 1, 0))


# Desempleados

bases_men <- bases_men %>% dplyr::mutate(desocup = ifelse(POBPCOAC==3 | POBPCOAC==4 | POBPCOAC==5, 1, 0))
base <- base %>% dplyr::mutate(desocup = ifelse(POBPCOAC==3 | POBPCOAC==4 | POBPCOAC==5, 1, 0))


# Horas remuneradas

bases_men <- bases_men  %>% dplyr::mutate(bc_horas = f85+f98)
base <- base  %>% dplyr::mutate(bc_horas = f85+f98)


# Sobrecarga de horas

bases_men <- bases_men  %>% dplyr::mutate(sobrecarga = ifelse(bc_horas>48, 1, 0))
base <- base  %>% dplyr::mutate(bc_horas = f85+f98)


# Subempleo

bases_men <- bases_men %>% dplyr::mutate(bc_subocupado = ifelse(f102 == 1 & f104 == 5 & bc_horas > 0 & bc_horas < 40, 1, 0))
base <- base %>% dplyr::mutate(bc_subocupado = ifelse(f102 == 1 & f104 == 5 & bc_horas > 0 & bc_horas < 40, 1, 0))


# No aporte a SS en trabajo principal y secundario

base <- base %>% dplyr::mutate(bc_register2 = ifelse(f96 == 1 | f82 == 1, 0, 1)) #no se pregunta por aprote en ocupacion secundaria en panel


# No aporte a SS en trabajo principal

bases_men <- bases_men %>% dplyr::mutate(bc_register = ifelse(f82 == 1, 0, 1))
base <- base %>% dplyr::mutate(bc_register = ifelse(f82 == 1, 0, 1))


# No aporte por totalidad del salario

base <- base %>% dplyr::mutate(noaportatot = ifelse(f84 == 2, 1, 0))


# Restricciones al empleo

base <- base %>% dplyr::mutate(restric = ifelse(bc_register2 == 1 | bc_subocupado ==1, 1, 0))


# Salario en ocupación principal

base <- base %>% dplyr::mutate(horamen = f85 * 4.3)
base <- base %>% dplyr::mutate(yhora = PT2/horamen)
base <- base %>% dplyr::mutate(yhoradef = yhora/bc_ipc_tot)

# Línea de pobreza individual

base <- base %>% dplyr::mutate(regionlp = case_when(REGION_4 == 1 ~ 1,
                                                    REGION_4 == 2 | REGION_4 == 3 ~ 2,
                                                    REGION_4 == 4 ~ 3))


base <- base %>% dplyr::mutate(grupolp = case_when(regionlp == 1 & mes == 1  ~ 1,
                                                   regionlp == 1 & mes == 2  ~ 2,
                                                   regionlp == 1 & mes == 3  ~ 3,
                                                   regionlp == 1 & mes == 4  ~ 4,
                                                   regionlp == 1 & mes == 5  ~ 5,
                                                   regionlp == 1 & mes == 6  ~ 6,
                                                   regionlp == 1 & mes == 7  ~ 7,
                                                   regionlp == 1 & mes == 8  ~ 8,
                                                   regionlp == 1 & mes == 9  ~ 9,
                                                   regionlp == 1 & mes == 10 ~ 10,
                                                   regionlp == 1 & mes == 11 ~ 11,
                                                   regionlp == 1 & mes == 12 ~ 12,
                                                   regionlp == 2 & mes == 1  ~ 13,
                                                   regionlp == 2 & mes == 2  ~ 14,
                                                   regionlp == 2 & mes == 3  ~ 15,
                                                   regionlp == 2 & mes == 4  ~ 16,
                                                   regionlp == 2 & mes == 5  ~ 17,
                                                   regionlp == 2 & mes == 6  ~ 18,
                                                   regionlp == 2 & mes == 7  ~ 19,
                                                   regionlp == 2 & mes == 8  ~ 20,
                                                   regionlp == 2 & mes == 9  ~ 21,
                                                   regionlp == 2 & mes == 10 ~ 22,
                                                   regionlp == 2 & mes == 11 ~ 23,
                                                   regionlp == 2 & mes == 12 ~ 24, 
                                                   regionlp == 3 & mes == 1  ~ 25,
                                                   regionlp == 3 & mes == 2  ~ 26,
                                                   regionlp == 3 & mes == 3  ~ 27,
                                                   regionlp == 3 & mes == 4  ~ 28,
                                                   regionlp == 3 & mes == 5  ~ 29,
                                                   regionlp == 3 & mes == 6  ~ 30,
                                                   regionlp == 3 & mes == 7  ~ 31,
                                                   regionlp == 3 & mes == 8  ~ 32,
                                                   regionlp == 3 & mes == 9  ~ 33,
                                                   regionlp == 3 & mes == 10 ~ 34,
                                                   regionlp == 3 & mes == 11 ~ 35,
                                                   regionlp == 3 & mes == 12 ~ 36))                                                    




base_unipersonales <- base %>%  filter(HT19==1)
base_unipersonales <- base_unipersonales[,c("grupolp", "lp_06")]
base_unipersonales <- base_unipersonales %>% dplyr::mutate(lp_unipersonales = lp_06)
base_unipersonales <- base_unipersonales[order(base_unipersonales$grupolp, 
                                               base_unipersonales$lp_unipersonales, 
                                               decreasing = TRUE), ]
base_unipersonales <- base_unipersonales %>% distinct(grupolp, .keep_all = TRUE)
base_unipersonales <- base_unipersonales[,c("grupolp","lp_unipersonales")]

base <- merge(base, base_unipersonales, by = "grupolp")



# Salario por debajo de línea de pobreza

base <- base %>% dplyr::mutate(salario_insuf = ifelse(PT2 < lp_unipersonales, 1, 0))


# Salario por debajo del salario mínimo nacional

base <- base %>% dplyr::mutate(salario_insuf_smn = ifelse(yhora<smn_h, 1, 0))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

### Información de muestreo ### 

base_men_svy <- srvyr::as_survey_design(bases_men, ids = ID, weights = W)
base_svy <- srvyr::as_survey_design(base, ids = ID, weights = W_ANO)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
### Procesamiento de indicadores ###
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Tasa de actividad
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Total

c_ano <- base_men_svy %>%
  srvyr::filter(e27>=14) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(pea, na.rm=T))

c_ano <- c_ano$colname
c_ano <- mean(c_ano)


# Ascendencia étnico racial

c_afro <- function(x) {
  x <- base_men_svy %>%
    filter(bd_e29_1 == x & e27>=14) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pea, na.rm=T))
  x <- mean(x$colname)
}       

c_e_afro <- numeric()

for(i in 1:2){
  c_e_afro[i] <- c_afro(x = i)
}     



# Departamento

c_dpto <- function(x) {
  x <- base_men_svy %>%
    filter(dpto == x & e27>14) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pea, na.rm=T))
  x <- mean(x$colname)
}       

c_e_dpto <- numeric()

for(i in 1:19){
  c_e_dpto[i] <- c_dpto(x = i)
}     

# Quintil de ingresos

c_quintil <- function(x) {
  x <- base_men_svy %>%
    filter(quintilesy == x & e27>=14) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pea, na.rm=T))
  x <- mean(x$colname)
}       

c_e_quintil <- numeric()

for(i in 1:5){
  c_e_quintil[i] <- c_quintil(x = i)
}     


# Sexo

c_sexo <- function(x) {
  x <- base_men_svy %>%
    filter(bc_pe2 == x & e27>=14) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pea, na.rm=T))
  x <- mean(x$colname)
}       

c_e_sexo <- numeric()

for(i in 1:2){
  c_e_sexo[i] <- c_sexo(x = i)
}     


CODIND       <- 530101
DIMENSIÓN    <- "Accesibilidad"
CODINDICADOR <- "Tasa de actividad"
NOMINDICADOR <- "Tasa de actividad"
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

NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. Las estimaciones desde 2022 se calculan a partir de la muestra panel. Las variables de corte son incorporadas a partir del formulario de implantación."

t_01 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, JERARQUIA_CAT, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA, NOTA_INDICADOR)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Tasa de empleo
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


# Total

c_ano <- base_men_svy %>%
  srvyr::filter(e27>=14) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(ocup, na.rm=T))

c_ano <-mean(c_ano$colname)

# Ascendencia étnico racial


c_afro <- function(x) {
  x <- base_men_svy %>%
    filter(bd_e29_1 == x & e27>=14) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(ocup, na.rm=T))
  x <- mean(x$colname)
}       

c_e_afro <- numeric()

for(i in 1:2){
  c_e_afro[i] <- c_afro(x = i)
}     


# Departamento


c_dpto <- function(x) {
  x <- base_men_svy %>%
    filter(dpto == x & e27>=14) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(ocup, na.rm=T))
  x <- mean(x$colname)
}       

c_e_dpto <- numeric()

for(i in 1:19){
  c_e_dpto[i] <- c_dpto(x = i)
}     


# Quintil de ingresos


c_quintil <- function(x) {
  x <- base_men_svy %>%
    filter(quintilesy == x & e27>=14) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(ocup, na.rm=T))
  x <- mean(x$colname)
}       

c_e_quintil <- numeric()

for(i in 1:5){
  c_e_quintil[i] <- c_quintil(x = i)
}     


# Sexo


c_sexo <- function(x) {
  x <- base_men_svy %>%
    filter(bc_pe2 == x & e27>=14) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(ocup, na.rm=T))
  x <- mean(x$colname)
}       

c_e_sexo <- numeric()

for(i in 1:2){
  c_e_sexo[i] <- c_sexo(x = i)
}     


CODIND       <- 530103
DIMENSIÓN    <- "Accesibilidad"
CODINDICADOR <- "Tasa de empleo"
NOMINDICADOR <- "Tasa de empleo"
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

NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. Las estimaciones desde 2022 se calculan a partir de la muestra panel. Las variables de corte son incorporadas a partir del formulario de implantación."

t_02 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, JERARQUIA_CAT, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA, NOTA_INDICADOR)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Tasa de desempleo
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


# Total

c_ano <- base_men_svy %>%
  srvyr::filter(pea == 1) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(desocup, na.rm=T))

c_ano <-mean(c_ano$colname)

# Ascendencia étnico racial


c_afro <- function(x) {
  x <- base_men_svy %>%
    filter(bd_e29_1 == x & pea == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(desocup, na.rm=T))
  x <- mean(x$colname)
}       

c_e_afro <- numeric()

for(i in 1:2){
  c_e_afro[i] <- c_afro(x = i)
}     


# Departamento


c_dpto <- function(x) {
  x <- base_men_svy %>%
    filter(dpto == x & pea == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(desocup, na.rm=T))
  x <- mean(x$colname)
}       

c_e_dpto <- numeric()

for(i in 1:19){
  c_e_dpto[i] <- c_dpto(x = i)
}     

# Quintil de ingresos

c_quintil <- function(x) {
  x <- base_men_svy %>%
    filter(quintilesy == x & pea == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(desocup, na.rm=T))
  x <- mean(x$colname)
}       

c_e_quintil <- numeric()

for(i in 1:5){
  c_e_quintil[i] <- c_quintil(x = i)
}     


# Sexo

c_sexo <- function(x) {
  x <- base_men_svy %>%
    filter(bc_pe2 == x & pea == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(desocup, na.rm=T))
  x <- mean(x$colname)
}       

c_e_sexo <- numeric()

for(i in 1:2){
  c_e_sexo[i] <- c_sexo(x = i)
}     


CODIND       <- 530102
DIMENSIÓN    <- "Accesibilidad"
CODINDICADOR <- "Tasa de desempleo"
NOMINDICADOR <- "Tasa de desempleo"
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

NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. Las estimaciones desde 2022 se calculan a partir de la muestra panel. Las variables de corte son incorporadas a partir del formulario de implantación."

t_03 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, JERARQUIA_CAT, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA, NOTA_INDICADOR)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Tasa de subempleo
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


# Total

c_ano <- base_men_svy %>%
  srvyr::filter(ocup == 1) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(bc_subocupado, na.rm=T))

c_ano <-mean(c_ano$colname)


# Ascendencia étnico racial
    

c_afro <- function(x) {
  x <- base_men_svy %>%
    filter(bd_e29_1 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_subocupado, na.rm=T))
  x <- mean(x$colname)
}       

c_e_afro <- numeric()

for(i in 1:2){
  c_e_afro[i] <- c_afro(x = i)
}     


# Departamento

c_dpto <- function(x) {
  x <- base_men_svy %>%
    filter(dpto == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_subocupado, na.rm=T))
  x <- mean(x$colname)
}       

c_e_dpto <- numeric()

for(i in 1:19){
  c_e_dpto[i] <- c_dpto(x = i)
}     


# Quintil de ingresos

c_quintil <- function(x) {
  x <- base_men_svy %>%
    filter(quintilesy == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_subocupado, na.rm=T))
  x <- mean(x$colname)
}       

c_e_quintil <- numeric()

for(i in 1:5){
  c_e_quintil[i] <- c_quintil(x = i)
}     


# Sexo

c_sexo <- function(x) {
  x <- base_men_svy %>%
    filter(bc_pe2 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_subocupado, na.rm=T))
  x <- mean(x$colname)
}       

c_e_sexo <- numeric()


for(i in 1:2){
  c_e_sexo[i] <- c_sexo(x = i)
}         


CODIND       <- 530104
DIMENSIÓN    <- "Accesibilidad"
CODINDICADOR <- "Tasa de subempleo"
NOMINDICADOR <- "Tasa de subempleo"
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

NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. Las estimaciones desde 2022 se calculan a partir de la muestra panel. Las variables de corte son incorporadas a partir del formulario de implantación."

t_04 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, JERARQUIA_CAT, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA, NOTA_INDICADOR)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de ocupados que no aportan a la seguridad social
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Total

c_ano <- base_svy %>%
  srvyr::filter(ocup == 1) %>%
  srvyr::summarise(colname = srvyr::survey_mean(bc_register2, na.rm=T))

c_ano <-c_ano$colname

# Ascendencia étnico racial

c_afro <- function(x) {
  x <- base_svy %>%
    filter(bd_e29_1 == x & ocup == 1) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_register2, na.rm=T))
  x <- mean(x$colname)
}       

c_e_afro <- numeric()

for(i in 1:2){
  c_e_afro[i] <- c_afro(x = i)
}     


# Departamento

c_dpto <- function(x) {
  x <- base_svy %>%
    filter(dpto == x & ocup == 1) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_register2, na.rm=T))
  x <- mean(x$colname)
}       

c_e_dpto <- numeric()

for(i in 1:19){
  c_e_dpto[i] <- c_dpto(x = i)
}     


# Quintil de ingresos

c_quintil <- function(x) {
  x <- base_svy %>%
    filter(quintilesy == x & ocup == 1) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_register2, na.rm=T))
  x <- mean(x$colname)
}       

c_e_quintil <- numeric()

for(i in 1:5){
  c_e_quintil[i] <- c_quintil(x = i)
}     


# Sexo

c_sexo <- function(x) {
  x <- base_svy %>%
    filter(bc_pe2 == x & ocup == 1) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_register2, na.rm=T))
  x <- mean(x$colname)
}       

c_e_sexo <- numeric()

for(i in 1:2){
  c_e_sexo[i] <- c_sexo(x = i)
}     


CODIND       <- 530202
DIMENSIÓN    <- "Calidad"
CODINDICADOR <- "Porcentaje de ocupados que no aporta a la seguridad social"
NOMINDICADOR <- "Porcentaje de ocupados sin aporte a la seguridad social"
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

NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. Las estimaciones desde 2022 se calculan a partir de la muestra de implantación."

t_05 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, JERARQUIA_CAT, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA, NOTA_INDICADOR)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de ocupados que no aportan a la seguridad social por la totalidad del salario
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Total

c_ano <- base_svy %>%
  srvyr::filter(ocup == 1) %>%
  srvyr::summarise(colname = srvyr::survey_mean(noaportatot, na.rm=T))

c_ano <-c_ano$colname


# Ascendencia étnico racial

c_afro <- function(x) {
  x <- base_svy %>%
    filter(bd_e29_1 == x & ocup == 1) %>%
    srvyr::summarise(colname = srvyr::survey_mean(noaportatot, na.rm=T))
  x <- mean(x$colname)
}       

c_e_afro <- numeric()

for(i in 1:2){
  c_e_afro[i] <- c_afro(x = i)
}     


# Departamento

c_dpto <- function(x) {
  x <- base_svy %>%
    filter(dpto == x & ocup == 1) %>%
    srvyr::summarise(colname = srvyr::survey_mean(noaportatot, na.rm=T))
  x <- mean(x$colname)
}       

c_e_dpto <- numeric()

for(i in 1:19){
  c_e_dpto[i] <- c_dpto(x = i)
}     


# Quintil de ingresos

c_quintil <- function(x) {
  x <- base_svy %>%
    filter(quintilesy == x & ocup == 1) %>%
    srvyr::summarise(colname = srvyr::survey_mean(noaportatot, na.rm=T))
  x <- mean(x$colname)
}       

c_e_quintil <- numeric()

for(i in 1:5){
  c_e_quintil[i] <- c_quintil(x = i)
}     


# Sexo

c_sexo <- function(x) {
  x <- base_svy %>%
    filter(bc_pe2 == x & ocup == 1) %>%
    srvyr::summarise(colname = srvyr::survey_mean(noaportatot, na.rm=T))
  x <- mean(x$colname)
}       

c_e_sexo <- numeric()

for(i in 1:2){
  c_e_sexo[i] <- c_sexo(x = i)
}     


CODIND       <- 530203
DIMENSIÓN    <- "Calidad"
CODINDICADOR <- "Porcentaje de personas ocupadas que no aporta a la seguridad social por la totalidad del salario"
NOMINDICADOR <- "Porcentaje de personas ocupadas que no aporta a la seguridad social por la totalidad del salario"
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

NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. Las estimaciones desde 2022 se calculan a partir de la muestra de implantación."

t_06 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, JERARQUIA_CAT, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA, NOTA_INDICADOR)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de ocupados con restricciones al empleo
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Total

c_ano <- base_svy %>%
  srvyr::filter(ocup == 1) %>%
  srvyr::summarise(colname = srvyr::survey_mean(restric, na.rm=T))

c_ano <-c_ano$colname


# Ascendencia étnico racial

c_afro <- function(x) {
  x <- base_svy %>%
    filter(bd_e29_1 == x & ocup == 1) %>%
    
    srvyr::summarise(colname = srvyr::survey_mean(restric, na.rm=T))
  x <- mean(x$colname)
}       

c_e_afro <- numeric()

for(i in 1:2){
  c_e_afro[i] <- c_afro(x = i)
}     


# Departamento

c_dpto <- function(x) {
  x <- base_svy %>%
    filter(dpto == x & ocup == 1) %>%
    
    srvyr::summarise(colname = srvyr::survey_mean(restric, na.rm=T))
  x <- mean(x$colname)
}       

c_e_dpto <- numeric()

for(i in 1:19){
  c_e_dpto[i] <- c_dpto(x = i)
}     


# Quintil de ingresos

c_quintil <- function(x) {
  x <- base_svy %>%
    filter(quintilesy == x & ocup == 1) %>%
    
    srvyr::summarise(colname = srvyr::survey_mean(restric, na.rm=T))
  x <- mean(x$colname)
}       

c_e_quintil <- numeric()

for(i in 1:5){
  c_e_quintil[i] <- c_quintil(x = i)
}     


# Sexo

c_sexo <- function(x) {
  x <- base_svy %>%
    filter(bc_pe2 == x & ocup == 1) %>%
    
    srvyr::summarise(colname = srvyr::survey_mean(restric, na.rm=T))
  x <- mean(x$colname)
}       

c_e_sexo <- numeric()

for(i in 1:2){
  c_e_sexo[i] <- c_sexo(x = i)
}     


CODIND       <- 530204
DIMENSIÓN    <- "Calidad"
CODINDICADOR <- "Porcentaje de ocupados con restricciones al empleo"
NOMINDICADOR <- "Porcentaje de ocupados con restricciones al empleo"
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

NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. Las estimaciones desde 2022 se calculan a partir de la muestra de implantación."

t_07 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, JERARQUIA_CAT, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA, NOTA_INDICADOR)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Promedio de horas trabajadas de forma remunerada
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


# Total

c_ano <- base_men_svy %>%
  srvyr::filter(ocup == 1) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(bc_horas, na.rm=T))

c_ano <-mean(c_ano$colname)

# Ascendencia étnico racial
  

c_afro <- function(x) {
  x <- base_men_svy %>%
    filter(bd_e29_1 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_horas, na.rm=T))
  x <- mean(x$colname)
}       

c_e_afro <- numeric()

for(i in 1:2){
  c_e_afro[i] <- c_afro(x = i)
}     


# Departamento

c_dpto <- function(x) {
  x <- base_men_svy %>%
    filter(dpto == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_horas, na.rm=T))
  x <- mean(x$colname)
}       

c_e_dpto <- numeric()

for(i in 1:19){
  c_e_dpto[i] <- c_dpto(x = i)
}     

# Quintil de ingresos

c_quintil <- function(x) {
  x <- base_men_svy %>%
    filter(quintilesy == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_horas, na.rm=T))
  x <- mean(x$colname)
}       

c_e_quintil <- numeric()

for(i in 1:5){
  c_e_quintil[i] <- c_quintil(x = i)
}     

# Sexo
 

c_sexo <- function(x) {
  x <- base_men_svy %>%
    filter(bc_pe2 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_horas, na.rm=T))
  x <- mean(x$colname)
}       

c_e_sexo <- numeric()

for(i in 1:2){
  c_e_sexo[i] <- c_sexo(x = i)
}     


CODIND       <- 530201
DIMENSIÓN    <- "Calidad"
CODINDICADOR <- "Promedio de horas remuneradas"
NOMINDICADOR <- "Promedio de horas trabajadas de forma remunerada"
VALOR	       <- c(c_ano, c_e_afro, c_e_dpto, c_e_quintil, c_e_sexo)
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

NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. Las estimaciones desde 2022 se calculan a partir de la muestra panel. Las variables de corte son incorporadas a partir del formulario de implantación."

t_08 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, JERARQUIA_CAT, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA, NOTA_INDICADOR)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de ocupados que trabajan más de 48 horas
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Total

c_ano <- base_men_svy %>%
  srvyr::filter(ocup == 1) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(sobrecarga, na.rm=T))

c_ano <-mean(c_ano$colname)

# Ascendencia étnico racial

c_afro <- function(x) {
  x <- base_men_svy %>%
    filter(bd_e29_1 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(sobrecarga, na.rm=T))
  x <- mean(x$colname)
}       

c_e_afro <- numeric()

for(i in 1:2){
  c_e_afro[i] <- c_afro(x = i)
}     


# Departamento


c_dpto <- function(x) {
  x <- base_men_svy %>%
    filter(dpto == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(sobrecarga, na.rm=T))
  x <- mean(x$colname)
}       

c_e_dpto <- numeric()

for(i in 1:19){
  c_e_dpto[i] <- c_dpto(x = i)
}     



# Quintil de ingresos

c_quintil <- function(x) {
  x <- base_men_svy %>%
    filter(quintilesy == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(sobrecarga, na.rm=T))
  x <- mean(x$colname)
}       

c_e_quintil <- numeric()

for(i in 1:5){
  c_e_quintil[i] <- c_quintil(x = i)
}     


# Sexo

c_sexo <- function(x) {
  x <- base_men_svy %>%
    filter(bc_pe2 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(sobrecarga, na.rm=T))
  x <- mean(x$colname)
}       

c_e_sexo <- numeric()

for(i in 1:2){
  c_e_sexo[i] <- c_sexo(x = i)
}     



CODIND       <- 530208
DIMENSIÓN    <- "Calidad"
CODINDICADOR <- "Trabajo excesivo"
NOMINDICADOR <- "Porcentaje de ocupados que trabajan más de 48 horas semanales"
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

NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. Las estimaciones desde 2022 se calculan a partir de la muestra panel. Las variables de corte son incorporadas a partir del formulario de implantación."

t_09 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, JERARQUIA_CAT, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA, NOTA_INDICADOR)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Salario promedio por hora en ocupación principal
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Total

c_ano <- base_svy %>%
  srvyr::filter(ocup == 1 & f85>0) %>%
  srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))

c_ano <-c_ano$colname

# Ascendencia étnico racial

c_afro <- function(x) {
  x <- base_svy %>%
    filter(bd_e29_1 == x & ocup == 1 & f85>0) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))
  x <- mean(x$colname)
}       

c_e_afro <- numeric()

for(i in 1:2){
  c_e_afro[i] <- c_afro(x = i)
}     


# Departamento

c_dpto <- function(x) {
  x <- base_svy %>%
    filter(dpto == x & ocup == 1 & f85>0) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))
  x <- mean(x$colname)
}       

c_e_dpto <- numeric()

for(i in 1:19){
  c_e_dpto[i] <- c_dpto(x = i)
}     


# Quintil de ingresos

c_quintil <- function(x) {
  x <- base_svy %>%
    filter(quintilesy == x & ocup == 1 & f85>0) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))
  x <- mean(x$colname)
}       

c_e_quintil <- numeric()

for(i in 1:5){
  c_e_quintil[i] <- c_quintil(x = i)
}     

# Sexo

c_sexo <- function(x) {
  x <- base_svy %>%
    filter(bc_pe2 == x & ocup == 1 & f85>0) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))
  x <- mean(x$colname)
}       

c_e_sexo <- numeric()

for(i in 1:2){
  c_e_sexo[i] <- c_sexo(x = i)
}     

CODIND       <- 530205
DIMENSIÓN    <- "Calidad"
CODINDICADOR <- "Salario promedio por hora en ocupación principal"
NOMINDICADOR <- "Salario promedio por hora en ocupación principal"
VALOR	       <- c(c_ano, c_e_afro, c_e_dpto, c_e_quintil, c_e_sexo)
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

NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. Las estimaciones desde 2022 se calculan a partir de la muestra de implantación."

t_10 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, JERARQUIA_CAT, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA, NOTA_INDICADOR)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de personas ocupadas que perciben ingresos por debajo de la línea de pobreza
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


# Total

c_ano <- base_svy %>%
  srvyr::filter(ocup == 1 & bc_horas>40) %>%
  srvyr::summarise(colname = srvyr::survey_mean(salario_insuf, na.rm=T))

c_ano <-c_ano$colname

# Ascendencia étnico racial

c_afro <- function(x) {
  x <- base_svy %>%
    filter(bd_e29_1 == x & ocup == 1 &bc_horas>40) %>%
    srvyr::summarise(colname = srvyr::survey_mean(salario_insuf, na.rm=T))
  x <- mean(x$colname)
}       

c_e_afro <- numeric()

for(i in 1:2){
  c_e_afro[i] <- c_afro(x = i)
}     


# Departamento

c_dpto <- function(x) {
  x <- base_svy %>%
    filter(dpto == x & ocup == 1 & bc_horas>40) %>%
    srvyr::summarise(colname = srvyr::survey_mean(salario_insuf, na.rm=T))
  x <- mean(x$colname)
}       

c_e_dpto <- numeric()

for(i in 1:19){
  c_e_dpto[i] <- c_dpto(x = i)
}     


# Quintil de ingresos

c_quintil <- function(x) {
  x <- base_svy %>%
    filter(quintilesy == x & ocup == 1 & bc_horas>40) %>%
    srvyr::summarise(colname = srvyr::survey_mean(salario_insuf, na.rm=T))
  x <- mean(x$colname)
}       

c_e_quintil <- numeric()

for(i in 1:5){
  c_e_quintil[i] <- c_quintil(x = i)
}     


# Sexo

c_sexo <- function(x) {
  x <- base_svy %>%
    filter(bc_pe2 == x & ocup == 1 & bc_horas>40) %>%
    srvyr::summarise(colname = srvyr::survey_mean(salario_insuf, na.rm=T))
  x <- mean(x$colname)
}       

c_e_sexo <- numeric()

for(i in 1:2){
  c_e_sexo[i] <- c_sexo(x = i)
}     



CODIND       <- 530209
DIMENSIÓN    <- "Calidad"
CODINDICADOR <- "Ingresos insuficientes (debajo de línea de pobreza)"
NOMINDICADOR <- "Porcentaje de personas ocupadas a tiempo completo que perciben ingresos por debajo de la línea de pobreza"
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

NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. Las estimaciones desde 2022 se calculan a partir de la muestra de implantación."

t_11 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, JERARQUIA_CAT, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA, NOTA_INDICADOR)




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de personas ocupadas que perciben ingresos por debajo del Salario Mïnimo Nacional
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


# Total

c_ano <- base_svy %>%
  srvyr::filter(ocup == 1) %>%
  srvyr::summarise(colname = srvyr::survey_mean(salario_insuf_smn, na.rm=T))

c_ano <-c_ano$colname

# Ascendencia étnico racial

c_afro <- function(x) {
  x <- base_svy %>%
    filter(bd_e29_1 == x & ocup == 1) %>%
    srvyr::summarise(colname = srvyr::survey_mean(salario_insuf_smn, na.rm=T))
  x <- mean(x$colname)
}       

c_e_afro <- numeric()

for(i in 1:2){
  c_e_afro[i] <- c_afro(x = i)
}     


# Departamento

c_dpto <- function(x) {
  x <- base_svy %>%
    filter(dpto == x & ocup == 1) %>%
    srvyr::summarise(colname = srvyr::survey_mean(salario_insuf_smn, na.rm=T))
  x <- mean(x$colname)
}       

c_e_dpto <- numeric()

for(i in 1:19){
  c_e_dpto[i] <- c_dpto(x = i)
}     


# Quintil de ingresos

c_quintil <- function(x) {
  x <- base_svy %>%
    filter(quintilesy == x & ocup == 1 & bc_horas>40) %>%
    srvyr::summarise(colname = srvyr::survey_mean(salario_insuf_smn, na.rm=T))
  x <- mean(x$colname)
}       

c_e_quintil <- numeric()

for(i in 1:5){
  c_e_quintil[i] <- c_quintil(x = i)
}     

# Sexo

c_sexo <- function(x) {
  x <- base_svy %>%
    filter(bc_pe2 == x & ocup == 1) %>%
    srvyr::summarise(colname = srvyr::survey_mean(salario_insuf_smn, na.rm=T))
  x <- mean(x$colname)
}       

c_e_sexo <- numeric()

for(i in 1:2){
  c_e_sexo[i] <- c_sexo(x = i)
}     



CODIND       <- 530207
DIMENSIÓN    <- "Calidad"
CODINDICADOR <- "Ingresos insuficientes (inferiores al salario mínimo nacional)"
NOMINDICADOR <- "Porcentaje de ocupados con salario por debajo del Salario Mínimo Nacional"
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

NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. Las estimaciones desde 2022 se calculan a partir de la muestra de implantación."

t_12 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, JERARQUIA_CAT, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA, NOTA_INDICADOR)




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de ocupados que viven en hogares en situación de pobreza
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


# Total

c_ano <- base_svy %>%
  srvyr::filter(ocup == 1) %>%
  srvyr::summarise(colname = srvyr::survey_mean(pobre06, na.rm=T))

c_ano <-c_ano$colname

# Ascendencia étnico racial

c_afro <- function(x) {
  x <- base_svy %>%
    filter(bd_e29_1 == x & ocup == 1) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pobre06, na.rm=T))
  x <- mean(x$colname)
}       

c_e_afro <- numeric()

for(i in 1:2){
  c_e_afro[i] <- c_afro(x = i)
}     

# Departamento

c_dpto <- function(x) {
  x <- base_svy %>%
    filter(dpto == x & ocup == 1) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pobre06, na.rm=T))
  x <- mean(x$colname)
}       

c_e_dpto <- numeric()

for(i in 1:19){
  c_e_dpto[i] <- c_dpto(x = i)
}     


# Quintil de ingresos

c_quintil <- function(x) {
  x <- base_svy %>%
    filter(quintilesy == x & ocup == 1 & bc_horas>40) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pobre06, na.rm=T))
  x <- mean(x$colname)
}       

c_e_quintil <- numeric()

for(i in 1:5){
  c_e_quintil[i] <- c_quintil(x = i)
}     


# Sexo

c_sexo <- function(x) {
  x <- base_svy %>%
    filter(bc_pe2 == x & ocup == 1) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pobre06, na.rm=T))
  x <- mean(x$colname)
}       

c_e_sexo <- numeric()

for(i in 1:2){
  c_e_sexo[i] <- c_sexo(x = i)
}     

CODIND       <- 530206
DIMENSIÓN    <- "Calidad"
CODINDICADOR <- "Porcentaje de ocupados que viven en hogares en situación de pobreza"
NOMINDICADOR <- "Porcentaje de ocupados que viven en hogares en situación de pobreza"
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

NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. Las estimaciones desde 2022 se calculan a partir de la muestra de implantación."

t_13 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, JERARQUIA_CAT, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA, NOTA_INDICADOR)





# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
### Generación de Base motor ###
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #



t_2024 <- rbind(t_01, t_02, t_03, t_04, t_05, t_06, t_07, t_08, t_09, t_10, t_11, t_12, t_13)


rio::export(t_2024, "Tabulados/Tabulados 2024/t_2024.xlsx" )
