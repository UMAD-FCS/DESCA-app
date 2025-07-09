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
AÑO	         <- 2020
FUENTE	     <- "Encuesta Continua de Hogares 2020, INE"
RESPONSABLE	 <- "J. Pandolfi"
JERARQUIA_CAT <- 1

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

### Carga de bases ###

ech <- rio::import("C:/Users/Usuario/Dropbox/1. Unidad de Métodos y Acceso a Datos/1. Observatorio UMAD/PISO I_MICRODATOS/ECH/2020/Fusionada_personasyhogares_20.dta")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

### Generación de nuevas variables ###

# Salario mínimo nacional

smn <- 16300
smn_h = smn/200

#IPC

ech <- ech %>% dplyr::mutate(ipc_ene2022 = case_when(     mes == 1 ~ 0.864309245,
                                                          mes == 2 ~ 0.869605104,
                                                          mes == 3 ~ 0.881197615,
                                                          mes == 4 ~ 0.898794879,
                                                          mes == 5 ~ 0.90388224,
                                                          mes == 6 ~ 0.904049039,
                                                          mes == 7 ~ 0.909011301,
                                                          mes == 8 ~ 0.91422376,
                                                          mes == 9 ~ 0.920061716,
                                                          mes == 10 ~ 0.925399274,
                                                          mes == 11 ~ 0.928026354,
                                                          mes == 12 ~ 0.926274968))

ech <- ech %>% dplyr::mutate(bc_ipc_tot = case_when(      mes == 1 ~ 0.367126174,
                                                          mes == 2 ~ 0.359598378,
                                                          mes == 3 ~ 0.357408439,
                                                          mes == 4 ~ 0.352706586,
                                                          mes == 5 ~ 0.345801038,
                                                          mes == 6 ~ 0.343854751,
                                                          mes == 7 ~ 0.343791309,
                                                          mes == 8 ~ 0.341914564,
                                                          mes == 9 ~ 0.339965133,
                                                          mes == 10 ~ 0.337807994,
                                                          mes == 11 ~ 0.33585957,
                                                          mes == 12 ~ 0.334908811))


# Ingresos

ech <- ech %>% dplyr::mutate(y_pc       =  ht11 / ht19 )                      #Ingreso per-cápita
ech <- ech %>% dplyr::mutate(y_pc_svl   =  (ht11 - ht13) / ht19)              #Ingreso per-cápita sin valor locativo
ech <- ech %>% dplyr::mutate(y_pc_d     =  ht11 / ht19 / bc_ipc_tot)          #Ingreso per-cápita deflactado
ech <- ech %>% dplyr::mutate(y_pc_svl_d =  (ht11 - ht13) / ht19 / bc_ipc_tot) #Ingreso per-cápita sin valor locativo deflactado


# Quintil de ingresos

ech_h <- ech %>% distinct(numero, .keep_all = TRUE)
ech_h <- ech_h %>% dplyr::mutate(quintilesy = statar::xtile(y_pc, n=5, wt = pesomen))
ech_h <- ech_h[,c("numero","quintilesy")]
ech <- merge(ech, ech_h, by = "numero")


# Sexo

ech <- ech %>% dplyr::mutate(bc_pe2 = e26)


# Ascendencia afro

ech <- ech %>% dplyr::mutate(bd_e29_1 = e29_1)


# Población económicamente activa

ech <- ech %>% dplyr::mutate(pea = ifelse(pobpcoac==2 | pobpcoac==3 | pobpcoac==4 | pobpcoac==5, 1, 0))


# Ocupados

ech <- ech %>% dplyr::mutate(ocup = ifelse(pobpcoac==2, 1, 0))


# Desempleados

ech <- ech %>% dplyr::mutate(desocup = ifelse(pobpcoac==3 | pobpcoac==4 | pobpcoac==5, 1, 0))


# Horas remuneradas

ech <- ech  %>% dplyr::mutate(bc_horas = f85+f98)


# Sobrecarga de horas

ech <- ech  %>% dplyr::mutate(sobrecarga = ifelse(bc_horas>48, 1, 0))


# Subempleo

ech <- ech %>% dplyr::mutate(bc_subocupado = ifelse(f102 == 1 & f104 == 5 & bc_horas > 0 & bc_horas < 40, 1, 0))
                                                              

# No aporte a SS en trabajo principal y secundario

ech <- ech %>% dplyr::mutate(bc_register2 = ifelse(f96 == 1 | f82 == 1, 0, 1))


# No aporte a SS en trabajo principal

ech <- ech %>% dplyr::mutate(bc_register = ifelse(f82 == 1, 0, 1))


# No aporte por totalidad del salario

ech <- ech %>% dplyr::mutate(noaportatot = ifelse(f84 == 2, 1, 0))


# Restricciones al empleo

ech <- ech %>% dplyr::mutate(restric = ifelse(bc_register2 == 1 | bc_subocupado ==1, 1, 0))


# Salario en ocupación principal

ech <- ech %>% dplyr::mutate(horamen = f85 * 4.3)
ech <- ech %>% dplyr::mutate(yhora = pt2/horamen)
ech <- ech %>% dplyr::mutate(yhoradef = yhora/bc_ipc_tot)

# Línea de pobreza individual

ech <- ech %>% dplyr::mutate(regionlp = case_when(region_4 == 1 ~ 1,
                                                          region_4 == 2 | region_4 == 3 ~ 2,
                                                          region_4 == 4 ~ 3))


ech <- ech %>% dplyr::mutate(grupolp = case_when(regionlp == 1 & mes == 1 ~ 1,
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
                                                         regionlp == 3 & mes == 12 ~ 36
))

ech_unipersonales <- ech %>%  filter(ht19==1)
ech_unipersonales <- ech_unipersonales[,c("numero","grupolp", "lp_06")]
ech_unipersonales <- ech_unipersonales %>% dplyr::mutate(lp_unipersonales = lp_06)
ech_unipersonales <- ech_unipersonales[order(ech_unipersonales$grupolp, ech_unipersonales$lp_unipersonales, decreasing = TRUE), ]
ech_unipersonales <- ech_unipersonales %>% distinct(grupolp, .keep_all = TRUE)
ech_unipersonales <- ech_unipersonales[,c("grupolp","lp_unipersonales")]

ech <- merge(ech, ech_unipersonales, by = "grupolp")



# Salario por debajo de línea de pobreza

ech <- ech %>% dplyr::mutate(salario_insuf = ifelse(pt2 < lp_unipersonales, 1, 0))


# Salario por debajo del salario mínimo nacional

ech <- ech %>% dplyr::mutate(salario_insuf_smn = ifelse(yhora<smn_h, 1, 0))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

### Información de muestreo ### 

ech_svy           <- srvyr::as_survey_design(ech, ids = numero, weights = pesomen)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
### Procesamiento de indicadores ###
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Tasa de actividad
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Total

c_ano <- ech_svy %>%
  srvyr::filter(e27>=14) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(pea, na.rm=T))

c_ano <- mean(as.numeric(c_ano$colname))



# Ascendencia étnico racial

a_afro <- function(x) {
  x <- ech_svy %>%
    filter(bd_e29_1 == x & e27>=14) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pea, na.rm=T))
  x <- mean(x$colname)
}       

c_afro <- numeric()

for(i in 1:2){
  c_afro[i] <- a_afro(x = i)
}     


# Departamento

a_dpto <- function(x) {
  x <- ech_svy %>%
    filter(dpto == x & e27>14) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pea, na.rm=T))
  x <- mean(x$colname)
}       

c_dpto <- numeric()

for(i in 1:19){
  c_dpto[i] <- a_dpto(x = i)
}     


# Quintil de ingresos

a_quintil <- function(x) {
  x <- ech_svy %>%
    filter(quintilesy == x & e27>=14) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pea, na.rm=T))
  x <- mean(x$colname)
}       

c_quintil <- numeric()

for(i in 1:5){
  c_quintil[i] <- a_quintil(x = i)
}     


# Sexo

a_sexo <- function(x) {
  x <- ech_svy %>%
    filter(bc_pe2 == x & e27>=14) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pea, na.rm=T))
  x <- mean(x$colname)
}       

c_sexo <- numeric()

for(i in 1:2){
  c_sexo[i] <- a_sexo(x = i)
}     


CODIND       <- 530101
DIMENSIÓN    <- "Accesibilidad"
CODINDICADOR <- "Tasa de actividad"
NOMINDICADOR <- "Tasa de actividad"
VALOR	       <- c(c_ano, c_afro, c_dpto, c_quintil, c_sexo)
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

NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. Este indicador se construye con la encuesta presencial hasta marzo de 2020 y, posteriormente, con la encuesta telefónica panel."  

t_01 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, JERARQUIA_CAT, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA, NOTA_INDICADOR)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Tasa de empleo
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


# Total

b_mes <- ech_svy %>%
  srvyr::filter(e27>=14) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(ocup, na.rm=T))

c_ano <- mean(as.numeric(b_mes$colname))


# Ascendencia étnico racial

b_afro <- function(x) {
  x <- ech_svy %>%
    filter(bd_e29_1 == x & e27>=14) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(ocup, na.rm=T))
  x <- mean(x$colname)
}       

c_afro <- numeric()

for(i in 1:2){
  c_afro[i] <- b_afro(x = i)
}     

# Departamento


b_dpto <- function(x) {
  x <- ech_svy %>%
    filter(dpto == x & e27>=14) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(ocup, na.rm=T))
  x <- mean(x$colname)
}       

c_dpto <- numeric()

for(i in 1:19){
  c_dpto[i] <- b_dpto(x = i)
}     


# Quintil de ingresos

b_quintil <- function(x) {
  x <- ech_svy %>%
    filter(quintilesy == x & e27>=14) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(ocup, na.rm=T))
  x <- mean(x$colname)
}       

c_quintil <- numeric()

for(i in 1:5){
  c_quintil[i] <- b_quintil(x = i)
}     


# Sexo

b_sexo <- function(x) {
  x <- ech_svy %>%
    filter(bc_pe2 == x & e27>=14) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(ocup, na.rm=T))
  x <- mean(x$colname)
}       

c_sexo <- numeric()

for(i in 1:2){
  c_sexo[i] <- b_sexo(x = i)
}     

       


CODIND       <- 530103
DIMENSIÓN    <- "Accesibilidad"
CODINDICADOR <- "Tasa de empleo"
NOMINDICADOR <- "Tasa de empleo"
VALOR	       <- c(c_ano, c_afro, c_dpto, c_quintil, c_sexo)
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

NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. Este indicador se construye con la encuesta presencial hasta marzo de 2020 y, posteriormente, con la encuesta telefónica panel."  

t_02 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, JERARQUIA_CAT, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA, NOTA_INDICADOR)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Tasa de desempleo
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


# Total

c_ano <- ech_svy %>%
  srvyr::filter(pea == 1) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(desocup, na.rm=T))

c_ano <- mean(as.numeric(c_ano$colname))


# Ascendencia étnico racial



b_afro <- function(x) {
  x <- ech_svy %>%
    filter(bd_e29_1 == x & pea == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(desocup, na.rm=T))
  x <- mean(x$colname)
}       

c_afro <- numeric()

for(i in 1:2){
  c_afro[i] <- b_afro(x = i)
}     

# Departamento


b_dpto <- function(x) {
  x <- ech_svy %>%
    filter(dpto == x & pea == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(desocup, na.rm=T))
  x <- mean(x$colname)
}       

c_dpto <- numeric()

for(i in 1:19){
  c_dpto[i] <- b_dpto(x = i)
}     



# Quintil de ingresos


b_quintil <- function(x) {
  x <- ech_svy %>%
    filter(quintilesy == x & pea == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(desocup, na.rm=T))
  x <- mean(x$colname)
}       

c_quintil <- numeric()

for(i in 1:5){
  c_quintil[i] <- b_quintil(x = i)
}     


# Sexo



b_sexo <- function(x) {
  x <- ech_svy %>%
    filter(bc_pe2 == x & pea == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(desocup, na.rm=T))
  x <- mean(x$colname)
}       

c_sexo <- numeric()


for(i in 1:2){
  c_sexo[i] <- b_sexo(x = i)
}         



CODIND       <- 530102
DIMENSIÓN    <- "Accesibilidad"
CODINDICADOR <- "Tasa de desempleo"
NOMINDICADOR <- "Tasa de desempleo"
VALOR	       <- c(c_ano, c_afro, c_dpto, c_quintil, c_sexo)
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

NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. Este indicador se construye con la encuesta presencial hasta marzo de 2020 y, posteriormente, con la encuesta telefónica panel."  

t_03 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, JERARQUIA_CAT, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA, NOTA_INDICADOR)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Tasa de subempleo
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


# Total

c_ano <- ech_svy %>%
  srvyr::filter(ocup == 1) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(bc_subocupado, na.rm=T))

c_ano <- mean(as.numeric(c_ano$colname))

# Ascendencia étnico racial


b_afro <- function(x) {
  x <- ech_svy %>%
    filter(bd_e29_1 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_subocupado, na.rm=T))
  x <- mean(x$colname)
}       

c_afro <- numeric()

for(i in 1:2){
  c_afro[i] <- b_afro(x = i)
}     


# Departamento


b_dpto <- function(x) {
  x <- ech_svy %>%
    filter(dpto == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_subocupado, na.rm=T))
  x <- mean(x$colname)
}       

c_dpto <- numeric()

for(i in 1:19){
  c_dpto[i] <- b_dpto(x = i)
}     


# Quintil de ingresos


b_quintil <- function(x) {
  x <- ech_svy %>%
    filter(quintilesy == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_subocupado, na.rm=T))
  x <- mean(x$colname)
}       

c_quintil <- numeric()

for(i in 1:5){
  c_quintil[i] <- b_quintil(x = i)
}     


# Sexo


b_sexo <- function(x) {
  x <- ech_svy %>%
    filter(bc_pe2 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_subocupado, na.rm=T))
  x <- mean(x$colname)
}       

c_sexo <- numeric()

for(i in 1:2){
  c_sexo[i] <- b_sexo(x = i)
}         


CODIND       <- 530104
DIMENSIÓN    <- "Accesibilidad"
CODINDICADOR <- "Tasa de subempleo"
NOMINDICADOR <- "Tasa de subempleo"
VALOR	       <- c(c_ano, c_afro, c_dpto, c_quintil, c_sexo)
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

NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. Este indicador se construye con la encuesta presencial hasta marzo de 2020 y, posteriormente, con la encuesta telefónica panel."  

t_04 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, JERARQUIA_CAT, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA, NOTA_INDICADOR)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de ocupados que no aportan a la seguridad social
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Total

c_ano <- ech_svy %>%
  srvyr::filter(ocup == 1) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(bc_register2, na.rm=T))

c_ano <- mean(as.numeric(c_ano$colname))


# Ascendencia étnico racial



b_afro <- function(x) {
  x <- ech_svy %>%
    filter(bd_e29_1 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_register2, na.rm=T))
  x <- mean(x$colname)
}       

c_afro <- numeric()

for(i in 1:2){
  c_afro[i] <- b_afro(x = i)
}     


# Departamento


b_dpto <- function(x) {
  x <- ech_svy %>%
    filter(dpto == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_register2, na.rm=T))
  x <- mean(x$colname)
}       

c_dpto <- numeric()

for(i in 1:19){
  c_dpto[i] <- b_dpto(x = i)
}     



# Quintil de ingresos


b_quintil <- function(x) {
  x <- ech_svy %>%
    filter(quintilesy == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_register2, na.rm=T))
  x <- mean(x$colname)
}       

c_quintil <- numeric()

for(i in 1:5){
  c_quintil[i] <- b_quintil(x = i)
}     

# Sexo


b_sexo <- function(x) {
  x <- ech_svy %>%
    filter(bc_pe2 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_register2, na.rm=T))
  x <- mean(x$colname)
}       

c_sexo <- numeric()



for(i in 1:2){
  c_sexo[i] <- b_sexo(x = i)
}         


CODIND       <- 530202
DIMENSIÓN    <- "Calidad"
CODINDICADOR <- "Porcentaje de ocupados que no aporta a la seguridad social"
NOMINDICADOR <- "Porcentaje de ocupados sin aporte a la seguridad social"
VALOR	       <- c(c_ano, c_afro, c_dpto, c_quintil, c_sexo)
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

NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. Este indicador se construye con la encuesta presencial hasta marzo de 2020 y, posteriormente, con la encuesta telefónica panel."  

t_05 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, JERARQUIA_CAT, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA, NOTA_INDICADOR)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de ocupados que no aportan a la seguridad social por la totalidad del salario
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Total

c_ano <- ech_svy %>%
  srvyr::filter(ocup == 1) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(noaportatot, na.rm=T))

c_ano <- mean(as.numeric(c_ano$colname))


# Ascendencia étnico racial

a_afro <- function(x) {
  x <- ech_svy %>%
    filter(bd_e29_1 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(noaportatot, na.rm=T))
  x <- mean(x$colname)
}       

c_afro <- numeric()

for(i in 1:2){
  c_afro[i] <- a_afro(x = i)
}     



# Departamento

a_dpto <- function(x) {
  x <- ech_svy %>%
    filter(dpto == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(noaportatot, na.rm=T))
  x <- mean(x$colname)
}       

c_dpto <- numeric()

for(i in 1:19){
  c_dpto[i] <- a_dpto(x = i)
}     



# Quintil de ingresos

a_quintil <- function(x) {
  x <- ech_svy %>%
    filter(quintilesy == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(noaportatot, na.rm=T))
  x <- mean(x$colname)
}       

c_quintil <- numeric()

for(i in 1:5){
  c_quintil[i] <- a_quintil(x = i)
}     

  
# Sexo

a_sexo <- function(x) {
  x <- ech_svy %>%
    filter(bc_pe2 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(noaportatot, na.rm=T))
  x <- mean(x$colname)
}       

c_sexo <- numeric()

for(i in 1:2){
  c_sexo[i] <- a_sexo(x = i)
}     

    

CODIND       <- 530203
DIMENSIÓN    <- "Calidad"
CODINDICADOR <- "Porcentaje de personas ocupadas que no aporta a la seguridad social por la totalidad del salario"
NOMINDICADOR <- "Porcentaje de personas ocupadas que no aporta a la seguridad social por la totalidad del salario"
VALOR	       <- c(c_ano, c_afro, c_dpto, c_quintil, c_sexo)
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

NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. Este indicador se construye con la encuesta presencial hasta marzo de 2020 y, posteriormente, con la encuesta telefónica panel."  

t_06 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, JERARQUIA_CAT, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA, NOTA_INDICADOR)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de ocupados con restricciones al empleo
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Total

c_ano <- ech_svy %>%
  srvyr::filter(ocup == 1) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(restric, na.rm=T))

c_ano <- mean(as.numeric(c_ano$colname))


# Ascendencia étnico racial


b_afro <- function(x) {
  x <- ech_svy %>%
    filter(bd_e29_1 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(restric, na.rm=T))
  x <- mean(x$colname)
}       

c_afro <- numeric()

for(i in 1:2){
  c_afro[i] <- b_afro(x = i)
}     


# Departamento

b_dpto <- function(x) {
  x <- ech_svy %>%
    filter(dpto == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(restric, na.rm=T))
  x <- mean(x$colname)
}       

c_dpto <- numeric()

for(i in 1:19){
  c_dpto[i] <- b_dpto(x = i)
}     


# Quintil de ingresos


b_quintil <- function(x) {
  x <- ech_svy %>%
    filter(quintilesy == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(restric, na.rm=T))
  x <- mean(x$colname)
}       

c_quintil <- numeric()

for(i in 1:5){
  c_quintil[i] <- b_quintil(x = i)
}     


# Sexo


b_sexo <- function(x) {
  x <- ech_svy %>%
    filter(bc_pe2 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(restric, na.rm=T))
  x <- mean(x$colname)
}       

c_sexo <- numeric()

for(i in 1:2){
  c_sexo[i] <- b_sexo(x = i)
}         


CODIND       <- 530204
DIMENSIÓN    <- "Calidad"
CODINDICADOR <- "Porcentaje de ocupados con restricciones al empleo"
NOMINDICADOR <- "Porcentaje de ocupados con restricciones al empleo"
VALOR	       <- c(c_ano, c_afro, c_dpto, c_quintil, c_sexo)
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

NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. Este indicador se construye con la encuesta presencial hasta marzo de 2020 y, posteriormente, con la encuesta telefónica panel."  

t_07 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, JERARQUIA_CAT, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA, NOTA_INDICADOR)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Promedio de horas trabajadas de forma remunerada
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


# Total

c_ano <- ech_svy %>%
  srvyr::filter(ocup == 1) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(bc_horas, na.rm=T))

c_ano <- mean(as.numeric(c_ano$colname))


# Ascendencia étnico racial

b_afro <- function(x) {
  x <- ech_svy %>%
    filter(bd_e29_1 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_horas, na.rm=T))
  x <- mean(x$colname)
}       

c_afro <- numeric()

for(i in 1:2){
  c_afro[i] <- b_afro(x = i)
}     


# Departamento


b_dpto <- function(x) {
  x <- ech_svy %>%
    filter(dpto == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_horas, na.rm=T))
  x <- mean(x$colname)
}       

c_dpto <- numeric()

for(i in 1:19){
  c_dpto[i] <- b_dpto(x = i)
}     



# Quintil de ingresos


b_quintil <- function(x) {
  x <- ech_svy %>%
    filter(quintilesy == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_horas, na.rm=T))
  x <- mean(x$colname)
}       

c_quintil <- numeric()

for(i in 1:5){
  c_quintil[i] <- b_quintil(x = i)
}     


# Sexo

b_sexo <- function(x) {
  x <- ech_svy %>%
    filter(bc_pe2 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_horas, na.rm=T))
  x <- mean(x$colname)
}       

c_sexo <- numeric()

for(i in 1:2){
  c_sexo[i] <- b_sexo(x = i)
}     


CODIND       <- 530201
DIMENSIÓN    <- "Calidad"
CODINDICADOR <- "Promedio de horas remuneradas"
NOMINDICADOR <- "Promedio de horas trabajadas de forma remunerada"
VALOR	       <- c(c_ano, c_afro, c_dpto, c_quintil, c_sexo)
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

NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. Este indicador se construye con la encuesta presencial hasta marzo de 2020 y, posteriormente, con la encuesta telefónica panel."  

t_08 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, JERARQUIA_CAT, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA, NOTA_INDICADOR)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de ocupados que trabajan más de 48 horas
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Total

c_ano <- ech_svy %>%
  srvyr::filter(ocup == 1) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(sobrecarga, na.rm=T))

c_ano <- mean(as.numeric(c_ano$colname))


# Ascendencia étnico racial

 

b_afro <- function(x) {
  x <- ech_svy %>%
    filter(bd_e29_1 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(sobrecarga, na.rm=T))
  x <- mean(x$colname)
}       

c_afro <- numeric()

for(i in 1:2){
  c_afro[i] <- b_afro(x = i)
}     

# Departamento

  

b_dpto <- function(x) {
  x <- ech_svy %>%
    filter(dpto == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(sobrecarga, na.rm=T))
  x <- mean(x$colname)
}       

c_dpto <- numeric()

for(i in 1:19){
  c_dpto[i] <- b_dpto(x = i)
}     


# Quintil de ingresos

b_quintil <- function(x) {
  x <- ech_svy %>%
    filter(quintilesy == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(sobrecarga, na.rm=T))
  x <- mean(x$colname)
}       

c_quintil <- numeric()

for(i in 1:5){
  c_quintil[i] <- b_quintil(x = i)
}     


# Sexo

b_sexo <- function(x) {
  x <- ech_svy %>%
    filter(bc_pe2 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(sobrecarga, na.rm=T))
  x <- mean(x$colname)
}       

c_sexo <- numeric()

for(i in 1:2){
  c_sexo[i] <- b_sexo(x = i)
}     


CODIND       <- 530208
DIMENSIÓN    <- "Calidad"
CODINDICADOR <- "Trabajo excesivo"
NOMINDICADOR <- "Porcentaje de ocupados que trabajan más de 48 horas semanales"
VALOR	       <- c(c_ano, c_afro, c_dpto, c_quintil, c_sexo)
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

NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. Este indicador se construye con la encuesta presencial hasta marzo de 2020 y, posteriormente, con la encuesta telefónica panel."  

t_09 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, JERARQUIA_CAT, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA, NOTA_INDICADOR)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Salario promedio por hora en ocupación principal
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Total

c_ano <- ech_svy %>%
  srvyr::filter(ocup == 1 & f85>0) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))

c_ano <- mean(as.numeric(c_ano$colname))


# Ascendencia étnico racial

a_afro <- function(x) {
  x <- ech_svy %>%
    filter(bd_e29_1 == x & ocup == 1 & f85>0) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))
  x <- mean(x$colname)
}       

c_afro <- numeric()

for(i in 1:2){
  c_afro[i] <- a_afro(x = i)
}     

# Departamento

a_dpto <- function(x) {
  x <- ech_svy %>%
    filter(dpto == x & ocup == 1 & f85>0) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))
  x <- mean(x$colname)
}       

c_dpto <- numeric()

for(i in 1:19){
  c_dpto[i] <- a_dpto(x = i)
}     

  
# Quintil de ingresos

a_quintil <- function(x) {
  x <- ech_svy %>%
    filter(quintilesy == x & ocup == 1 & f85>0) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))
  x <- mean(x$colname)
}       

c_quintil <- numeric()

for(i in 1:5){
  c_quintil[i] <- a_quintil(x = i)
}     
    

# Sexo

a_sexo <- function(x) {
  x <- ech_svy %>%
    filter(bc_pe2 == x & ocup == 1 & f85>0) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))
  x <- mean(x$colname)
}       

c_sexo <- numeric()

for(i in 1:2){
  c_sexo[i] <- a_sexo(x = i)
}     

      
CODIND       <- 530205
DIMENSIÓN    <- "Calidad"
CODINDICADOR <- "Salario promedio por hora en ocupación principal"
NOMINDICADOR <- "Salario promedio por hora en ocupación principal"
VALOR	       <- c(c_ano, c_afro, c_dpto, c_quintil, c_sexo)
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

NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. Este indicador se construye con la encuesta presencial hasta marzo de 2020 y, posteriormente, con la encuesta telefónica panel."  

t_10 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, JERARQUIA_CAT, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA, NOTA_INDICADOR)




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de personas ocupadas que perciben ingresos por debajo de la línea de pobreza
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


# Total

c_ano <- ech_svy %>%
  srvyr::filter(ocup == 1 & bc_horas>40) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(salario_insuf, na.rm=T))

c_ano <- mean(as.numeric(c_ano$colname))


# Ascendencia étnico racial

a_afro <- function(x) {
  x <- ech_svy %>%
    filter(bd_e29_1 == x & ocup == 1 & bc_horas>40) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(salario_insuf, na.rm=T))
  x <- mean(x$colname)
}       

c_afro <- numeric()

for(i in 1:2){
  c_afro[i] <- a_afro(x = i)
}     

# Departamento

a_dpto <- function(x) {
  x <- ech_svy %>%
    filter(dpto == x & ocup == 1 & bc_horas>40) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(salario_insuf, na.rm=T))
  x <- mean(x$colname)
}       

c_dpto <- numeric()

for(i in 1:19){
  c_dpto[i] <- a_dpto(x = i)
}     

   

# Quintil de ingresos

a_quintil <- function(x) {
  x <- ech_svy %>%
    filter(quintilesy == x & ocup == 1 & bc_horas>40) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(salario_insuf, na.rm=T))
  x <- mean(x$colname)
}       

c_quintil <- numeric()

for(i in 1:5){
  c_quintil[i] <- a_quintil(x = i)
}     


# Sexo

a_sexo <- function(x) {
  x <- ech_svy %>%
    filter(bc_pe2 == x & ocup == 1 & bc_horas>40) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(salario_insuf, na.rm=T))
  x <- mean(x$colname)
}       

c_sexo <- numeric()

for(i in 1:2){
  c_sexo[i] <- a_sexo(x = i)
}     

   
  
CODIND       <- 530209
DIMENSIÓN    <- "Calidad"
CODINDICADOR <- "Ingresos insuficientes (debajo de línea de pobreza)"
NOMINDICADOR <- "Porcentaje de personas ocupadas a tiempo completo que perciben ingresos por debajo de la línea de pobreza"
VALOR	       <- c(c_ano, c_afro, c_dpto, c_quintil, c_sexo)
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

NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. Este indicador se construye con la encuesta presencial hasta marzo de 2020 y, posteriormente, con la encuesta telefónica panel."  

t_11 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, JERARQUIA_CAT, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA, NOTA_INDICADOR)




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de personas ocupadas que perciben ingresos por debajo del Salario Mïnimo Nacional
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


# Total

c_ano <- ech_svy %>%
  srvyr::filter(ocup == 1) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(salario_insuf_smn, na.rm=T))

c_ano <- mean(as.numeric(c_ano$colname))


# Ascendencia étnico racial

a_afro <- function(x) {
  x <- ech_svy %>%
    filter(bd_e29_1 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(salario_insuf_smn, na.rm=T))
  x <- mean(x$colname)
}       

c_afro <- numeric()

for(i in 1:2){
  c_afro[i] <- a_afro(x = i)
}     


# Departamento

a_dpto <- function(x) {
  x <- ech_svy %>%
    filter(dpto == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(salario_insuf_smn, na.rm=T))
  x <- mean(x$colname)
}       

c_dpto <- numeric()

for(i in 1:19){
  c_dpto[i] <- a_dpto(x = i)
}     

    

# Quintil de ingresos

a_quintil <- function(x) {
  x <- ech_svy %>%
    filter(quintilesy == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(salario_insuf_smn, na.rm=T))
  x <- mean(x$colname)
}       

c_quintil <- numeric()

for(i in 1:5){
  c_quintil[i] <- a_quintil(x = i)
}     

   
# Sexo

a_sexo <- function(x) {
  x <- ech_svy %>%
    filter(bc_pe2 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(salario_insuf_smn, na.rm=T))
  x <- mean(x$colname)
}       

c_sexo <- numeric()

for(i in 1:2){
  c_sexo[i] <- a_sexo(x = i)
}     


CODIND       <- 530207
DIMENSIÓN    <- "Calidad"
CODINDICADOR <- "Ingresos insuficientes (inferiores al salario mínimo nacional)"
NOMINDICADOR <- "Porcentaje de ocupados con salario por debajo del Salario Mínimo Nacional"
VALOR	       <- c(c_ano, c_afro, c_dpto, c_quintil, c_sexo)
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

NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. Este indicador se construye con la encuesta presencial hasta marzo de 2020 y, posteriormente, con la encuesta telefónica panel."  

t_12 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, JERARQUIA_CAT, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA, NOTA_INDICADOR)




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de ocupados que viven en hogares en situación de pobreza
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


# Total

c_ano <- ech_svy %>%
  srvyr::filter(ocup == 1) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(pobre_06, na.rm=T))

c_ano <- mean(as.numeric(c_ano$colname))


# Ascendencia étnico racial

a_afro <- function(x) {
  x <- ech_svy %>%
    filter(bd_e29_1 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pobre_06, na.rm=T))
  x <- mean(x$colname)
}       

c_afro <- numeric()

for(i in 1:2){
  c_afro[i] <- a_afro(x = i)
}     


# Departamento

a_dpto <- function(x) {
  x <- ech_svy %>%
    filter(dpto == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pobre_06, na.rm=T))
  x <- mean(x$colname)
}       

c_dpto <- numeric()

for(i in 1:19){
  c_dpto[i] <- a_dpto(x = i)
}     



# Quintil de ingresos

a_quintil <- function(x) {
  x <- ech_svy %>%
    filter(quintilesy == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pobre_06, na.rm=T))
  x <- mean(x$colname)
}       

c_quintil <- numeric()

for(i in 1:5){
  c_quintil[i] <- a_quintil(x = i)
}     


# Sexo

a_sexo <- function(x) {
  x <- ech_svy %>%
    filter(bc_pe2 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pobre_06, na.rm=T))
  x <- mean(x$colname)
}       

c_sexo <- numeric()

for(i in 1:2){
  c_sexo[i] <- a_sexo(x = i)
}     


CODIND       <- 530206
DIMENSIÓN    <- "Calidad"
CODINDICADOR <- "Porcentaje de ocupados que viven en hogares en situación de pobreza"
NOMINDICADOR <- "Porcentaje de ocupados que viven en hogares en situación de pobreza"
VALOR	       <- c(c_ano, c_afro, c_dpto, c_quintil, c_sexo)
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

NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. Este indicador se construye con la encuesta presencial hasta marzo de 2020 y, posteriormente, con la encuesta telefónica panel."  

t_13 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, JERARQUIA_CAT, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA, NOTA_INDICADOR)




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
### Generación de Base motor ###
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #



t_2020 <- rbind(t_01, t_02, t_03, t_04, t_05, t_06, t_07, t_08, t_09, t_10, t_11, t_12, t_13)

rio::export(t_2020, "t_2020.xlsx" )


