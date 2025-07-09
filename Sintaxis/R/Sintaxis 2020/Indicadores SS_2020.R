### Mirador DESCA
### Derecho a la Seguridad Social
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


DERECHO      <- "Seguridad Social"
TIPOIND      <- "Resultados"
AÑO	         <- 2020
FUENTE	     <- "Encuesta Continua de Hogares 2020, INE"
RESPONSABLE	 <- "J. Pandolfi"

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


### Carga de bases ###

ech2020 <- rio::import("C:/Users/Usuario/Dropbox/1. Unidad de Métodos y Acceso a Datos/1. Observatorio UMAD/PISO I_MICRODATOS/ECH/2020/Fusionada_personasyhogares_20.dta")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

### Generación de nuevas variables ###

# Salario mínimo nacional enero 2022

smn <- 19364

#IPC

ech2020 <- ech2020 %>% dplyr::mutate(ipc_ene2022 = case_when(    mes == 1 ~ 0.864309245,
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




# Ingresos

ech2020 <- ech2020 %>% dplyr::mutate(y_pc       =  ht11 / ht19 )                      #Ingreso per-cápita
ech2020 <- ech2020 %>% dplyr::mutate(y_pc_svl   =  (ht11 - ht13) / ht19)              #Ingreso per-cápita sin valor locativo
#ech2020 <- sem2_implant %>% dplyr::mutate(y_pc_d     =  ht11 / ht19 / bc_ipc_tot)          #Ingreso per-cápita deflactado
#ech2020 <- sem2_implant %>% dplyr::mutate(y_pc_svl_d =  (ht11 - ht13) / ht19 / bc_ipc_tot) #Ingreso per-cápita sin valor locativo deflactado


# Quintil de ingresos

ech2020_h <- ech2020 %>% distinct(numero, .keep_all = TRUE)
ech2020_h <- ech2020_h %>% dplyr::mutate(quintilesy = statar::xtile(y_pc, n=5, wt = pesomen))
ech2020_h <- ech2020_h[,c("numero","quintilesy")]
ech2020 <- merge(ech2020, ech2020_h, by = "numero")


# Sexo
ech2020 <- ech2020 %>% dplyr::mutate(bc_pe2 = e26)

# Ascendencia afro
ech2020 <- ech2020 %>% dplyr::mutate(bd_e29_1 = e29_1)


# Desempleados sin Seguro de Desempleo

ech2020 <- ech2020 %>% dplyr::mutate(seguroparo = ifelse(pobpcoac == 4, 1, 0))

# Percepción de jubilaciones


ech2020 <- ech2020 %>% dplyr::mutate(jub = ifelse(g148_1_1 > 0 |
                                              g148_1_2 > 0 |
                                              g148_1_3 > 0 |
                                              g148_1_5 > 0 |
                                              g148_1_6 > 0 |
                                              g148_1_7 > 0 |
                                              g148_1_8 > 0 |
                                              g148_1_9 > 0 |
                                              g148_1_10 > 0 |
                                              g148_1_11 > 0, 1, 0))


# Percepción de pensiones


ech2020 <- ech2020 %>% dplyr::mutate(pens = ifelse(g148_2_1 > 0 |
                                                              g148_2_2 > 0 |
                                                              g148_2_3 > 0 |
                                                              g148_2_5 > 0 |
                                                              g148_2_6 > 0 |
                                                              g148_2_7 > 0 |
                                                              g148_2_8 > 0 |
                                                              g148_2_9 > 0 |
                                                              g148_2_10 > 0 |
                                                              g148_2_11 > 0, 1, 0)) # No está la variable g148_2_4 "UNIÓN POSTAL"

# Percepción de jubilaciones o pensiones

ech2020 <- ech2020 %>% dplyr::mutate(nijubpens = ifelse(jub == 1 | pens == 1, 0, 1))


# No aporte a SS en trabajo principal y secundario

ech2020 <- ech2020 %>% dplyr::mutate(bc_register2 = ifelse(f96 == 1 | f82 == 1, 0, 1))

# No aporte a SS en trabajo principal

ech2020 <- ech2020 %>% dplyr::mutate(bc_register = ifelse(f82 == 1, 0, 1))

# No aporte por totalidad del salario

ech2020 <- ech2020 %>% dplyr::mutate(noaportatot = ifelse(f84 == 2, 1, 0))


# No percepción de transferencias


ech2020 <- ech2020 %>% dplyr::mutate(p_afam = ifelse(g150 == 1, 1, 0))
ech2020_afam <- ech2020[,c("numero","p_afam")]
ech2020_afam <- ech2020_afam %>% dplyr::mutate(h_afam = p_afam)
ech2020_afam <- ech2020_afam[order(ech2020_afam$numero, ech2020_afam$h_afam, decreasing = TRUE), ]
ech2020_afam <- ech2020_afam %>% distinct(numero, .keep_all = TRUE)
ech2020_afam <- ech2020_afam[,c("numero","h_afam")]
ech2020 <- merge(ech2020, ech2020_afam, by = "numero")

ech2020 <- ech2020 %>% dplyr::mutate(p_tus = ifelse(e560 == 1, 1, 0))

ech2020_tus <- ech2020[,c("numero","p_tus")]
ech2020_tus <- ech2020_tus %>% dplyr::mutate(h_tus = p_tus)
ech2020_tus <- ech2020_tus[order(ech2020_tus$numero, ech2020_tus$h_tus, decreasing = TRUE), ]
ech2020_tus <- ech2020_tus %>% distinct(numero, .keep_all = TRUE)
ech2020_tus <- ech2020_tus[,c("numero","h_tus")]

ech2020 <- merge(ech2020, ech2020_tus, by = "numero")


ech2020 <- ech2020 %>% dplyr::mutate(nopercibe = ifelse(h_afam == 0 & h_tus == 0, 1, 0))


# Desempleados de 1 a 6 meses

ech2020 <- ech2020 %>% dplyr::mutate(desemp1a6 = ifelse(f113 >= 4 & f113<=24, 1, 0))


# Línea de pobreza individual

ech2020 <- ech2020 %>% dplyr::mutate(regionlp = case_when(region_4 == 1 ~ 1,
                                                    region_4 == 2 | region_4 == 3 ~ 2,
                                                    region_4 == 4 ~ 3))


ech2020 <- ech2020 %>% dplyr::mutate(grupolp = case_when(regionlp == 1 & mes == 1 ~ 1,
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

ech2020_unipersonales <- ech2020 %>%  filter(ht19==1)
ech2020_unipersonales <- ech2020_unipersonales[,c("numero","grupolp", "lp_06")]
ech2020_unipersonales <- ech2020_unipersonales %>% dplyr::mutate(lp_unipersonales = lp_06)
ech2020_unipersonales <- ech2020_unipersonales[order(ech2020_unipersonales$grupolp, ech2020_unipersonales$lp_unipersonales, decreasing = TRUE), ]
ech2020_unipersonales <- ech2020_unipersonales %>% distinct(grupolp, .keep_all = TRUE)
ech2020_unipersonales <- ech2020_unipersonales[,c("grupolp","lp_unipersonales")]

ech2020 <- merge(ech2020, ech2020_unipersonales, by = "grupolp")


# Ingresos desempleo por debajo de línea de pobreza

ech2020 <- ech2020 %>% dplyr::mutate(desempinsuf = ifelse(g148_3 < lp_unipersonales, 1, 0))


# Ingresos desempleo por debajo de la mitad de la media y mediana del salario nacional

ech2020_coningresos <- ech2020 %>%  filter(pt4>0)
ech2020_coningresos_svy <- srvyr::as_survey_design(ech2020_coningresos, ids = numero, weights = pesomen)
ech2020_media <-  ech2020_coningresos_svy %>% srvyr::summarise(colname = srvyr::survey_mean(pt4))
ech2020_media <-  ech2020_media$colname

ech2020_mediana <-  ech2020_coningresos_svy %>% srvyr::summarise(colname = srvyr::survey_median(pt4))
ech2020_mediana <-  ech2020_mediana$colname

ech2020 <- ech2020 %>% dplyr::mutate(desempinsuf_media = ifelse(g148_3 < (ech2020_media/2), 1, 0))
ech2020 <- ech2020 %>% dplyr::mutate(desempinsuf_mediana = ifelse(g148_3 < (ech2020_mediana/2), 1, 0))



# Ingresos desempleo por debajo del salario mínimo nacional

ech2020 <- ech2020 %>% dplyr::mutate(subsidio_def = g148_3 /  ipc_ene2022)
ech2020 <- ech2020 %>% dplyr::mutate(desempinsuf_smn = ifelse(subsidio_def < smn, 1, 0))


# Ingresos por jubilaciones

ech2020 <- ech2020 %>% dplyr::mutate(y_jub = g148_1_1 + g148_1_2 + g148_1_3 + g148_1_5 + g148_1_6 + g148_1_7 + g148_1_8 + g148_1_9 + g148_1_12 + g148_1_10 + g148_1_11)


# Ingresos por jubilaciones por debajo de la línea de pobreza

ech2020 <- ech2020 %>% dplyr::mutate(jub_insuf = ifelse(y_jub < lp_unipersonales, 1, 0))


# Ingresos por jubilaciones por debajo de la media y mediana del salario nacional

ech2020 <- ech2020 %>% dplyr::mutate(jub_insuf_media = ifelse(y_jub < (ech2020_media/2), 1, 0))
ech2020 <- ech2020 %>% dplyr::mutate(jub_insuf_mediana = ifelse(y_jub < (ech2020_mediana/2), 1, 0))



# Ingresos por jubilaciones por debajo del salario mÃ­nimo nacional

ech2020 <- ech2020 %>% dplyr::mutate(y_jub_def = y_jub /  ipc_ene2022)

ech2020 <- ech2020 %>% dplyr::mutate(jub_insuf_smn = ifelse(y_jub_def < smn, 1, 0))


# Ingresos por pensiones


ech2020 <- ech2020 %>% dplyr::mutate(y_pens = g148_2_1 + g148_2_2 + g148_2_3 + g148_2_5 + g148_2_6 + g148_2_7 + g148_2_8 + g148_2_9 + g148_2_12 + g148_2_10 + g148_2_11)

# Ingresos por pensiones debajo de la línea de pobreza

ech2020 <- ech2020 %>% dplyr::mutate(pens_insuf = ifelse(y_pens < lp_unipersonales, 1, 0))

# Ingresos por pensiones debajo de la mitad de la media y la mediana del salario nacional

ech2020 <- ech2020 %>% dplyr::mutate(pens_insuf_media = ifelse(y_pens < (ech2020_media/2), 1, 0))
ech2020 <- ech2020 %>% dplyr::mutate(pens_insuf_mediana = ifelse(y_pens < (ech2020_mediana/2), 1, 0))

# Ingresos por pensiones debajo del SMN

ech2020 <- ech2020 %>% dplyr::mutate(y_pens_def = y_pens /  ipc_ene2022)
ech2020 <- ech2020 %>% dplyr::mutate(pens_insuf_smn = ifelse(y_pens_def < smn, 1, 0))


# Ingresos por transferencias

ech2020 <- ech2020 %>% dplyr::mutate(y_transf = e560_1_1 + e560_2_1 + g257)
ech2020 <-  ech2020 %>%
  group_by(numero) %>%
  mutate(y_transf_h = sum(y_transf)) %>% ungroup()

ech2020 <- ech2020 %>% dplyr::mutate(hpc_y_transf =y_transf_h/ht19)
ech2020 <- ech2020 %>% dplyr::mutate(trans_insuf= ifelse(hpc_y_transf < li_06, 1, 0))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

### Información de muestreo ### 

ech2020_svy           <- srvyr::as_survey_design(ech2020, ids = numero, weights = pesomen)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
### Procesamiento de indicadores ###
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de desempleados que no cobra seguro de desempleo
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


# Total

c_ano <- ech2020_svy %>%
  srvyr::filter(pobpcoac>=4 & pobpcoac<=5) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(seguroparo))

c_ano <- mean(as.numeric(c_ano$colname))


CODIND       <- 330101
DIMENSIÓN    <- "Accesibilidad"
CODINDICADOR <- "Desempleados que no perciben subsidio"
NOMINDICADOR <- "Porcentaje de desempleados que no cobra seguro de desempleo"
VALOR	       <- c_ano
RESPONSABLE	 <- "J. Pandolfi"
JERARQUIA	   <- 1
CORTE        <- "Total"
DEPARTAMENTO <- ""
QUINTIL	     <- ""
SEXO		     <- ""
ASCENDENCIA  <- ""

ss_01 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de mayores de 65 años que no perciben jubilaciones ni pensiones
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


# Total

c_ano <- ech2020_svy %>%
  srvyr::filter(e27>=65 & pobpcoac!=2) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(nijubpens))

c_ano <- mean(as.numeric(c_ano$colname))


# Ascendencia étnico racial

c_f_afro <- function(x) {
  x <- ech2020_svy %>%
    filter(bd_e29_1 == x & e27>=65 & pobpcoac!=2) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nijubpens))
  x <- mean(x$colname)
}       

c_afro <- numeric()

for(i in 1:2){
  c_afro[i] <- c_f_afro(x = i)
}     


# Departamento

c_f_dpto <- function(x) {
  x <- ech2020_svy %>%
    filter(dpto == x & e27>=65 & pobpcoac!=2) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nijubpens))
  x <- mean(x$colname)
}       

c_dpto <- numeric()

for(i in 1:19){
  c_dpto[i] <- c_f_dpto(x = i)
}     



# Quintil de ingresos

c_f_quintil <- function(x) {
  x <- ech2020_svy %>%
    filter(quintilesy == x & e27>=65 & pobpcoac!=2) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nijubpens))
  x <- mean(x$colname)
}       

c_quintil <- numeric()

for(i in 1:5){
  c_quintil[i] <- c_f_quintil(x = i)
}     


# Sexo

c_f_sexo <- function(x) {
  x <- ech2020_svy %>%
    filter(bc_pe2 == x & e27>=65 & pobpcoac!=2) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nijubpens))
  x <- mean(x$colname)
}       

c_sexo <- numeric()

for(i in 1:2){
  c_sexo[i] <- c_f_sexo(x = i)
}     



CODIND       <- 330102
DIMENSIÓN    <- "Accesibilidad"
CODINDICADOR <- "Mayores de 65 años sin pensión ni jubilación"
NOMINDICADOR <- "Porcentaje de mayores de 65 años que no perciben pensiones ni pensiones"
VALOR	       <- c(c_ano, c_afro, c_dpto, c_quintil, c_sexo)
RESPONSABLE	 <- "J. Pandolfi"
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

ss_02 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de la población económicamente activa que no aporta a la seguridad social
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Total

c_ano <- ech2020_svy %>%
  srvyr::filter(pobpcoac>=2 & pobpcoac<=5) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(bc_register2))

c_ano <- mean(as.numeric(c_ano$colname))


# Ascendencia étnico racial

a_afro <- function(x) {
  x <- ech2020_svy %>%
    filter(bd_e29_1 == x & pobpcoac>=2 & pobpcoac<=5) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_register2))
  x <- mean(x$colname)
}       

a_e_afro <- numeric()

for(i in 1:2){
  a_e_afro[i] <- a_afro(x = i)
}     
c_afro <- a_e_afro

# Departamento

a_dpto <- function(x) {
  x <- ech2020_svy %>%
    filter(dpto == x & pobpcoac>=2 & pobpcoac<=5) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_register2))
  x <- mean(x$colname)
}       

a_e_dpto <- numeric()

for(i in 1:19){
  a_e_dpto[i] <- a_dpto(x = i)
}     

c_dpto <- a_e_dpto


# Quintil de ingresos

a_quintil <- function(x) {
  x <- ech2020_svy %>%
    filter(quintilesy == x & pobpcoac>=2 & pobpcoac<=5) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_register2))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}     

c_quintil <- a_e_quintil


# Sexo

a_sexo <- function(x) {
  x <- ech2020_svy %>%
    filter(bc_pe2 == x & pobpcoac>=2 & pobpcoac<=5) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_register2))
  x <- mean(x$colname)
}       

a_e_sexo <- numeric()

for(i in 1:2){
  a_e_sexo[i] <- a_sexo(x = i)
}     

c_sexo <- a_e_sexo


CODIND       <- 330103
DIMENSIÓN    <- "Accesibilidad"
CODINDICADOR <- "Población activa que no aporta a la seguridad social"
NOMINDICADOR <- "Porcentaje de la población económicamente activa que no aporta a la seguridad social"
VALOR	       <- c(c_ano, c_afro, c_dpto, c_quintil, c_sexo)
RESPONSABLE	 <- "J. Pandolfi"
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

ss_03 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA)




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de ocupados que no aporta a la seguridad social
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Total

a_mes <- ech2020_svy %>%
  srvyr::filter(pobpcoac==2) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(bc_register2))

c_ano <- mean(as.numeric(a_mes$colname))


# Ascendencia étnico racial

a_afro <- function(x) {
  x <- ech2020_svy %>%
    filter(bd_e29_1 == x & pobpcoac==2) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_register2))
  x <- mean(x$colname)
}       

a_e_afro <- numeric()

for(i in 1:2){
  a_e_afro[i] <- a_afro(x = i)
}     
c_afro <- a_e_afro

# Departamento

a_dpto <- function(x) {
  x <- ech2020_svy %>%
    filter(dpto == x & pobpcoac==2) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_register2))
  x <- mean(x$colname)
}       

a_e_dpto <- numeric()

for(i in 1:19){
  a_e_dpto[i] <- a_dpto(x = i)
}     

c_dpto <- a_e_dpto



# Quintil de ingresos

a_quintil <- function(x) {
  x <- ech2020_svy %>%
    filter(quintilesy == x & pobpcoac==2) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_register2))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}     

c_quintil <- a_e_quintil


# Sexo

a_sexo <- function(x) {
  x <- ech2020_svy %>%
    filter(bc_pe2 == x & pobpcoac==2) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_register2))
  x <- mean(x$colname)
}       

a_e_sexo <- numeric()

for(i in 1:2){
  a_e_sexo[i] <- a_sexo(x = i)
}     

c_sexo <- a_e_sexo


CODIND       <- 330104
DIMENSIÓN    <- "Accesibilidad"
CODINDICADOR <- "Porcentaje de ocupados que no aporta a la seguridad social"
NOMINDICADOR <- "Porcentaje de ocupados que no aporta a la seguridad social"
VALOR	       <- c(c_ano, c_afro, c_dpto, c_quintil, c_sexo)
RESPONSABLE	 <- "J. Pandolfi"
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

ss_04 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de ocupados que no aporta a la seguridad social por la totalidad del salario
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Total

c_ano <- ech2020_svy %>%
  srvyr::filter(pobpcoac==2) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(noaportatot))

c_ano <- mean(as.numeric(c_ano$colname))


# Ascendencia étnico racial

c_f_afro <- function(x) {
  x <- ech2020_svy %>%
    filter(bd_e29_1 == x & pobpcoac==2) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(noaportatot))
  x <- mean(x$colname)
}       

c_afro <- numeric()

for(i in 1:2){
  c_afro[i] <- c_f_afro(x = i)
}     



# Departamento

c_f_dpto <- function(x) {
  x <- ech2020_svy %>%
    filter(dpto == x & pobpcoac==2) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(noaportatot))
  x <- mean(x$colname)
}       

c_dpto <- numeric()

for(i in 1:19){
  c_dpto[i] <- c_f_dpto(x = i)
}     


# Quintil de ingresos

c_f_quintil <- function(x) {
  x <- ech2020_svy %>%
    filter(quintilesy == x & pobpcoac==2) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(noaportatot))
  x <- mean(x$colname)
}       

c_quintil <- numeric()

for(i in 1:5){
  c_quintil[i] <- c_f_quintil(x = i)
}     


# Sexo

c_f_sexo <- function(x) {
  x <- ech2020_svy %>%
    filter(bc_pe2 == x & pobpcoac==2) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(noaportatot))
  x <- mean(x$colname)
}       

c_sexo <- numeric()

for(i in 1:2){
  c_sexo[i] <- c_f_sexo(x = i)
}     



CODIND       <- 330105
DIMENSIÓN    <- "Accesibilidad"
CODINDICADOR <- "Informalidad parcial"
NOMINDICADOR <- "Porcentaje de ocupados que no aporta a la seguridad social por la totalidad del salario"
VALOR	       <- c(c_ano, c_afro, c_dpto, c_quintil, c_sexo)
RESPONSABLE	 <- "J. Pandolfi"
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

ss_05 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de personas que residen en hogares pobres que no perciben transferencias (TUS o AFAM)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Total

c_ano <- ech2020_svy %>%
  srvyr::filter(pobre_06==1) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(nopercibe))

c_ano <- mean(as.numeric(c_ano$colname))


CODIND       <- 330106
DIMENSIÓN    <- "Accesibilidad"
CODINDICADOR <- "Personas pobres que no reciben transferencias monetarias"
NOMINDICADOR <- "Porcentaje de personas que residen en hogares pobres que no perciben transferencias (TUS o AFAM)"
VALOR	       <- c_ano
RESPONSABLE	 <- "J. Pandolfi"
JERARQUIA	   <- 1
CORTE        <- "Total"
DEPARTAMENTO <- ""
QUINTIL	     <- ""
SEXO		     <- ""
ASCENDENCIA  <- ""
ss_06 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de mayores de 60 años que no perciben jubilaciones ni pensiones
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Total

c_ano <- ech2020_svy %>%
  srvyr::filter(e27>=60 & pobpcoac!=2) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(nijubpens))

c_ano <- mean(as.numeric(c_ano$colname))


# Ascendencia étnico racial

c_f_afro <- function(x) {
  x <- ech2020_svy %>%
    filter(bd_e29_1 == x & E27>=60 & POBPCOAC!=2) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nijubpens))
  x <- mean(x$colname)
}       

c_afro <- numeric()

for(i in 1:2){
  c_afro[i] <- c_f_afro(x = i)
}     


# Departamento

c_f_dpto <- function(x) {
  x <- ech2020_svy %>%
    filter(dpto == x & E27>=60 & POBPCOAC!=2) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nijubpens))
  x <- mean(x$colname)
}       

c_dpto <- numeric()

for(i in 1:19){
  c_dpto[i] <- c_f_dpto(x = i)
}     


# Quintil de ingresos

c_f_quintil <- function(x) {
  x <- ech2020_svy %>%
    filter(quintilesy == x & E27>=60 & POBPCOAC!=2) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nijubpens))
  x <- mean(x$colname)
}       

c_quintil <- numeric()

for(i in 1:5){
  c_quintil[i] <- c_f_quintil(x = i)
}     


# Sexo

c_f_sexo <- function(x) {
  x <- ech2020_svy %>%
    filter(bc_pe2 == x & E27>=60 & POBPCOAC!=2) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nijubpens))
  x <- mean(x$colname)
}       

c_sexo <- numeric()

for(i in 1:2){
  c_sexo[i] <- c_f_sexo(x = i)
}     



CODIND       <- 330107
DIMENSIÓN    <- "Accesibilidad"
CODINDICADOR <- "Inactivos mayores de 60 años sin pensión ni jubilación"
NOMINDICADOR <- "Porcentaje de inactivos mayores de 60 años que no perciben jubilaciones ni pensiones"
VALOR	       <- c(c_ano, c_afro, c_dpto, c_quintil, c_sexo)
RESPONSABLE	 <- "J. Pandolfi"
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

ss_07 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de personas desempleadas durante uno a seis meses que cobra seguro de desempleo por debajo de la línea de pobreza
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Total

c_ano <- ech2020_svy %>%
  srvyr::filter(desemp1a6 ==1) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(desempinsuf))

c_ano <- mean(as.numeric(c_ano$colname))



CODIND       <- 330201
DIMENSIÓN    <- "Adecuación"
CODINDICADOR <- "Insuficiencia del subsidio por desempleo en relación a la línea de pobreza"
NOMINDICADOR <- "Porcentaje de personas desempleadas durante uno a seis meses que cobra seguro de desempleo por debajo de la línea de pobreza"
VALOR	       <- c_ano
RESPONSABLE	 <- "J. Pandolfi"
JERARQUIA	   <- 1
CORTE        <- "Total"
DEPARTAMENTO <- ""
QUINTIL	     <- ""
SEXO		     <- ""
ASCENDENCIA  <- ""
ss_08 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de personas desempleadas durante uno a seis meses que cobra seguro de desempleo por debajo de la mitad de la media del salario nacional
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Total

c_ano <- ech2020_svy %>%
  srvyr::filter(desemp1a6 ==1) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(desempinsuf_media))

c_ano <- mean(as.numeric(c_ano$colname))


CODIND       <- 330202
DERECHO      <- "Seguridad Social"
CODINDICADOR <- "Insuficiencia del subsidio por desempleo en relación al salario nacional (mitad del promedio)"
NOMINDICADOR <- "Porcentaje de personas desempleadas durante uno a seis meses que cobra seguro de desempleo por debajo de la mitad de la media del salario nacional"
VALOR	       <- c_ano
RESPONSABLE	 <- "J. Pandolfi"
JERARQUIA	   <- 1
CORTE        <- "Total"
DEPARTAMENTO <- ""
QUINTIL	     <- ""
SEXO		     <- ""
ASCENDENCIA  <- ""
ss_09 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de personas desempleadas durante uno a seis meses que cobra seguro de desempleo por debajo de la mitad de la mediana del salario nacional
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Total

c_ano <- ech2020_svy %>%
  srvyr::filter(desemp1a6 ==1) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(desempinsuf_mediana))

c_ano <- mean(as.numeric(c_ano$colname))


CODIND       <- 330203
DIMENSIÓN    <- "Adecuación"
CODINDICADOR <- "Insuficiencia del subsidio por desempleo en relación al salario nacional (mitad de la mediana)"
NOMINDICADOR <- "Porcentaje de personas desempleadas durante uno a seis meses que cobra seguro de desempleo por debajo de la mitad de la mediana del salario nacional"
VALOR	       <- c_ano
RESPONSABLE	 <- "J. Pandolfi"
JERARQUIA	   <- 1
CORTE        <- "Total"
DEPARTAMENTO <- ""
QUINTIL	     <- ""
SEXO		     <- ""
ASCENDENCIA  <- ""
ss_10 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de personas desempleadas durante uno a seis meses que cobra seguro de desempleo por debajo del SMN
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Total

c_ano <- ech2020_svy %>%
  srvyr::filter(desemp1a6 ==1) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(desempinsuf_smn))

c_ano <- mean(as.numeric(c_ano$colname))




CODIND       <- 330204
DIMENSIÓN    <- "Adecuación"
CODINDICADOR <- "Insuficiencia del subsidio por desempleo en relación al salario mínimo nacional"
NOMINDICADOR <- "Porcentaje de personas desempleadas durante uno a seis meses que cobra seguro de desempleo por debajo del salario mínimo nacional"
VALOR	       <- c_ano
RESPONSABLE	 <- "J. Pandolfi"
JERARQUIA	   <- 1
CORTE        <- "Total"
DEPARTAMENTO <- ""
QUINTIL	     <- ""
SEXO		     <- ""
ASCENDENCIA  <- ""
ss_11 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de personas que perciben jubilaciones por debajo de la línea de pobreza
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Total

c_ano <- ech2020_svy %>%
  srvyr::filter(y_jub > 0) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(jub_insuf))

c_ano <- mean(as.numeric(c_ano$colname))


CODIND       <- 330205
DIMENSIÓN    <- "Adecuación"
CODINDICADOR <- "Insuficiencia de las jubilaciones en relación a la línea de las pobreza"
NOMINDICADOR <- "Porcentaje de personas que perciben jubilaciones por debajo de la línea de pobreza"
VALOR	       <- c_ano
RESPONSABLE	 <- "J. Pandolfi"
JERARQUIA	   <- 1
CORTE        <- "Total"
DEPARTAMENTO <- ""
QUINTIL	     <- ""
SEXO		     <- ""
ASCENDENCIA  <- ""
ss_12 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de personas que perciben jubilaciones por debajo de mitad de la media del salario nacional
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Total

c_ano <- ech2020_svy %>%
  srvyr::filter(y_jub > 0) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(jub_insuf_media))

c_ano <- mean(as.numeric(c_ano$colname))


CODIND       <- 330206
DIMENSIÓN    <- "Adecuación"
CODINDICADOR <- "Insuficiencia de las jubilaciones en relación al salario nacional (mitad del promedio)"
NOMINDICADOR <- "Porcentaje de personas que perciben jubilaciones por debajo de la mitad de la media del salario nacional"
VALOR	       <- c_ano
RESPONSABLE	 <- "J. Pandolfi"
JERARQUIA	   <- 1
CORTE        <- "Total"
DEPARTAMENTO <- ""
QUINTIL	     <- ""
SEXO		     <- ""
ASCENDENCIA  <- ""
ss_13 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA)




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de personas que perciben jubilaciones por debajo de mitad de la mediana del salario nacional
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Total

c_ano <- ech2020_svy %>%
  srvyr::filter(y_jub > 0) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(jub_insuf_mediana))

c_ano <- mean(as.numeric(c_ano$colname))


CODIND       <- 330207
DIMENSIÓN    <- "Adecuación"
CODINDICADOR <- "Insuficiencia de las jubilaciones en relación al salario nacional (mitad de las la mediana)"
NOMINDICADOR <- "Porcentaje de personas que perciben jubilaciones por debajo de la mitad de la mediana del salario nacional"
VALOR	       <- c_ano
RESPONSABLE	 <- "J. Pandolfi"
JERARQUIA	   <- 1
CORTE        <- "Total"
DEPARTAMENTO <- ""
QUINTIL	     <- ""
SEXO		     <- ""
ASCENDENCIA  <- ""
ss_14 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de personas que perciben jubilaciones por debajo del SMN
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Total

c_ano <- ech2020_svy %>%
  srvyr::filter(y_jub > 0) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(jub_insuf_smn))

c_ano <- mean(as.numeric(c_ano$colname))


CODIND       <- 330208
DIMENSIÓN    <- "Adecuación"
CODINDICADOR <- "Insuficiencia de las jubilaciones en relación al salario mínimo nacional"
NOMINDICADOR <- "Porcentaje de personas que perciben jubilaciones por debajo del salario mínimo nacional"
VALOR	       <- c_ano
RESPONSABLE	 <- "J. Pandolfi"
JERARQUIA	   <- 1
CORTE        <- "Total"
DEPARTAMENTO <- ""
QUINTIL	     <- ""
SEXO		     <- ""
ASCENDENCIA  <- ""
ss_15 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA)




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de personas que perciben pensiones por debajo de la línea de pobreza
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Total

c_ano <- ech2020_svy %>%
  srvyr::filter(y_pens > 0) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(pens_insuf))

c_ano <- mean(as.numeric(c_ano$colname))



CODIND       <- 330209
DIMENSIÓN    <- "Adecuación"
CODINDICADOR <- "Insuficiencia de las pensiones en relación a la línea de las pobreza"
NOMINDICADOR <- "Porcentaje de personas que perciben pensiones por debajo de la línea de pobreza"
VALOR	       <- c_ano
RESPONSABLE	 <- "J. Pandolfi"
JERARQUIA	   <- 1
CORTE        <- "Total"
DEPARTAMENTO <- ""
QUINTIL	     <- ""
SEXO		     <- ""
ASCENDENCIA  <- ""
ss_16 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de personas que perciben pensiones por debajo de la mitad de la media del salario nacional
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Total

c_ano <- ech2020_svy %>%
  srvyr::filter(y_pens > 0) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(pens_insuf_media))

c_ano <- mean(as.numeric(c_ano$colname))



CODIND       <- 330210
DIMENSIÓN    <- "Adecuación"
CODINDICADOR <- "Insuficiencia de las pensiones en relación al salario nacional (mitad del promedio)"
NOMINDICADOR <- "Porcentaje de personas que perciben pensiones por debajo de la mitad de la media del salario nacional"
VALOR	       <- c_ano
RESPONSABLE	 <- "J. Pandolfi"
JERARQUIA	   <- 1
CORTE        <- "Total"
DEPARTAMENTO <- ""
QUINTIL	     <- ""
SEXO		     <- ""
ASCENDENCIA  <- ""
ss_17 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de personas que perciben pensiones por debajo de la mitad de la mediana del salario nacional
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Total

c_ano <- ech2020_svy %>%
  srvyr::filter(y_pens > 0) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(pens_insuf_mediana))

c_ano <- mean(as.numeric(c_ano$colname))



CODIND       <- 330211
DIMENSIÓN    <- "Adecuación"
CODINDICADOR <- "Insuficiencia de las pensiones en relación al salario nacional (mitad de las la mediana)"
NOMINDICADOR <- "Porcentaje de personas que perciben pensiones por debajo de la mitad de la mediana del salario nacional"
VALOR	       <- c_ano
RESPONSABLE	 <- "J. Pandolfi"
JERARQUIA	   <- 1
CORTE        <- "Total"
DEPARTAMENTO <- ""
QUINTIL	     <- ""
SEXO		     <- ""
ASCENDENCIA  <- ""
ss_18 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de personas que perciben pensiones por debajo del SMN
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Total

c_ano <- ech2020_svy %>%
  srvyr::filter(y_pens > 0) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(pens_insuf_smn))

c_ano <- mean(as.numeric(c_ano$colname))



CODIND       <- 330212
DIMENSIÓN    <- "Adecuación"
CODINDICADOR <- "Insuficiencia de las pensiones en relación al salario mínimo nacional"
NOMINDICADOR <- "Porcentaje de personas que perciben pensiones por debajo del salario mínimo nacional"
VALOR	       <- c_ano
RESPONSABLE	 <- "J. Pandolfi"
JERARQUIA	   <- 1
CORTE        <- "Total"
DEPARTAMENTO <- ""
QUINTIL	     <- ""
SEXO		     <- ""
ASCENDENCIA  <- ""
ss_19 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de personas que residen en hogares que perciben transferencias (TUS y/o AFAM) cuyo monto per-cápita es inferior a una canasta básica alimentaria
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Total

c_ano <- ech2020_svy %>%
  srvyr::filter(y_transf_h > 0) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(trans_insuf))

c_ano <- mean(as.numeric(c_ano$colname))


CODIND       <- 330213
DIMENSIÓN    <- "Adecuación"
CODINDICADOR <- "Insuficiencia de las transferencias monetarias en relación a la canasta básica alimentaria"
NOMINDICADOR <- "Porcentaje de personas que residen en hogares que perciben transferencias (TUS y/o AFAM) cuyo monto per-cápita es inferior a una canasta básica alimentaria"
VALOR	       <- c_ano
RESPONSABLE	 <- "J. Pandolfi"
JERARQUIA	   <- 1
CORTE        <- "Total"
DEPARTAMENTO <- ""
QUINTIL	     <- ""
SEXO		     <- ""
ASCENDENCIA  <- ""
ss_20 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA)


ss_2020 <- rbind(ss_01, ss_02, ss_03, ss_04, ss_05, ss_06, ss_07, ss_08, ss_09, ss_10, ss_11, ss_12, ss_13, ss_14, ss_15, ss_16, ss_17, ss_18, ss_19, ss_20)

rio::export(ss_2020, "ss_2020.xlsx" )


