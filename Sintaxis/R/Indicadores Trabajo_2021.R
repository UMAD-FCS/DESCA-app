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
AÑO	         <- 2021
FUENTE	     <- "Encuesta Continua de Hogares 2021, INE"
RESPONSABLE	 <- "J. Pandolfi"
JERARQUIA_CAT <- 1

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

### Carga de bases ###

sem1          <- rio::import("Bases/ECH_2021_sem1_terceros.sav")
sem2_implant  <- rio::import("Bases/ECH_implantacion_sem2_2021.csv")
sem2_panel_07 <- rio::import("Bases/Bases_mensuales_terceros/ECH_07_21.csv")
sem2_panel_08 <- rio::import("Bases/Bases_mensuales_terceros/ECH_08_21.csv")
sem2_panel_09 <- rio::import("Bases/Bases_mensuales_terceros/ECH_09_21.csv")
sem2_panel_10 <- rio::import("Bases/Bases_mensuales_terceros/ECH_10_21.csv")
sem2_panel_11 <- rio::import("Bases/Bases_mensuales_terceros/ECH_11_21.csv")
sem2_panel_12 <- rio::import("Bases/Bases_mensuales_terceros/ECH_12_21.csv")

pesos_boost_07 <- rio::import('Bases/pesos_replicados/pesos_replicados_07-2021.csv')
pesos_boost_08 <- rio::import('Bases/pesos_replicados/pesos_replicados_08-2021.csv')
pesos_boost_09 <- rio::import('Bases/pesos_replicados/pesos_replicados_09-2021.csv')
pesos_boost_10 <- rio::import('Bases/pesos_replicados/pesos_replicados_10-2021.csv')
pesos_boost_11 <- rio::import('Bases/pesos_replicados/pesos_replicados_11-2021.csv')
pesos_boost_12 <- rio::import('Bases/pesos_replicados/pesos_replicados_12-2021.csv')


sem2_panel_07 = sem2_panel_07 %>% dplyr::left_join(pesos_boost_07)
sem2_panel_08 = sem2_panel_08 %>% dplyr::left_join(pesos_boost_08)
sem2_panel_09 = sem2_panel_09 %>% dplyr::left_join(pesos_boost_09)
sem2_panel_10 = sem2_panel_10 %>% dplyr::left_join(pesos_boost_10)
sem2_panel_11 = sem2_panel_11 %>% dplyr::left_join(pesos_boost_11)
sem2_panel_12 = sem2_panel_12 %>% dplyr::left_join(pesos_boost_12)

sem2_panel <- rbind(sem2_panel_07, sem2_panel_08,sem2_panel_09, sem2_panel_10, sem2_panel_11, sem2_panel_12)




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

### Generación de nuevas variables ###

# Salario mínimo nacional

smn <- 17930
smn_h = smn/200

#IPC

sem1 <- sem1 %>% dplyr::mutate(bc_ipc_tot = case_when(mes == 1 ~ 0.335542051,
                                                      mes == 2 ~ 0.330249262,
                                                      mes == 3 ~ 0.327549795,
                                                      mes == 4 ~ 0.32554687,
                                                      mes == 5 ~ 0.323919843,
                                                      mes == 6 ~ 0.322448435))

sem2_implant <- sem2_implant %>% dplyr::mutate(bc_ipc_tot = case_when(
  mes == 7  ~ 0.320314392,
  mes == 8  ~ 0.318657357,
  mes == 9  ~ 0.31596912,
  mes == 10 ~ 0.314515807,
  mes == 11 ~ 0.31128448,
  mes == 12 ~ 0.310493463))


sem1 <- sem1 %>% dplyr::mutate(bc_ipc = case_when(    mes == 1 ~ 0.329708731,
                                                      mes == 2 ~ 0.325120854,
                                                      mes == 3 ~ 0.32193312,
                                                      mes == 4 ~ 0.320163041,
                                                      mes == 5 ~ 0.318357918,
                                                      mes == 6 ~ 0.316774856))

sem2_implant <- sem2_implant %>% dplyr::mutate(bc_ipc = case_when(
  mes == 7 ~  0.314794762,
  mes == 8 ~  0.31306265,
  mes == 9 ~  0.310558149,
  mes == 10 ~ 0.309320866,
  mes == 11 ~ 0.306270364,
  mes == 12 ~ 0.305191859))                                                 


sem1 <- sem1 %>% dplyr::mutate(ipc_ene2022 = case_when(    mes == 1 ~ 0.941120053,
                                                      mes == 2 ~ 0.948876194,
                                                      mes == 3 ~ 0.32193312,
                                                      mes == 4 ~ 0.959509612,
                                                      mes == 5 ~ 0.963888078,
                                                      mes == 6 ~ 0.970309829))

sem2_implant <- sem2_implant %>% dplyr::mutate(ipc_ene2022 = case_when(
  mes == 7 ~  0.97535549,
  mes == 8 ~  0.983653726,
  mes == 9 ~  0.988198991,
  mes == 10 ~ 0.998457112,
  mes == 11 ~ 1.001000792,
  mes == 12 ~ 1))                                                 

sem2_panel <- sem2_panel %>% dplyr::mutate(ipc_ene2022 = case_when(
  mes == 7 ~  0.97535549,
  mes == 8 ~  0.983653726,
  mes == 9 ~  0.988198991,
  mes == 10 ~ 0.998457112,
  mes == 11 ~ 1.001000792,
  mes == 12 ~ 1))   


# Ingresos

sem1 <- sem1 %>% dplyr::mutate(y_pc       =  HT11 / ht19 )                      #Ingreso per-cápita
sem1 <- sem1 %>% dplyr::mutate(y_pc_svl   =  (HT11 - HT13) / ht19)              #Ingreso per-cápita sin valor locativo
sem1 <- sem1 %>% dplyr::mutate(y_pc_d     =  HT11 / ht19 / bc_ipc_tot)          #Ingreso per-cápita deflactado
sem1 <- sem1 %>% dplyr::mutate(y_pc_svl_d =  (HT11 - HT13) / ht19 / bc_ipc_tot) #Ingreso per-cápita sin valor locativo deflactado

sem2_implant <- sem2_implant %>% dplyr::mutate(y_pc       =  ht11 / ht19 )                      #Ingreso per-cápita
sem2_implant <- sem2_implant %>% dplyr::mutate(y_pc_svl   =  (ht11 - ht13) / ht19)              #Ingreso per-cápita sin valor locativo
sem2_implant <- sem2_implant %>% dplyr::mutate(y_pc_d     =  ht11 / ht19 / bc_ipc_tot)          #Ingreso per-cápita deflactado
sem2_implant <- sem2_implant %>% dplyr::mutate(y_pc_svl_d =  (ht11 - ht13) / ht19 / bc_ipc_tot) #Ingreso per-cápita sin valor locativo deflactado


# Quintil de ingresos
sem1_h <- sem1 %>% distinct(numero, .keep_all = TRUE)
sem1_h <- sem1_h %>% dplyr::mutate(quintilesy = statar::xtile(y_pc, n=5, wt = pesomen))
sem1_h <- sem1_h[,c("numero","quintilesy")]
sem1 <- merge(sem1, sem1_h, by = "numero")

sem2_h <- sem2_implant %>% distinct(ID, .keep_all = TRUE)
sem2_h <- sem2_h %>% dplyr::mutate(quintilesy = statar::xtile(y_pc, n=5, wt = w_sem))
sem2_h <- sem2_h[,c("ID","quintilesy")]
sem2_implant <- merge(sem2_implant, sem2_h, by = "ID")

quintil <- sem2_implant[,c("ID","quintilesy")]
sem2_panel  <- merge(sem2_panel, quintil, by = "ID")


# Sexo

sem1 <- sem1 %>% dplyr::mutate(bc_pe2 = E26)
sem2_implant <- sem2_implant %>% dplyr::mutate(bc_pe2 = e26)
sem2_panel <- sem2_panel %>% dplyr::mutate(bc_pe2 = e26)


# Ascendencia afro

sem1 <- sem1 %>% dplyr::mutate(bd_e29_1 = e29_1)
sem2_implant <- sem2_implant %>% dplyr::mutate(bd_e29_1 = e29_1)
sem2_panel <- sem2_panel %>% dplyr::mutate(bd_e29_1 = e29_1)


# Población económicamente activa

sem1 <- sem1 %>% dplyr::mutate(pea = ifelse(POBPCOAC==2 | POBPCOAC==3 | POBPCOAC==4 | POBPCOAC==5, 1, 0))
sem2_panel <- sem2_panel %>% dplyr::mutate(pea = ifelse(POBPCOAC==2 | POBPCOAC==3 | POBPCOAC==4 | POBPCOAC==5, 1, 0))


# Ocupados

sem1 <- sem1 %>% dplyr::mutate(ocup = ifelse(POBPCOAC==2, 1, 0))
sem2_implant <- sem2_implant %>% dplyr::mutate(ocup = ifelse(pobpcoac==2, 1, 0))
sem2_panel <- sem2_panel %>% dplyr::mutate(ocup = ifelse(POBPCOAC==2, 1, 0))


# Desempleados

sem1 <- sem1 %>% dplyr::mutate(desocup = ifelse(POBPCOAC==3 | POBPCOAC==4 | POBPCOAC==5, 1, 0))
sem2_panel <- sem2_panel %>% dplyr::mutate(desocup = ifelse(POBPCOAC==3 | POBPCOAC==4 | POBPCOAC==5, 1, 0))


# Horas remuneradas

sem1 <- sem1 %>% dplyr::mutate(bc_horas = F85+F98)
sem2_implant <- sem2_implant  %>% dplyr::mutate(bc_horas = f85+f98)
sem2_panel <- sem2_panel  %>% dplyr::mutate(bc_horas = f85+f98)


# Sobrecarga de horas
sem1 <- sem1 %>% dplyr::mutate(sobrecarga = ifelse(bc_horas>48, 1, 0))
sem2_panel <- sem2_panel  %>% dplyr::mutate(sobrecarga = ifelse(bc_horas>48, 1, 0))


# Subempleo

sem1 <- sem1 %>% dplyr::mutate(bc_subocupado = ifelse(F102 == 1 & F104 == 5 & bc_horas > 0 & bc_horas < 40, 1, 0))
sem2_panel <- sem2_panel %>% dplyr::mutate(bc_subocupado = ifelse(f102 == 1 & f104 == 5 & bc_horas > 0 & bc_horas < 40, 1, 0))
                                                              

# No aporte a SS en trabajo principal y secundario

sem1 <- sem1 %>% dplyr::mutate(bc_register2 = ifelse(F96 == 1 | F82 == 1, 0, 1))
sem2_implant <- sem2_implant %>% dplyr::mutate(bc_register2 = ifelse(f96 == 1 | f82 == 1, 0, 1))
sem2_panel <- sem2_panel %>% dplyr::mutate(bc_register2 = ifelse(f96 == 1 | f82 == 1, 0, 1))


# No aporte a SS en trabajo principal

sem1 <- sem1 %>% dplyr::mutate(bc_register = ifelse(F82 == 1, 0, 1))
sem2_implant <- sem2_implant %>% dplyr::mutate(bc_register = ifelse(f82 == 1, 0, 1))
sem2_panel <- sem2_panel %>% dplyr::mutate(bc_register = ifelse(f82 == 1, 0, 1))


# No aporte por totalidad del salario

sem1 <- sem1 %>% dplyr::mutate(noaportatot = ifelse(F84 == 2, 1, 0))
sem2_implant <- sem2_implant %>% dplyr::mutate(noaportatot = ifelse(f84 == 2, 1, 0))


# Restricciones al empleo

sem1 <- sem1 %>% dplyr::mutate(restric = ifelse(bc_register2 == 1 | bc_subocupado ==1, 1, 0))
sem2_panel <- sem2_panel %>% dplyr::mutate(restric = ifelse(bc_register2 == 1 | bc_subocupado ==1, 1, 0))


# Salario en ocupación principal

sem1 <- sem1 %>% dplyr::mutate(horamen = F85 * 4.3)
sem1 <- sem1 %>% dplyr::mutate(yhora = PT2/horamen)
sem1 <- sem1 %>% dplyr::mutate(yhoradef = yhora/bc_ipc_tot)

sem2_implant <- sem2_implant %>% dplyr::mutate(horamen = f85 * 4.3)
sem2_implant <- sem2_implant %>% dplyr::mutate(yhora = pt2/horamen)
sem2_implant <- sem2_implant %>% dplyr::mutate(yhoradef = yhora/bc_ipc_tot)

# Línea de pobreza individual

sem1 <- sem1 %>% dplyr::mutate(regionlp = case_when(region_4 == 1 ~ 1,
                                                    region_4 == 2 | region_4 == 3 ~ 2,
                                                    region_4 == 4 ~ 3))


sem1 <- sem1 %>% dplyr::mutate(grupolp = case_when(regionlp == 1 & mes == 1 ~ 1,
                                                   regionlp == 1 & mes == 2 ~ 2,
                                                   regionlp == 1 & mes == 3 ~ 3,
                                                   regionlp == 1 & mes == 4 ~ 4,
                                                   regionlp == 1 & mes == 5 ~ 5,
                                                   regionlp == 1 & mes == 6 ~ 6,
                                                   regionlp == 2 & mes == 1 ~ 7,
                                                   regionlp == 2 & mes == 2 ~ 8,
                                                   regionlp == 2 & mes == 3 ~ 9,
                                                   regionlp == 2 & mes == 4 ~ 10,
                                                   regionlp == 2 & mes == 5 ~ 11,
                                                   regionlp == 2 & mes == 6 ~ 12,
                                                   regionlp == 3 & mes == 1 ~ 13,
                                                   regionlp == 3 & mes == 2 ~ 14,
                                                   regionlp == 3 & mes == 3 ~ 15,
                                                   regionlp == 3 & mes == 4 ~ 16,
                                                   regionlp == 3 & mes == 5 ~ 17,
                                                   regionlp == 3 & mes == 6 ~ 18))

sem1_unipersonales <- sem1 %>%  filter(ht19==1)
sem1_unipersonales <- sem1_unipersonales[,c("numero","grupolp", "lp_06")]
sem1_unipersonales <- sem1_unipersonales %>% dplyr::mutate(lp_unipersonales = lp_06)
sem1_unipersonales <- sem1_unipersonales[order(sem1_unipersonales$grupolp, sem1_unipersonales$lp_unipersonales, decreasing = TRUE), ]
sem1_unipersonales <- sem1_unipersonales %>% distinct(grupolp, .keep_all = TRUE)
sem1_unipersonales <- sem1_unipersonales[,c("grupolp","lp_unipersonales")]

sem1 <- merge(sem1, sem1_unipersonales, by = "grupolp")


sem2_implant <- sem2_implant %>% dplyr::mutate(regionlp = case_when(region_4 == 1 ~ 1,
                                                    region_4 == 2 | region_4 == 3 ~ 2,
                                                    region_4 == 4 ~ 3))


sem2_implant <- sem2_implant %>% dplyr::mutate(grupolp = case_when(regionlp == 1 & mes == 7 ~ 1,
                                                   regionlp == 1 & mes == 8 ~ 2,
                                                   regionlp == 1 & mes == 9 ~ 3,
                                                   regionlp == 1 & mes == 10 ~ 4,
                                                   regionlp == 1 & mes == 11 ~ 5,
                                                   regionlp == 1 & mes == 12 ~ 6,
                                                   regionlp == 2 & mes == 7 ~ 7,
                                                   regionlp == 2 & mes == 8 ~ 8,
                                                   regionlp == 2 & mes == 9 ~ 9,
                                                   regionlp == 2 & mes == 10 ~ 10,
                                                   regionlp == 2 & mes == 11 ~ 11,
                                                   regionlp == 2 & mes == 12 ~ 12,
                                                   regionlp == 3 & mes == 7 ~ 13,
                                                   regionlp == 3 & mes == 8 ~ 14,
                                                   regionlp == 3 & mes == 9 ~ 15,
                                                   regionlp == 3 & mes == 10 ~ 16,
                                                   regionlp == 3 & mes == 11 ~ 17,
                                                   regionlp == 3 & mes == 12 ~ 18))

sem2_implant_unipersonales <- sem2_implant %>%  filter(ht19==1)
sem2_implant_unipersonales <- sem2_implant_unipersonales[,c("grupolp", "lp")]
sem2_implant_unipersonales <- sem2_implant_unipersonales %>% dplyr::mutate(lp_unipersonales = lp)
sem2_implant_unipersonales <- sem2_implant_unipersonales[order(sem2_implant_unipersonales$grupolp, sem2_implant_unipersonales$lp_unipersonales, decreasing = TRUE), ]
sem2_implant_unipersonales <- sem2_implant_unipersonales %>% distinct(grupolp, .keep_all = TRUE)
sem2_implant_unipersonales <- sem2_implant_unipersonales[,c("grupolp","lp_unipersonales")]

sem2_implant <- merge(sem2_implant, sem2_implant_unipersonales, by = "grupolp")



# Salario por debajo de línea de pobreza

sem1 <- sem1 %>% dplyr::mutate(salario_insuf   = ifelse(PT2 < lp_unipersonales, 1, 0))
sem2_implant <- sem2_implant %>% dplyr::mutate(salario_insuf = ifelse(pt2 < lp_unipersonales, 1, 0))


# Salario por debajo del salario mínimo nacional

sem1 <- sem1 %>% dplyr::mutate(salario_insuf_smn = ifelse(yhora<smn_h, 1, 0))
sem2_implant <- sem2_implant %>% dplyr::mutate(salario_insuf_smn = ifelse(yhora<smn_h, 1, 0))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

### Información de muestreo ### 

sem1_svy           <- srvyr::as_survey_design(sem1, ids = numero, weights = pesomen)
#sem1_h_svy         <- srvyr::as_survey_design(sem1_h, ids = numero, weights = pesomen)

sem2_implant_svy   <- srvyr::as_survey_design(sem2_implant, ids = ID, weights = w_sem)
#sem2_implant_h_svy <- srvyr::as_survey_design(sem2_implant_h, ids = ID, weights = w_sem)

sem2_panel_svy   <-  srvyr::as_survey_design(sem2_panel, ids = ID, weights = w)

#sem2_panel_svy     <- svrepdesign(data = sem2_panel,
#                                  type = "bootstrap",
#                                  weights =~ w,
#                                  repweights = sem2_panel %>% dplyr::select(dplyr::starts_with("wr")))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
### Procesamiento de indicadores ###
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Tasa de actividad
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Total

a_mes <- sem1_svy %>%
  srvyr::filter(E27>=14) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(pea, na.rm=T))

a_sem <- mean(as.numeric(a_mes$colname))

b_mes <- sem2_panel_svy %>%
  srvyr::filter(e27>=14) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(pea, na.rm=T))

b_sem <- mean(as.numeric(b_mes$colname))

c_ano <- mean(c(a_sem, b_sem))

# Ascendencia étnico racial

a_afro <- function(x) {
  x <- sem1_svy %>%
    filter(bd_e29_1 == x & E27>=14) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pea, na.rm=T))
  x <- mean(x$colname)
}       

a_e_afro <- numeric()

for(i in 1:2){
  a_e_afro[i] <- a_afro(x = i)
}     

b_afro <- function(x) {
  x <- sem2_panel_svy %>%
    filter(bd_e29_1 == x & e27>=14) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pea, na.rm=T))
  x <- mean(x$colname)
}       

b_e_afro <- numeric()

for(i in 1:2){
  b_e_afro[i] <- b_afro(x = i)
}     

c_afro <- as.data.frame(cbind(a_e_afro, b_e_afro))
c_afro <- c_afro %>% dplyr::mutate(m_afro = (a_e_afro + b_e_afro)/2)
c_afro <- c_afro[,c("m_afro")]

# Departamento

a_dpto <- function(x) {
  x <- sem1_svy %>%
    filter(dpto == x & E27>14) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pea, na.rm=T))
  x <- mean(x$colname)
}       

a_e_dpto <- numeric()

for(i in 1:19){
  a_e_dpto[i] <- a_dpto(x = i)
}     

b_dpto <- function(x) {
  x <- sem2_panel_svy %>%
    filter(dpto == x & e27>14) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pea, na.rm=T))
  x <- mean(x$colname)
}       

b_e_dpto <- numeric()

for(i in 1:19){
  b_e_dpto[i] <- b_dpto(x = i)
}     

c_dpto <- as.data.frame(cbind(a_e_dpto, b_e_dpto))
c_dpto <- c_dpto %>% dplyr::mutate(m_dpto = (a_e_dpto + b_e_dpto)/2)
c_dpto <- c_dpto[,c("m_dpto")]



# Quintil de ingresos

a_quintil <- function(x) {
  x <- sem1_svy %>%
    filter(quintilesy == x & E27>=14) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pea, na.rm=T))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}     

b_quintil <- function(x) {
  x <- sem2_panel_svy %>%
    filter(quintilesy == x & e27>=14) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pea, na.rm=T))
  x <- mean(x$colname)
}       

b_e_quintil <- numeric()

for(i in 1:5){
  b_e_quintil[i] <- b_quintil(x = i)
}     

c_quintil <- as.data.frame(cbind(a_e_quintil, b_e_quintil))
c_quintil <- c_quintil %>% dplyr::mutate(m_quintil = (a_e_quintil + b_e_quintil)/2)
c_quintil <- c_quintil[,c("m_quintil")]


# Sexo

a_sexo <- function(x) {
  x <- sem1_svy %>%
    filter(bc_pe2 == x & E27>=14) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pea, na.rm=T))
  x <- mean(x$colname)
}       

a_e_sexo <- numeric()

for(i in 1:2){
  a_e_sexo[i] <- a_sexo(x = i)
}     

b_sexo <- function(x) {
  x <- sem2_panel_svy %>%
    filter(bc_pe2 == x & e27>=14) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pea, na.rm=T))
  x <- mean(x$colname)
}       

b_e_sexo <- numeric()

for(i in 1:2){
  b_e_sexo[i] <- b_sexo(x = i)
}     

for(i in 1:2){
  b_e_sexo[i] <- b_sexo(x = i)
}         

c_sexo <- as.data.frame(cbind(a_e_sexo, b_e_sexo))
c_sexo <- c_sexo %>% dplyr::mutate(m_sexo = (a_e_sexo + b_e_sexo)/2)
c_sexo <- c_sexo[,c("m_sexo")]


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

NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. Este indicador se calcula a partir de la encuesta telefónica del primer semestre de 2021 y el formulario telefónico de modalidad panel del segundo semestre de 2021. En el segundo semestre, el quintil de ingresos del hogar corresponde a los ingresos declarados durante la implantación del panel en la encuesta presencial."

t_01 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, JERARQUIA_CAT, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA, NOTA_INDICADOR)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Tasa de empleo
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


# Total

a_mes <- sem1_svy %>%
  srvyr::filter(E27>=14) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(ocup, na.rm=T))

a_sem <- mean(as.numeric(a_mes$colname))

b_mes <- sem2_panel_svy %>%
  srvyr::filter(e27>=14) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(ocup, na.rm=T))

b_sem <- mean(as.numeric(b_mes$colname))

c_ano <- mean(c(a_sem, b_sem))

# Ascendencia étnico racial

a_afro <- function(x) {
  x <- sem1_svy %>%
    filter(bd_e29_1 == x & E27>=14) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(ocup, na.rm=T))
  x <- mean(x$colname)
}       

a_e_afro <- numeric()

for(i in 1:2){
  a_e_afro[i] <- a_afro(x = i)
}     

b_afro <- function(x) {
  x <- sem2_panel_svy %>%
    filter(bd_e29_1 == x & e27>=14) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(ocup, na.rm=T))
  x <- mean(x$colname)
}       

b_e_afro <- numeric()

for(i in 1:2){
  b_e_afro[i] <- b_afro(x = i)
}     

c_afro <- as.data.frame(cbind(a_e_afro, b_e_afro))
c_afro <- c_afro %>% dplyr::mutate(m_afro = (a_e_afro + b_e_afro)/2)
c_afro <- c_afro[,c("m_afro")]

# Departamento

a_dpto <- function(x) {
  x <- sem1_svy %>%
    filter(dpto == x & E27>=14) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(ocup, na.rm=T))
  x <- mean(x$colname)
}       

a_e_dpto <- numeric()

for(i in 1:19){
  a_e_dpto[i] <- a_dpto(x = i)
}     

b_dpto <- function(x) {
  x <- sem2_panel_svy %>%
    filter(dpto == x & e27>=14) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(ocup, na.rm=T))
  x <- mean(x$colname)
}       

b_e_dpto <- numeric()

for(i in 1:19){
  b_e_dpto[i] <- b_dpto(x = i)
}     

c_dpto <- as.data.frame(cbind(a_e_dpto, b_e_dpto))
c_dpto <- c_dpto %>% dplyr::mutate(m_dpto = (a_e_dpto + b_e_dpto)/2)
c_dpto <- c_dpto[,c("m_dpto")]



# Quintil de ingresos

a_quintil <- function(x) {
  x <- sem1_svy %>%
    filter(quintilesy == x & E27>=14) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(ocup, na.rm=T))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}     

b_quintil <- function(x) {
  x <- sem2_panel_svy %>%
    filter(quintilesy == x & e27>=14) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(ocup, na.rm=T))
  x <- mean(x$colname)
}       

b_e_quintil <- numeric()

for(i in 1:5){
  b_e_quintil[i] <- b_quintil(x = i)
}     

c_quintil <- as.data.frame(cbind(a_e_quintil, b_e_quintil))
c_quintil <- c_quintil %>% dplyr::mutate(m_quintil = (a_e_quintil + b_e_quintil)/2)
c_quintil <- c_quintil[,c("m_quintil")]


# Sexo

a_sexo <- function(x) {
  x <- sem1_svy %>%
    filter(bc_pe2 == x & E27>=14) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(ocup, na.rm=T))
  x <- mean(x$colname)
}       

a_e_sexo <- numeric()

for(i in 1:2){
  a_e_sexo[i] <- a_sexo(x = i)
}     

b_sexo <- function(x) {
  x <- sem2_panel_svy %>%
    filter(bc_pe2 == x & e27>=14) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(ocup, na.rm=T))
  x <- mean(x$colname)
}       

b_e_sexo <- numeric()

for(i in 1:2){
  b_e_sexo[i] <- b_sexo(x = i)
}     

for(i in 1:2){
  b_e_sexo[i] <- b_sexo(x = i)
}         

c_sexo <- as.data.frame(cbind(a_e_sexo, b_e_sexo))
c_sexo <- c_sexo %>% dplyr::mutate(m_sexo = (a_e_sexo + b_e_sexo)/2)
c_sexo <- c_sexo[,c("m_sexo")]


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

NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. Este indicador se calcula a partir de la encuesta telefónica del primer semestre de 2021 y el formulario telefónico de modalidad panel del segundo semestre de 2021. En el segundo semestre, el quintil de ingresos del hogar corresponde a los ingresos declarados durante la implantación del panel en la encuesta presencial."

t_02 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, JERARQUIA_CAT, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA, NOTA_INDICADOR)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Tasa de desempleo
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


# Total

a_mes <- sem1_svy %>%
  srvyr::filter(pea == 1) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(desocup, na.rm=T))

a_sem <- mean(as.numeric(a_mes$colname))

b_mes <- sem2_panel_svy %>%
  srvyr::filter(pea == 1) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(desocup, na.rm=T))

b_sem <- mean(as.numeric(b_mes$colname))

c_ano <- mean(c(a_sem, b_sem))

# Ascendencia étnico racial

a_afro <- function(x) {
  x <- sem1_svy %>%
    filter(bd_e29_1 == x & pea == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(desocup, na.rm=T))
  x <- mean(x$colname)
}       

a_e_afro <- numeric()

for(i in 1:2){
  a_e_afro[i] <- a_afro(x = i)
}     

b_afro <- function(x) {
  x <- sem2_panel_svy %>%
    filter(bd_e29_1 == x & pea == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(desocup, na.rm=T))
  x <- mean(x$colname)
}       

b_e_afro <- numeric()

for(i in 1:2){
  b_e_afro[i] <- b_afro(x = i)
}     

c_afro <- as.data.frame(cbind(a_e_afro, b_e_afro))
c_afro <- c_afro %>% dplyr::mutate(m_afro = (a_e_afro + b_e_afro)/2)
c_afro <- c_afro[,c("m_afro")]

# Departamento

a_dpto <- function(x) {
  x <- sem1_svy %>%
    filter(dpto == x & pea == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(desocup, na.rm=T))
  x <- mean(x$colname)
}       

a_e_dpto <- numeric()

for(i in 1:19){
  a_e_dpto[i] <- a_dpto(x = i)
}     

b_dpto <- function(x) {
  x <- sem2_panel_svy %>%
    filter(dpto == x & pea == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(desocup, na.rm=T))
  x <- mean(x$colname)
}       

b_e_dpto <- numeric()

for(i in 1:19){
  b_e_dpto[i] <- b_dpto(x = i)
}     

c_dpto <- as.data.frame(cbind(a_e_dpto, b_e_dpto))
c_dpto <- c_dpto %>% dplyr::mutate(m_dpto = (a_e_dpto + b_e_dpto)/2)
c_dpto <- c_dpto[,c("m_dpto")]



# Quintil de ingresos

a_quintil <- function(x) {
  x <- sem1_svy %>%
    filter(quintilesy == x & pea == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(desocup, na.rm=T))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}     

b_quintil <- function(x) {
  x <- sem2_panel_svy %>%
    filter(quintilesy == x & pea == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(desocup, na.rm=T))
  x <- mean(x$colname)
}       

b_e_quintil <- numeric()

for(i in 1:5){
  b_e_quintil[i] <- b_quintil(x = i)
}     

c_quintil <- as.data.frame(cbind(a_e_quintil, b_e_quintil))
c_quintil <- c_quintil %>% dplyr::mutate(m_quintil = (a_e_quintil + b_e_quintil)/2)
c_quintil <- c_quintil[,c("m_quintil")]


# Sexo

a_sexo <- function(x) {
  x <- sem1_svy %>%
    filter(bc_pe2 == x & pea == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(desocup, na.rm=T))
  x <- mean(x$colname)
}       

a_e_sexo <- numeric()

for(i in 1:2){
  a_e_sexo[i] <- a_sexo(x = i)
}     

b_sexo <- function(x) {
  x <- sem2_panel_svy %>%
    filter(bc_pe2 == x & pea == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(desocup, na.rm=T))
  x <- mean(x$colname)
}       

b_e_sexo <- numeric()

for(i in 1:2){
  b_e_sexo[i] <- b_sexo(x = i)
}     

for(i in 1:2){
  b_e_sexo[i] <- b_sexo(x = i)
}         

c_sexo <- as.data.frame(cbind(a_e_sexo, b_e_sexo))
c_sexo <- c_sexo %>% dplyr::mutate(m_sexo = (a_e_sexo + b_e_sexo)/2)
c_sexo <- c_sexo[,c("m_sexo")]


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

NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. Este indicador se calcula a partir de la encuesta telefónica del primer semestre de 2021 y el formulario telefónico de modalidad panel del segundo semestre de 2021. En el segundo semestre, el quintil de ingresos del hogar corresponde a los ingresos declarados durante la implantación del panel en la encuesta presencial."

t_03 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, JERARQUIA_CAT, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA, NOTA_INDICADOR)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Tasa de subempleo
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


# Total

a_mes <- sem1_svy %>%
  srvyr::filter(ocup == 1) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(bc_subocupado, na.rm=T))

a_sem <- mean(as.numeric(a_mes$colname))

b_mes <- sem2_panel_svy %>%
  srvyr::filter(ocup == 1) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(bc_subocupado, na.rm=T))

b_sem <- mean(as.numeric(b_mes$colname))

c_ano <- mean(c(a_sem, b_sem))

# Ascendencia étnico racial

a_afro <- function(x) {
  x <- sem1_svy %>%
    filter(bd_e29_1 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_subocupado, na.rm=T))
  x <- mean(x$colname)
}       

a_e_afro <- numeric()

for(i in 1:2){
  a_e_afro[i] <- a_afro(x = i)
}     

b_afro <- function(x) {
  x <- sem2_panel_svy %>%
    filter(bd_e29_1 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_subocupado, na.rm=T))
  x <- mean(x$colname)
}       

b_e_afro <- numeric()

for(i in 1:2){
  b_e_afro[i] <- b_afro(x = i)
}     

c_afro <- as.data.frame(cbind(a_e_afro, b_e_afro))
c_afro <- c_afro %>% dplyr::mutate(m_afro = (a_e_afro + b_e_afro)/2)
c_afro <- c_afro[,c("m_afro")]

# Departamento

a_dpto <- function(x) {
  x <- sem1_svy %>%
    filter(dpto == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_subocupado, na.rm=T))
  x <- mean(x$colname)
}       

a_e_dpto <- numeric()

for(i in 1:19){
  a_e_dpto[i] <- a_dpto(x = i)
}     

b_dpto <- function(x) {
  x <- sem2_panel_svy %>%
    filter(dpto == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_subocupado, na.rm=T))
  x <- mean(x$colname)
}       

b_e_dpto <- numeric()

for(i in 1:19){
  b_e_dpto[i] <- b_dpto(x = i)
}     

c_dpto <- as.data.frame(cbind(a_e_dpto, b_e_dpto))
c_dpto <- c_dpto %>% dplyr::mutate(m_dpto = (a_e_dpto + b_e_dpto)/2)
c_dpto <- c_dpto[,c("m_dpto")]



# Quintil de ingresos

a_quintil <- function(x) {
  x <- sem1_svy %>%
    filter(quintilesy == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_subocupado, na.rm=T))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}     

b_quintil <- function(x) {
  x <- sem2_panel_svy %>%
    filter(quintilesy == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_subocupado, na.rm=T))
  x <- mean(x$colname)
}       

b_e_quintil <- numeric()

for(i in 1:5){
  b_e_quintil[i] <- b_quintil(x = i)
}     

c_quintil <- as.data.frame(cbind(a_e_quintil, b_e_quintil))
c_quintil <- c_quintil %>% dplyr::mutate(m_quintil = (a_e_quintil + b_e_quintil)/2)
c_quintil <- c_quintil[,c("m_quintil")]


# Sexo

a_sexo <- function(x) {
  x <- sem1_svy %>%
    filter(bc_pe2 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_subocupado, na.rm=T))
  x <- mean(x$colname)
}       

a_e_sexo <- numeric()

for(i in 1:2){
  a_e_sexo[i] <- a_sexo(x = i)
}     

b_sexo <- function(x) {
  x <- sem2_panel_svy %>%
    filter(bc_pe2 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_subocupado, na.rm=T))
  x <- mean(x$colname)
}       

b_e_sexo <- numeric()

for(i in 1:2){
  b_e_sexo[i] <- b_sexo(x = i)
}     

for(i in 1:2){
  b_e_sexo[i] <- b_sexo(x = i)
}         

c_sexo <- as.data.frame(cbind(a_e_sexo, b_e_sexo))
c_sexo <- c_sexo %>% dplyr::mutate(m_sexo = (a_e_sexo + b_e_sexo)/2)
c_sexo <- c_sexo[,c("m_sexo")]


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

NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. Este indicador se calcula a partir de la encuesta telefónica del primer semestre de 2021 y el formulario telefónico de modalidad panel del segundo semestre de 2021. En el segundo semestre, el quintil de ingresos del hogar corresponde a los ingresos declarados durante la implantación del panel en la encuesta presencial."

t_04 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, JERARQUIA_CAT, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA, NOTA_INDICADOR)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de ocupados que no aportan a la seguridad social
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Total

a_mes <- sem1_svy %>%
  srvyr::filter(ocup == 1) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(bc_register2, na.rm=T))

a_sem <- mean(as.numeric(a_mes$colname))

b_mes <- sem2_panel_svy %>%
  srvyr::filter(ocup == 1) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(bc_register2, na.rm=T))

b_sem <- mean(as.numeric(b_mes$colname))

c_ano <- mean(c(a_sem, b_sem))

# Ascendencia étnico racial

a_afro <- function(x) {
  x <- sem1_svy %>%
    filter(bd_e29_1 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_register2, na.rm=T))
  x <- mean(x$colname)
}       

a_e_afro <- numeric()

for(i in 1:2){
  a_e_afro[i] <- a_afro(x = i)
}     

b_afro <- function(x) {
  x <- sem2_panel_svy %>%
    filter(bd_e29_1 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_register2, na.rm=T))
  x <- mean(x$colname)
}       

b_e_afro <- numeric()

for(i in 1:2){
  b_e_afro[i] <- b_afro(x = i)
}     

c_afro <- as.data.frame(cbind(a_e_afro, b_e_afro))
c_afro <- c_afro %>% dplyr::mutate(m_afro = (a_e_afro + b_e_afro)/2)
c_afro <- c_afro[,c("m_afro")]

# Departamento

a_dpto <- function(x) {
  x <- sem1_svy %>%
    filter(dpto == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_register2, na.rm=T))
  x <- mean(x$colname)
}       

a_e_dpto <- numeric()

for(i in 1:19){
  a_e_dpto[i] <- a_dpto(x = i)
}     

b_dpto <- function(x) {
  x <- sem2_panel_svy %>%
    filter(dpto == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_register2, na.rm=T))
  x <- mean(x$colname)
}       

b_e_dpto <- numeric()

for(i in 1:19){
  b_e_dpto[i] <- b_dpto(x = i)
}     

c_dpto <- as.data.frame(cbind(a_e_dpto, b_e_dpto))
c_dpto <- c_dpto %>% dplyr::mutate(m_dpto = (a_e_dpto + b_e_dpto)/2)
c_dpto <- c_dpto[,c("m_dpto")]



# Quintil de ingresos

a_quintil <- function(x) {
  x <- sem1_svy %>%
    filter(quintilesy == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_register2, na.rm=T))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}     

b_quintil <- function(x) {
  x <- sem2_panel_svy %>%
    filter(quintilesy == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_register2, na.rm=T))
  x <- mean(x$colname)
}       

b_e_quintil <- numeric()

for(i in 1:5){
  b_e_quintil[i] <- b_quintil(x = i)
}     

c_quintil <- as.data.frame(cbind(a_e_quintil, b_e_quintil))
c_quintil <- c_quintil %>% dplyr::mutate(m_quintil = (a_e_quintil + b_e_quintil)/2)
c_quintil <- c_quintil[,c("m_quintil")]


# Sexo

a_sexo <- function(x) {
  x <- sem1_svy %>%
    filter(bc_pe2 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_register2, na.rm=T))
  x <- mean(x$colname)
}       

a_e_sexo <- numeric()

for(i in 1:2){
  a_e_sexo[i] <- a_sexo(x = i)
}     

b_sexo <- function(x) {
  x <- sem2_panel_svy %>%
    filter(bc_pe2 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_register2, na.rm=T))
  x <- mean(x$colname)
}       

b_e_sexo <- numeric()

for(i in 1:2){
  b_e_sexo[i] <- b_sexo(x = i)
}     

for(i in 1:2){
  b_e_sexo[i] <- b_sexo(x = i)
}         

c_sexo <- as.data.frame(cbind(a_e_sexo, b_e_sexo))
c_sexo <- c_sexo %>% dplyr::mutate(m_sexo = (a_e_sexo + b_e_sexo)/2)
c_sexo <- c_sexo[,c("m_sexo")]


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

NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. Este indicador se calcula a partir de la encuesta telefónica del primer semestre de 2021 y el formulario telefónico de modalidad panel del segundo semestre de 2021. En el segundo semestre, el quintil de ingresos del hogar corresponde a los ingresos declarados durante la implantación del panel en la encuesta presencial."

t_05 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, JERARQUIA_CAT, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA, NOTA_INDICADOR)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de ocupados que no aportan a la seguridad social por la totalidad del salario
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Total

a_mes <- sem1_svy %>%
  srvyr::filter(ocup == 1) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(noaportatot, na.rm=T))

a_sem <- mean(as.numeric(a_mes$colname))

b_mes <- sem2_implant_svy %>%
  srvyr::filter(ocup == 1) %>%
  srvyr::summarise(colname = srvyr::survey_mean(noaportatot, na.rm=T))

b_sem <- mean(as.numeric(b_mes$colname))

c_ano <- mean(c(a_sem, b_sem))

# Ascendencia étnico racial

a_afro <- function(x) {
  x <- sem1_svy %>%
    filter(bd_e29_1 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(noaportatot, na.rm=T))
  x <- mean(x$colname)
}       

a_e_afro <- numeric()

for(i in 1:2){
  a_e_afro[i] <- a_afro(x = i)
}     

b_afro <- function(x) {
  x <- sem2_implant_svy %>%
    filter(bd_e29_1 == x & ocup == 1) %>%
    srvyr::summarise(colname = srvyr::survey_mean(noaportatot, na.rm=T))
  x <- mean(x$colname)
}       

b_e_afro <- numeric()

for(i in 1:2){
  b_e_afro[i] <- b_afro(x = i)
}     

c_afro <- as.data.frame(cbind(a_e_afro, b_e_afro))
c_afro <- c_afro %>% dplyr::mutate(m_afro = (a_e_afro + b_e_afro)/2)
c_afro <- c_afro[,c("m_afro")]

# Departamento

a_dpto <- function(x) {
  x <- sem1_svy %>%
    filter(dpto == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(noaportatot, na.rm=T))
  x <- mean(x$colname)
}       

a_e_dpto <- numeric()

for(i in 1:19){
  a_e_dpto[i] <- a_dpto(x = i)
}     

b_dpto <- function(x) {
  x <- sem2_implant_svy %>%
    filter(dpto == x & ocup == 1) %>%
    srvyr::summarise(colname = srvyr::survey_mean(noaportatot, na.rm=T))
  x <- mean(x$colname)
}       

b_e_dpto <- numeric()

for(i in 1:19){
  b_e_dpto[i] <- b_dpto(x = i)
}     

c_dpto <- as.data.frame(cbind(a_e_dpto, b_e_dpto))
c_dpto <- c_dpto %>% dplyr::mutate(m_dpto = (a_e_dpto + b_e_dpto)/2)
c_dpto <- c_dpto[,c("m_dpto")]



# Quintil de ingresos

a_quintil <- function(x) {
  x <- sem1_svy %>%
    filter(quintilesy == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(noaportatot, na.rm=T))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}     

b_quintil <- function(x) {
  x <- sem2_implant_svy %>%
    filter(quintilesy == x & ocup == 1) %>%
    srvyr::summarise(colname = srvyr::survey_mean(noaportatot, na.rm=T))
  x <- mean(x$colname)
}       

b_e_quintil <- numeric()

for(i in 1:5){
  b_e_quintil[i] <- b_quintil(x = i)
}     

c_quintil <- as.data.frame(cbind(a_e_quintil, b_e_quintil))
c_quintil <- c_quintil %>% dplyr::mutate(m_quintil = (a_e_quintil + b_e_quintil)/2)
c_quintil <- c_quintil[,c("m_quintil")]


# Sexo

a_sexo <- function(x) {
  x <- sem1_svy %>%
    filter(bc_pe2 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(noaportatot, na.rm=T))
  x <- mean(x$colname)
}       

a_e_sexo <- numeric()

for(i in 1:2){
  a_e_sexo[i] <- a_sexo(x = i)
}     

b_sexo <- function(x) {
  x <- sem2_implant_svy %>%
    filter(bc_pe2 == x & ocup == 1) %>%
    srvyr::summarise(colname = srvyr::survey_mean(noaportatot, na.rm=T))
  x <- mean(x$colname)
}       

b_e_sexo <- numeric()

for(i in 1:2){
  b_e_sexo[i] <- b_sexo(x = i)
}     

for(i in 1:2){
  b_e_sexo[i] <- b_sexo(x = i)
}         

c_sexo <- as.data.frame(cbind(a_e_sexo, b_e_sexo))
c_sexo <- c_sexo %>% dplyr::mutate(m_sexo = (a_e_sexo + b_e_sexo)/2)
c_sexo <- c_sexo[,c("m_sexo")]


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

NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. Este indicador se calcula a partir de la encuesta telefónica del primer semestre de 2021 y el formulario presencial de implantación del panel del segundo semestre de 2021."

t_06 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, JERARQUIA_CAT, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA, NOTA_INDICADOR)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de ocupados con restricciones al empleo
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Total

a_mes <- sem1_svy %>%
  srvyr::filter(ocup == 1) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(restric, na.rm=T))

a_sem <- mean(as.numeric(a_mes$colname))

b_mes <- sem2_panel_svy %>%
  srvyr::filter(ocup == 1) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(restric, na.rm=T))

b_sem <- mean(as.numeric(b_mes$colname))

c_ano <- mean(c(a_sem, b_sem))

# Ascendencia étnico racial

a_afro <- function(x) {
  x <- sem1_svy %>%
    filter(bd_e29_1 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(restric, na.rm=T))
  x <- mean(x$colname)
}       

a_e_afro <- numeric()

for(i in 1:2){
  a_e_afro[i] <- a_afro(x = i)
}     

b_afro <- function(x) {
  x <- sem2_panel_svy %>%
    filter(bd_e29_1 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(restric, na.rm=T))
  x <- mean(x$colname)
}       

b_e_afro <- numeric()

for(i in 1:2){
  b_e_afro[i] <- b_afro(x = i)
}     

c_afro <- as.data.frame(cbind(a_e_afro, b_e_afro))
c_afro <- c_afro %>% dplyr::mutate(m_afro = (a_e_afro + b_e_afro)/2)
c_afro <- c_afro[,c("m_afro")]

# Departamento

a_dpto <- function(x) {
  x <- sem1_svy %>%
    filter(dpto == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(restric, na.rm=T))
  x <- mean(x$colname)
}       

a_e_dpto <- numeric()

for(i in 1:19){
  a_e_dpto[i] <- a_dpto(x = i)
}     

b_dpto <- function(x) {
  x <- sem2_panel_svy %>%
    filter(dpto == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(restric, na.rm=T))
  x <- mean(x$colname)
}       

b_e_dpto <- numeric()

for(i in 1:19){
  b_e_dpto[i] <- b_dpto(x = i)
}     

c_dpto <- as.data.frame(cbind(a_e_dpto, b_e_dpto))
c_dpto <- c_dpto %>% dplyr::mutate(m_dpto = (a_e_dpto + b_e_dpto)/2)
c_dpto <- c_dpto[,c("m_dpto")]



# Quintil de ingresos

a_quintil <- function(x) {
  x <- sem1_svy %>%
    filter(quintilesy == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(restric, na.rm=T))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}     

b_quintil <- function(x) {
  x <- sem2_panel_svy %>%
    filter(quintilesy == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(restric, na.rm=T))
  x <- mean(x$colname)
}       

b_e_quintil <- numeric()

for(i in 1:5){
  b_e_quintil[i] <- b_quintil(x = i)
}     

c_quintil <- as.data.frame(cbind(a_e_quintil, b_e_quintil))
c_quintil <- c_quintil %>% dplyr::mutate(m_quintil = (a_e_quintil + b_e_quintil)/2)
c_quintil <- c_quintil[,c("m_quintil")]


# Sexo

a_sexo <- function(x) {
  x <- sem1_svy %>%
    filter(bc_pe2 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(restric, na.rm=T))
  x <- mean(x$colname)
}       

a_e_sexo <- numeric()

for(i in 1:2){
  a_e_sexo[i] <- a_sexo(x = i)
}     

b_sexo <- function(x) {
  x <- sem2_panel_svy %>%
    filter(bc_pe2 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(restric, na.rm=T))
  x <- mean(x$colname)
}       

b_e_sexo <- numeric()

for(i in 1:2){
  b_e_sexo[i] <- b_sexo(x = i)
}     

for(i in 1:2){
  b_e_sexo[i] <- b_sexo(x = i)
}         

c_sexo <- as.data.frame(cbind(a_e_sexo, b_e_sexo))
c_sexo <- c_sexo %>% dplyr::mutate(m_sexo = (a_e_sexo + b_e_sexo)/2)
c_sexo <- c_sexo[,c("m_sexo")]


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

NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. Este indicador se calcula a partir de la encuesta telefónica del primer semestre de 2021 y el formulario telefónico de modalidad panel del segundo semestre de 2021. En el segundo semestre, el quintil de ingresos del hogar corresponde a los ingresos declarados durante la implantación del panel en la encuesta presencial."

t_07 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, JERARQUIA_CAT, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA, NOTA_INDICADOR)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Promedio de horas trabajadas de forma remunerada
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


# Total

a_mes <- sem1_svy %>%
  srvyr::filter(ocup == 1) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(bc_horas, na.rm=T))

a_sem <- mean(as.numeric(a_mes$colname))

b_mes <- sem2_panel_svy %>%
  srvyr::filter(ocup == 1) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(bc_horas, na.rm=T))

b_sem <- mean(as.numeric(b_mes$colname))

c_ano <- mean(c(a_sem, b_sem))

# Ascendencia étnico racial

a_afro <- function(x) {
  x <- sem1_svy %>%
    filter(bd_e29_1 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_horas, na.rm=T))
  x <- mean(x$colname)
}       

a_e_afro <- numeric()

for(i in 1:2){
  a_e_afro[i] <- a_afro(x = i)
}     

b_afro <- function(x) {
  x <- sem2_panel_svy %>%
    filter(bd_e29_1 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_horas, na.rm=T))
  x <- mean(x$colname)
}       

b_e_afro <- numeric()

for(i in 1:2){
  b_e_afro[i] <- b_afro(x = i)
}     

c_afro <- as.data.frame(cbind(a_e_afro, b_e_afro))
c_afro <- c_afro %>% dplyr::mutate(m_afro = (a_e_afro + b_e_afro)/2)
c_afro <- c_afro[,c("m_afro")]

# Departamento

a_dpto <- function(x) {
  x <- sem1_svy %>%
    filter(dpto == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_horas, na.rm=T))
  x <- mean(x$colname)
}       

a_e_dpto <- numeric()

for(i in 1:19){
  a_e_dpto[i] <- a_dpto(x = i)
}     

b_dpto <- function(x) {
  x <- sem2_panel_svy %>%
    filter(dpto == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_horas, na.rm=T))
  x <- mean(x$colname)
}       

b_e_dpto <- numeric()

for(i in 1:19){
  b_e_dpto[i] <- b_dpto(x = i)
}     

c_dpto <- as.data.frame(cbind(a_e_dpto, b_e_dpto))
c_dpto <- c_dpto %>% dplyr::mutate(m_dpto = (a_e_dpto + b_e_dpto)/2)
c_dpto <- c_dpto[,c("m_dpto")]



# Quintil de ingresos

a_quintil <- function(x) {
  x <- sem1_svy %>%
    filter(quintilesy == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_horas, na.rm=T))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}     

b_quintil <- function(x) {
  x <- sem2_panel_svy %>%
    filter(quintilesy == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_horas, na.rm=T))
  x <- mean(x$colname)
}       

b_e_quintil <- numeric()

for(i in 1:5){
  b_e_quintil[i] <- b_quintil(x = i)
}     

c_quintil <- as.data.frame(cbind(a_e_quintil, b_e_quintil))
c_quintil <- c_quintil %>% dplyr::mutate(m_quintil = (a_e_quintil + b_e_quintil)/2)
c_quintil <- c_quintil[,c("m_quintil")]


# Sexo

a_sexo <- function(x) {
  x <- sem1_svy %>%
    filter(bc_pe2 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_horas, na.rm=T))
  x <- mean(x$colname)
}       

a_e_sexo <- numeric()

for(i in 1:2){
  a_e_sexo[i] <- a_sexo(x = i)
}     

b_sexo <- function(x) {
  x <- sem2_panel_svy %>%
    filter(bc_pe2 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_horas, na.rm=T))
  x <- mean(x$colname)
}       

b_e_sexo <- numeric()

for(i in 1:2){
  b_e_sexo[i] <- b_sexo(x = i)
}     

for(i in 1:2){
  b_e_sexo[i] <- b_sexo(x = i)
}         

c_sexo <- as.data.frame(cbind(a_e_sexo, b_e_sexo))
c_sexo <- c_sexo %>% dplyr::mutate(m_sexo = (a_e_sexo + b_e_sexo)/2)
c_sexo <- c_sexo[,c("m_sexo")]


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

NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. Este indicador se calcula a partir de la encuesta telefónica del primer semestre de 2021 y el formulario telefónico de modalidad panel del segundo semestre de 2021. En el segundo semestre, el quintil de ingresos del hogar corresponde a los ingresos declarados durante la implantación del panel en la encuesta presencial."

t_08 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, JERARQUIA_CAT, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA, NOTA_INDICADOR)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de ocupados que trabajan más de 48 horas
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Total

a_mes <- sem1_svy %>%
  srvyr::filter(ocup == 1) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(sobrecarga, na.rm=T))

a_sem <- mean(as.numeric(a_mes$colname))

b_mes <- sem2_panel_svy %>%
  srvyr::filter(ocup == 1) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(sobrecarga, na.rm=T))

b_sem <- mean(as.numeric(b_mes$colname))

c_ano <- mean(c(a_sem, b_sem))

# Ascendencia étnico racial

a_afro <- function(x) {
  x <- sem1_svy %>%
    filter(bd_e29_1 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(sobrecarga, na.rm=T))
  x <- mean(x$colname)
}       

a_e_afro <- numeric()

for(i in 1:2){
  a_e_afro[i] <- a_afro(x = i)
}     

b_afro <- function(x) {
  x <- sem2_panel_svy %>%
    filter(bd_e29_1 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(sobrecarga, na.rm=T))
  x <- mean(x$colname)
}       

b_e_afro <- numeric()

for(i in 1:2){
  b_e_afro[i] <- b_afro(x = i)
}     

c_afro <- as.data.frame(cbind(a_e_afro, b_e_afro))
c_afro <- c_afro %>% dplyr::mutate(m_afro = (a_e_afro + b_e_afro)/2)
c_afro <- c_afro[,c("m_afro")]

# Departamento

a_dpto <- function(x) {
  x <- sem1_svy %>%
    filter(dpto == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(sobrecarga, na.rm=T))
  x <- mean(x$colname)
}       

a_e_dpto <- numeric()

for(i in 1:19){
  a_e_dpto[i] <- a_dpto(x = i)
}     

b_dpto <- function(x) {
  x <- sem2_panel_svy %>%
    filter(dpto == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(sobrecarga, na.rm=T))
  x <- mean(x$colname)
}       

b_e_dpto <- numeric()

for(i in 1:19){
  b_e_dpto[i] <- b_dpto(x = i)
}     

c_dpto <- as.data.frame(cbind(a_e_dpto, b_e_dpto))
c_dpto <- c_dpto %>% dplyr::mutate(m_dpto = (a_e_dpto + b_e_dpto)/2)
c_dpto <- c_dpto[,c("m_dpto")]



# Quintil de ingresos

a_quintil <- function(x) {
  x <- sem1_svy %>%
    filter(quintilesy == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(sobrecarga, na.rm=T))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}     

b_quintil <- function(x) {
  x <- sem2_panel_svy %>%
    filter(quintilesy == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(sobrecarga, na.rm=T))
  x <- mean(x$colname)
}       

b_e_quintil <- numeric()

for(i in 1:5){
  b_e_quintil[i] <- b_quintil(x = i)
}     

c_quintil <- as.data.frame(cbind(a_e_quintil, b_e_quintil))
c_quintil <- c_quintil %>% dplyr::mutate(m_quintil = (a_e_quintil + b_e_quintil)/2)
c_quintil <- c_quintil[,c("m_quintil")]


# Sexo

a_sexo <- function(x) {
  x <- sem1_svy %>%
    filter(bc_pe2 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(sobrecarga, na.rm=T))
  x <- mean(x$colname)
}       

a_e_sexo <- numeric()

for(i in 1:2){
  a_e_sexo[i] <- a_sexo(x = i)
}     

b_sexo <- function(x) {
  x <- sem2_panel_svy %>%
    filter(bc_pe2 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(sobrecarga, na.rm=T))
  x <- mean(x$colname)
}       

b_e_sexo <- numeric()

for(i in 1:2){
  b_e_sexo[i] <- b_sexo(x = i)
}     

for(i in 1:2){
  b_e_sexo[i] <- b_sexo(x = i)
}         

c_sexo <- as.data.frame(cbind(a_e_sexo, b_e_sexo))
c_sexo <- c_sexo %>% dplyr::mutate(m_sexo = (a_e_sexo + b_e_sexo)/2)
c_sexo <- c_sexo[,c("m_sexo")]


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

NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. Este indicador se calcula a partir de la encuesta telefónica del primer semestre de 2021 y el formulario telefónico de modalidad panel del segundo semestre de 2021. En el segundo semestre, el quintil de ingresos del hogar corresponde a los ingresos declarados durante la implantación del panel en la encuesta presencial."

t_09 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, JERARQUIA_CAT, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA, NOTA_INDICADOR)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Salario promedio por hora en ocupación principal
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Total

a_mes <- sem1_svy %>%
  srvyr::filter(ocup == 1 & F85>0) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))

a_sem <- mean(as.numeric(a_mes$colname))

b_mes <- sem2_implant_svy %>%
  srvyr::filter(ocup == 1 & f85>0) %>%
  srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))

b_sem <- mean(as.numeric(b_mes$colname))

c_ano <- mean(c(a_sem, b_sem))

# Ascendencia étnico racial

a_afro <- function(x) {
  x <- sem1_svy %>%
    filter(bd_e29_1 == x & ocup == 1 & F85>0) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))
  x <- mean(x$colname)
}       

a_e_afro <- numeric()

for(i in 1:2){
  a_e_afro[i] <- a_afro(x = i)
}     

b_afro <- function(x) {
  x <- sem2_implant_svy %>%
    filter(bd_e29_1 == x & ocup == 1 & f85>0) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))
  x <- mean(x$colname)
}       

b_e_afro <- numeric()

for(i in 1:2){
  b_e_afro[i] <- b_afro(x = i)
}     

c_afro <- as.data.frame(cbind(a_e_afro, b_e_afro))
c_afro <- c_afro %>% dplyr::mutate(m_afro = (a_e_afro + b_e_afro)/2)
c_afro <- c_afro[,c("m_afro")]

# Departamento

a_dpto <- function(x) {
  x <- sem1_svy %>%
    filter(dpto == x & ocup == 1 & F85>0) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))
  x <- mean(x$colname)
}       

a_e_dpto <- numeric()

for(i in 1:19){
  a_e_dpto[i] <- a_dpto(x = i)
}     

b_dpto <- function(x) {
  x <- sem2_implant_svy %>%
    filter(dpto == x & ocup == 1 & f85>0) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))
  x <- mean(x$colname)
}       

b_e_dpto <- numeric()

for(i in 1:19){
  b_e_dpto[i] <- b_dpto(x = i)
}     

c_dpto <- as.data.frame(cbind(a_e_dpto, b_e_dpto))
c_dpto <- c_dpto %>% dplyr::mutate(m_dpto = (a_e_dpto + b_e_dpto)/2)
c_dpto <- c_dpto[,c("m_dpto")]



# Quintil de ingresos

a_quintil <- function(x) {
  x <- sem1_svy %>%
    filter(quintilesy == x & ocup == 1 & F85>0) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}     

b_quintil <- function(x) {
  x <- sem2_implant_svy %>%
    filter(quintilesy == x & ocup == 1 & f85>0) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))
  x <- mean(x$colname)
}       

b_e_quintil <- numeric()

for(i in 1:5){
  b_e_quintil[i] <- b_quintil(x = i)
}     

c_quintil <- as.data.frame(cbind(a_e_quintil, b_e_quintil))
c_quintil <- c_quintil %>% dplyr::mutate(m_quintil = (a_e_quintil + b_e_quintil)/2)
c_quintil <- c_quintil[,c("m_quintil")]


# Sexo

a_sexo <- function(x) {
  x <- sem1_svy %>%
    filter(bc_pe2 == x & ocup == 1 & F85>0) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))
  x <- mean(x$colname)
}       

a_e_sexo <- numeric()

for(i in 1:2){
  a_e_sexo[i] <- a_sexo(x = i)
}     

b_sexo <- function(x) {
  x <- sem2_implant_svy %>%
    filter(bc_pe2 == x & ocup == 1 & f85>0) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef, na.rm=T))
  x <- mean(x$colname)
}       

b_e_sexo <- numeric()

for(i in 1:2){
  b_e_sexo[i] <- b_sexo(x = i)
}     

for(i in 1:2){
  b_e_sexo[i] <- b_sexo(x = i)
}         

c_sexo <- as.data.frame(cbind(a_e_sexo, b_e_sexo))
c_sexo <- c_sexo %>% dplyr::mutate(m_sexo = (a_e_sexo + b_e_sexo)/2)
c_sexo <- c_sexo[,c("m_sexo")]


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

NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. Este indicador se calcula a partir de la encuesta telefónica del primer semestre de 2021 y el formulario presencial de implantación del panel del segundo semestre de 2021."

t_10 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, JERARQUIA_CAT, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA, NOTA_INDICADOR)




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de personas ocupadas que perciben ingresos por debajo de la línea de pobreza
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


# Total

a_mes <- sem1_svy %>%
  srvyr::filter(ocup == 1 & bc_horas>40) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(salario_insuf, na.rm=T))

a_sem <- mean(as.numeric(a_mes$colname))

b_mes <- sem2_implant_svy %>%
  srvyr::filter(ocup == 1 & bc_horas>40) %>%
  srvyr::summarise(colname = srvyr::survey_mean(salario_insuf, na.rm=T))

b_sem <- mean(as.numeric(b_mes$colname))

c_ano <- mean(c(a_sem, b_sem))

# Ascendencia étnico racial

a_afro <- function(x) {
  x <- sem1_svy %>%
    filter(bd_e29_1 == x & ocup == 1 & bc_horas>40) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(salario_insuf, na.rm=T))
  x <- mean(x$colname)
}       

a_e_afro <- numeric()

for(i in 1:2){
  a_e_afro[i] <- a_afro(x = i)
}     

b_afro <- function(x) {
  x <- sem2_implant_svy %>%
    filter(bd_e29_1 == x & ocup == 1 &bc_horas>40) %>%
    srvyr::summarise(colname = srvyr::survey_mean(salario_insuf, na.rm=T))
  x <- mean(x$colname)
}       

b_e_afro <- numeric()

for(i in 1:2){
  b_e_afro[i] <- b_afro(x = i)
}     

c_afro <- as.data.frame(cbind(a_e_afro, b_e_afro))
c_afro <- c_afro %>% dplyr::mutate(m_afro = (a_e_afro + b_e_afro)/2)
c_afro <- c_afro[,c("m_afro")]

# Departamento

a_dpto <- function(x) {
  x <- sem1_svy %>%
    filter(dpto == x & ocup == 1 & bc_horas>40) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(salario_insuf, na.rm=T))
  x <- mean(x$colname)
}       

a_e_dpto <- numeric()

for(i in 1:19){
  a_e_dpto[i] <- a_dpto(x = i)
}     

b_dpto <- function(x) {
  x <- sem2_implant_svy %>%
    filter(dpto == x & ocup == 1 & bc_horas>40) %>%
    srvyr::summarise(colname = srvyr::survey_mean(salario_insuf, na.rm=T))
  x <- mean(x$colname)
}       

b_e_dpto <- numeric()

for(i in 1:19){
  b_e_dpto[i] <- b_dpto(x = i)
}     

c_dpto <- as.data.frame(cbind(a_e_dpto, b_e_dpto))
c_dpto <- c_dpto %>% dplyr::mutate(m_dpto = (a_e_dpto + b_e_dpto)/2)
c_dpto <- c_dpto[,c("m_dpto")]



# Quintil de ingresos

a_quintil <- function(x) {
  x <- sem1_svy %>%
    filter(quintilesy == x & ocup == 1 & bc_horas>40) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(salario_insuf, na.rm=T))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}     

b_quintil <- function(x) {
  x <- sem2_implant_svy %>%
    filter(quintilesy == x & ocup == 1 & bc_horas>40) %>%
    srvyr::summarise(colname = srvyr::survey_mean(salario_insuf, na.rm=T))
  x <- mean(x$colname)
}       

b_e_quintil <- numeric()

for(i in 1:5){
  b_e_quintil[i] <- b_quintil(x = i)
}     

c_quintil <- as.data.frame(cbind(a_e_quintil, b_e_quintil))
c_quintil <- c_quintil %>% dplyr::mutate(m_quintil = (a_e_quintil + b_e_quintil)/2)
c_quintil <- c_quintil[,c("m_quintil")]


# Sexo

a_sexo <- function(x) {
  x <- sem1_svy %>%
    filter(bc_pe2 == x & ocup == 1 & bc_horas>40) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(salario_insuf, na.rm=T))
  x <- mean(x$colname)
}       

a_e_sexo <- numeric()

for(i in 1:2){
  a_e_sexo[i] <- a_sexo(x = i)
}     

b_sexo <- function(x) {
  x <- sem2_implant_svy %>%
    filter(bc_pe2 == x & ocup == 1 & bc_horas>40) %>%
    srvyr::summarise(colname = srvyr::survey_mean(salario_insuf, na.rm=T))
  x <- mean(x$colname)
}       

b_e_sexo <- numeric()

for(i in 1:2){
  b_e_sexo[i] <- b_sexo(x = i)
}     

for(i in 1:2){
  b_e_sexo[i] <- b_sexo(x = i)
}         

c_sexo <- as.data.frame(cbind(a_e_sexo, b_e_sexo))
c_sexo <- c_sexo %>% dplyr::mutate(m_sexo = (a_e_sexo + b_e_sexo)/2)
c_sexo <- c_sexo[,c("m_sexo")]


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

NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. Este indicador se calcula a partir de la encuesta telefónica del primer semestre de 2021 y el formulario presencial de implantación del panel del segundo semestre de 2021."

t_11 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, JERARQUIA_CAT, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA, NOTA_INDICADOR)




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de personas ocupadas que perciben ingresos por debajo del Salario Mïnimo Nacional
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


# Total

a_mes <- sem1_svy %>%
  srvyr::filter(ocup == 1) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(salario_insuf_smn, na.rm=T))

a_sem <- mean(as.numeric(a_mes$colname))

b_mes <- sem2_implant_svy %>%
  srvyr::filter(ocup == 1) %>%
  srvyr::summarise(colname = srvyr::survey_mean(salario_insuf_smn, na.rm=T))

b_sem <- mean(as.numeric(b_mes$colname))

c_ano <- mean(c(a_sem, b_sem))

# Ascendencia étnico racial

a_afro <- function(x) {
  x <- sem1_svy %>%
    filter(bd_e29_1 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(salario_insuf_smn, na.rm=T))
  x <- mean(x$colname)
}       

a_e_afro <- numeric()

for(i in 1:2){
  a_e_afro[i] <- a_afro(x = i)
}     

b_afro <- function(x) {
  x <- sem2_implant_svy %>%
    filter(bd_e29_1 == x & ocup == 1) %>%
    srvyr::summarise(colname = srvyr::survey_mean(salario_insuf_smn, na.rm=T))
  x <- mean(x$colname)
}       

b_e_afro <- numeric()

for(i in 1:2){
  b_e_afro[i] <- b_afro(x = i)
}     

c_afro <- as.data.frame(cbind(a_e_afro, b_e_afro))
c_afro <- c_afro %>% dplyr::mutate(m_afro = (a_e_afro + b_e_afro)/2)
c_afro <- c_afro[,c("m_afro")]

# Departamento

a_dpto <- function(x) {
  x <- sem1_svy %>%
    filter(dpto == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(salario_insuf_smn, na.rm=T))
  x <- mean(x$colname)
}       

a_e_dpto <- numeric()

for(i in 1:19){
  a_e_dpto[i] <- a_dpto(x = i)
}     

b_dpto <- function(x) {
  x <- sem2_implant_svy %>%
    filter(dpto == x & ocup == 1) %>%
    srvyr::summarise(colname = srvyr::survey_mean(salario_insuf_smn, na.rm=T))
  x <- mean(x$colname)
}       

b_e_dpto <- numeric()

for(i in 1:19){
  b_e_dpto[i] <- b_dpto(x = i)
}     

c_dpto <- as.data.frame(cbind(a_e_dpto, b_e_dpto))
c_dpto <- c_dpto %>% dplyr::mutate(m_dpto = (a_e_dpto + b_e_dpto)/2)
c_dpto <- c_dpto[,c("m_dpto")]



# Quintil de ingresos

a_quintil <- function(x) {
  x <- sem1_svy %>%
    filter(quintilesy == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(salario_insuf_smn, na.rm=T))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}     

b_quintil <- function(x) {
  x <- sem2_implant_svy %>%
    filter(quintilesy == x & ocup == 1 & bc_horas>40) %>%
    srvyr::summarise(colname = srvyr::survey_mean(salario_insuf_smn, na.rm=T))
  x <- mean(x$colname)
}       

b_e_quintil <- numeric()

for(i in 1:5){
  b_e_quintil[i] <- b_quintil(x = i)
}     

c_quintil <- as.data.frame(cbind(a_e_quintil, b_e_quintil))
c_quintil <- c_quintil %>% dplyr::mutate(m_quintil = (a_e_quintil + b_e_quintil)/2)
c_quintil <- c_quintil[,c("m_quintil")]


# Sexo

a_sexo <- function(x) {
  x <- sem1_svy %>%
    filter(bc_pe2 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(salario_insuf_smn, na.rm=T))
  x <- mean(x$colname)
}       

a_e_sexo <- numeric()

for(i in 1:2){
  a_e_sexo[i] <- a_sexo(x = i)
}     

b_sexo <- function(x) {
  x <- sem2_implant_svy %>%
    filter(bc_pe2 == x & ocup == 1) %>%
    srvyr::summarise(colname = srvyr::survey_mean(salario_insuf_smn, na.rm=T))
  x <- mean(x$colname)
}       

b_e_sexo <- numeric()

for(i in 1:2){
  b_e_sexo[i] <- b_sexo(x = i)
}     

for(i in 1:2){
  b_e_sexo[i] <- b_sexo(x = i)
}         

c_sexo <- as.data.frame(cbind(a_e_sexo, b_e_sexo))
c_sexo <- c_sexo %>% dplyr::mutate(m_sexo = (a_e_sexo + b_e_sexo)/2)
c_sexo <- c_sexo[,c("m_sexo")]


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

NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. Este indicador se calcula a partir de la encuesta telefónica del primer semestre de 2021 y el formulario presencial de implantación del panel del segundo semestre de 2021."

t_12 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, JERARQUIA_CAT, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA, NOTA_INDICADOR)




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de ocupados que viven en hogares en situación de pobreza
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


# Total

a_mes <- sem1_svy %>%
  srvyr::filter(ocup == 1) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(pobre06, na.rm=T))

a_sem <- mean(as.numeric(a_mes$colname))

b_mes <- sem2_implant_svy %>%
  srvyr::filter(ocup == 1) %>%
  srvyr::summarise(colname = srvyr::survey_mean(pobre, na.rm=T))

b_sem <- mean(as.numeric(b_mes$colname))

c_ano <- mean(c(a_sem, b_sem))

# Ascendencia étnico racial

a_afro <- function(x) {
  x <- sem1_svy %>%
    filter(bd_e29_1 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pobre06, na.rm=T))
  x <- mean(x$colname)
}       

a_e_afro <- numeric()

for(i in 1:2){
  a_e_afro[i] <- a_afro(x = i)
}     

b_afro <- function(x) {
  x <- sem2_implant_svy %>%
    filter(bd_e29_1 == x & ocup == 1) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pobre, na.rm=T))
  x <- mean(x$colname)
}       

b_e_afro <- numeric()

for(i in 1:2){
  b_e_afro[i] <- b_afro(x = i)
}     

c_afro <- as.data.frame(cbind(a_e_afro, b_e_afro))
c_afro <- c_afro %>% dplyr::mutate(m_afro = (a_e_afro + b_e_afro)/2)
c_afro <- c_afro[,c("m_afro")]

# Departamento

a_dpto <- function(x) {
  x <- sem1_svy %>%
    filter(dpto == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pobre06, na.rm=T))
  x <- mean(x$colname)
}       

a_e_dpto <- numeric()

for(i in 1:19){
  a_e_dpto[i] <- a_dpto(x = i)
}     

b_dpto <- function(x) {
  x <- sem2_implant_svy %>%
    filter(dpto == x & ocup == 1) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pobre, na.rm=T))
  x <- mean(x$colname)
}       

b_e_dpto <- numeric()

for(i in 1:19){
  b_e_dpto[i] <- b_dpto(x = i)
}     

c_dpto <- as.data.frame(cbind(a_e_dpto, b_e_dpto))
c_dpto <- c_dpto %>% dplyr::mutate(m_dpto = (a_e_dpto + b_e_dpto)/2)
c_dpto <- c_dpto[,c("m_dpto")]



# Quintil de ingresos

a_quintil <- function(x) {
  x <- sem1_svy %>%
    filter(quintilesy == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pobre06, na.rm=T))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}     

b_quintil <- function(x) {
  x <- sem2_implant_svy %>%
    filter(quintilesy == x & ocup == 1 & bc_horas>40) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pobre, na.rm=T))
  x <- mean(x$colname)
}       

b_e_quintil <- numeric()

for(i in 1:5){
  b_e_quintil[i] <- b_quintil(x = i)
}     

c_quintil <- as.data.frame(cbind(a_e_quintil, b_e_quintil))
c_quintil <- c_quintil %>% dplyr::mutate(m_quintil = (a_e_quintil + b_e_quintil)/2)
c_quintil <- c_quintil[,c("m_quintil")]


# Sexo

a_sexo <- function(x) {
  x <- sem1_svy %>%
    filter(bc_pe2 == x & ocup == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pobre06, na.rm=T))
  x <- mean(x$colname)
}       

a_e_sexo <- numeric()

for(i in 1:2){
  a_e_sexo[i] <- a_sexo(x = i)
}     

b_sexo <- function(x) {
  x <- sem2_implant_svy %>%
    filter(bc_pe2 == x & ocup == 1) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pobre, na.rm=T))
  x <- mean(x$colname)
}       

b_e_sexo <- numeric()

for(i in 1:2){
  b_e_sexo[i] <- b_sexo(x = i)
}     

for(i in 1:2){
  b_e_sexo[i] <- b_sexo(x = i)
}         

c_sexo <- as.data.frame(cbind(a_e_sexo, b_e_sexo))
c_sexo <- c_sexo %>% dplyr::mutate(m_sexo = (a_e_sexo + b_e_sexo)/2)
c_sexo <- c_sexo[,c("m_sexo")]


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

NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. Este indicador se calcula a partir de la encuesta telefónica del primer semestre de 2021 y el formulario presencial de implantación del panel del segundo semestre de 2021."

t_13 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, JERARQUIA_CAT, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA, NOTA_INDICADOR)





# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
### Generación de Base motor ###
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #



t_2021 <- rbind(t_01, t_02, t_03, t_04, t_05, t_06, t_07, t_08, t_09, t_10, t_11, t_12, t_13)

rio::export(t_2021, "t_2021.xlsx" )


