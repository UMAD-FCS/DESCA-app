### Mirador DESCA
### Unidad de Métodos y Acceso a datos
### Genero

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


library(rio)
library(grid)
library(Matrix)
library(survival)
library(survey)
library(srvyr)
library(tidyverse)
library(laeken)
library(statar)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

### Carga de bases ###

ech23  <- rio::import("C:/Users/Usuario/Dropbox/UMAD/Sociodemografico/GITHUB DEFINITIVOS/Genero/Bases/ECH_2023.csv")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

### Generación de nuevas variables ###
ech23 <- ech23 %>% dplyr::mutate(bc_correlat = ID)

### Renombro condición de actividad
ech23 <- ech23 %>% dplyr::mutate(bc_pobp = POBPCOAC)


##Ingresos propios
ech23 <- ech23 %>% dplyr::mutate(ypropio = case_when(PT1>0~ 1, PT1==0 ~ 0)) 

##Ocupados
ech23 <- ech23 %>% dplyr::mutate(ocup = case_when(POBPCOAC==2 ~ 1, POBPCOAC>2 ~ 0)) 

##Menores de 12 en el hogar
ech23 <- ech23 %>% dplyr::mutate(menor12 = case_when(e27<13 ~ 1, e27>12 ~ 0)) 
ech23 <- ech23 %>% group_by(bc_correlat) %>% mutate(menor12H_aux=max(menor12)) %>% as.data.frame()
ech23 <- ech23 %>% mutate(menor12H = case_when(menor12H_aux==0 ~ 2, menor12H_aux==1 ~ 1))
                                                

##Activos
ech23 <- ech23 %>% dplyr::mutate(activos = case_when(POBPCOAC>=2&POBPCOAC<=5 ~ 1, POBPCOAC>5 ~ 0)) 

##Ingresos por hora

ech23 <- ech23 %>% dplyr::mutate(horamen = f85*4.3) 
ech23 <- ech23 %>% dplyr::mutate(yhora = (case_when( horamen>0 ~ PT2/horamen, horamen==0 ~ 0))) 
ech23 <- ech23 %>% dplyr::mutate(bc_ipc_tot = case_when(
  mes == 1  ~ 0.286995721,
  mes == 2  ~ 0.282605309,
  mes == 3  ~ 0.279798078,
  mes == 4  ~ 0.277298982,
  mes == 5  ~ 0.275240151,
  mes == 6  ~ 0.275267734,
  mes == 7  ~ 0.276549203,
  mes == 8  ~ 0.277553197,
  mes == 9  ~ 0.277076807,
  mes == 10 ~ 0.275395538,
  mes == 11 ~ 0.273682943,
  mes == 12 ~ 0.272746995))
ech23 <- ech23 %>% dplyr::mutate(yhoradef = yhora/bc_ipc_tot) 



# Sexo
ech23 <- ech23 %>% dplyr::mutate(bc_pe2 = e26)
ech23 <- ech23 %>% dplyr::mutate(sexo1 = case_when(bc_pe2==1 ~ 1, bc_pe2==2 ~ 0))
ech23 <- ech23 %>% dplyr::mutate(sexo2 = case_when(bc_pe2==2 ~ 1, bc_pe2==1 ~ 0))

# Ascendencia afro
ech23 <- ech23 %>% dplyr::mutate(bd_e29_1 = e29_1)

ech23 <- ech23 %>% dplyr::mutate(afro1 = case_when(bd_e29_1==1 ~ 1, bd_e29_1==2 ~ 0))
ech23 <- ech23 %>% dplyr::mutate(afro2 = case_when(bd_e29_1==2 ~ 1, bd_e29_1==1 ~ 0))


# Edad
ech23 <- ech23 %>% dplyr::mutate(bc_pe3=e27)
ech23 <- ech23 %>% dplyr::mutate(tramo = case_when(e27>=14 & e27 <=29 ~ 1,
                                                  e27>=30 & e27 <=44 ~ 2,
                                                  e27>=45 & e27 <=64 ~ 3,
                                                  e27>=65 ~ 4))

ech23 <- ech23 %>% dplyr::mutate(tramo_2 = case_when(e27>=14 & e27 <=29 ~ 1,
                                                                 e27>=30 & e27 <=49 ~ 2))



ech23 <- ech23 %>% dplyr::mutate(tramo1 = case_when(tramo==1 ~ 1, tramo!=1 ~ 0))
ech23 <- ech23 %>% dplyr::mutate(tramo2 = case_when(tramo==2 ~ 1, tramo!=2 ~ 0))
ech23 <- ech23 %>% dplyr::mutate(tramo3 = case_when(tramo==3 ~ 1, tramo!=3 ~ 0))
ech23 <- ech23 %>% dplyr::mutate(tramo4 = case_when(tramo==4 ~ 1, tramo!=4 ~ 0))

##Relación de parentesco
ech23 <- ech23 %>% dplyr::mutate(bc_pe4 = e30)

# Quintil de ingresos
ech23 <- ech23 %>% dplyr::mutate(y_pc       =  HT11 / HT19 )                      #Ingreso per-cápita
ech23 <- ech23 %>% dplyr::mutate(y_pc_svl   =  (HT11 - HT13) / HT19)              #Ingreso per-cápita sin valor locativo

ECH_h <- ech23 %>% distinct(bc_correlat, .keep_all = TRUE)
ECH_h <- ECH_h %>% dplyr::mutate(quintilesy = statar::xtile(y_pc, n=5, wt = W_ANO))
ECH_h <- ECH_h[,c("bc_correlat","quintilesy")]
ech23 <- merge(ech23, ECH_h, by = "bc_correlat")   

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

### Información de muestreo ### 

ech23_svy   <- srvyr::as_survey_design(ech23, ids = ID, weights = W_ANO)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
### Procesamiento de indicadores ###
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de mujeres sin ingresos propios
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Total

b_tot <- ech23_svy %>%
  filter(bc_pe2==2&bc_pe3>23) %>%
  srvyr::summarise(colname = srvyr::survey_mean(ypropio))

b_tot <b_tot$colname

c_ano <- 1-b_tot[[1]]

# Tramo de edad

b_edad <- function(x) {
  x <- ech23_svy %>%
    filter(bc_pe2==2&bc_pe3>23&tramo == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(ypropio))
  x <- mean(x$colname)
}       

b_e_edad <- numeric()

for(i in 1:4){
  b_e_edad[i] <- b_edad(x = i)
}         

c_edad <- 1-b_e_edad

# Quintil de ingresos

b_quintil <- function(x) {
  x <- ech23_svy %>%
    filter(bc_pe2==2&bc_pe3>23&quintilesy == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(ypropio))
  x <- mean(x$colname)
}       

b_e_quintil <- numeric()

for(i in 1:5){
  b_e_quintil[i] <- b_quintil(x = i)
}         

c_quintil <- 1-b_e_quintil

# Ascendencia étnico racial

b_afro <- function(x) {
  x <- ech23_svy %>%
    filter(bc_pe2==2&bc_pe3>23&bd_e29_1 == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(ypropio))
  x <- mean(x$colname)
}       

b_e_afro <- numeric()

for(i in 1:2){
  b_e_afro[i] <- b_afro(x = i)
}         

c_afro <- 1-b_e_afro


CODIND       <- "8300109"
POBLACION    <- "Mujeres"
DERECHO      <- "Trabajo"
TIPOIND      <- "Resultados"
DIMENSIÓN    <- "Accesibilidad"
CODINDICADOR <- "Autonomía económica"
NOMINDICADOR <- "Porcentaje de mujeres de 24 años o más sin ingresos propios"
AÑO	         <- 2023
fecha_cat    <- ""
VALOR	       <- c(c_ano, c_edad, c_quintil, c_afro)
FUENTE	     <- "Encuesta Continua de Hogares 2023, INE"
RESPONSABLE	 <- "S. Katzkowicz"
JERARQUIA	   <- c("1", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0")
JERARQUIA_CAT<- 1
CORTE        <- c("Total", "Edad", "Edad", "Edad", "Edad", "Quintil de ingresos",
                  "Quintil de ingresos", "Quintil de ingresos", "Quintil de ingresos", "Quintil de ingresos",
                  "Ascendencia étnico-racial", "Ascendencia étnico-racial")
CORTE_2      <- ""
DEPARTAMENTO <- ""
QUINTIL	     <- c("", "", "", "", "", "Quintil 1",
                  "Quintil 2", "Quintil 3", "Quintil 4", "Quintil 5",
                  "", "")
SEXO		     <- ""
EDAD         <- c("", "Ente 24 y 29 años", "Entre 30 y 44 años", "Entre 45 y 64 años", "65 años o más", "",
                  "", "", "", "",
                  "", "")
ASCENDENCIA  <- c("", "", "", "", "", "",
                  "", "", "", "",
                  "Afrodescendientes", "No afrodescendientes")
MENORESH     <- ""
genero_01 <- cbind(CODIND, POBLACION,DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, fecha_cat, VALOR, FUENTE, RESPONSABLE, JERARQUIA, JERARQUIA_CAT, CORTE, CORTE_2, DEPARTAMENTO,QUINTIL, SEXO, EDAD, ASCENDENCIA, MENORESH)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de mujeres que no perciben ingresos propios
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

CODIND       <- "8200105"
POBLACION    <- "Mujeres"
DERECHO      <- "Seguridad social"
TIPOIND      <- "Resultados"
DIMENSIÓN    <- "Accesibilidad"
CODINDICADOR <- "Autonomía económica - sin ingresos propios"
NOMINDICADOR <- "Porcentaje de mujeres de 24 años o más que no perciben ingresos propios"
AÑO	         <- 2023
fecha_cat    <- ""
VALOR	       <- c(c_ano, c_edad, c_quintil, c_afro)
FUENTE	     <- "Encuesta Continua de Hogares 2023, INE"
RESPONSABLE	 <- "S. Katzkowicz"
JERARQUIA	   <- c("1", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0")
JERARQUIA_CAT<- 1
CORTE        <- c("Total", "Edad", "Edad", "Edad", "Edad", "Quintil de ingresos",
                  "Quintil de ingresos", "Quintil de ingresos", "Quintil de ingresos", "Quintil de ingresos",
                  "Ascendencia étnico-racial", "Ascendencia étnico-racial")
CORTE_2      <- ""
DEPARTAMENTO <- ""
QUINTIL	     <- c("", "", "", "", "", "Quintil 1",
                  "Quintil 2", "Quintil 3", "Quintil 4", "Quintil 5",
                  "", "")
SEXO		     <- ""
EDAD         <- c("", "Ente 24 y 29 años", "Entre 30 y 44 años", "Entre 45 y 64 años", "65 años o más", "",
                  "", "", "", "",
                  "", "")
ASCENDENCIA  <- c("", "", "", "", "", "",
                  "", "", "", "",
                  "Afrodescendientes", "No afrodescendientes")
MENORESH     <- ""
genero_02 <- cbind(CODIND, POBLACION,DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, fecha_cat, VALOR, FUENTE, RESPONSABLE, JERARQUIA, JERARQUIA_CAT, CORTE, CORTE_2, DEPARTAMENTO,QUINTIL, SEXO, EDAD, ASCENDENCIA, MENORESH)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Tasa de actividad de varones y mujeres entre 14 y 49 años de edad, según presencia de menores de 13 años en el hogar. 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


# Sexo, con menores

b_sexo <- function(x) {
  x <- ech23_svy %>%
    filter(bc_pe3 >= 14 & bc_pe3 <= 49 & menor12H == 1 & bc_pe2 == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(activos))
  x <- mean(x$colname)
}       

b_e_sexo <- numeric()

for(i in 1:2){
  b_e_sexo[i] <- b_sexo(x = i)
}         

c_sexo_men <- b_e_sexo

# Sexo, sin menores

b_sexo <- function(x) {
  x <- ech23_svy %>%
    filter(bc_pe3 >= 14 & bc_pe3 <= 49 & menor12H == 2 & bc_pe2 == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(activos))
  x <- mean(x$colname)
}       

b_e_sexo <- numeric()

for(i in 1:2){
  b_e_sexo[i] <- b_sexo(x = i)
}         

c_sexo_smen <- b_e_sexo



CODIND       <- "8300111"
POBLACION    <- "Mujeres"
DERECHO      <- "Trabajo"
TIPOIND      <- "Resultados"
DIMENSI�N    <- "Accesibilidad"
CODINDICADOR <- "Participación laboral con menores"
NOMINDICADOR <- "Tasa de actividad de varones y mujeres entre 14 y 49 años de edad, según presencia de menores de 13 años en el hogar"
A�O	         <- 2023
fecha_cat    <- ""
VALOR	       <- c(c_sexo_men, c_sexo_smen)
FUENTE	     <- "Encuesta Continua de Hogares 2023, INE"
RESPONSABLE	 <- "S. Katzkowicz"
JERARQUIA	   <- 1
JERARQUIA_CAT<- 1
CORTE        <- c("Sexo", "Sexo", "Sexo", "Sexo")
CORTE_2      <- "Menores de 13 en el hogar"
DEPARTAMENTO <- ""
QUINTIL	     <- ""
SEXO		     <- c("Varones", "Mujeres", "Varones", "Mujeres")
EDAD         <- ""
ASCENDENCIA  <- ""
MENORESH     <- c("Con menores en el hogar", "Con menores en el hogar", "Sin menores en el hogar", "Sin menores en el hogar")
genero_03 <- cbind(CODIND, POBLACION,DERECHO, TIPOIND, DIMENSI�N, CODINDICADOR, NOMINDICADOR, A�O, fecha_cat, VALOR, FUENTE, RESPONSABLE, JERARQUIA, JERARQUIA_CAT, CORTE, CORTE_2, DEPARTAMENTO,QUINTIL, SEXO, EDAD, ASCENDENCIA, MENORESH)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Tasa de actividad de varones y mujeres jefe, jefa y cónyuge entre 14 y 49 años de edad, según sexo y presencia de menores de 13 años en el hogar (SIN MENORES) 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Total

b_tot <- ech23_svy %>%
  filter(bc_pe3 >= 14 & bc_pe3 <= 49 & (bc_pe4 == 1 | bc_pe4 == 2) & menor12H == 2) %>%
  srvyr::summarise(colname = srvyr::survey_mean(activos))

b_tot <-b_tot$colname

c_ano <- b_tot

# Sexo

b_sexo <- function(x) {
  x <- ech23_svy %>%
    filter(bc_pe3 >= 14 & bc_pe3 <= 49 & (bc_pe4 == 1 | bc_pe4 == 2) & menor12H == 2 & bc_pe2 == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(activos))
  x <- mean(x$colname)
}       

b_e_sexo <- numeric()

for(i in 1:2){
  b_e_sexo[i] <- b_sexo(x = i)
}         

c_sexo <- b_e_sexo




CODIND       <- "8300111"
POBLACION    <- "Mujeres"
DERECHO      <- "Trabajo"
TIPOIND      <- "Resultados"
DIMENSIÓN    <- "Accesibilidad"
CODINDICADOR <- "Participación laboral con menores"
NOMINDICADOR <- "Tasa de actividad de varones y mujeres jefe, jefa y cónyuge entre 14 y 49 años de edad según sexo y presencia de menores de 13 años en el hogar"
AÑO	         <- 2023
fecha_cat    <- ""
VALOR	       <- c(c_ano, c_sexo)
FUENTE	     <- "Encuesta Continua de Hogares 2023, INE"
RESPONSABLE	 <- "S. Katzkowicz"
JERARQUIA	   <- c("1", "1", "1")
JERARQUIA_CAT<- 1
CORTE        <- c("Total", "Sexo", "Sexo")
CORTE_2      <- ""
DEPARTAMENTO <- ""
QUINTIL	     <- ""
SEXO		     <- c("", "Varones", "Mujeres")
EDAD         <- ""
ASCENDENCIA  <- ""
MENORESH     <- "Sin menores"
genero_03_b <- cbind(CODIND, POBLACION,DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, fecha_cat, VALOR, FUENTE, RESPONSABLE, JERARQUIA, JERARQUIA_CAT, CORTE, CORTE_2, DEPARTAMENTO,QUINTIL, SEXO, EDAD, ASCENDENCIA, MENORESH)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de mujeres en edad de trabajar que no se encuentran trabajando
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Total

b_tot <- ech23_svy %>%
  filter(bc_pe2==2&bc_pe3>13) %>%
  srvyr::summarise(colname = srvyr::survey_mean(ocup))

b_tot <-b_tot$colname

c_ano <- 1-b_tot

# Tramo de edad

b_edad <- function(x) {
  x <- ech23_svy %>%
    filter(bc_pe2==2&bc_pe3>13&tramo == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(ocup))
  x <- mean(x$colname)
}       

b_e_edad <- numeric()

for(i in 1:4){
  b_e_edad[i] <- b_edad(x = i)
}         

c_edad <- 1-b_e_edad


# Quintil de ingresos

b_quintil <- function(x) {
  x <- ech23_svy %>%
    filter(bc_pe2==2&bc_pe3>13&quintilesy == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(ocup))
  x <- mean(x$colname)
}       

b_e_quintil <- numeric()

for(i in 1:5){
  b_e_quintil[i] <- b_quintil(x = i)
}         

c_quintil <- 1-b_e_quintil


# Ascendencia étnico racial
b_afro <- function(x) {
  x <- ech23_svy %>%
    filter(bc_pe2==2&bc_pe3>13&bd_e29_1 == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(ocup))
  x <- mean(x$colname)
}       

b_e_afro <- numeric()

for(i in 1:2){
  b_e_afro[i] <- b_afro(x = i)
}         

c_afro <- 1-b_e_afro


# Menores de 13 en el hogar

b_men <- function(x) {
  x <- ech23_svy %>%
    filter(bc_pe2==2&bc_pe3>13&menor12H == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(ocup))
  x <- mean(x$colname)
}       

b_e_men <- numeric()

for(i in 1:2){
  b_e_men[i] <- b_men(x = i)
}         

c_men <- 1-b_e_men


CODIND       <- "8300110"
POBLACION    <- "Mujeres"
DERECHO      <- "Trabajo"
TIPOIND      <- "Resultados"
DIMENSIÓN    <- "Accesibilidad"
CODINDICADOR <- "Participación laboral"
NOMINDICADOR <- "Porcentaje de mujeres en edad de trabajar que no se encuentran trabajando"
AÑO	         <- 2023
fecha_cat    <- ""
VALOR	       <- c(c_ano, c_edad, c_quintil, c_afro, c_men)
FUENTE	     <- "Encuesta Continua de Hogares 2023, INE"
RESPONSABLE	 <- "S. Katzkowicz"
JERARQUIA	   <- c("1", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0")
JERARQUIA_CAT<- 1
CORTE        <- c("Total", "Edad", "Edad", "Edad", "Edad", "Quintil de ingresos",
                  "Quintil de ingresos", "Quintil de ingresos", 
                  "Quintil de ingresos", "Quintil de ingresos",
                  "Ascendencia étnico-racial", "Ascendencia étnico-racial",
                  "Menores de 13 en el hogar", "Menores de 13 en el hogar")
CORTE_2      <- ""
DEPARTAMENTO <- ""
QUINTIL	     <- c("", "", "", "", "", "Quintil 1",
                  "Quintil 2", "Quintil 3", "Quintil 4", "Quintil 5",
                  "", "", "", "")
SEXO		     <- ""
EDAD         <- c("", "Ente 14 y 29 años", "Entre 30 y 44 años", "Entre 45 y 64 años", "65 años o más", "",
                  "", "", "", "",
                  "", "", "", "")
ASCENDENCIA  <- c("", "", "", "", "", "",
                  "", "", "", "",
                  "Afrodescendientes", "No afrodescendientes","", "")
MENORESH     <- c("", "", "", "", "", "",
                  "", "", "", "",
                  "", "","Con menores", "Sin menores")

genero_04 <- cbind(CODIND, POBLACION, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, fecha_cat, VALOR, FUENTE, RESPONSABLE, JERARQUIA, JERARQUIA_CAT, CORTE, CORTE_2, DEPARTAMENTO,QUINTIL, SEXO, EDAD, ASCENDENCIA, MENORESH)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Brecha de ingresos por sexo por hora de trabajo en ocupación principal
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Total
b_sexo <- function(x) {
  x <- ech23_svy %>%
    filter(bc_pobp==2&bc_pe2 == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef))
  x <- mean(x$colname)
}       

b_e_sexo <- numeric()

for(i in 1:2){
  b_e_sexo[i] <- b_sexo(x = i)
}         

c_sexo_1 <- b_e_sexo
c_ano <- c_sexo_1[2]/c_sexo_1[1]



# Tramo de edad
#Varones
b_edad <- function(x) {
  x <- ech23_svy %>%
    filter(bc_pobp==2&bc_pe2 == 1&tramo == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef))
  x <- mean(x$colname)
}       

b_e_edad <- numeric()

for(i in 1:4){
  b_e_edad[i] <- b_edad(x = i)
}         

c_edad_1 <- b_e_edad

#Mujeres
b_edad <- function(x) {
  x <- ech23_svy %>%
    filter(bc_pobp==2&bc_pe2 == 2&tramo == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef))
  x <- mean(x$colname)
}       

b_e_edad <- numeric()

for(i in 1:4){
  b_e_edad[i] <- b_edad(x = i)
}         

c_edad_2 <- b_e_edad

c_edad <- c_edad_2/c_edad_1

# Quintil de ingresos
#Varones
b_quintil <- function(x) {
  x <- ech23_svy %>%
    filter(bc_pobp==2&bc_pe2==1&quintilesy == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef))
  x <- mean(x$colname)
}       

b_e_quintil <- numeric()

for(i in 1:5){
  b_e_quintil[i] <- b_quintil(x = i)
}         

c_quintil_1 <- b_e_quintil

#Mujeres
b_quintil <- function(x) {
  x <- ech23_svy %>%
    filter(bc_pobp==2&bc_pe2==2&quintilesy == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef))
  x <- mean(x$colname)
}       

b_e_quintil <- numeric()

for(i in 1:5){
  b_e_quintil[i] <- b_quintil(x = i)
}         

c_quintil_2 <- b_e_quintil
c_quintil <- c_quintil_2/c_quintil_1


# Ascendencia étnico racial
b_afro <- function(x) {
  x <- ech23_svy %>%
    filter(bc_pobp==2&bc_pe2==1&bd_e29_1 == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef))
  x <- mean(x$colname)
}       

b_e_afro <- numeric()

for(i in 1:2){
  b_e_afro[i] <- b_afro(x = i)
}         

c_afro_1 <- b_e_afro

#Mujeres
b_afro <- function(x) {
  x <- ech23_svy %>%
    filter(bc_pobp==2&bc_pe2==2&bd_e29_1 == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef))
  x <- mean(x$colname)
}       

b_e_afro <- numeric()

for(i in 1:2){
  b_e_afro[i] <- b_afro(x = i)
}         

c_afro_2 <- b_e_afro
c_afro <- c_afro_2/c_afro_1


# Menores de 12 en el hogar
b_men <- function(x) {
  x <- ech23_svy %>%
    filter(bc_pobp==2&bc_pe2==1&menor12H == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef))
  x <- mean(x$colname)
}       

b_e_men <- numeric()

for(i in 1:2){
  b_e_men[i] <- b_men(x = i)
}         

c_men_1 <- b_e_men

#Mujeres
b_men <- function(x) {
  x <- ech23_svy %>%
    filter(bc_pobp==2&bc_pe2==2&menor12H == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(yhoradef))
  x <- mean(x$colname)
}       

b_e_men <- numeric()

for(i in 1:2){
  b_e_men[i] <- b_men(x = i)
}         

c_men_2 <- b_e_men
c_men <- c_men_2/c_men_1


CODIND       <- "8300201"
POBLACION    <- "Mujeres"
DERECHO      <- "Trabajo"
TIPOIND      <- "Resultados"
DIMENSIÓN    <- "Calidad"
CODINDICADOR <- "Brecha de ingresos"
NOMINDICADOR <- "Porcentaje del ingreso de las mujeres respecto al de los varones por hora de trabajo en ocupación principal"
AÑO	         <- 2023
fecha_cat    <- ""
VALOR	       <- c(c_ano, c_edad, c_quintil, c_afro, c_men)
FUENTE	     <- "Encuesta Continua de Hogares 2023, INE"
RESPONSABLE	 <- "S. Katzkowicz"
JERARQUIA	   <- c("1", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0")
JERARQUIA_CAT<- 1
CORTE        <- c("Total", "Edad", "Edad", "Edad", "Edad",
                  "Quintil de ingresos", "Quintil de ingresos", "Quintil de ingresos", "Quintil de ingresos", "Quintil de ingresos",
                  "Ascendencia étnico-racial", "Ascendencia étnico-racial", 
                  "Menores de 13 en el hogar", "Menores de 13 en el hogar")
CORTE_2      <- ""
DEPARTAMENTO <- ""
QUINTIL	     <- c("", "", "", "", "", 
                  "Quintil 1", "Quintil 2", "Quintil 3", "Quintil 4", "Quintil 5",
                  "", "", 
                  "", "")
SEXO		     <- ""
EDAD         <- c("", "Entre 14 y 29 años", "Entre 30 y 44 años", "Entre 45 y 64 años", "65 o más años",
                  "", "", "", "", "",
                  "", "", 
                  "", "")
ASCENDENCIA  <- c("", "", "", "", "", 
                  "", "", "", "", "",
                  "Afrodescendientes", "No afrodescendientes", 
                  "", "")
MENORESH     <- c("", "", "", "", "", 
                  "", "", "", "", "",
                  "", "", 
                  "Con menores", "Sin menores")
genero_05 <- cbind(CODIND, POBLACION,DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, fecha_cat, VALOR, FUENTE, RESPONSABLE, JERARQUIA, JERARQUIA_CAT, CORTE, CORTE_2, DEPARTAMENTO,QUINTIL, SEXO, EDAD, ASCENDENCIA, MENORESH)






genero_2023 <- rbind(genero_01, genero_02, genero_03, genero_03_b, genero_04, genero_05)

rio::export(genero_2023, "genero_2023.xlsx" )
