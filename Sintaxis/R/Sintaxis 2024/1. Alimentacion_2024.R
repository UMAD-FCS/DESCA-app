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
library(assertthat)
library(psychotools)
library(Hmisc) 
library(RM.weights)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

### Carga de bases ###
ech2024  <- rio::import("data_ech/ECH_2024.csv")
#sa  <- rio::import("Bases/Base_FIES_2024.csv")[,c(1:9,18:20)]
#sa_h <- sa %>% distinct(ID, .keep_all = TRUE)

#ech23 <- merge(sa_h, ech2024, by="ID", all=T)

proyecciones  <- rio::import("Proyecciones.xlsx")
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

### Generación de nuevas variables ###

##Comedor
ech2024 <- ech2024 %>% dplyr::mutate(comedor = case_when(e559==1|e582==1 ~ 1, e559!=1&e582!=1 ~ 0)) 

##Canasta
ech2024 <- ech2024 %>% dplyr::mutate(canasta = case_when(e59==1 ~ 1, e59!=1 ~ 0)) 

##Política alimentaria (excluídas las canastas)
ech2024 <- ech2024 %>% dplyr::mutate(pol_alim_scan = case_when(e559==1|e582==1|e584==1 ~ 1, 
                                                               e559!=1&e582!=1&e584!=1 ~ 0)) 

##Política alimentaria (incluídas las canastas)
ech2024 <- ech2024 %>% dplyr::mutate(pol_alim = case_when(e559==1|e582==1|e584==1|e59==1 ~ 1, 
                                                          e59!=1&e559!=1&e582!=1&e584!=1 ~ 0)) 

# Indigencia
ech2024 <- ech2024 %>% dplyr::mutate(indig = indig06)

# Sexo
ech2024 <- ech2024 %>% dplyr::mutate(bc_pe2 = e26)
ech2024 <- ech2024 %>% dplyr::mutate(sexo1 = case_when(e26==1 ~ 1, e26==2 ~ 0))
ech2024 <- ech2024 %>% dplyr::mutate(sexo2 = case_when(e26==2 ~ 1, e26==1 ~ 0))
                                                                          
# Ascendencia afro
ech2024 <- ech2024 %>% dplyr::mutate(bd_e29_1 = e29_1)
ech2024 <- ech2024 %>% dplyr::mutate(afro1 = case_when(bd_e29_1==1 ~ 1, bd_e29_1==2 ~ 0))
ech2024 <- ech2024 %>% dplyr::mutate(afro2 = case_when(bd_e29_1==2 ~ 1, bd_e29_1==1 ~ 0))
                                                                          
# Edad
ech2024 <- ech2024 %>% dplyr::mutate(tramo1 = case_when(e27>=0 & e27 <=5 ~ 1,
                                                  e27>=6 & e27 <=12 ~ 2,
                                                  e27>=13 & e27 <=17 ~ 3,
                                                  e27>=18 & e27 <=29 ~ 4,
                                                  e27>=30 & e27 <=64 ~ 5,
                                                  e27>=65 ~ 6))
ech2024 <- ech2024 %>% dplyr::mutate(tramo2 = case_when(e27>=0 & e27 <=5 ~ 1,
                                                  e27>=6 & e27 <=12 ~ 2,
                                                  e27>=13 & e27 <=17 ~ 3,
                                                  e27>=18 & e27 <=29 ~ 4,
                                                  e27>=30 ~ 5))
                                                                                             


ech2024 <- ech2024 %>% dplyr::mutate(tramo1_1 = case_when(e27>=0 & e27 <=5 ~ 1, e27>5 ~ 0))
ech2024 <- ech2024 %>% dplyr::mutate(tramo1_2 = case_when(e27>=6 & e27 <=12 ~ 1, e27<6|e27>12 ~ 0))
ech2024 <- ech2024 %>% dplyr::mutate(tramo1_3 = case_when(e27>=13 & e27 <=17 ~ 1, e27<13|e27>17~0))
ech2024 <- ech2024 %>% dplyr::mutate(tramo1_4 = case_when(e27>=18 & e27 <=29 ~ 1, e27<18|e27>29~0))
ech2024 <- ech2024 %>% dplyr::mutate(tramo1_5 = case_when(e27>=30 & e27 <=64 ~ 1, e27<30|e27>64~0))
ech2024 <- ech2024 %>% dplyr::mutate(tramo1_6 = case_when(e27>=65 ~ 1, e27<65 ~ 0))

ech2024 <- ech2024 %>% dplyr::mutate(tramo2_1 = case_when(e27>=0 & e27 <=5 ~ 1, e27>5 ~ 0))
ech2024 <- ech2024 %>% dplyr::mutate(tramo2_2 = case_when(e27>=6 & e27 <=12 ~ 1, e27<6|e27>12 ~ 0))
ech2024 <- ech2024 %>% dplyr::mutate(tramo2_3 = case_when(e27>=13 & e27 <=17 ~ 1, e27<13|e27>17~0))
ech2024 <- ech2024 %>% dplyr::mutate(tramo2_4 = case_when(e27>=18 & e27 <=29 ~ 1, e27<18|e27>29~0))
ech2024 <- ech2024 %>% dplyr::mutate(tramo2_5 = case_when(e27>=30  ~ 1, e27<30~0))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# ech23 <- ech23 %>% dplyr::mutate(inseg_alim_mod_aux = case_when(sa5==1 ~ 1, sa5!=1 ~ 0))
# ech23 <- ech23 %>% dplyr::mutate(inseg_alim_grave_aux = case_when(sa6==1&sa8==1 ~ 1, sa6!=1|sa8!=1 ~ 0))
# ech23 <- ech23 %>% group_by(ID) %>% mutate(inseg_alim_mod=max(inseg_alim_mod_aux)) %>% as.data.frame()
# ech23 <- ech23 %>% group_by(ID) %>% mutate(inseg_alim_grave=max(inseg_alim_grave_aux)) %>% as.data.frame()
# 
# ech23 <- ech23 %>% dplyr::mutate(pondera = case_when(is.na(w)==F ~ w, is.na(w)==T ~ 0))

### Información de muestreo ### 

ech2024_svy   <- srvyr::as_survey_design(ech2024, ids = ID, weights = W_ANO)
# ech23_sa_svy   <- srvyr::as_survey_design(ech23, ids = ID, weights = pondera)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
### Procesamiento de indicadores ###
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de personas en hogares con ingresos menores a una CBA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Total

c_ano <- ech2024_svy %>%
  srvyr::summarise(colname = srvyr::survey_mean(indig))

c_ano <-c_ano$colname



# Sexo

c_sexo <- function(x) {
  x <- ech2024_svy %>%
    filter(bc_pe2 == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(indig))
  x <- mean(x$colname)
}       

c_e_sexo <- numeric()

for(i in 1:2){
  c_e_sexo[i] <- c_sexo(x = i)
}         

c_sexo <- as.data.frame(c_e_sexo)


# Ascendencia étnico racial

c_afro <- function(x) {
  x <- ech2024_svy %>%
    filter(bd_e29_1 == x) %>%
    srvyr::summarise(colname = srvyr::survey_mean(indig))
  x <- mean(x$colname)
}       

c_e_afro <- numeric()

for(i in 1:2){
  c_e_afro[i] <- c_afro(x = i)
}         

c_afro <- as.data.frame(c_e_afro)


CODIND       <- "730201"
POBLACION    <- c("", "Mujeres", "Mujeres", "Afrodescendientes","Afrodescendientes")
DERECHO      <- "Alimentación"
TIPOIND      <- "Resultados"
DIMENSIÓN    <- "Accesibilidad"
CODINDICADOR <- "Insuficiencia de ingresos"
NOMINDICADOR <- "Porcentaje de personas en hogares con ingresos menores a una CBA"
AÑO	         <- 2024
fecha_cat    <- ""
VALOR	       <- c(c_ano, c_sexo[1,1], c_sexo[2,1], c_afro[1,1], c_afro[2,1])*100
FUENTE	     <- "Encuesta Continua de Hogares 2024, INE"
RESPONSABLE	 <- "S. Katzkowicz"
JERARQUIA	   <- 1
JERARQUIA_CAT<- ""
CORTE        <- c("Total", "Sexo", "Sexo", "Ascendencia étnico-racial", "Ascendencia étnico-racial")
CORTE_2      <- ""
DEPARTAMENTO <- ""
QUINTIL	     <- ""
SEXO		     <- c("", "Varones", "Mujeres", "", "")
EDAD         <- ""
ASCENDENCIA  <- c("", "", "", "Afrodescendiente", "No afrodescendiente")

alim_01 <- cbind(CODIND, POBLACION,DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, fecha_cat, VALOR, FUENTE, RESPONSABLE, JERARQUIA, JERARQUIA_CAT, CORTE, CORTE_2, DEPARTAMENTO,QUINTIL, SEXO, EDAD, ASCENDENCIA)




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de personas en hogares con con ingresos menores a una CBNA que no perciben alimentación en algún programa público (excluídas canastas)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Total

c_ano <- ech2024_svy %>%
  filter(pobre06 == 1) %>%
  srvyr::summarise(colname = srvyr::survey_mean(pol_alim_scan))

c_ano <-c_ano$colname


# Sexo

c_sexo <- function(x) {
  x <- ech2024_svy %>%
    filter(bc_pe2 == x & pobre06==1) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pol_alim_scan))
  x <- mean(x$colname)
}       

c_e_sexo <- numeric()

for(i in 1:2){
  c_e_sexo[i] <- c_sexo(x = i)
}         

c_sexo <- as.data.frame(c_e_sexo)


# Edad

c_edad <- function(x) {
  x <- ech2024_svy %>%
    filter(tramo2 == x & pobre06==1) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pol_alim_scan))
  x <- mean(x$colname)
}       

c_e_edad <- numeric()

for(i in 1:5){
  c_e_edad[i] <- c_edad(x = i)
}         

c_edad <- as.data.frame(c_e_edad)


# Ascendencia étnico racial

c_afro <- function(x) {
  x <- ech2024_svy %>%
    filter(bd_e29_1 == x & pobre06==1) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pol_alim_scan))
  x <- mean(x$colname)
}       

c_e_afro <- numeric()

for(i in 1:2){
  c_e_afro[i] <- c_afro(x = i)
}         

c_afro <- as.data.frame(c_e_afro)




CODIND       <- "730203"
POBLACION    <- c("", "Mujeres", "Mujeres", "Niños, niñas y adolescentes",
                  "Niños, niñas y adolescentes", "Niños, niñas y adolescentes", "Niños, niñas y adolescentes",
                  "Niños, niñas y adolescentes", "Afrodescendientes", "Afrodescendientes")
DERECHO      <- "Alimentación"
TIPOIND      <- "Resultados"
DIMENSIÓN    <- "Adecuación"
CODINDICADOR <- "Suficiencia de programas de alimentación"
NOMINDICADOR <- "Porcentaje de personas en hogares con con ingresos menores a una CBNA que no perciben alimentación en algún programa público (excluídas canastas)"
AÑO	         <- 2024
fecha_cat    <- ""
VALOR	       <- (1-c(c_ano, c_sexo[1,1], c_sexo[2,1], c_edad[1,1], c_edad[2,1], c_edad[3,1], c_edad[4,1], c_edad[5,1], c_afro[1,1], c_afro[2,1]))*100
FUENTE	     <- "Encuesta Continua de Hogares 2024, INE"
RESPONSABLE	 <- "S. Katzkowicz"
JERARQUIA	   <- 1
JERARQUIA_CAT<- ""
CORTE        <- c("Total", "Sexo", "Sexo", "Edad", "Edad", "Edad", "Edad", "Edad", "Ascendencia étnico-racial", "Ascendencia étnico-racial")
CORTE_2      <- ""
DEPARTAMENTO <- ""
QUINTIL	     <- ""
SEXO		     <- c("", "Varones", "Mujeres", "", "", "", "", "", "", "")
EDAD         <- c("", "", "", "Entre 0 y 5 años", "Entre 6 y 12 años", "Entre 13 y 17 años", "Entre 18 y 29 años", "30 años o más","", "")
ASCENDENCIA  <- c("", "", "", "", "", "", "", "", "Afrodescendiente", "No afrodescendiente")

alim_02 <- cbind(CODIND, POBLACION,DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, fecha_cat, VALOR, FUENTE, RESPONSABLE, JERARQUIA, JERARQUIA_CAT, CORTE, CORTE_2, DEPARTAMENTO,QUINTIL, SEXO, EDAD, ASCENDENCIA)





# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de personas en hogares con con ingresos menores a una CBNA que no perciben alimentación de canastas
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Total
c_ano <- ech2024_svy %>%
  filter(pobre06 == 1) %>%
  srvyr::summarise(colname = srvyr::survey_mean(canasta))

c_ano <-c_ano$colname


CODIND       <- "730204"
POBLACION    <- ""
DERECHO      <- "Alimentación"
TIPOIND      <- "Resultados"
DIMENSIÓN    <- "Adecuación"
CODINDICADOR <- "Suficiencia de canastas"
NOMINDICADOR <- "Porcentaje de personas en hogares con con ingresos menores a una CBNA que no perciben canastas"
AÑO	         <- 2024
fecha_cat    <- ""
VALOR	       <- (1-c_ano)*100
FUENTE	     <- "Encuesta Continua de Hogares 2024, INE"
RESPONSABLE	 <- "S. Katzkowicz"
JERARQUIA	   <- 1
JERARQUIA_CAT<- ""
CORTE        <- "Total"
CORTE_2      <- ""
DEPARTAMENTO <- ""
QUINTIL	     <- ""
SEXO		     <- ""
EDAD         <- ""
ASCENDENCIA  <- ""

alim_03 <- cbind(CODIND, POBLACION,DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, fecha_cat, VALOR, FUENTE, RESPONSABLE, JERARQUIA, JERARQUIA_CAT, CORTE, CORTE_2, DEPARTAMENTO,QUINTIL, SEXO, EDAD, ASCENDENCIA)




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de personas en hogares con con ingresos menores a una CBNA que no perciben ningún tipo de presetación de programas de alimentación
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Total

c_ano <- ech2024_svy %>%
  filter(pobre06 == 1) %>%
  srvyr::summarise(colname = srvyr::survey_mean(pol_alim))

c_ano <-c_ano$colname



# Sexo

c_sexo <- function(x) {
  x <- ech2024_svy %>%
    filter(bc_pe2 == x & pobre06==1) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pol_alim))
  x <- mean(x$colname)
}       

c_e_sexo <- numeric()

for(i in 1:2){
  c_e_sexo[i] <- c_sexo(x = i)
}         

c_sexo <- as.data.frame(as.numeric(c_e_sexo))


# Edad

c_edad <- function(x) {
  x <- ech2024_svy %>%
    filter(tramo2 == x & pobre06==1) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pol_alim))
  x <- mean(x$colname)
}       

c_e_edad <- numeric()

for(i in 1:5){
  c_e_edad[i] <- c_edad(x = i)
}         

c_edad <- as.data.frame(c_e_edad)



# Ascendencia étnico racial

c_afro <- function(x) {
  x <- ech2024_svy %>%
    filter(bd_e29_1 == x & pobre06==1) %>%
    srvyr::summarise(colname = srvyr::survey_mean(pol_alim))
  x <- mean(x$colname)
}       

c_e_afro <- numeric()

for(i in 1:2){
  c_e_afro[i] <- c_afro(x = i)
}         

c_afro <- as.data.frame(c_e_afro)





CODIND       <- "730205"
POBLACION    <- c("", "Mujeres", "Mujeres", "Niños, niñas y adolescentes", "Niños, niñas y adolescentes",
                  "Niños, niñas y adolescentes", "Niños, niñas y adolescentes", "Niños, niñas y adolescentes",
                  "Afrodescendientes", "Afrodescendientes")
DERECHO      <- "Alimentación"
TIPOIND      <- "Resultados"
DIMENSIÓN    <- "Adecuación"
CODINDICADOR <- "Suficiencia de programas de alimentación"
NOMINDICADOR <- "Porcentaje de personas en hogares con con ingresos menores a una CBNA que no perciben ningún tipo de presetación de programas de alimentación"
AÑO	         <- 2024
fecha_cat    <- ""
VALOR	       <- (1-c(c_ano, c_sexo[1,1], c_sexo[2,1], c_edad[1,1], c_edad[2,1], c_edad[3,1], c_edad[4,1], c_edad[5,1], c_afro[1,1], c_afro[2,1]))*100
FUENTE	     <- "Encuesta Continua de Hogares 2024, INE"
RESPONSABLE	 <- "S. Katzkowicz"
JERARQUIA	   <- 1
JERARQUIA_CAT<- ""
CORTE        <- c("Total", "Sexo", "Sexo", "Edad", "Edad", "Edad", "Edad", "Edad", "Ascendencia étnico-racial", "Ascendencia étnico-racial")
CORTE_2      <- ""
DEPARTAMENTO <- ""
QUINTIL	     <- ""
SEXO		     <- c("", "Varones", "Mujeres", "", "", "", "", "", "", "")
EDAD         <- c("", "", "", "Entre 0 y 5 años", "Entre 6 y 12 años", "Entre 13 y 17 años", "Entre 18 y 29 años", "30 años o más","", "")
ASCENDENCIA  <- c("", "", "", "", "", "", "", "", "Afrodescendiente", "No afrodescendiente")

alim_04 <- cbind(CODIND, POBLACION,DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, fecha_cat, VALOR, FUENTE, RESPONSABLE, JERARQUIA, JERARQUIA_CAT, CORTE, CORTE_2, DEPARTAMENTO,QUINTIL, SEXO, EDAD, ASCENDENCIA)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de personas en hogares con ingresos menores a una CBNA que no asisten a merenderos
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Total

c_ano <- ech2024_svy %>%
  filter(pobre06 == 1) %>%
  srvyr::summarise(colname = srvyr::survey_mean(comedor))

c_ano <-c_ano$colname


# Sexo

c_sexo <- function(x) {
  x <- ech2024_svy %>%
    filter(bc_pe2 == x & pobre06==1) %>%
    srvyr::summarise(colname = srvyr::survey_mean(comedor))
  x <- mean(x$colname)
}       

c_e_sexo <- numeric()

for(i in 1:2){
  c_e_sexo[i] <- c_sexo(x = i)
}         

c_sexo <- as.data.frame(c_e_sexo)


# Ascendencia étnico racial

c_afro <- function(x) {
  x <- ech2024_svy %>%
    filter(bd_e29_1 == x & pobre06==1) %>%
    srvyr::summarise(colname = srvyr::survey_mean(comedor))
  x <- mean(x$colname)
}       

c_e_afro <- numeric()

for(i in 1:2){
  c_e_afro[i] <- c_afro(x = i)
}         

c_afro <- as.data.frame(c_e_afro)




CODIND       <- "730202"
POBLACION    <- c("", "Mujeres", "Mujeres", "Afrodescendientes", "Afrodescendientes")
DERECHO      <- "Alimentación"
TIPOIND      <- "Resultados"
DIMENSIÓN    <- "Adecuación"
CODINDICADOR <- "Suficiencia de merenderos"
NOMINDICADOR <- "Porcentaje de personas en hogares con ingresos menores a una CBNA que no asisten a merenderos"
AÑO	         <- 2024
fecha_cat    <- ""
VALOR	       <- (1-c(c_ano, c_sexo[1,1], c_sexo[2,1], c_afro[1,1], c_afro[2,1]))*100
FUENTE	     <- "Encuesta Continua de Hogares 2024, INE"
RESPONSABLE	 <- "S. Katzkowicz"
JERARQUIA	   <- 1
JERARQUIA_CAT<- ""
CORTE        <- c("Total", "Sexo", "Sexo", "Ascendencia étnico-racial", "Ascendencia étnico-racial")
CORTE_2      <- ""
DEPARTAMENTO <- ""
QUINTIL	     <- ""
SEXO		     <- c("", "Varones", "Mujeres", "", "")
EDAD         <- ""
ASCENDENCIA  <- c("", "", "", "Afrodescendiente", "No afrodescendiente")

alim_05 <- cbind(CODIND, POBLACION,DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, fecha_cat, VALOR, FUENTE, RESPONSABLE, JERARQUIA, JERARQUIA_CAT, CORTE, CORTE_2, DEPARTAMENTO,QUINTIL, SEXO, EDAD, ASCENDENCIA)




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Cantidad de personas que asisten a comedor o merenderos
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Total

c_ano <- ech2024_svy %>%
  srvyr::summarise(colname = srvyr::survey_mean(comedor))

c_ano <-c_ano$colname


# Sexo

var_int <- c("sexo1", "sexo2")
c_e_sexo <- sapply(ech2024_svy$variables %>% filter(comedor==1) %>% select(var_int), function(x){
  ech2024_svy %>% filter(comedor==1) %>%
    srvyr::summarise(stat = srvyr::survey_mean(x, na.rm = TRUE)) 
})


c_sexo <- matrix(,2,1)
for (i in 1:2) {
  c_sexo[i,1] <- c(as.numeric(c(c_e_sexo[1,i])))
}


# Edad

var_int <- c("tramo1_1", "tramo1_2", "tramo1_3", "tramo1_4", "tramo1_5", "tramo1_6")
c_e_edad <- sapply(ech2024_svy$variables %>% filter(comedor==1) %>% select(var_int), function(x){
  ech2024_svy %>% filter(comedor==1) %>%
    srvyr::summarise(stat = srvyr::survey_mean(x, na.rm = TRUE)) 
})


c_edad <- matrix(,6,1)
for (i in 1:6) {
  c_edad[i,1] <- c(as.numeric(c(c_e_edad[1,i])))
}


# Ascendencia

var_int <- c("afro1", "afro2")
c_e_afro <- sapply(ech2024_svy$variables %>%  filter(comedor==1) %>% select(var_int), function(x){
  ech2024_svy %>% filter(comedor==1) %>%
    srvyr::summarise(stat = srvyr::survey_mean(x, na.rm = TRUE)) 
})


c_afro <- matrix(,2,1)
for (i in 1:2) {
  c_afro[i,1] <- c(as.numeric(c(c_e_afro[1,i])))
}


proyecciones24 = subset(proyecciones, Año==2024)[,2]


CODIND       <- "730101"
POBLACION    <- c("", "Mujeres", "Mujeres","Niños, niñas y adolescentes",
"Niños, niñas y adolescentes","Niños, niñas y adolescentes","Niños, niñas y adolescentes",
"Niños, niñas y adolescentes","Niños, niñas y adolescentes","Afrodescendientes","Afrodescendientes")
DERECHO      <- "Alimentación"
TIPOIND      <- "Resultados"
DIMENSIÓN    <- "Adecuación"
CODINDICADOR <- "Cobertura de merenderos"
NOMINDICADOR <- "Cantidad de personas que asisten a comedor o merenderos"
AÑO	         <- 2024
fecha_cat    <- ""
VALOR	       <- c(c_ano*proyecciones24, c_sexo[1]*c_ano*proyecciones24, c_sexo[2]*c_ano*proyecciones24, 
                  c_edad[1]*c_ano*proyecciones24,c_edad[2]*c_ano*proyecciones24,c_edad[3]*c_ano*proyecciones24,
                  c_edad[4]*c_ano*proyecciones24,c_edad[5]*c_ano*proyecciones24,c_edad[6]*c_ano*proyecciones24, 
                  c_afro[1]*c_ano*proyecciones24,c_afro[2]*c_ano*proyecciones24)
FUENTE	     <- "Encuesta Continua de Hogares 2024, INE"
RESPONSABLE	 <- "S. Katzkowicz"
JERARQUIA	   <- 1
JERARQUIA_CAT<- ""
CORTE        <- c("Total", "Sexo", "Sexo", "Edad", "Edad", "Edad", "Edad", "Edad", "Edad", "Ascendencia", "Ascendencia")
CORTE_2      <- ""
DEPARTAMENTO <- ""
QUINTIL	     <- ""
SEXO		     <- c("", "Varones", "Mujeres", "", "", "", "", "", "", "", "")
EDAD         <- c("", "", "", "Entre 0 y 5 años", "Entre 6 y 12 años", "Entre 13 y 17 años", "Entre 18 y 29 años", "Entre 30 y 64 años","65 años o más", "", "")
ASCENDENCIA  <- c("", "", "", "", "", "", "", "", "", "Afrodescendiente", "No afrodescendiente")



alim_06 <- cbind(CODIND, POBLACION,DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, fecha_cat, VALOR, FUENTE, RESPONSABLE, JERARQUIA, JERARQUIA_CAT, CORTE, CORTE_2, DEPARTAMENTO,QUINTIL, SEXO, EDAD, ASCENDENCIA)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Cantidad de personas que perciben alimentación en algún programa público (excluídas canastas)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Total

c_ano <- ech2024_svy %>%
  srvyr::summarise(colname = srvyr::survey_mean(pol_alim_scan))

c_ano <-c_ano$colname


# Sexo

var_int <- c("sexo1", "sexo2")
c_e_sexo <- sapply(ech2024_svy$variables %>% filter(pol_alim_scan==1) %>% select(var_int), function(x){
  ech2024_svy %>% filter(pol_alim_scan==1) %>%
    srvyr::summarise(stat = srvyr::survey_mean(x, na.rm = TRUE)) 
})


c_sexo <- matrix(,2,1)
for (i in 1:2) {
  c_sexo[i,1] <- c(as.numeric(c(c_e_sexo[1,i])))
}


# Edad

var_int <- c("tramo1_1", "tramo1_2", "tramo1_3", "tramo1_4", "tramo1_5", "tramo1_6")
c_e_edad <- sapply(ech2024_svy$variables %>% filter(pol_alim_scan==1) %>% select(var_int), function(x){
  ech2024_svy %>% filter(pol_alim_scan==1) %>%
    srvyr::summarise(stat = srvyr::survey_mean(x, na.rm = TRUE)) 
})


c_edad <- matrix(,6,1)
for (i in 1:6) {
  c_edad[i,1] <- c(as.numeric(c(c_e_edad[1,i])))
}


# Ascendencia

var_int <- c("afro1", "afro2")
c_e_afro <- sapply(ech2024_svy$variables %>%  filter(pol_alim_scan==1) %>% select(var_int), function(x){
  ech2024_svy %>% filter(pol_alim_scan==1) %>%
    srvyr::summarise(stat = srvyr::survey_mean(x, na.rm = TRUE)) 
})


c_afro <- matrix(,2,1)
for (i in 1:2) {
  c_afro[i,1] <- c(as.numeric(c(c_e_afro[1,i])))
}


proyecciones24 = subset(proyecciones, Año==2024)[,2]


CODIND       <- "730103"
POBLACION    <- c("", "Mujeres", "Mujeres","Niños, niñas y adolescentes",
                  "Niños, niñas y adolescentes","Niños, niñas y adolescentes","Niños, niñas y adolescentes",
                  "Niños, niñas y adolescentes","Niños, niñas y adolescentes","Afrodescendientes","Afrodescendientes")
DERECHO      <- "Alimentación"
TIPOIND      <- "Resultados"
DIMENSIÓN    <- "Adecuación"
CODINDICADOR <- "Cobertura de programas de alimentación"
NOMINDICADOR <- "Cantidad de personas que perciben alimentación en algún programa público (excluídas canastas)"
AÑO	         <- 2024
fecha_cat    <- ""
VALOR	       <- c(c_ano*proyecciones24, c_sexo[1]*c_ano*proyecciones24, c_sexo[2]*c_ano*proyecciones24, 
                  c_edad[1]*c_ano*proyecciones24,c_edad[2]*c_ano*proyecciones24,c_edad[3]*c_ano*proyecciones24,
                  c_edad[4]*c_ano*proyecciones24,c_edad[5]*c_ano*proyecciones24,c_edad[6]*c_ano*proyecciones24, 
                  c_afro[1]*c_ano*proyecciones24,c_afro[2]*c_ano*proyecciones24)
FUENTE	     <- "Encuesta Continua de Hogares 2024, INE"
RESPONSABLE	 <- "S. Katzkowicz"
JERARQUIA	   <- 1
JERARQUIA_CAT<- ""
CORTE        <- c("Total", "Sexo", "Sexo", "Edad", "Edad", "Edad", "Edad", "Edad", "Edad", "Ascendencia", "Ascendencia")
CORTE_2      <- ""
DEPARTAMENTO <- ""
QUINTIL	     <- ""
SEXO		     <- c("", "Varones", "Mujeres", "", "", "", "", "", "", "", "")
EDAD         <- c("", "", "", "Entre 0 y 5 años", "Entre 6 y 12 años", "Entre 13 y 17 años", "Entre 18 y 29 años", "Entre 30 y 64 años","65 años o más", "", "")
ASCENDENCIA  <- c("", "", "", "", "", "", "", "", "", "Afrodescendiente", "No afrodescendiente")


alim_07 <- cbind(CODIND, POBLACION,DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, fecha_cat, VALOR, FUENTE, RESPONSABLE, JERARQUIA, JERARQUIA_CAT, CORTE, CORTE_2, DEPARTAMENTO,QUINTIL, SEXO, EDAD, ASCENDENCIA)




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Cantidad de personas que perciben canastas alimentarias
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Total

c_ano <- ech2024_svy %>%
  srvyr::summarise(colname = srvyr::survey_mean(canasta))

c_ano <-c_ano$colname


# Sexo

var_int <- c("sexo1", "sexo2")
c_e_sexo <- sapply(ech2024_svy$variables %>% filter(canasta==1) %>% select(var_int), function(x){
  ech2024_svy %>% filter(canasta==1) %>%
    srvyr::summarise(stat = srvyr::survey_mean(x, na.rm = TRUE)) 
})


c_sexo <- matrix(,2,1)
for (i in 1:2) {
  c_sexo[i,1] <- c(as.numeric(c(c_e_sexo[1,i])))
}



proyecciones24 = subset(proyecciones, Año==2024)[,2]


CODIND       <- "730104"
POBLACION    <- c("", "Mujeres", "Mujeres")
DERECHO      <- "Alimentación"
TIPOIND      <- "Resultados"
DIMENSIÓN    <- "Adecuación"
CODINDICADOR <- "Cobertura de canastas"
NOMINDICADOR <- "Cantidad de personas que perciben canastas alimentarias"
AÑO	         <- 2024
fecha_cat    <- ""
VALOR	       <- c(c_ano*proyecciones24, c_sexo[1,1]*c_ano*proyecciones24, c_sexo[2,1]*c_ano*proyecciones24)
FUENTE	     <- "Encuesta Continua de Hogares 2024, INE"
RESPONSABLE	 <- "S. Katzkowicz"
JERARQUIA	   <- 1
JERARQUIA_CAT<- ""
CORTE        <- c("Total", "Sexo", "Sexo")
CORTE_2      <- ""
DEPARTAMENTO <- ""
QUINTIL	     <- ""
SEXO		     <- c("", "Varones", "Mujeres")
EDAD         <- ""
ASCENDENCIA  <- ""



alim_08 <- cbind(CODIND, POBLACION,DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, fecha_cat, VALOR, FUENTE, RESPONSABLE, JERARQUIA, JERARQUIA_CAT, CORTE, CORTE_2, DEPARTAMENTO,QUINTIL, SEXO, EDAD, ASCENDENCIA)




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Cantidad de personas que perciben alimentación en algún programa público 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Total

c_ano <- ech2024_svy %>%
  srvyr::summarise(colname = srvyr::survey_mean(pol_alim))

c_ano <-c_ano$colname


# Sexo

var_int <- c("sexo1", "sexo2")
c_e_sexo <- sapply(ech2024_svy$variables %>% filter(pol_alim==1) %>% select(var_int), function(x){
  ech2024_svy %>% filter(pol_alim==1) %>%
    srvyr::summarise(stat = srvyr::survey_mean(x, na.rm = TRUE)) 
})


c_sexo <- matrix(,2,1)
for (i in 1:2) {
  c_sexo[i,1] <- c(as.numeric(c(c_e_sexo[1,i])))
}


# Edad

var_int <- c("tramo1_1", "tramo1_2", "tramo1_3", "tramo1_4", "tramo1_5", "tramo1_6")
c_e_edad <- sapply(ech2024_svy$variables %>% filter(pol_alim==1) %>% select(var_int), function(x){
  ech2024_svy %>% filter(pol_alim==1) %>%
    srvyr::summarise(stat = srvyr::survey_mean(x, na.rm = TRUE)) 
})


c_edad <- matrix(,6,1)
for (i in 1:6) {
  c_edad[i,1] <- c(as.numeric(c(c_e_edad[1,i])))
}


# Ascendencia

var_int <- c("afro1", "afro2")
c_e_afro <- sapply(ech2024_svy$variables %>%  filter(pol_alim==1) %>% select(var_int), function(x){
  ech2024_svy %>% filter(pol_alim==1) %>%
    srvyr::summarise(stat = srvyr::survey_mean(x, na.rm = TRUE)) 
})


c_afro <- matrix(,2,1)
for (i in 1:2) {
  c_afro[i,1] <- c(as.numeric(c(c_e_afro[1,i])))
}


proyecciones24 = subset(proyecciones, Año==2024)[,2]


CODIND       <- "730102"
POBLACION    <- c("", "Mujeres", "Mujeres","Niños, niñas y adolescentes",
                  "Niños, niñas y adolescentes","Niños, niñas y adolescentes","Niños, niñas y adolescentes",
                  "Niños, niñas y adolescentes","Niños, niñas y adolescentes","Afrodescendientes","Afrodescendientes")
DERECHO      <- "Alimentación"
TIPOIND      <- "Resultados"
DIMENSIÓN    <- "Adecuación"
CODINDICADOR <- "Cobertura de programas de alimentación"
NOMINDICADOR <- "Cantidad de personas que perciben alimentación en algún programa público"
AÑO	         <- 2024
fecha_cat    <- ""
VALOR	       <- c(c_ano*proyecciones24, c_sexo[1]*c_ano*proyecciones24, c_sexo[2]*c_ano*proyecciones24, 
                  c_edad[1]*c_ano*proyecciones24,c_edad[2]*c_ano*proyecciones24,c_edad[3]*c_ano*proyecciones24,
                  c_edad[4]*c_ano*proyecciones24,c_edad[5]*c_ano*proyecciones24,c_edad[6]*c_ano*proyecciones24, 
                  c_afro[1]*c_ano*proyecciones24,c_afro[2]*c_ano*proyecciones24)
FUENTE	     <- "Encuesta Continua de Hogares 2024, INE"
RESPONSABLE	 <- "S. Katzkowicz"
JERARQUIA	   <- 1
JERARQUIA_CAT<- ""
CORTE        <- c("Total", "Sexo", "Sexo", "Edad", "Edad", "Edad", "Edad", "Edad", "Edad", "Ascendencia", "Ascendencia")
CORTE_2      <- ""
DEPARTAMENTO <- ""
QUINTIL	     <- ""
SEXO		     <- c("", "Varones", "Mujeres", "", "", "", "", "", "", "", "")
EDAD         <- c("", "", "", "Entre 0 y 5 años", "Entre 6 y 12 años", "Entre 13 y 17 años", "Entre 18 y 29 años", "Entre 30 y 64 años","65 años o más", "", "")
ASCENDENCIA  <- c("", "", "", "", "", "", "", "", "", "Afrodescendiente", "No afrodescendiente")




alim_09 <- cbind(CODIND, POBLACION,DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, fecha_cat, VALOR, FUENTE, RESPONSABLE, JERARQUIA, JERARQUIA_CAT, CORTE, CORTE_2, DEPARTAMENTO,QUINTIL, SEXO, EDAD, ASCENDENCIA)




alim_2024 <- rbind(alim_01, alim_02, alim_03, alim_04, alim_05, alim_06, alim_07, alim_08, alim_09)

rio::export(alim_2024, "Tabulados/Tabulados 2024/alim_2024.xlsx" )

