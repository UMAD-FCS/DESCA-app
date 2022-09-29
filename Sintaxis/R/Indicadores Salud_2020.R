### Mirador DESCA
### Derecho a la Salud
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

DERECHO      <- "Salud"
TIPOIND      <- "Resultados"
AÑO	         <- 2020
FUENTE	     <- "Encuesta Continua de Hogares 2020, INE"
RESPONSABLE	 <- "J. Pandolfi"
JERARQUIA_CAT <- 1

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

### Carga de bases ###

ech2020 <- rio::import("C:/Users/Usuario/Dropbox/1. Unidad de Métodos y Acceso a Datos/1. Observatorio UMAD/PISO I_MICRODATOS/ECH/2020/Fusionada_personasyhogares_20.dta")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

### Generación de nuevas variables ###


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

# Tramo de edad

ech2020 <- ech2020 %>% dplyr::mutate(tramo_edad = case_when(e27 >= 0  & e27 <= 5  ~  1,
                                                            e27 >= 6  & e27 <= 12 ~  2,
                                                            e27 >= 13 & e27 <= 17 ~  3,
                                                            e27 >= 18 & e27 <= 29 ~  4,
                                                            e27 >= 30 & e27 <= 64 ~  5,
                                                            e27 >= 65             ~  6))
# Prestador de Salud

ech2020 <- ech2020 %>% dplyr::mutate(msp = ifelse(e45_cv == 1 , 1, 0))
ech2020 <- ech2020 %>% dplyr::mutate(mut = ifelse(e45_cv == 2 , 1, 0))
ech2020 <- ech2020 %>% dplyr::mutate(smp = ifelse(e45_cv == 3 , 1, 0))
ech2020 <- ech2020 %>% dplyr::mutate(hpm = ifelse(e45_cv == 4 , 1, 0))
ech2020 <- ech2020 %>% dplyr::mutate(otr = ifelse(e45_cv == 5 | e45_cv == 6 , 1, 0))
ech2020 <- ech2020 %>% dplyr::mutate(sin = ifelse(e45_cv == 7 , 1, 0))


prestador <- c("msp", "mut", "smp", "hpm", "otr", "sin")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

### Información de muestreo ### 


# Ponderador anual:
ech2020 <- ech2020 %>% dplyr::mutate(pesoano = pesomen / 12)
ech2020_svy           <- srvyr::as_survey_design(ech2020, ids = numero, weights = pesoano)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
### Procesamiento de indicadores ###
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Distribución porcentual de personas según institución prestadora en la cual declaran tener cobertura vigente
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

Prestador <- c("Público (ASSE)", "IAMC", "Seguro Privado", "Policial / Militar", "Otros", "Sin cobertura")



# Total

c_ano <- sapply(ech2020_svy$variables %>% select(prestador), function(x){
  ech2020_svy %>%
    srvyr::summarise(VALOR = srvyr::survey_mean(x, na.rm = TRUE)) 
})

c_ano <- data.frame(t(c_ano))
c_ano <- c_ano %>% dplyr::mutate(DEPARTAMENTO = "")
c_ano <- c_ano %>% dplyr::mutate(ASCENDENCIA = "")
c_ano <- c_ano %>% dplyr::mutate(SEXO = "")
c_ano <- c_ano %>% dplyr::mutate(Edad = "")
c_ano <- c_ano %>% dplyr::mutate(QUINTIL = "")
c_ano <- c_ano %>% dplyr::mutate(CORTE_2 = "Total")
c_ano <- c_ano %>% dplyr::mutate(CORTE = "Prestador")
c_ano <- c_ano %>% dplyr::mutate(JERARQUIA = 1)

c_ano <- c_ano[, c("VALOR", "DEPARTAMENTO", "QUINTIL", "SEXO", "CORTE", "CORTE_2", "ASCENDENCIA", "JERARQUIA", "Edad")]
c_ano <- cbind(c_ano, Prestador)



# Ascendencia étnico racial


c_afro_f <- function(y) {
  base <- subset(ech2020_svy, bd_e29_1 == y) 
  resultados  <-  sapply(base$variables %>% select(prestador), function(x){
    base %>%
      srvyr::summarise(VALOR = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

c_afro = matrix(, nrow = 2, ncol = 6)

for(i in 1:2){
  c_afro[i,] <- c_afro_f(y = i)
} 

rownames(c_afro) <- c("afro", "noafro")
c_afro <- data.frame(t(c_afro))
c_afro <- cbind(c_afro, Prestador)

c_afro_1 <-  c_afro[, c("afro", "Prestador") ]
c_afro_1 <- c_afro_1 %>% dplyr::mutate(ASCENDENCIA = "Afrodescendiente")
c_afro_1 <- c_afro_1 %>% dplyr::rename(VALOR = "afro")

c_afro_2 <-  c_afro[, c("noafro", "Prestador") ]
c_afro_2 <- c_afro_2 %>% dplyr::mutate(ASCENDENCIA = "No afrodescendiente")
c_afro_2 <- c_afro_2 %>% dplyr::rename(VALOR = "noafro")


c_afro <- rbind(c_afro_1, c_afro_2)

c_afro <- c_afro %>% dplyr::mutate(DEPARTAMENTO = "")
c_afro <- c_afro %>% dplyr::mutate(QUINTIL = "")
c_afro <- c_afro %>% dplyr::mutate(SEXO = "")
c_afro <- c_afro %>% dplyr::mutate(Edad = "")
c_afro <- c_afro %>% dplyr::mutate(CORTE_2 = "Ascendencia étnico-racial")
c_afro <- c_afro %>% dplyr::mutate(CORTE = "Prestador")
c_afro <- c_afro %>% dplyr::mutate(JERARQUIA = 0)



# Departamento


DEPARTAMENTO <- c( "Montevideo", "Artigas", "Canelones", "Cerro Largo", "Colonia", "Durazno", "Flores", "Florida", "Lavalleja", "Maldonado",
                   "Paysandú", "Río Negro", "Rivera", "Rocha", "Salto", "San José", "Soriano", "Tacuarembó", "Treinta y Tres")

c_dpto_f <- function(y) {
  base <- subset(ech2020_svy, dpto == y) 
  resultados  <-  sapply(base$variables %>% select(prestador), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

c_dpto = matrix(, nrow = 19, ncol = 6)

for(i in 1:19){
  c_dpto[i,] <- c_dpto_f(y = i)
} 

colnames(c_dpto) <- prestador
c_dpto <- data.frame(cbind(c_dpto, DEPARTAMENTO))

msp <- c_dpto[, c("msp", "DEPARTAMENTO")]
msp <- rename(msp, VALOR = msp)
msp <- mutate(msp, Prestador = "Público (ASSE)")

mut <- c_dpto[, c("mut", "DEPARTAMENTO")]
mut <- rename(mut, VALOR = mut)
mut <- mutate(mut, Prestador = "IAMC")

smp <- c_dpto[, c("smp", "DEPARTAMENTO")]
smp <- rename(smp, VALOR = smp)
smp <- mutate(smp, Prestador = "Seguro Privado")

hpm <- c_dpto[, c("hpm", "DEPARTAMENTO")]
hpm <- rename(hpm, VALOR = hpm)
hpm <- mutate(hpm, Prestador = "Policial / Militar")

otr <- c_dpto[, c("otr", "DEPARTAMENTO")]
otr <- rename(otr, VALOR = otr)
otr <- mutate(otr, Prestador = "Otros")

sin <- c_dpto[, c("sin", "DEPARTAMENTO")]
sin <- rename(sin, VALOR = sin)
sin <- mutate(sin, Prestador = "Sin cobertura")

c_dpto <- rbind(msp, mut, smp, hpm, otr, sin)

c_dpto <- c_dpto %>% dplyr::mutate(ASCENDENCIA = "")
c_dpto <- c_dpto %>% dplyr::mutate(QUINTIL = "")
c_dpto <- c_dpto %>% dplyr::mutate(SEXO = "")
c_dpto <- c_dpto %>% dplyr::mutate(Edad = "")
c_dpto <- c_dpto %>% dplyr::mutate(CORTE_2 = "Departamento")
c_dpto <- c_dpto %>% dplyr::mutate(CORTE = "Prestador")
c_dpto <- c_dpto %>% dplyr::mutate(JERARQUIA = 0)



# Quintil de ingresos


QUINTIL <- c("Quintil 1", "Quintil 2", "Quintil 3", "Quintil 4", "Quintil 5")



c_quintil_f <- function(y) {
  base <- subset(ech2020_svy, quintilesy == y) 
  resultados  <-  sapply(base$variables %>% select(prestador), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

c_quintil = matrix(, nrow = 5, ncol = 6)

for(i in 1:5){
  c_quintil[i,] <- c_quintil_f(y = i)
} 

colnames(c_quintil) <- prestador
c_quintil <- data.frame(cbind(c_quintil, QUINTIL))

msp <- c_quintil[, c("msp", "QUINTIL")]
msp <- rename(msp, VALOR = msp)
msp <- mutate(msp, Prestador = "Público (ASSE)")

mut <- c_quintil[, c("mut", "QUINTIL")]
mut <- rename(mut, VALOR = mut)
mut <- mutate(mut, Prestador = "IAMC")

smp <- c_quintil[, c("smp", "QUINTIL")]
smp <- rename(smp, VALOR = smp)
smp <- mutate(smp, Prestador = "Seguro Privado")

hpm <- c_quintil[, c("hpm", "QUINTIL")]
hpm <- rename(hpm, VALOR = hpm)
hpm <- mutate(hpm, Prestador = "Policial / Militar")

otr <- c_quintil[, c("otr", "QUINTIL")]
otr <- rename(otr, VALOR = otr)
otr <- mutate(otr, Prestador = "Otros")

sin <- c_quintil[, c("sin", "QUINTIL")]
sin <- rename(sin, VALOR = sin)
sin <- mutate(sin, Prestador = "Sin cobertura")

c_quintil <- rbind(msp, mut, smp, hpm, otr,  sin)

c_quintil <- c_quintil %>% dplyr::mutate(DEPARTAMENTO = "")
c_quintil <- c_quintil %>% dplyr::mutate(ASCENDENCIA = "")
c_quintil <- c_quintil %>% dplyr::mutate(SEXO = "")
c_quintil <- c_quintil %>% dplyr::mutate(Edad = "")
c_quintil <- c_quintil %>% dplyr::mutate(CORTE_2 = "Quintil de ingresos")
c_quintil <- c_quintil %>% dplyr::mutate(CORTE = "Prestador")
c_quintil <- c_quintil %>% dplyr::mutate(JERARQUIA = 0)



# Sexo


SEXO <- c("Varones","Mujeres")


c_sexo_f <- function(y) {
  base <- subset(ech2020_svy, bc_pe2 == y) 
  resultados  <-  sapply(base$variables %>% select(prestador), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

c_sexo = matrix(, nrow = 2, ncol = 6)

for(i in 1:2){
  c_sexo[i,] <- c_sexo_f(y = i)
} 

colnames(c_sexo) <- prestador
c_sexo <- data.frame(cbind(c_sexo, SEXO))

msp <- c_sexo[, c("msp", "SEXO")]
msp <- rename(msp, VALOR = msp)
msp <- mutate(msp, Prestador = "Público (ASSE)")

mut <- c_sexo[, c("mut", "SEXO")]
mut <- rename(mut, VALOR = mut)
mut <- mutate(mut, Prestador = "IAMC")

smp <- c_sexo[, c("smp", "SEXO")]
smp <- rename(smp, VALOR = smp)
smp <- mutate(smp, Prestador = "Seguro Privado")

hpm <- c_sexo[, c("hpm", "SEXO")]
hpm <- rename(hpm, VALOR = hpm)
hpm <- mutate(hpm, Prestador = "Policial / Militar")

otr <- c_sexo[, c("otr", "SEXO")]
otr <- rename(otr, VALOR = otr)
otr <- mutate(otr, Prestador = "Otros")

sin <- c_sexo[, c("sin", "SEXO")]
sin <- rename(sin, VALOR = sin)
sin <- mutate(sin, Prestador = "Sin cobertura")

c_sexo <- rbind(msp, mut, smp, hpm, otr, sin)

c_sexo <- c_sexo %>% dplyr::mutate(DEPARTAMENTO = "")
c_sexo <- c_sexo %>% dplyr::mutate(ASCENDENCIA = "")
c_sexo <- c_sexo %>% dplyr::mutate(QUINTIL = "")
c_sexo <- c_sexo %>% dplyr::mutate(Edad = "")
c_sexo <- c_sexo %>% dplyr::mutate(CORTE_2 = "Sexo")
c_sexo <- c_sexo %>% dplyr::mutate(CORTE = "Prestador")
c_sexo <- c_sexo %>% dplyr::mutate(JERARQUIA = 0)



# Tramo de edad


Edad <- c("0 a 5 años", "6 a 12 años", "13 a 17 años", "18 a 29 años", "30 a 64 años", "65 años o más")


c_edad_f <- function(y) {
  base <- subset(ech2020_svy, tramo_edad == y) 
  resultados  <-  sapply(base$variables %>% select(prestador), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

c_edad = matrix(, nrow = 6, ncol = 6)

for(i in 1:6){
  c_edad[i,] <- c_edad_f(y = i)
} 

colnames(c_edad) <- prestador
c_edad <- data.frame(cbind(c_edad, Edad))

msp <- c_edad[, c("msp", "Edad")]
msp <- rename(msp, VALOR = msp)
msp <- mutate(msp, Prestador = "Público (ASSE)")

mut <- c_edad[, c("mut", "Edad")]
mut <- rename(mut, VALOR = mut)
mut <- mutate(mut, Prestador = "IAMC")

smp <- c_edad[, c("smp", "Edad")]
smp <- rename(smp, VALOR = smp)
smp <- mutate(smp, Prestador = "Seguro Privado")

hpm <- c_edad[, c("hpm", "Edad")]
hpm <- rename(hpm, VALOR = hpm)
hpm <- mutate(hpm, Prestador = "Policial / Militar")

otr <- c_edad[, c("otr", "Edad")]
otr <- rename(otr, VALOR = otr)
otr <- mutate(otr, Prestador = "Otros")

sin <- c_edad[, c("sin", "Edad")]
sin <- rename(sin, VALOR = sin)
sin <- mutate(sin, Prestador = "Sin cobertura")

c_edad <- rbind(msp, mut, smp, hpm, otr, sin)

c_edad <- c_edad %>% dplyr::mutate(DEPARTAMENTO = "")
c_edad <- c_edad %>% dplyr::mutate(ASCENDENCIA = "")
c_edad <- c_edad %>% dplyr::mutate(QUINTIL = "")
c_edad <- c_edad %>% dplyr::mutate(SEXO = "")
c_edad <- c_edad %>% dplyr::mutate(CORTE_2 = "Edad")
c_edad <- c_edad %>% dplyr::mutate(CORTE = "Prestador")
c_edad <- c_edad %>% dplyr::mutate(JERARQUIA = 0)





c_ano <- c_ano[, c("VALOR","CORTE", "CORTE_2", "DEPARTAMENTO", "QUINTIL", "SEXO", "Edad", "ASCENDENCIA", "Prestador", "JERARQUIA")]
c_afro <- c_afro[, c("VALOR","CORTE", "CORTE_2","DEPARTAMENTO", "QUINTIL", "SEXO", "Edad", "ASCENDENCIA", "Prestador", "JERARQUIA")]
c_dpto <- c_dpto[, c("VALOR","CORTE", "CORTE_2","DEPARTAMENTO", "QUINTIL", "SEXO", "Edad", "ASCENDENCIA", "Prestador", "JERARQUIA")]
c_quintil <- c_quintil[, c("VALOR","CORTE", "CORTE_2","DEPARTAMENTO", "QUINTIL", "SEXO", "Edad", "ASCENDENCIA", "Prestador", "JERARQUIA")]
c_sexo <- c_sexo[, c("VALOR","CORTE", "CORTE_2", "DEPARTAMENTO", "QUINTIL", "SEXO", "Edad", "ASCENDENCIA", "Prestador", "JERARQUIA")]
c_edad <- c_edad[, c("VALOR","CORTE", "CORTE_2", "DEPARTAMENTO", "QUINTIL", "SEXO", "Edad", "ASCENDENCIA", "Prestador", "JERARQUIA")]



s_01 <- rbind(c_ano, c_afro, c_dpto, c_quintil, c_sexo, c_edad)



CODIND       <- 430101
DIMENSIÓN    <- "Disponibilidad"
CODINDICADOR <- "Cobertura integral de salud"
NOMINDICADOR <- "Distribución porcentual de personas según institución prestadora en la cual declaran tener cobertura vigente"
JERARQUIA_CAT<- 1
NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. Bajo esta nueva modalidad, cambia el modo de relevar cobertura de salud. Antes de esta fecha se les consultaba a los/as encuestados por cobertura en cada uno de los prestadores posibles. En 2020, se releva únicamente el principal prestador de salud"


s_01 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, FUENTE, RESPONSABLE, JERARQUIA_CAT, s_01)





# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
### Generación de Base motor ###
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #



rio::export(s_01, "salud_2020.xlsx" )


