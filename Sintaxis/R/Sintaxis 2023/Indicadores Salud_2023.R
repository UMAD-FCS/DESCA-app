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
AÑO	         <- 2023
FUENTE	     <- "Encuesta Continua de Hogares 2023, INE"
RESPONSABLE	 <- "J. Pandolfi"
JERARQUIA_CAT <- 1

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

### Carga de bases ###

base          <- rio::import("ECH_2023.csv")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

### Generación de nuevas variables ###


# Ingresos

base <- base %>% dplyr::mutate(y_pc =  HT11 / HT19 ) #Ingreso per-cápita

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

base <- base %>% dplyr::mutate(tramo_edad = case_when(e27 >= 0  & e27 <= 5  ~  1,
                                                      e27 >= 6  & e27 <= 12 ~  2,
                                                      e27 >= 13 & e27 <= 17 ~  3,
                                                      e27 >= 18 & e27 <= 29 ~  4,
                                                      e27 >= 30 & e27 <= 64 ~  5,
                                                      e27 >= 65             ~  6))


# Prestador de Salud

base <- base %>% dplyr::mutate(msp = ifelse(e45_cv == 1 , 1, 0))
base <- base %>% dplyr::mutate(mut = ifelse(e45_cv == 2 , 1, 0))
base <- base %>% dplyr::mutate(smp = ifelse(e45_cv == 3 , 1, 0))
base <- base %>% dplyr::mutate(hpm = ifelse(e45_cv == 4 , 1, 0))
base <- base %>% dplyr::mutate(otr = ifelse(e45_cv == 5 | e45_cv == 6 , 1, 0))
base <- base %>% dplyr::mutate(sin = ifelse(e45_cv == 7 , 1, 0))
base <- base %>% dplyr::mutate(mas = ifelse(e45_cva >= 1 & e45_cva <=6 , 1, 0))


prestador <- c("msp", "mut", "smp", "hpm", "otr", "mas", "sin")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

### Información de muestreo ### 

base_svy <- srvyr::as_survey_design(base, ids = ID, weights = W_ANO)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
### Procesamiento de indicadores ###
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Distribución porcentual de personas según institución prestadora en la cual declaran tener cobertura vigente
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

Prestador <- c("Público (ASSE)", "IAMC", "Seguro Privado", "Policial / Militar", "Otros", "Más de un prestador", "Sin cobertura")


# Total

c_ano <- sapply(base_svy$variables %>% select(prestador), function(x){
  base_svy %>%
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
  base2 <- subset(base_svy, bd_e29_1 == y) 
  resultados  <-  sapply(base2$variables %>% select(prestador), function(x){
    base2 %>%
      srvyr::summarise(VALOR = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

c_afro = matrix(, nrow = 2, ncol = 7)

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
  base2 <- subset(base_svy, dpto == y) 
  resultados  <-  sapply(base2$variables %>% select(prestador), function(x){
    base2 %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

c_dpto = matrix(, nrow = 19, ncol = 7)

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

mas <- c_dpto[, c("mas", "DEPARTAMENTO")]
mas <- rename(mas, VALOR = mas)
mas <- mutate(mas, Prestador = "Más de un prestador")

sin <- c_dpto[, c("sin", "DEPARTAMENTO")]
sin <- rename(sin, VALOR = sin)
sin <- mutate(sin, Prestador = "Sin cobertura")

c_dpto <- rbind(msp, mut, smp, hpm, otr, mas, sin)

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
  base2 <- subset(base_svy, quintilesy == y) 
  resultados  <-  sapply(base2$variables %>% select(prestador), function(x){
    base2 %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

c_quintil = matrix(, nrow = 5, ncol = 7)

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

mas <- c_quintil[, c("mas", "QUINTIL")]
mas <- rename(mas, VALOR = mas)
mas <- mutate(mas, Prestador = "Más de un prestador")

sin <- c_quintil[, c("sin", "QUINTIL")]
sin <- rename(sin, VALOR = sin)
sin <- mutate(sin, Prestador = "Sin cobertura")

c_quintil <- rbind(msp, mut, smp, hpm, otr, mas, sin)

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
  base2 <- subset(base_svy, bc_pe2 == y) 
  resultados  <-  sapply(base2$variables %>% select(prestador), function(x){
    base2 %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

c_sexo = matrix(, nrow = 2, ncol = 7)

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

mas <- c_sexo[, c("mas", "SEXO")]
mas <- rename(mas, VALOR = mas)
mas <- mutate(mas, Prestador = "Más de un prestador")

sin <- c_sexo[, c("sin", "SEXO")]
sin <- rename(sin, VALOR = sin)
sin <- mutate(sin, Prestador = "Sin cobertura")

c_sexo <- rbind(msp, mut, smp, hpm, otr, mas, sin)

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
  base2 <- subset(base_svy, tramo_edad == y) 
  resultados  <-  sapply(base2$variables %>% select(prestador), function(x){
    base2 %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

c_edad = matrix(, nrow = 6, ncol = 7)

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

mas <- c_edad[, c("mas", "Edad")]
mas <- rename(mas, VALOR = mas)
mas <- mutate(mas, Prestador = "Más de un prestador")

sin <- c_edad[, c("sin", "Edad")]
sin <- rename(sin, VALOR = sin)
sin <- mutate(sin, Prestador = "Sin cobertura")

c_edad <- rbind(msp, mut, smp, hpm, otr, mas, sin)

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
NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. Este indicador se calcula únicamente a partir de la implantación de modalidad panel del segundo semestre de 2021. Dados los cambios metodolóigicos en la formulación de las preguntas, no se considera la información del primer semestre. A partir de 2020 cambia el modo de relevar cobertura de salud. Antes de esta fecha se les consultaba a los/as encuestados por cobertura en cada uno de los prestadores posibles. Durante el 2020 y el primer semestre de 2021, se releva únicamente el principal prestador de salud. A partir del segundo semestre de 2021 se releva el prestador principal y secundario, hecho que habilita reconstruir un indicador más próximo al calculado antes de 2019."


s_01 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, FUENTE, RESPONSABLE, JERARQUIA_CAT, s_01)

s_01 <- s_01 %>% dplyr::mutate(VALOR = as.numeric(VALOR)*100)




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
### Generación de Base motor ###
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #



rio::export(s_01, "salud_2023.xlsx" )


