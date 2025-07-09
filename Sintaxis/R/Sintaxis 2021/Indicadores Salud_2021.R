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


# Tramo de edad

sem1 <- sem1 %>% dplyr::mutate(tramo_edad = case_when(E27 >= 0 & E27  <= 5  ~  1,
                                                      E27 >= 6 & E27  <= 12 ~  2,
                                                      E27 >= 13 & E27 <= 17 ~  3,
                                                      E27 >= 18 & E27 <= 29 ~  4,
                                                      E27 >= 30 & E27 <= 64 ~  5,
                                                      E27 >= 65             ~  6))

sem2_implant <- sem2_implant %>% dplyr::mutate(tramo_edad = case_when(e27 >= 0  & e27 <= 5  ~  1,
                                                                      e27 >= 6  & e27 <= 12 ~  2,
                                                                      e27 >= 13 & e27 <= 17 ~  3,
                                                                      e27 >= 18 & e27 <= 29 ~  4,
                                                                      e27 >= 30 & e27 <= 64 ~  5,
                                                                      e27 >= 65             ~  6))


# Prestador de Salud

sem1 <- sem1 %>% dplyr::mutate(msp = ifelse(E45_CV == 1 , 1, 0))
sem1 <- sem1 %>% dplyr::mutate(mut = ifelse(E45_CV == 2 , 1, 0))
sem1 <- sem1 %>% dplyr::mutate(smp = ifelse(E45_CV == 3 , 1, 0))
sem1 <- sem1 %>% dplyr::mutate(hpm = ifelse(E45_CV == 4 , 1, 0))
sem1 <- sem1 %>% dplyr::mutate(otr = ifelse(E45_CV == 5 | E45_CV == 6 , 1, 0))
sem1 <- sem1 %>% dplyr::mutate(sin = ifelse(E45_CV == 7 , 1, 0))

sem2_implant <- sem2_implant %>% dplyr::mutate(msp = ifelse(e45_cv == 1 & e45_cva ==7, 1, 0))
sem2_implant <- sem2_implant %>% dplyr::mutate(mut = ifelse(e45_cv == 2 & e45_cva ==7, 1, 0))
sem2_implant <- sem2_implant %>% dplyr::mutate(smp = ifelse(e45_cv == 3 & e45_cva ==7, 1, 0))
sem2_implant <- sem2_implant %>% dplyr::mutate(hpm = ifelse(e45_cv == 4 & e45_cva ==7, 1, 0))
sem2_implant <- sem2_implant %>% dplyr::mutate(otr = ifelse((e45_cv == 5 | e45_cv == 6) & e45_cva ==7 , 1, 0))
sem2_implant <- sem2_implant %>% dplyr::mutate(mas = ifelse(e45_cva >= 1 & e45_cva <=6 , 1, 0))
sem2_implant <- sem2_implant %>% dplyr::mutate(sin = ifelse(e45_cv == 7 , 1, 0))

prestador <- c("msp", "mut", "smp", "hpm", "otr", "mas", "sin")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

### Información de muestreo ### 

# Ponderador semestral semestre 1:
sem1 <- sem1 %>% dplyr::mutate(pesosem = pesomen / 6)

sem1_svy           <- srvyr::as_survey_design(sem1, ids = numero, weights = pesosem)
#sem1_h_svy         <- srvyr::as_survey_design(sem1_h, ids = numero, weights = pesomen)

sem2_implant_svy   <- srvyr::as_survey_design(sem2_implant, ids = ID, weights = w_sem)
#sem2_implant_h_svy <- srvyr::as_survey_design(sem2_implant_h, ids = ID, weights = w_sem)

#sem2_panel_svy   <-  srvyr::as_survey_design(sem2_panel, ids = ID, weights = w)

#sem2_panel_svy     <- svrepdesign(data = sem2_panel,
#                                  type = "bootstrap",
#                                  weights =~ w,
#                                  repweights = sem2_panel %>% dplyr::select(dplyr::starts_with("wr")))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
### Procesamiento de indicadores ###
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Distribución porcentual de personas según institución prestadora en la cual declaran tener cobertura vigente
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

Prestador <- c("Público (ASSE)", "IAMC", "Seguro Privado", "Policial / Militar", "Otros", "Más de un prestador", "Sin cobertura")


# Total

c_ano <- sapply(sem2_implant_svy$variables %>% select(prestador), function(x){
  sem2_implant_svy %>%
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
  base <- subset(sem2_implant_svy, bd_e29_1 == y) 
  resultados  <-  sapply(base$variables %>% select(prestador), function(x){
    base %>%
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
  base <- subset(sem2_implant_svy, dpto == y) 
  resultados  <-  sapply(base$variables %>% select(prestador), function(x){
    base %>%
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
  base <- subset(sem2_implant_svy, quintilesy == y) 
  resultados  <-  sapply(base$variables %>% select(prestador), function(x){
    base %>%
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
  base <- subset(sem2_implant_svy, bc_pe2 == y) 
  resultados  <-  sapply(base$variables %>% select(prestador), function(x){
    base %>%
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
  base <- subset(sem2_implant_svy, tramo_edad == y) 
  resultados  <-  sapply(base$variables %>% select(prestador), function(x){
    base %>%
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
NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. Este indicador se calcula únicamente a partir de la implantación de modalidad panel del segundo semestre de 2021.Dados los cambios metodolóigicos en la formulación de las preguntas, no se considera la información del primer semestre. A partir de 2020 cambia el modo de relevar cobertura de salud. Antes de esta fecha se les consultaba a los/as encuestados por cobertura en cada uno de los prestadores posibles. Durante el 2020 y el primer semestre de 2021, se releva únicamente el principal prestador de salud. En el segundo semestre se releva el prestador principal y secundario, hecho que habilita reconstruir un indicador más próximo al calculado antes de 2019."


s_01 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, FUENTE, RESPONSABLE, JERARQUIA_CAT, s_01)





# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
### Generación de Base motor ###
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #



rio::export(s_01, "salud_2021.xlsx" )


