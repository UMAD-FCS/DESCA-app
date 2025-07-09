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
AÑO	         <- 2023
FUENTE	     <- "Encuesta Continua de Hogares 2023, INE"
RESPONSABLE	 <- "J. Pandolfi"


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

### Carga de bases ###

base          <- rio::import("ECH_2023.csv")
bases_men       <- rio::import("ECH_men_23.csv")

varsimplant <- select(base, ID, HT11, HT19, HT13)
varsimplant <- varsimplant %>% distinct(ID, .keep_all = TRUE)

bases_men       <- base::merge(bases_men, varsimplant, by = "ID", all.x = TRUE, all.y = FALSE)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

### Generación de nuevas variables ###

# Salario mínimo nacional enero 2023

smn <- 21107

#IPC

base <- base %>% dplyr::mutate(bc_ipc_tot = case_when(
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

bases_men <- bases_men %>% dplyr::mutate(bc_ipc_tot = case_when(
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



base <- base %>% dplyr::mutate(ipc_ene2023 = case_when(
  mes == 1 ~  1,
  mes == 2 ~  1.01553549,
  mes == 3 ~  1.02572442,
  mes == 4 ~  1.03496853,
  mes == 5 ~  1.04271023,
  mes == 6 ~  1.04260575,
  mes == 7 ~  1.03777454,
  mes == 8 ~  1.03402059,
  mes == 9 ~  1.03579843,
  mes == 10 ~  1.04212190,
  mes == 11 ~  1.04864307,
  mes == 12 ~  1.05224155))


bases_men <- bases_men %>% dplyr::mutate(ipc_ene2023 = case_when(
  mes == 1 ~  1,
  mes == 2 ~  1.01553549,
  mes == 3 ~  1.02572442,
  mes == 4 ~  1.03496853,
  mes == 5 ~  1.04271023,
  mes == 6 ~  1.04260575,
  mes == 7 ~  1.03777454,
  mes == 8 ~  1.03402059,
  mes == 9 ~  1.03579843,
  mes == 10 ~  1.04212190,
  mes == 11 ~  1.04864307,
  mes == 12 ~  1.05224155))

# Ingresos

base <- base %>% dplyr::mutate(y_pc       =  HT11 / HT19 )                      #Ingreso per-cápita
base <- base %>% dplyr::mutate(y_pc_svl   =  (HT11 - HT13) / HT19)              #Ingreso per-cápita sin valor locativo
base <- base %>% dplyr::mutate(y_pc_d     =  HT11 / HT19 / bc_ipc_tot)          #Ingreso per-cápita deflactado
base <- base %>% dplyr::mutate(y_pc_svl_d =  (HT11 - HT13) / HT19 / bc_ipc_tot) #Ingreso per-cápita sin valor locativo deflactado

bases_men <- bases_men %>% dplyr::mutate(y_pc       =  HT11 / HT19 )                      #Ingreso per-cápita
bases_men <- bases_men %>% dplyr::mutate(y_pc_svl   =  (HT11 - HT13) / HT19)              #Ingreso per-cápita sin valor locativo
bases_men <- bases_men %>% dplyr::mutate(y_pc_d     =  HT11 / HT19 / bc_ipc_tot)          #Ingreso per-cápita deflactado
bases_men <- bases_men %>% dplyr::mutate(y_pc_svl_d =  (HT11 - HT13) / HT19 / bc_ipc_tot) #Ingreso per-cápita sin valor locativo deflactado

# Quintil de ingresos

base_h_a <- base %>% distinct(ID, .keep_all = TRUE)
base_h_a <- base_h_a %>% dplyr::mutate(quintilesy = statar::xtile(y_pc, n=5, wt = W_ANO))
base_h_a <- base_h_a[,c("ID","quintilesy")]
base <- merge(base, base_h_a, by = "ID")

base_h <- bases_men %>% distinct(ID, .keep_all = TRUE)
base_h <- base_h %>% dplyr::mutate(quintilesy = statar::xtile(y_pc, n=5, wt = W))
base_h <- base_h[,c("ID","quintilesy")]
bases_men <- merge(bases_men, base_h, by = "ID")

# Sexo

base <- base %>% dplyr::mutate(bc_pe2 = e26)
bases_men <- bases_men %>% dplyr::mutate(bc_pe2 = e26)


# Ascendencia afro

base <- base %>% dplyr::mutate(bd_e29_1 = e29_1)
bases_men <- bases_men %>% dplyr::mutate(bd_e29_1 = e29_1)


# Desempleados sin Seguro de Desempleo

base <- base %>% dplyr::mutate(seguroparo = ifelse(POBPCOAC == 4, 1, 0))
bases_men <- bases_men %>% dplyr::mutate(seguroparo = ifelse(POBPCOAC == 4, 1, 0))


# Percepción de jubilaciones

base <- base %>% dplyr::mutate(jub = ifelse(g148_1_1 > 0 |
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

base <- base %>% dplyr::mutate(pens = ifelse(g148_2_1 > 0 |
                                             g148_2_2 > 0 |
                                             g148_2_3 > 0 |
                                             g148_2_5 > 0 |
                                             g148_2_6 > 0 |
                                             g148_2_7 > 0 |
                                             g148_2_8 > 0 |
                                             g148_2_9 > 0 |
                                             g148_2_10 > 0 |
                                             g148_2_11 > 0, 1, 0)) 


# Percepción de jubilaciones o pensiones

base <- base %>% dplyr::mutate(nijubpens = ifelse(jub == 1 | pens == 1, 0, 1))


# No aporte a SS en trabajo principal y secundario

base <- base %>% dplyr::mutate(bc_register2 = ifelse(f96 == 1 | f82 == 1, 0, 1))


# No aporte a SS en trabajo principal

base <- base %>% dplyr::mutate(bc_register = ifelse(f82 == 1, 0, 1))
bases_men <- bases_men %>% dplyr::mutate(bc_register = ifelse(f82 == 1, 0, 1))


# No aporte por totalidad del salario

base <- base %>% dplyr::mutate(noaportatot = ifelse(f84 == 2, 1, 0))


# No percepción de transferencias

base <- base %>% dplyr::mutate(p_afam = ifelse(g150 == 1, 1, 0))
base_afam <- base[,c("ID","p_afam")]
base_afam <- base_afam %>% dplyr::mutate(h_afam = p_afam)
base_afam <- base_afam[order(base_afam$ID, base_afam$h_afam, decreasing = TRUE), ]
base_afam <- base_afam %>% distinct(ID, .keep_all = TRUE)
base_afam <- base_afam[,c("ID","h_afam")]
base <- merge(base, base_afam, by = "ID")

base <- base %>% dplyr::mutate(p_tus = ifelse(e560 == 1, 1, 0))
base_tus <- base[,c("ID","p_tus")]
base_tus <- base_tus %>% dplyr::mutate(h_tus = p_tus)
base_tus <- base_tus[order(base_tus$ID, base_tus$h_tus, decreasing = TRUE), ]
base_tus <- base_tus %>% distinct(ID, .keep_all = TRUE)
base_tus <- base_tus[,c("ID","h_tus")]
base <- merge(base, base_tus, by = "ID")

base <- base %>% dplyr::mutate(nopercibe = ifelse(h_afam == 0 & h_tus == 0, 1, 0))


# Desempleados de 1 a 6 meses

base <- base %>% dplyr::mutate(desemp1a6 = ifelse(f113 >= 4 & f113<=24, 1, 0))
bases_men <- bases_men %>% dplyr::mutate(desemp1a6 = ifelse(f113 >= 4 & f113<=24, 1, 0))


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


# Ingresos desempleo por debajo de línea de pobreza

base <- base %>% dplyr::mutate(desempinsuf = ifelse(g148_3 < lp_unipersonales, 1, 0))


# Ingresos desempleo por debajo de la mitad de la media y mediana del salario nacional

base_coningresos <- base %>%  filter(PT4>0)

base_coningresos_svy <- srvyr::as_survey_design(base_coningresos, ids = ID, weights = W_ANO)
base_media <-  base_coningresos_svy %>% srvyr::summarise(colname = srvyr::survey_mean(PT4))
base_media <-  base_media$colname

base_mediana <-  base_coningresos_svy %>% srvyr::summarise(colname = srvyr::survey_median(PT4))
base_mediana <-  base_mediana$colname

base <- base %>% dplyr::mutate(desempinsuf_media   = ifelse(g148_3 < (base_media/2), 1, 0))
base <- base %>% dplyr::mutate(desempinsuf_mediana = ifelse(g148_3 < (base_mediana/2), 1, 0))


# Ingresos desempleo por debajo del salario mínimo nacional

base <- base %>% dplyr::mutate(subsidio_def = g148_3 /  ipc_ene2023)

base <- base %>% dplyr::mutate(desempinsuf_smn = ifelse(subsidio_def < smn, 1, 0))


# Ingresos por jubilaciones

base <- base %>% dplyr::mutate(y_jub = g148_1_1 + g148_1_2 + g148_1_3 + g148_1_5 +
                                 g148_1_6 + g148_1_7 + g148_1_8 + g148_1_9 + 
                                 g148_1_12 + g148_1_10 + g148_1_11)


# Ingresos por jubilaciones por debajo de la línea de pobreza

base <- base %>% dplyr::mutate(jub_insuf = ifelse(y_jub < lp_unipersonales, 1, 0))


# Ingresos por jubilaciones por debajo de la media y mediana del salario nacional

base <- base %>% dplyr::mutate(jub_insuf_media = ifelse(y_jub < (base_media/2), 1, 0))
base <- base %>% dplyr::mutate(jub_insuf_mediana = ifelse(y_jub < (base_mediana/2), 1, 0))


# Ingresos por jubilaciones por debajo del salario mínimo nacional

base <- base %>% dplyr::mutate(y_jub_def = y_jub /  ipc_ene2023)
base <- base %>% dplyr::mutate(jub_insuf_smn = ifelse(y_jub_def < smn, 1, 0))


# Ingresos por pensiones

base <- base %>% dplyr::mutate(y_pens = g148_2_1 + g148_2_2 + g148_2_3 + g148_2_5 + 
                                 g148_2_6 + g148_2_7 + g148_2_8 + g148_2_9 + 
                                 g148_2_12 + g148_2_10 + g148_2_11)


# Ingresos por pensiones debajo de la línea de pobreza

base <- base %>% dplyr::mutate(pens_insuf = ifelse(y_pens < lp_unipersonales, 1, 0))

# Ingresos por pensiones debajo de la mitad de la media y la mediana del salario nacional

base <- base %>% dplyr::mutate(pens_insuf_media = ifelse(y_pens < (base_media/2), 1, 0))
base <- base %>% dplyr::mutate(pens_insuf_mediana = ifelse(y_pens < (base_mediana/2), 1, 0))

# Ingresos por pensiones debajo del SMN

base <- base %>% dplyr::mutate(y_pens_def = y_pens /  ipc_ene2023)

base <- base %>% dplyr::mutate(pens_insuf_smn = ifelse(y_pens_def < smn, 1, 0))


# Ingresos por transferencias

base <- base %>% dplyr::mutate(y_transf = e560_1_1 + e560_2_1 + g257)
base <-  base %>%
  group_by(ID) %>%
  mutate(y_transf_h = sum(y_transf)) %>% ungroup()

base <- base %>% dplyr::mutate(hpc_y_transf =y_transf_h/HT19)
base <- base %>% dplyr::mutate(trans_insuf= ifelse(hpc_y_transf < li_06, 1, 0))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

### Información de muestreo ### 

base_svy <- srvyr::as_survey_design(base, ids = ID, weights = W_ANO)
base_men_svy <- srvyr::as_survey_design(bases_men, ids = ID, weights = W)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
### Procesamiento de indicadores ###
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de desempleados que no cobra seguro de desempleo
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Total

c_ano <- base_men_svy %>%
  srvyr::filter(POBPCOAC>=4 & POBPCOAC<=5) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(seguroparo))

c_ano <- mean(c_ano$colname)

CODIND       <- 330101
DIMENSIÓN    <- "Accesibilidad"
CODINDICADOR <- "Desempleados que no perciben subsidio"
NOMINDICADOR <- "Porcentaje de desempleados que no cobra seguro de desempleo"
VALOR	       <- c_ano*100
CORTE        <- "Total"
DEPARTAMENTO <- ""
QUINTIL	     <- ""
SEXO		     <- ""
ASCENDENCIA  <- ""
JERARQUIA    <- 1
NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. La estimaciones desde 2022 se calculan a partir de la muestra panel. Las variables de corte son incorporadas a partir del formulario de implantación."

ss_01 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, 
               AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, CORTE, DEPARTAMENTO, 
               QUINTIL, SEXO, ASCENDENCIA, NOTA_INDICADOR)




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de mayores de 65 años que no perciben jubilaciones ni pensiones
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Total

c_ano <- base_svy %>%
  srvyr::filter(e27>=65 & POBPCOAC!= 2) %>%
  srvyr::summarise(colname = srvyr::survey_mean(nijubpens))

c_ano <-c_ano$colname


# Ascendencia étnico racial

c_afro <- function(x) {
  x <- base_svy %>%
    filter(bd_e29_1 == x & e27>=65 & POBPCOAC!= 2) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nijubpens))
  x <- mean(x$colname)
}       

c_e_afro <- numeric()

for(i in 1:2){
  c_e_afro[i] <- c_afro(x = i)
}  


# Departamento

c_dpto <- function(x) {
  x <- base_svy %>%
    filter(dpto == x & e27>=65 & POBPCOAC!= 2) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nijubpens))
  x <- mean(x$colname)
}       

c_e_dpto <- numeric()

for(i in 1:19){
  c_e_dpto[i] <- c_dpto(x = i)
}         



# Quintil de ingresos

c_quintil <- function(x) {
  x <- base_svy %>%
    filter(quintilesy == x & e27>=65 & POBPCOAC!= 2) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nijubpens))
  x <- mean(x$colname)
}       

c_e_quintil <- numeric()

for(i in 1:5){
  c_e_quintil[i] <- c_quintil(x = i)
}         



# Sexo

c_sexo <- function(x) {
  x <- base_svy %>%
    filter(bc_pe2 == x & e27>=65 & POBPCOAC!= 2) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nijubpens))
  x <- mean(x$colname)
}       

c_e_sexo <- numeric()

for(i in 1:2){
  c_e_sexo[i] <- c_sexo(x = i)
}         



CODIND       <- 330102
DIMENSIÓN    <- "Accesibilidad"
CODINDICADOR <- "Mayores de 65 años sin pensión ni jubilación"
NOMINDICADOR <- "Porcentaje de mayores de 65 años que no perciben jubilaciones ni pensiones"
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
NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. La estimaciones desde 2022 se calculan a partir de la muestra de implantación."

ss_02 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, 
               AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, CORTE, DEPARTAMENTO, 
               QUINTIL, SEXO, ASCENDENCIA, NOTA_INDICADOR)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de la población económicamente activa que no aporta a la seguridad social
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Total

c_ano <- base_svy %>%
  srvyr::filter(POBPCOAC>=2 & POBPCOAC<=5) %>%
  srvyr::summarise(colname = srvyr::survey_mean(bc_register2))

c_ano <-c_ano$colname

# Ascendencia étnico racial

c_afro <- function(x) {
  x <- base_svy %>%
    filter(bd_e29_1 == x & POBPCOAC>=2 & POBPCOAC<=5) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_register2))
  x <- mean(x$colname)
}       

c_e_afro <- numeric()

for(i in 1:2){
  c_e_afro[i] <- c_afro(x = i)
}         


# Departamento

c_dpto <- function(x) {
  x <- base_svy %>%
    filter(dpto == x & POBPCOAC>=2 & POBPCOAC<=5) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_register2))
  x <- mean(x$colname)
}       

c_e_dpto <- numeric()

for(i in 1:19){
  c_e_dpto[i] <- c_dpto(x = i)
}         


# Quintil de ingresos

c_quintil <- function(x) {
  x <- base_svy %>%
    filter(quintilesy == x & POBPCOAC>=2 & POBPCOAC<=5) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_register2))
  x <- mean(x$colname)
}       

c_e_quintil <- numeric()

for(i in 1:5){
  c_e_quintil[i] <- c_quintil(x = i)
}         


# Sexo

c_sexo <- function(x) {
  x <- base_svy %>%
    filter(bc_pe2 == x & POBPCOAC>=2 & POBPCOAC<=5) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_register2))
  x <- mean(x$colname)
}       

c_e_sexo <- numeric()

for(i in 1:2){
  c_e_sexo[i] <- c_sexo(x = i)
}         


CODIND       <- 330103
DIMENSIÓN    <- "Accesibilidad"
CODINDICADOR <- "Población activa que no aporta a la seguridad social"
NOMINDICADOR <- "Porcentaje de la población económicamente activa que no aporta a la seguridad social"
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
NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. La estimaciones desde 2022 se calculan a partir de la muestra de implantación."

ss_03 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, 
               AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, CORTE, DEPARTAMENTO, 
               QUINTIL, SEXO, ASCENDENCIA, NOTA_INDICADOR)




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de ocupados que no aporta a la seguridad social
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Total

c_ano <- base_svy %>%
  srvyr::filter(POBPCOAC == 2) %>%
  srvyr::summarise(colname = srvyr::survey_mean(bc_register2))

c_ano <-c_ano$colname


# Ascendencia étnico racial

c_afro <- function(x) {
  x <- base_svy %>%
    filter(bd_e29_1 == x  & POBPCOAC == 2) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_register2))
  x <- mean(x$colname)
}       

c_e_afro <- numeric()

for(i in 1:2){
  c_e_afro[i] <- c_afro(x = i)
}         


# Departamento

c_dpto <- function(x) {
  x <- base_svy %>%
    filter(dpto == x  & POBPCOAC ==2) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_register2))
  x <- mean(x$colname)
}       

c_e_dpto <- numeric()

for(i in 1:19){
  c_e_dpto[i] <- c_dpto(x = i)
}         


# Quintil de ingresos

c_quintil <- function(x) {
  x <- base_svy %>%
    filter(quintilesy == x  & POBPCOAC ==2) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_register2))
  x <- mean(x$colname)
}       

c_e_quintil <- numeric()

for(i in 1:5){
  c_e_quintil[i] <- c_quintil(x = i)
}         


# Sexo

c_sexo <- function(x) {
  x <- base_svy %>%
    filter(bc_pe2 == x & POBPCOAC ==2) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_register2))
  x <- mean(x$colname)
}       

c_e_sexo <- numeric()

for(i in 1:2){
 c_e_sexo[i] <- c_sexo(x = i)
}         



CODIND       <- 330104
DIMENSIÓN    <- "Accesibilidad"
CODINDICADOR <- "Porcentaje de ocupados que no aporta a la seguridad social"
NOMINDICADOR <- "Porcentaje de ocupados que no aporta a la seguridad social"
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
NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. La estimaciones desde 2022 se calculan a partir de la muestra de implantación."

ss_04 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, 
               AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, CORTE, DEPARTAMENTO, 
               QUINTIL, SEXO, ASCENDENCIA, NOTA_INDICADOR)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de ocupados que no aporta a la seguridad social por la totalidad del salario
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Total

c_ano <- base_svy %>%
  srvyr::filter(POBPCOAC == 2) %>%
  srvyr::summarise(colname = srvyr::survey_mean(noaportatot))

c_ano <-c_ano$colname

# Ascendencia étnico racial


c_afro <- function(x) {
  x <- base_svy %>%
    filter(bd_e29_1 == x  & POBPCOAC == 2) %>%
    srvyr::summarise(colname = srvyr::survey_mean(noaportatot))
  x <- mean(x$colname)
}       

c_e_afro <- numeric()

for(i in 1:2){
  c_e_afro[i] <- c_afro(x = i)
}         


# Departamento


c_dpto <- function(x) {
  x <- base_svy %>%
    filter(dpto == x  & POBPCOAC ==2) %>%
    srvyr::summarise(colname = srvyr::survey_mean(noaportatot))
  x <- mean(x$colname)
}       

c_e_dpto <- numeric()

for(i in 1:19){
  c_e_dpto[i] <- c_dpto(x = i)
}         


# Quintil de ingresos

c_quintil <- function(x) {
  x <- base_svy %>%
    filter(quintilesy == x  & POBPCOAC ==2) %>%
    srvyr::summarise(colname = srvyr::survey_mean(noaportatot))
  x <- mean(x$colname)
}       

c_e_quintil <- numeric()

for(i in 1:5){
  c_e_quintil[i] <- c_quintil(x = i)
}         


# Sexo

c_sexo <- function(x) {
  x <- base_svy %>%
    filter(bc_pe2 == x & POBPCOAC ==2) %>%
    srvyr::summarise(colname = srvyr::survey_mean(noaportatot))
  x <- mean(x$colname)
}       

c_e_sexo <- numeric()

for(i in 1:2){
  c_e_sexo[i] <- c_sexo(x = i)
}         



CODIND       <- 330105
DERECHO      <- "Seguridad Social"
CODINDICADOR <- "Informalidad parcial"
NOMINDICADOR <- "Porcentaje de ocupados que no aporta a la seguridad social por la totalidad del salario"
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
NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. La estimaciones desde 2022 se calculan a partir de la muestra de implantación."

ss_05 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, 
               AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, CORTE, DEPARTAMENTO, 
               QUINTIL, SEXO, ASCENDENCIA, NOTA_INDICADOR)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de personas que residen en hogares pobres que no perciben transferencias (TUS o AFAM)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Total

c_ano <- base_svy %>%
  srvyr::filter(pobre06==1) %>%
  srvyr::summarise(colname = srvyr::survey_mean(nopercibe))

c_ano <-c_ano$colname


CODIND       <- 330106
DIMENSIÓN    <- "Accesibilidad"
CODINDICADOR <- "Personas pobres que no reciben transferencias monetarias"
NOMINDICADOR <- "Porcentaje de personas que residen en hogares pobres que no perciben transferencias (TUS o AFAM)"
VALOR	       <- c_ano*100
JERARQUIA	   <- 1
CORTE        <- "Total"
DEPARTAMENTO <- ""
QUINTIL	     <- ""
SEXO		     <- ""
ASCENDENCIA  <- ""
NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. La estimaciones desde 2022 se calculan a partir de la muestra de implantación."

ss_06 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, 
               AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, CORTE, DEPARTAMENTO, 
               QUINTIL, SEXO, ASCENDENCIA, NOTA_INDICADOR)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de mayores de 60 años que no perciben jubilaciones ni pensiones
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Total

c_ano <- base_svy %>%
  srvyr::filter(e27>=60 & POBPCOAC!= 2) %>%
  srvyr::summarise(colname = srvyr::survey_mean(nijubpens))

c_ano <-c_ano$colname

# Ascendencia étnico racial

c_afro <- function(x) {
  x <- base_svy %>%
    filter(bd_e29_1 == x & e27>=60 & POBPCOAC!= 2) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nijubpens))
  x <- mean(x$colname)
}       

c_e_afro <- numeric()

for(i in 1:2){
  c_e_afro[i] <- c_afro(x = i)
}         


# Departamento

c_dpto <- function(x) {
  x <- base_svy %>%
    filter(dpto == x & e27>=60 & POBPCOAC!= 2) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nijubpens))
  x <- mean(x$colname)
}       

c_e_dpto <- numeric()

for(i in 1:19){
  c_e_dpto[i] <- c_dpto(x = i)
}         


# Quintil de ingresos

c_quintil <- function(x) {
  x <- base_svy %>%
    filter(quintilesy == x & e27>=60 & POBPCOAC!= 2) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nijubpens))
  x <- mean(x$colname)
}       

c_e_quintil <- numeric()

for(i in 1:5){
  c_e_quintil[i] <- c_quintil(x = i)
}         


# Sexo

c_sexo <- function(x) {
  x <- base_svy %>%
    filter(bc_pe2 == x & e27>=60 & POBPCOAC!= 2) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nijubpens))
  x <- mean(x$colname)
}       

c_e_sexo <- numeric()

for(i in 1:2){
  c_e_sexo[i] <- c_sexo(x = i)
}         



CODIND       <- 330107
DIMENSIÓN    <- "Accesibilidad"
CODINDICADOR <- "Inactivos mayores de 60 años sin pensión ni jubilación"
NOMINDICADOR <- "Porcentaje de inactivos mayores de 60 años que no perciben jubilaciones ni pensiones"
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
NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. La estimaciones desde 2022 se calculan a partir de la muestra de implantación."

ss_07 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, 
               AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, CORTE, DEPARTAMENTO, 
               QUINTIL, SEXO, ASCENDENCIA, NOTA_INDICADOR)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de personas desempleadas durante uno a seis meses que cobra seguro de desempleo por debajo de la línea de pobreza
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Total

c_ano <- base_svy %>%
  srvyr::filter(desemp1a6 ==1) %>%
  srvyr::summarise(colname = srvyr::survey_mean(desempinsuf))

c_ano <-c_ano$colname


CODIND       <- 330201
DIMENSIÓN    <- "Adecuación"
CODINDICADOR <- "Insuficiencia del subsidio por desempleo en relación a la línea de pobreza"
NOMINDICADOR <- "Porcentaje de personas desempleadas durante uno a seis meses que cobra seguro de desempleo por debajo de la línea de pobreza"
VALOR	       <- c_ano*100
JERARQUIA	   <- 1
CORTE        <- "Total"
DEPARTAMENTO <- ""
QUINTIL	     <- ""
SEXO		     <- ""
ASCENDENCIA  <- ""
NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. La estimaciones desde 2022 se calculan a partir de la muestra de implantación."

ss_08 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, 
               AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, CORTE, DEPARTAMENTO, 
               QUINTIL, SEXO, ASCENDENCIA, NOTA_INDICADOR)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de personas desempleadas durante uno a seis meses que cobra seguro de desempleo por debajo de la mitad de la media del salario nacional
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Total

c_ano <- base_svy %>%
  srvyr::filter(desemp1a6 ==1) %>%
  srvyr::summarise(colname = srvyr::survey_mean(desempinsuf_media))

c_ano <-c_ano$colname

CODIND       <- 330202
DIMENSIÓN    <- "Adecuación"
CODINDICADOR <- "Insuficiencia del subsidio por desempleo en relación al salario nacional (mitad del promedio)"
NOMINDICADOR <- "Porcentaje de personas desempleadas durante uno a seis meses que cobra seguro de desempleo por debajo de la mitad de la media del salario nacional"
VALOR	       <- c_ano*100
JERARQUIA	   <- 1
CORTE        <- "Total"
DEPARTAMENTO <- ""
QUINTIL	     <- ""
SEXO		     <- ""
ASCENDENCIA  <- ""
NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. La estimaciones desde 2022 se calculan a partir de la muestra de implantación."

ss_09 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, 
               AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, CORTE, DEPARTAMENTO, 
               QUINTIL, SEXO, ASCENDENCIA, NOTA_INDICADOR)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de personas desempleadas durante uno a seis meses que cobra seguro de desempleo por debajo de la mitad de la mediana del salario nacional
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Total

c_ano <- base_svy %>%
  srvyr::filter(desemp1a6 ==1) %>%
  srvyr::summarise(colname = srvyr::survey_mean(desempinsuf_mediana))

c_ano <-c_ano$colname

CODIND       <- 330203
DIMENSIÓN    <- "Adecuación"
CODINDICADOR <- "Insuficiencia del subsidio por desempleo en relación al salario nacional (mitad de la mediana)"
NOMINDICADOR <- "Porcentaje de personas desempleadas durante uno a seis meses que cobra seguro de desempleo por debajo de la mitad de la mediana del salario nacional"
VALOR	       <- c_ano*100
JERARQUIA	   <- 1
CORTE        <- "Total"
DEPARTAMENTO <- ""
QUINTIL	     <- ""
SEXO		     <- ""
ASCENDENCIA  <- ""
NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. La estimaciones desde 2022 se calculan a partir de la muestra de implantación."

ss_10 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, 
               AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, CORTE, DEPARTAMENTO, 
               QUINTIL, SEXO, ASCENDENCIA, NOTA_INDICADOR)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de personas desempleadas durante uno a seis meses que cobra seguro de desempleo por debajo del SMN
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Total

c_ano <- base_svy %>%
  srvyr::filter(desemp1a6 ==1) %>%
  srvyr::summarise(colname = srvyr::survey_mean(desempinsuf_smn))

c_ano <-c_ano$colname

CODIND       <- 330204
DIMENSIÓN    <- "Adecuación"
CODINDICADOR <- "Insuficiencia del subsidio por desempleo en relación al salario mínimo nacional"
NOMINDICADOR <- "Porcentaje de personas desempleadas durante uno a seis meses que cobra seguro de desempleo por debajo del salario mínimo nacional"
VALOR	       <- c_ano*100
JERARQUIA	   <- 1
CORTE        <- "Total"
DEPARTAMENTO <- ""
QUINTIL	     <- ""
SEXO		     <- ""
ASCENDENCIA  <- ""
NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. La estimaciones desde 2022 se calculan a partir de la muestra de implantación."

ss_11 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, 
               AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, CORTE, DEPARTAMENTO, 
               QUINTIL, SEXO, ASCENDENCIA, NOTA_INDICADOR)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de personas que perciben jubilaciones por debajo de la línea de pobreza
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Total

c_ano <- base_svy %>%
  srvyr::filter(y_jub > 0) %>%
  srvyr::summarise(colname = srvyr::survey_mean(jub_insuf))

c_ano <-c_ano$colname


CODIND       <- 330205
DIMENSIÓN    <- "Adecuación"
CODINDICADOR <- "Insuficiencia de las jubilaciones en relación a la línea de las pobreza"
NOMINDICADOR <- "Porcentaje de personas que perciben jubilaciones por debajo de la línea de pobreza"
VALOR	       <- c_ano*100
JERARQUIA	   <- 1
CORTE        <- "Total"
DEPARTAMENTO <- ""
QUINTIL	     <- ""
SEXO		     <- ""
ASCENDENCIA  <- ""
NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. La estimaciones desde 2022 se calculan a partir de la muestra de implantación."

ss_12 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, 
               AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, CORTE, DEPARTAMENTO, 
               QUINTIL, SEXO, ASCENDENCIA, NOTA_INDICADOR)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de personas que perciben jubilaciones por debajo de mitad de la media del salario nacional
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Total

c_ano <- base_svy %>%
  srvyr::filter(y_jub > 0) %>%
  srvyr::summarise(colname = srvyr::survey_mean(jub_insuf_media))

c_ano <-c_ano$colname


CODIND       <- 330206
DIMENSIÓN    <- "Adecuación"
CODINDICADOR <- "Insuficiencia de las jubilaciones en relación al salario nacional (mitad del promedio)"
NOMINDICADOR <- "Porcentaje de personas que perciben jubilaciones por debajo de la mitad de la media del salario nacional"
VALOR	       <- c_ano*100
JERARQUIA	   <- 1
CORTE        <- "Total"
DEPARTAMENTO <- ""
QUINTIL	     <- ""
SEXO		     <- ""
ASCENDENCIA  <- ""
NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. La estimaciones desde 2022 se calculan a partir de la muestra de implantación."

ss_13 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, 
               AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, CORTE, DEPARTAMENTO, 
               QUINTIL, SEXO, ASCENDENCIA, NOTA_INDICADOR)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de personas que perciben jubilaciones por debajo de mitad de la mediana del salario nacional
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Total


c_ano <- base_svy %>%
  srvyr::filter(y_jub > 0) %>%
  srvyr::summarise(colname = srvyr::survey_mean(jub_insuf_mediana))

c_ano <-c_ano$colname


CODIND       <- 330207
DIMENSIÓN    <- "Adecuación"
CODINDICADOR <- "Insuficiencia de las jubilaciones en relación al salario nacional (mitad de las la mediana)"
NOMINDICADOR <- "Porcentaje de personas que perciben jubilaciones por debajo de la mitad de la mediana del salario nacional"
VALOR	       <- c_ano*100
JERARQUIA	   <- 1
CORTE        <- "Total"
DEPARTAMENTO <- ""
QUINTIL	     <- ""
SEXO		     <- ""
ASCENDENCIA  <- ""
NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. La estimaciones desde 2022 se calculan a partir de la muestra de implantación."

ss_14 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, 
               AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, CORTE, DEPARTAMENTO, 
               QUINTIL, SEXO, ASCENDENCIA, NOTA_INDICADOR)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de personas que perciben jubilaciones por debajo del SMN
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Total

c_ano <- base_svy %>%
  srvyr::filter(y_jub > 0) %>%
  srvyr::summarise(colname = srvyr::survey_mean(jub_insuf_smn))

c_ano <-c_ano$colname


CODIND       <- 330208
DIMENSIÓN    <- "Adecuación"
CODINDICADOR <- "Insuficiencia de las jubilaciones en relación al salario mínimo nacional"
NOMINDICADOR <- "Porcentaje de personas que perciben jubilaciones por debajo del salario mínimo nacional"
VALOR	       <- c_ano*100
JERARQUIA	   <- 1
CORTE        <- "Total"
DEPARTAMENTO <- ""
QUINTIL	     <- ""
SEXO		     <- ""
ASCENDENCIA  <- ""
NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. La estimaciones desde 2022 se calculan a partir de la muestra de implantación."

ss_15 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, 
               AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, CORTE, DEPARTAMENTO, 
               QUINTIL, SEXO, ASCENDENCIA, NOTA_INDICADOR)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de personas que perciben pensiones por debajo de la línea de pobreza
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Total

c_ano <- base_svy %>%
  srvyr::filter(y_pens > 0) %>%
  srvyr::summarise(colname = srvyr::survey_mean(pens_insuf))

c_ano <-c_ano$colname


CODIND       <- 330209
DIMENSIÓN    <- "Adecuación"
CODINDICADOR <- "Insuficiencia de las pensiones en relación a la línea de las pobreza"
NOMINDICADOR <- "Porcentaje de personas que perciben pensiones por debajo de la línea de pobreza"
VALOR	       <- c_ano*100
JERARQUIA	   <- 1
CORTE        <- "Total"
DEPARTAMENTO <- ""
QUINTIL	     <- ""
SEXO		     <- ""
ASCENDENCIA  <- ""
NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. La estimaciones desde 2022 se calculan a partir de la muestra de implantación."

ss_16 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, 
               AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, CORTE, DEPARTAMENTO, 
               QUINTIL, SEXO, ASCENDENCIA, NOTA_INDICADOR)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de personas que perciben pensiones por debajo de la mitad de la media del salario nacional
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Total

c_ano <- base_svy %>%
  srvyr::filter(y_pens > 0) %>%
  srvyr::summarise(colname = srvyr::survey_mean(pens_insuf_media))

c_ano <-c_ano$colname

CODIND       <- 330210
DIMENSIÓN    <- "Adecuación"
CODINDICADOR <- "Insuficiencia de las pensiones en relación al salario nacional (mitad del promedio)"
NOMINDICADOR <- "Porcentaje de personas que perciben pensiones por debajo de la mitad de la media del salario nacional"
VALOR	       <- c_ano*100
JERARQUIA	   <- 1
CORTE        <- "Total"
DEPARTAMENTO <- ""
QUINTIL	     <- ""
SEXO		     <- ""
ASCENDENCIA  <- ""
NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. La estimaciones desde 2022 se calculan a partir de la muestra de implantación."

ss_17 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, 
               AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, CORTE, DEPARTAMENTO, 
               QUINTIL, SEXO, ASCENDENCIA, NOTA_INDICADOR)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de personas que perciben pensiones por debajo de la mitad de la mediana del salario nacional
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Total

c_ano <- base_svy %>%
  srvyr::filter(y_pens > 0) %>%
  srvyr::summarise(colname = srvyr::survey_mean(pens_insuf_mediana))

c_ano <-c_ano$colname


CODIND       <- 330211
DIMENSIÓN    <- "Adecuación"
CODINDICADOR <- "Insuficiencia de las pensiones en relación al salario nacional (mitad de las la mediana)"
NOMINDICADOR <- "Porcentaje de personas que perciben pensiones por debajo de la mitad de la mediana del salario nacional"
VALOR	       <- c_ano*100
JERARQUIA	   <- 1
CORTE        <- "Total"
DEPARTAMENTO <- ""
QUINTIL	     <- ""
SEXO		     <- ""
ASCENDENCIA  <- ""
NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. La estimaciones desde 2022 se calculan a partir de la muestra de implantación."

ss_18 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, 
               AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, CORTE, DEPARTAMENTO, 
               QUINTIL, SEXO, ASCENDENCIA, NOTA_INDICADOR)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de personas que perciben pensiones por debajo del SMN
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Total

c_ano <- base_svy %>%
  srvyr::filter(y_pens > 0) %>%
  srvyr::summarise(colname = srvyr::survey_mean(pens_insuf_smn))

c_ano <-c_ano$colname

CODIND       <- 330212
DIMENSIÓN    <- "Adecuación"
CODINDICADOR <- "Insuficiencia de las pensiones en relación al salario mínimo nacional"
NOMINDICADOR <- "Porcentaje de personas que perciben pensiones por debajo del salario mínimo nacional"
VALOR	       <- c_ano*100
JERARQUIA	   <- 1
CORTE        <- "Total"
DEPARTAMENTO <- ""
QUINTIL	     <- ""
SEXO		     <- ""
ASCENDENCIA  <- ""
NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. La estimaciones desde 2022 se calculan a partir de la muestra de implantación."

ss_19 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, 
               AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, CORTE, DEPARTAMENTO, 
               QUINTIL, SEXO, ASCENDENCIA, NOTA_INDICADOR)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de personas que residen en hogares que perciben transferencias (TUS y/o AFAM) cuyo monto per-cápita es inferior a una canasta básica alimentaria
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Total

c_ano <- base_svy %>%
  srvyr::filter(y_transf_h > 0) %>%
  srvyr::summarise(colname = srvyr::survey_mean(trans_insuf))

c_ano <-c_ano$colname


CODIND       <- 330213
DIMENSIÓN    <- "Adecuación"
CODINDICADOR <- "Insuficiencia de las transferencias monetarias en relación a la canasta básica alimentaria"
NOMINDICADOR <- "Porcentaje de personas que residen en hogares que perciben transferencias (TUS y/o AFAM) cuyo monto per-cápita es inferior a una canasta básica alimentaria"
VALOR	       <- c_ano*100
JERARQUIA	   <- 1
CORTE        <- "Total"
DEPARTAMENTO <- ""
QUINTIL	     <- ""
SEXO		     <- ""
ASCENDENCIA  <- ""
NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. Las estimaciones desde 2022 se calcula a partir de la muestra de implantación."

ss_20 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, 
               AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, CORTE, DEPARTAMENTO, 
               QUINTIL, SEXO, ASCENDENCIA, NOTA_INDICADOR)


ss_2023 <- rbind(ss_01, ss_02, ss_03, ss_04, ss_05, ss_06, ss_07, ss_08, ss_09, ss_10, ss_11, ss_12, ss_13, ss_14, ss_15, ss_16, ss_17, ss_18, ss_19, ss_20)

rio::export(ss_2023, "ss_2023.xlsx" )


