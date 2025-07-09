### Mirador DESCA
### Niños, niñas y adolescentes - Derecho a la Seguridad Social
### Unidad de Métodos y Acceso a datos


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

library(rio)
library(survey)
library(srvyr)
library(tidyverse)
library(laeken)
library(statar)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Referencias para la base motor

DERECHO      <- "Seguridad Social"
TIPOIND      <- "Resultados"
DIMENSIÓN    <- "Accesibilidad"
AÑO	         <- 2023
FUENTE	     <- "Encuesta Continua de Hogares 2023, INE"
RESPONSABLE	 <- "J. Pandolfi"
JERARQUIA	   <- 1
JERARQUIA_CAT<- 1
CORTE        <- "Total"


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

### Carga de bases ###

base          <- rio::import("ECH_2023.csv")
bases_men       <- rio::import("ECH_men_23.csv")

varsimplant <- select(base, ID, HT11, HT19, HT13, e30)
varsimplant <- varsimplant %>% distinct(ID, .keep_all = TRUE)

bases_men       <- base::merge(bases_men, varsimplant, by = "ID", all.x = TRUE, all.y = FALSE)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

### Generación de nuevas variables ###

# NNA

base <- base %>% dplyr::mutate(nna = ifelse(e27>= 0 & e27<=17, 1, 0))
bases_men <- bases_men %>% dplyr::mutate(nna = ifelse(e27>= 0 & e27<=17, 1, 0))

proyeccion_ine <- 828014

#Para que quede para los siguientes años:	
#2024: 823853	
#2025: 820094	
#2026: 816403	
#2027: 812667	
#2028: 808893	
#2029: 805032


# Captación AFAM

base <- base %>% dplyr::mutate(afam_pe = ifelse((g152 == 1 & f73 != 2 & f92 !=2 ) | (g150 == 1 & (POBPCOAC %in% c(1, 3, 4, 6, 7, 8, 11) | (POBPCOAC == 2 & f82 != 1 & f96 != 1))), 1, 0),   
                                               afam_cont = ifelse(g150 == 1 & afam_pe == 0, 1, 0),
                                               afam_total = ifelse(afam_pe == 1 | afam_cont == 1, 1, 0)) 

base_afam <- base[,c("ID","afam_pe", "afam_cont", "afam_total")]
base_afam <- base_afam %>% dplyr::mutate(h_afam_pe = afam_pe,
                                         h_afam_cont = afam_cont,
                                         h_afam_total = afam_total)

base_afam_1 <- base_afam[order(base_afam$ID, base_afam$h_afam_pe, decreasing = TRUE), ]
base_afam_2 <- base_afam[order(base_afam$ID, base_afam$h_afam_cont, decreasing = TRUE), ]
base_afam_3 <- base_afam[order(base_afam$ID, base_afam$h_afam_total, decreasing = TRUE), ]

base_afam_1 <- base_afam_1 %>% distinct(ID, .keep_all = TRUE)
base_afam_2 <- base_afam_2 %>% distinct(ID, .keep_all = TRUE)
base_afam_3 <- base_afam_3 %>% distinct(ID, .keep_all = TRUE)

base_afam_1 <- base_afam_1[,c("ID","h_afam_pe")]
base_afam_2 <- base_afam_2[,c("ID","h_afam_cont")]
base_afam_3 <- base_afam_3[,c("ID","h_afam_total")]

base <- merge(base, base_afam_1, by = "ID")
base <- merge(base, base_afam_2, by = "ID")
base <- merge(base, base_afam_3, by = "ID")



#  Captación TUS


base <- base %>% dplyr::mutate(tus = ifelse(e560==1, 1, 0))

base_tus <- base[,c("ID","tus")]
base_tus <- base_tus %>% dplyr::mutate(h_tus = tus)
base_tus <- base_tus[order(base_tus$ID, base_tus$h_tus, decreasing = TRUE), ]
base_tus <- base_tus %>% distinct(ID, .keep_all = TRUE)
base_tus <- base_tus[,c("ID","h_tus")]
base <- merge(base, base_tus, by = "ID")




# No percepción de transferencias

base <- base %>% dplyr::mutate(noafam_pe = ifelse(h_afam_pe == 0 , 1, 0))

base <- base %>% dplyr::mutate(notus = ifelse(h_tus == 0 , 1, 0))

base <- base %>% dplyr::mutate(notrans = ifelse(h_tus == 0 & h_afam_pe == 0, 1, 0))


# Jefe o cónyugue desempleado sin Seguro de Desempleo


bases_men <- bases_men %>% dplyr::mutate(jefenoseguro    = ifelse(e30 ==1 & POBPCOAC == 4, 1, 0),
                               jefedesempleado = ifelse(e30 ==1 & (POBPCOAC == 4 | POBPCOAC == 5), 1, 0),
                               conynoseguro    = ifelse(e30 == 2 & POBPCOAC == 4, 1, 0),
                               coyndesempleado = ifelse(e30 ==2 & (POBPCOAC == 4 | POBPCOAC == 5), 1, 0))


base_jefe <- bases_men[,c("ID","jefenoseguro", "jefedesempleado", "conynoseguro", "coyndesempleado")]
base_jefe <- base_jefe %>% dplyr::mutate(h_jefenoseguro = jefenoseguro,
                                         h_jefedesempleado = jefedesempleado,
                                         h_conynoseguro = conynoseguro,
                                         h_coyndesempleado = coyndesempleado)

base_jefe_1 <- base_jefe[order(base_jefe$ID, base_jefe$h_jefenoseguro, decreasing = TRUE), ]
base_jefe_2 <- base_jefe[order(base_jefe$ID, base_jefe$h_jefedesempleado, decreasing = TRUE), ]
base_jefe_3 <- base_jefe[order(base_jefe$ID, base_jefe$h_conynoseguro, decreasing = TRUE), ]
base_jefe_4 <- base_jefe[order(base_jefe$ID, base_jefe$h_coyndesempleado, decreasing = TRUE), ]


base_jefe_1 <- base_jefe_1 %>% distinct(ID, .keep_all = TRUE)
base_jefe_2 <- base_jefe_2 %>% distinct(ID, .keep_all = TRUE)
base_jefe_3 <- base_jefe_3 %>% distinct(ID, .keep_all = TRUE)
base_jefe_4 <- base_jefe_4 %>% distinct(ID, .keep_all = TRUE)

base_jefe_1 <- base_jefe_1[,c("ID","h_jefenoseguro")]
base_jefe_2 <- base_jefe_2[,c("ID","h_jefedesempleado")]
base_jefe_3 <- base_jefe_3[,c("ID","h_conynoseguro")]
base_jefe_4 <- base_jefe_4[,c("ID","h_coyndesempleado")]

bases_men <- merge(bases_men, base_jefe_1, by = "ID")
bases_men <- merge(bases_men, base_jefe_2, by = "ID")
bases_men <- merge(bases_men, base_jefe_3, by = "ID")
bases_men <- merge(bases_men, base_jefe_4, by = "ID")

bases_men <- bases_men %>% dplyr::mutate(h_noseguro = ifelse(h_jefenoseguro == 1 | h_conynoseguro == 1, 1, 0),
                                         h_desempleado = ifelse(h_jefedesempleado == 1 | h_coyndesempleado == 1, 1, 0))



# Jefe o cónyuge ocupado que no aporta a la seguridad social


bases_men <- bases_men %>% dplyr::mutate(jefenoaporta = ifelse(e30 == 1 & POBPCOAC == 2 & f82 == 2, 1, 0),
                               jefeocupado = ifelse(e30 == 1 & POBPCOAC == 2, 1, 0),
                               conynoaporta = ifelse(e30 == 2 & POBPCOAC == 2 & f82 == 2, 1, 0),
                               conyocupado = ifelse(e30 == 2 & POBPCOAC == 2, 1, 0))


base_jefe <- bases_men[,c("ID","jefenoaporta", "jefeocupado", "conynoaporta", "conyocupado")]
base_jefe <- base_jefe %>% dplyr::mutate(h_jefenoaporta = jefenoaporta,
                                         h_jefeocupado = jefeocupado,
                                         h_conynoaporta = conynoaporta,
                                         h_conyocupado = conyocupado)

base_jefe_1 <- base_jefe[order(base_jefe$ID, base_jefe$h_jefenoaporta, decreasing = TRUE), ]
base_jefe_2 <- base_jefe[order(base_jefe$ID, base_jefe$h_jefeocupado, decreasing = TRUE), ]
base_jefe_3 <- base_jefe[order(base_jefe$ID, base_jefe$h_conynoaporta, decreasing = TRUE), ]
base_jefe_4 <- base_jefe[order(base_jefe$ID, base_jefe$h_conyocupado, decreasing = TRUE), ]


base_jefe_1 <- base_jefe_1 %>% distinct(ID, .keep_all = TRUE)
base_jefe_2 <- base_jefe_2 %>% distinct(ID, .keep_all = TRUE)
base_jefe_3 <- base_jefe_3 %>% distinct(ID, .keep_all = TRUE)
base_jefe_4 <- base_jefe_4 %>% distinct(ID, .keep_all = TRUE)

base_jefe_1 <- base_jefe_1[,c("ID","h_jefenoaporta")]
base_jefe_2 <- base_jefe_2[,c("ID","h_jefeocupado")]
base_jefe_3 <- base_jefe_3[,c("ID","h_conynoaporta")]
base_jefe_4 <- base_jefe_4[,c("ID","h_conyocupado")]

bases_men <- merge(bases_men, base_jefe_1, by = "ID")
bases_men <- merge(bases_men, base_jefe_2, by = "ID")
bases_men <- merge(bases_men, base_jefe_3, by = "ID")
bases_men <- merge(bases_men, base_jefe_4, by = "ID")

bases_men <- bases_men %>% dplyr::mutate(h_noaporta = ifelse(h_jefenoaporta == 1 | h_conynoaporta == 1, 1, 0),
                               denominador = ifelse(h_jefeocupado == 1 | h_conyocupado == 1, 1, 0))





# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

### Información de muestreo ### 

base_svy <- srvyr::as_survey_design(base, ids = ID, weights = W_ANO)
base_men_svy <- srvyr::as_survey_design(bases_men, ids = ID, weights = W)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
### Procesamiento de indicadores ###
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# NNA que residen en hogares con beneficiaros de AFAM contributivas
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


c_ano <- base_svy %>%
  srvyr::filter(nna == 1) %>%
  srvyr::summarise(colname = srvyr::survey_mean(h_afam_cont))

c_ano <-c_ano$colname

c_ano <- c_ano*proyeccion_ine
  
  
  
CODIND       <- 330107
CODINDICADOR <- "NNA que residen en hogares con beneficiaros de AFAM contributivas"
NOMINDICADOR <- "Cantidad de NNA que residen en hogares con beneficiaros de AFAM contributivas"
VALOR	       <- c_ano
NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. Las estimaciones desde 2022 se calculan a partir de la muestra de implantación."

nna_01 <- as.data.frame(cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, JERARQUIA_CAT, CORTE, NOTA_INDICADOR))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# NNA que residen en hogares con beneficiaros de AFAM-PE
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

c_ano <- base_svy %>%
  srvyr::filter(nna == 1) %>%
  srvyr::summarise(colname = srvyr::survey_mean(h_afam_pe))

c_ano <-c_ano$colname

c_ano <- c_ano*proyeccion_ine



CODIND       <- 330108
CODINDICADOR <- "NNA que residen en hogares con beneficiaros de AFAM-PE"
NOMINDICADOR <- "Cantidad de NNA que residen en hogares con beneficiaros de AFAM-PE"
VALOR	       <- c_ano
NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. Las estimaciones desde 2022 se calculan a partir de la muestra de implantación."

nna_02 <- as.data.frame(cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, JERARQUIA_CAT, CORTE, NOTA_INDICADOR))




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# NNA que residen en hogares con beneficiaros de TUS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

c_ano <- base_svy %>%
  srvyr::filter(nna == 1) %>%
  srvyr::summarise(colname = srvyr::survey_mean(h_tus))

c_ano <-c_ano$colname

c_ano <- c_ano*proyeccion_ine



CODIND       <- 330109
CODINDICADOR <- "NNA que residen en hogares con beneficiaros de TUS"
NOMINDICADOR <- "Cantidad de NNA que residen en hogares con beneficiaros de TUS"
VALOR	       <- c_ano
NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. Las estimaciones desde 2022 se calculan a partir de la muestra de implantación."

nna_03 <- as.data.frame(cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, JERARQUIA_CAT, CORTE, NOTA_INDICADOR))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# NNA en hogares pobres que no perciben AFAM-PE
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

c_ano <- base_svy %>%
  srvyr::filter(nna == 1 & pobre06 == 1) %>%
  srvyr::summarise(colname = srvyr::survey_mean(noafam_pe))

c_ano <-c_ano$colname


CODIND       <- 330110
CODINDICADOR <- "NNA en hogares pobres que no perciben AFAM-PE"
NOMINDICADOR <- "Porcentaje de NNA en hogares pobres que no perciben AFAM-PE"
VALOR	       <- c_ano*100
NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. Las estimaciones desde 2022 se calculan a partir de la muestra de implantación."

nna_04 <- as.data.frame(cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, JERARQUIA_CAT, CORTE, NOTA_INDICADOR))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# NNA en hogares pobres que no perciben TUS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

c_ano <- base_svy %>%
  srvyr::filter(nna == 1 & pobre06 == 1) %>%
  srvyr::summarise(colname = srvyr::survey_mean(notus))

c_ano <-c_ano$colname



CODIND       <- 330111
CODINDICADOR <- "NNA en hogares pobres que no perciben TUS"
NOMINDICADOR <- "Porcentaje de NNA en hogares pobres que no perciben TUS"
VALOR	       <- c_ano*100
NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. Las estimaciones desde 2022 se calculan a partir de la muestra de implantación."

nna_05 <- as.data.frame(cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, JERARQUIA_CAT, CORTE, NOTA_INDICADOR))



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# NNA en hogares pobres que no perciben AFAM-PE ni TUS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

c_ano <- base_svy %>%
  srvyr::filter(nna == 1 & pobre06 == 1) %>%
  srvyr::summarise(colname = srvyr::survey_mean(notrans))

c_ano <-c_ano$colname


CODIND       <- 330112
CODINDICADOR <- "NNA en hogares pobres que no perciben AFAM-PE ni TUS"
NOMINDICADOR <- "Porcentaje de NNA en hogares pobres que no perciben AFAM-PE ni TUS"
VALOR	       <- c_ano*100
NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. Las estimaciones desde 2022 se calculan a partir de la muestra panel. Las variables de corte son incorporadas a partir del formulario de implantación."

nna_06 <- as.data.frame(cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, JERARQUIA_CAT, CORTE, NOTA_INDICADOR))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# NNA en hogares con jefe/a y/o cónyuge desempleado sin seguro de desempleo
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


c_ano <- base_men_svy %>%
  srvyr::filter(nna == 1 & h_desempleado == 1) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(h_noseguro))

c_ano <-mean(c_ano$colname)


CODIND       <- 330113
CODINDICADOR <- "NNA en hogares con jefe/a y/o cónyuge desempleado sin seguro de desempleo"
NOMINDICADOR <- "Porcentaje de NNA en hogares con jefe/a y/o cónyuge desempleado sin seguro de desempleo"
VALOR	       <- c_ano*100
NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. Las estimaciones desde 2022 se calculan a partir de la muestra panel. Las variables de corte son incorporadas a partir del formulario de implantación."

nna_07 <- as.data.frame(cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, JERARQUIA_CAT, CORTE, NOTA_INDICADOR))




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# NNA en hogares con jefe/a y/o cónyuge ocupados sin aporte a la seguridad social
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

c_ano <- base_men_svy %>%
  srvyr::filter(nna == 1 & denominador == 1) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(h_noaporta))

c_ano <-mean(c_ano$colname)


CODIND       <- 330114
CODINDICADOR <- "NNA en hogares con jefe/a y/o cónyuge ocupados sin aporte a la seguridad social"
NOMINDICADOR <- "Porcentaje de NNA en hogares con jefe/a y/o cónyuge ocupados sin aporte a la seguridad social"
VALOR	       <- c_ano*100
NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. Las estimaciones desde 2022 se calculan a partir de la muestra panel. Las variables de corte son incorporadas a partir del formulario de implantación."

nna_08 <- as.data.frame(cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, JERARQUIA_CAT, CORTE, NOTA_INDICADOR))



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
### Generación de Base motor ###
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


nna_2023 <- rbind(nna_01, nna_02, nna_03, nna_04, nna_05, nna_06, nna_07, nna_08)

rio::export(nna_2023, "nna_2023.xlsx")


