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
AÑO	         <- 2020
FUENTE	     <- "Encuesta Continua de Hogares 2020, INE"
RESPONSABLE	 <- "J. Pandolfi"
JERARQUIA	   <- 1
JERARQUIA_CAT<- 1
CORTE        <- "Total"


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

### Carga de bases ###

ech2020 <- rio::import("C:/Users/Usuario/Dropbox/1. Unidad de Métodos y Acceso a Datos/1. Observatorio UMAD/PISO I_MICRODATOS/ECH/2020/Fusionada_personasyhogares_20.dta")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

### Generación de nuevas variables ###

# NNA

ech2020 <- ech2020 %>% dplyr::mutate(nna = ifelse(e27>= 0 & e27<=17, 1, 0))

proyeccion_ine <- 844974


# Captación AFAM


ech2020 <- ech2020 %>% dplyr::mutate(afam_pe = ifelse((g152 == 1 & f73 != 2 & f92 !=2 ) | (g150 == 1 & (pobpcoac %in% c(1, 3, 4, 6, 7, 8, 11) | (pobpcoac == 2 & f82 != 1 & f96 != 1))), 1, 0),   
                                               afam_cont = ifelse(g150 == 1 & afam_pe == 0, 1, 0),
                                               afam_total = ifelse(afam_pe == 1 | afam_cont == 1, 1, 0)) 

afam <- ech2020[,c("numero","afam_pe", "afam_cont", "afam_total")]
afam <- afam %>% dplyr::mutate(h_afam_pe = afam_pe,
                                         h_afam_cont = afam_cont,
                                         h_afam_total = afam_total)

afam_1 <- afam[order(afam$numero, afam$h_afam_pe, decreasing = TRUE), ]
afam_2 <- afam[order(afam$numero, afam$h_afam_cont, decreasing = TRUE), ]
afam_3 <- afam[order(afam$numero, afam$h_afam_total, decreasing = TRUE), ]

afam_1 <- afam_1 %>% distinct(numero, .keep_all = TRUE)
afam_2 <- afam_2 %>% distinct(numero, .keep_all = TRUE)
afam_3 <- afam_3 %>% distinct(numero, .keep_all = TRUE)

afam_1 <- afam_1[,c("numero","h_afam_pe")]
afam_2 <- afam_2[,c("numero","h_afam_cont")]
afam_3 <- afam_3[,c("numero","h_afam_total")]

ech2020 <- merge(ech2020, afam_1, by = "numero")
ech2020 <- merge(ech2020, afam_2, by = "numero")
ech2020 <- merge(ech2020, afam_3, by = "numero")



#  Captación TUS


ech2020 <- ech2020 %>% dplyr::mutate(tus = ifelse(e560==1, 1, 0))

tus <- ech2020[,c("numero","tus")]
tus <- tus %>% dplyr::mutate(h_tus = tus)
tus <- tus[order(tus$numero, tus$h_tus, decreasing = TRUE), ]
tus <- tus %>% distinct(numero, .keep_all = TRUE)
tus <- tus[,c("numero","h_tus")]
ech2020 <- merge(ech2020, tus, by = "numero")




# No percepción de transferencias

ech2020 <- ech2020 %>% dplyr::mutate(noafam_pe = ifelse(h_afam_pe == 0 , 1, 0))

ech2020 <- ech2020 %>% dplyr::mutate(notus = ifelse(h_tus == 0 , 1, 0))

ech2020 <- ech2020 %>% dplyr::mutate(notrans = ifelse(h_tus == 0 & h_afam_pe == 0, 1, 0))


# Jefe o cónyugue desempleado sin Seguro de Desempleo


ech2020 <- ech2020 %>% dplyr::mutate(jefenoseguro    = ifelse(e30 ==1 & pobpcoac == 4, 1, 0),
                               jefedesempleado = ifelse(e30 ==1 & (pobpcoac == 4 | pobpcoac == 5), 1, 0),
                               conynoseguro    = ifelse(e30 == 2 & pobpcoac == 4, 1, 0),
                               coyndesempleado = ifelse(e30 ==2 & (pobpcoac == 4 | pobpcoac == 5), 1, 0))


jefe <- ech2020[,c("numero","jefenoseguro", "jefedesempleado", "conynoseguro", "coyndesempleado")]
jefe <- jefe %>% dplyr::mutate(h_jefenoseguro = jefenoseguro,
                                         h_jefedesempleado = jefedesempleado,
                                         h_conynoseguro = conynoseguro,
                                         h_coyndesempleado = coyndesempleado)

jefe_1 <- jefe[order(jefe$numero, jefe$h_jefenoseguro, decreasing = TRUE), ]
jefe_2 <- jefe[order(jefe$numero, jefe$h_jefedesempleado, decreasing = TRUE), ]
jefe_3 <- jefe[order(jefe$numero, jefe$h_conynoseguro, decreasing = TRUE), ]
jefe_4 <- jefe[order(jefe$numero, jefe$h_coyndesempleado, decreasing = TRUE), ]


jefe_1 <- jefe_1 %>% distinct(numero, .keep_all = TRUE)
jefe_2 <- jefe_2 %>% distinct(numero, .keep_all = TRUE)
jefe_3 <- jefe_3 %>% distinct(numero, .keep_all = TRUE)
jefe_4 <- jefe_4 %>% distinct(numero, .keep_all = TRUE)

jefe_1 <- jefe_1[,c("numero","h_jefenoseguro")]
jefe_2 <- jefe_2[,c("numero","h_jefedesempleado")]
jefe_3 <- jefe_3[,c("numero","h_conynoseguro")]
jefe_4 <- jefe_4[,c("numero","h_coyndesempleado")]

ech2020 <- merge(ech2020, jefe_1, by = "numero")
ech2020 <- merge(ech2020, jefe_2, by = "numero")
ech2020 <- merge(ech2020, jefe_3, by = "numero")
ech2020 <- merge(ech2020, jefe_4, by = "numero")

ech2020 <- ech2020 %>% dplyr::mutate(h_noseguro = ifelse(h_jefenoseguro == 1 | h_conynoseguro == 1, 1, 0),
                                         h_desempleado = ifelse(h_jefedesempleado == 1 | h_coyndesempleado == 1, 1, 0))



# Jefe o cónyuge ocupado que no aporta a la seguridad social


ech2020 <- ech2020 %>% dplyr::mutate(jefenoaporta = ifelse(e30 == 1 & pobpcoac == 2 & f82 == 2, 1, 0),
                               jefeocupado = ifelse(e30 == 1 & pobpcoac == 2, 1, 0),
                               conynoaporta = ifelse(e30 == 2 & pobpcoac == 2 & f82 == 2, 1, 0),
                               conyocupado = ifelse(e30 == 2 & pobpcoac == 2, 1, 0))


jefe <- ech2020[,c("numero","jefenoaporta", "jefeocupado", "conynoaporta", "conyocupado")]
jefe <- jefe %>% dplyr::mutate(h_jefenoaporta = jefenoaporta,
                                         h_jefeocupado = jefeocupado,
                                         h_conynoaporta = conynoaporta,
                                         h_conyocupado = conyocupado)

jefe_1 <- jefe[order(jefe$numero, jefe$h_jefenoaporta, decreasing = TRUE), ]
jefe_2 <- jefe[order(jefe$numero, jefe$h_jefeocupado, decreasing = TRUE), ]
jefe_3 <- jefe[order(jefe$numero, jefe$h_conynoaporta, decreasing = TRUE), ]
jefe_4 <- jefe[order(jefe$numero, jefe$h_conyocupado, decreasing = TRUE), ]


jefe_1 <- jefe_1 %>% distinct(numero, .keep_all = TRUE)
jefe_2 <- jefe_2 %>% distinct(numero, .keep_all = TRUE)
jefe_3 <- jefe_3 %>% distinct(numero, .keep_all = TRUE)
jefe_4 <- jefe_4 %>% distinct(numero, .keep_all = TRUE)

jefe_1 <- jefe_1[,c("numero","h_jefenoaporta")]
jefe_2 <- jefe_2[,c("numero","h_jefeocupado")]
jefe_3 <- jefe_3[,c("numero","h_conynoaporta")]
jefe_4 <- jefe_4[,c("numero","h_conyocupado")]

ech2020 <- merge(ech2020, jefe_1, by = "numero")
ech2020 <- merge(ech2020, jefe_2, by = "numero")
ech2020 <- merge(ech2020, jefe_3, by = "numero")
ech2020 <- merge(ech2020, jefe_4, by = "numero")

ech2020 <- ech2020 %>% dplyr::mutate(h_noaporta = ifelse(h_jefenoaporta == 1 | h_conynoaporta == 1, 1, 0),
                               denominador = ifelse(h_jefeocupado == 1 | h_conyocupado == 1, 1, 0))






# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

### Información de muestreo ### 

ech2020_svy           <- srvyr::as_survey_design(ech2020, ids = numero, weights = pesomen)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
### Procesamiento de indicadores ###
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# NNA que residen en hogares con beneficiaros de AFAM contributivas
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

c_ano <- ech2020_svy %>%
  srvyr::filter(nna == 1) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(h_afam_cont))

c_ano <- mean(as.numeric(c_ano$colname))

c_ano <- c_ano*proyeccion_ine

  
CODIND       <- 330107
CODINDICADOR <- "NNA que residen en hogares con beneficiaros de AFAM contributivas"
NOMINDICADOR <- "Cantidad de NNA que residen en hogares con beneficiaros de AFAM contributivas"
VALOR	       <- c_ano
NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior."
nna_01 <- as.data.frame(cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, JERARQUIA_CAT, CORTE, NOTA_INDICADOR))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# NNA que residen en hogares con beneficiaros de AFAM-PE
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

c_ano <- ech2020_svy %>%
  srvyr::filter(nna == 1) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(h_afam_pe))

c_ano <- mean(as.numeric(c_ano$colname))

c_ano <- c_ano*proyeccion_ine



CODIND       <- 330108
CODINDICADOR <- "NNA que residen en hogares con beneficiaros de AFAM-PE"
NOMINDICADOR <- "Cantidad de NNA que residen en hogares con beneficiaros de AFAM-PE"
VALOR	       <- c_ano
NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior."
nna_02 <- as.data.frame(cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, JERARQUIA_CAT, CORTE, NOTA_INDICADOR))




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# NNA que residen en hogares con beneficiaros de TUS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

c_ano <- ech2020_svy %>%
  srvyr::filter(nna == 1) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(h_tus))

c_ano <- mean(as.numeric(c_ano$colname))

c_ano <- c_ano*proyeccion_ine



CODIND       <- 330109
CODINDICADOR <- "NNA que residen en hogares con beneficiaros de TUS"
NOMINDICADOR <- "Cantidad de NNA que residen en hogares con beneficiaros de TUS"
VALOR	       <- c_ano
NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior."
nna_03 <- as.data.frame(cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, JERARQUIA_CAT, CORTE, NOTA_INDICADOR))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# NNA en hogares pobres que no perciben AFAM-PE
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

c_ano <- ech2020_svy %>%
  srvyr::filter(nna == 1 & pobre_06 == 1) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(noafam_pe))

c_ano <- mean(as.numeric(c_ano$colname))



CODIND       <- 330110
CODINDICADOR <- "NNA en hogares pobres que no perciben AFAM-PE"
NOMINDICADOR <- "Porcentaje de NNA en hogares pobres que no perciben AFAM-PE"
VALOR	       <- c_ano
NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior."
nna_04 <- as.data.frame(cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, JERARQUIA_CAT, CORTE, NOTA_INDICADOR))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# NNA en hogares pobres que no perciben TUS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

c_ano <- ech2020_svy %>%
  srvyr::filter(nna == 1 & pobre_06 == 1) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(notus))

c_ano <- mean(as.numeric(c_ano$colname))


CODIND       <- 330111
CODINDICADOR <- "NNA en hogares pobres que no perciben TUS"
NOMINDICADOR <- "Porcentaje de NNA en hogares pobres que no perciben TUS"
VALOR	       <- c_ano
NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior."
nna_05 <- as.data.frame(cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, JERARQUIA_CAT, CORTE, NOTA_INDICADOR))



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# NNA en hogares pobres que no perciben AFAM-PE ni TUS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

c_ano <- ech2020_svy %>%
  srvyr::filter(nna == 1 & pobre_06 == 1) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(notrans))

c_ano <- mean(as.numeric(c_ano$colname))


CODIND       <- 330112
CODINDICADOR <- "NNA en hogares pobres que no perciben AFAM-PE ni TUS"
NOMINDICADOR <- "Porcentaje de NNA en hogares pobres que no perciben AFAM-PE ni TUS"
VALOR	       <- c_ano
NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior."
nna_06 <- as.data.frame(cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, JERARQUIA_CAT, CORTE, NOTA_INDICADOR))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# NNA en hogares con jefe/a y/o cónyuge desempleado sin seguro de desempleo
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

c_ano <- ech2020_svy %>%
  srvyr::filter(nna == 1 & h_desempleado == 1) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(h_noseguro))

c_ano <- mean(as.numeric(c_ano$colname))


CODIND       <- 330113
CODINDICADOR <- "NNA en hogares con jefe/a y/o cónyuge desempleado sin seguro de desempleo"
NOMINDICADOR <- "Porcentaje de NNA en hogares con jefe/a y/o cónyuge desempleado sin seguro de desempleo"
VALOR	       <- c_ano
NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior."
nna_07 <- as.data.frame(cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, JERARQUIA_CAT, CORTE, NOTA_INDICADOR))




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# NNA en hogares con jefe/a y/o cónyuge ocupados sin aporte a la seguridad social
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


c_ano <- ech2020_svy %>%
  srvyr::filter(nna == 1 & denominador == 1) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(h_noaporta))

c_ano <- mean(as.numeric(c_ano$colname))


CODIND       <- 330114
CODINDICADOR <- "NNA en hogares con jefe/a y/o cónyuge ocupados sin aporte a la seguridad social"
NOMINDICADOR <- "Porcentaje de NNA en hogares con jefe/a y/o cónyuge ocupados sin aporte a la seguridad social"
VALOR	       <- c_ano
NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior."
nna_08 <- as.data.frame(cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, JERARQUIA_CAT, CORTE, NOTA_INDICADOR))



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
### Generación de Base motor ###
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


nna_2020 <- rbind(nna_01, nna_02, nna_03, nna_04, nna_05, nna_06, nna_07, nna_08)

rio::export(nna_2020, "nna_2020.xlsx" )


