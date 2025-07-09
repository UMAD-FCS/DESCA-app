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
AÑO	         <- 2021
FUENTE	     <- "Encuesta Continua de Hogares 2021, INE"
RESPONSABLE	 <- "J. Pandolfi"
JERARQUIA	   <- 1
JERARQUIA_CAT<- 1
CORTE        <- "Total"


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

# NNA

sem1 <- sem1 %>% dplyr::mutate(nna = ifelse(E27>= 0 & E27<=17, 1, 0))
sem2_implant <- sem2_implant %>% dplyr::mutate(nna = ifelse(e27>= 0 & e27<=17, 1, 0))
sem2_panel <- sem2_panel %>% dplyr::mutate(nna = ifelse(e27>= 0 & e27<=17, 1, 0))

proyeccion_ine <- 838689


# Captación AFAM

sem1 <- sem1 %>% dplyr::mutate(afam_pe = ifelse((G152 == 1 & F73 != 2 & F92 !=2 ) | (G150 == 1 & (POBPCOAC %in% c(1, 3, 4, 6, 7, 8, 11) | (POBPCOAC == 2 & F82 != 1 & F96 != 1))), 1, 0),   
                               afam_cont = ifelse(G150 == 1 & afam_pe == 0, 1, 0),
                               afam_total = ifelse(afam_pe == 1 | afam_cont == 1, 1, 0)) 

sem1_afam <- sem1[,c("numero","afam_pe", "afam_cont", "afam_total")]
sem1_afam <- sem1_afam %>% dplyr::mutate(h_afam_pe = afam_pe,
                                         h_afam_cont = afam_cont,
                                         h_afam_total = afam_total)

sem1_afam_1 <- sem1_afam[order(sem1_afam$numero, sem1_afam$h_afam_pe, decreasing = TRUE), ]
sem1_afam_2 <- sem1_afam[order(sem1_afam$numero, sem1_afam$h_afam_cont, decreasing = TRUE), ]
sem1_afam_3 <- sem1_afam[order(sem1_afam$numero, sem1_afam$h_afam_total, decreasing = TRUE), ]

sem1_afam_1 <- sem1_afam_1 %>% distinct(numero, .keep_all = TRUE)
sem1_afam_2 <- sem1_afam_2 %>% distinct(numero, .keep_all = TRUE)
sem1_afam_3 <- sem1_afam_3 %>% distinct(numero, .keep_all = TRUE)

sem1_afam_1 <- sem1_afam_1[,c("numero","h_afam_pe")]
sem1_afam_2 <- sem1_afam_2[,c("numero","h_afam_cont")]
sem1_afam_3 <- sem1_afam_3[,c("numero","h_afam_total")]

sem1 <- merge(sem1, sem1_afam_1, by = "numero")
sem1 <- merge(sem1, sem1_afam_2, by = "numero")
sem1 <- merge(sem1, sem1_afam_3, by = "numero")


sem2_implant <- sem2_implant %>% dplyr::mutate(afam_pe = ifelse((g152 == 1 & f73 != 2 & f92 !=2 ) | (g150 == 1 & (pobpcoac %in% c(1, 3, 4, 6, 7, 8, 11) | (pobpcoac == 2 & f82 != 1 & f96 != 1))), 1, 0),   
                                               afam_cont = ifelse(g150 == 1 & afam_pe == 0, 1, 0),
                                               afam_total = ifelse(afam_pe == 1 | afam_cont == 1, 1, 0)) 

sem2_afam <- sem2_implant[,c("ID","afam_pe", "afam_cont", "afam_total")]
sem2_afam <- sem2_afam %>% dplyr::mutate(h_afam_pe = afam_pe,
                                         h_afam_cont = afam_cont,
                                         h_afam_total = afam_total)

sem2_afam_1 <- sem2_afam[order(sem2_afam$ID, sem2_afam$h_afam_pe, decreasing = TRUE), ]
sem2_afam_2 <- sem2_afam[order(sem2_afam$ID, sem2_afam$h_afam_cont, decreasing = TRUE), ]
sem2_afam_3 <- sem2_afam[order(sem2_afam$ID, sem2_afam$h_afam_total, decreasing = TRUE), ]

sem2_afam_1 <- sem2_afam_1 %>% distinct(ID, .keep_all = TRUE)
sem2_afam_2 <- sem2_afam_2 %>% distinct(ID, .keep_all = TRUE)
sem2_afam_3 <- sem2_afam_3 %>% distinct(ID, .keep_all = TRUE)

sem2_afam_1 <- sem2_afam_1[,c("ID","h_afam_pe")]
sem2_afam_2 <- sem2_afam_2[,c("ID","h_afam_cont")]
sem2_afam_3 <- sem2_afam_3[,c("ID","h_afam_total")]

sem2_implant <- merge(sem2_implant, sem2_afam_1, by = "ID")
sem2_implant <- merge(sem2_implant, sem2_afam_2, by = "ID")
sem2_implant <- merge(sem2_implant, sem2_afam_3, by = "ID")



#  Captación TUS

sem1 <- sem1 %>% dplyr::mutate(tus = ifelse(E560==1, 1, 0))

sem1_tus <- sem1[,c("numero","tus")]
sem1_tus <- sem1_tus %>% dplyr::mutate(h_tus = tus)
sem1_tus <- sem1_tus[order(sem1_tus$numero, sem1_tus$h_tus, decreasing = TRUE), ]
sem1_tus <- sem1_tus %>% distinct(numero, .keep_all = TRUE)
sem1_tus <- sem1_tus[,c("numero","h_tus")]
sem1 <- merge(sem1, sem1_tus, by = "numero")


sem2_implant <- sem2_implant %>% dplyr::mutate(tus = ifelse(e560==1, 1, 0))

sem2_tus <- sem2_implant[,c("ID","tus")]
sem2_tus <- sem2_tus %>% dplyr::mutate(h_tus = tus)
sem2_tus <- sem2_tus[order(sem2_tus$ID, sem2_tus$h_tus, decreasing = TRUE), ]
sem2_tus <- sem2_tus %>% distinct(ID, .keep_all = TRUE)
sem2_tus <- sem2_tus[,c("ID","h_tus")]
sem2_implant <- merge(sem2_implant, sem2_tus, by = "ID")




# No percepción de transferencias

sem1 <- sem1 %>% dplyr::mutate(noafam_pe = ifelse(h_afam_pe == 0 , 1, 0))
sem2_implant <- sem2_implant %>% dplyr::mutate(noafam_pe = ifelse(h_afam_pe == 0 , 1, 0))

sem1 <- sem1 %>% dplyr::mutate(notus = ifelse(h_tus == 0 , 1, 0))
sem2_implant <- sem2_implant %>% dplyr::mutate(notus = ifelse(h_tus == 0 , 1, 0))

sem1 <- sem1 %>% dplyr::mutate(notrans = ifelse(h_tus == 0 & h_afam_pe == 0, 1, 0))
sem2_implant <- sem2_implant %>% dplyr::mutate(notrans = ifelse(h_tus == 0 & h_afam_pe == 0, 1, 0))


# Jefe o cónyugue desempleado sin Seguro de Desempleo

sem1 <- sem1 %>% dplyr::mutate(jefenoseguro    = ifelse(e30 ==1 & POBPCOAC == 4, 1, 0),
                               jefedesempleado = ifelse(e30 ==1 & (POBPCOAC == 4 | POBPCOAC == 5), 1, 0),
                               conynoseguro    = ifelse(e30 == 2 & POBPCOAC == 4, 1, 0),
                               coyndesempleado = ifelse(e30 ==2 & (POBPCOAC == 4 | POBPCOAC == 5), 1, 0))


sem1_jefe <- sem1[,c("numero","jefenoseguro", "jefedesempleado", "conynoseguro", "coyndesempleado")]
sem1_jefe <- sem1_jefe %>% dplyr::mutate(h_jefenoseguro = jefenoseguro,
                                         h_jefedesempleado = jefedesempleado,
                                         h_conynoseguro = conynoseguro,
                                         h_coyndesempleado = coyndesempleado)

sem1_jefe_1 <- sem1_jefe[order(sem1_jefe$numero, sem1_jefe$h_jefenoseguro, decreasing = TRUE), ]
sem1_jefe_2 <- sem1_jefe[order(sem1_jefe$numero, sem1_jefe$h_jefedesempleado, decreasing = TRUE), ]
sem1_jefe_3 <- sem1_jefe[order(sem1_jefe$numero, sem1_jefe$h_conynoseguro, decreasing = TRUE), ]
sem1_jefe_4 <- sem1_jefe[order(sem1_jefe$numero, sem1_jefe$h_coyndesempleado, decreasing = TRUE), ]


sem1_jefe_1 <- sem1_jefe_1 %>% distinct(numero, .keep_all = TRUE)
sem1_jefe_2 <- sem1_jefe_2 %>% distinct(numero, .keep_all = TRUE)
sem1_jefe_3 <- sem1_jefe_3 %>% distinct(numero, .keep_all = TRUE)
sem1_jefe_4 <- sem1_jefe_4 %>% distinct(numero, .keep_all = TRUE)

sem1_jefe_1 <- sem1_jefe_1[,c("numero","h_jefenoseguro")]
sem1_jefe_2 <- sem1_jefe_2[,c("numero","h_jefedesempleado")]
sem1_jefe_3 <- sem1_jefe_3[,c("numero","h_conynoseguro")]
sem1_jefe_4 <- sem1_jefe_4[,c("numero","h_coyndesempleado")]


sem1 <- merge(sem1, sem1_jefe_1, by = "numero")
sem1 <- merge(sem1, sem1_jefe_2, by = "numero")
sem1 <- merge(sem1, sem1_jefe_3, by = "numero")
sem1 <- merge(sem1, sem1_jefe_4, by = "numero")

sem1 <- sem1 %>% dplyr::mutate(h_noseguro = ifelse(h_jefenoseguro == 1 | h_conynoseguro == 1, 1, 0),
                                         h_desempleado = ifelse(h_jefedesempleado == 1 | h_coyndesempleado == 1, 1, 0))




sem2_implant <- sem2_implant %>% dplyr::mutate(jefenoseguro    = ifelse(e30 ==1 & pobpcoac == 4, 1, 0),
                               jefedesempleado = ifelse(e30 ==1 & (pobpcoac == 4 | pobpcoac == 5), 1, 0),
                               conynoseguro    = ifelse(e30 == 2 & pobpcoac == 4, 1, 0),
                               coyndesempleado = ifelse(e30 ==2 & (pobpcoac == 4 | pobpcoac == 5), 1, 0))


sem2_jefe <- sem2_implant[,c("ID","jefenoseguro", "jefedesempleado", "conynoseguro", "coyndesempleado")]
sem2_jefe <- sem2_jefe %>% dplyr::mutate(h_jefenoseguro = jefenoseguro,
                                         h_jefedesempleado = jefedesempleado,
                                         h_conynoseguro = conynoseguro,
                                         h_coyndesempleado = coyndesempleado)

sem2_jefe_1 <- sem2_jefe[order(sem2_jefe$ID, sem2_jefe$h_jefenoseguro, decreasing = TRUE), ]
sem2_jefe_2 <- sem2_jefe[order(sem2_jefe$ID, sem2_jefe$h_jefedesempleado, decreasing = TRUE), ]
sem2_jefe_3 <- sem2_jefe[order(sem2_jefe$ID, sem2_jefe$h_conynoseguro, decreasing = TRUE), ]
sem2_jefe_4 <- sem2_jefe[order(sem2_jefe$ID, sem2_jefe$h_coyndesempleado, decreasing = TRUE), ]


sem2_jefe_1 <- sem2_jefe_1 %>% distinct(ID, .keep_all = TRUE)
sem2_jefe_2 <- sem2_jefe_2 %>% distinct(ID, .keep_all = TRUE)
sem2_jefe_3 <- sem2_jefe_3 %>% distinct(ID, .keep_all = TRUE)
sem2_jefe_4 <- sem2_jefe_4 %>% distinct(ID, .keep_all = TRUE)

sem2_jefe_1 <- sem2_jefe_1[,c("ID","h_jefenoseguro")]
sem2_jefe_2 <- sem2_jefe_2[,c("ID","h_jefedesempleado")]
sem2_jefe_3 <- sem2_jefe_3[,c("ID","h_conynoseguro")]
sem2_jefe_4 <- sem2_jefe_4[,c("ID","h_coyndesempleado")]

sem2_implant <- merge(sem2_implant, sem2_jefe_1, by = "ID")
sem2_implant <- merge(sem2_implant, sem2_jefe_2, by = "ID")
sem2_implant <- merge(sem2_implant, sem2_jefe_3, by = "ID")
sem2_implant <- merge(sem2_implant, sem2_jefe_4, by = "ID")

sem2_implant <- sem2_implant %>% dplyr::mutate(h_noseguro = ifelse(h_jefenoseguro == 1 | h_conynoseguro == 1, 1, 0),
                                         h_desempleado = ifelse(h_jefedesempleado == 1 | h_coyndesempleado == 1, 1, 0))



# Jefe o cónyuge ocupado que no aporta a la seguridad social

sem1 <- sem1 %>% dplyr::mutate(jefenoaporta = ifelse(e30 == 1 & POBPCOAC == 2 & F82 == 2, 1, 0),
                               jefeocupado = ifelse(e30 == 1 & POBPCOAC == 2, 1, 0),
                               conynoaporta = ifelse(e30 == 2 & POBPCOAC == 2 & F82 == 2, 1, 0),
                               conyocupado = ifelse(e30 == 2 & POBPCOAC == 2, 1, 0))



sem1_jefe <- sem1[,c("numero","jefenoaporta", "jefeocupado", "conynoaporta", "conyocupado")]
sem1_jefe <- sem1_jefe %>% dplyr::mutate(h_jefenoaporta = jefenoaporta,
                                         h_jefeocupado = jefeocupado,
                                         h_conynoaporta = conynoaporta,
                                         h_conyocupado = conyocupado)

sem1_jefe_1 <- sem1_jefe[order(sem1_jefe$numero, sem1_jefe$h_jefenoaporta, decreasing = TRUE), ]
sem1_jefe_2 <- sem1_jefe[order(sem1_jefe$numero, sem1_jefe$h_jefeocupado, decreasing = TRUE), ]
sem1_jefe_3 <- sem1_jefe[order(sem1_jefe$numero, sem1_jefe$h_conynoaporta, decreasing = TRUE), ]
sem1_jefe_4 <- sem1_jefe[order(sem1_jefe$numero, sem1_jefe$h_conyocupado, decreasing = TRUE), ]


sem1_jefe_1 <- sem1_jefe_1 %>% distinct(numero, .keep_all = TRUE)
sem1_jefe_2 <- sem1_jefe_2 %>% distinct(numero, .keep_all = TRUE)
sem1_jefe_3 <- sem1_jefe_3 %>% distinct(numero, .keep_all = TRUE)
sem1_jefe_4 <- sem1_jefe_4 %>% distinct(numero, .keep_all = TRUE)

sem1_jefe_1 <- sem1_jefe_1[,c("numero","h_jefenoaporta")]
sem1_jefe_2 <- sem1_jefe_2[,c("numero","h_jefeocupado")]
sem1_jefe_3 <- sem1_jefe_3[,c("numero","h_conynoaporta")]
sem1_jefe_4 <- sem1_jefe_4[,c("numero","h_conyocupado")]


sem1 <- merge(sem1, sem1_jefe_1, by = "numero")
sem1 <- merge(sem1, sem1_jefe_2, by = "numero")
sem1 <- merge(sem1, sem1_jefe_3, by = "numero")
sem1 <- merge(sem1, sem1_jefe_4, by = "numero")

sem1 <- sem1 %>% dplyr::mutate(h_noaporta = ifelse(h_jefenoaporta == 1 | h_conynoaporta == 1, 1, 0),
                               denominador = ifelse(h_jefeocupado == 1 | h_conyocupado == 1, 1, 0))


sem2_implant <- sem2_implant %>% dplyr::mutate(jefenoaporta = ifelse(e30 == 1 & pobpcoac == 2 & f82 == 2, 1, 0),
                               jefeocupado = ifelse(e30 == 1 & pobpcoac == 2, 1, 0),
                               conynoaporta = ifelse(e30 == 2 & pobpcoac == 2 & f82 == 2, 1, 0),
                               conyocupado = ifelse(e30 == 2 & pobpcoac == 2, 1, 0))


sem2_jefe <- sem2_implant[,c("ID","jefenoaporta", "jefeocupado", "conynoaporta", "conyocupado")]
sem2_jefe <- sem2_jefe %>% dplyr::mutate(h_jefenoaporta = jefenoaporta,
                                         h_jefeocupado = jefeocupado,
                                         h_conynoaporta = conynoaporta,
                                         h_conyocupado = conyocupado)

sem2_jefe_1 <- sem2_jefe[order(sem2_jefe$ID, sem2_jefe$h_jefenoaporta, decreasing = TRUE), ]
sem2_jefe_2 <- sem2_jefe[order(sem2_jefe$ID, sem2_jefe$h_jefeocupado, decreasing = TRUE), ]
sem2_jefe_3 <- sem2_jefe[order(sem2_jefe$ID, sem2_jefe$h_conynoaporta, decreasing = TRUE), ]
sem2_jefe_4 <- sem2_jefe[order(sem2_jefe$ID, sem2_jefe$h_conyocupado, decreasing = TRUE), ]


sem2_jefe_1 <- sem2_jefe_1 %>% distinct(ID, .keep_all = TRUE)
sem2_jefe_2 <- sem2_jefe_2 %>% distinct(ID, .keep_all = TRUE)
sem2_jefe_3 <- sem2_jefe_3 %>% distinct(ID, .keep_all = TRUE)
sem2_jefe_4 <- sem2_jefe_4 %>% distinct(ID, .keep_all = TRUE)

sem2_jefe_1 <- sem2_jefe_1[,c("ID","h_jefenoaporta")]
sem2_jefe_2 <- sem2_jefe_2[,c("ID","h_jefeocupado")]
sem2_jefe_3 <- sem2_jefe_3[,c("ID","h_conynoaporta")]
sem2_jefe_4 <- sem2_jefe_4[,c("ID","h_conyocupado")]

sem2_implant <- merge(sem2_implant, sem2_jefe_1, by = "ID")
sem2_implant <- merge(sem2_implant, sem2_jefe_2, by = "ID")
sem2_implant <- merge(sem2_implant, sem2_jefe_3, by = "ID")
sem2_implant <- merge(sem2_implant, sem2_jefe_4, by = "ID")

sem2_implant <- sem2_implant %>% dplyr::mutate(h_noaporta = ifelse(h_jefenoaporta == 1 | h_conynoaporta == 1, 1, 0),
                               denominador = ifelse(h_jefeocupado == 1 | h_conyocupado == 1, 1, 0))






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
# NNA que residen en hogares con beneficiaros de AFAM contributivas
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

a_mes <- sem1_svy %>%
  srvyr::filter(nna == 1) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(h_afam_cont))

a_sem <- mean(as.numeric(a_mes$colname))

b_sem <- sem2_implant_svy %>%
  srvyr::filter(nna == 1) %>%
  srvyr::summarise(colname = srvyr::survey_mean(h_afam_cont))

b_sem <-b_sem$colname

c_ano <- mean(c(a_sem, b_sem))

c_ano <- c_ano*proyeccion_ine
  
  
  
CODIND       <- 330107
CODINDICADOR <- "NNA que residen en hogares con beneficiaros de AFAM contributivas"
NOMINDICADOR <- "Cantidad de NNA que residen en hogares con beneficiaros de AFAM contributivas"
VALOR	       <- c_ano
NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. Este indicador se calcula a partir de la encuesta telefónica del primer semestre de 2021 y el formulario de implantación de modalidad panel del segundo semestre de 2021."
nna_01 <- as.data.frame(cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, JERARQUIA_CAT, CORTE, NOTA_INDICADOR))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# NNA que residen en hogares con beneficiaros de AFAM-PE
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

a_mes <- sem1_svy %>%
  srvyr::filter(nna == 1) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(h_afam_pe))

a_sem <- mean(as.numeric(a_mes$colname))

b_sem <- sem2_implant_svy %>%
  srvyr::filter(nna == 1) %>%
  srvyr::summarise(colname = srvyr::survey_mean(h_afam_pe))

b_sem <-b_sem$colname

c_ano <- mean(c(a_sem, b_sem))

c_ano <- c_ano*proyeccion_ine



CODIND       <- 330108
CODINDICADOR <- "NNA que residen en hogares con beneficiaros de AFAM-PE"
NOMINDICADOR <- "Cantidad de NNA que residen en hogares con beneficiaros de AFAM-PE"
VALOR	       <- c_ano
NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. Este indicador se calcula a partir de la encuesta telefónica del primer semestre de 2021 y el formulario de implantación de modalidad panel del segundo semestre de 2021."
nna_02 <- as.data.frame(cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, JERARQUIA_CAT, CORTE, NOTA_INDICADOR))




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# NNA que residen en hogares con beneficiaros de TUS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

a_mes <- sem1_svy %>%
  srvyr::filter(nna == 1) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(h_tus))

a_sem <- mean(as.numeric(a_mes$colname))

b_sem <- sem2_implant_svy %>%
  srvyr::filter(nna == 1) %>%
  srvyr::summarise(colname = srvyr::survey_mean(h_tus))

b_sem <-b_sem$colname

c_ano <- mean(c(a_sem, b_sem))

c_ano <- c_ano*proyeccion_ine



CODIND       <- 330109
CODINDICADOR <- "NNA que residen en hogares con beneficiaros de TUS"
NOMINDICADOR <- "Cantidad de NNA que residen en hogares con beneficiaros de TUS"
VALOR	       <- c_ano
NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. Este indicador se calcula a partir de la encuesta telefónica del primer semestre de 2021 y el formulario de implantación de modalidad panel del segundo semestre de 2021."
nna_03 <- as.data.frame(cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, JERARQUIA_CAT, CORTE, NOTA_INDICADOR))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# NNA en hogares pobres que no perciben AFAM-PE
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

a_mes <- sem1_svy %>%
  srvyr::filter(nna == 1 & pobre06 == 1) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(noafam_pe))

a_sem <- mean(as.numeric(a_mes$colname))

b_sem <- sem2_implant_svy %>%
  srvyr::filter(nna == 1 & pobre == 1) %>%
  srvyr::summarise(colname = srvyr::survey_mean(noafam_pe))

b_sem <-b_sem$colname

c_ano <- mean(c(a_sem, b_sem))


CODIND       <- 330110
CODINDICADOR <- "NNA en hogares pobres que no perciben AFAM-PE"
NOMINDICADOR <- "Porcentaje de NNA en hogares pobres que no perciben AFAM-PE"
VALOR	       <- c_ano
NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. Este indicador se calcula a partir de la encuesta telefónica del primer semestre de 2021 y el formulario de implantación de modalidad panel del segundo semestre de 2021."
nna_04 <- as.data.frame(cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, JERARQUIA_CAT, CORTE, NOTA_INDICADOR))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# NNA en hogares pobres que no perciben TUS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

a_mes <- sem1_svy %>%
  srvyr::filter(nna == 1 & pobre06 == 1) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(notus))

a_sem <- mean(as.numeric(a_mes$colname))

b_sem <- sem2_implant_svy %>%
  srvyr::filter(nna == 1 & pobre == 1) %>%
  srvyr::summarise(colname = srvyr::survey_mean(notus))

b_sem <-b_sem$colname

c_ano <- mean(c(a_sem, b_sem))


CODIND       <- 330111
CODINDICADOR <- "NNA en hogares pobres que no perciben TUS"
NOMINDICADOR <- "Porcentaje de NNA en hogares pobres que no perciben TUS"
VALOR	       <- c_ano
NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. Este indicador se calcula a partir de la encuesta telefónica del primer semestre de 2021 y el formulario de implantación de modalidad panel del segundo semestre de 2021."
nna_05 <- as.data.frame(cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, JERARQUIA_CAT, CORTE, NOTA_INDICADOR))



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# NNA en hogares pobres que no perciben AFAM-PE ni TUS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

a_mes <- sem1_svy %>%
  srvyr::filter(nna == 1 & pobre06 == 1) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(notrans))

a_sem <- mean(as.numeric(a_mes$colname))

b_sem <- sem2_implant_svy %>%
  srvyr::filter(nna == 1 & pobre == 1) %>%
  srvyr::summarise(colname = srvyr::survey_mean(notrans))

b_sem <-b_sem$colname

c_ano <- mean(c(a_sem, b_sem))


CODIND       <- 330112
CODINDICADOR <- "NNA en hogares pobres que no perciben AFAM-PE ni TUS"
NOMINDICADOR <- "Porcentaje de NNA en hogares pobres que no perciben AFAM-PE ni TUS"
VALOR	       <- c_ano
NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. Este indicador se calcula a partir de la encuesta telefónica del primer semestre de 2021 y el formulario de implantación de modalidad panel del segundo semestre de 2021."
nna_06 <- as.data.frame(cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, JERARQUIA_CAT, CORTE, NOTA_INDICADOR))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# NNA en hogares con jefe/a y/o cónyuge desempleado sin seguro de desempleo
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

a_mes <- sem1_svy %>%
  srvyr::filter(nna == 1 & h_desempleado == 1) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(h_noseguro))

a_sem <- mean(as.numeric(a_mes$colname))

b_sem <- sem2_implant_svy %>%
  srvyr::filter(nna == 1 & h_desempleado == 1) %>%
  srvyr::summarise(colname = srvyr::survey_mean(h_noseguro))

b_sem <-b_sem$colname

c_ano <- mean(c(a_sem, b_sem))


CODIND       <- 330113
CODINDICADOR <- "NNA en hogares con jefe/a y/o cónyuge desempleado sin seguro de desempleo"
NOMINDICADOR <- "Porcentaje de NNA en hogares con jefe/a y/o cónyuge desempleado sin seguro de desempleo"
VALOR	       <- c_ano
NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. Este indicador se calcula a partir de la encuesta telefónica del primer semestre de 2021 y el formulario de implantación de modalidad panel del segundo semestre de 2021."
nna_07 <- as.data.frame(cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, JERARQUIA_CAT, CORTE, NOTA_INDICADOR))




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# NNA en hogares con jefe/a y/o cónyuge ocupados sin aporte a la seguridad social
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


a_mes <- sem1_svy %>%
  srvyr::filter(nna == 1 & denominador == 1) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(h_noaporta))

a_sem <- mean(as.numeric(a_mes$colname))

b_sem <- sem2_implant_svy %>%
  srvyr::filter(nna == 1 & denominador == 1) %>%
  srvyr::summarise(colname = srvyr::survey_mean(h_noaporta))

b_sem <-b_sem$colname

c_ano <- mean(c(a_sem, b_sem))


CODIND       <- 330114
CODINDICADOR <- "NNA en hogares con jefe/a y/o cónyuge ocupados sin aporte a la seguridad social"
NOMINDICADOR <- "Porcentaje de NNA en hogares con jefe/a y/o cónyuge ocupados sin aporte a la seguridad social"
VALOR	       <- c_ano
NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. Este indicador se calcula a partir de la encuesta telefónica del primer semestre de 2021 y el formulario de implantación de modalidad panel del segundo semestre de 2021."
nna_08 <- as.data.frame(cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, JERARQUIA_CAT, CORTE, NOTA_INDICADOR))



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
### Generación de Base motor ###
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


nna_2021 <- rbind(nna_01, nna_02, nna_03, nna_04, nna_05, nna_06, nna_07, nna_08)

rio::export(nna_2021, "nna_2021.xlsx" )


