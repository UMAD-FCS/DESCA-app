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

# Salario mínimo nacional enero 2022

smn <- 19364

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


# Sexo
sem1 <- sem1 %>% dplyr::mutate(bc_pe2 = E26)
sem2_implant <- sem2_implant %>% dplyr::mutate(bc_pe2 = e26)

# Ascendencia afro
sem1 <- sem1 %>% dplyr::mutate(bd_e29_1 = e29_1)
sem2_implant <- sem2_implant %>% dplyr::mutate(bd_e29_1 = e29_1)



# Desempleados sin Seguro de Desempleo

sem1 <- sem1 %>% dplyr::mutate(seguroparo = ifelse(POBPCOAC == 4, 1, 0))
sem2_panel <- sem2_panel %>% dplyr::mutate(seguroparo = ifelse(POBPCOAC == 4, 1, 0))

# Percepción de jubilaciones

sem1 <- sem1 %>% dplyr::mutate(jub = ifelse(G148_1_1 > 0 |
                                              G148_1_2 > 0 |
                                              G148_1_3 > 0 |
                                              G148_1_4 > 0 |
                                              G148_1_5 > 0 |
                                              G148_1_6 > 0 |
                                              G148_1_7 > 0 |
                                              G148_1_8 > 0 |
                                              G148_1_9 > 0 |
                                              G148_1_10 > 0 |
                                              G148_1_11 > 0, 1, 0))

sem2_implant <- sem2_implant %>% dplyr::mutate(jub = ifelse(g148_1_1 > 0 |
                                              g148_1_2 > 0 |
                                              g148_1_3 > 0 |
                                              g148_1_5 > 0 |
                                              g148_1_6 > 0 |
                                              g148_1_7 > 0 |
                                              g148_1_8 > 0 |
                                              g148_1_9 > 0 |
                                              g148_1_10 > 0 |
                                              g148_1_11 > 0, 1, 0)) # No está la variable g148_1_4 "UNIÓN POSTAL"


# Percepción de pensiones

sem1 <- sem1 %>% dplyr::mutate(pens = ifelse(G148_2_1 > 0 |
                                              G148_2_2 > 0 |
                                              G148_2_3 > 0 |
                                              G148_2_4 > 0 |
                                              G148_2_5 > 0 |
                                              G148_2_6 > 0 |
                                              G148_2_7 > 0 |
                                              G148_2_8 > 0 |
                                              G148_2_9 > 0 |
                                              G148_2_10 > 0 |
                                              G148_2_11 > 0, 1, 0))

sem2_implant <- sem2_implant %>% dplyr::mutate(pens = ifelse(g148_2_1 > 0 |
                                                              g148_2_2 > 0 |
                                                              g148_2_3 > 0 |
                                                              g148_2_5 > 0 |
                                                              g148_2_6 > 0 |
                                                              g148_2_7 > 0 |
                                                              g148_2_8 > 0 |
                                                              g148_2_9 > 0 |
                                                              g148_2_10 > 0 |
                                                              g148_2_11 > 0, 1, 0)) # No está la variable g148_2_4 "UNIÓN POSTAL"

# Percepción de jubilaciones o pensiones

sem1 <- sem1 %>% dplyr::mutate(nijubpens = ifelse(jub == 1 | pens == 1, 0, 1))
sem2_implant <- sem2_implant %>% dplyr::mutate(nijubpens = ifelse(jub == 1 | pens == 1, 0, 1))


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


# No percepción de transferencias

sem1 <- sem1 %>% dplyr::mutate(p_afam = ifelse(G150 == 1, 1, 0))
sem1_afam <- sem1[,c("numero","p_afam")]
sem1_afam <- sem1_afam %>% dplyr::mutate(h_afam = p_afam)
sem1_afam <- sem1_afam[order(sem1_afam$numero, sem1_afam$h_afam, decreasing = TRUE), ]
sem1_afam <- sem1_afam %>% distinct(numero, .keep_all = TRUE)
sem1_afam <- sem1_afam[,c("numero","h_afam")]
sem1 <- merge(sem1, sem1_afam, by = "numero")

sem1 <- sem1 %>% dplyr::mutate(p_tus = ifelse(E560 == 1, 1, 0))
sem1_tus <- sem1[,c("numero","p_tus")]
sem1_tus <- sem1_tus %>% dplyr::mutate(h_tus = p_tus)
sem1_tus <- sem1_tus[order(sem1_tus$numero, sem1_tus$h_tus, decreasing = TRUE), ]
sem1_tus <- sem1_tus %>% distinct(numero, .keep_all = TRUE)
sem1_tus <- sem1_tus[,c("numero","h_tus")]
sem1 <- merge(sem1, sem1_tus, by = "numero")

sem1 <- sem1 %>% dplyr::mutate(nopercibe = ifelse(h_afam == 0 & h_tus == 0, 1, 0))

sem2_implant <- sem2_implant %>% dplyr::mutate(p_afam = ifelse(g150 == 1, 1, 0))
sem2_implant_afam <- sem2_implant[,c("ID","p_afam")]
sem2_implant_afam <- sem2_implant_afam %>% dplyr::mutate(h_afam = p_afam)
sem2_implant_afam <- sem2_implant_afam[order(sem2_implant_afam$ID, sem2_implant_afam$h_afam, decreasing = TRUE), ]
sem2_implant_afam <- sem2_implant_afam %>% distinct(ID, .keep_all = TRUE)
sem2_implant_afam <- sem2_implant_afam[,c("ID","h_afam")]
sem2_implant <- merge(sem2_implant, sem2_implant_afam, by = "ID")

sem2_implant <- sem2_implant %>% dplyr::mutate(p_tus = ifelse(e560 == 1, 1, 0))

sem2_implant_tus <- sem2_implant[,c("ID","p_tus")]
sem2_implant_tus <- sem2_implant_tus %>% dplyr::mutate(h_tus = p_tus)
sem2_implant_tus <- sem2_implant_tus[order(sem2_implant_tus$ID, sem2_implant_tus$h_tus, decreasing = TRUE), ]
sem2_implant_tus <- sem2_implant_tus %>% distinct(ID, .keep_all = TRUE)
sem2_implant_tus <- sem2_implant_tus[,c("ID","h_tus")]

sem2_implant <- merge(sem2_implant, sem2_implant_tus, by = "ID")


sem2_implant <- sem2_implant %>% dplyr::mutate(nopercibe = ifelse(h_afam == 0 & h_tus == 0, 1, 0))


# Desempleados de 1 a 6 meses

sem1 <- sem1 %>% dplyr::mutate(desemp1a6 = ifelse(F113 >= 4 & F113<=24, 1, 0))
sem2_implant <- sem2_implant %>% dplyr::mutate(desemp1a6 = ifelse(f113 >= 4 & f113<=24, 1, 0))


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




# Ingresos desempleo por debajo de línea de pobreza

sem1 <- sem1 %>% dplyr::mutate(desempinsuf = ifelse(G148_3 < lp_unipersonales, 1, 0))
sem2_implant <- sem2_implant %>% dplyr::mutate(desempinsuf = ifelse(g148_3 < lp_unipersonales, 1, 0))


# Ingresos desempleo por debajo de la mitad de la media y mediana del salario nacional

sem1_coningresos <- sem1 %>%  filter(PT4>0)
sem1_coningresos_svy <- srvyr::as_survey_design(sem1_coningresos, ids = numero, weights = pesomen)
sem1_media <-  sem1_coningresos_svy %>% srvyr::summarise(colname = srvyr::survey_mean(PT4))
sem1_media <-  sem1_media$colname

sem1_mediana <-  sem1_coningresos_svy %>% srvyr::summarise(colname = srvyr::survey_median(PT4))
sem1_mediana <-  sem1_mediana$colname


sem1 <- sem1 %>% dplyr::mutate(desempinsuf_media = ifelse(G148_3 < (sem1_media/2), 1, 0))
sem1 <- sem1 %>% dplyr::mutate(desempinsuf_mediana = ifelse(G148_3 < (sem1_mediana/2), 1, 0))

sem2_implat_coningresos <- sem2_implant %>%  filter(pt4>0)
sem2_implat_coningresos_svy <- srvyr::as_survey_design(sem2_implat_coningresos, ids = ID, weights = w_sem)
sem2_media <-  sem2_implat_coningresos_svy %>% srvyr::summarise(colname = srvyr::survey_mean(pt4))
sem2_media <-  sem2_media$colname

sem2_mediana <-  sem2_implat_coningresos_svy %>% srvyr::summarise(colname = srvyr::survey_median(pt4))
sem2_mediana <-  sem2_mediana$colname

sem2_implant <- sem2_implant %>% dplyr::mutate(desempinsuf_media = ifelse(g148_3 < (sem2_media/2), 1, 0))
sem2_implant <- sem2_implant %>% dplyr::mutate(desempinsuf_mediana = ifelse(g148_3 < (sem2_mediana/2), 1, 0))


# Ingresos desempleo por debajo del salario mínimo nacional

sem1 <- sem1 %>% dplyr::mutate(subsidio_def = G148_3 /  ipc_ene2022)
sem2_implant <- sem2_implant %>% dplyr::mutate(subsidio_def = g148_3 /  ipc_ene2022)

sem1 <- sem1 %>% dplyr::mutate(desempinsuf_smn = ifelse(subsidio_def < smn, 1, 0))
sem2_implant <- sem2_implant %>% dplyr::mutate(desempinsuf_smn = ifelse(subsidio_def < smn, 1, 0))


# Ingresos por jubilaciones

sem1 <- sem1 %>% dplyr::mutate(y_jub = G148_1_1 + G148_1_2 + G148_1_3 + G148_1_4 + G148_1_5 + G148_1_6 + G148_1_7 + G148_1_8 + G148_1_9 + G148_1_12 + G148_1_10 + G148_1_11)
sem2_implant <- sem2_implant %>% dplyr::mutate(y_jub = g148_1_1 + g148_1_2 + g148_1_3 + g148_1_5 + g148_1_6 + g148_1_7 + g148_1_8 + g148_1_9 + g148_1_12 + g148_1_10 + g148_1_11)


# Ingresos por jubilaciones por debajo de la línea de pobreza

sem1 <- sem1 %>% dplyr::mutate(jub_insuf = ifelse(y_jub < lp_unipersonales, 1, 0))
sem2_implant <- sem2_implant %>% dplyr::mutate(jub_insuf = ifelse(y_jub < lp_unipersonales, 1, 0))


# Ingresos por jubilaciones por debajo de la media y mediana del salario nacional

sem1 <- sem1 %>% dplyr::mutate(jub_insuf_media = ifelse(y_jub < (sem1_media/2), 1, 0))
sem1 <- sem1 %>% dplyr::mutate(jub_insuf_mediana = ifelse(y_jub < (sem1_mediana/2), 1, 0))

sem2_implant <- sem2_implant %>% dplyr::mutate(jub_insuf_media = ifelse(y_jub < (sem2_media/2), 1, 0))
sem2_implant <- sem2_implant %>% dplyr::mutate(jub_insuf_mediana = ifelse(y_jub < (sem2_mediana/2), 1, 0))


# Ingresos por jubilaciones por debajo del salario mínimo nacional

sem1 <- sem1 %>% dplyr::mutate(y_jub_def = y_jub /  ipc_ene2022)
sem2_implant <- sem2_implant %>% dplyr::mutate(y_jub_def = y_jub /  ipc_ene2022)

sem1 <- sem1 %>% dplyr::mutate(jub_insuf_smn = ifelse(y_jub_def < smn, 1, 0))
sem2_implant <- sem2_implant %>% dplyr::mutate(jub_insuf_smn = ifelse(y_jub_def < smn, 1, 0))


# Ingresos por pensiones


sem1 <- sem1 %>% dplyr::mutate(y_pens = G148_2_1 + G148_2_2 + G148_2_3 + G148_2_4 + G148_2_5 + G148_2_6 + G148_2_7 + G148_2_8 + G148_2_9 + G148_2_12 + G148_2_10 + G148_2_11)
sem2_implant <- sem2_implant %>% dplyr::mutate(y_pens = g148_2_1 + g148_2_2 + g148_2_3 + g148_2_5 + g148_2_6 + g148_2_7 + g148_2_8 + g148_2_9 + g148_2_12 + g148_2_10 + g148_2_11)

# Ingresos por pensiones debajo de la línea de pobreza

sem1 <- sem1 %>% dplyr::mutate(pens_insuf = ifelse(y_pens < lp_unipersonales, 1, 0))
sem2_implant <- sem2_implant %>% dplyr::mutate(pens_insuf = ifelse(y_pens < lp_unipersonales, 1, 0))

# Ingresos por pensiones debajo de la mitad de la media y la mediana del salario nacional

sem1 <- sem1 %>% dplyr::mutate(pens_insuf_media = ifelse(y_pens < (sem1_media/2), 1, 0))
sem1 <- sem1 %>% dplyr::mutate(pens_insuf_mediana = ifelse(y_pens < (sem1_mediana/2), 1, 0))

sem2_implant <- sem2_implant %>% dplyr::mutate(pens_insuf_media = ifelse(y_pens < (sem2_media/2), 1, 0))
sem2_implant <- sem2_implant %>% dplyr::mutate(pens_insuf_mediana = ifelse(y_pens < (sem2_mediana/2), 1, 0))

# Ingresos por pensiones debajo del SMN

sem1 <- sem1 %>% dplyr::mutate(y_pens_def = y_pens /  ipc_ene2022)
sem2_implant <- sem2_implant %>% dplyr::mutate(y_pens_def = y_pens /  ipc_ene2022)

sem1 <- sem1 %>% dplyr::mutate(pens_insuf_smn = ifelse(y_pens_def < smn, 1, 0))
sem2_implant <- sem2_implant %>% dplyr::mutate(pens_insuf_smn = ifelse(y_pens_def < smn, 1, 0))


# Ingresos por transferencias


sem1 <- sem1 %>% dplyr::mutate(y_transf = E560_1_1 + E560_2_1 + G257)
sem1 <-  sem1 %>%
  group_by(numero) %>%
  mutate(y_transf_h = sum(y_transf)) %>% ungroup()

sem1 <- sem1 %>% dplyr::mutate(hpc_y_transf =y_transf_h/ht19)
sem1 <- sem1 %>% dplyr::mutate(trans_insuf= ifelse(hpc_y_transf < li_06, 1, 0))


sem2_implant <- sem2_implant %>% dplyr::mutate(y_transf = e560_1_1 + e560_2_1 + g257)
sem2_implant <-  sem2_implant %>%
  group_by(ID) %>%
  mutate(y_transf_h = sum(y_transf)) %>% ungroup()

sem2_implant <- sem2_implant %>% dplyr::mutate(hpc_y_transf =y_transf_h/ht19)
sem2_implant <- sem2_implant %>% dplyr::mutate(trans_insuf= ifelse(hpc_y_transf < li, 1, 0))


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
# Porcentaje de desempleados que no cobra seguro de desempleo
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Total

a_mes <- sem1_svy %>%
  srvyr::filter(POBPCOAC>=4 & POBPCOAC<=5) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(seguroparo))

a_sem <- mean(as.numeric(a_mes$colname))

b_sem <- sem2_panel_svy %>%
  srvyr::filter(POBPCOAC>=4 & POBPCOAC<=5) %>%
  srvyr::summarise(colname = srvyr::survey_mean(seguroparo))

b_sem <-b_sem$colname

c_ano <- mean(c(a_sem, b_sem))


CODIND       <- 330101
DERECHO      <- "Seguridad Social"
TIPOIND      <- "Resultados"
DIMENSIÓN    <- "Accesibilidad"
CODINDICADOR <- "Desempleados que no perciben subsidio"
NOMINDICADOR <- "Porcentaje de desempleados que no cobra seguro de desempleo"
AÑO	         <- 2021
VALOR	       <- c_ano
FUENTE	     <- "Encuesta Continua de Hogares 2021, INE"
RESPONSABLE	 <- "J. Pandolfi"
JERARQUIA	   <- 1
CORTE        <- "Total"
DEPARTAMENTO <- ""
QUINTIL	     <- ""
SEXO		     <- ""
ASCENDENCIA  <- ""

ss_01 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de mayores de 65 años que no perciben jubilaciones ni pensiones
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Total

a_mes <- sem1_svy %>%
  srvyr::filter(E27>=65 & POBPCOAC!=2) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(nijubpens))

a_sem <- mean(as.numeric(a_mes$colname))

b_sem <- sem2_implant_svy %>%
  srvyr::filter(e27>=65 & pobpcoac!= 2) %>%
  srvyr::summarise(colname = srvyr::survey_mean(nijubpens))

b_sem <-b_sem$colname

c_ano <- mean(c(a_sem, b_sem))

# Ascendencia étnico racial

a_afro <- function(x) {
  x <- sem1_svy %>%
    filter(bd_e29_1 == x & E27>=65 & POBPCOAC!=2) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nijubpens))
  x <- mean(x$colname)
}       

a_e_afro <- numeric()

for(i in 1:2){
  a_e_afro[i] <- a_afro(x = i)
}     

b_afro <- function(x) {
  x <- sem2_implant_svy %>%
    filter(bd_e29_1 == x & e27>=65 & pobpcoac!= 2) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nijubpens))
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
    filter(dpto == x & E27>=65 & POBPCOAC!=2) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nijubpens))
  x <- mean(x$colname)
}       

a_e_dpto <- numeric()

for(i in 1:19){
  a_e_dpto[i] <- a_dpto(x = i)
}     

b_dpto <- function(x) {
  x <- sem2_implant_svy %>%
    filter(dpto == x & e27>=65 & pobpcoac!= 2) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nijubpens))
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
    filter(quintilesy == x & E27>=65 & POBPCOAC!=2) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nijubpens))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}     

b_quintil <- function(x) {
  x <- sem2_implant_svy %>%
    filter(quintilesy == x & e27>=65 & pobpcoac!= 2) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nijubpens))
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
    filter(bc_pe2 == x & E27>=65 & POBPCOAC!=2) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nijubpens))
  x <- mean(x$colname)
}       

a_e_sexo <- numeric()

for(i in 1:2){
  a_e_sexo[i] <- a_sexo(x = i)
}     

b_sexo <- function(x) {
  x <- sem2_implant_svy %>%
    filter(bc_pe2 == x & e27>=65 & pobpcoac!= 2) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nijubpens))
  x <- mean(x$colname)
}       

b_e_sexo <- numeric()

for(i in 1:2){
  b_e_sexo[i] <- b_sexo(x = i)
}         

c_sexo <- as.data.frame(cbind(a_e_sexo, b_e_sexo))
c_sexo <- c_sexo %>% dplyr::mutate(m_sexo = (a_e_sexo + b_e_sexo)/2)
c_sexo <- c_sexo[,c("m_sexo")]


CODIND       <- 330102
DERECHO      <- "Seguridad Social"
TIPOIND      <- "Resultados"
DIMENSIÓN    <- "Accesibilidad"
CODINDICADOR <- "Mayores de 65 años sin pensión ni jubilación"
NOMINDICADOR <- "Porcentaje de mayores de 65 años que no perciben jubilaciones ni pensiones"
AÑO	         <- 2021
VALOR	       <- c(c_ano, c_afro, c_dpto, c_quintil, c_sexo)
FUENTE	     <- "Encuesta Continua de Hogares 2021, INE"
RESPONSABLE	 <- "J. Pandolfi"
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

ss_02 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de la población económicamente activa que no aporta a la seguridad social
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Total

a_mes <- sem1_svy %>%
  srvyr::filter(POBPCOAC>=2 & POBPCOAC<=5) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(bc_register2))

a_sem <- mean(as.numeric(a_mes$colname))

b_sem <- sem2_implant_svy %>%
  srvyr::filter(pobpcoac>=2 & pobpcoac<=5) %>%
  srvyr::summarise(colname = srvyr::survey_mean(bc_register2))

b_sem <-b_sem$colname

c_ano <- mean(c(a_sem, b_sem))

# Ascendencia étnico racial

a_afro <- function(x) {
  x <- sem1_svy %>%
    filter(bd_e29_1 == x & POBPCOAC>=2 & POBPCOAC<=5) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_register2))
  x <- mean(x$colname)
}       

a_e_afro <- numeric()

for(i in 1:2){
  a_e_afro[i] <- a_afro(x = i)
}     

b_afro <- function(x) {
  x <- sem2_implant_svy %>%
    filter(bd_e29_1 == x & pobpcoac>=2 & pobpcoac<=5) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_register2))
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
    filter(dpto == x & POBPCOAC>=2 & POBPCOAC<=5) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_register2))
  x <- mean(x$colname)
}       

a_e_dpto <- numeric()

for(i in 1:19){
  a_e_dpto[i] <- a_dpto(x = i)
}     

b_dpto <- function(x) {
  x <- sem2_implant_svy %>%
    filter(dpto == x & pobpcoac>=2 & pobpcoac<=5) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_register2))
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
    filter(quintilesy == x & POBPCOAC>=2 & POBPCOAC<=5) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_register2))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}     

b_quintil <- function(x) {
  x <- sem2_implant_svy %>%
    filter(quintilesy == x & pobpcoac>=2 & pobpcoac<=5) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_register2))
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
    filter(bc_pe2 == x & POBPCOAC>=2 & POBPCOAC<=5) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_register2))
  x <- mean(x$colname)
}       

a_e_sexo <- numeric()

for(i in 1:2){
  a_e_sexo[i] <- a_sexo(x = i)
}     

b_sexo <- function(x) {
  x <- sem2_implant_svy %>%
    filter(bc_pe2 == x & pobpcoac>=2 & pobpcoac<=5) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_register2))
  x <- mean(x$colname)
}       

b_e_sexo <- numeric()

for(i in 1:2){
  b_e_sexo[i] <- b_sexo(x = i)
}         

c_sexo <- as.data.frame(cbind(a_e_sexo, b_e_sexo))
c_sexo <- c_sexo %>% dplyr::mutate(m_sexo = (a_e_sexo + b_e_sexo)/2)
c_sexo <- c_sexo[,c("m_sexo")]


CODIND       <- 330103
DERECHO      <- "Seguridad Social"
TIPOIND      <- "Resultados"
DIMENSIÓN    <- "Accesibilidad"
CODINDICADOR <- "Población activa que no aporta a la seguridad social"
NOMINDICADOR <- "Porcentaje de la población económicamente activa que no aporta a la seguridad social"
AÑO	         <- 2021
VALOR	       <- c(c_ano, c_afro, c_dpto, c_quintil, c_sexo)
FUENTE	     <- "Encuesta Continua de Hogares 2021, INE"
RESPONSABLE	 <- "J. Pandolfi"
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

ss_03 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA)




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de ocupados que no aporta a la seguridad social
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Total

a_mes <- sem1_svy %>%
  srvyr::filter(POBPCOAC==2) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(bc_register2))

a_sem <- mean(as.numeric(a_mes$colname))

b_sem <- sem2_implant_svy %>%
  srvyr::filter(pobpcoac == 2) %>%
  srvyr::summarise(colname = srvyr::survey_mean(bc_register2))

b_sem <-b_sem$colname

c_ano <- mean(c(a_sem, b_sem))

# Ascendencia étnico racial

a_afro <- function(x) {
  x <- sem1_svy %>%
    filter(bd_e29_1 == x & POBPCOAC==2) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_register2))
  x <- mean(x$colname)
}       

a_e_afro <- numeric()

for(i in 1:2){
  a_e_afro[i] <- a_afro(x = i)
}     

b_afro <- function(x) {
  x <- sem2_implant_svy %>%
    filter(bd_e29_1 == x  & pobpcoac == 2) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_register2))
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
    filter(dpto == x & POBPCOAC==2) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_register2))
  x <- mean(x$colname)
}       

a_e_dpto <- numeric()

for(i in 1:19){
  a_e_dpto[i] <- a_dpto(x = i)
}     

b_dpto <- function(x) {
  x <- sem2_implant_svy %>%
    filter(dpto == x  & pobpcoac ==2) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_register2))
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
    filter(quintilesy == x & POBPCOAC==2) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_register2))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}     

b_quintil <- function(x) {
  x <- sem2_implant_svy %>%
    filter(quintilesy == x  & pobpcoac ==2) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_register2))
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
    filter(bc_pe2 == x & POBPCOAC==2) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_register2))
  x <- mean(x$colname)
}       

a_e_sexo <- numeric()

for(i in 1:2){
  a_e_sexo[i] <- a_sexo(x = i)
}     

b_sexo <- function(x) {
  x <- sem2_implant_svy %>%
    filter(bc_pe2 == x & pobpcoac ==2) %>%
    srvyr::summarise(colname = srvyr::survey_mean(bc_register2))
  x <- mean(x$colname)
}       

b_e_sexo <- numeric()

for(i in 1:2){
  b_e_sexo[i] <- b_sexo(x = i)
}         

c_sexo <- as.data.frame(cbind(a_e_sexo, b_e_sexo))
c_sexo <- c_sexo %>% dplyr::mutate(m_sexo = (a_e_sexo + b_e_sexo)/2)
c_sexo <- c_sexo[,c("m_sexo")]


CODIND       <- 330104
DERECHO      <- "Seguridad Social"
TIPOIND      <- "Resultados"
DIMENSIÓN    <- "Accesibilidad"
CODINDICADOR <- "Porcentaje de ocupados que no aporta a la seguridad social"
NOMINDICADOR <- "Porcentaje de ocupados que no aporta a la seguridad social"
AÑO	         <- 2021
VALOR	       <- c(c_ano, c_afro, c_dpto, c_quintil, c_sexo)
FUENTE	     <- "Encuesta Continua de Hogares 2021, INE"
RESPONSABLE	 <- "J. Pandolfi"
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

ss_04 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de ocupados que no aporta a la seguridad social por la totalidad del salario
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Total

a_mes <- sem1_svy %>%
  srvyr::filter(POBPCOAC==2) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(noaportatot))

a_sem <- mean(as.numeric(a_mes$colname))

b_sem <- sem2_implant_svy %>%
  srvyr::filter(pobpcoac == 2) %>%
  srvyr::summarise(colname = srvyr::survey_mean(noaportatot))

b_sem <-b_sem$colname

c_ano <- mean(c(a_sem, b_sem))

# Ascendencia étnico racial

a_afro <- function(x) {
  x <- sem1_svy %>%
    filter(bd_e29_1 == x & POBPCOAC==2) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(noaportatot))
  x <- mean(x$colname)
}       

a_e_afro <- numeric()

for(i in 1:2){
  a_e_afro[i] <- a_afro(x = i)
}     

b_afro <- function(x) {
  x <- sem2_implant_svy %>%
    filter(bd_e29_1 == x  & pobpcoac == 2) %>%
    srvyr::summarise(colname = srvyr::survey_mean(noaportatot))
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
    filter(dpto == x & POBPCOAC==2) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(noaportatot))
  x <- mean(x$colname)
}       

a_e_dpto <- numeric()

for(i in 1:19){
  a_e_dpto[i] <- a_dpto(x = i)
}     

b_dpto <- function(x) {
  x <- sem2_implant_svy %>%
    filter(dpto == x  & pobpcoac ==2) %>%
    srvyr::summarise(colname = srvyr::survey_mean(noaportatot))
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
    filter(quintilesy == x & POBPCOAC==2) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(noaportatot))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}     

b_quintil <- function(x) {
  x <- sem2_implant_svy %>%
    filter(quintilesy == x  & pobpcoac ==2) %>%
    srvyr::summarise(colname = srvyr::survey_mean(noaportatot))
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
    filter(bc_pe2 == x & POBPCOAC==2) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(noaportatot))
  x <- mean(x$colname)
}       

a_e_sexo <- numeric()

for(i in 1:2){
  a_e_sexo[i] <- a_sexo(x = i)
}     

b_sexo <- function(x) {
  x <- sem2_implant_svy %>%
    filter(bc_pe2 == x & pobpcoac ==2) %>%
    srvyr::summarise(colname = srvyr::survey_mean(noaportatot))
  x <- mean(x$colname)
}       

b_e_sexo <- numeric()

for(i in 1:2){
  b_e_sexo[i] <- b_sexo(x = i)
}         

c_sexo <- as.data.frame(cbind(a_e_sexo, b_e_sexo))
c_sexo <- c_sexo %>% dplyr::mutate(m_sexo = (a_e_sexo + b_e_sexo)/2)
c_sexo <- c_sexo[,c("m_sexo")]


CODIND       <- 330105
DERECHO      <- "Seguridad Social"
TIPOIND      <- "Resultados"
DIMENSIÓN    <- "Accesibilidad"
CODINDICADOR <- "Informalidad parcial"
NOMINDICADOR <- "Porcentaje de ocupados que no aporta a la seguridad social por la totalidad del salario"
AÑO	         <- 2021
VALOR	       <- c(c_ano, c_afro, c_dpto, c_quintil, c_sexo)
FUENTE	     <- "Encuesta Continua de Hogares 2021, INE"
RESPONSABLE	 <- "J. Pandolfi"
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

ss_05 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de personas que residen en hogares pobres que no perciben transferencias (TUS o AFAM)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Total

a_mes <- sem1_svy %>%
  srvyr::filter(pobre06==1) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(nopercibe))

a_sem <- mean(as.numeric(a_mes$colname))

b_sem <- sem2_implant_svy %>%
  srvyr::filter(pobre==1) %>%
  srvyr::summarise(colname = srvyr::survey_mean(nopercibe))

b_sem <-b_sem$colname

c_ano <- mean(c(a_sem, b_sem))


CODIND       <- 330106
DERECHO      <- "Seguridad Social"
TIPOIND      <- "Resultados"
DIMENSIÓN    <- "Accesibilidad"
CODINDICADOR <- "Personas pobres que no reciben transferencias monetarias"
NOMINDICADOR <- "Porcentaje de personas que residen en hogares pobres que no perciben transferencias (TUS o AFAM)"
AÑO	         <- 2021
VALOR	       <- c_ano
FUENTE	     <- "Encuesta Continua de Hogares 2021, INE"
RESPONSABLE	 <- "J. Pandolfi"
JERARQUIA	   <- 1
CORTE        <- "Total"
DEPARTAMENTO <- ""
QUINTIL	     <- ""
SEXO		     <- ""
ASCENDENCIA  <- ""
ss_06 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de mayores de 60 años que no perciben jubilaciones ni pensiones
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Total

a_mes <- sem1_svy %>%
  srvyr::filter(E27>=60 & POBPCOAC!=2) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(nijubpens))

a_sem <- mean(as.numeric(a_mes$colname))

b_sem <- sem2_implant_svy %>%
  srvyr::filter(e27>=60 & pobpcoac!= 2) %>%
  srvyr::summarise(colname = srvyr::survey_mean(nijubpens))

b_sem <-b_sem$colname

c_ano <- mean(c(a_sem, b_sem))

# Ascendencia étnico racial

a_afro <- function(x) {
  x <- sem1_svy %>%
    filter(bd_e29_1 == x & E27>=60 & POBPCOAC!=2) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nijubpens))
  x <- mean(x$colname)
}       

a_e_afro <- numeric()

for(i in 1:2){
  a_e_afro[i] <- a_afro(x = i)
}     

b_afro <- function(x) {
  x <- sem2_implant_svy %>%
    filter(bd_e29_1 == x & e27>=60 & pobpcoac!= 2) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nijubpens))
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
    filter(dpto == x & E27>=60 & POBPCOAC!=2) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nijubpens))
  x <- mean(x$colname)
}       

a_e_dpto <- numeric()

for(i in 1:19){
  a_e_dpto[i] <- a_dpto(x = i)
}     

b_dpto <- function(x) {
  x <- sem2_implant_svy %>%
    filter(dpto == x & e27>=60 & pobpcoac!= 2) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nijubpens))
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
    filter(quintilesy == x & E27>=60 & POBPCOAC!=2) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nijubpens))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}     

b_quintil <- function(x) {
  x <- sem2_implant_svy %>%
    filter(quintilesy == x & e27>=60 & pobpcoac!= 2) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nijubpens))
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
    filter(bc_pe2 == x & E27>=60 & POBPCOAC!=2) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nijubpens))
  x <- mean(x$colname)
}       

a_e_sexo <- numeric()

for(i in 1:2){
  a_e_sexo[i] <- a_sexo(x = i)
}     

b_sexo <- function(x) {
  x <- sem2_implant_svy %>%
    filter(bc_pe2 == x & e27>=60 & pobpcoac!= 2) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nijubpens))
  x <- mean(x$colname)
}       

b_e_sexo <- numeric()

for(i in 1:2){
  b_e_sexo[i] <- b_sexo(x = i)
}         

c_sexo <- as.data.frame(cbind(a_e_sexo, b_e_sexo))
c_sexo <- c_sexo %>% dplyr::mutate(m_sexo = (a_e_sexo + b_e_sexo)/2)
c_sexo <- c_sexo[,c("m_sexo")]


CODIND       <- 330107
DERECHO      <- "Seguridad Social"
TIPOIND      <- "Resultados"
DIMENSIÓN    <- "Accesibilidad"
CODINDICADOR <- "Inactivos mayores de 60 años sin pensión ni jubilación"
NOMINDICADOR <- "Porcentaje de inactivos mayores de 60 años que no perciben jubilaciones ni pensiones"
AÑO	         <- 2021
VALOR	       <- c(c_ano, c_afro, c_dpto, c_quintil, c_sexo)
FUENTE	     <- "Encuesta Continua de Hogares 2021, INE"
RESPONSABLE	 <- "J. Pandolfi"
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

ss_07 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de personas desempleadas durante uno a seis meses que cobra seguro de desempleo por debajo de la línea de pobreza
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Total

a_mes <- sem1_svy %>%
  srvyr::filter(desemp1a6 ==1) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(desempinsuf))

a_sem <- mean(as.numeric(a_mes$colname))

b_sem <- sem2_implant_svy %>%
  srvyr::filter(desemp1a6 ==1) %>%
  srvyr::summarise(colname = srvyr::survey_mean(desempinsuf))

b_sem <-b_sem$colname

c_ano <- mean(c(a_sem, b_sem))


CODIND       <- 330201
DERECHO      <- "Seguridad Social"
TIPOIND      <- "Resultados"
DIMENSIÓN    <- "Adecuación"
CODINDICADOR <- "Insuficiencia del subsidio por desempleo en relación a la línea de pobreza"
NOMINDICADOR <- "Porcentaje de personas desempleadas durante uno a seis meses que cobra seguro de desempleo por debajo de la línea de pobreza"
AÑO	         <- 2021
VALOR	       <- c_ano
FUENTE	     <- "Encuesta Continua de Hogares 2021, INE"
RESPONSABLE	 <- "J. Pandolfi"
JERARQUIA	   <- 1
CORTE        <- "Total"
DEPARTAMENTO <- ""
QUINTIL	     <- ""
SEXO		     <- ""
ASCENDENCIA  <- ""
ss_08 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de personas desempleadas durante uno a seis meses que cobra seguro de desempleo por debajo de la mitad de la media del salario nacional
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Total

a_mes <- sem1_svy %>%
  srvyr::filter(desemp1a6 ==1) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(desempinsuf_media))

a_sem <- mean(as.numeric(a_mes$colname))

b_sem <- sem2_implant_svy %>%
  srvyr::filter(desemp1a6 ==1) %>%
  srvyr::summarise(colname = srvyr::survey_mean(desempinsuf_media))

b_sem <-b_sem$colname

c_ano <- mean(c(a_sem, b_sem))


CODIND       <- 330202
DERECHO      <- "Seguridad Social"
TIPOIND      <- "Resultados"
DIMENSIÓN    <- "Adecuación"
CODINDICADOR <- "Insuficiencia del subsidio por desempleo en relación al salario nacional (mitad del promedio)"
NOMINDICADOR <- "Porcentaje de personas desempleadas durante uno a seis meses que cobra seguro de desempleo por debajo de la mitad de la media del salario nacional"
AÑO	         <- 2021
VALOR	       <- c_ano
FUENTE	     <- "Encuesta Continua de Hogares 2021, INE"
RESPONSABLE	 <- "J. Pandolfi"
JERARQUIA	   <- 1
CORTE        <- "Total"
DEPARTAMENTO <- ""
QUINTIL	     <- ""
SEXO		     <- ""
ASCENDENCIA  <- ""
ss_09 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de personas desempleadas durante uno a seis meses que cobra seguro de desempleo por debajo de la mitad de la mediana del salario nacional
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Total

a_mes <- sem1_svy %>%
  srvyr::filter(desemp1a6 ==1) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(desempinsuf_mediana))

a_sem <- mean(as.numeric(a_mes$colname))

b_sem <- sem2_implant_svy %>%
  srvyr::filter(desemp1a6 ==1) %>%
  srvyr::summarise(colname = srvyr::survey_mean(desempinsuf_mediana))

b_sem <-b_sem$colname

c_ano <- mean(c(a_sem, b_sem))


CODIND       <- 330203
DERECHO      <- "Seguridad Social"
TIPOIND      <- "Resultados"
DIMENSIÓN    <- "Adecuación"
CODINDICADOR <- "Insuficiencia del subsidio por desempleo en relación al salario nacional (mitad de la mediana)"
NOMINDICADOR <- "Porcentaje de personas desempleadas durante uno a seis meses que cobra seguro de desempleo por debajo de la mitad de la mediana del salario nacional"
AÑO	         <- 2021
VALOR	       <- c_ano
FUENTE	     <- "Encuesta Continua de Hogares 2021, INE"
RESPONSABLE	 <- "J. Pandolfi"
JERARQUIA	   <- 1
CORTE        <- "Total"
DEPARTAMENTO <- ""
QUINTIL	     <- ""
SEXO		     <- ""
ASCENDENCIA  <- ""
ss_10 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de personas desempleadas durante uno a seis meses que cobra seguro de desempleo por debajo del SMN
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Total

a_mes <- sem1_svy %>%
  srvyr::filter(desemp1a6 ==1) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(desempinsuf_smn))

a_sem <- mean(as.numeric(a_mes$colname))

b_sem <- sem2_implant_svy %>%
  srvyr::filter(desemp1a6 ==1) %>%
  srvyr::summarise(colname = srvyr::survey_mean(desempinsuf_smn))

b_sem <-b_sem$colname

c_ano <- mean(c(a_sem, b_sem))


CODIND       <- 330204
DERECHO      <- "Seguridad Social"
TIPOIND      <- "Resultados"
DIMENSIÓN    <- "Adecuación"
CODINDICADOR <- "Insuficiencia del subsidio por desempleo en relación al salario mínimo nacional"
NOMINDICADOR <- "Porcentaje de personas desempleadas durante uno a seis meses que cobra seguro de desempleo por debajo del salario mínimo nacional"
AÑO	         <- 2021
VALOR	       <- c_ano
FUENTE	     <- "Encuesta Continua de Hogares 2021, INE"
RESPONSABLE	 <- "J. Pandolfi"
JERARQUIA	   <- 1
CORTE        <- "Total"
DEPARTAMENTO <- ""
QUINTIL	     <- ""
SEXO		     <- ""
ASCENDENCIA  <- ""
ss_11 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de personas que perciben jubilaciones por debajo de la línea de pobreza
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Total

a_mes <- sem1_svy %>%
  srvyr::filter(y_jub > 0) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(jub_insuf))

a_sem <- mean(as.numeric(a_mes$colname))

b_sem <- sem2_implant_svy %>%
  srvyr::filter(y_jub > 0) %>%
  srvyr::summarise(colname = srvyr::survey_mean(jub_insuf))

b_sem <-b_sem$colname

c_ano <- mean(c(a_sem, b_sem))


CODIND       <- 330205
DERECHO      <- "Seguridad Social"
TIPOIND      <- "Resultados"
DIMENSIÓN    <- "Adecuación"
CODINDICADOR <- "Insuficiencia de las jubilaciones en relación a la línea de las pobreza"
NOMINDICADOR <- "Porcentaje de personas que perciben jubilaciones por debajo de la línea de pobreza"
AÑO	         <- 2021
VALOR	       <- c_ano
FUENTE	     <- "Encuesta Continua de Hogares 2021, INE"
RESPONSABLE	 <- "J. Pandolfi"
JERARQUIA	   <- 1
CORTE        <- "Total"
DEPARTAMENTO <- ""
QUINTIL	     <- ""
SEXO		     <- ""
ASCENDENCIA  <- ""
ss_12 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de personas que perciben jubilaciones por debajo de mitad de la media del salario nacional
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Total

a_mes <- sem1_svy %>%
  srvyr::filter(y_jub > 0) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(jub_insuf_media))

a_sem <- mean(as.numeric(a_mes$colname))

b_sem <- sem2_implant_svy %>%
  srvyr::filter(y_jub > 0) %>%
  srvyr::summarise(colname = srvyr::survey_mean(jub_insuf_media))

b_sem <-b_sem$colname

c_ano <- mean(c(a_sem, b_sem))


CODIND       <- 330206
DERECHO      <- "Seguridad Social"
TIPOIND      <- "Resultados"
DIMENSIÓN    <- "Adecuación"
CODINDICADOR <- "Insuficiencia de las jubilaciones en relación al salario nacional (mitad del promedio)"
NOMINDICADOR <- "Porcentaje de personas que perciben jubilaciones por debajo de la mitad de la media del salario nacional"
AÑO	         <- 2021
VALOR	       <- c_ano
FUENTE	     <- "Encuesta Continua de Hogares 2021, INE"
RESPONSABLE	 <- "J. Pandolfi"
JERARQUIA	   <- 1
CORTE        <- "Total"
DEPARTAMENTO <- ""
QUINTIL	     <- ""
SEXO		     <- ""
ASCENDENCIA  <- ""
ss_13 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA)




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de personas que perciben jubilaciones por debajo de mitad de la mediana del salario nacional
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Total

a_mes <- sem1_svy %>%
  srvyr::filter(y_jub > 0) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(jub_insuf_mediana))

a_sem <- mean(as.numeric(a_mes$colname))

b_sem <- sem2_implant_svy %>%
  srvyr::filter(y_jub > 0) %>%
  srvyr::summarise(colname = srvyr::survey_mean(jub_insuf_mediana))

b_sem <-b_sem$colname

c_ano <- mean(c(a_sem, b_sem))


CODIND       <- 330207
DERECHO      <- "Seguridad Social"
TIPOIND      <- "Resultados"
DIMENSIÓN    <- "Adecuación"
CODINDICADOR <- "Insuficiencia de las jubilaciones en relación al salario nacional (mitad de las la mediana)"
NOMINDICADOR <- "Porcentaje de personas que perciben jubilaciones por debajo de la mitad de la mediana del salario nacional"
AÑO	         <- 2021
VALOR	       <- c_ano
FUENTE	     <- "Encuesta Continua de Hogares 2021, INE"
RESPONSABLE	 <- "J. Pandolfi"
JERARQUIA	   <- 1
CORTE        <- "Total"
DEPARTAMENTO <- ""
QUINTIL	     <- ""
SEXO		     <- ""
ASCENDENCIA  <- ""
ss_14 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de personas que perciben jubilaciones por debajo del SMN
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Total

a_mes <- sem1_svy %>%
  srvyr::filter(y_jub > 0) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(jub_insuf_smn))

a_sem <- mean(as.numeric(a_mes$colname))

b_sem <- sem2_implant_svy %>%
  srvyr::filter(y_jub > 0) %>%
  srvyr::summarise(colname = srvyr::survey_mean(jub_insuf_smn))

b_sem <-b_sem$colname

c_ano <- mean(c(a_sem, b_sem))


CODIND       <- 330208
DERECHO      <- "Seguridad Social"
TIPOIND      <- "Resultados"
DIMENSIÓN    <- "Adecuación"
CODINDICADOR <- "Insuficiencia de las jubilaciones en relación al salario mínimo nacional"
NOMINDICADOR <- "Porcentaje de personas que perciben jubilaciones por debajo del salario mínimo nacional"
AÑO	         <- 2021
VALOR	       <- c_ano
FUENTE	     <- "Encuesta Continua de Hogares 2021, INE"
RESPONSABLE	 <- "J. Pandolfi"
JERARQUIA	   <- 1
CORTE        <- "Total"
DEPARTAMENTO <- ""
QUINTIL	     <- ""
SEXO		     <- ""
ASCENDENCIA  <- ""
ss_15 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA)




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de personas que perciben pensiones por debajo de la línea de pobreza
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Total

a_mes <- sem1_svy %>%
  srvyr::filter(y_pens > 0) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(pens_insuf))

a_sem <- mean(as.numeric(a_mes$colname))

b_sem <- sem2_implant_svy %>%
  srvyr::filter(y_pens > 0) %>%
  srvyr::summarise(colname = srvyr::survey_mean(pens_insuf))

b_sem <-b_sem$colname

c_ano <- mean(c(a_sem, b_sem))


CODIND       <- 330209
DERECHO      <- "Seguridad Social"
TIPOIND      <- "Resultados"
DIMENSIÓN    <- "Adecuación"
CODINDICADOR <- "Insuficiencia de las pensiones en relación a la línea de las pobreza"
NOMINDICADOR <- "Porcentaje de personas que perciben pensiones por debajo de la línea de pobreza"
AÑO	         <- 2021
VALOR	       <- c_ano
FUENTE	     <- "Encuesta Continua de Hogares 2021, INE"
RESPONSABLE	 <- "J. Pandolfi"
JERARQUIA	   <- 1
CORTE        <- "Total"
DEPARTAMENTO <- ""
QUINTIL	     <- ""
SEXO		     <- ""
ASCENDENCIA  <- ""
ss_16 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de personas que perciben pensiones por debajo de la mitad de la media del salario nacional
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Total

a_mes <- sem1_svy %>%
  srvyr::filter(y_pens > 0) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(pens_insuf_media))

a_sem <- mean(as.numeric(a_mes$colname))

b_sem <- sem2_implant_svy %>%
  srvyr::filter(y_pens > 0) %>%
  srvyr::summarise(colname = srvyr::survey_mean(pens_insuf_media))

b_sem <-b_sem$colname

c_ano <- mean(c(a_sem, b_sem))


CODIND       <- 330210
DERECHO      <- "Seguridad Social"
TIPOIND      <- "Resultados"
DIMENSIÓN    <- "Adecuación"
CODINDICADOR <- "Insuficiencia de las pensiones en relación al salario nacional (mitad del promedio)"
NOMINDICADOR <- "Porcentaje de personas que perciben pensiones por debajo de la mitad de la media del salario nacional"
AÑO	         <- 2021
VALOR	       <- c_ano
FUENTE	     <- "Encuesta Continua de Hogares 2021, INE"
RESPONSABLE	 <- "J. Pandolfi"
JERARQUIA	   <- 1
CORTE        <- "Total"
DEPARTAMENTO <- ""
QUINTIL	     <- ""
SEXO		     <- ""
ASCENDENCIA  <- ""
ss_17 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de personas que perciben pensiones por debajo de la mitad de la mediana del salario nacional
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Total

a_mes <- sem1_svy %>%
  srvyr::filter(y_pens > 0) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(pens_insuf_mediana))

a_sem <- mean(as.numeric(a_mes$colname))

b_sem <- sem2_implant_svy %>%
  srvyr::filter(y_pens > 0) %>%
  srvyr::summarise(colname = srvyr::survey_mean(pens_insuf_mediana))

b_sem <-b_sem$colname

c_ano <- mean(c(a_sem, b_sem))


CODIND       <- 330211
DERECHO      <- "Seguridad Social"
TIPOIND      <- "Resultados"
DIMENSIÓN    <- "Adecuación"
CODINDICADOR <- "Insuficiencia de las pensiones en relación al salario nacional (mitad de las la mediana)"
NOMINDICADOR <- "Porcentaje de personas que perciben pensiones por debajo de la mitad de la mediana del salario nacional"
AÑO	         <- 2021
VALOR	       <- c_ano
FUENTE	     <- "Encuesta Continua de Hogares 2021, INE"
RESPONSABLE	 <- "J. Pandolfi"
JERARQUIA	   <- 1
CORTE        <- "Total"
DEPARTAMENTO <- ""
QUINTIL	     <- ""
SEXO		     <- ""
ASCENDENCIA  <- ""
ss_18 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de personas que perciben pensiones por debajo del SMN
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Total

a_mes <- sem1_svy %>%
  srvyr::filter(y_pens > 0) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(pens_insuf_smn))

a_sem <- mean(as.numeric(a_mes$colname))

b_sem <- sem2_implant_svy %>%
  srvyr::filter(y_pens > 0) %>%
  srvyr::summarise(colname = srvyr::survey_mean(pens_insuf_smn))

b_sem <-b_sem$colname

c_ano <- mean(c(a_sem, b_sem))


CODIND       <- 330212
DERECHO      <- "Seguridad Social"
TIPOIND      <- "Resultados"
DIMENSIÓN    <- "Adecuación"
CODINDICADOR <- "Insuficiencia de las pensiones en relación al salario mínimo nacional"
NOMINDICADOR <- "Porcentaje de personas que perciben pensiones por debajo del salario mínimo nacional"
AÑO	         <- 2021
VALOR	       <- c_ano
FUENTE	     <- "Encuesta Continua de Hogares 2021, INE"
RESPONSABLE	 <- "J. Pandolfi"
JERARQUIA	   <- 1
CORTE        <- "Total"
DEPARTAMENTO <- ""
QUINTIL	     <- ""
SEXO		     <- ""
ASCENDENCIA  <- ""
ss_19 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de personas que residen en hogares que perciben transferencias (TUS y/o AFAM) cuyo monto per-cápita es inferior a una canasta básica alimentaria
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Total

a_mes <- sem1_svy %>%
  srvyr::filter(y_transf_h > 0) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(trans_insuf))

a_sem <- mean(as.numeric(a_mes$colname))

b_sem <- sem2_implant_svy %>%
  srvyr::filter(y_transf_h > 0) %>%
  srvyr::summarise(colname = srvyr::survey_mean(trans_insuf))

b_sem <-b_sem$colname

c_ano <- mean(c(a_sem, b_sem))


CODIND       <- 330213
DERECHO      <- "Seguridad Social"
TIPOIND      <- "Resultados"
DIMENSIÓN    <- "Adecuación"
CODINDICADOR <- "Insuficiencia de las transferencias monetarias en relación a la canasta básica alimentaria"
NOMINDICADOR <- "Porcentaje de personas que residen en hogares que perciben transferencias (TUS y/o AFAM) cuyo monto per-cápita es inferior a una canasta básica alimentaria"
AÑO	         <- 2021
VALOR	       <- c_ano
FUENTE	     <- "Encuesta Continua de Hogares 2021, INE"
RESPONSABLE	 <- "J. Pandolfi"
JERARQUIA	   <- 1
CORTE        <- "Total"
DEPARTAMENTO <- ""
QUINTIL	     <- ""
SEXO		     <- ""
ASCENDENCIA  <- ""
ss_20 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA)


ss_2021 <- rbind(ss_01, ss_02, ss_03, ss_04, ss_05, ss_06, ss_07, ss_08, ss_09, ss_10, ss_11, ss_12, ss_13, ss_14, ss_15, ss_16, ss_17, ss_18, ss_19, ss_20)

rio::export(ss_2021, "ss_2021.xlsx" )


