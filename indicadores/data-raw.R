
## ***************************************************************************
## Script para procesar data cruda
## ***************************************************************************

library(tidyverse)

rm(list = ls())

metadata <- readxl::read_excel("Data/Base_fichas_indicadores.xlsx") %>% 
  select(-DERECHO, -TIPOIND, -DIMENSIÓN, -CONINDICADOR, -NOMINDICADOR,
         -FUENTE)

dat <- readxl::read_excel("Data/Base_mirador_desca.xlsx",
                          col_types = c("numeric", "text", "text","text",
                                        "text", "text", "numeric", "numeric",
                                        "text", "text", "text", "text", 
                                        "text", "text", "text", "text",
                                        "text", "text", "text", "text",
                                        "text", "text", "text", "text",
                                        "text", "text", "text", "text",
                                        "text", "text", "text", "text")) %>%
  mutate(FECHA = as.Date(ISOdate(AÑO, 1, 1))) %>% 
  mutate(VALOR = ifelse(VALOR  > 0.00001 & VALOR < 0.1, round(VALOR, digits = 3),
                        ifelse(VALOR >= 0.1 & VALOR  < 1, round(VALOR, digits = 2),
                               ifelse(VALOR > 1 & VALOR < 100, round(VALOR, digits = 1),
                                      ifelse(VALOR > 100  & VALOR < 1000, round(VALOR, digits = 0),
                                             ifelse(VALOR > 1000, round(VALOR, digits = 0),
                                                    NA))))))
dat$SEXO <- as.factor(dat$SEXO)
dat$SEXO <- factor(dat$SEXO, levels = levels(dat$SEXO)[c(1, 3, 2)])

dat$EDAD <- as.factor(dat$EDAD)
dat$EDAD <- factor(dat$EDAD, levels = levels(dat$EDAD)[c(1,	3,	18,	27,	29,	30,	31,	34,	35,	36,	4,	5,	6,	7,	10,	11,	12,	13,	14,	16,	19,	20,	22,	23,	24,	2, 32,	8,	9,	15,	17,	21,	25,	26,	28,	33,	37)])

table(dat$PRESTADOR)
dat$PRESTADOR <- as.factor(dat$PRESTADOR)
dat$PRESTADOR <- factor(dat$PRESTADOR, levels = levels(dat$PRESTADOR)[c(1, 5, 6, 4, 3, 7, 9, 8)])
table(dat$PRESTADOR)

dat$REGIÓN <- as.factor(dat$REGIÓN)
dat$REGIÓN <- factor(dat$REGIÓN, levels = levels(dat$REGIÓN)[c(2, 1, 3)])

dat$DEPARTAMENTO <- as.factor(dat$DEPARTAMENTO)
dat$DEPARTAMENTO <- factor(dat$DEPARTAMENTO, levels = levels(dat$DEPARTAMENTO)[c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 20, 19)])

# dat$NIVEL <- as.factor(dat$NIVEL)
# dat$NIVEL <- factor(dat$NIVEL, levels = levels(dat$NIVEL)[c(1, 5, 6, 2, 3, 7, 4, 8)])
# 
# dat$GRADO <- as.factor(dat$GRADO)
# dat$GRADO <- factor(dat$GRADO, levels = levels(dat$GRADO)[c(1, 2, 3, 5, 7, 9, 4, 6, 8, 10)])
# 
# dat$CATESCUELA <- as.factor(dat$CATESCUELA)
# dat$CATESCUELA <- factor(dat$CATESCUELA, levels = levels(dat$CATESCUELA)[c(1, 2, 3, 4, 5, 6, 7, 9, 8)])
# 
# dat$CONESCUELA <- as.factor(dat$CONESCUELA)
# dat$CONESCUELA <- factor(dat$CONESCUELA, levels = levels(dat$CONESCUELA)[c(1, 2, 3, 4, 5, 13, 6, 7, 8, 9, 10, 12, 11)])
# 
# dat$URBANORURALUY <- as.factor(dat$URBANORURALUY)
# dat$URBANORURALUY <- factor(dat$URBANORURALUY, levels = levels(dat$URBANORURALUY)[c(3, 4, 1, 2)])
# 
# dat$RIESGO <- as.factor(dat$RIESGO)
# dat$RIESGO <- factor(dat$RIESGO, levels = levels(dat$RIESGO)[c(3, 5, 6, 1, 2, 4, 7)])
# 
# dat$SOLHABITAC <- as.factor(dat$SOLHABITAC)
# dat$SOLHABITAC <- factor(dat$SOLHABITAC, levels = levels(dat$SOLHABITAC)[c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 12)])

dat <- dat %>% 
  left_join(metadata, by = "CODINDICADOR") %>% 
  relocate(FECHA, id, DEFINICIÓN, CÁLCULO, UNIDAD, COBERTURA_GEO,
           COBERTURA_TEMPORAL, FRECUENCIA_REPORTE, CITA, UMBRAL, APERTURA, WEB) %>% 
  # filter(WEB == 1) %>%
  janitor::clean_names()

rio::export(dat, 'Data/data_motor.rda')
