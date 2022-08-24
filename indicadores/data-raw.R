
## ***************************************************************************
## Script para procesar data cruda
## ***************************************************************************

library(tidyverse)

rm(list = ls())

source("utils.R")

metadata <- readxl::read_excel("Data/Base_fichas_indicadores.xlsx") %>% 
  select(-DERECHO, -TIPOIND, -DIMENSIÓN, -CONINDICADOR, -NOMINDICADOR,
         -FUENTE)

glimpse(metadata)

ind_con_ceros <- c(220105, 220106, 230501, 130201, 430305)

dat <- readxl::read_excel("Data/Base_mirador_desca.xlsx",
                          col_types = c("numeric", "text", "text","text",
                                        "text", "text", "numeric", "numeric",
                                        "text", "text", "numeric", "text",
                                        "text", "text", "text", "text",
                                        "text", "text", "text", "text", "text",
                                        "text", "text", "text", "text",
                                        "text", "text", "text", "text",
                                        "text", "text", "text", "text")) %>%
  mutate(FECHA = as.Date(ISOdate(AÑO, 1, 1))) %>% 

  mutate(VALOR = ifelse(VALOR  > 0.00001 & VALOR < 0.1, round(VALOR, digits = 3),
                        ifelse(VALOR >= 0.1 & VALOR  < 1, round(VALOR, digits = 2),
                               ifelse(VALOR >= 1 & VALOR < 100, round(VALOR, digits = 1),
                                      ifelse(VALOR >= 100  & VALOR < 1000, round(VALOR, digits = 0),
                                             ifelse(VALOR >= 1000, round(VALOR, digits = 0),
                                                    VALOR))))))

glimpse(dat)

dat$Sexo <- as.factor(dat$Sexo)
dat$Sexo <- factor(dat$Sexo, levels = levels(dat$Sexo)[c(1, 3, 2)])

dat$Edad <- as.factor(dat$Edad)
dat$Edad <- factor(dat$Edad, levels = levels(dat$Edad)[c(1,	3,	18,	27,	29,	30,	31,	34,	35,	36,	4,	5,	6,	7,	10,	11,	12,	13,	14,	16,	19,	20,	22,	23,	24,	2, 32,	8,	9,	15,	17,	21,	25,	26,	28,	33,	37)])

dat$Región <- as.factor(dat$Región)
dat$Región <- factor(dat$Región, levels = levels(dat$Región)[c(2, 1, 3)])

dat$Departamento <- as.factor(dat$Departamento)
dat$Departamento <- factor(dat$Departamento, levels = levels(dat$Departamento)[c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 20, 19)])

dat <- dat %>% 
  left_join(metadata, by = "CODINDICADOR") %>% 
  relocate(FECHA, DEFINICIÓN, CÁLCULO, UNIDAD, COBERTURA_GEO,
           COBERTURA_TEMPORAL, FRECUENCIA_REPORTE, CITA, UMBRAL, APERTURA, WEB,
           WEB_POB, SEXO_POB, ASCENDENCIA_POB, EDAD_POB) %>% 
  # filter(WEB == 1) %>%
  janitor::clean_names()

rio::export(dat, 'Data/data_motor.rda')

# Crear clave de variables y nombres a presentar
test <- readxl::read_excel("Data/Base_mirador_desca.xlsx")

names_proper <- firstup(tolower(gsub("_", " ", names(readxl::read_excel("Data/Base_mirador_desca_2.xlsx")))))
names_var <- names(janitor::clean_names(readxl::read_excel("Data/Base_mirador_desca_2.xlsx")))
df_keys <- tibble(names_proper = names_proper,
                  names_var = names_var)
writexl::write_xlsx(df_keys, "data/keys.xlsx")

