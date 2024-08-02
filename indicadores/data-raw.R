
## ***************************************************************************
## Script para procesar data cruda
## ***************************************************************************

library(tidyverse)

rm(list = ls())

source("utils.R")

metadata <- readxl::read_excel("Data/Base_fichas_indicadores.xlsx") %>% 
  select(-DERECHO, -POBLACION, -TIPOIND, -DIMENSIÓN, -CONINDICADOR, -NOMINDICADOR,
         -FUENTE)

metadata$ACTUALIZACION <- as.character(metadata$ACTUALIZACION)

glimpse(metadata)

ind_con_ceros <- c(220105, 220106, 230501, 130201, 430305)

dat <- readxl::read_excel("Data/Base_mirador_desca.xlsx",
                          col_types = c("numeric", "text", "text","text", "text",
                                        "text", "text", "numeric", "text", "numeric",
                                        "text", "text", "numeric", "numeric",
                                        "text", "text", "text", "text", "text",
                                        "text", "text", "text", "text", "text",
                                        "text", "text", "text", "text", "text",
                                        "text", "text", "text", "text", "text",
                                        "text", "text", "text", "text", "text",
                                        "text", "text", "text", "text", "text",
                                        "text", "text", "text", "text", "text",
                                        "text", "text", "text", "text", "text", 
                                        "text", "text", "text", "text")) %>%
  mutate(FECHA = as.Date(ISOdate(AÑO, 1, 1))) %>% 
  mutate(VALOR = ifelse(VALOR  > 0.00001 & VALOR < 0.1, round(VALOR, digits = 3),
                        ifelse(VALOR >= 0.1 & VALOR  < 1, round(VALOR, digits = 2),
                               ifelse(VALOR >= 1 & VALOR < 100, round(VALOR, digits = 1),
                                      ifelse(VALOR >= 100  & VALOR < 1000, round(VALOR, digits = 1),
                                             ifelse(VALOR >= 1000, round(VALOR, digits = 0),
                                                    VALOR))))))
glimpse(dat)

dat$Sexo <- as.factor(dat$Sexo)
dat$Sexo <- factor(dat$Sexo, levels = levels(dat$Sexo)[c(1, 3, 2)])

dat$Edad <- as.factor(dat$Edad)
dat$Edad <- factor(dat$Edad, levels = levels(dat$Edad)[c(1, 4, 24, 35, 42, 48, 50, 55, 56, 57, 5, 6, 7, 8, 11, 15, 17, 18, 19, 22, 25, 27, 29, 30, 31, 3, 2, 51, 9, 10, 12, 13, 14, 16, 20, 21, 23, 26, 28, 32, 33, 34, 36, 37, 38, 39, 40, 41, 43, 44, 45, 46, 47, 49, 52, 53, 54, 58, 69, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 70, 71)])
                                                         

dat$Región <- as.factor(dat$Región)
dat$Región <- factor(dat$Región, levels = levels(dat$Región)[c(2, 1, 3)])

dat$Departamento <- as.factor(dat$Departamento)
#dat$Departamento <- factor(dat$Departamento, levels = levels(dat$Departamento)[c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 20, 19)])


dat <- dat %>% 
  left_join(metadata, by = "CODINDICADOR") %>% 
  relocate(FECHA, DEFINICIÓN, CÁLCULO, UNIDAD, COBERTURA_GEO,
           COBERTURA_TEMPORAL, FRECUENCIA_REPORTE, CITA, UMBRAL, APERTURA, OBSERVACIONES, ACTUALIZACION, WEB,
           WEB_POB, SEXO_POB, ASCENDENCIA_POB, EDAD_POB) %>% 
  # filter(WEB == 1) %>%
  janitor::clean_names()

rio::export(dat, 'Data/data_motor.rda')

# Crear clave de variables y nombres a presentar
test <- readxl::read_excel("Data/Base_mirador_desca.xlsx")

names_proper <- firstup(tolower(gsub("_", " ", names(readxl::read_excel("Data/Base_mirador_desca.xlsx")))))
names_var <- names(janitor::clean_names(readxl::read_excel("Data/Base_mirador_desca.xlsx")))
df_keys <- tibble(names_proper = names_proper,
                  names_var = names_var)

writexl::write_xlsx(df_keys, "Data/keys.xlsx")

