### Mirador DESCA
### Derecho a la educación
### Unidad de Métodos y Acceso a datos

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

library(here)
library(rio)
library(survey)
library(srvyr)
library(tidyverse)
library(laeken)
library(statar)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

### Referencias generales para la base motor ###


DERECHO      <- "Educación"
TIPOIND      <- "Resultados"
AÑO	         <- 2020
FUENTE	     <- "Encuesta Continua de Hogares 2023, INE"
RESPONSABLE	 <- "J. Pandolfi"
JERARQUIA_CAT <- 1

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


### Carga de bases ###


if (!dir.exists(here::here("data_ech"))) {dir.create(here::here("data_ech"))}

osfr::osf_retrieve_file("ydnsa") %>% 
  osfr::osf_download(here("data_ech"), conflicts = "overwrite")

df <- rio::import(here::here("data_ech", "HyP_2020_Terceros.RData"))

anio = 2020



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

### Generación de nuevas variables ###

# No asistencia a un centro educativo

df <- df %>% mutate(noasiste = case_when(e49_cv == 1 ~ 0,
                                         e49_cv == 2 ~ 1,
                                         e49_cv == 0 | e49_cv == 99 ~ NA))


#IPC

df <- df %>% dplyr::mutate(bc_ipc_tot = case_when(
  mes == 1  ~ 0.367126174,
  mes == 2  ~ 0.359598378,
  mes == 3  ~ 0.357408439,
  mes == 4  ~ 0.352706586,
  mes == 5  ~ 0.345801038,
  mes == 6  ~ 0.343854751,
  mes == 7  ~ 0.343791309,
  mes == 8  ~ 0.341914564,
  mes == 9  ~ 0.339965133,
  mes == 10 ~ 0.337807994,
  mes == 11 ~ 0.33585957,
  mes == 12 ~ 0.334908811))



# Ingresos

df <- df %>% dplyr::mutate(y_pc       =  HT11 / ht19 )                      #Ingreso per-cápita
df <- df %>% dplyr::mutate(y_pc_svl   =  (HT11 - ht13) / ht19)              #Ingreso per-cápita sin valor locativo
df <- df %>% dplyr::mutate(y_pc_d     =  HT11 / ht19 / bc_ipc_tot)          #Ingreso per-cápita deflactado
df <- df %>% dplyr::mutate(y_pc_svl_d =  (HT11 - ht13) / ht19 / bc_ipc_tot) #Ingreso per-cápita sin valor locativo deflactado

# Quintil de ingresos

base_h <- df %>% distinct(numero, .keep_all = TRUE)
base_h <- base_h %>% dplyr::mutate(quintilesy = statar::xtile(y_pc, n=5, wt = pesomen))
base_h <- base_h[,c("numero","quintilesy")]
df <- merge(df, base_h, by = "numero")



# Sexo

df <- df %>% dplyr::mutate(bc_pe2 = e26)


# Ascendencia afro

df <- df %>% dplyr::mutate(bd_e29_1 = e29_1)


# Tramo de edad


df <- df %>% dplyr::mutate(tramoed = case_when(e27 == 20 ~ 1,
                                               e27 >= 21 & e27 <= 23 ~ 2,
                                               e27 >= 24 ~ 3,
                                               TRUE ~ NA_real_))




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

### Información de muestreo ### 

base_svy <- srvyr::as_survey_design(df, ids = numero, weights = pesomen)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
### Procesamiento de indicadores ###
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de personas de 4 a 17 años que no asisten a centros educativos
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


# Total

c_ano <- base_svy %>%
  srvyr::filter(e27>= 4 & e27<=17 & mes>2 & is.na(noasiste)==F) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(noasiste))
c_ano <- mean(c_ano$colname)


# Ascendencia étnico racial

c_afro <- function(x) {
  x <- base_svy %>%
    filter(bd_e29_1 == x & e27>= 4 & e27<=17  & mes>2 & is.na(noasiste)==F) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(noasiste))
  x <- mean(x$colname)
}       

c_e_afro <- numeric()

for(i in 1:2){
  c_e_afro[i] <- c_afro(x = i)
}  



# Departamento

c_dpto <- function(x) {
  x <- base_svy %>%
    filter(dpto == x & e27>= 4 & e27<=17  & mes>2 & is.na(noasiste)==F) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(noasiste))
  x <- mean(x$colname)
}       

c_e_dpto <- numeric()

for(i in 1:19){
  c_e_dpto[i] <- c_dpto(x = i)
}         



# Quintil de ingresos

c_quintil <- function(x) {
  x <- base_svy %>%
    filter(quintilesy == x & e27>= 4 & e27<=17  & mes>2 & is.na(noasiste)==F) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(noasiste))
  x <- mean(x$colname)
}       

c_e_quintil <- numeric()

for(i in 1:5){
  c_e_quintil[i] <- c_quintil(x = i)
}         



# Sexo

c_sexo <- function(x) {
  x <- base_svy %>%
    filter(bc_pe2 == x & e27>= 4 & e27<=17  & mes>2 & is.na(noasiste)==F) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(noasiste))
  x <- mean(x$colname)
}       

c_e_sexo <- numeric()

for(i in 1:2){
  c_e_sexo[i] <- c_sexo(x = i)
}         



# No dan los casos para hacer corte por edad


CODIND       <- 130101
DIMENSIÓN    <- "Accesibilidad"
CODINDICADOR <- "No asistencia a centros educativos (4 a 17 años)"
NOMINDICADOR <- "Porcentaje de personas de 4 a 17 años que no asisten a centros educativos"
VALOR	       <- c(c_ano, c_e_afro, c_e_dpto, c_e_quintil, c_e_sexo)*100
JERARQUIA	   <- c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
CORTE        <- c("Total", "Ascendencia étnico-racial", "Ascendencia étnico-racial", "Departamento", "Departamento", "Departamento", "Departamento",
                  "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento",
                  "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Quintil de ingresos",
                  "Quintil de ingresos", "Quintil de ingresos", "Quintil de ingresos", "Quintil de ingresos", "Sexo", "Sexo")

DEPARTAMENTO <- c("","","", "Montevideo", "Artigas", "Canelones", "Cerro Largo", "Colonia", "Durazno", "Flores", "Florida", "Lavalleja", "Maldonado",
                  "Paysandú", "Río Negro", "Rivera", "Rocha", "Salto", "San José", "Soriano", "Tacuarembó", "Treinta y Tres", "", "", "", "","","","")

QUINTIL	      <- c("","","","","","","","","","","","","","","","","","","","","","","Quintil 1", "Quintil 2", "Quintil 3", "Quintil 4", "Quintil 5","","")

SEXO		      <- c("","","","","","","","","","","","","","","","","","","","","","","", "", "", "", "","Varones","Mujeres")

ASCENDENCIA   <- c("","Afrodescendiente","No afrodescendiente","","","","","","","","","","","","","","","","","","","","", "", "", "", "","","") 

EDAD		      <- ""

NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. Las estimaciones desde 2020 hasta mediados de 2021 se calculan a partir del formulario relefónico. A partir de julio de 2021, se considera el formulario de implantación. A su vez, respecto a la forma de preguntar asistencia a centros educativos, el INE realizó un cambio metodológico en el relevamiento. Anteriormente, se consultaba a las personas por la asistencia a cada nivel educativo. Se generaban, así, ocho variables de asistencia, una correspondiente a cada nivel.  El porcentaje de personas que no asisten era un indicador resumen de esta información.  A partir de 2020, se consulta a las personas si asisten a un establecimiento de enseñanaza de manera general, mediante una única pregunta. A partir de esta fecha, el porcentaje de no asistentes se calcula únicamente a partir de esta pregunta. Cabe considerar que durante 2020 y el primer semestre de 2021, esta pregunta se realiza únicamente a mayores de 3 años de edad"

edu_02 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, 
                JERARQUIA_CAT, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA, EDAD, NOTA_INDICADOR)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de personas de 18 a 24 años que no asisten a centros educativos
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


# Total

c_ano <- base_svy %>%
  srvyr::filter(e27 >= 18 & e27 <= 24 & mes>2 & is.na(noasiste)==F) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(noasiste))

c_ano <-mean(c_ano$colname)


# Ascendencia étnico racial

c_afro <- function(x) {
  x <- base_svy %>%
    filter(bd_e29_1 == x & e27 >= 18 & e27 <= 24 & mes>2 & is.na(noasiste)==F) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(noasiste))
  x <- mean(x$colname)
}       

c_e_afro <- numeric()

for(i in 1:2){
  c_e_afro[i] <- c_afro(x = i)
}  


# Departamento

c_dpto <- function(x) {
  x <- base_svy %>%
    filter(dpto == x & e27 >= 18 & e27 <= 24 & mes>2 & is.na(noasiste)==F) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(noasiste))
  x <- mean(x$colname)
}       

c_e_dpto <- numeric()

for(i in 1:19){
  c_e_dpto[i] <- c_dpto(x = i)
}         



# Quintil de ingresos

c_quintil <- function(x) {
  x <- base_svy %>%
    filter(quintilesy == x & e27 >= 18 & e27 <= 24 & mes>2 & is.na(noasiste)==F) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(noasiste))
  x <- mean(x$colname)
}       

c_e_quintil <- numeric()

for(i in 1:5){
  c_e_quintil[i] <- c_quintil(x = i)
}         



# Sexo

c_sexo <- function(x) {
  x <- base_svy %>%
    filter(bc_pe2 == x & e27 >= 18 & e27 <= 24 & mes>2 & is.na(noasiste)==F) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(noasiste))
  x <- mean(x$colname)
}       

c_e_sexo <- numeric()

for(i in 1:2){
  c_e_sexo[i] <- c_sexo(x = i)
}         


# No dan los casos para abrir por edad


CODIND       <- 130105
DIMENSIÓN    <- "Accesibilidad"
CODINDICADOR <- "No asistencia a centros educativos (18 a 24 años)"
NOMINDICADOR <- "Porcentaje de personas de 18 a 24 años que no asisten a centros educativos"
VALOR	       <- c(c_ano, c_e_afro, c_e_dpto, c_e_quintil, c_e_sexo)*100
JERARQUIA	   <- c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
JERARQUIA_CAT<- 1

CORTE        <- c("Total", "Ascendencia étnico-racial", "Ascendencia étnico-racial", "Departamento", "Departamento", "Departamento", "Departamento",
                  "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento",
                  "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Quintil de ingresos",
                  "Quintil de ingresos", "Quintil de ingresos", "Quintil de ingresos", "Quintil de ingresos", "Sexo", "Sexo")

DEPARTAMENTO <- c("","","", "Montevideo", "Artigas", "Canelones", "Cerro Largo", "Colonia", "Durazno", "Flores", "Florida", "Lavalleja", "Maldonado",
                  "Paysandú", "Río Negro", "Rivera", "Rocha", "Salto", "San José", "Soriano", "Tacuarembó", "Treinta y Tres","", "","","","", "","")

QUINTIL	      <- c("","","","","","","","","","","","","","","","","","","","","","","Quintil 1", "Quintil 2", "Quintil 3", "Quintil 4", "Quintil 5","","")

SEXO		      <- c("","","","","","","","","","","","","","","","","","","","","","","", "", "", "", "","Varones","Mujeres")

ASCENDENCIA   <- c("","Afrodescendiente","No afrodescendiente","","","","","", "", "","","","","","","","","","","", "", "", "", "", "","","","","") 

EDAD		      <- ""

NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. Las estimaciones desde 2020 hasta mediados de 2021 se calculan a partir del formulario relefónico. A partir de julio de 2021, se considera el formulario de implantación. A su vez, respecto a la forma de preguntar asistencia a centros educativos, el INE realizó un cambio metodológico en el relevamiento. Anteriormente, se consultaba a las personas por la asistencia a cada nivel educativo. Se generaban, así, ocho variables de asistencia, una correspondiente a cada nivel.  El porcentaje de personas que no asisten era un indicador resumen de esta información.  A partir de 2020, se consulta a las personas si asisten a un establecimiento de enseñanaza de manera general, mediante una única pregunta. A partir de esta fecha, el porcentaje de no asistentes se calcula únicamente a partir de esta pregunta. Cabe considerar que durante 2020 y el primer semestre de 2021, esta pregunta se realiza únicamente a mayores de 3 años de edad"

edu_06 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, 
                JERARQUIA_CAT, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA, EDAD, NOTA_INDICADOR)



edu_2020 <- rbind(edu_02, edu_06)


edu_2020 <- as.data.frame(edu_2020)

rio::export(edu_2020, "Tabulados educ 2020-2022/edu_2020.xlsx" )