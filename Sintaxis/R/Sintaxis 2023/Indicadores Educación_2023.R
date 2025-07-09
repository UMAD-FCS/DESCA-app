### Mirador DESCA
### Derecho a la educación
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


DERECHO      <- "Educación"
TIPOIND      <- "Resultados"
AÑO	         <- 2023
FUENTE	     <- "Encuesta Continua de Hogares 2023, INE"
RESPONSABLE	 <- "J. Pandolfi"
JERARQUIA_CAT <- 1

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


### Carga de bases ###

base          <- rio::import("ECH_2023.csv")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

### Generación de nuevas variables ###

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


# Ingresos

base <- base %>% dplyr::mutate(y_pc       =  HT11 / HT19 )                      #Ingreso per-cápita
base <- base %>% dplyr::mutate(y_pc_svl   =  (HT11 - HT13) / HT19)              #Ingreso per-cápita sin valor locativo
base <- base %>% dplyr::mutate(y_pc_d     =  HT11 / HT19 / bc_ipc_tot)          #Ingreso per-cápita deflactado
base <- base %>% dplyr::mutate(y_pc_svl_d =  (HT11 - HT13) / HT19 / bc_ipc_tot) #Ingreso per-cápita sin valor locativo deflactado


# Quintil de ingresos

base_h <- base %>% distinct(ID, .keep_all = TRUE)
base_h <- base_h %>% dplyr::mutate(quintilesy = statar::xtile(y_pc, n=5, wt = W_ANO))
base_h <- base_h[,c("ID","quintilesy")]
base <- merge(base, base_h, by = "ID")


# Sexo

base <- base %>% dplyr::mutate(bc_pe2 = e26)


# Ascendencia afro

base <- base %>% dplyr::mutate(bd_e29_1 = e29_1)


# Tramo de edad


base <- base %>% dplyr::mutate(tramoed = case_when(e27 == 20 ~ 1,
                                                   e27 >= 21 & e27 <= 23 ~ 2,
                                                   e27 >= 24 ~ 3,
                                                   TRUE ~ NA_real_))



# No asistencia a centros educativos

base <- base %>% dplyr::mutate(noasiste = ifelse(e49 == 1 | e49 == 2 , 1, 0))                            



# Nivel educativo (Iecon)


base <- base %>% dplyr::mutate(bc_pe11 = ifelse(e49==3, 1, 2)) #Asiste Iecon (adaptado)
base <- base %>% dplyr::mutate(bc_pe12 = ifelse(e49==3, -9, e49)) #Asistió Iecon (adaptado)


base <- base %>% dplyr::mutate(bc_nivel = case_when(bc_pe12==2 ~ 0, #Nunca asistió
                                                    (e51_2==0 & e51_3==0) | (e51_2==9) ~ 0, #No tiene años aprobados ni en primaria ni en educación inicial (se agrega variable años en ed. inicial y se quita pregunta de asistencia específica)
                                                    ((((e51_2>0 & e51_2<=6) | (e51_3>0 & e51_3<=6)) &  e51_4_a==0 & e51_4_b==0) | (e51_2==6 & e51_4_a==9 & e51_4_b==9)) ~ 1, #Primaria
                                                    (((e51_4_a>0 & e51_4_a<=3) | (e51_4_b>0 & e51_4_b<=3)) & (e51_8==0 & e51_9==0 & e51_10==0)) ~ 2, #Secundaria
                                                    (((e51_5>0 & e51_5<=3) | (e51_6>0 & e51_6<=3)) & (e51_8==0 & e51_9==0 & e51_10==0)) ~ 2,
                                                    (e51_6a>0 &  e51_6a<=10) | ((e51_6a!=0 & e51_4_a==0 & e51_4_b==0 & e51_5==0 & e51_6==0) & e51_8==0 & e51_9==0 & e51_10==0) ~ 3,  #UTU
                                                    (e51_8>0 & e51_8<=5) &  e51_9==0 &  e51_10==0 &  e51_11==0 ~ 4, #Magisterio o profesorado
                                                    (e51_9>0 & e51_9<=9) | (e51_10>0 & e51_10<=9) | (e51_11>0 & e51_11<=9) ~ 5, #Universidad o similar
                                                    TRUE ~ NA_real_ ))


# Años de educación

base <- base %>% dplyr::mutate(e51_5 = as.numeric(e51_5))
base <- base %>% dplyr::mutate(e51_6 = as.numeric(e51_6))

base <- base %>% dplyr::mutate(bc_edu_1 = case_when(bc_nivel == 5 & e51_11 == 9  & e51_9 >= e51_10 ~  12 + e51_9,
                                                    bc_nivel == 5 & e51_11 == 9  & e51_9 <  e51_10 ~  12 + e51_10,  
  
                                                    bc_nivel == 5 & e51_9  == 9 & (e218_1==1 | e221_1==1) ~ 15,
                                                    bc_nivel == 5 & e51_10 == 9 & (e218_1==1 | e221_1==1) ~ 16,
  
                                                    bc_nivel == 5 & e51_9  == 9 ~ 12,
                                                    bc_nivel == 5 & e51_10 == 9 ~ 12,
  
                                                    bc_nivel == 5 & e51_9  < 9  &  e51_9  > 0 ~ e51_9  + 12,  # Terciaria
                                                    bc_nivel == 5 & e51_10 < 9  &  e51_10 > 0 ~ e51_10 + 12,
                                                    bc_nivel == 5 & e51_11 < 9  &  e51_11 > 0 & e51_9 >= e51_10 ~ e51_11 + 12 + e51_9,
                                                    bc_nivel == 5 & e51_11 < 9  &  e51_11 > 0 & e51_9 < e51_10 ~  e51_11 + 12 + e51_10,
  
                                                    bc_nivel == 4 & e51_8 < 9  &  e51_8 > 0 ~ e51_8 + 12,  # Magisterio
                                                    bc_nivel == 4 & e51_8 == 9 ~ 12,
                                                    bc_nivel == 4 & e51_8 == 9 & e215_1 == 1 ~ 15,
  
                                                    bc_nivel == 3 & e51_6a > 0  & e51_6a != 9 ~ e51_6a*1, # UTU
  
                                                    bc_nivel == 2 & e51_4_a == 9 & e201_1a == 1 ~ 9,
                                                    bc_nivel == 2 & e51_4_b == 9 & e201_1b == 1 ~ 9,
  
                                                    bc_nivel == 2 & e51_4_a == 9 & e201_1a == 2 ~ 6,
                                                    bc_nivel == 2 & e51_4_b == 9 & e201_1b == 2 ~ 6,
  
                                                    bc_nivel == 2 & e51_5 == 9 & (e51_4_a == 3 | e51_4_b == 3) & e201_1c == 1  ~ 12,
                                                    bc_nivel == 2 & e51_6 == 9 & (e51_4_a == 3 | e51_4_b == 3) & e201_1d == 1  ~ 12,
  
  
                                                    bc_nivel == 2 & (e51_4_a > 0 | e51_4_b > 0) & e51_5 == 0 & e51_6 == 0 & (e201_1a == 1 | e201_1b == 1)   ~ 9,
                                                    bc_nivel == 2 & e51_5 == 9 & (e51_4_a == 3 | e51_4_b == 3)   ~ 9,
                                                    bc_nivel == 2 & e51_6 == 9 & (e51_4_a == 3 | e51_4_b == 3)   ~ 9,
  
  
                                                    bc_nivel == 2 & e51_5 >= e51_6 & e51_5 <= 3 & e51_5 > 0    ~ 9 + e51_5, # Educación media superior
                                                    bc_nivel == 2 & e51_5 > 0 & e51_5 != 9 & e51_6 == 9        ~ 9 + e51_5, 
                                                    bc_nivel == 2 & e51_6 >  e51_5 & e51_6 <= 3 & e51_6 > 0    ~ 9 + e51_6,
                                                    bc_nivel == 2 & e51_6 > 0 & e51_6 != 9 & e51_5 == 9        ~ 9 + e51_6, 
                                                    
                                                    bc_nivel == 2 & ((e51_4_a >= e51_4_b & e51_4_a <= 3 & e51_4_a > 0) | (e51_4_a > 0 & e51_4_a != 9 & e51_4_b == 9))  ~ 6 + e51_4_a, # Educación media básica
                                                    bc_nivel == 2 & ((e51_4_b >  e51_4_a & e51_4_b <= 3 & e51_4_b > 0) | (e51_4_b > 0 & e51_4_b != 9 & e51_4_a == 9))  ~ 6 + e51_4_b,

                                                    bc_nivel == 0 ~ 0, # Primaria
                                                    bc_nivel == 1 & ((e51_2 >= e51_3 & e51_2 != 9) | (e51_2 > 0 & e51_2 != 9 & e51_3 == 9)) ~ e51_2*1,
                                                    bc_nivel == 1 & ((e51_3 >  e51_2 & e51_3 != 9) | (e51_3 > 0 & e51_3 != 9 & e51_2 == 9)) ~ e51_3*1, 
                                                    
                                                    TRUE ~ NA_real_))

  
mayores <- filter(base, e27 >=24)                                                    
tabla = as.data.frame(table(mayores$bc_edu_1))
rio::export(tabla, "aniosed.xlsx")





# Expulsión muy temprana del sistema educativo

base <- base %>% dplyr::mutate(expulsiontemp  = case_when(bc_edu_1 < 9 & bc_pe11 == 2 ~ 1,
                                                          TRUE ~ 0))      


# Expulsión temprana del sistema educativo

base <- base %>% dplyr::mutate(expulsiontemp_ems  = case_when(bc_edu_1 < 12 & bc_pe11 == 2 ~ 1,
                                                          TRUE ~ 0))      


# No culminación de educación media básica 

base <- base %>% dplyr::mutate(noculmin  = case_when(bc_edu_1 < 9  ~ 1,
                                                     TRUE ~ 0))  


# No culminación de educación media superior

base <- base %>% dplyr::mutate(noculmin_ems  = case_when(bc_edu_1 < 12  ~ 1,
                                                     TRUE ~ 0)) 


# Analfabetismo

base <- base %>% dplyr::mutate(bc_analf = ifelse(e48 == 2, 1, 0))




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

### Información de muestreo ### 

base_svy <- srvyr::as_survey_design(base, ids = ID, weigHTs = W_ANO)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
### Procesamiento de indicadores ###
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de personas de 15 a 24 años que no sabe leer y escribir
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


# Total

c_ano <- base_svy %>%
  srvyr::filter(e27>= 15 & e27<=24) %>%
  srvyr::summarise(colname = srvyr::survey_mean(bc_analf))

c_ano <-c_ano$colname


CODIND       <- 130305
DIMENSIÓN    <- "Calidad"
CODINDICADOR <- "Analfabetismo (15 a 24 años)"
NOMINDICADOR <- "Porcentaje de personas de 15 a 24 años que no sabe leer y escribir"
VALOR	       <- c_ano*100
JERARQUIA	   <- 1
CORTE        <- "Total"
DEPARTAMENTO <- ""
QUINTIL	     <- ""
SEXO		     <- "" 
ASCENDENCIA  <- "" 
EDAD         <- "" 
NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. Durante el año 2020 y hasta julio del año 2021 se suspende el relevamiento de la información necesaria para construir indicadores relativos al nivel y la trayectoria educativa. A partir de esta fecha, las preguntas se relevan en el formulario presencial. Un conjunto importante de indicadores educativos tienen un efecto estacional, por lo que no se recomienda comparar los resultados del segundo semestre del 2021 con la información anual. Las estimaciones desde 2022 se calculan a partir de la muestra de implantación."

edu_01 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, 
                JERARQUIA_CAT, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA, EDAD, NOTA_INDICADOR)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de personas de 4 a 17 años que no asisten a centros educativos
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


# Total

c_ano <- base_svy %>%
  srvyr::filter(e27>= 4 & e27<=17) %>%
  srvyr::summarise(colname = srvyr::survey_mean(noasiste))

c_ano <-c_ano$colname


# Ascendencia étnico racial

c_afro <- function(x) {
  x <- base_svy %>%
    filter(bd_e29_1 == x & e27>= 4 & e27<=17) %>%
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
    filter(dpto == x & e27>= 4 & e27<=17) %>%
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
    filter(quintilesy == x & e27>= 4 & e27<=17) %>%
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
    filter(bc_pe2 == x & e27>= 4 & e27<=17) %>%
    srvyr::summarise(colname = srvyr::survey_mean(noasiste))
  x <- mean(x$colname)
}       

c_e_sexo <- numeric()

for(i in 1:2){
  c_e_sexo[i] <- c_sexo(x = i)
}         


# Edad

c_edad <- function(x) {
  x <- base_svy %>%
    filter(e27 == x & e27>= 4 & e27<=17) %>%
    srvyr::summarise(colname = srvyr::survey_mean(noasiste))
  x <- mean(x$colname)
}       

c_e_edad <- numeric()

for(i in 1:17){
  c_e_edad[i] <- c_edad(x = i)
}  

c_e_edad <- c_e_edad[c(4:17)]


CODIND       <- 130101
DIMENSIÓN    <- "Accesibilidad"
CODINDICADOR <- "No asistencia a centros educativos (4 a 17 años)"
NOMINDICADOR <- "Porcentaje de personas de 4 a 17 años que no asisten a centros educativos"
VALOR	       <- c(c_ano, c_e_afro, c_e_dpto, c_e_quintil, c_e_sexo, c_e_edad)*100
JERARQUIA	   <- c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
CORTE        <- c("Total", "Ascendencia étnico-racial", "Ascendencia étnico-racial", "Departamento", "Departamento", "Departamento", "Departamento",
                  "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento",
                  "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Quintil de ingresos",
                  "Quintil de ingresos", "Quintil de ingresos", "Quintil de ingresos", "Quintil de ingresos", "Sexo", "Sexo", "Edad", "Edad", "Edad", "Edad", "Edad", "Edad", "Edad", "Edad"
                  , "Edad", "Edad", "Edad", "Edad", "Edad", "Edad")

DEPARTAMENTO <- c("","","", "Montevideo", "Artigas", "Canelones", "Cerro Largo", "Colonia", "Durazno", "Flores", "Florida", "Lavalleja", "Maldonado",
                  "Paysandú", "Río Negro", "Rivera", "Rocha", "Salto", "San José", "Soriano", "Tacuarembó", "Treinta y Tres", "", "", "", "","","","","","","","","", "", "", "", "", "", "", "", "","")

QUINTIL	      <- c("","","","","","","","","","","","","","","","","","","","","","","Quintil 1", "Quintil 2", "Quintil 3", "Quintil 4", "Quintil 5","","","","","","","","","", "", "", "", "", "","","")

SEXO		      <- c("","","","","","","","","","","","","","","","","","","","","","","", "", "", "", "","Varones","Mujeres", "", "", "", "", "","","","","","","","","","")

ASCENDENCIA   <- c("","Afrodescendiente","No afrodescendiente","","","","","","","","","","","","","","","","","","","","", "", "", "", "","","","","","","","","","","", "", "", "", "", "","") 

EDAD		      <- c("","","","","","","","","","","","","","","","","","","","","","","", "", "", "", "","","", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17")

NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. Las estimaciones desde 2020 hasta mediados de 2021 se calculan a partir del formulario relefónico. A partir de julio de 2021, se considera el formulario de implantación. A su vez, respecto a la forma de preguntar asistencia a centros educativos, el INE realizó un cambio metodológico en el relevamiento. Anteriormente, se consultaba a las personas por la asistencia a cada nivel educativo. Se generaban, así, ocho variables de asistencia, una correspondiente a cada nivel.  El porcentaje de personas que no asisten era un indicador resumen de esta información.  A partir de 2020, se consulta a las personas si asisten a un establecimiento de enseñanaza de manera general, mediante una única pregunta. A partir de esta fecha, el porcentaje de no asistentes se calcula únicamente a partir de esta pregunta. Cabe considerar que durante 2020 y el primer semestre de 2021, esta pregunta se realiza únicamente a mayores de 3 años de edad. A partir del segundo semestre de 2021, se realiza a todas las personas." 


edu_02 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, 
                JERARQUIA_CAT, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA, EDAD, NOTA_INDICADOR)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de personas de 15 y 16 años que no asisten a centros educativos y no finalizaron educación media básica
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


# Total

c_ano <- base_svy %>%
  srvyr::filter(e27 >= 15 & e27 <= 16) %>%
  srvyr::summarise(colname = srvyr::survey_mean(expulsiontemp))

c_ano <-c_ano$colname


# Ascendencia étnico racial

c_afro <- function(x) {
  x <- base_svy %>%
    filter(bd_e29_1 == x & e27 >= 15 & e27 <= 16) %>%
    srvyr::summarise(colname = srvyr::survey_mean(expulsiontemp))
  x <- mean(x$colname)
}       

c_e_afro <- numeric()

for(i in 1:2){
  c_e_afro[i] <- c_afro(x = i)
}  


# Departamento

c_dpto <- function(x) {
  x <- base_svy %>%
    filter(dpto == x & e27 >= 15 & e27 <= 16) %>%
    srvyr::summarise(colname = srvyr::survey_mean(expulsiontemp))
  x <- mean(x$colname)
}       

c_e_dpto <- numeric()

for(i in 1:19){
  c_e_dpto[i] <- c_dpto(x = i)
}         



# Quintil de ingresos

c_quintil <- function(x) {
  x <- base_svy %>%
    filter(quintilesy == x & e27 >= 15 & e27 <= 16) %>%
    srvyr::summarise(colname = srvyr::survey_mean(expulsiontemp))
  x <- mean(x$colname)
}       

c_e_quintil <- numeric()

for(i in 1:5){
  c_e_quintil[i] <- c_quintil(x = i)
}         



# Sexo

c_sexo <- function(x) {
  x <- base_svy %>%
    filter(bc_pe2 == x & e27 >= 15 & e27 <= 16) %>%
    srvyr::summarise(colname = srvyr::survey_mean(expulsiontemp))
  x <- mean(x$colname)
}       

c_e_sexo <- numeric()

for(i in 1:2){
  c_e_sexo[i] <- c_sexo(x = i)
}         


# Edad

c_edad <- function(x) {
  x <- base_svy %>%
    filter(e27 == x & e27 >= 15 & e27 <= 16) %>%
    srvyr::summarise(colname = srvyr::survey_mean(expulsiontemp))
  x <- mean(x$colname)
}       

c_e_edad <- numeric()

for(i in 1:16){
  c_e_edad[i] <- c_edad(x = i)
}  

c_e_edad <- c_e_edad[c(15:16)]


CODIND       <- 130102
DIMENSIÓN    <- "Accesibilidad"
CODINDICADOR <- "Expulsión muy temprana del sistema educativo"
NOMINDICADOR <- "Porcentaje de personas de 15 y 16 años que no asisten a centros educativos y no finalizaron educación media básica"
VALOR	       <- c(c_ano, c_e_afro, c_e_dpto, c_e_quintil, c_e_sexo, c_e_edad)*100
JERARQUIA	   <- c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
CORTE        <- c("Total", "Ascendencia étnico-racial", "Ascendencia étnico-racial", "Departamento", "Departamento", "Departamento", "Departamento",
                  "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento",
                  "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Quintil de ingresos",
                  "Quintil de ingresos", "Quintil de ingresos", "Quintil de ingresos", "Quintil de ingresos", "Sexo", "Sexo", "Edad", "Edad")

DEPARTAMENTO <- c("","","", "Montevideo", "Artigas", "Canelones", "Cerro Largo", "Colonia", "Durazno", "Flores", "Florida", "Lavalleja", "Maldonado",
                  "Paysandú", "Río Negro", "Rivera", "Rocha", "Salto", "San José", "Soriano", "Tacuarembó", "Treinta y Tres", "", "", "", "","","","","","")

QUINTIL	      <- c("","","","","","","","","","","","","","","","","","","","","","","Quintil 1", "Quintil 2", "Quintil 3", "Quintil 4", "Quintil 5","","","","")

SEXO		      <- c("","","","","","","","","","","","","","","","","","","","","","","", "", "", "", "","Varones","Mujeres", "", "")

ASCENDENCIA   <- c("","Afrodescendiente","No afrodescendiente","","","","","","","","","","","","","","","","","","","","", "", "", "", "","","","","") 

EDAD		      <- c("","","","","","","","","","","","","","","","","","","","","","","", "", "", "", "","","", "15", "16")

NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. Durante el año 2020 y hasta julio del año 2021 se suspende el relevamiento de la información necesaria para construir indicadores relativos al nivel y la trayectoria educativa. A partir de esta fecha, las preguntas se relevan en el formulario presencial. Un conjunto importante de indicadores educativos tienen un efecto estacional, por lo que no se recomienda comparar los resultados del segundo semestre del 2021 con la información anual. Las estimaciones desde 2022 se calculan a partir de la muestra de implantación. Respecto a la forma de preguntar asistencia a centros educativos, el INE realizó un cambio metodológico en el relevamiento. Anteriormente, se consultaba a las personas por la asistencia a cada nivel educativo. Se generaban, así, ocho variables de asistencia, una correspondiente a cada nivel.  El porcentaje de personas que no asisten era un indicador resumen de esta información.  A partir de 2020, se consulta a las personas si asisten a un establecimiento de enseñanaza de manera general, mediante una única pregunta. A partir de esta fecha, el porcentaje de no asistentes se calcula únicamente a partir de esta pregunta. A su vez, se modifica la forma de relevamiento en la culminación de ciclos educativos. En particular, cambia el relevamiento de cantidad de años aprobados en UTU. Hasta el año 2019 se relevaban los años aprobados en bachillerato tecnológico y en educación técnica. En el segundo caso era posible distinguir el curso según la exigencia previa para cursarlo. A partir de julio de 2021 se distinguen los años de Educación Media Básica y Educación Media Superior, tanto de liceo como de CEPT-UTU. Además, se consulta de forma independiente los años realizados en cursos técnicos en CEPT-UTU. No se releva el nivel de exigencia previa para asistir a estos cursos." 


edu_03 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, 
                JERARQUIA_CAT, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA, EDAD, NOTA_INDICADOR)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de personas de 18 y 19 años que no asisten a centros educativos y no finalizaron educación media superior
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


# Total

c_ano <- base_svy %>%
  srvyr::filter(e27 >= 18 & e27 <= 19) %>%
  srvyr::summarise(colname = srvyr::survey_mean(expulsiontemp_ems))

c_ano <-c_ano$colname


# Ascendencia étnico racial

c_afro <- function(x) {
  x <- base_svy %>%
    filter(bd_e29_1 == x & e27 >= 18 & e27 <= 19) %>%
    srvyr::summarise(colname = srvyr::survey_mean(expulsiontemp_ems))
  x <- mean(x$colname)
}       

c_e_afro <- numeric()

for(i in 1:2){
  c_e_afro[i] <- c_afro(x = i)
}  


# Departamento

c_dpto <- function(x) {
  x <- base_svy %>%
    filter(dpto == x & e27 >= 18 & e27 <= 19) %>%
    srvyr::summarise(colname = srvyr::survey_mean(expulsiontemp_ems))
  x <- mean(x$colname)
}       

c_e_dpto <- numeric()

for(i in 1:19){
  c_e_dpto[i] <- c_dpto(x = i)
}         



# Quintil de ingresos

c_quintil <- function(x) {
  x <- base_svy %>%
    filter(quintilesy == x & e27 >= 18 & e27 <= 19) %>%
    srvyr::summarise(colname = srvyr::survey_mean(expulsiontemp_ems))
  x <- mean(x$colname)
}       

c_e_quintil <- numeric()

for(i in 1:5){
  c_e_quintil[i] <- c_quintil(x = i)
}         



# Sexo

c_sexo <- function(x) {
  x <- base_svy %>%
    filter(bc_pe2 == x & e27 >= 18 & e27 <= 19) %>%
    srvyr::summarise(colname = srvyr::survey_mean(expulsiontemp_ems))
  x <- mean(x$colname)
}       

c_e_sexo <- numeric()

for(i in 1:2){
  c_e_sexo[i] <- c_sexo(x = i)
}         


# Edad

c_edad <- function(x) {
  x <- base_svy %>%
    filter(e27 == x & e27 >= 18 & e27 <= 19) %>%
    srvyr::summarise(colname = srvyr::survey_mean(expulsiontemp_ems))
  x <- mean(x$colname)
}       

c_e_edad <- numeric()

for(i in 1:19){
  c_e_edad[i] <- c_edad(x = i)
}  

c_e_edad <- c_e_edad[c(18:19)]


CODIND       <- 130103
DIMENSIÓN    <- "Accesibilidad"
CODINDICADOR <- "Expulsión temprana del sistema educativo"
NOMINDICADOR <- "Porcentaje de personas de 18 y 19 años que no asisten a centros educativos y no finalizaron educación media superior"
VALOR	       <- c(c_ano, c_e_afro, c_e_dpto, c_e_quintil, c_e_sexo, c_e_edad)*100
JERARQUIA	   <- c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
CORTE        <- c("Total", "Ascendencia étnico-racial", "Ascendencia étnico-racial", "Departamento", "Departamento", "Departamento", "Departamento",
                  "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento",
                  "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Quintil de ingresos",
                  "Quintil de ingresos", "Quintil de ingresos", "Quintil de ingresos", "Quintil de ingresos", "Sexo", "Sexo", "Edad", "Edad")

DEPARTAMENTO <- c("","","", "Montevideo", "Artigas", "Canelones", "Cerro Largo", "Colonia", "Durazno", "Flores", "Florida", "Lavalleja", "Maldonado",
                  "Paysandú", "Río Negro", "Rivera", "Rocha", "Salto", "San José", "Soriano", "Tacuarembó", "Treinta y Tres", "", "", "", "","","","","","")

QUINTIL	      <- c("","","","","","","","","","","","","","","","","","","","","","","Quintil 1", "Quintil 2", "Quintil 3", "Quintil 4", "Quintil 5","","","","")

SEXO		      <- c("","","","","","","","","","","","","","","","","","","","","","","", "", "", "", "","Varones","Mujeres", "", "")

ASCENDENCIA   <- c("","Afrodescendiente","No afrodescendiente","","","","","","","","","","","","","","","","","","","","", "", "", "", "","","","","") 

EDAD		      <- c("","","","","","","","","","","","","","","","","","","","","","","", "", "", "", "","","", "18", "19")

NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. Durante el año 2020 y hasta julio del año 2021 se suspende el relevamiento de la información necesaria para construir indicadores relativos al nivel y la trayectoria educativa. A partir de esta fecha, las preguntas se relevan en el formulario presencial. Un conjunto importante de indicadores educativos tienen un efecto estacional, por lo que no se recomienda comparar los resultados del segundo semestre del 2021 con la información anual. Las estimaciones desde 2022 se calculan a partir de la muestra de implantación. Respecto a la forma de preguntar asistencia a centros educativos, el INE realizó un cambio metodológico en el relevamiento. Anteriormente, se consultaba a las personas por la asistencia a cada nivel educativo. Se generaban, así, ocho variables de asistencia, una correspondiente a cada nivel.  El porcentaje de personas que no asisten era un indicador resumen de esta información.  A partir de 2020, se consulta a las personas si asisten a un establecimiento de enseñanaza de manera general, mediante una única pregunta. A partir de esta fecha, el porcentaje de no asistentes se calcula únicamente a partir de esta pregunta. A su vez, se modifica la forma de relevamiento en la culminación de ciclos educativos. En particular, cambia el relevamiento de cantidad de años aprobados en UTU. Hasta el año 2019 se relevaban los años aprobados en bachillerato tecnológico y en educación técnica. En el segundo caso era posible distinguir el curso según la exigencia previa para cursarlo. A partir de julio de 2021 se distinguen los años de Educación Media Básica y Educación Media Superior, tanto de liceo como de CEPT-UTU. Además, se consulta de forma independiente los años realizados en cursos técnicos en CEPT-UTU. No se releva el nivel de exigencia previa para asistir a estos cursos." 


edu_04 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, 
                JERARQUIA_CAT, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA, EDAD, NOTA_INDICADOR)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de personas de 0 a 24 años que no asisten a centros educativos
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


# Total

c_ano <- base_svy %>%
  srvyr::filter(e27 >= 0 & e27 <= 24) %>%
  srvyr::summarise(colname = srvyr::survey_mean(noasiste))

c_ano <-c_ano$colname


# Ascendencia étnico racial

c_afro <- function(x) {
  x <- base_svy %>%
    filter(bd_e29_1 == x & e27 >= 0 & e27 <= 24) %>%
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
    filter(dpto == x & e27 >= 0 & e27 <= 24) %>%
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
    filter(quintilesy == x & e27 >= 0 & e27 <= 24) %>%
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
    filter(bc_pe2 == x & e27 >= 0 & e27 <= 24) %>%
    srvyr::summarise(colname = srvyr::survey_mean(noasiste))
  x <- mean(x$colname)
}       

c_e_sexo <- numeric()

for(i in 1:2){
  c_e_sexo[i] <- c_sexo(x = i)
}         


# Edad

c_edad <- function(x) {
  x <- base_svy %>%
    filter(e27 == x & e27 >= 0 & e27 <= 24) %>%
    srvyr::summarise(colname = srvyr::survey_mean(noasiste))
  x <- mean(x$colname)
}       

c_e_edad <- numeric()

for(i in 0:24){
  c_e_edad[i] <- c_edad(x = i)
}  

c_edad_0 <- base_svy %>%
    filter(e27 == 0) %>%
    srvyr::summarise(colname = srvyr::survey_mean(noasiste))
c_edad_0 <- mean(c_edad_0$colname)



CODIND       <- 130104
DIMENSIÓN    <- "Accesibilidad"
CODINDICADOR <- "No asistencia a centros educativos (0 a 24 años)"
NOMINDICADOR <- "Porcentaje de personas de 0 a 24 años que no asisten a centros educativos"
VALOR	       <- c(c_ano, c_e_afro, c_e_dpto, c_e_quintil, c_e_sexo, c_edad_0, c_e_edad)*100
JERARQUIA	   <- c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
JERARQUIA_CAT<- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0 ,1)

CORTE        <- c("Total", "Ascendencia étnico-racial", "Ascendencia étnico-racial", "Departamento", "Departamento", "Departamento", "Departamento",
                  "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento",
                  "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Quintil de ingresos",
                  "Quintil de ingresos", "Quintil de ingresos", "Quintil de ingresos", "Quintil de ingresos", "Sexo", "Sexo", "Edad", "Edad", "Edad", "Edad", "Edad", "Edad", "Edad", "Edad"
                  , "Edad", "Edad", "Edad", "Edad", "Edad", "Edad", "Edad", "Edad", "Edad", "Edad", "Edad", "Edad", "Edad", "Edad", "Edad", "Edad", "Edad")

DEPARTAMENTO <- c("","","", "Montevideo", "Artigas", "Canelones", "Cerro Largo", "Colonia", "Durazno", "Flores", "Florida", "Lavalleja", "Maldonado",
                  "Paysandú", "Río Negro", "Rivera", "Rocha", "Salto", "San José", "Soriano", "Tacuarembó", "Treinta y Tres","", "","","","","","","","","","","", "", "", "","","","","","","","","", "", "", "", "", "", "", "", "","")

QUINTIL	      <- c("","","","","","","","","","","","","","","","","","","","","","","Quintil 1", "Quintil 2", "Quintil 3", "Quintil 4", "Quintil 5","","","","","","","","","","","","","","","","","","","","", "", "", "", "", "","","")

SEXO		      <- c("","","","","","","","","","","","","","","","","","","","","","","", "", "", "", "","Varones","Mujeres","", "", "", "", "", "","","","","","","","","","","","","","","","","","","","")

ASCENDENCIA   <- c("","Afrodescendiente","No afrodescendiente","","","","","","","","","","","","","","","","","","","","","", "", "", "", "","","","","","","","","","","", "", "", "", "", "","","","","","","","","","","","") 

EDAD		      <- c("","","","","","","","","","","","","","","","","","","","","","","", "", "", "", "","","", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24")

NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. Respecto a la forma de preguntar asistencia a centros educativos, el INE realizó un cambio metodológico en el relevamiento. Anteriormente, se consultaba a las personas por la asistencia a cada nivel educativo. Se generaban, así, ocho variables de asistencia, una correspondiente a cada nivel.  El porcentaje de personas que no asisten era un indicador resumen de esta información.  A partir de 2020, se consulta a las personas si asisten a un establecimiento de enseñanaza de manera general, mediante una única pregunta. A partir de esta fecha, el porcentaje de no asistentes se calcula únicamente a partir de esta pregunta. Cabe considerar que durante 2020 y el primer semestre de 2021, esta pregunta se realiza únicamente a mayores de 3 años de edad. A partir del segundo semestre de 2021, se realiza a todas las personas. Por lo tanto, se presentan las estimaciones a partir de 2022, considerando el formulario de implantación." 


edu_05 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, 
                JERARQUIA_CAT, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA, EDAD, NOTA_INDICADOR)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de personas de 18 a 24 años que no asisten a centros educativos
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


# Total

c_ano <- base_svy %>%
  srvyr::filter(e27 >= 18 & e27 <= 24) %>%
  srvyr::summarise(colname = srvyr::survey_mean(noasiste))

c_ano <-c_ano$colname


# Ascendencia étnico racial

c_afro <- function(x) {
  x <- base_svy %>%
    filter(bd_e29_1 == x & e27 >= 18 & e27 <= 24) %>%
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
    filter(dpto == x & e27 >= 18 & e27 <= 24) %>%
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
    filter(quintilesy == x & e27 >= 18 & e27 <= 24) %>%
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
    filter(bc_pe2 == x & e27 >= 18 & e27 <= 24) %>%
    srvyr::summarise(colname = srvyr::survey_mean(noasiste))
  x <- mean(x$colname)
}       

c_e_sexo <- numeric()

for(i in 1:2){
  c_e_sexo[i] <- c_sexo(x = i)
}         


# Edad

c_edad <- function(x) {
  x <- base_svy %>%
    filter(e27 == x & e27 >= 18 & e27 <= 24) %>%
    srvyr::summarise(colname = srvyr::survey_mean(noasiste))
  x <- mean(x$colname)
}       

c_e_edad <- numeric()

for(i in 1:24){
  c_e_edad[i] <- c_edad(x = i)
}  

c_e_edad <- c_e_edad[18:24]



CODIND       <- 130105
DIMENSIÓN    <- "Accesibilidad"
CODINDICADOR <- "No asistencia a centros educativos (18 a 24 años)"
NOMINDICADOR <- "Porcentaje de personas de 18 a 24 años que no asisten a centros educativos"
VALOR	       <- c(c_ano, c_e_afro, c_e_dpto, c_e_quintil, c_e_sexo, c_e_edad)*100
JERARQUIA	   <- c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
JERARQUIA_CAT<- 1

CORTE        <- c("Total", "Ascendencia étnico-racial", "Ascendencia étnico-racial", "Departamento", "Departamento", "Departamento", "Departamento",
                  "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento",
                  "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Quintil de ingresos",
                  "Quintil de ingresos", "Quintil de ingresos", "Quintil de ingresos", "Quintil de ingresos", "Sexo", "Sexo", "Edad", "Edad", "Edad", "Edad", "Edad", "Edad", "Edad")

DEPARTAMENTO <- c("","","", "Montevideo", "Artigas", "Canelones", "Cerro Largo", "Colonia", "Durazno", "Flores", "Florida", "Lavalleja", "Maldonado",
                  "Paysandú", "Río Negro", "Rivera", "Rocha", "Salto", "San José", "Soriano", "Tacuarembó", "Treinta y Tres","", "","","","", "","","","","", "", "", "", "")

QUINTIL	      <- c("","","","","","","","","","","","","","","","","","","","","","","Quintil 1", "Quintil 2", "Quintil 3", "Quintil 4", "Quintil 5","","","","","","","","","")

SEXO		      <- c("","","","","","","","","","","","","","","","","","","","","","","", "", "", "", "","Varones","Mujeres","", "", "", "", "", "","")

ASCENDENCIA   <- c("","Afrodescendiente","No afrodescendiente","","","","","", "", "","","","","","","","","","","", "", "", "", "", "","","","","","","","","","","","") 

EDAD		      <- c("","","","","","","","","","","","","","","","","","","","","","","", "", "", "", "","","", "18", "19", "20", "21", "22", "23", "24")

NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. Las estimaciones desde 2020 hasta mediados de 2021 se calculan a partir del formulario relefónico. A partir de julio de 2021, se considera el formulario de implantación. A su vez, respecto a la forma de preguntar asistencia a centros educativos, el INE realizó un cambio metodológico en el relevamiento. Anteriormente, se consultaba a las personas por la asistencia a cada nivel educativo. Se generaban, así, ocho variables de asistencia, una correspondiente a cada nivel.  El porcentaje de personas que no asisten era un indicador resumen de esta información.  A partir de 2020, se consulta a las personas si asisten a un establecimiento de enseñanaza de manera general, mediante una única pregunta. A partir de esta fecha, el porcentaje de no asistentes se calcula únicamente a partir de esta pregunta. Cabe considerar que durante 2020 y el primer semestre de 2021, esta pregunta se realiza únicamente a mayores de 3 años de edad. A partir del segundo semestre de 2021, se realiza a todas las personas." 


edu_06 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, 
                JERARQUIA_CAT, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA, EDAD, NOTA_INDICADOR)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de personas de 0 a 3 años que no asisten a centros educativos
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


# Total

c_ano <- base_svy %>%
  srvyr::filter(e27 >= 0 & e27 <= 3) %>%
  srvyr::summarise(colname = srvyr::survey_mean(noasiste))

c_ano <-c_ano$colname


# Ascendencia étnico racial

c_afro <- function(x) {
  x <- base_svy %>%
    filter(bd_e29_1 == x & e27 >= 0 & e27 <= 3) %>%
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
    filter(dpto == x & e27 >= 0 & e27 <= 3) %>%
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
    filter(quintilesy == x & e27 >= 0 & e27 <= 3) %>%
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
    filter(bc_pe2 == x & e27 >= 0 & e27 <= 3) %>%
    srvyr::summarise(colname = srvyr::survey_mean(noasiste))
  x <- mean(x$colname)
}       

c_e_sexo <- numeric()

for(i in 1:2){
  c_e_sexo[i] <- c_sexo(x = i)
}         


# Edad

c_edad <- function(x) {
  x <- base_svy %>%
    filter(e27 == x & e27 >= 0 & e27 <= 3) %>%
    srvyr::summarise(colname = srvyr::survey_mean(noasiste))
  x <- mean(x$colname)
}       

c_e_edad <- numeric()

for(i in 1:3){
  c_e_edad[i] <- c_edad(x = i)
}  

c_e_edad <- c_e_edad[1:3]

c_edad_0 <- base_svy %>%
  filter(e27 == 0) %>%
  srvyr::summarise(colname = srvyr::survey_mean(noasiste))
c_edad_0 <- mean(c_edad_0$colname)

CODIND       <- 130106
DIMENSIÓN    <- "Accesibilidad"
CODINDICADOR <- "No asistencia a centros educativos (0 a 3 años)"
NOMINDICADOR <- "Porcentaje de personas de 0 a 3 años que no asisten a centros educativos"
VALOR	       <- c(c_ano, c_e_afro, c_e_dpto, c_e_quintil, c_e_sexo, c_edad_0, c_e_edad)*100
JERARQUIA	   <- c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
JERARQUIA_CAT<- 1

CORTE        <- c("Total", "Ascendencia étnico-racial", "Ascendencia étnico-racial", "Departamento", "Departamento", "Departamento", "Departamento",
                  "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento",
                  "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Quintil de ingresos",
                  "Quintil de ingresos", "Quintil de ingresos", "Quintil de ingresos", "Quintil de ingresos", "Sexo", "Sexo", "Edad", "Edad", "Edad", "Edad")

DEPARTAMENTO <- c("","","", "Montevideo", "Artigas", "Canelones", "Cerro Largo", "Colonia", "Durazno", "Flores", "Florida", "Lavalleja", "Maldonado",
                  "Paysandú", "Río Negro", "Rivera", "Rocha", "Salto", "San José", "Soriano", "Tacuarembó", "Treinta y Tres","","", "","","","","", "", "", "", "")

QUINTIL	      <- c("","","","","","","","","","","","","","","","","","","","","","","Quintil 1", "Quintil 2", "Quintil 3", "Quintil 4", "Quintil 5","","","","","","")

SEXO		      <- c("","","","","","","","","","","","","","","","","","","","","","","", "", "", "", "","Varones","Mujeres","", "", "", "")

ASCENDENCIA   <- c("","Afrodescendiente","No afrodescendiente","","","","","", "", "","","","","","","","","","","", "", "", "", "", "","","","","","","","","") 

EDAD		      <- c("","","","","","","","","","","","","","","","","","","","","","","", "", "", "", "","","", "0", "1", "2", "3")

NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. Respecto a la forma de preguntar asistencia a centros educativos, el INE realizó un cambio metodológico en el relevamiento. Anteriormente, se consultaba a las personas por la asistencia a cada nivel educativo. Se generaban, así, ocho variables de asistencia, una correspondiente a cada nivel.  El porcentaje de personas que no asisten era un indicador resumen de esta información.  A partir de 2020, se consulta a las personas si asisten a un establecimiento de enseñanaza de manera general, mediante una única pregunta. A partir de esta fecha, el porcentaje de no asistentes se calcula únicamente a partir de esta pregunta. Cabe considerar que durante 2020 y el primer semestre de 2021, esta pregunta se realiza únicamente a mayores de 3 años de edad. A partir del segundo semestre de 2021, se realiza a todas las personas. Por lo tanto, se presentan las estimaciones a partir de 2022, considerando el formulario de implantación. " 


edu_07 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, 
                JERARQUIA_CAT, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA, EDAD, NOTA_INDICADOR)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de personas de 21 a 23 años que no culminaron educación media básica
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


# Total

c_ano <- base_svy %>%
  srvyr::filter(e27 >= 21 & e27 <= 23) %>%
  srvyr::summarise(colname = srvyr::survey_mean(noculmin))

c_ano <-c_ano$colname


# Ascendencia étnico racial

c_afro <- function(x) {
  x <- base_svy %>%
    filter(bd_e29_1 == x & e27 >= 21 & e27 <= 23) %>%
    srvyr::summarise(colname = srvyr::survey_mean(noculmin))
  x <- mean(x$colname)
}       

c_e_afro <- numeric()

for(i in 1:2){
  c_e_afro[i] <- c_afro(x = i)
}  


# Departamento

c_dpto <- function(x) {
  x <- base_svy %>%
    filter(dpto == x & e27 >= 21 & e27 <= 23) %>%
    srvyr::summarise(colname = srvyr::survey_mean(noculmin))
  x <- mean(x$colname)
}       

c_e_dpto <- numeric()

for(i in 1:19){
  c_e_dpto[i] <- c_dpto(x = i)
}         



# Quintil de ingresos

c_quintil <- function(x) {
  x <- base_svy %>%
    filter(quintilesy == x & e27 >= 21 & e27 <= 23) %>%
    srvyr::summarise(colname = srvyr::survey_mean(noculmin))
  x <- mean(x$colname)
}       

c_e_quintil <- numeric()

for(i in 1:5){
  c_e_quintil[i] <- c_quintil(x = i)
}         



# Sexo

c_sexo <- function(x) {
  x <- base_svy %>%
    filter(bc_pe2 == x & e27 >= 21 & e27 <= 23) %>%
    srvyr::summarise(colname = srvyr::survey_mean(noculmin))
  x <- mean(x$colname)
}       

c_e_sexo <- numeric()

for(i in 1:2){
  c_e_sexo[i] <- c_sexo(x = i)
}         


# Edad

c_edad <- function(x) {
  x <- base_svy %>%
    filter(e27 == x & e27 >= 21 & e27 <= 23) %>%
    srvyr::summarise(colname = srvyr::survey_mean(noculmin))
  x <- mean(x$colname)
}       

c_e_edad <- numeric()

for(i in 21:23){
  c_e_edad[i] <- c_edad(x = i)
}  

c_e_edad <- c_e_edad[c(21:23)]


CODIND       <- 130107
DIMENSIÓN    <- "Accesibilidad"
CODINDICADOR <- "No culminación de educación media básica (21 a 23 años)"
NOMINDICADOR <- "Porcentaje de personas de 21 a 23 años que no culminaron educación media básica"
VALOR	       <- c(c_ano, c_e_afro, c_e_dpto, c_e_quintil, c_e_sexo, c_e_edad)*100
JERARQUIA	   <- c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
CORTE        <- c("Total", "Ascendencia étnico-racial", "Ascendencia étnico-racial", "Departamento", "Departamento", "Departamento", "Departamento",
                  "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento",
                  "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Quintil de ingresos",
                  "Quintil de ingresos", "Quintil de ingresos", "Quintil de ingresos", "Quintil de ingresos", "Sexo", "Sexo", "Edad", "Edad", "Edad")

DEPARTAMENTO <- c("","","", "Montevideo", "Artigas", "Canelones", "Cerro Largo", "Colonia", "Durazno", "Flores", "Florida", "Lavalleja", "Maldonado",
                  "Paysandú", "Río Negro", "Rivera", "Rocha", "Salto", "San José", "Soriano", "Tacuarembó", "Treinta y Tres", "", "", "","", "","","","","","")

QUINTIL	      <- c("","","","","","","","","","","","","","","","","","","","","","","Quintil 1", "Quintil 2", "Quintil 3", "Quintil 4", "Quintil 5","","","","","")

SEXO		      <- c("","","","","","","","","","","","","","","","","","","","","","","", "", "", "", "","Varones","Mujeres","", "", "")

ASCENDENCIA   <- c("","Afrodescendiente","No afrodescendiente","","","","","","","","","","","","","","","","","","","","", "","", "", "", "","","","","") 

EDAD		      <- c("","","","","","","","","","","","","","","","","","","","","","","", "", "", "", "","","", "21", "22", "23")

NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. Durante el año 2020 y hasta julio del año 2021 se suspende el relevamiento de la información necesaria para construir indicadores relativos al nivel y la trayectoria educativa. A partir de esta fecha, las preguntas se relevan en el formulario presencial. Un conjunto importante de indicadores educativos tienen un efecto estacional, por lo que no se recomienda comparar los resultados del segundo semestre del 2021 con la información anual. Las estimaciones desde 2022 se calculan a partir de la muestra de implantación. A su vez, se modifica la forma de relevamiento en la culminación de ciclos educativos. En particular, cambia el relevamiento de cantidad de años aprobados en UTU. Hasta el año 2019 se relevaban los años aprobados en bachillerato tecnológico y en educación técnica. En el segundo caso era posible distinguir el curso según la exigencia previa para cursarlo. A partir de julio de 2021 se distinguen los años de Educación Media Básica y Educación Media Superior, tanto de liceo como de CEPT-UTU. Además, se consulta de forma independiente los años realizados en cursos técnicos en CEPT-UTU. No se releva el nivel de exigencia previa para asistir a estos cursos." 



edu_08 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, 
                JERARQUIA_CAT, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA, EDAD, NOTA_INDICADOR)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# No culminación de educación media superior (21 a 23 años)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


# Total

c_ano <- base_svy %>%
  srvyr::filter(e27 >= 21 & e27 <= 23) %>%
  srvyr::summarise(colname = srvyr::survey_mean(noculmin_ems))

c_ano <-c_ano$colname


# Ascendencia étnico racial

c_afro <- function(x) {
  x <- base_svy %>%
    filter(bd_e29_1 == x & e27 >= 21 & e27 <= 23) %>%
    srvyr::summarise(colname = srvyr::survey_mean(noculmin_ems))
  x <- mean(x$colname)
}       

c_e_afro <- numeric()

for(i in 1:2){
  c_e_afro[i] <- c_afro(x = i)
}  


# Departamento

c_dpto <- function(x) {
  x <- base_svy %>%
    filter(dpto == x & e27 >= 21 & e27 <= 23) %>%
    srvyr::summarise(colname = srvyr::survey_mean(noculmin_ems))
  x <- mean(x$colname)
}       

c_e_dpto <- numeric()

for(i in 1:19){
  c_e_dpto[i] <- c_dpto(x = i)
}         



# Quintil de ingresos

c_quintil <- function(x) {
  x <- base_svy %>%
    filter(quintilesy == x & e27 >= 21 & e27 <= 23) %>%
    srvyr::summarise(colname = srvyr::survey_mean(noculmin_ems))
  x <- mean(x$colname)
}       

c_e_quintil <- numeric()

for(i in 1:5){
  c_e_quintil[i] <- c_quintil(x = i)
}         



# Sexo

c_sexo <- function(x) {
  x <- base_svy %>%
    filter(bc_pe2 == x & e27 >= 21 & e27 <= 23) %>%
    srvyr::summarise(colname = srvyr::survey_mean(noculmin_ems))
  x <- mean(x$colname)
}       

c_e_sexo <- numeric()

for(i in 1:2){
  c_e_sexo[i] <- c_sexo(x = i)
}         


# Edad

c_edad <- function(x) {
  x <- base_svy %>%
    filter(e27 == x & e27 >= 21 & e27 <= 23) %>%
    srvyr::summarise(colname = srvyr::survey_mean(noculmin_ems))
  x <- mean(x$colname)
}       

c_e_edad <- numeric()

for(i in 21:23){
  c_e_edad[i] <- c_edad(x = i)
}  

c_e_edad <- c_e_edad[c(21:23)]


CODIND       <- 130108
DIMENSIÓN    <- "Accesibilidad"
CODINDICADOR <- "No culminación de educación media superior (21 a 23 años)"
NOMINDICADOR <- "Porcentaje de personas de 21 a 23 años que no culminaron educación media superior"
VALOR	       <- c(c_ano, c_e_afro, c_e_dpto, c_e_quintil, c_e_sexo, c_e_edad)*100
JERARQUIA	   <- c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
CORTE        <- c("Total", "Ascendencia étnico-racial", "Ascendencia étnico-racial", "Departamento", "Departamento", "Departamento", "Departamento",
                  "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento",
                  "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Quintil de ingresos",
                  "Quintil de ingresos", "Quintil de ingresos", "Quintil de ingresos", "Quintil de ingresos", "Sexo", "Sexo", "Edad", "Edad", "Edad")

DEPARTAMENTO <- c("","","", "Montevideo", "Artigas", "Canelones", "Cerro Largo", "Colonia", "Durazno", "Flores", "Florida", "Lavalleja", "Maldonado",
                  "Paysandú", "Río Negro", "Rivera", "Rocha", "Salto", "San José", "Soriano", "Tacuarembó", "Treinta y Tres", "", "", "","", "","","","","","")

QUINTIL	      <- c("","","","","","","","","","","","","","","","","","","","","","","Quintil 1", "Quintil 2", "Quintil 3", "Quintil 4", "Quintil 5","","","","","")

SEXO		      <- c("","","","","","","","","","","","","","","","","","","","","","","", "", "", "", "","Varones","Mujeres","", "", "")

ASCENDENCIA   <- c("","Afrodescendiente","No afrodescendiente","","","","","","","","","","","","","","","","","","","","", "","", "", "", "","","","","") 

EDAD		      <- c("","","","","","","","","","","","","","","","","","","","","","","", "", "", "", "","","", "21", "22", "23")

NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. Durante el año 2020 y hasta julio del año 2021 se suspende el relevamiento de la información necesaria para construir indicadores relativos al nivel y la trayectoria educativa. A partir de esta fecha, las preguntas se relevan en el formulario presencial. Un conjunto importante de indicadores educativos tienen un efecto estacional, por lo que no se recomienda comparar los resultados del segundo semestre del 2021 con la información anual. Las estimaciones desde 2022 se calculan a partir de la muestra de implantación. A su vez, se modifica la forma de relevamiento en la culminación de ciclos educativos. En particular, cambia el relevamiento de cantidad de años aprobados en UTU. Hasta el año 2019 se relevaban los años aprobados en bachillerato tecnológico y en educación técnica. En el segundo caso era posible distinguir el curso según la exigencia previa para cursarlo. A partir de julio de 2021 se distinguen los años de Educación Media Básica y Educación Media Superior, tanto de liceo como de CEPT-UTU. Además, se consulta de forma independiente los años realizados en cursos técnicos en CEPT-UTU. No se releva el nivel de exigencia previa para asistir a estos cursos." 



edu_09 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, 
                JERARQUIA_CAT, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA, EDAD, NOTA_INDICADOR)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Porcentaje de personas de 20 años o más que no culminaron educación media superior
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


# Total

c_ano <- base_svy %>%
  srvyr::filter(e27 >= 20) %>%
  srvyr::summarise(colname = srvyr::survey_mean(noculmin_ems))

c_ano <-c_ano$colname


# Ascendencia étnico racial

c_afro <- function(x) {
  x <- base_svy %>%
    filter(bd_e29_1 == x & e27 >= 20) %>%
    srvyr::summarise(colname = srvyr::survey_mean(noculmin_ems))
  x <- mean(x$colname)
}       

c_e_afro <- numeric()

for(i in 1:2){
  c_e_afro[i] <- c_afro(x = i)
}  


# Departamento

c_dpto <- function(x) {
  x <- base_svy %>%
    filter(dpto == x & e27 >= 20) %>%
    srvyr::summarise(colname = srvyr::survey_mean(noculmin_ems))
  x <- mean(x$colname)
}       

c_e_dpto <- numeric()

for(i in 1:19){
  c_e_dpto[i] <- c_dpto(x = i)
}         



# Quintil de ingresos

c_quintil <- function(x) {
  x <- base_svy %>%
    filter(quintilesy == x & e27 >= 20) %>%
    srvyr::summarise(colname = srvyr::survey_mean(noculmin_ems))
  x <- mean(x$colname)
}       

c_e_quintil <- numeric()

for(i in 1:5){
  c_e_quintil[i] <- c_quintil(x = i)
}         



# Sexo

c_sexo <- function(x) {
  x <- base_svy %>%
    filter(bc_pe2 == x & e27 >= 20) %>%
    srvyr::summarise(colname = srvyr::survey_mean(noculmin_ems))
  x <- mean(x$colname)
}       

c_e_sexo <- numeric()

for(i in 1:2){
  c_e_sexo[i] <- c_sexo(x = i)
}         


# Edad

c_edad <- function(x) {
  x <- base_svy %>%
    filter(tramoed == x & e27 >= 20) %>%
    srvyr::summarise(colname = srvyr::survey_mean(noculmin_ems))
  x <- mean(x$colname)
}       

c_e_edad <- numeric()

for(i in 1:3){
  c_e_edad[i] <- c_edad(x = i)
}  



CODIND       <- 130306
DIMENSIÓN    <- "Accesibilidad"
CODINDICADOR <- "No culminación de educación media superior (mayores de 20 años)"
NOMINDICADOR <- "Porcentaje de personas de 20 años o más que no culminaron educación media superior"
VALOR	       <- c(c_ano, c_e_afro, c_e_dpto, c_e_quintil, c_e_sexo, c_e_edad)*100
JERARQUIA	   <- c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
CORTE        <- c("Total", "Ascendencia étnico-racial", "Ascendencia étnico-racial", "Departamento", "Departamento", "Departamento", "Departamento",
                  "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento",
                  "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Quintil de ingresos",
                  "Quintil de ingresos", "Quintil de ingresos", "Quintil de ingresos", "Quintil de ingresos", "Sexo", "Sexo", "Edad", "Edad", "Edad")

DEPARTAMENTO <- c("","","", "Montevideo", "Artigas", "Canelones", "Cerro Largo", "Colonia", "Durazno", "Flores", "Florida", "Lavalleja", "Maldonado",
                  "Paysandú", "Río Negro", "Rivera", "Rocha", "Salto", "San José", "Soriano", "Tacuarembó", "Treinta y Tres", "", "", "","", "","","","","","")

QUINTIL	      <- c("","","","","","","","","","","","","","","","","","","","","","","Quintil 1", "Quintil 2", "Quintil 3", "Quintil 4", "Quintil 5","","","","","")

SEXO		      <- c("","","","","","","","","","","","","","","","","","","","","","","", "", "", "", "","Varones","Mujeres","", "", "")

ASCENDENCIA   <- c("","Afrodescendiente","No afrodescendiente","","","","","","","","","","","","","","","","","","","","", "","", "", "", "","","","","") 

EDAD		      <- c("","","","","","","","","","","","","","","","","","","","","","","", "", "", "", "","","", "20", "21 a 23 años", "24 años o más")

NOTA_INDICADOR <- "Desde marzo de 2020 hasta junio de 2021 se interrumpió el relevamiento presencial y se aplicó de manera telefónica un cuestionario restringido con el objetivo de continuar publicando los indicadores de ingresos y mercado de trabajo. En ese período la encuesta pasó a ser de paneles rotativos elegidos al azar a partir de los casos respondentes del año anterior. En julio de 2021 el INE retomó la realización de encuestas presenciales, pero introdujo un cambio metodológico, ya que la ECH pasa a ser una encuesta de panel rotativo con periodicidad mensual compuesta por seis paneles o grupos de rotación, cada uno de los cuales es una muestra representativa de la población. Con esta nueva metodología, cada hogar seleccionado participa durante seis meses de la ECH. Durante el año 2020 y hasta julio del año 2021 se suspende el relevamiento de la información necesaria para construir indicadores relativos al nivel y la trayectoria educativa. A partir de esta fecha, las preguntas se relevan en el formulario presencial. Un conjunto importante de indicadores educativos tienen un efecto estacional, por lo que no se recomienda comparar los resultados del segundo semestre del 2021 con la información anual. Las estimaciones desde 2022 se calculan a partir de la muestra de implantación. A su vez, se modifica la forma de relevamiento en la culminación de ciclos educativos. En particular, cambia el relevamiento de cantidad de años aprobados en UTU. Hasta el año 2019 se relevaban los años aprobados en bachillerato tecnológico y en educación técnica. En el segundo caso era posible distinguir el curso según la exigencia previa para cursarlo. A partir de julio de 2021 se distinguen los años de Educación Media Básica y Educación Media Superior, tanto de liceo como de CEPT-UTU. Además, se consulta de forma independiente los años realizados en cursos técnicos en CEPT-UTU. No se releva el nivel de exigencia previa para asistir a estos cursos." 


edu_10 <- cbind(CODIND, DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, VALOR, FUENTE, RESPONSABLE, JERARQUIA, 
                JERARQUIA_CAT, CORTE, DEPARTAMENTO, QUINTIL, SEXO, ASCENDENCIA, EDAD, NOTA_INDICADOR)




edu_2023 <- rbind(edu_01, edu_02, edu_03, edu_04, edu_05, edu_06, edu_07, edu_08, edu_09, edu_10)


edu_2023 <- as.data.frame(edu_2023)

edu_2023 <- edu_2023 %>% mutate(filter = ifelse(CORTE == "Edad" & 
                                               (CODIND == "130107" | 
                                                CODIND == "130108"), 1, 0))


table(edu_2023$filter)
edu_2023 <- filter(edu_2023, filter == 0)

edu_2023$filter <- NULL

rio::export(edu_2023, "edu_2023.xlsx" )