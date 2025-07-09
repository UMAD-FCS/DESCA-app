### ANÁLISIS DATOS FIES ECH 2023
library(dplyr)
library(readxl)
library(RM.weights)

### Leo los datasets
SA <- rio::import("Bases/Base_FIES_2023.csv")


#si quiero solo hogares
SA<-SA[!duplicated(SA[,"ID"]),]


XX.country1 = SA[,2:9]
wt.country1 = SA$w

# acomodo la base 
XX.country1$sa1<- ifelse(XX.country1$sa1==2,0,XX.country1$sa1)
XX.country1$sa2<- ifelse(XX.country1$sa2==2,0,XX.country1$sa2)
XX.country1$sa3<- ifelse(XX.country1$sa3==2,0,XX.country1$sa3)
XX.country1$sa4<- ifelse(XX.country1$sa4==2,0,XX.country1$sa4)
XX.country1$sa5<- ifelse(XX.country1$sa5==2,0,XX.country1$sa5)
XX.country1$sa6<- ifelse(XX.country1$sa6==2,0,XX.country1$sa6)
XX.country1$sa7<- ifelse(XX.country1$sa7==2,0,XX.country1$sa7)
XX.country1$sa8<- ifelse(XX.country1$sa8==2,0,XX.country1$sa8)

XX.country1$sa1<- ifelse(XX.country1$sa1>=3,NA,XX.country1$sa1)
XX.country1$sa2<- ifelse(XX.country1$sa2>2,NA,XX.country1$sa2)
XX.country1$sa3<- ifelse(XX.country1$sa3>2,NA,XX.country1$sa3)
XX.country1$sa4<- ifelse(XX.country1$sa4>2,NA,XX.country1$sa4)
XX.country1$sa5<- ifelse(XX.country1$sa5>2,NA,XX.country1$sa5)
XX.country1$sa6<- ifelse(XX.country1$sa6>2,NA,XX.country1$sa6)
XX.country1$sa7<- ifelse(XX.country1$sa7>2,NA,XX.country1$sa7)
XX.country1$sa8<- ifelse(XX.country1$sa8>2,NA,XX.country1$sa8)


### Guarda los datos de FIES y el ponderador en objetos separados para hacer más fácil el manejo
XX.2023 <- XX.country1
wt.2023 <- wt.country1

#Cambia el nombre de los items

colnames(XX.2023)<-c("WORRIED", "HEALTHY", "FEWFOODS","SKIPPED","ATELESS","RUNOUT","HUNGRY","WHLDAY")



### Análisis de datos utilizando el paquete RM.weights


# Análisis de calidad de los datos----------------------------------------------------------------

extr = sum(rowSums(XX.2023)==8, na.rm=T)/sum(!is.na(rowSums(XX.2023)) & !rowSums(XX.2023)==0)
extr = c(min(7.7, 7.5+extr))


rr.2023 <- RM.w(XX.2023, wt.2023,.d=c(0.5, extr))

#
rr.2023$a

# Severidad de los items
rr.2023$b

# Infit (deseable: entre 0.7 y 1.3)
rr.2023$infit

# Outfit (deseable: menor a 5 -ideal- o 10)
rr.2023$outfit

#Correlación entre items (deseable: todas las correlaciones bajas)
rr.2023$res.corr

# Rasch reliability (deseable mayor a 0.7)
rr.2023$reliab



sthresh = c(-0.6529851, 3.9676415)

pp = prob.assign(rr.2023, sthres = sthresh)$sprob

# Probabilidad de presentar una severidad mayor a la severidad del item ATELESS
pp[1]*100

# Probabilidad de presentar una severidad mayor a la severidad del item WHLDAY 
pp[2]*100



rsXX <- rowSums(XX.2023) 

# Probabilidades asociadas a cada puntaje bruto 
p1=1-pnorm(sthresh[1], rr.2023$a, rr.2023$se.a)
p2=1-pnorm(sthresh[2], rr.2023$a, rr.2023$se.a)
p1[1]=p2[1]=0  
prob.mod.hh=p1[rsXX+1]
prob.sev.hh=p2[rsXX+1]

prob.mod.hh<-as.data.frame(prob.mod.hh)
prob.sev.hh<-as.data.frame(prob.sev.hh)

# Une las dos probabilidades a la base
data.hogares.2023<- SA

data.hogares.2023<-cbind(data.hogares.2023,prob.mod.hh,prob.sev.hh)
data.hogares.2023<- data.hogares.2023 %>% filter(!is.na(prob.mod.hh))

# Las prevalencias se pueden calcular a partir de las probabilidades de esta forma

totM<-sum(data.hogares.2023$prob.mod.hh*data.hogares.2023$w, na.rm=T)/sum(data.hogares.2023$w, na.rm=T)*100
totS<-sum(data.hogares.2023$prob.sev.hh*data.hogares.2023$w, na.rm=T)/sum(data.hogares.2023$w)*100


# Cálculos segmentados HOGARES
#REGION
#Moderado
fun <- function(x) {
  x <- sum(data.hogares.2023$prob.mod.hh[data.hogares.2023$region==x]*data.hogares.2023$w[data.hogares.2023$region==x])/sum(data.hogares.2023$w[data.hogares.2023$region==x])*100   
}       
dat <- numeric()
for(i in 1:2){
  dat[i] <- fun(x = i)
}         
Mreg <- as.data.frame(dat)

#Severo
fun <- function(x) {
  x <- sum(data.hogares.2023$prob.sev.hh[data.hogares.2023$region==x]*data.hogares.2023$w[data.hogares.2023$region==x])/sum(data.hogares.2023$w[data.hogares.2023$region==x])*100   
}       
dat <- numeric()
for(i in 1:2){
  dat[i] <- fun(x = i)
}         
Sreg <- as.data.frame(dat)


# Por quintiles
#Moderado
fun <- function(x) {
  x <- sum(data.hogares.2023$prob.mod.hh[data.hogares.2023$quintiles==x]*data.hogares.2023$w[data.hogares.2023$quintiles==x])/sum(data.hogares.2023$w[data.hogares.2023$quintiles==x])*100   
}       
dat <- numeric()
for(i in 1:5){
  dat[i] <- fun(x = i)
}         
MQ <- as.data.frame(dat)

#Severo
fun <- function(x) {
  x <- sum(data.hogares.2023$prob.sev.hh[data.hogares.2023$quintiles==x]*data.hogares.2023$w[data.hogares.2023$quintiles==x])/sum(data.hogares.2023$w[data.hogares.2023$quintiles==x])*100   
}       
dat <- numeric()
for(i in 1:5){
  dat[i] <- fun(x = i)
}         
SQ <- as.data.frame(dat)


# Según menores de 6
#Moderado
fun <- function(x) {
  x <- sum(data.hogares.2023$prob.mod.hh[data.hogares.2023$menores6==x]*data.hogares.2023$w[data.hogares.2023$menores6==x])/sum(data.hogares.2023$w[data.hogares.2023$menores6==x])*100   
}       
dat <- numeric()
for(i in 0:1){
  dat[i+1] <- fun(x = i)
}         
Mmen6 <- as.data.frame(dat)

#Severo
fun <- function(x) {
  x <- sum(data.hogares.2023$prob.sev.hh[data.hogares.2023$menores6==x]*data.hogares.2023$w[data.hogares.2023$menores6==x])/sum(data.hogares.2023$w[data.hogares.2023$menores6==x])*100   
}       
dat <- numeric()
for(i in 0:1){
  dat[i+1] <- fun(x = i)
}         
Smen6 <- as.data.frame(dat)


# Según menores de 18
#Moderado
fun <- function(x) {
  x <- sum(data.hogares.2023$prob.mod.hh[data.hogares.2023$menores18==x]*data.hogares.2023$w[data.hogares.2023$menores18==x])/sum(data.hogares.2023$w[data.hogares.2023$menores18==x])*100   
}       
dat <- numeric()
for(i in 0:1){
  dat[i+1] <- fun(x = i)
}         
Mmen18 <- as.data.frame(dat)

#Severo
fun <- function(x) {
  x <- sum(data.hogares.2023$prob.sev.hh[data.hogares.2023$menores18==x]*data.hogares.2023$w[data.hogares.2023$menores18==x])/sum(data.hogares.2023$w[data.hogares.2023$menores18==x])*100   
}       
dat <- numeric()
for(i in 0:1){
  dat[i+1] <- fun(x = i)
}         
Smen18 <- as.data.frame(dat)


MOD_H <- rbind(totM, Mreg, MQ, Mmen6, Mmen18)
SEV_H <- rbind(totS, Sreg, SQ, Smen6, Smen18)

CODIND       <- "730206"
POBLACION    <- c("","","","","","","","","Niños, niñas y adolescentes","Niños, niñas y adolescentes","Niños, niñas y adolescentes","Niños, niñas y adolescentes")
DERECHO      <- "Alimentación"
TIPOIND      <- "Resultados"
DIMENSIÓN    <- "Accesibilidad"
CODINDICADOR <- "Inseguridad alimentaria moderada (hogares)"
NOMINDICADOR <- "Porcentaje de hogares con inseguridad alimentaria moderada"
AÑO	         <- 2023
fecha_cat    <- ""
VALOR	       <- MOD_H[[1]]
FUENTE	     <- "Módulo Seguridad alimentaria, Encuesta Continua de Hogares 2023, INE"
RESPONSABLE	 <- "S. Katzkowicz"
JERARQUIA	   <- 1
JERARQUIA_CAT<- ""
CORTE        <- c("Total", "Región", "Región", "Quintil", "Quintil", "Quintil", "Quintil", "Quintil", "Menores", "Menores", "Menores", "Menores")
CORTE_2      <- ""
DEPARTAMENTO <- ""
QUINTIL	     <- c("", "", "", "Quintil 1", "Quintil 2", "Quintil 3", "Quintil 4", "Quintil 5", "", "", "", "")
SEXO		     <- ""
EDAD         <- ""
ASCENDENCIA  <- ""
REGION <- c("", "Montevideo", "Interior", "", "", "", "", "", "", "", "", "")
MENORES <- c("", "", "", "", "", "", "", "", "Sin menores de 6", "Con menores de 6", "Sin menores de 18", "Con menores de 18")   
TPOLITICA <- ""

sa01 <- cbind(CODIND, POBLACION,DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, fecha_cat, VALOR, FUENTE, RESPONSABLE, JERARQUIA, JERARQUIA_CAT, CORTE, CORTE_2, DEPARTAMENTO,QUINTIL, SEXO, EDAD, ASCENDENCIA, REGION, MENORES, TPOLITICA)


CODIND       <- "730207"
POBLACION    <- c("", "","", "", "", "", "", "",  
                  "Niños, niñas y adolescentes", "Niños, niñas y adolescentes","Niños, niñas y adolescentes", "Niños, niñas y adolescentes") 
DERECHO      <- "Alimentación"
TIPOIND      <- "Resultados"
DIMENSIÓN    <- "Accesibilidad"
CODINDICADOR <- "Inseguridad alimentaria severa (hogares)"
NOMINDICADOR <- "Porcentaje de hogares con inseguridad alimentaria severa"
AÑO	         <- 2023
fecha_cat    <- ""
VALOR	       <- SEV_H[[1]]
FUENTE	     <- "Módulo Seguridad alimentaria, Encuesta Continua de Hogares 2023, INE"
RESPONSABLE	 <- "S. Katzkowicz"
JERARQUIA	   <- 1
JERARQUIA_CAT<- ""
CORTE        <- c("Total", "Región", "Región", "Quintil", "Quintil", "Quintil", "Quintil", "Quintil", "Menores", "Menores", "Menores", "Menores")
CORTE_2      <- ""
DEPARTAMENTO <- ""
QUINTIL	     <- c("", "", "", "Quintil 1", "Quintil 2", "Quintil 3", "Quintil 4", "Quintil 5", "", "", "", "")
SEXO		     <- ""
EDAD         <- ""
ASCENDENCIA  <- ""
REGION <- c("", "Montevideo", "Interior", "", "", "", "", "", "", "", "", "")
MENORES <- c("", "", "", "", "", "", "", "", "Sin menores de 6", "Con menores de 6", "Sin menores de 18", "Con menores de 18")   
TPOLITICA <- ""

sa02 <- cbind(CODIND, POBLACION,DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, fecha_cat, VALOR, FUENTE, RESPONSABLE, JERARQUIA, JERARQUIA_CAT, CORTE, CORTE_2, DEPARTAMENTO,QUINTIL, SEXO, EDAD, ASCENDENCIA, REGION, MENORES, TPOLITICA)


#Calcula las prevalencias expresadas en personas por hogar

totM <- sum(data.hogares.2023$prob.mod.hh*data.hogares.2023$w*data.hogares.2023$HT19)/sum(data.hogares.2023$w*data.hogares.2023$HT19)*100
totS <- sum(data.hogares.2023$prob.sev.hh*data.hogares.2023$w*data.hogares.2023$HT19)/sum(data.hogares.2023$w*data.hogares.2023$HT19)*100


ech2023  <- rio::import("Bases/ECH_2023.csv")
ech2023$w  <- NULL
ech2023$HT19  <- NULL
ech2023$region  <- NULL
ech23 <- merge(data.hogares.2023, ech2023, by="ID")


#Total, Quintil de ingresos, sexo, afrodescendientes, departamento, tramo de edad


# Por quintiles
#Moderado
fun <- function(x) {
  x <- sum(ech23$prob.mod.hh[ech23$quintiles==x]*ech23$HT19[ech23$quintiles==x]*ech23$w[ech23$quintiles==x])/sum(ech23$HT19[ech23$quintiles==x]*ech23$w[ech23$quintiles==x])*100   
}       
dat <- numeric()
for(i in 1:5){
  dat[i] <- fun(x = i)
}         
MQ <- as.data.frame(dat)

#Severo
fun <- function(x) {
  x <- sum(ech23$prob.sev.hh[ech23$quintiles==x]*ech23$HT19[ech23$quintiles==x]*ech23$w[ech23$quintiles==x])/sum(ech23$HT19[ech23$quintiles==x]*ech23$w[ech23$quintiles==x])*100   
}       
dat <- numeric()
for(i in 1:5){
  dat[i] <- fun(x = i)
}         
SQ <- as.data.frame(dat)




# Por sexo
#Moderado
fun <- function(x) {
  x <- sum(ech23$prob.mod.hh[ech23$e26==x]*ech23$HT19[ech23$e26==x]*ech23$w[ech23$e26==x])/sum(ech23$HT19[ech23$e26==x]*ech23$w[ech23$e26==x])*100   
}       
dat <- numeric()
for(i in 1:2){
  dat[i] <- fun(x = i)
}         
Msex <- as.data.frame(dat)

#Severo
fun <- function(x) {
  x <- sum(ech23$prob.sev.hh[ech23$e26==x]*ech23$HT19[ech23$e26==x]*ech23$w[ech23$e26==x])/sum(ech23$HT19[ech23$e26==x]*ech23$w[ech23$e26==x])*100   
}       
dat <- numeric()
for(i in 1:2){
  dat[i] <- fun(x = i)
}         
Ssex <- as.data.frame(dat)



# Por sexo
#Moderado
fun <- function(x) {
  x <- sum(ech23$prob.mod.hh[ech23$e29_1==x]*ech23$HT19[ech23$e29_1==x]*ech23$w[ech23$e29_1==x])/sum(ech23$HT19[ech23$e29_1==x]*ech23$w[ech23$e29_1==x])*100   
}       
dat <- numeric()
for(i in 1:2){
  dat[i] <- fun(x = i)
}         
Masc <- as.data.frame(dat)

#Severo
fun <- function(x) {
  x <- sum(ech23$prob.sev.hh[ech23$e29_1==x]*ech23$HT19[ech23$e29_1==x]*ech23$w[ech23$e29_1==x])/sum(ech23$HT19[ech23$e29_1==x]*ech23$w[ech23$e29_1==x])*100   
}       
dat <- numeric()
for(i in 1:2){
  dat[i] <- fun(x = i)
}         
Sasc <- as.data.frame(dat)


##Región
#Moderado
fun <- function(x) {
  x <- sum(ech23$prob.mod.hh[ech23$region==x]*ech23$HT19[ech23$region==x]*ech23$w[ech23$region==x])/sum(ech23$HT19[ech23$region==x]*ech23$w[ech23$region==x])*100   
}       
dat <- numeric()
for(i in 1:2){
  dat[i] <- fun(x = i)
}         
Mreg <- as.data.frame(dat)

#Severo
fun <- function(x) {
  x <- sum(ech23$prob.sev.hh[ech23$region==x]*ech23$HT19[ech23$region==x]*ech23$w[ech23$region==x])/sum(ech23$HT19[ech23$region==x]*ech23$w[ech23$region==x])*100   
}       
dat <- numeric()
for(i in 1:2){
  dat[i] <- fun(x = i)
}         
Sreg <- as.data.frame(dat)


#Tramo de edad
ech23 <- ech23 %>% dplyr::mutate(tramo1 = case_when(e27>=0 & e27 <=5 ~ 1,
                                                    e27>=6 & e27 <=12 ~ 2,
                                                    e27>=13 & e27 <=17 ~ 3,
                                                    e27>=18 & e27 <=29 ~ 4,
                                                    e27>=30 & e27 <=64 ~ 5,
                                                    e27>=65 ~ 6))

#Moderado
fun <- function(x) {
  x <- sum(ech23$prob.mod.hh[ech23$tramo1==x]*ech23$HT19[ech23$tramo1==x]*ech23$w[ech23$tramo1==x])/sum(ech23$HT19[ech23$tramo1==x]*ech23$w[ech23$tramo1==x])*100   
}       
dat <- numeric()
for(i in 1:6){
  dat[i] <- fun(x = i)
}         
Medad <- as.data.frame(dat)

#Severo
fun <- function(x) {
  x <- sum(ech23$prob.sev.hh[ech23$tramo1==x]*ech23$HT19[ech23$tramo1==x]*ech23$w[ech23$tramo1==x])/sum(ech23$HT19[ech23$tramo1==x]*ech23$w[ech23$tramo1==x])*100   
}       
dat <- numeric()
for(i in 1:6){
  dat[i] <- fun(x = i)
}         
Sedad <- as.data.frame(dat)


MOD <- rbind(totM, MQ, Msex, Masc, Mreg, Medad)
SEV <- rbind(totS, SQ, Ssex, Sasc, Sreg, Sedad)



CODIND       <- "730208"
POBLACION    <- c("", "", "", "", "", "", "Mujeres", "Mujeres", "Afrodescendientes", "Afrodescendientes", "", "", "Niños, niñas y adolescentes",
                  "Niños, niñas y adolescentes","Niños, niñas y adolescentes","Niños, niñas y adolescentes","Niños, niñas y adolescentes","Niños, niñas y adolescentes")
DERECHO      <- "Alimentación"
TIPOIND      <- "Resultados"
DIMENSIÓN    <- "Accesibilidad"
CODINDICADOR <- "Inseguridad alimentaria moderada (personas)"
NOMINDICADOR <- "Porcentaje de personas en hogares con inseguridad alimentaria moderada"
AÑO	         <- 2023
fecha_cat    <- ""
VALOR	       <- MOD[[1]]
FUENTE	     <- "Módulo Seguridad alimentaria, Encuesta Continua de Hogares 2023, INE"
RESPONSABLE	 <- "S. Katzkowicz"
JERARQUIA	   <- 1
JERARQUIA_CAT<- ""
CORTE        <- c("Total", "Quintil", "Quintil", "Quintil", "Quintil", "Quintil", "Sexo", "Sexo", "Ascendencia étnico-racial", "Ascendencia étnico-racial",
                  "Region", "Region", "Edad", "Edad", "Edad", "Edad", "Edad", "Edad")
CORTE_2      <- ""
DEPARTAMENTO <- ""
QUINTIL	     <- c("", "Quintil 1", "Quintil 2", "Quintil 3", "Quintil 4", "Quintil 5", "", "", "", "", "", "", "", "", "", "", "", "")
SEXO		     <- c("", "", "", "", "", "", "Varones", "Mujeres", "", "",
                "", "", "", "", "", "", "", "")
EDAD         <- c("", "", "", "", "", "", "", "", "", "",
                  "", "", "Entre 0 y 5", "Entre 6 y 12", "Entre 13y 17", "Entre 18 y 29", "Entre 30 y 64", "65 o más")
ASCENDENCIA  <- c("", "", "", "", "", "", "", "", "Afrodescendientes", "No afrodescendientes",
                  "", "", "", "", "", "", "", "")
REGION <- c("", "", "", "", "", "", "", "", "", "",
            "Montevideo", "Interior", "", "", "", "", "", "")
MENORES <- ""
TPOLITICA <- ""

sa03 <- cbind(CODIND, POBLACION,DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, fecha_cat, VALOR, FUENTE, RESPONSABLE, JERARQUIA, JERARQUIA_CAT, CORTE, CORTE_2, DEPARTAMENTO,QUINTIL, SEXO, EDAD, ASCENDENCIA, REGION, MENORES, TPOLITICA)



CODIND       <- "730209"
POBLACION    <- c("", "", "", "", "", "", "Mujeres", "Mujeres", "Afrodescendientes", "Afrodescendientes", "", "", "Niños, niñas y adolescentes",
                  "Niños, niñas y adolescentes","Niños, niñas y adolescentes","Niños, niñas y adolescentes","Niños, niñas y adolescentes","Niños, niñas y adolescentes")
DERECHO      <- "Alimentación"
TIPOIND      <- "Resultados"
DIMENSIÓN    <- "Accesibilidad"
CODINDICADOR <- "Inseguridad alimentaria severa (personas)"
NOMINDICADOR <- "Porcentaje de personas en hogares con inseguridad alimentaria severa"
AÑO	         <- 2023
fecha_cat    <- ""
VALOR	       <- SEV[[1]]
FUENTE	     <- "Módulo Seguridad alimentaria, Encuesta Continua de Hogares 2023, INE"
RESPONSABLE	 <- "S. Katzkowicz"
JERARQUIA	   <- 1
JERARQUIA_CAT<- ""
CORTE        <- c("Total", "Quintil", "Quintil", "Quintil", "Quintil", "Quintil", "Sexo", "Sexo", "Ascendencia étnico-racial", "Ascendencia étnico-racial",
                  "Region", "Region", "Edad", "Edad", "Edad", "Edad", "Edad", "Edad")
CORTE_2      <- ""
DEPARTAMENTO <- ""
QUINTIL	     <- c("", "Quintil 1", "Quintil 2", "Quintil 3", "Quintil 4", "Quintil 5", "", "", "", "", "", "", "", "", "", "", "", "")
SEXO		     <- c("", "", "", "", "", "", "Varones", "Mujeres", "", "",
                "", "", "", "", "", "", "", "")
EDAD         <- c("", "", "", "", "", "", "", "", "", "",
                  "", "", "Entre 0 y 5", "Entre 6 y 12", "Entre 13y 17", "Entre 18 y 29", "Entre 30 y 64", "65 o más")
ASCENDENCIA  <- c("", "", "", "", "", "", "", "", "Afrodescendientes", "No afrodescendientes",
                  "", "", "", "", "", "", "", "")
REGION <- c("", "", "", "", "", "", "", "", "", "",
            "Montevideo", "Interior", "", "", "", "", "", "")
MENORES <- ""
TPOLITICA <- ""

sa04 <- cbind(CODIND, POBLACION,DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, fecha_cat, VALOR, FUENTE, RESPONSABLE, JERARQUIA, JERARQUIA_CAT, CORTE, CORTE_2, DEPARTAMENTO,QUINTIL, SEXO, EDAD, ASCENDENCIA, REGION, MENORES, TPOLITICA)





##Comedor
ech23 <- ech23 %>% dplyr::mutate(comedor = case_when(e559==1|e582==1 ~ 1, e559!=1&e582!=1 ~ 0)) 

##Canasta
ech23 <- ech23 %>% dplyr::mutate(canasta = case_when(e59==1 ~ 1, e59!=1 ~ 0)) 

##Política alimentaria (excluídas las canastas)
ech23 <- ech23 %>% dplyr::mutate(pol_alim_scan = case_when(e559==1|e582==1|e560==1 ~ 1, e559!=1&e582!=1&e560!=1 ~ 0)) 

##Política alimentaria (incluídas las canastas)
ech23 <- ech23 %>% dplyr::mutate(pol_alim = case_when(e559==1|e582==1|e560==1|e59==1 ~ 1, e59!=1&e559!=1&e582!=1&e560!=1 ~ 0)) 


###Políticas alimentarias para quienes tienen inseguridad alimentaria moderada o grave
comedor_mod <- sum(ech23$prob.mod.hh[ech23$comedor==0]*ech23$HT19[ech23$comedor==0]*ech23$w[ech23$comedor==0])/sum(ech23$prob.mod.hh*ech23$HT19*ech23$w)*100   
comedor_sev <- sum(ech23$prob.sev.hh[ech23$comedor==0]*ech23$HT19[ech23$comedor==0]*ech23$w[ech23$comedor==0])/sum(ech23$prob.sev.hh*ech23$HT19*ech23$w)*100   

canasta_mod <- sum(ech23$prob.mod.hh[ech23$canasta==0]*ech23$HT19[ech23$canasta==0]*ech23$w[ech23$canasta==0])/sum(ech23$prob.mod.hh*ech23$HT19*ech23$w)*100   
canasta_sev <- sum(ech23$prob.sev.hh[ech23$canasta==0]*ech23$HT19[ech23$canasta==0]*ech23$w[ech23$canasta==0])/sum(ech23$prob.sev.hh*ech23$HT19*ech23$w)*100   

pol_alim_scan_mod <- sum(ech23$prob.mod.hh[ech23$pol_alim_scan==0]*ech23$HT19[ech23$pol_alim_scan==0]*ech23$w[ech23$pol_alim_scan==0])/sum(ech23$prob.mod.hh*ech23$HT19*ech23$w)*100   
pol_alim_scan_sev <- sum(ech23$prob.sev.hh[ech23$pol_alim_scan==0]*ech23$HT19[ech23$pol_alim_scan==0]*ech23$w[ech23$pol_alim_scan==0])/sum(ech23$prob.sev.hh*ech23$HT19*ech23$w)*100   

pol_alim_mod <- sum(ech23$prob.mod.hh[ech23$pol_alim==0]*ech23$HT19[ech23$pol_alim==0]*ech23$w[ech23$pol_alim==0])/sum(ech23$prob.mod.hh*ech23$HT19*ech23$w)*100   
pol_alim_sev <- sum(ech23$prob.sev.hh[ech23$pol_alim==0]*ech23$HT19[ech23$pol_alim==0]*ech23$w[ech23$pol_alim==0])/sum(ech23$prob.sev.hh*ech23$HT19*ech23$w)*100   






CODIND       <- "730210"
POBLACION    <- ""
DERECHO      <- "Alimentación"
TIPOIND      <- "Resultados"
DIMENSIÓN    <- "Adecuación"
CODINDICADOR <- "Suficiencia políticas - inseguridad severa"
NOMINDICADOR <- c("Porcentaje de personas con inseguridad alimentaria severa que no reciben políticas alimentarias")
AÑO	         <- 2023
fecha_cat    <- ""
VALOR	       <- c(comedor_sev, canasta_sev, pol_alim_scan_sev, pol_alim_sev)
FUENTE	     <- "Módulo Seguridad alimentaria, Encuesta Continua de Hogares 2023, INE"
RESPONSABLE	 <- "S. Katzkowicz"
JERARQUIA	   <- 1
JERARQUIA_CAT<- ""
CORTE        <- c("Tipo de política","Tipo de política","Tipo de política","Total")
CORTE_2      <- ""
DEPARTAMENTO <- ""
QUINTIL	     <- ""
SEXO		     <- ""
EDAD         <- ""
ASCENDENCIA  <- ""
REGION <- ""
MENORES <- ""
TPOLITICA <- c("Comedor", "Canastas", "Políticas Alimentarias - Sin Canastas", "")


sa05 <- cbind(CODIND, POBLACION,DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, fecha_cat, VALOR, FUENTE, RESPONSABLE, JERARQUIA, JERARQUIA_CAT, CORTE, CORTE_2, DEPARTAMENTO,QUINTIL, SEXO, EDAD, ASCENDENCIA, REGION, MENORES, TPOLITICA)



CODIND       <- "730211"
POBLACION    <- ""
DERECHO      <- "Alimentación"
TIPOIND      <- "Resultados"
DIMENSIÓN    <- "Adecuación"
CODINDICADOR <- "Suficiencia políticas - inseguridad moderada"
NOMINDICADOR <- "Porcentaje de personas con inseguridad alimentaria moderada que no reciben políticas alimentarias"
AÑO	         <- 2023
fecha_cat    <- ""
VALOR	       <- c(comedor_mod, canasta_mod, pol_alim_scan_mod, pol_alim_mod)
FUENTE	     <- "Módulo Seguridad alimentaria, Encuesta Continua de Hogares 2023, INE"
RESPONSABLE	 <- "S. Katzkowicz"
JERARQUIA	   <- 1
JERARQUIA_CAT<- ""
CORTE        <- c("Tipo de política","Tipo de política","Tipo de política","Total")
CORTE_2      <- ""
DEPARTAMENTO <- ""
QUINTIL	     <- ""
SEXO		     <- ""
EDAD         <- ""
ASCENDENCIA  <- ""
REGION <- ""
MENORES <- ""
TPOLITICA <- c("Comedor", "Canastas", "Políticas Alimentarias - Sin Canastas", "")


sa06 <- cbind(CODIND, POBLACION,DERECHO, TIPOIND, DIMENSIÓN, CODINDICADOR, NOMINDICADOR, AÑO, fecha_cat, VALOR, FUENTE, RESPONSABLE, JERARQUIA, JERARQUIA_CAT, CORTE, CORTE_2, DEPARTAMENTO,QUINTIL, SEXO, EDAD, ASCENDENCIA, REGION, MENORES, TPOLITICA)



seg_alim <- rbind(sa01, sa02, sa03, sa04, sa05, sa06)

rio::export(seg_alim, "seg_alim_2023.xlsx" )

