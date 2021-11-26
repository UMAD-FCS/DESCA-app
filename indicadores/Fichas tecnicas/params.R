
## ************************************************************************
## Script para generar las Notas Conceptuales Markdown (.pdf)
## ************************************************************************

library(tidyverse)
library(pagedown)

## Internacional - Derechos ----

base <- readxl::read_excel("Base Normativa Completa.xlsx") %>%
  filter(Derecho != "Todos") 

ruta <- "notas-pdf/internacional/derechos/"

lista_id <- base$id

## Loop para crear markdown en html

for (i in lista_id) {
  
out <- paste0(ruta, "int-der", i, ".html") 
                    
rmarkdown::render("Internacional-Derechos.Rmd", "html_document", output_file = out, 
                  
params = list(lista_id = i), encoding = "UTF-8")

}

## Loop para pasar markdowns a pdf

# Leer todas los documentos
filenames <- list.files("notas-pdf/internacional/derechos", 
                        pattern = "*.html",
                        full.names=TRUE) 

namesfiles <- substr(filenames, 34, 150) # Los modifico por comodidad
namesfiles <- gsub(".html", "", namesfiles)

# Cargo todas las bases con la función "read_twitter_csv" de rtweet 
for (i in seq_along(filenames)) {
  
  chrome_print(filenames[i])
  
}


## Internacional - Poblaciones  ----

base <- readxl::read_excel("Base Normativa Completa.xlsx") %>%
  filter(Población != "Todos") 

ruta <- "notas-pdf/internacional/poblaciones/"

lista_id <- base$id

## Loop para crear markdown en html

for (i in lista_id) {
  
  out <- paste0(ruta, "int-pob", i, ".html") 
  
  rmarkdown::render("Internacional-Poblaciones.Rmd", "html_document", output_file = out, 
                    
                    params = list(lista_id = i), encoding = "UTF-8")
  
}

## Loop para pasar markdowns a pdf

# Leer todas los documentos
filenames <- list.files("notas-pdf/internacional/poblaciones", 
                        pattern = "*.html",
                        full.names=TRUE) 

namesfiles <- substr(filenames, 34, 150) # Los modifico por comodidad
namesfiles <- gsub(".html", "", namesfiles)

# Cargo todas las bases con la función "read_twitter_csv" de rtweet 
for (i in seq_along(filenames)) {
  
  chrome_print(filenames[i])
  
}


## Constitucion - Derechos ----

base <- readxl::read_excel("Base Normativa Completa.xlsx", 2) %>%
  filter(Derecho != "Todos") 

ruta <- "notas-pdf/constitucion/derechos/"

lista_id <- base$id

## Loop para crear markdown en html

for (i in lista_id) {
  
  out <- paste0(ruta, "con-der", i, ".html") 
  
  rmarkdown::render("Constitucion-Derechos.Rmd", "html_document", output_file = out, 
                    
                    params = list(lista_id = i), encoding = "UTF-8")
  
}

## Loop para pasar markdowns a pdf

# Leer todas los documentos
filenames <- list.files(ruta, 
                        pattern = "*.html",
                        full.names=TRUE) 

namesfiles <- substr(filenames, 34, 150) # Los modifico por comodidad
namesfiles <- gsub(".html", "", namesfiles)

# Cargo todas las bases con la función "read_twitter_csv" de rtweet 
for (i in seq_along(filenames)) {
  
  chrome_print(filenames[i])
  
}



## Constitucion - Poblaciones  ----

base <- readxl::read_excel("Base Normativa Completa.xlsx", 2) %>%
  filter(Población != "Todos") 

ruta <- "notas-pdf/constitucion/poblaciones/"

lista_id <- base$id

## Loop para crear markdown en html

for (i in lista_id) {
  
  out <- paste0(ruta, "con-pob", i, ".html") 
  
  rmarkdown::render("Constitucion-Poblaciones.Rmd", "html_document", output_file = out, 
                    
                    params = list(lista_id = i), encoding = "UTF-8")
  
}

## Loop para pasar markdowns a pdf

# Leer todas los documentos
filenames <- list.files(ruta, 
                        pattern = "*.html",
                        full.names=TRUE) 

namesfiles <- substr(filenames, 34, 150) # Los modifico por comodidad
namesfiles <- gsub(".html", "", namesfiles)

# Cargo todas las bases con la función "read_twitter_csv" de rtweet 
for (i in seq_along(filenames)) {
  
  chrome_print(filenames[i])
  
}


## Leyes - Derechos ----

base <- readxl::read_excel("Base Normativa Completa.xlsx", 3) %>%
  filter(Derecho != "Todos") 

ruta <- "notas-pdf/leyes/derechos/"

lista_id <- base$id

## Loop para crear markdown en html

for (i in lista_id) {
  
  out <- paste0(ruta, "ley-der", i, ".html") 
  
  rmarkdown::render("Leyes-Derechos.Rmd", "html_document", output_file = out, 
                    
                    params = list(lista_id = i), encoding = "UTF-8")
  
}

## Loop para pasar markdowns a pdf

# Leer todas los documentos
filenames <- list.files(ruta, 
                        pattern = "*.html",
                        full.names=TRUE) 

namesfiles <- substr(filenames, 34, 150) # Los modifico por comodidad
namesfiles <- gsub(".html", "", namesfiles)

# Cargo todas las bases con la función "read_twitter_csv" de rtweet 
for (i in seq_along(filenames)) {
  
  chrome_print(filenames[i])
  
}



## Leyes - Poblaciones ----

base <- readxl::read_excel("Base Normativa Completa.xlsx", 3) %>%
  filter(Población != "Todos") 

ruta <- "notas-pdf/leyes/poblaciones/"

lista_id <- base$id

## Loop para crear markdown en html

for (i in lista_id) {
  
  out <- paste0(ruta, "ley-pob", i, ".html") 
  
  rmarkdown::render("Leyes-Poblaciones.Rmd", "html_document", output_file = out, 
                    
                    params = list(lista_id = i), encoding = "UTF-8")
  
}

## Loop para pasar markdowns a pdf

# Leer todas los documentos
filenames <- list.files(ruta, 
                        pattern = "*.html",
                        full.names=TRUE) 

namesfiles <- substr(filenames, 34, 150) # Los modifico por comodidad
namesfiles <- gsub(".html", "", namesfiles)

# Cargo todas las bases con la función "read_twitter_csv" de rtweet 
for (i in seq_along(filenames)) {
  
  chrome_print(filenames[i])
  
}

