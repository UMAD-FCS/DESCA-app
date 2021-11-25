
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



