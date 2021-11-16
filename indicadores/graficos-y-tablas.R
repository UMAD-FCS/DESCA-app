
## =========================================================================
## Graficos individuales series
## =========================================================================

## 0. Paquetes y data ======================================================

library(tidyverse)
library(ggrepel)

rm(list = ls())
source("utils.R")


## 1. Cargar data  =========================================================

dat <- load("Data/data_motor.rda") 

dat <- tibble::as_tibble(x) %>%
  mutate(Fecha = format(fecha, format = "%Y")) %>%
  rename(Valor = valor) %>%
  relocate(Fecha) %>%
  mutate(departamento = case_when(
    departamento != "Total" ~ toupper(departamento))) %>% 
  mutate(tipoind = case_when(
    tipoind == "Políticas públicas y esfuerzo económico" ~ "Políticas Públicas y Esfuerzo Económico",
    TRUE ~ tipoind 
  )) %>% 
  filter(web == 1)

rm(x)


## 2. Loop graficos  =======================================================

paleta_expandida <- c(RColorBrewer::brewer.pal(8, "Dark2"), "#B76A16", "#75A61A", "#D9318E",
                      "#986A74", "#C14D6A", "#C1632B", "#698446", "#7B6BB0",
                      "#A9A80F", "#DEAA03")

list_ind <- dat %>% 
  distinct(codindicador) %>% 
  pull()

for (i in seq_along(list_ind)) {
  
  # Filtro base para cada indicador
  dat_ind <- dat %>% 
    filter(jerarquia == 1) %>% 
    filter(codindicador == list_ind[i]) 
    
  if(dat_ind$corte == "Total"){
  
  # Base para último valor
  data_ends <- dat_ind %>%
    arrange(desc(Fecha)) %>%
    slice(1)

  # Gráfico
  temp_plot <- ggplot(dat_ind,
                      aes(x = fecha, y = Valor)) +
    geom_line(size = 1, alpha = .6, color = "#1979c6") +
    geom_point(size = 2.5, color = "#1979c6") +
    theme_m(base_size = 9) +
    labs(title = unique(dat_ind$nomindicador),
         x = "",
         y = "",
         caption = wrapit(unique(dat_ind$cita))) +
    geom_text_repel(
      aes(label = Valor), data = data_ends,
      size = 7,
      vjust = -1,
      color = "#1979c6") +
    ylim(min(dat_ind$Valor) - min(dat_ind$Valor)/5, max(dat_ind$Valor) + max(dat_ind$Valor)/5 )
  
  # Guardar
  ggsave(temp_plot, 
         file = paste0("indicadores/viz/", list_ind[i],".png"), 
         width = 40, height = 25, units = "cm")
  
  } else if (dat_ind$corte != "Total") {
    
    dat_ind <- dat_ind %>% 
      janitor::remove_empty("cols") 
    
    # Base para último valor
    data_ends <- dat_ind %>%
      group_by(get(names(dat_ind[,ncol(dat_ind)]))) %>% 
      arrange(desc(Fecha)) %>%
      slice(1)
    
    # Gráfico
    temp_plot <- ggplot(dat_ind,
                        aes_string(x = "fecha", y = "Valor", colour = names(dat_ind[,ncol(dat_ind)]))) +
      geom_line(size = 1, alpha = .6) +
      geom_point(size = 2.5) +
      theme_m(base_size = 9) +
      labs(title = unique(dat_ind$nomindicador),
           x = "",
           y = "",
           caption = wrapit(unique(dat_ind$cita))) +
      geom_text_repel(
        aes(label = Valor), data = data_ends,
        size = 7,
        vjust = -1,
        show.legend = FALSE) +
      ylim(min(dat_ind$Valor) - min(dat_ind$Valor)/10, max(dat_ind$Valor) + max(dat_ind$Valor)/10 )  +
      scale_colour_manual(name = "", values = paleta_expandida) + 
      theme(legend.position = "bottom")
    
    # Guardar
    ggsave(temp_plot, 
           file = paste0("indicadores/viz/", list_ind[i],".png"), 
           width = 40, height = 25, units = "cm")
    
  }
}


## 4. Loop excel  =========================================================

for (i in seq_along(list_ind)) {
  
  # Filtro base para cada indicador
  dat_ind <- dat %>% 
    filter(jerarquia == 1) %>% 
    filter(codindicador == list_ind[i]) 
  

  if(dat_ind$corte == "Total"){
    
    # Data 
    dat_cut <- dat_ind %>% 
      filter(codindicador == list_ind[i]) %>%
      select(Fecha, Valor) 
    
    # Metadata 
    metadat <-  readxl::read_excel("Data/Base_fichas_indicadores.xlsx") %>%
      filter(CODINDICADOR == list_ind[i]) %>% 
      select(DERECHO, DIMENSIÓN, CONINDICADOR, NOMINDICADOR, 
             DEFINICIÓN, CÁLCULO) %>% 
      gather(key = "", value = " ")
    
    # Excel merge
    lista_excel <- list("Datos" = dat_cut,
                        "Ficha técnica" = metadat)
    
    # Guardar
    openxlsx::write.xlsx(lista_excel, 
                         file = paste0("indicadores/tablas/", list_ind[i], ".xlsx")) 
                         
  } else if (dat_ind$corte != "Total") {
    
    # Data 
    dat_cut <- dat_ind %>% 
      janitor::remove_empty("cols") 
    
    dat_cut <- dat_cut %>% 
      select(Fecha, Valor, names(dat_cut[,ncol(dat_cut)])) %>% 
      pivot_wider(names_from = names(dat_cut[,ncol(dat_cut)]),
                  values_from = "Valor")
    
    # Metadata 
    metadat <-  readxl::read_excel("Data/Base_fichas_indicadores.xlsx") %>%
      filter(CODINDICADOR == list_ind[i]) %>% 
      select(DERECHO, DIMENSIÓN, CONINDICADOR, NOMINDICADOR, 
             DEFINICIÓN, CÁLCULO) %>% 
      gather(key = "", value = " ")
    
    # Excel merge
    lista_excel <- list("Datos" = dat_cut,
                        "Ficha técnica" = metadat)
    
    # Guardar
    openxlsx::write.xlsx(lista_excel, 
                         file = paste0("indicadores/tablas/", list_ind[i], ".xlsx")) 
  }

}
