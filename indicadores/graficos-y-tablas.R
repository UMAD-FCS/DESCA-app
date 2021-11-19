
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
  )) 

rm(x)


## 2. Loop graficos  =======================================================

paleta_expandida <- c(RColorBrewer::brewer.pal(8, "Dark2"), "#B76A16", "#75A61A", "#D9318E",
                      "#986A74", "#C14D6A", "#C1632B", "#698446", "#7B6BB0",
                      "#A9A80F", "#DEAA03")

list_ind <- dat %>% 
  filter(web == 1) %>% 
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
                        aes_string(x = "fecha", y = "Valor",  color = names(dat_ind[,ncol(dat_ind)]))) +
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
      scale_color_manual(name = "", values = paleta_expandida) + 
      theme(legend.position = "bottom",
            legend.text = element_text(size=10))
    
    # Guardar
    ggsave(temp_plot, 
           file = paste0("indicadores/viz/", list_ind[i],".png"), 
           width = 40, height = 25, units = "cm")
    
  }
  
  print(list_ind[i])
}


## 3. Loop graficos población  =============================================

list_ind_pob <- dat %>% 
  filter(web_pob == 1) %>% 
  filter(codindicador != 430101) %>% 
  distinct(codindicador) %>% 
  pull()


## Loop para generar gráficos con todas las combinaciones

for (i in seq_along(list_ind_pob)) {
  
  ## Gráfico total
  dat_ind <- dat %>% 
    filter(codindicador == list_ind_pob[i]) %>% 
    filter(corte == "Total")
  
  if(nrow(dat_ind) > 0){
    
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
           file = paste0("indicadores/viz/pob/", list_ind_pob[i],".png"), 
           width = 40, height = 25, units = "cm")
    
    ## Exportar excel también 
    dat_cut <- dat_ind %>% 
      filter(codindicador == list_ind_pob[i]) %>%
      select(Fecha, Valor) 
    
    # Metadata 
    metadat <-  readxl::read_excel("Data/Base_fichas_indicadores.xlsx") %>%
      filter(CODINDICADOR == list_ind_pob[i]) %>% 
      select(DERECHO, CONINDICADOR, NOMINDICADOR, 
             DEFINICIÓN, CÁLCULO, TIPOIND, CITA) %>% 
      gather(key = "", value = " ")
    
    # Excel merge
    lista_excel <- list("Datos" = dat_cut,
                        "Ficha técnica" = metadat)
    
    # Guardar
    openxlsx::write.xlsx(lista_excel, 
                         file = paste0("indicadores/tablas/pob/", list_ind_pob[i], ".xlsx")) 
    
  } else  {
    
  }
  
  ## Gráficos sexo  
  dat_sexo <- dat %>%
    filter(codindicador == list_ind_pob[i]) %>%
    filter(corte == "Sexo")
  
  if(nrow(dat_sexo) > 0 & max(dat_sexo$sexo_pob) == 1){
    
    # Base para último valor
    data_ends <- dat_sexo %>%
      group_by(sexo) %>% 
      arrange(desc(Fecha)) %>%
      slice(1)
    
    # Gráfico
    temp_plot <- ggplot(dat_sexo,
                        aes(x = fecha, y = Valor,  color = sexo)) +
      geom_line(size = 1, alpha = .6) +
      geom_point(size = 2.5) +
      theme_m(base_size = 9) +
      labs(title = unique(dat_sexo$nomindicador),
           x = "",
           y = "",
           caption = wrapit(unique(dat_sexo$cita))) +
      geom_text_repel(
        aes(label = Valor), data = data_ends,
        size = 7,
        vjust = -1,
        show.legend = FALSE) +
      ylim(min(dat_sexo$Valor) - min(dat_sexo$Valor)/10, max(dat_sexo$Valor) + max(dat_sexo$Valor)/10 )  +
      scale_color_manual(name = "", values = paleta_expandida) + 
      theme(legend.position = "bottom",
            legend.text = element_text(size=10))
    
    # Guardar
    ggsave(temp_plot, 
           file = paste0("indicadores/viz/pob/", list_ind_pob[i], "_sexo", ".png"), 
           width = 40, height = 25, units = "cm")

    ## Excel para exportar
    dat_cut <- dat_sexo %>% 
      select(Fecha, Valor, sexo) %>% 
      pivot_wider(names_from = "sexo",
                  values_from = "Valor")

    # Metadata 
    metadat <-  readxl::read_excel("Data/Base_fichas_indicadores.xlsx") %>%
      filter(CODINDICADOR == list_ind_pob[i]) %>% 
      select(DERECHO, CONINDICADOR, NOMINDICADOR, 
             DEFINICIÓN, CÁLCULO, TIPOIND, CITA) %>% 
      gather(key = "", value = " ")
    
    # Excel merge
    lista_excel <- list("Datos" = dat_cut,
                        "Ficha técnica" = metadat)
    
    # Guardar
    openxlsx::write.xlsx(lista_excel, 
                         file = paste0("indicadores/tablas/pob/", list_ind_pob[i], "_sexo.xlsx")) 
    
    
  } else {
    
    print(paste(list_ind_pob[i], "no tiene corte por sexo"))
  }
  
  
  ## Gráficos edad  
  dat_edad <- dat %>%
    filter(codindicador == list_ind_pob[i]) %>%
    filter(corte == "Edad")
  
  if(nrow(dat_edad) > 0 & max(dat_edad$edad_pob) == 1){
    
    # Base para último valor
    data_ends <- dat_edad %>%
      group_by(edad) %>% 
      arrange(desc(Fecha)) %>%
      slice(1)
    
    # Gráfico
    temp_plot <- ggplot(dat_edad,
                        aes(x = fecha, y = Valor,  color = edad)) +
      geom_line(size = 1, alpha = .6) +
      geom_point(size = 2.5) +
      theme_m(base_size = 9) +
      labs(title = unique(dat_edad$nomindicador),
           x = "",
           y = "",
           caption = wrapit(unique(dat_edad$cita))) +
      geom_text_repel(
        aes(label = Valor), data = data_ends,
        size = 7,
        vjust = -1,
        show.legend = FALSE) +
      ylim(min(dat_edad$Valor) - min(dat_edad$Valor)/10, max(dat_edad$Valor) + max(dat_edad$Valor)/10 )  +
      scale_color_manual(name = "", values = paleta_expandida) + 
      theme(legend.position = "bottom",
            legend.text = element_text(size=10))
    
    # Guardar
    ggsave(temp_plot, 
           file = paste0("indicadores/viz/pob/", list_ind_pob[i], "_edad", ".png"), 
           width = 40, height = 25, units = "cm")
    
    ## Excel para exportar
    dat_cut <- dat_edad %>% 
      select(Fecha, Valor, edad) %>% 
      pivot_wider(names_from = "edad",
                  values_from = "Valor")
    
    # Metadata 
    metadat <-  readxl::read_excel("Data/Base_fichas_indicadores.xlsx") %>%
      filter(CODINDICADOR == list_ind_pob[i]) %>% 
      select(DERECHO, CONINDICADOR, NOMINDICADOR, 
             DEFINICIÓN, CÁLCULO, TIPOIND, CITA) %>% 
      gather(key = "", value = " ")
    
    # Excel merge
    lista_excel <- list("Datos" = dat_cut,
                        "Ficha técnica" = metadat)
    
    # Guardar
    openxlsx::write.xlsx(lista_excel, 
                         file = paste0("indicadores/tablas/pob/", list_ind_pob[i], "_edad.xlsx")) 
    
  } else {
    
    print(paste(list_ind_pob[i], "no tiene corte por edad"))
    
  }
  
  ## Gráficos ascendencia  
  dat_ascendencia <- dat %>%
    filter(codindicador == list_ind_pob[i]) %>%
    filter(corte == "Ascendencia étnico-racial")
  
  if(nrow(dat_ascendencia) > 0 & max(dat_ascendencia$ascendencia_pob) == 1){
    
    # Base para último valor
    data_ends <- dat_ascendencia %>%
      group_by(ascendencia) %>% 
      arrange(desc(Fecha)) %>%
      slice(1)
    
    # Gráfico
    temp_plot <- ggplot(dat_ascendencia,
                        aes(x = fecha, y = Valor,  color = ascendencia)) +
      geom_line(size = 1, alpha = .6) +
      geom_point(size = 2.5) +
      theme_m(base_size = 9) +
      labs(title = unique(dat_ascendencia$nomindicador),
           x = "",
           y = "",
           caption = wrapit(unique(dat_ascendencia$cita))) +
      geom_text_repel(
        aes(label = Valor), data = data_ends,
        size = 7,
        vjust = -1,
        show.legend = FALSE) +
      ylim(min(dat_ascendencia$Valor) - min(dat_ascendencia$Valor)/10, max(dat_ascendencia$Valor) + max(dat_ascendencia$Valor)/10 )  +
      scale_color_manual(name = "", values = paleta_expandida) + 
      theme(legend.position = "bottom",
            legend.text = element_text(size=10))
    
    # Guardar
    ggsave(temp_plot, 
           file = paste0("indicadores/viz/pob/", list_ind_pob[i], "_ascendencia", ".png"), 
           width = 40, height = 25, units = "cm")
    
    
    ## Excel para exportar
    dat_cut <- dat_ascendencia %>% 
      select(Fecha, Valor, ascendencia) %>% 
      pivot_wider(names_from = "ascendencia",
                  values_from = "Valor")
    
    # Metadata 
    metadat <-  readxl::read_excel("Data/Base_fichas_indicadores.xlsx") %>%
      filter(CODINDICADOR == list_ind_pob[i]) %>% 
      select(DERECHO, CONINDICADOR, NOMINDICADOR, 
             DEFINICIÓN, CÁLCULO, TIPOIND, CITA) %>% 
      gather(key = "", value = " ")
    
    # Excel merge
    lista_excel <- list("Datos" = dat_cut,
                        "Ficha técnica" = metadat)
    
    # Guardar
    openxlsx::write.xlsx(lista_excel, 
                         file = paste0("indicadores/tablas/pob/", list_ind_pob[i], "_ascendencia.xlsx")) 
    
  } else {
    
    print(paste(list_ind_pob[i], "no tiene corte por ascendencia"))
    
  }
  
  # Estado loop
  print(list_ind_pob[i])
  
}


## Solución manual para indicador 430101 (gráficos por facetas)

#### Sexo
dat_430101 <- dat %>%
  filter(codindicador == 430101) %>%
  filter(corte == "Sexo")

# Base para último valor
data_ends <- dat_430101 %>%
  filter(prestador != "Otros") %>% 
  group_by(sexo, prestador) %>% 
  arrange(desc(Fecha)) %>%
  slice(1)

# Gráfico
temp_plot <- ggplot(dat_430101, aes(x = fecha, y = Valor,  color = prestador)) +
  geom_line(size = 1, alpha = .6) +
  geom_point(size = 2.5) +
  theme_m(base_size = 9) +
  labs(title = unique(dat_430101$nomindicador),
       x = "",
       y = "",
       caption = wrapit(unique(dat_430101$cita))) +
  geom_text_repel(
    aes(label = Valor), data = data_ends,
    size = 7,
    vjust = -1,
    show.legend = FALSE) +
  ylim(min(dat_430101$Valor) - min(dat_430101$Valor)/10, max(dat_430101$Valor) + max(dat_430101$Valor)/10 )  +
  scale_color_manual(name = "", values = paleta_expandida) + 
  theme(legend.position = "bottom",
        legend.text = element_text(size=10)) +
  facet_wrap(~ sexo)

# Guardar
ggsave(temp_plot, 
       file = "indicadores/viz/pob/430101_sexo.png", 
       width = 40, height = 25, units = "cm")

## Excel para exportar
dat_cut <- dat_430101 %>% 
  select(Fecha, Valor, prestador, sexo) %>% 
  pivot_wider(names_from = "sexo",
              values_from = "Valor")

# Metadata 
metadat <-  readxl::read_excel("Data/Base_fichas_indicadores.xlsx") %>%
  filter(CODINDICADOR == 430101) %>% 
  select(DERECHO, CONINDICADOR, NOMINDICADOR, 
         DEFINICIÓN, CÁLCULO, TIPOIND, CITA) %>% 
  gather(key = "", value = " ")

# Excel merge
lista_excel <- list("Datos" = dat_cut,
                    "Ficha técnica" = metadat)

# Guardar
openxlsx::write.xlsx(lista_excel, 
                     file = paste0("indicadores/tablas/pob/430101_sexo.xlsx")) 


#### Edad
dat_430101 <- dat %>%
  filter(codindicador == 430101) %>%
  filter(corte == "Edad")

# Base para último valor
data_ends <- dat_430101 %>%
  filter(prestador != "Otros") %>% 
  group_by(edad, prestador) %>% 
  arrange(desc(Fecha)) %>%
  slice(1)

# Gráfico
temp_plot <- ggplot(dat_430101, aes(x = fecha, y = Valor,  color = prestador)) +
  geom_line(size = 1, alpha = .6) +
  geom_point(size = 2.5) +
  theme_m(base_size = 9) +
  labs(title = unique(dat_430101$nomindicador),
       x = "",
       y = "",
       caption = wrapit(unique(dat_430101$cita))) +
  geom_text_repel(
    aes(label = Valor), data = data_ends,
    size = 5,
    vjust = -1,
    show.legend = FALSE) +
  ylim(min(dat_430101$Valor) - min(dat_430101$Valor)/10, max(dat_430101$Valor) + max(dat_430101$Valor)/10 )  +
  scale_color_manual(name = "", values = paleta_expandida) + 
  theme(legend.position = "bottom",
        legend.text = element_text(size=10)) +
  facet_wrap(~ edad)

# Guardar
ggsave(temp_plot, 
       file = "indicadores/viz/pob/430101_edad.png", 
       width = 40, height = 25, units = "cm")

## Excel para exportar
dat_cut <- dat_430101 %>% 
  select(Fecha, Valor, prestador, edad) %>% 
  pivot_wider(names_from = "edad",
              values_from = "Valor")

# Metadata 
metadat <-  readxl::read_excel("Data/Base_fichas_indicadores.xlsx") %>%
  filter(CODINDICADOR == 430101) %>% 
  select(DERECHO, CONINDICADOR, NOMINDICADOR, 
         DEFINICIÓN, CÁLCULO, TIPOIND, CITA) %>% 
  gather(key = "", value = " ")

# Excel merge
lista_excel <- list("Datos" = dat_cut,
                    "Ficha técnica" = metadat)

# Guardar
openxlsx::write.xlsx(lista_excel, 
                     file = paste0("indicadores/tablas/pob/430101_edad.xlsx")) 


#### Ascendencia
dat_430101 <- dat %>%
  filter(codindicador == 430101) %>%
  filter(prestador != "Otros") %>%
  filter(corte == "Ascendencia étnico-racial")

# Base para último valor
data_ends <- dat_430101 %>%
  filter(prestador != "Otros") %>% 
  group_by(ascendencia, prestador) %>% 
  arrange(desc(Fecha)) %>%
  slice(1)
  
# Gráfico
temp_plot <- ggplot(dat_430101, aes(x = fecha, y = Valor,  color = prestador)) +
    geom_line(size = 1, alpha = .6) +
    geom_point(size = 2.5) +
    theme_m(base_size = 9) +
    labs(title = unique(dat_430101$nomindicador),
         x = "",
         y = "",
         caption = wrapit(unique(dat_430101$cita))) +
    geom_text_repel(
      aes(label = Valor), data = data_ends,
      size = 7,
      vjust = -1,
      show.legend = FALSE) +
    ylim(min(dat_430101$Valor) - min(dat_430101$Valor)/10, max(dat_430101$Valor) + max(dat_430101$Valor)/10 )  +
    scale_color_manual(name = "", values = paleta_expandida) + 
    theme(legend.position = "bottom",
          legend.text = element_text(size=10)) +
  facet_wrap(~ ascendencia)
  
# Guardar
ggsave(temp_plot, 
         file = "indicadores/viz/pob/430101_ascendencia.png", 
         width = 40, height = 25, units = "cm")

## Excel para exportar
dat_cut <- dat_430101 %>% 
  select(Fecha, Valor, prestador, ascendencia) %>% 
  pivot_wider(names_from = "ascendencia",
              values_from = "Valor")

# Metadata 
metadat <-  readxl::read_excel("Data/Base_fichas_indicadores.xlsx") %>%
  filter(CODINDICADOR == 430101) %>% 
  select(DERECHO, CONINDICADOR, NOMINDICADOR, 
         DEFINICIÓN, CÁLCULO, TIPOIND, CITA) %>% 
  gather(key = "", value = " ")

# Excel merge
lista_excel <- list("Datos" = dat_cut,
                    "Ficha técnica" = metadat)

# Guardar
openxlsx::write.xlsx(lista_excel, 
                     file = paste0("indicadores/tablas/pob/430101_ascendencia.xlsx")) 


## 4. Loop excel  =========================================================


for (i in seq_along(list_ind)) {
  
  # Filtro base para cada indicador
  dat_ind <- dat %>% 
    filter(web == 1) %>%
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
      select(DERECHO, CONINDICADOR, NOMINDICADOR, 
             DEFINICIÓN, CÁLCULO, TIPOIND, CITA) %>% 
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
      select(DERECHO, CONINDICADOR, NOMINDICADOR, 
             DEFINICIÓN, CÁLCULO, TIPOIND, CITA) %>% 
      gather(key = "", value = " ")
    
    # Excel merge
    lista_excel <- list("Datos" = dat_cut,
                        "Ficha técnica" = metadat)
    
    # Guardar
    openxlsx::write.xlsx(lista_excel, 
                         file = paste0("indicadores/tablas/", list_ind[i], ".xlsx")) 
  }

}

