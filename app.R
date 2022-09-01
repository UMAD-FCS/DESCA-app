
## ***************************************************************************
## Shiny app para Mirador DESCA
## 11/2021
## ***************************************************************************


##  0. DATA Y PAQUETES  ======================================================

library(DT)
library(shinythemes)
library(here)
library(plotly)
library(shinyWidgets)
library(stringr)
library(scales)
library(viridis)
library(tidyverse)
library(bslib)
library(shinycssloaders)

source("utils.R")

theme_desca <- bs_theme(
    version = 4,
    bg = "#FFFFFF", fg = "#21618C", 
    primary = "#21618C",
    base_font = font_google("Poppins"),
    code_font = font_google("Poppins"),
    heading_font = font_google("Poppins"),
    font_scale = 0.9
)

color_defecto <- "#21618C"
# bs_theme_preview(theme_desca)

# Spinner options 
options(spinner.color = "#21618C",
        spinner.color.background="#ffffff", 
        spinner.size = 2)

# thematic::thematic_shiny() 
# 
theme_set(theme_bdd(base_size = 12))
update_geom_defaults("text", list(family = theme_get()$text$family))

dir.create('~/.fonts')
file.copy("www/Titillium Web.ttf", "~/.fonts")
system('fc-cache -f ~/.fonts')


##  1.  PREPARAR DATA  ======================================================

dat <- load("Data/data_motor.rda")

# Pasar luego estas transformaciones a data-raw
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

dat$departamento <- chartr("ÁÉÍÓÚ", "AEIOU", dat$departamento)

# Cargar geometría para mapas
dep <- geouy::load_geouy("Departamentos")

# Lista de indicadores
ind_edu_pp <- dat %>% 
    filter(derecho == "Educación",
           tipoind == "Políticas Públicas y Esfuerzo Económico") %>% 
    arrange() %>% 
    distinct(nomindicador) %>% 
    pull()

ind_edu_r <- dat %>% 
    filter(derecho == "Educación",
           tipoind == "Resultados") %>% 
    arrange() %>% 
    distinct(nomindicador) %>% 
    pull()

ind_salud_pp <- dat %>% 
    filter(derecho == "Salud",
           tipoind == "Políticas Públicas y Esfuerzo Económico") %>% 
    arrange() %>% 
    distinct(nomindicador) %>% 
    pull()

ind_salud_r <- dat %>% 
    filter(derecho == "Salud",
           tipoind == "Resultados") %>% 
    arrange() %>% 
    distinct(nomindicador) %>% 
    pull()

ind_ssocial_pp <- dat %>% 
    filter(derecho == "Seguridad Social",
           tipoind == "Políticas Públicas y Esfuerzo Económico") %>% 
    arrange() %>% 
    distinct(nomindicador) %>% 
    pull()

ind_ssocial_r <- dat %>% 
    filter(derecho == "Seguridad Social",
           tipoind == "Resultados") %>% 
    arrange() %>% 
    distinct(nomindicador) %>% 
    pull()

ind_vivienda_pp <- dat %>% 
    filter(derecho == "Vivienda",
           tipoind == "Políticas Públicas y Esfuerzo Económico") %>% 
    arrange() %>% 
    distinct(nomindicador) %>% 
    pull()

ind_vivienda_r <- dat %>% 
    filter(derecho == "Vivienda",
           tipoind == "Resultados") %>% 
    arrange() %>% 
    distinct(nomindicador) %>% 
    pull()

ind_trabajo_pp <- dat %>% 
  filter(derecho == "Trabajo",
         tipoind == "Políticas Públicas y Esfuerzo Económico") %>% 
  arrange() %>% 
  distinct(nomindicador) %>% 
  pull()

ind_trabajo_r <- dat %>% 
  filter(derecho == "Trabajo",
         tipoind == "Resultados") %>% 
  arrange() %>% 
  distinct(nomindicador) %>% 
  pull()

ind_ambiente_pp <- dat %>% 
  filter(derecho == "Ambiente",
         tipoind == "Políticas Públicas y Esfuerzo Económico") %>% 
  arrange() %>% 
  distinct(nomindicador) %>% 
  pull()

ind_ambiente_r <- dat %>% 
  filter(derecho == "Ambiente",
         tipoind == "Resultados") %>% 
  arrange() %>% 
  distinct(nomindicador) %>% 
  pull()

# Lista indicadores con 2 cortes  
lista_ind_2 <- dat %>% 
  filter(!is.na(corte_2)) %>% 
  distinct(nomindicador) %>% 
  pull()

test <- dat %>% 
  filter(nomindicador == "Demanda Bioquímica de Oxígeno (DBO5) en agua superficial (mgO2/L)")

# Lista indicadore con valores únicos
lista_vunico <- dat %>% 
  group_by(nomindicador) %>% 
  distinct(fecha_cat) %>% 
  summarise(n = n()) %>% 
  filter(n == 1) %>% 
  pull(nomindicador)

# Paleta de colores expandida
library(RColorBrewer)
paleta_expandida <- c(brewer.pal(8, "Dark2"), "#B76A16", "#75A61A", "#D9318E",
                      "#986A74", "#C14D6A", "#C1632B", "#698446", "#7B6BB0",
                      "#A9A80F", "#DEAA03")


##  2.  USER INTERFACE  =====================================================

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    tags$style(".fa-calculator {color:#21618C}"),
    tags$head(HTML("<title>Mirador DESCA</title>")),
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"), # Quita mensajes de error (ojo)
    
    navbarPage(
        title = tags$a(
          href="http://miradordesca.uy/", 
          tags$img(src="logodesca.png", 
                   style="margin-top: -2px;", height = 30, width = 100,
                   height=30,
                   width = 100)
        ),
        
        # div(img(src='logodesca.png', style="margin-top: -2px;", height = 30, width = 100)),
        # titlePanel(title=div(img(src="logo_umad.png", height="5%", width="5%"), "Mirador DESCA")),
        # title = div("Mirador DESCA", img(src="logo_umad.png", height="90%", width = "90%")),
        collapsible = TRUE,
        fluid = TRUE,
        # theme = shinytheme("cerulean"),
        theme = theme_desca,
        # theme = bs_theme(version = 3, bootswatch = "united"),
        
        ## Educación    =====================================================
        
        tabPanel(
            title = "Educación", icon = icon("fas fa-user-graduate"),
            
            tabsetPanel(
                type = "pills",
                id   = "CP",
                
                tabPanel(
                    "Políticas Públicas y esfuerzo económico",
                    icon = icon("fas fa-chart-bar"),

                    br(),
                    
                    fluidRow(
                        
                        sidebarPanel(
                            
                            width = 3,
    
                             selectInput(
                                 inputId = "indicador_edu_pp",
                                 label = "Seleccione indicador:",
                                 choices = ind_edu_pp
                             ),
                            
                            uiOutput("selector_edu_pp_corte_2"),
                            
                            uiOutput("selector_edu_pp_corte"),
                            
                            uiOutput("s_edu_pp_fecha"),
                            
                            uiOutput("chbox_edu_pp"),
                            
                            HTML("<b> Instituciones:</b>"),
                            br(),
                            tags$a(href="https://www.gub.uy/institucion-nacional-derechos-humanos-uruguay/",
                                   tags$img(src="INDDHH-Logo.png",
                                            style=";vertical-align:top;",
                                            # title="Example Image Link", 
                                            height="75%",
                                            width = "65%")),
                            br(),
                            br(),
                            tags$a( href="https://umad.cienciassociales.edu.uy/",
                                    tags$img(src="logo_umad.png",
                                             style=";vertical-align:top;",
                                             height="75%",
                                             width = "65%")),
                            br(),
                            br(),
                            HTML("<b> Con el apoyo de:</b>"),    
                            br(),
                            tags$a(href="https://www.ohchr.org/SP/Countries/LACRegion/Pages/SouthAmerica2010.aspx",
                                   tags$img(src="ohchr.png",
                                            style=";vertical-align:top;",
                                            # title="Example Image Link", 
                                            height="60%",
                                            width = "45%")),
                        ),
                        
                        mainPanel(
                            
                            tags$h3(style="display:inline-block;margin: 0px;",
                                    uiOutput("title_edu_pp")),
                            div(style="display:inline-block;margin: 0px;", 
                                dropdown(
                                    style = "minimal",
                                    status = "primary",
                                    width = "500px",
                                    right = TRUE,
                                    icon = icon("calculator", lib = "font-awesome"),
                                    uiOutput("calculo_edu_pp"))
                            ),
                            tags$h5(uiOutput("subtitle_edu_pp")),
                            br(),
                            withSpinner(plotOutput("plot_edu_pp", height = "500px"),
                                        type = 2),
                            br(),
                            fluidRow(column(12, div(downloadButton(outputId = "baja_p_edu_pp",
                                                                   label = "Descarga el gráfico"),
                                                    style = "float: right"))),
                            br(),
                            br(),
                            DTOutput("table_edu_pp"),
                            br(),
                            br(),
                            fluidRow(column(12, div(downloadButton("dwl_tab_edu_pp",
                                                                   "Descarga la tabla"),
                                                    style = "float: right"))),
                            br(),
                            br()
                            
                            )
                    )
                    
                    ),
                
                # * Resultados ----
                
                tabPanel(
                    "Resultados",
                    icon = icon("fas fa-chart-bar"),
                    
                    br(),
                
                    fluidRow(
                        
                        sidebarPanel(
                            
                            width = 3,
                            
                            selectInput(
                                inputId = "indicador_edu_r",
                                label = "Seleccione indicador:",
                                choices = ind_edu_r
                            ),
                            
                            uiOutput("selector_edu_r_corte_2"),
                            
                            uiOutput("selector_edu_r_corte"),
                            
                            uiOutput("s_edu_r_fecha"),
                            
                            uiOutput("chbox_edu_r"),
                            
                            HTML("<b> Instituciones:</b>"),
                            br(),
                            tags$a(href="https://www.gub.uy/institucion-nacional-derechos-humanos-uruguay/",
                                   tags$img(src="INDDHH-Logo.png",
                                            style=";vertical-align:top;",
                                            # title="Example Image Link", 
                                            height="75%",
                                            width = "65%")),
                            br(),
                            br(),
                            tags$a( href="https://umad.cienciassociales.edu.uy/",
                                    tags$img(src="logo_umad.png",
                                             style=";vertical-align:top;",
                                             height="75%",
                                             width = "65%")),
                            br(),
                            br(),
                            HTML("<b> Con el apoyo de:</b>"),    
                            br(),
                            tags$a(href="https://www.ohchr.org/SP/Countries/LACRegion/Pages/SouthAmerica2010.aspx",
                                   tags$img(src="ohchr.png",
                                            style=";vertical-align:top;",
                                            # title="Example Image Link", 
                                            height="60%",
                                            width = "45%")),
                        ),
                        
                        mainPanel(
                            
                            tags$h3(style="display:inline-block",
                                    uiOutput("title_edu_r")),
                            div(style="display:inline-block", 
                                dropdown(
                                    style = "minimal",
                                    status = "primary",
                                    width = "500px",
                                    right = TRUE,
                                    icon = icon("calculator", lib = "font-awesome"),
                                    uiOutput("calculo_edu_r"))
                            ),
                            tags$h5(uiOutput("subtitle_edu_r")),
                            br(),
                            withSpinner(plotOutput("plot_edu_r", height = "500px" ),
                                        type = 2),
                            br(),
                            fluidRow(column(12, div(downloadButton(outputId = "baja_p_edu_r",
                                                                   label = "Descarga el gráfico"),
                                                    style = "float: right"))),
                            br(),
                            br(),
                            DTOutput("table_edu_r"),
                            br(),
                            br(),
                            fluidRow(column(12, div(downloadButton("dwl_tab_edu_r", 
                                                                   "Descarga la tabla"),
                                                    style = "float: right"))),
                            br(),
                            br()
                            )
                            
                        )
                )
            )
        ),
        
        ## Salud    =========================================================
        
        tabPanel(
            title = "Salud", icon = icon("fas fa-plus-square"),
            
            tabsetPanel(
                type = "pills",
                id   = "CP",
                
                tabPanel(
                    "Políticas Públicas y esfuerzo económico", 
                    icon = icon("fas fa-chart-bar"),
                    br(),
                    
                    fluidRow(
                        
                        sidebarPanel(
                        
                            width = 3,
                            
                            selectInput(
                                inputId = "indicador_salud_pp",
                                label = "Seleccione indicador:",
                                choices = ind_salud_pp
                            ),
                            
                            uiOutput("selector_salud_pp_corte_2"),
                            
                            uiOutput("selector_salud_pp_corte"),
                            
                            uiOutput("s_salud_pp_fecha"),
                            
                            uiOutput("chbox_salud_pp"),
                            
                            HTML("<b> Instituciones:</b>"),
                            br(),
                            tags$a(href="https://www.gub.uy/institucion-nacional-derechos-humanos-uruguay/",
                                   tags$img(src="INDDHH-Logo.png",
                                            style=";vertical-align:top;",
                                            # title="Example Image Link", 
                                            height="75%",
                                            width = "65%")),
                            br(),
                            br(),
                            tags$a( href="https://umad.cienciassociales.edu.uy/",
                                    tags$img(src="logo_umad.png",
                                             style=";vertical-align:top;",
                                             height="75%",
                                             width = "65%")),
                            br(),
                            br(),
                            HTML("<b> Con el apoyo de:</b>"),    
                            br(),
                            tags$a(href="https://www.ohchr.org/SP/Countries/LACRegion/Pages/SouthAmerica2010.aspx",
                                   tags$img(src="ohchr.png",
                                            style=";vertical-align:top;",
                                            # title="Example Image Link", 
                                            height="60%",
                                            width = "45%")),
                        ),
                        
                        mainPanel(
                            
                            tags$h3(style="display:inline-block",
                                    uiOutput("title_salud_pp")),
                            div(style="display:inline-block", 
                                dropdown(
                                    style = "minimal",
                                    status = "primary",
                                    width = "500px",
                                    right = TRUE,
                                    icon = icon("calculator", lib = "font-awesome"),
                                    uiOutput("calculo_salud_pp"))
                            ),
                            tags$h5(uiOutput("subtitle_salud_pp")),
                            br(),
                            withSpinner(plotOutput("plot_salud_pp", height = "500px" ),
                                        type = 2),
                            br(),
                            fluidRow(column(12, div(downloadButton(outputId = "baja_p_salud_pp",
                                                                   label = "Descarga el gráfico"),
                                                    style = "float: right"))),
                            br(),
                            br(),
                            DTOutput("table_salud_pp"),
                            br(),
                            br(),
                            fluidRow(column(12, div(downloadButton("dwl_tab_salud_pp", "Descarga la tabla"),
                                                    style = "float: right"))),
                            br(),
                            br()
                        )
                    )
                    
                ),
                
                # * Resultados ----
                
                tabPanel(
                    "Resultados", 
                    icon = icon("fas fa-chart-bar"),

                    br(),
                    
                    fluidRow(
                        
                        sidebarPanel(
                            
                            width = 3,
                            
                            selectInput(
                                inputId = "indicador_salud_r",
                                label = "Seleccione indicador:",
                                choices = ind_salud_r,
                                selected = "Esperanza de vida al nacer"
                            ),
                            
                            uiOutput("selector_salud_r_corte_2"),
                            
                            uiOutput("selector_salud_r_corte"),
                            
                            uiOutput("s_salud_r_fecha"),
                            
                            uiOutput("chbox_salud_r"),
                            
                            HTML("<b> Instituciones:</b>"),
                            br(),
                            tags$a(href="https://www.gub.uy/institucion-nacional-derechos-humanos-uruguay/",
                                   tags$img(src="INDDHH-Logo.png",
                                            style=";vertical-align:top;",
                                            # title="Example Image Link", 
                                            height="75%",
                                            width = "65%")),
                            br(),
                            br(),
                            tags$a( href="https://umad.cienciassociales.edu.uy/",
                                    tags$img(src="logo_umad.png",
                                             style=";vertical-align:top;",
                                             height="75%",
                                             width = "65%")),
                            br(),
                            br(),
                            HTML("<b> Con el apoyo de:</b>"),    
                            br(),
                            tags$a(href="https://www.ohchr.org/SP/Countries/LACRegion/Pages/SouthAmerica2010.aspx",
                                   tags$img(src="ohchr.png",
                                            style=";vertical-align:top;",
                                            # title="Example Image Link", 
                                            height="60%",
                                            width = "45%")),
                        ),
                        
                        mainPanel(
                            
                            tags$h3(style="display:inline-block",
                                    uiOutput("title_salud_r")),
                            div(style="display:inline-block", 
                                dropdown(
                                    style = "minimal",
                                    status = "primary",
                                    width = "500px",
                                    right = TRUE,
                                    icon = icon("calculator", lib = "font-awesome"),
                                    uiOutput("calculo_salud_r"))
                            ),
                            tags$h5(uiOutput("subtitle_salud_r")),
                            br(),
                            withSpinner(plotOutput("plot_salud_r", height = "500px" ),
                                        type = 2),
                            br(),
                            fluidRow(column(12, div(downloadButton(outputId = "baja_p_salud_r", 
                                           label = "Descarga el gráfico"),
                                           style = "float: right"))),
                            br(),
                            br(),
                            DTOutput("table_salud_r"),
                            br(),
                            br(),
                            fluidRow(column(12, div(downloadButton("dwl_tab_salud_r", "Descarga la tabla"),
                                                    style = "float: right"))),
                            br(),
                            br()
                            )
                        )
                )
            )
        ),
        
        ## Seguridad Social    ==============================================
        
        tabPanel(
            title = "Seguridad Social", icon = icon("briefcase"),
            
            tabsetPanel(
                type = "pills",
                id   = "CP",
                
                tabPanel(
                    "Políticas Públicas y esfuerzo económico", 
                    icon = icon("fas fa-chart-bar"),
                    
                    br(),
                    
                    fluidRow(
                        
                    
                        sidebarPanel(
                            
                            width = 3,
                            
                            selectInput(
                                inputId = "indicador_ssocial_pp",
                                label = "Seleccione indicador:",
                                choices = ind_ssocial_pp
                            ),
                            
                            uiOutput("selector_ssocial_pp_corte_2"),
                            
                            uiOutput("selector_ssocial_pp_corte"),
                            
                            uiOutput("s_ssocial_pp_fecha"),
                            
                            uiOutput("chbox_ssocial_pp"),
                            
                            HTML("<b> Instituciones:</b>"),
                            br(),
                            tags$a(href="https://www.gub.uy/institucion-nacional-derechos-humanos-uruguay/",
                                   tags$img(src="INDDHH-Logo.png",
                                            style=";vertical-align:top;",
                                            # title="Example Image Link", 
                                            height="75%",
                                            width = "65%")),
                            br(),
                            br(),
                            tags$a( href="https://umad.cienciassociales.edu.uy/",
                                    tags$img(src="logo_umad.png",
                                             style=";vertical-align:top;",
                                             height="75%",
                                             width = "65%")),
                            br(),
                            br(),
                            HTML("<b> Con el apoyo de:</b>"),    
                            br(),
                            tags$a(href="https://www.ohchr.org/SP/Countries/LACRegion/Pages/SouthAmerica2010.aspx",
                                   tags$img(src="ohchr.png",
                                            style=";vertical-align:top;",
                                            # title="Example Image Link", 
                                            height="60%",
                                            width = "45%")),
                        ),
                        
                        mainPanel(
                            
                            tags$h3(style="display:inline-block",
                                    uiOutput("title_ssocial_pp")),
                            div(style="display:inline-block", 
                                dropdown(
                                    style = "minimal",
                                    status = "primary",
                                    width = "500px",
                                    right = TRUE,
                                    icon = icon("calculator", lib = "font-awesome"),
                                    uiOutput("calculo_ssocial_pp"))
                            ),
                            tags$h5(uiOutput("subtitle_ssocial_pp")),
                            br(),
                            withSpinner(plotOutput("plot_ssocial_pp", height = "500px" ),
                                        type = 2),
                            br(),
                            fluidRow(column(12, div(downloadButton(outputId = "baja_p_ssocial_pp", 
                                           label = "Descarga el gráfico"),
                                           style = "float: right"))),
                            br(),
                            br(),
                            DTOutput("table_ssocial_pp"),
                            br(),
                            br(),
                            fluidRow(column(12, div(downloadButton("dwl_tab_ssocial_pp", "Descarga la tabla"),
                                                    style = "float: right"))),
                            br(),
                            br()
                        )
                    )
                ),
                
                # * Resultados ----
                
                tabPanel(
                    "Resultados", 
                    icon = icon("fas fa-chart-bar"),
                    
                    br(),
                    
                    fluidRow(
                        
                        sidebarPanel(
                            
                            width = 3,
                            
                            selectInput(
                                inputId = "indicador_ssocial_r",
                                label = "Seleccione indicador:",
                                choices = ind_ssocial_r
                            ),
                            
                            uiOutput("selector_ssocial_r_corte_2"),
                            
                            uiOutput("selector_ssocial_r_corte"),
                            
                            uiOutput("s_ssocial_r_fecha"),
                            
                            uiOutput("chbox_ssocial_r"),
                            
                            HTML("<b> Instituciones:</b>"),
                            br(),
                            tags$a(href="https://www.gub.uy/institucion-nacional-derechos-humanos-uruguay/",
                                   tags$img(src="INDDHH-Logo.png",
                                            style=";vertical-align:top;",
                                            # title="Example Image Link", 
                                            height="75%",
                                            width = "65%")),
                            br(),
                            br(),
                            tags$a( href="https://umad.cienciassociales.edu.uy/",
                                    tags$img(src="logo_umad.png",
                                             style=";vertical-align:top;",
                                             height="75%",
                                             width = "65%")),
                            br(),
                            br(),
                            HTML("<b> Con el apoyo de:</b>"),    
                            br(),
                            tags$a(href="https://www.ohchr.org/SP/Countries/LACRegion/Pages/SouthAmerica2010.aspx",
                                   tags$img(src="ohchr.png",
                                            style=";vertical-align:top;",
                                            # title="Example Image Link", 
                                            height="60%",
                                            width = "45%")),
                        ),
                        
                        mainPanel(
                            
                            tags$h3(style="display:inline-block",
                                    uiOutput("title_ssocial_r")),
                            div(style="display:inline-block", 
                                dropdown(
                                    style = "minimal",
                                    status = "primary",
                                    width = "500px",
                                    right = TRUE,
                                    icon = icon("calculator", lib = "font-awesome"),
                                    uiOutput("calculo_ssocial_r"))
                            ),
                            tags$h5(uiOutput("subtitle_ssocial_r")),
                            br(),
                            withSpinner(plotOutput("plot_ssocial_r", height = "500px"),
                                        type = 2),
                            br(),
                            fluidRow(column(12, div(downloadButton(outputId = "baja_p_ssocial_r", 
                                           label = "Descarga el gráfico"),
                                           style = "float: right"))),
                            br(),
                            br(),
                            DTOutput("table_ssocial_r"),
                            br(),
                            br(),
                            fluidRow(column(12, div(downloadButton("dwl_tab_ssocial_r", "Descarga la tabla"),
                                                    style = "float: right"))),
                            br(),
                            br()
                        )
                    )
                )
            )
        ),
        
        ## Vivienda    =====================================================

        tabPanel(
            title = "Vivienda", icon = icon("home"),
            
            tabsetPanel(
                type = "pills",
                id   = "CP",
                
                tabPanel(
                    "Políticas Públicas y esfuerzo económico", 
                    icon = icon("fas fa-chart-bar"),
                    br(),
                    
                    fluidRow(
                        
                        sidebarPanel(
                            
                            width = 3,
                            
                            selectInput(
                                inputId = "indicador_vivienda_pp",
                                label = "Seleccione indicador:",
                                choices = ind_vivienda_pp
                            ),
                            
                            uiOutput("selector_vivienda_pp_corte_2"),
                            
                            uiOutput("selector_vivienda_pp_corte"),
                            
                            uiOutput("s_vivienda_pp_fecha"),
                            
                            uiOutput("chbox_vivienda_pp"),
                            
                            HTML("<b> Instituciones:</b>"),
                            br(),
                            tags$a(href="https://www.gub.uy/institucion-nacional-derechos-humanos-uruguay/",
                                   tags$img(src="INDDHH-Logo.png",
                                            style=";vertical-align:top;",
                                            # title="Example Image Link", 
                                            height="75%",
                                            width = "65%")),
                            br(),
                            br(),
                            tags$a( href="https://umad.cienciassociales.edu.uy/",
                                    tags$img(src="logo_umad.png",
                                             style=";vertical-align:top;",
                                             height="75%",
                                             width = "65%")),
                            br(),
                            br(),
                            HTML("<b> Con el apoyo de:</b>"),    
                            br(),
                            tags$a(href="https://www.ohchr.org/SP/Countries/LACRegion/Pages/SouthAmerica2010.aspx",
                                   tags$img(src="ohchr.png",
                                            style=";vertical-align:top;",
                                            # title="Example Image Link", 
                                            height="60%",
                                            width = "45%")),
                        ),
                        
                        mainPanel(
                            
                            tags$h3(style="display:inline-block",
                                    uiOutput("title_vivienda_pp")),
                            div(style="display:inline-block", 
                                dropdown(
                                    style = "minimal",
                                    status = "primary",
                                    width = "500px",
                                    right = TRUE,
                                    icon = icon("calculator", lib = "font-awesome"),
                                    uiOutput("calculo_vivienda_pp"))
                            ),
                            tags$h5(uiOutput("subtitle_vivienda_pp")),
                            br(),
                            withSpinner(plotOutput("plot_vivienda_pp", height = "500px"),
                                        type = 2),
                            br(),
                            fluidRow(column(12, div(downloadButton(outputId = "baja_p_vivienda_pp", 
                                           label = "Descarga el gráfico"),
                                           style = "float: right"))),
                            br(),
                            br(),
                            DTOutput("table_vivienda_pp"),
                            br(),
                            br(),
                            fluidRow(column(12, div(downloadButton("dwl_tab_vivienda_pp", "Descarga la tabla"),
                                                    style = "float: right"))),
                            br(),
                            br()
                        )
                    )
                ),
                
                # * Resultados ----
                
                
                tabPanel(
                    "Resultados", 
                    icon = icon("fas fa-chart-bar"),
                    
                    br(),
                    
                    fluidRow(
                        
                        sidebarPanel(
                            
                            width = 3,
                            
                            selectInput(
                                inputId = "indicador_vivienda_r",
                                label = "Seleccione indicador:",
                                choices = ind_vivienda_r
                            ),
                            
                            uiOutput("selector_vivienda_r_corte_2"),
                            
                            uiOutput("selector_vivienda_r_corte"),
                            
                            uiOutput("s_vivienda_r_fecha"),
                            
                            uiOutput("chbox_vivienda_r"),
                            
                            HTML("<b> Instituciones:</b>"),
                            br(),
                            tags$a(href="https://www.gub.uy/institucion-nacional-derechos-humanos-uruguay/",
                                   tags$img(src="INDDHH-Logo.png",
                                            style=";vertical-align:top;",
                                            # title="Example Image Link", 
                                            height="75%",
                                            width = "65%")),
                            br(),
                            br(),
                            tags$a( href="https://umad.cienciassociales.edu.uy/",
                                    tags$img(src="logo_umad.png",
                                             style=";vertical-align:top;",
                                             height="75%",
                                             width = "65%")),
                            br(),
                            br(),
                            HTML("<b> Con el apoyo de:</b>"),    
                            br(),
                            tags$a(href="https://www.ohchr.org/SP/Countries/LACRegion/Pages/SouthAmerica2010.aspx",
                                   tags$img(src="ohchr.png",
                                            style=";vertical-align:top;",
                                            # title="Example Image Link", 
                                            height="60%",
                                            width = "45%")),
                        ),
                        
                        mainPanel(
                            
                            tags$h3(style="display:inline-block",
                                    uiOutput("title_vivienda_r")),
                            div(style="display:inline-block", 
                                dropdown(
                                    style = "minimal",
                                    status = "primary",
                                    width = "500px",
                                    right = TRUE,
                                    icon = icon("calculator", lib = "font-awesome"),
                                    uiOutput("calculo_vivienda_r"))
                            ),
                            tags$h5(uiOutput("subtitle_vivienda_r")),
                            br(),
                            withSpinner(plotOutput("plot_vivienda_r", height = "500px"),
                                        type = 2),
                            br(),
                            fluidRow(column(12, div(downloadButton(outputId = "baja_p_vivienda_r", 
                                           label = "Descarga el gráfico"),
                                           style = "float: right"))),
                            br(),
                            br(),
                            DTOutput("table_vivienda_r"),
                            br(),
                            br(),
                            fluidRow(column(12, div(downloadButton("dwl_tab_vivienda_r", "Descarga la tabla"),
                                                    style = "float: right"))),
                            br(),
                            br()
                        )
                    )
                )
            )
        ),
        
        ## Trabajo    =====================================================
        
        tabPanel(
          title = "Trabajo", icon = icon("fas fa-users-cog"),
          
          tabsetPanel(
            type = "pills",
            id   = "CP",
            
            tabPanel(
              "Políticas Públicas y esfuerzo económico", 
              icon = icon("fas fa-chart-bar"),
              br(),
              
              fluidRow(
                
                sidebarPanel(
                  
                  width = 3,
                  
                  selectInput(
                    inputId = "indicador_trabajo_pp",
                    label = "Seleccione indicador:",
                    choices = ind_trabajo_pp
                  ),
                  
                  uiOutput("selector_trabajo_pp_corte_2"),
                  
                  uiOutput("selector_trabajo_pp_corte"),
                  
                  uiOutput("s_trabajo_pp_fecha"),
                  
                  uiOutput("chbox_trabajo_pp"),
                  
                  HTML("<b> Instituciones:</b>"),
                  br(),
                  tags$a(href="https://www.gub.uy/institucion-nacional-derechos-humanos-uruguay/",
                         tags$img(src="INDDHH-Logo.png",
                                  style=";vertical-align:top;",
                                  # title="Example Image Link", 
                                  height="75%",
                                  width = "65%")),
                  br(),
                  br(),
                  tags$a( href="https://umad.cienciassociales.edu.uy/",
                          tags$img(src="logo_umad.png",
                                   style=";vertical-align:top;",
                                   height="75%",
                                   width = "65%")),
                  br(),
                  br(),
                  HTML("<b> Con el apoyo de:</b>"),    
                  br(),
                  tags$a(href="https://www.ohchr.org/SP/Countries/LACRegion/Pages/SouthAmerica2010.aspx",
                         tags$img(src="ohchr.png",
                                  style=";vertical-align:top;",
                                  # title="Example Image Link", 
                                  height="60%",
                                  width = "45%")),
                ),
                
                mainPanel(
                  
                  tags$h3(style="display:inline-block",
                          uiOutput("title_trabajo_pp")),
                  div(style="display:inline-block", 
                      dropdown(
                        style = "minimal",
                        status = "primary",
                        width = "500px",
                        right = TRUE,
                        icon = icon("calculator", lib = "font-awesome"),
                        uiOutput("calculo_trabajo_pp"))
                  ),
                  tags$h5(uiOutput("subtitle_trabajo_pp")),
                  br(),
                  withSpinner(plotOutput("plot_trabajo_pp", height = "500px"),
                              type = 2),
                  br(),
                  fluidRow(column(12, div(downloadButton(outputId = "baja_p_trabajo_pp", 
                                                         label = "Descarga el gráfico"),
                                          style = "float: right"))),
                  br(),
                  br(),
                  DTOutput("table_trabajo_pp"),
                  br(),
                  br(),
                  fluidRow(column(12, div(downloadButton("dwl_tab_trabajo_pp", "Descarga la tabla"),
                                          style = "float: right"))),
                  br(),
                  br()
                )
              )
            ),
            
            # * Resultados ----
            
            
            tabPanel(
              "Resultados", 
              icon = icon("fas fa-chart-bar"),
              
              br(),
              
              fluidRow(
                
                sidebarPanel(
                  
                  width = 3,
                  
                  selectInput(
                    inputId = "indicador_trabajo_r",
                    label = "Seleccione indicador:",
                    choices = ind_trabajo_r
                  ),
                  
                  uiOutput("selector_trabajo_r_corte_2"),
                  
                  uiOutput("selector_trabajo_r_corte"),
                  
                  uiOutput("s_trabajo_r_fecha"),
                  
                  uiOutput("chbox_trabajo_r"),
                  
                  HTML("<b> Instituciones:</b>"),
                  br(),
                  tags$a(href="https://www.gub.uy/institucion-nacional-derechos-humanos-uruguay/",
                         tags$img(src="INDDHH-Logo.png",
                                  style=";vertical-align:top;",
                                  # title="Example Image Link", 
                                  height="75%",
                                  width = "65%")),
                  br(),
                  br(),
                  tags$a( href="https://umad.cienciassociales.edu.uy/",
                          tags$img(src="logo_umad.png",
                                   style=";vertical-align:top;",
                                   height="75%",
                                   width = "65%")),
                  br(),
                  br(),
                  HTML("<b> Con el apoyo de:</b>"),    
                  br(),
                  tags$a(href="https://www.ohchr.org/SP/Countries/LACRegion/Pages/SouthAmerica2010.aspx",
                         tags$img(src="ohchr.png",
                                  style=";vertical-align:top;",
                                  # title="Example Image Link", 
                                  height="60%",
                                  width = "45%")),
                ),
                
                mainPanel(
                  
                  tags$h3(style="display:inline-block",
                          uiOutput("title_trabajo_r")),
                  div(style="display:inline-block", 
                      dropdown(
                        style = "minimal",
                        status = "primary",
                        width = "500px",
                        right = TRUE,
                        icon = icon("calculator", lib = "font-awesome"),
                        uiOutput("calculo_trabajo_r"))
                  ),
                  tags$h5(uiOutput("subtitle_trabajo_r")),
                  br(),
                  withSpinner(plotOutput("plot_trabajo_r", height = "500px"),
                              type = 2),
                  br(),
                  fluidRow(column(12, div(downloadButton(outputId = "baja_p_trabajo_r", 
                                                         label = "Descarga el gráfico"),
                                          style = "float: right"))),
                  br(),
                  br(),
                  DTOutput("table_trabajo_r"),
                  br(),
                  br(),
                  fluidRow(column(12, div(downloadButton("dwl_tab_trabajo_r", "Descarga la tabla"),
                                          style = "float: right"))),
                  br(),
                  br()
                )
              )
            )
          )
        ),
        
        ## Ambiente    =====================================================
        
        tabPanel(
          title = "Ambiente", icon = icon("fas fa-leaf"),
          
          tabsetPanel(
            type = "pills",
            id   = "CP",
            
            tabPanel(
              "Políticas Públicas y esfuerzo económico", 
              icon = icon("fas fa-chart-bar"),
              br(),
              
              fluidRow(
                
                sidebarPanel(
                  
                  width = 3,
                  
                  selectInput(
                    inputId = "indicador_ambiente_pp",
                    label = "Seleccione indicador:",
                    choices = ind_ambiente_pp
                  ),
                  
                  uiOutput("selector_ambiente_pp_corte_2"),
                  
                  uiOutput("selector_ambiente_pp_corte"),
                  
                  uiOutput("s_ambiente_pp_fecha"),
                  
                  uiOutput("chbox_ambiente_pp"),
                  
                  HTML("<b> Instituciones:</b>"),
                  br(),
                  tags$a(href="https://www.gub.uy/institucion-nacional-derechos-humanos-uruguay/",
                         tags$img(src="INDDHH-Logo.png",
                                  style=";vertical-align:top;",
                                  # title="Example Image Link", 
                                  height="75%",
                                  width = "65%")),
                  br(),
                  br(),
                  tags$a( href="https://umad.cienciassociales.edu.uy/",
                          tags$img(src="logo_umad.png",
                                   style=";vertical-align:top;",
                                   height="75%",
                                   width = "65%")),
                  br(),
                  br(),
                  HTML("<b> Con el apoyo de:</b>"),    
                  br(),
                  tags$a(href="https://www.ohchr.org/SP/Countries/LACRegion/Pages/SouthAmerica2010.aspx",
                         tags$img(src="ohchr.png",
                                  style=";vertical-align:top;",
                                  # title="Example Image Link", 
                                  height="60%",
                                  width = "45%")),
                ),
                
                mainPanel(
                  
                  tags$h3(style="display:inline-block",
                          uiOutput("title_ambiente_pp")),
                  div(style="display:inline-block", 
                      dropdown(
                        style = "minimal",
                        status = "primary",
                        width = "500px",
                        right = TRUE,
                        icon = icon("calculator", lib = "font-awesome"),
                        uiOutput("calculo_ambiente_pp"))
                  ),
                  tags$h5(uiOutput("subtitle_ambiente_pp")),
                  br(),
                  withSpinner(plotOutput("plot_ambiente_pp", height = "500px"),
                              type = 2),
                  br(),
                  fluidRow(column(12, div(downloadButton(outputId = "baja_p_ambiente_pp", 
                                                         label = "Descarga el gráfico"),
                                          style = "float: right"))),
                  br(),
                  br(),
                  DTOutput("table_ambiente_pp"),
                  br(),
                  br(),
                  fluidRow(column(12, div(downloadButton("dwl_tab_ambiente_pp", "Descarga la tabla"),
                                          style = "float: right"))),
                  br(),
                  br()
                )
              )
            ),
            
            # * Resultados ----
            
            
            tabPanel(
              "Resultados", 
              icon = icon("fas fa-chart-bar"),
              
              br(),
              
              fluidRow(
                
                sidebarPanel(
                  
                  width = 3,
                  
                  selectInput(
                    inputId = "indicador_ambiente_r",
                    label = "Seleccione indicador:",
                    choices = ind_ambiente_r
                  ),
                  
                  uiOutput("selector_ambiente_r_corte_2"),
                  
                  uiOutput("selector_ambiente_r_corte"),
                  
                  uiOutput("s_ambiente_r_fecha"),
                  
                  uiOutput("chbox_ambiente_r"),
                  
                  HTML("<b> Instituciones:</b>"),
                  br(),
                  tags$a(href="https://www.gub.uy/institucion-nacional-derechos-humanos-uruguay/",
                         tags$img(src="INDDHH-Logo.png",
                                  style=";vertical-align:top;",
                                  # title="Example Image Link", 
                                  height="75%",
                                  width = "65%")),
                  br(),
                  br(),
                  tags$a( href="https://umad.cienciassociales.edu.uy/",
                          tags$img(src="logo_umad.png",
                                   style=";vertical-align:top;",
                                   height="75%",
                                   width = "65%")),
                  br(),
                  br(),
                  HTML("<b> Con el apoyo de:</b>"),    
                  br(),
                  tags$a(href="https://www.ohchr.org/SP/Countries/LACRegion/Pages/SouthAmerica2010.aspx",
                         tags$img(src="ohchr.png",
                                  style=";vertical-align:top;",
                                  # title="Example Image Link", 
                                  height="60%",
                                  width = "45%")),
                ),
                
                mainPanel(
                  
                  tags$h3(style="display:inline-block",
                          uiOutput("title_ambiente_r")),
                  div(style="display:inline-block", 
                      dropdown(
                        style = "minimal",
                        status = "primary",
                        width = "500px",
                        right = TRUE,
                        icon = icon("calculator", lib = "font-awesome"),
                        uiOutput("calculo_ambiente_r"))
                  ),
                  tags$h5(uiOutput("subtitle_ambiente_r")),
                  br(),
                  withSpinner(plotOutput("plot_ambiente_r", height = "500px"),
                              type = 2),
                  br(),
                  fluidRow(column(12, div(downloadButton(outputId = "baja_p_ambiente_r", 
                                                         label = "Descarga el gráfico"),
                                          style = "float: right"))),
                  br(),
                  br(),
                  DTOutput("table_ambiente_r"),
                  br(),
                  br(),
                  fluidRow(column(12, div(downloadButton("dwl_tab_ambiente_r", "Descarga la tabla"),
                                          style = "float: right"))),
                  br(),
                  br()
                )
              )
            )
          )
        ),
    )
)


##  3.  SERVER  =============================================================

server <- function(input, output) {
    
  
  ### 3.1. Educación Políticas   ==============================================
  
  # 3.1.1. Data reactiva   =================================================
  
  dat_edu_pp <- reactive({
    
    req(input$indicador_edu_pp)
    
    dat %>%
      filter(nomindicador == input$indicador_edu_pp) 
    
  })
  
  output$selector_edu_pp_corte <- renderUI({
    
    selectInput(
      inputId = "edu_pp_corte",
      label = "Seleccione corte:",
      choices = dat_edu_pp() %>% 
        select(corte) %>%
        arrange(corte) %>% 
        unique() %>% 
        pull(),
      selected = dat_edu_pp() %>% 
        filter(jerarquia == "1") %>%  
        distinct(corte) %>% 
        pull()
    )
    
  })
  
  output$selector_edu_pp_corte_2 <- renderUI({
    
    if(input$indicador_edu_pp %in% lista_ind_2){
      
      selectInput(
        inputId = "edu_pp_corte_2",
        label = "Seleccione primer corte:",
        choices = dat_edu_pp() %>% 
          select(corte_2) %>%
          arrange(corte_2) %>% 
          unique() %>% 
          pull(),
        selected = dat_edu_pp() %>% 
          filter(jerarquia == "1") %>%  
          distinct(corte_2) %>% 
          pull()
      )
      
    } else {
      
      NULL
    }
    
  })
  
  # Selector de fecha
  output$s_edu_pp_fecha <- renderUI({
    
    if(input$edu_pp_corte == "Departamento" & input$indicador_edu_pp %notin% lista_ind_2) {
      
      req(input$edu_pp_corte, input$indicador_edu_pp)
      
      req(nrow(dat_edu_pp()) > 0)
      
      selectInput(
        inputId = "fecha_dpto_edu_pp",
        label = "Seleccione año:",
        choices = dat_edu_pp() %>% 
          filter(nomindicador == input$indicador_edu_pp) %>%
          drop_na(Valor) %>%
          select(ano) %>%
          arrange(desc(ano)) %>% 
          unique() %>% 
          pull(),
        selected = 2019
      )
      
    } else if (input$indicador_edu_pp %in% lista_vunico){
      
      return(NULL)
      
    } else  {
      
      req(input$edu_pp_corte, input$indicador_edu_pp)
      req(nrow(dat_edu_pp()) > 0)
      
      tagList(
        # tags$style(type = 'text/css', 
        #            '#big_slider .irs-grid-text {font-size: 12px; transform: rotate(-90deg) translate(-10px);} ,.irs-grid-pol.large {height: 0px;}'),
        # div(id = 'big_slider',
        
        sliderInput("fecha_edu_pp", 
                    label = "Rango de tiempo", 
                    sep = "",
                    dragRange = T,
                    min = min(dat_edu_pp()$ano), 
                    max = max(dat_edu_pp()$ano), 
                    value = c(min(dat_edu_pp()$ano), 
                              max(dat_edu_pp()$ano))
        )
      )
      
    }
  })
  
  
  output$chbox_edu_pp <- renderUI({
    
    if(input$edu_pp_corte %notin% c("Total", "Departamento") & input$indicador_edu_pp %notin% lista_vunico) {
      
      edu_pp_corte_var <- rlang::sym(to_varname(input$edu_pp_corte))
      
      checkboxGroupInput(inputId = "checkbox_edu_pp",
                         label = "Seleccione categorías",
                         inline = TRUE,
                         choices =  dat_edu_pp() %>%
                           filter(corte == input$edu_pp_corte) %>% 
                           distinct(!!edu_pp_corte_var) %>%
                           pull(),
                         selected = dat_edu_pp() %>%
                           filter(corte == input$edu_pp_corte) %>% 
                           filter(jerarquia_cat == "1") %>%
                           distinct(!!edu_pp_corte_var) %>%
                           pull()
      )
      
    } else {
      
      return(NULL)
    }
    
  })
  
  # # Selector de corte según categoría y data temporal
  # dat_edu_pp <- reactive({
  #   
  #   req(input$edu_pp_corte)
  #   
  #   if(input$indicador_edu_pp %in% lista_ind_2){
  #     
  #     dat_salarios <- dat_edu_pp() %>%
  #       filter(corte_2 == input$edu_pp_corte_2) %>% 
  #       filter(corte == input$edu_pp_corte) %>%
  #       janitor::remove_empty("cols")
  #     
  #   } else {
  #     
  #     dat_edu_pp() %>%
  #       filter(corte == input$edu_pp_corte) %>%
  #       janitor::remove_empty("cols")
  #     
  #   }
  # })
  
  # 3.1.2. Metadata   ======================================================
  
  # Title
  output$title_edu_pp <- renderUI({ 
    helpText(HTML(unique(dat_edu_pp()$nomindicador)))
  })
  
  # Subtitle
  output$subtitle_edu_pp <- renderUI({ 
    helpText(HTML(unique(dat_edu_pp()$definicion)))
  })
  
  # Calculo
  output$calculo_edu_pp <- renderUI({ 
    helpText(HTML(paste("<b> Forma de cálculo:</b>", unique(dat_edu_pp()$calculo))))
  })
  
  
  # 3.1.3. Gráficos   ======================================================
  
  output$plot_edu_pp <- renderPlot({
    
    req(input$indicador_edu_pp)
    
    if(input$indicador_edu_pp %in% lista_vunico & input$indicador_edu_pp %in% lista_ind_2){
      
      req(input$edu_pp_corte, input$indicador_edu_pp)
      
      edu_pp_corte_var <- rlang::sym(to_varname(input$edu_pp_corte))
      edu_pp_corte_var_2 <- rlang::sym(to_varname(input$edu_pp_corte_2))
      
      dat_plot <- dat_edu_pp() %>%
        filter(ano >= input$fecha_edu_pp[1] &
                 ano <= input$fecha_edu_pp[2]) %>%
        filter(corte == input$edu_pp_corte) %>%
        filter(!!edu_pp_corte_var %in% input$checkbox_edu_pp) %>% 
        filter(corte_2 == input$edu_pp_corte_2)  
      
      plot_edu_corte <- ggplot(dat_plot,
                                 aes_string(x = "fecha_cat", y = "Valor",
                                            fill = edu_pp_corte_var)) +
        geom_col(position = "dodge", width = .7, alpha = .8) +
        theme_bdd(base_size = 12) +
        theme(axis.text.x=element_blank(),
              legend.position = "bottom") +
        labs(x = "",  y = "",
             title = wrapit(paste(input$indicador_edu_pp,
                                  "según",
                                  tolower(input$edu_pp_corte),
                                  "en",
                                  unique(dat_plot$fecha_cat))),
             caption = wrapit(unique(dat_plot$cita))) +
        scale_fill_brewer(name = "", palette = "Paired") +
        facet_wrap(as.formula(paste("~", edu_pp_corte_var_2)))
      
      print(plot_edu_corte)
      ggsave("www/indicador edu pp.png", width = 30, height = 20, units = "cm")
      
    } else if(input$indicador_edu_pp %in% lista_vunico & input$edu_pp_corte != "Departamento") {
      
      req(input$edu_pp_corte, input$indicador_edu_pp)
      
      dat_plot <- dat_edu_pp() %>%
        filter(corte == input$edu_pp_corte) %>%
        janitor::remove_empty("cols")
      
      if(input$edu_pp_corte == "Total"){
        
        plot_edu_corte <- ggplot(dat_plot,
                                 aes_string(x = "fecha_cat", y = "Valor")) +
          geom_col(position = "dodge", width = .4, alpha = .8, fill = color_defecto) +
          geom_text(aes(label = Valor), vjust = -0.4, fontface = "bold", size = 5) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x=element_blank(),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_edu_pp,
                                    "según",
                                    tolower(input$edu_pp_corte),
                                    "en",
                                    unique(dat_plot$fecha_cat))),
               caption = wrapit(unique(dat_plot$cita))) 
        
        print(plot_edu_corte)
        ggsave("www/indicador edu pp.png", width = 30, height = 20, units = "cm")
        
        
      } else {
      
        edu_pp_corte_var <- rlang::sym(to_varname(input$edu_pp_corte))
        
        plot_edu_corte <- ggplot(dat_plot,
                               aes_string(x = "fecha_cat", y = "Valor",
                                          fill = edu_pp_corte_var)) +
        geom_col(position = "dodge", width = .7, alpha = .8) +
        theme_bdd(base_size = 12) +
        theme(axis.text.x=element_blank(),
              legend.position = "bottom") +
        labs(x = "",  y = "",
             title = wrapit(paste(input$indicador_edu_pp,
                                  "según",
                                  tolower(input$edu_pp_corte),
                                  "en",
                                  unique(dat_plot$fecha_cat))),
             caption = wrapit(unique(dat_plot$cita))) +
        scale_fill_brewer(name = "", palette = "Paired") 
      
      print(plot_edu_corte)
      ggsave("www/indicador edu pp.png", width = 30, height = 20, units = "cm")
      
      }
      
      
    } else if(input$indicador_edu_pp %in% lista_ind_2) {
      
      req(input$edu_pp_corte, input$edu_pp_corte_2,
          input$fecha_edu_pp, input$checkbox_edu_pp)
      
      edu_pp_corte_var <- rlang::sym(to_varname(input$edu_pp_corte))
      
      dat_plot <- dat_edu_pp() %>%
        filter(ano >= input$fecha_edu_pp[1] &
                 ano <= input$fecha_edu_pp[2]) %>%
        filter(corte == input$edu_pp_corte) %>%
        filter(!!edu_pp_corte_var %in% input$checkbox_edu_pp)
      
      if(input$edu_pp_corte_2 == "Total"){
        
        dat_plot <- dat_plot %>% 
          filter(corte_2 == "Total")
        
        plot_edu_corte <- ggplot(dat_plot,
                                 aes_string(x = "fecha", y = "Valor", colour = edu_pp_corte_var)) +
          geom_line(size = 1, alpha = 0.5) +
          geom_point(size = 3) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_edu_pp,
                                    "según",
                                    tolower(input$edu_pp_corte))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_colour_manual(name = "", values = paleta_expandida) 
        
      } else if(input$edu_pp_corte_2 != "Total") {
        
        edu_pp_corte_var_2 <- rlang::sym(to_varname(input$edu_pp_corte_2))
        
        dat_plot <- dat_plot %>%
          filter(corte_2 == input$edu_pp_corte_2)  
        
        plot_edu_corte <- ggplot(dat_plot,
                                 aes_string(x = "fecha", y = "Valor", colour = edu_pp_corte_var)) +
          geom_line(size = 1, alpha = 0.5) +
          geom_point(size = 3) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_edu_pp,
                                    "según",
                                    tolower(input$edu_pp_corte))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_colour_manual(name = "", values = paleta_expandida) + 
          facet_wrap(as.formula(paste("~", edu_pp_corte_var_2)))
        
      }
      
      print(plot_edu_corte)
      ggsave("www/indicador edu pp.png", width = 30, height = 20, units = "cm")
      
      
    } else if(input$edu_pp_corte == "Total") {
      
      req(input$indicador_edu_pp, input$fecha_edu_pp)
      
      dat_plot <- dat_edu_pp() %>% 
        filter(ano >= input$fecha_edu_pp[1] &
                 ano <= input$fecha_edu_pp[2]) %>% 
        filter(corte == "Total")
      
      plot_edu <- ggplot(dat_plot,
                         aes(x = fecha, y = Valor)) +
        geom_line(size = 1, alpha = 0.5, colour = color_defecto) +
        geom_point(size = 3, colour = color_defecto) +
        theme_bdd(base_size = 12) +
        theme(axis.text.x = element_text(angle = 0),
              legend.position = "bottom") +
        labs(x = "",  y = "",
             title = wrapit(input$indicador_edu_pp),
             caption = wrapit(unique(dat_plot$cita))) 
      
      print(plot_edu)
      ggsave("www/indicador edu pp.png", width = 30, height = 20, units = "cm")
      
      
    } else if(input$edu_pp_corte == "Departamento" & 
              input$indicador_edu_pp %notin% lista_ind_2 ) {
      
      req(input$indicador_edu_pp, input$fecha_dpto_edu_pp)
      
      dat_plot <- dat_edu_pp() %>%
        filter(corte == "Departamento") %>%
        filter(ano == input$fecha_dpto_edu_pp) %>% 
        select(departamento, Valor, fuente, cita)
      
      dep_j <- dep %>%
        left_join(dat_plot, by = c("nombre" = "departamento"))
      
      plot_edu_dpto <- geouy::plot_geouy(dep_j,
                                         "Valor",
                                         viri_opt = "plasma",
                                         l = "n") +
        labs(x = "",  y = "",
             title = wrapit(paste(input$indicador_edu_pp, 
                                  "en",
                                  input$fecha_dpto_edu_pp), w = 80),
             caption = wrapit(unique(dat_plot$cita), w = 80)) +
        theme_bdd(base_size = 14)
      
      print(plot_edu_dpto)
      ggsave("www/indicador edu pp.png", width = 30, height = 20, units = "cm")
      
      
    } else if(input$edu_pp_corte != "Total") {
      
      req(input$edu_pp_corte, input$indicador_edu_pp, 
          input$fecha_edu_pp, input$checkbox_edu_pp)
      
      dat_plot <- dat_edu_pp() %>%
        filter(ano >= input$fecha_edu_pp[1] &
                 ano <= input$fecha_edu_pp[2]) %>%
        filter(corte == input$edu_pp_corte) %>%
        janitor::remove_empty("cols")
      
      edu_pp_corte_var <- rlang::sym(to_varname(input$edu_pp_corte))
      
      dat_plot <- filter(dat_plot,
                         !!edu_pp_corte_var %in% input$checkbox_edu_pp)
      
      plot_edu_corte <- ggplot(dat_plot,
                               aes_string(x = "fecha", y = "Valor", colour = edu_pp_corte_var)) +
        geom_line(size = 1, alpha = 0.5) +
        geom_point(size = 3) +
        theme_bdd(base_size = 12) +
        theme(axis.text.x = element_text(angle = 0),
              legend.position = "bottom") +
        labs(x = "",  y = "",
             title = wrapit(paste(input$indicador_edu_pp,
                                  "según",
                                  tolower(input$edu_pp_corte))),
             caption = wrapit(unique(dat_plot$cita))) +
        scale_colour_manual(name = "", values = paleta_expandida) 
      
      print(plot_edu_corte)
      ggsave("www/indicador edu pp.png", width = 30, height = 20, units = "cm")
      
    }
    
  })
  
  # 3.1.4. Descarga gráficos   =============================================
  
  output$baja_p_edu_pp <- downloadHandler(
    filename <- function() {
      paste("indicador edu pp", "png", sep = ".")
    },
    
    content <- function(file) {
      file.copy("www/indicador edu pp.png", file)
    },
    contentType = "www/indicador edu pp"
  )
  
  
  # 3.1.5. Tablas   ========================================================
  
  # Data
  edu_pp_tab <- reactive({
    
    if(input$indicador_edu_pp %in% lista_vunico & input$edu_pp_corte == "Total"){
      
      req(input$edu_pp_corte, input$indicador_edu_pp)
      
      dat_edu_pp() %>%
        filter(corte == "Total") %>% 
        select(Fecha, Valor) %>%
        arrange(desc(Fecha))
      
    } else if(input$indicador_edu_pp %in% lista_vunico & input$edu_pp_corte != "Total") {
      
      req(input$edu_pp_corte, input$indicador_edu_pp)
      
      edu_pp_corte_var <- rlang::sym(to_varname(input$edu_pp_corte))
      
      dat_cut <- dat_edu_pp() %>%
        filter(corte == input$edu_pp_corte) %>%
        janitor::remove_empty("cols") 
      
      dat_cut %>%     
        select(Fecha, edu_pp_corte_var, Valor) %>%
        arrange(desc(Fecha)) %>% 
        pivot_wider(values_from = "Valor",
                    names_from = edu_pp_corte_var)
      
      
    } else if(input$indicador_edu_pp %in% lista_ind_2) {
      
      req(input$edu_pp_corte, input$indicador_edu_pp, input$fecha_edu_pp)
      
      edu_pp_corte_var <- rlang::sym(to_varname(input$edu_pp_corte))
      
      dat_cut <- dat_edu_pp() %>%
        filter(corte == input$edu_pp_corte) %>%
        filter(!!edu_pp_corte_var %in% input$checkbox_edu_pp)
      
      if(input$edu_pp_corte_2 == "Total"){
        
        dat_cut %>%
          filter(ano >= input$fecha_edu_pp[1] &
                   ano <= input$fecha_edu_pp[2]) %>%
          filter(corte_2 == "Total") %>% 
          select(Fecha, edu_pp_corte_var, Valor) %>%
          arrange(desc(Fecha)) %>%
          pivot_wider(values_from = "Valor",
                      names_from = edu_pp_corte_var)
        
      } else if(input$edu_pp_corte_2 != "Total") {
        
        edu_pp_corte_var_2 <- rlang::sym(to_varname(input$edu_pp_corte_2))
        
        dat_cut %>%
          filter(ano >= input$fecha_edu_pp[1] &
                   ano <= input$fecha_edu_pp[2]) %>%
          filter(corte_2 == input$edu_pp_corte_2) %>% 
          select(Fecha, edu_pp_corte_var, edu_pp_corte_var_2, Valor) %>%
          arrange(desc(Fecha)) %>%
          pivot_wider(values_from = "Valor",
                      names_from = edu_pp_corte_var) 
        
      }
      
    } else if(input$edu_pp_corte == "Total") {
      
      req(input$edu_pp_corte, input$indicador_edu_pp, input$fecha_edu_pp)
      
      dat_edu_pp() %>%
        filter(corte == "Total") %>% 
        filter(ano >= input$fecha_edu_pp[1] &
                 ano <= input$fecha_edu_pp[2]) %>%
        select(Fecha, Valor) %>%
        arrange(desc(Fecha))
      
    } else if(input$edu_pp_corte != "Total") {
      
      req(input$edu_pp_corte, input$indicador_edu_pp, input$fecha_edu_pp)
      
      edu_pp_corte_var <- rlang::sym(to_varname(input$edu_pp_corte))
      
      dat_cut <- dat_edu_pp() %>%
        filter(corte == input$edu_pp_corte) %>%
        janitor::remove_empty("cols") 
      
      dat_cut %>%     
        filter(ano >= input$fecha_edu_pp[1] &
                 ano <= input$fecha_edu_pp[2]) %>% 
        select(Fecha, edu_pp_corte_var, Valor) %>%
        arrange(desc(Fecha)) %>% 
        pivot_wider(values_from = "Valor",
                    names_from = edu_pp_corte_var)
      
    }
  })
  
  # Metadata 
  edu_pp_meta <- reactive({
    
    dat_edu_pp() %>%
      select(nomindicador, derecho, conindicador, tipoind, definicion, calculo, cita) %>% 
      mutate(`Mirador DESCA - UMAD/FCS – INDDHH` = " ") %>% 
      distinct() %>% 
      gather(key = "", value = " ")
    
  })
  
  # Excel
  list_edu_pp <- reactive({
    list_edu_pp <- list("Data" = edu_pp_tab(),
                         "Metadata" = edu_pp_meta())
  })
  
  # Render
  output$table_edu_pp <- renderDT({
    
    DT::datatable(edu_pp_tab(),
                  rownames = FALSE,
                  caption = htmltools::tags$caption(
                    input$indicador_edu_pp,
                    style = "color:black; font-size:110%;")
    ) 
    
  })
  
  # 3.1.6. Descarga tablas   ================================================
  
  output$dwl_tab_edu_pp <- downloadHandler(
    
    filename = function() {
      paste("pp-", input$indicador_edu_pp, ".xlsx", sep = "")
    },
    content = function(file) {
      
      openxlsx::write.xlsx(list_edu_pp(), file)
      
    }
  )
  
  
  ### 3.2. Educación Resultados   =============================================
  
  # 3.2.1. Data reactiva   =================================================
  
  dat_edu_r <- reactive({
    
    req(input$indicador_edu_r)
    
    dat %>%
      filter(nomindicador == input$indicador_edu_r) 
    
  })
  
  output$selector_edu_r_corte <- renderUI({
    
    selectInput(
      inputId = "edu_r_corte",
      label = "Seleccione corte:",
      choices = dat_edu_r() %>% 
        select(corte) %>%
        arrange(corte) %>% 
        unique() %>% 
        pull(),
      selected = dat_edu_r() %>% 
        filter(jerarquia == "1") %>%  
        distinct(corte) %>% 
        pull()
    )
    
  })
  
  output$selector_edu_r_corte_2 <- renderUI({
    
    if(input$indicador_edu_r %in% lista_ind_2){
      
      selectInput(
        inputId = "edu_r_corte_2",
        label = "Seleccione primer corte:",
        choices = dat_edu_r() %>% 
          select(corte_2) %>%
          arrange(corte_2) %>% 
          unique() %>% 
          pull(),
        selected = dat_edu_r() %>% 
          filter(jerarquia == "1") %>%  
          distinct(corte_2) %>% 
          pull()
      )
      
    } else {
      
      NULL
    }
    
  })
  
  # Selector de fecha
  output$s_edu_r_fecha <- renderUI({
    
    if(input$edu_r_corte == "Departamento" & input$indicador_edu_r %notin% lista_ind_2) {
      
      req(input$edu_r_corte, input$indicador_edu_r)
      
      req(nrow(dat_edu_r()) > 0)
      
      selectInput(
        inputId = "fecha_dpto_edu_r",
        label = "Seleccione año:",
        choices = dat_edu_r() %>% 
          filter(nomindicador == input$indicador_edu_r) %>%
          drop_na(Valor) %>%
          select(ano) %>%
          arrange(desc(ano)) %>% 
          unique() %>% 
          pull(),
        selected = 2019
      )
      
    } else if (input$indicador_edu_r %in% lista_vunico){
      
      return(NULL)
      
    } else  {
      
      req(input$edu_r_corte, input$indicador_edu_r)
      req(nrow(dat_edu_r()) > 0)
      
      tagList(
        # tags$style(type = 'text/css', 
        #            '#big_slider .irs-grid-text {font-size: 12px; transform: rotate(-90deg) translate(-10px);} ,.irs-grid-pol.large {height: 0px;}'),
        # div(id = 'big_slider',
        
        sliderInput("fecha_edu_r", 
                    label = "Rango de tiempo", 
                    sep = "",
                    dragRange = T,
                    min = min(dat_edu_r()$ano), 
                    max = max(dat_edu_r()$ano), 
                    value = c(min(dat_edu_r()$ano), 
                              max(dat_edu_r()$ano))
        )
      )
      
    }
  })
  
  
  output$chbox_edu_r <- renderUI({
    
    if(input$edu_r_corte %notin% c("Total", "Departamento") & input$indicador_edu_r %notin% lista_vunico) {
      
      edu_r_corte_var <- rlang::sym(to_varname(input$edu_r_corte))
      
      checkboxGroupInput(inputId = "checkbox_edu_r",
                         label = "Seleccione categorías",
                         inline = TRUE,
                         choices =  dat_edu_r() %>%
                           filter(corte == input$edu_r_corte) %>% 
                           distinct(!!edu_r_corte_var) %>%
                           pull(),
                         selected = dat_edu_r() %>%
                           filter(corte == input$edu_r_corte) %>% 
                           filter(jerarquia_cat == "1") %>%
                           distinct(!!edu_r_corte_var) %>%
                           pull()
      )
      
    } else {
      
      return(NULL)
    }
    
  })
  
  # # Selector de corte según categoría y data temporal
  # dat_edu_r <- reactive({
  #   
  #   req(input$edu_r_corte)
  #   
  #   if(input$indicador_edu_r %in% lista_ind_2){
  #     
  #     dat_salarios <- dat_edu_r() %>%
  #       filter(corte_2 == input$edu_r_corte_2) %>% 
  #       filter(corte == input$edu_r_corte) %>%
  #       janitor::remove_empty("cols")
  #     
  #   } else {
  #     
  #     dat_edu_r() %>%
  #       filter(corte == input$edu_r_corte) %>%
  #       janitor::remove_empty("cols")
  #     
  #   }
  # })
  
  # 3.2.2. Metadata   ======================================================
  
  # Title
  output$title_edu_r <- renderUI({ 
    helpText(HTML(unique(dat_edu_r()$nomindicador)))
  })
  
  # Subtitle
  output$subtitle_edu_r <- renderUI({ 
    helpText(HTML(unique(dat_edu_r()$definicion)))
  })
  
  # Calculo
  output$calculo_edu_r <- renderUI({ 
    helpText(HTML(paste("<b> Forma de cálculo:</b>", unique(dat_edu_r()$calculo))))
  })
  
  
  # 3.2.3. Gráficos   ======================================================
  
  output$plot_edu_r <- renderPlot({
    
    req(input$indicador_edu_r)
    
    if(input$indicador_edu_r %in% lista_vunico & input$indicador_edu_r %in% lista_ind_2){
      
      req(input$edu_r_corte, input$indicador_edu_r)
      
      edu_r_corte_var <- rlang::sym(to_varname(input$edu_r_corte))
      edu_r_corte_var_2 <- rlang::sym(to_varname(input$edu_r_corte_2))
      
      dat_plot <- dat_edu_r() %>%
        filter(ano >= input$fecha_edu_r[1] &
                 ano <= input$fecha_edu_r[2]) %>%
        filter(corte == input$edu_r_corte) %>%
        filter(!!edu_r_corte_var %in% input$checkbox_edu_r) %>% 
        filter(corte_2 == input$edu_r_corte_2)  
      
      plot_edu_corte <- ggplot(dat_plot,
                               aes_string(x = "fecha_cat", y = "Valor",
                                          fill = edu_r_corte_var)) +
        geom_col(position = "dodge", width = .7, alpha = .8) +
        theme_bdd(base_size = 12) +
        theme(axis.text.x=element_blank(),
              legend.position = "bottom") +
        labs(x = "",  y = "",
             title = wrapit(paste(input$indicador_edu_r,
                                  "según",
                                  tolower(input$edu_r_corte),
                                  "en",
                                  unique(dat_plot$fecha_cat))),
             caption = wrapit(unique(dat_plot$cita))) +
        scale_fill_brewer(name = "", palette = "Paired") +
        facet_wrap(as.formula(paste("~", edu_r_corte_var_2)))
      
      print(plot_edu_corte)
      ggsave("www/indicador edu r.png", width = 30, height = 20, units = "cm")
      
    } else if(input$indicador_edu_r %in% lista_vunico & input$edu_r_corte != "Departamento") {
      
      req(input$edu_r_corte, input$indicador_edu_r)
      
      dat_plot <- dat_edu_r() %>%
        filter(corte == input$edu_r_corte) %>%
        janitor::remove_empty("cols")
      
      if(input$edu_r_corte == "Total"){
        
        plot_edu_corte <- ggplot(dat_plot,
                                 aes_string(x = "fecha_cat", y = "Valor")) +
          geom_col(position = "dodge", width = .4, alpha = .8, fill = color_defecto) +
          geom_text(aes(label = Valor), vjust = -0.4, fontface = "bold", size = 5) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x=element_blank(),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_edu_r,
                                    "según",
                                    tolower(input$edu_r_corte),
                                    "en",
                                    unique(dat_plot$fecha_cat))),
               caption = wrapit(unique(dat_plot$cita))) 
        
        print(plot_edu_corte)
        ggsave("www/indicador edu r.png", width = 30, height = 20, units = "cm")
        
        
      } else {
        
        edu_r_corte_var <- rlang::sym(to_varname(input$edu_r_corte))
        
        plot_edu_corte <- ggplot(dat_plot,
                                 aes_string(x = "fecha_cat", y = "Valor",
                                            fill = edu_r_corte_var)) +
          geom_col(position = "dodge", width = .7, alpha = .8) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x=element_blank(),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_edu_r,
                                    "según",
                                    tolower(input$edu_r_corte),
                                    "en",
                                    unique(dat_plot$fecha_cat))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_fill_brewer(name = "", palette = "Paired") 
        
        print(plot_edu_corte)
        ggsave("www/indicador edu r.png", width = 30, height = 20, units = "cm")
        
      }
      
      print(plot_edu_corte)
      ggsave("www/indicador edu r.png", width = 30, height = 20, units = "cm")
      
    } else if(input$indicador_edu_r %in% lista_ind_2) {
      
      req(input$edu_r_corte, input$edu_r_corte_2,
          input$fecha_edu_r, input$checkbox_edu_r)
      
      edu_r_corte_var <- rlang::sym(to_varname(input$edu_r_corte))
      
      dat_plot <- dat_edu_r() %>%
        filter(ano >= input$fecha_edu_r[1] &
                 ano <= input$fecha_edu_r[2]) %>%
        filter(corte == input$edu_r_corte) %>%
        filter(!!edu_r_corte_var %in% input$checkbox_edu_r)
      
      if(input$edu_r_corte_2 == "Total"){
        
        dat_plot <- dat_plot %>% 
          filter(corte_2 == "Total")
        
        plot_edu_corte <- ggplot(dat_plot,
                                 aes_string(x = "fecha", y = "Valor", colour = edu_r_corte_var)) +
          geom_line(size = 1, alpha = 0.5) +
          geom_point(size = 3) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_edu_r,
                                    "según",
                                    tolower(input$edu_r_corte))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_colour_manual(name = "", values = paleta_expandida) 
        
      } else if(input$edu_r_corte_2 != "Total") {
        
        edu_r_corte_var_2 <- rlang::sym(to_varname(input$edu_r_corte_2))
        
        dat_plot <- dat_plot %>%
          filter(corte_2 == input$edu_r_corte_2)  
        
        plot_edu_corte <- ggplot(dat_plot,
                                 aes_string(x = "fecha", y = "Valor", colour = edu_r_corte_var)) +
          geom_line(size = 1, alpha = 0.5) +
          geom_point(size = 3) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_edu_r,
                                    "según",
                                    tolower(input$edu_r_corte))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_colour_manual(name = "", values = paleta_expandida) + 
          facet_wrap(as.formula(paste("~", edu_r_corte_var_2)))
        
      }
      
      print(plot_edu_corte)
      ggsave("www/indicador edu r.png", width = 30, height = 20, units = "cm")
      
      
    } else if(input$edu_r_corte == "Total") {
      
      req(input$indicador_edu_r, input$fecha_edu_r)
      
      dat_plot <- dat_edu_r() %>% 
        filter(ano >= input$fecha_edu_r[1] &
                 ano <= input$fecha_edu_r[2]) %>% 
        filter(corte == "Total")
      
      plot_edu <- ggplot(dat_plot,
                         aes(x = fecha, y = Valor)) +
        geom_line(size = 1, alpha = 0.5, colour = color_defecto) +
        geom_point(size = 3, colour = color_defecto) +
        theme_bdd(base_size = 12) +
        theme(axis.text.x = element_text(angle = 0),
              legend.position = "bottom") +
        labs(x = "",  y = "",
             title = wrapit(input$indicador_edu_r),
             caption = wrapit(unique(dat_plot$cita))) 
      
      print(plot_edu)
      ggsave("www/indicador edu r.png", width = 30, height = 20, units = "cm")
      
      
    } else if(input$edu_r_corte == "Departamento" & 
              input$indicador_edu_r %notin% lista_ind_2 ) {
      
      req(input$indicador_edu_r, input$fecha_dpto_edu_r)
      
      dat_plot <- dat_edu_r() %>%
        filter(corte == "Departamento") %>%
        filter(ano == input$fecha_dpto_edu_r) %>% 
        select(departamento, Valor, fuente, cita)
      
      dep_j <- dep %>%
        left_join(dat_plot, by = c("nombre" = "departamento"))
      
      plot_edu_dpto <- geouy::plot_geouy(dep_j,
                                         "Valor",
                                         viri_opt = "plasma",
                                         l = "n") +
        labs(x = "",  y = "",
             title = wrapit(paste(input$indicador_edu_r, 
                                  "en",
                                  input$fecha_dpto_edu_r), w = 80),
             caption = wrapit(unique(dat_plot$cita), w = 80)) +
        theme_bdd(base_size = 14)
      
      print(plot_edu_dpto)
      ggsave("www/indicador edu r.png", width = 30, height = 20, units = "cm")
      
      
    } else if(input$edu_r_corte != "Total") {
      
      req(input$edu_r_corte, input$indicador_edu_r, 
          input$fecha_edu_r, input$checkbox_edu_r)
      
      dat_plot <- dat_edu_r() %>%
        filter(ano >= input$fecha_edu_r[1] &
                 ano <= input$fecha_edu_r[2]) %>%
        filter(corte == input$edu_r_corte) %>%
        janitor::remove_empty("cols")
      
      edu_r_corte_var <- rlang::sym(to_varname(input$edu_r_corte))
      
      dat_plot <- filter(dat_plot,
                         !!edu_r_corte_var %in% input$checkbox_edu_r)
      
      plot_edu_corte <- ggplot(dat_plot,
                               aes_string(x = "fecha", y = "Valor", colour = edu_r_corte_var)) +
        geom_line(size = 1, alpha = 0.5) +
        geom_point(size = 3) +
        theme_bdd(base_size = 12) +
        theme(axis.text.x = element_text(angle = 0),
              legend.position = "bottom") +
        labs(x = "",  y = "",
             title = wrapit(paste(input$indicador_edu_r,
                                  "según",
                                  tolower(input$edu_r_corte))),
             caption = wrapit(unique(dat_plot$cita))) +
        scale_colour_manual(name = "", values = paleta_expandida) 
      
      print(plot_edu_corte)
      ggsave("www/indicador edu r.png", width = 30, height = 20, units = "cm")
      
    }
    
  })
  
  # 3.2.4. Descarga gráficos   =============================================
  
  output$baja_p_edu_r <- downloadHandler(
    filename <- function() {
      paste("indicador edu r", "png", sep = ".")
    },
    
    content <- function(file) {
      file.copy("www/indicador edu r.png", file)
    },
    contentType = "www/indicador edu r"
  )
  
  
  # 3.2.5. Tablas   ========================================================
  
  # Data
  edu_r_tab <- reactive({
    
    if(input$indicador_edu_r %in% lista_vunico & input$edu_r_corte == "Total"){
    
      req(input$edu_r_corte, input$indicador_edu_r)
      
      dat_edu_r() %>%
        filter(corte == "Total") %>% 
        select(Fecha, Valor) %>%
        arrange(desc(Fecha))
    
    } else if(input$indicador_edu_r %in% lista_vunico & input$edu_r_corte != "Total") {

      req(input$edu_r_corte, input$indicador_edu_r)
      
      edu_r_corte_var <- rlang::sym(to_varname(input$edu_r_corte))
      
      dat_cut <- dat_edu_r() %>%
        filter(corte == input$edu_r_corte) %>%
        janitor::remove_empty("cols") 
      
      dat_cut %>%     
        select(Fecha, edu_r_corte_var, Valor) %>%
        arrange(desc(Fecha)) %>% 
        pivot_wider(values_from = "Valor",
                    names_from = edu_r_corte_var)
      
      
      } else if(input$indicador_edu_r %in% lista_ind_2) {
      
      req(input$edu_r_corte, input$indicador_edu_r, input$fecha_edu_r)
      
      edu_r_corte_var <- rlang::sym(to_varname(input$edu_r_corte))
      
      dat_cut <- dat_edu_r() %>%
        filter(corte == input$edu_r_corte) %>%
        filter(!!edu_r_corte_var %in% input$checkbox_edu_r)
      
      if(input$edu_r_corte_2 == "Total"){
        
        dat_cut %>%
          filter(ano >= input$fecha_edu_r[1] &
                   ano <= input$fecha_edu_r[2]) %>%
          filter(corte_2 == "Total") %>% 
          select(Fecha, edu_r_corte_var, Valor) %>%
          arrange(desc(Fecha)) %>%
          pivot_wider(values_from = "Valor",
                      names_from = edu_r_corte_var)
        
      } else if(input$edu_r_corte_2 != "Total") {
        
        edu_r_corte_var_2 <- rlang::sym(to_varname(input$edu_r_corte_2))
        
        dat_cut %>%
          filter(ano >= input$fecha_edu_r[1] &
                   ano <= input$fecha_edu_r[2]) %>%
          filter(corte_2 == input$edu_r_corte_2) %>% 
          select(Fecha, edu_r_corte_var, edu_r_corte_var_2, Valor) %>%
          arrange(desc(Fecha)) %>%
          pivot_wider(values_from = "Valor",
                      names_from = edu_r_corte_var) 
        
      }
      
    } else if(input$edu_r_corte == "Total") {
      
      req(input$edu_r_corte, input$indicador_edu_r, input$fecha_edu_r)
      
      dat_edu_r() %>%
        filter(corte == "Total") %>% 
        filter(ano >= input$fecha_edu_r[1] &
                 ano <= input$fecha_edu_r[2]) %>%
        select(Fecha, Valor) %>%
        arrange(desc(Fecha))
      
    } else if(input$edu_r_corte != "Total") {
      
      req(input$edu_r_corte, input$indicador_edu_r, input$fecha_edu_r)
      
      edu_r_corte_var <- rlang::sym(to_varname(input$edu_r_corte))
      
      dat_cut <- dat_edu_r() %>%
        filter(corte == input$edu_r_corte) %>%
        janitor::remove_empty("cols") 
      
      dat_cut %>%     
        filter(ano >= input$fecha_edu_r[1] &
                 ano <= input$fecha_edu_r[2]) %>% 
        select(Fecha, edu_r_corte_var, Valor) %>%
        arrange(desc(Fecha)) %>% 
        pivot_wider(values_from = "Valor",
                    names_from = edu_r_corte_var)
      
    }
  })
  
  # Metadata 
  edu_r_meta <- reactive({
    
    dat_edu_r() %>%
      select(nomindicador, derecho, conindicador, tipoind, definicion, calculo, cita) %>% 
      mutate(`Mirador DESCA - UMAD/FCS – INDDHH` = " ") %>% 
      distinct() %>% 
      gather(key = "", value = " ")
    
  })
  
  # Excel
  list_edu_r <- reactive({
    list_edu_r <- list("Data" = edu_r_tab(),
                        "Metadata" = edu_r_meta())
  })
  
  # Render
  output$table_edu_r <- renderDT({
    
    DT::datatable(edu_r_tab(),
                  rownames = FALSE,
                  caption = htmltools::tags$caption(
                    input$indicador_edu_r,
                    style = "color:black; font-size:110%;")
    ) 
    
  })
  
  # 3.2.6. Descarga tablas   ================================================
  
  output$dwl_tab_edu_r <- downloadHandler(
    
    filename = function() {
      paste("pp-", input$indicador_edu_r, ".xlsx", sep = "")
    },
    content = function(file) {
      
      openxlsx::write.xlsx(list_edu_r(), file)
      
    }
  )
  
  
        
  
  ### 4.1. Salud Políticas   ==============================================
  
  # 4.1.1. Data reactiva   =================================================
  
  dat_salud_pp <- reactive({
    
    req(input$indicador_salud_pp)
    
    dat %>%
      filter(nomindicador == input$indicador_salud_pp) 
    
  })
  
  output$selector_salud_pp_corte <- renderUI({
    
    selectInput(
      inputId = "salud_pp_corte",
      label = "Seleccione corte:",
      choices = dat_salud_pp() %>% 
        select(corte) %>%
        arrange(corte) %>% 
        unique() %>% 
        pull(),
      selected = dat_salud_pp() %>% 
        filter(jerarquia == "1") %>%  
        distinct(corte) %>% 
        pull()
    )
    
  })
  
  output$selector_salud_pp_corte_2 <- renderUI({
    
    if(input$indicador_salud_pp %in% lista_ind_2){
      
      selectInput(
        inputId = "salud_pp_corte_2",
        label = "Seleccione primer corte:",
        choices = dat_salud_pp() %>% 
          select(corte_2) %>%
          arrange(corte_2) %>% 
          unique() %>% 
          pull(),
        selected = dat_salud_pp() %>% 
          filter(jerarquia == "1") %>%  
          distinct(corte_2) %>% 
          pull()
      )
      
    } else {
      
      NULL
    }
    
  })
  
  # Selector de fecha
  output$s_salud_pp_fecha <- renderUI({
    
    if(input$salud_pp_corte == "Departamento" & input$indicador_salud_pp %notin% lista_ind_2) {
      
      req(input$salud_pp_corte, input$indicador_salud_pp)
      
      req(nrow(dat_salud_pp()) > 0)
      
      selectInput(
        inputId = "fecha_dpto_salud_pp",
        label = "Seleccione año:",
        choices = dat_salud_pp() %>% 
          filter(nomindicador == input$indicador_salud_pp) %>%
          drop_na(Valor) %>%
          select(ano) %>%
          arrange(desc(ano)) %>% 
          unique() %>% 
          pull(),
        selected = 2019
      )
      
    } else if (input$indicador_salud_pp %in% lista_vunico){
      
      return(NULL)
      
    } else  {
      
      req(input$salud_pp_corte, input$indicador_salud_pp)
      req(nrow(dat_salud_pp()) > 0)
      
      tagList(
        # tags$style(type = 'text/css', 
        #            '#big_slider .irs-grid-text {font-size: 12px; transform: rotate(-90deg) translate(-10px);} ,.irs-grid-pol.large {height: 0px;}'),
        # div(id = 'big_slider',
        
        sliderInput("fecha_salud_pp", 
                    label = "Rango de tiempo", 
                    sep = "",
                    dragRange = T,
                    min = min(dat_salud_pp()$ano), 
                    max = max(dat_salud_pp()$ano), 
                    value = c(min(dat_salud_pp()$ano), 
                              max(dat_salud_pp()$ano))
        )
      )
      
    }
  })
  
  
  output$chbox_salud_pp <- renderUI({
    
    if(input$salud_pp_corte %notin% c("Total", "Departamento") & input$indicador_salud_pp %notin% lista_vunico) {
      
      salud_pp_corte_var <- rlang::sym(to_varname(input$salud_pp_corte))
      
      checkboxGroupInput(inputId = "checkbox_salud_pp",
                         label = "Seleccione categorías",
                         inline = TRUE,
                         choices =  dat_salud_pp() %>%
                           filter(corte == input$salud_pp_corte) %>% 
                           distinct(!!salud_pp_corte_var) %>%
                           pull(),
                         selected = dat_salud_pp() %>%
                           filter(corte == input$salud_pp_corte) %>% 
                           filter(jerarquia_cat == "1") %>%
                           distinct(!!salud_pp_corte_var) %>%
                           pull()
      )
      
    } else {
      
      return(NULL)
    }
    
  })
  
  # # Selector de corte según categoría y data temporal
  # dat_salud_pp <- reactive({
  #   
  #   req(input$salud_pp_corte)
  #   
  #   if(input$indicador_salud_pp %in% lista_ind_2){
  #     
  #     dat_salarios <- dat_salud_pp() %>%
  #       filter(corte_2 == input$salud_pp_corte_2) %>% 
  #       filter(corte == input$salud_pp_corte) %>%
  #       janitor::remove_empty("cols")
  #     
  #   } else {
  #     
  #     dat_salud_pp() %>%
  #       filter(corte == input$salud_pp_corte) %>%
  #       janitor::remove_empty("cols")
  #     
  #   }
  # })
  
  # 4.1.2. Metadata   ======================================================
  
  # Title
  output$title_salud_pp <- renderUI({ 
    helpText(HTML(unique(dat_salud_pp()$nomindicador)))
  })
  
  # Subtitle
  output$subtitle_salud_pp <- renderUI({ 
    helpText(HTML(unique(dat_salud_pp()$definicion)))
  })
  
  # Calculo
  output$calculo_salud_pp <- renderUI({ 
    helpText(HTML(paste("<b> Forma de cálculo:</b>", unique(dat_salud_pp()$calculo))))
  })
  
  
  # 4.1.3. Gráficos   ======================================================
  
  output$plot_salud_pp <- renderPlot({
    
    req(input$indicador_salud_pp)
    
    if(input$indicador_salud_pp %in% lista_vunico & input$indicador_salud_pp %in% lista_ind_2){
      
      req(input$salud_pp_corte, input$indicador_salud_pp)
      
      salud_pp_corte_var <- rlang::sym(to_varname(input$salud_pp_corte))
      salud_pp_corte_var_2 <- rlang::sym(to_varname(input$salud_pp_corte_2))
      
      dat_plot <- dat_salud_pp() %>%
        filter(ano >= input$fecha_salud_pp[1] &
                 ano <= input$fecha_salud_pp[2]) %>%
        filter(corte == input$salud_pp_corte) %>%
        filter(!!salud_pp_corte_var %in% input$checkbox_salud_pp) %>% 
        filter(corte_2 == input$salud_pp_corte_2)  
      
      plot_salud_corte <- ggplot(dat_plot,
                               aes_string(x = "fecha_cat", y = "Valor",
                                          fill = salud_pp_corte_var)) +
        geom_col(position = "dodge", width = .7, alpha = .8) +
        theme_bdd(base_size = 12) +
        theme(axis.text.x=element_blank(),
              legend.position = "bottom") +
        labs(x = "",  y = "",
             title = wrapit(paste(input$indicador_salud_pp,
                                  "según",
                                  tolower(input$salud_pp_corte),
                                  "en",
                                  unique(dat_plot$fecha_cat))),
             caption = wrapit(unique(dat_plot$cita))) +
        scale_fill_brewer(name = "", palette = "Paired") +
        facet_wrap(as.formula(paste("~", salud_pp_corte_var_2)))
      
      print(plot_salud_corte)
      ggsave("www/indicador salud pp.png", width = 30, height = 20, units = "cm")
      
    } else if(input$indicador_salud_pp %in% lista_vunico & input$salud_pp_corte != "Departamento") {
      
      req(input$salud_pp_corte, input$indicador_salud_pp)
      
      dat_plot <- dat_salud_pp() %>%
        filter(corte == input$salud_pp_corte) %>%
        janitor::remove_empty("cols")
      
      if(input$salud_pp_corte == "Total"){
        
        plot_salud_corte <- ggplot(dat_plot,
                                 aes_string(x = "fecha_cat", y = "Valor")) +
          geom_col(position = "dodge", width = .4, alpha = .8, fill = color_defecto) +
          geom_text(aes(label = Valor), vjust = -0.4, fontface = "bold", size = 5) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x=element_blank(),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_salud_pp,
                                    "según",
                                    tolower(input$salud_pp_corte),
                                    "en",
                                    unique(dat_plot$fecha_cat))),
               caption = wrapit(unique(dat_plot$cita))) 
        
        print(plot_salud_corte)
        ggsave("www/indicador salud pp.png", width = 30, height = 20, units = "cm")
        
        
      } else {
        
        salud_pp_corte_var <- rlang::sym(to_varname(input$salud_pp_corte))
        
        plot_salud_corte <- ggplot(dat_plot,
                                 aes_string(x = "fecha_cat", y = "Valor",
                                            fill = salud_pp_corte_var)) +
          geom_col(position = "dodge", width = .7, alpha = .8) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x=element_blank(),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_salud_pp,
                                    "según",
                                    tolower(input$salud_pp_corte),
                                    "en",
                                    unique(dat_plot$fecha_cat))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_fill_brewer(name = "", palette = "Paired") 
        
        print(plot_salud_corte)
        ggsave("www/indicador salud pp.png", width = 30, height = 20, units = "cm")
        
      }
      
      print(plot_salud_corte)
      ggsave("www/indicador salud pp.png", width = 30, height = 20, units = "cm")
      
    } else if(input$indicador_salud_pp %in% lista_ind_2) {
      
      req(input$salud_pp_corte, input$salud_pp_corte_2,
          input$fecha_salud_pp, input$checkbox_salud_pp)
      
      salud_pp_corte_var <- rlang::sym(to_varname(input$salud_pp_corte))
      
      dat_plot <- dat_salud_pp() %>%
        filter(ano >= input$fecha_salud_pp[1] &
                 ano <= input$fecha_salud_pp[2]) %>%
        filter(corte == input$salud_pp_corte) %>%
        filter(!!salud_pp_corte_var %in% input$checkbox_salud_pp)
      
      if(input$salud_pp_corte_2 == "Total"){
        
        dat_plot <- dat_plot %>% 
          filter(corte_2 == "Total")
        
        plot_salud_corte <- ggplot(dat_plot,
                                 aes_string(x = "fecha", y = "Valor", colour = salud_pp_corte_var)) +
          geom_line(size = 1, alpha = 0.5) +
          geom_point(size = 3) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_salud_pp,
                                    "según",
                                    tolower(input$salud_pp_corte))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_colour_manual(name = "", values = paleta_expandida) 
        
      } else if(input$salud_pp_corte_2 != "Total") {
        
        salud_pp_corte_var_2 <- rlang::sym(to_varname(input$salud_pp_corte_2))
        
        dat_plot <- dat_plot %>%
          filter(corte_2 == input$salud_pp_corte_2)  
        
        plot_salud_corte <- ggplot(dat_plot,
                                 aes_string(x = "fecha", y = "Valor", colour = salud_pp_corte_var)) +
          geom_line(size = 1, alpha = 0.5) +
          geom_point(size = 3) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_salud_pp,
                                    "según",
                                    tolower(input$salud_pp_corte))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_colour_manual(name = "", values = paleta_expandida) + 
          facet_wrap(as.formula(paste("~", salud_pp_corte_var_2)))
        
      }
      
      print(plot_salud_corte)
      ggsave("www/indicador salud pp.png", width = 30, height = 20, units = "cm")
      
      
    } else if(input$salud_pp_corte == "Total") {
      
      req(input$indicador_salud_pp, input$fecha_salud_pp)
      
      dat_plot <- dat_salud_pp() %>% 
        filter(ano >= input$fecha_salud_pp[1] &
                 ano <= input$fecha_salud_pp[2]) %>% 
        filter(corte == "Total")
      
      plot_salud <- ggplot(dat_plot,
                         aes(x = fecha, y = Valor)) +
        geom_line(size = 1, alpha = 0.5, colour = color_defecto) +
        geom_point(size = 3, colour = color_defecto) +
        theme_bdd(base_size = 12) +
        theme(axis.text.x = element_text(angle = 0),
              legend.position = "bottom") +
        labs(x = "",  y = "",
             title = wrapit(input$indicador_salud_pp),
             caption = wrapit(unique(dat_plot$cita))) 
      
      print(plot_salud)
      ggsave("www/indicador salud pp.png", width = 30, height = 20, units = "cm")
      
      
    } else if(input$salud_pp_corte == "Departamento" & 
              input$indicador_salud_pp %notin% lista_ind_2 ) {
      
      req(input$indicador_salud_pp, input$fecha_dpto_salud_pp)
      
      dat_plot <- dat_salud_pp() %>%
        filter(corte == "Departamento") %>%
        filter(ano == input$fecha_dpto_salud_pp) %>% 
        select(departamento, Valor, fuente, cita)
      
      dep_j <- dep %>%
        left_join(dat_plot, by = c("nombre" = "departamento"))
      
      plot_salud_dpto <- geouy::plot_geouy(dep_j,
                                         "Valor",
                                         viri_opt = "plasma",
                                         l = "n") +
        labs(x = "",  y = "",
             title = wrapit(paste(input$indicador_salud_pp, 
                                  "en",
                                  input$fecha_dpto_salud_pp), w = 80),
             caption = wrapit(unique(dat_plot$cita), w = 80)) +
        theme_bdd(base_size = 14)
      
      print(plot_salud_dpto)
      ggsave("www/indicador salud pp.png", width = 30, height = 20, units = "cm")
      
      
    } else if(input$salud_pp_corte != "Total") {
      
      req(input$salud_pp_corte, input$indicador_salud_pp, 
          input$fecha_salud_pp, input$checkbox_salud_pp)
      
      dat_plot <- dat_salud_pp() %>%
        filter(ano >= input$fecha_salud_pp[1] &
                 ano <= input$fecha_salud_pp[2]) %>%
        filter(corte == input$salud_pp_corte) %>%
        janitor::remove_empty("cols")
      
      salud_pp_corte_var <- rlang::sym(to_varname(input$salud_pp_corte))
      
      dat_plot <- filter(dat_plot,
                         !!salud_pp_corte_var %in% input$checkbox_salud_pp)
      
      plot_salud_corte <- ggplot(dat_plot,
                               aes_string(x = "fecha", y = "Valor", colour = salud_pp_corte_var)) +
        geom_line(size = 1, alpha = 0.5) +
        geom_point(size = 3) +
        theme_bdd(base_size = 12) +
        theme(axis.text.x = element_text(angle = 0),
              legend.position = "bottom") +
        labs(x = "",  y = "",
             title = wrapit(paste(input$indicador_salud_pp,
                                  "según",
                                  tolower(input$salud_pp_corte))),
             caption = wrapit(unique(dat_plot$cita))) +
        scale_colour_manual(name = "", values = paleta_expandida) 
      
      print(plot_salud_corte)
      ggsave("www/indicador salud pp.png", width = 30, height = 20, units = "cm")
      
    }
    
  })
  
  # 4.1.4. Descarga gráficos   =============================================
  
  output$baja_p_salud_pp <- downloadHandler(
    filename <- function() {
      paste("indicador salud pp", "png", sep = ".")
    },
    
    content <- function(file) {
      file.copy("www/indicador salud pp.png", file)
    },
    contentType = "www/indicador salud pp"
  )
  
  
  # 4.1.5. Tablas   ========================================================
  
  # Data
  salud_pp_tab <- reactive({
    
    if(input$indicador_salud_pp %in% lista_vunico & input$salud_pp_corte == "Total"){
      
      req(input$salud_pp_corte, input$indicador_salud_pp)
      
      dat_salud_pp() %>%
        filter(corte == "Total") %>% 
        select(Fecha, Valor) %>%
        arrange(desc(Fecha))
      
    } else if(input$indicador_salud_pp %in% lista_vunico & input$salud_pp_corte != "Total") {
      
      req(input$salud_pp_corte, input$indicador_salud_pp)
      
      salud_pp_corte_var <- rlang::sym(to_varname(input$salud_pp_corte))
      
      dat_cut <- dat_salud_pp() %>%
        filter(corte == input$salud_pp_corte) %>%
        janitor::remove_empty("cols") 
      
      dat_cut %>%     
        select(Fecha, salud_pp_corte_var, Valor) %>%
        arrange(desc(Fecha)) %>% 
        pivot_wider(values_from = "Valor",
                    names_from = salud_pp_corte_var)
      
      
    } else if(input$indicador_salud_pp %in% lista_ind_2) {
      
      req(input$salud_pp_corte, input$indicador_salud_pp, input$fecha_salud_pp)
      
      salud_pp_corte_var <- rlang::sym(to_varname(input$salud_pp_corte))
      
      dat_cut <- dat_salud_pp() %>%
        filter(corte == input$salud_pp_corte) %>%
        filter(!!salud_pp_corte_var %in% input$checkbox_salud_pp)
      
      if(input$salud_pp_corte_2 == "Total"){
        
        dat_cut %>%
          filter(ano >= input$fecha_salud_pp[1] &
                   ano <= input$fecha_salud_pp[2]) %>%
          filter(corte_2 == "Total") %>% 
          select(Fecha, salud_pp_corte_var, Valor) %>%
          arrange(desc(Fecha)) %>%
          pivot_wider(values_from = "Valor",
                      names_from = salud_pp_corte_var)
        
      } else if(input$salud_pp_corte_2 != "Total") {
        
        salud_pp_corte_var_2 <- rlang::sym(to_varname(input$salud_pp_corte_2))
        
        dat_cut %>%
          filter(ano >= input$fecha_salud_pp[1] &
                   ano <= input$fecha_salud_pp[2]) %>%
          filter(corte_2 == input$salud_pp_corte_2) %>% 
          select(Fecha, salud_pp_corte_var, salud_pp_corte_var_2, Valor) %>%
          arrange(desc(Fecha)) %>%
          pivot_wider(values_from = "Valor",
                      names_from = salud_pp_corte_var) 
        
      }
      
    } else if(input$salud_pp_corte == "Total") {
      
      req(input$salud_pp_corte, input$indicador_salud_pp, input$fecha_salud_pp)
      
      dat_salud_pp() %>%
        filter(corte == "Total") %>% 
        filter(ano >= input$fecha_salud_pp[1] &
                 ano <= input$fecha_salud_pp[2]) %>%
        select(Fecha, Valor) %>%
        arrange(desc(Fecha))
      
    } else if(input$salud_pp_corte != "Total") {
      
      req(input$salud_pp_corte, input$indicador_salud_pp, input$fecha_salud_pp)
      
      salud_pp_corte_var <- rlang::sym(to_varname(input$salud_pp_corte))
      
      dat_cut <- dat_salud_pp() %>%
        filter(corte == input$salud_pp_corte) %>%
        janitor::remove_empty("cols") 
      
      dat_cut %>%     
        filter(ano >= input$fecha_salud_pp[1] &
                 ano <= input$fecha_salud_pp[2]) %>% 
        select(Fecha, salud_pp_corte_var, Valor) %>%
        arrange(desc(Fecha)) %>% 
        pivot_wider(values_from = "Valor",
                    names_from = salud_pp_corte_var)
      
    }
  })
  
  # Metadata 
  salud_pp_meta <- reactive({
    
    dat_salud_pp() %>%
      select(nomindicador, derecho, conindicador, tipoind, definicion, calculo, cita) %>% 
      mutate(`Mirador DESCA - UMAD/FCS – INDDHH` = " ") %>% 
      distinct() %>% 
      gather(key = "", value = " ")
    
  })
  
  # Excel
  list_salud_pp <- reactive({
    list_salud_pp <- list("Data" = salud_pp_tab(),
                        "Metadata" = salud_pp_meta())
  })
  
  # Render
  output$table_salud_pp <- renderDT({
    
    DT::datatable(salud_pp_tab(),
                  rownames = FALSE,
                  caption = htmltools::tags$caption(
                    input$indicador_salud_pp,
                    style = "color:black; font-size:110%;")
    ) 
    
  })
  
  # 4.1.6. Descarga tablas   ================================================
  
  output$dwl_tab_salud_pp <- downloadHandler(
    
    filename = function() {
      paste("pp-", input$indicador_salud_pp, ".xlsx", sep = "")
    },
    content = function(file) {
      
      openxlsx::write.xlsx(list_salud_pp(), file)
      
    }
  )

    
### 4.2. Salud Resultados   =============================================
    
    # 4.2.1. Data reactiva   =================================================
    
    dat_salud_r <- reactive({
        
        req(input$indicador_salud_r)
        
        dat %>%
            filter(nomindicador == input$indicador_salud_r) 
        
    })
    
    output$selector_salud_r_corte <- renderUI({
        
        selectInput(
            inputId = "salud_r_corte",
            label = "Seleccione corte:",
            choices = dat_salud_r() %>% 
                select(corte) %>%
                arrange(corte) %>% 
                unique() %>% 
                pull(),
            selected = dat_salud_r() %>% 
                filter(jerarquia == "1") %>%  
                distinct(corte) %>% 
                pull()
            )

    })
    
    output$selector_salud_r_corte_2 <- renderUI({
      
      if(input$indicador_salud_r %in% lista_ind_2){
        
      selectInput(
        inputId = "salud_r_corte_2",
        label = "Seleccione primer corte:",
        choices = dat_salud_r() %>% 
          select(corte_2) %>%
          arrange(corte_2) %>% 
          unique() %>% 
          pull(),
        selected = dat_salud_r() %>% 
          filter(jerarquia == "1") %>%  
          distinct(corte_2) %>% 
          pull()
      )
        
      } else {
        
        NULL
      }
      
    })
    
    # Selector de fecha
    output$s_salud_r_fecha <- renderUI({
        
        if(input$salud_r_corte == "Departamento" & input$indicador_salud_r %notin% lista_ind_2) {
            
            req(input$salud_r_corte, input$indicador_salud_r)
            
            req(nrow(dat_salud_r()) > 0)
            
            selectInput(
                inputId = "fecha_dpto_salud_r",
                label = "Seleccione año:",
                choices = dat_salud_r() %>% 
                    filter(nomindicador == input$indicador_salud_r) %>%
                    drop_na(Valor) %>%
                    select(ano) %>%
                    arrange(desc(ano)) %>% 
                    unique() %>% 
                    pull(),
                selected = 2019
            )
            
        } else if (input$indicador_salud_r %in% lista_vunico){
          
          return(NULL)
            
        } else  {
            
            req(input$salud_r_corte, input$indicador_salud_r)
            req(nrow(dat_salud_r()) > 0)
            
            tagList(
                # tags$style(type = 'text/css', 
                #            '#big_slider .irs-grid-text {font-size: 12px; transform: rotate(-90deg) translate(-10px);} ,.irs-grid-pol.large {height: 0px;}'),
                # div(id = 'big_slider',
                
                sliderInput("fecha_salud_r", 
                            label = "Rango de tiempo", 
                            sep = "",
                            dragRange = T,
                            min = min(dat_salud_r()$ano), 
                            max = max(dat_salud_r()$ano), 
                            value = c(min(dat_salud_r()$ano), 
                                      max(dat_salud_r()$ano))
                )
            )

        }
    })
    
    
    output$chbox_salud_r <- renderUI({
        
      if(input$salud_r_corte %notin% c("Total", "Departamento") & input$indicador_salud_r %notin% lista_vunico) {
        
        salud_r_corte_var <- rlang::sym(to_varname(input$salud_r_corte))
        
        checkboxGroupInput(inputId = "checkbox_salud_r",
                           label = "Seleccione categorías",
                           inline = TRUE,
                           choices =  dat_salud_r() %>%
                             filter(corte == input$salud_r_corte) %>% 
                             distinct(!!salud_r_corte_var) %>%
                             pull(),
                           selected = dat_salud_r() %>%
                             filter(corte == input$salud_r_corte) %>% 
                             filter(jerarquia_cat == "1") %>%
                             distinct(!!salud_r_corte_var) %>%
                             pull()
                           )
        
      } else {
            
          return(NULL)
      }
          
    })
    
    # # Selector de corte según categoría y data temporal
    # dat_salud_r <- reactive({
    #   
    #   req(input$salud_r_corte)
    #   
    #   if(input$indicador_salud_r %in% lista_ind_2){
    #     
    #     dat_salarios <- dat_salud_r() %>%
    #       filter(corte_2 == input$salud_r_corte_2) %>% 
    #       filter(corte == input$salud_r_corte) %>%
    #       janitor::remove_empty("cols")
    #     
    #   } else {
    #     
    #     dat_salud_r() %>%
    #       filter(corte == input$salud_r_corte) %>%
    #       janitor::remove_empty("cols")
    #     
    #   }
    # })
    
    # 4.2.2. Metadata   ======================================================
    
    # Title
    output$title_salud_r <- renderUI({ 
        helpText(HTML(unique(dat_salud_r()$nomindicador)))
    })
    
    # Subtitle
    output$subtitle_salud_r <- renderUI({ 
        helpText(HTML(unique(dat_salud_r()$definicion)))
    })
    
    # Calculo
    output$calculo_salud_r <- renderUI({ 
        helpText(HTML(paste("<b> Forma de cálculo:</b>", unique(dat_salud_r()$calculo))))
    })
    
    
    # 4.2.3. Gráficos   ======================================================
    
    output$plot_salud_r <- renderPlot({
      
      req(input$indicador_salud_r)
      
      if(input$indicador_salud_r %in% lista_vunico & input$indicador_salud_r %in% lista_ind_2){
        
        req(input$salud_r_corte, input$indicador_salud_r)
        
        salud_r_corte_var <- rlang::sym(to_varname(input$salud_r_corte))
        salud_r_corte_var_2 <- rlang::sym(to_varname(input$salud_r_corte_2))
        
        dat_plot <- dat_salud_r() %>%
          filter(ano >= input$fecha_salud_r[1] &
                   ano <= input$fecha_salud_r[2]) %>%
          filter(corte == input$salud_r_corte) %>%
          filter(!!salud_r_corte_var %in% input$checkbox_salud_r) %>% 
          filter(corte_2 == input$salud_r_corte_2)  
        
        plot_salud_corte <- ggplot(dat_plot,
                                   aes_string(x = "fecha_cat", y = "Valor",
                                              fill = salud_r_corte_var)) +
          geom_col(position = "dodge", width = .7, alpha = .8) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x=element_blank(),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_salud_r,
                                    "según",
                                    tolower(input$salud_r_corte),
                                    "en",
                                    unique(dat_plot$fecha_cat))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_fill_brewer(name = "", palette = "Paired") +
          facet_wrap(as.formula(paste("~", salud_r_corte_var_2)))
        
        print(plot_salud_corte)
        ggsave("www/indicador salud r.png", width = 30, height = 20, units = "cm")
        
      } else if(input$indicador_salud_r %in% lista_vunico & input$indicador_salud_r %in% lista_ind_2){

        req(input$salud_r_corte, input$indicador_salud_r)
        
        salud_r_corte_var <- rlang::sym(to_varname(input$salud_r_corte))
        salud_r_corte_var_2 <- rlang::sym(to_varname(input$salud_r_corte_2))
        
        dat_plot <- dat_salud_r() %>%
          filter(ano >= input$fecha_salud_r[1] &
                 ano <= input$fecha_salud_r[2]) %>%
          filter(corte == input$salud_r_corte) %>%
          filter(!!salud_r_corte_var %in% input$checkbox_salud_r) %>% 
          filter(corte_2 == input$salud_r_corte_2)  
        
        plot_salud_corte <- ggplot(dat_plot,
                                   aes_string(x = "fecha_cat", y = "Valor",
                                              fill = salud_r_corte_var)) +
          geom_col(position = "dodge", width = .7, alpha = .8) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x=element_blank(),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_salud_r,
                                    "según",
                                    tolower(input$salud_r_corte),
                                    "en",
                                    unique(dat_plot$fecha_cat))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_fill_brewer(name = "", palette = "Paired") +
          facet_wrap(as.formula(paste("~", salud_r_corte_var_2)))
        
        print(plot_salud_corte)
        ggsave("www/indicador salud r.png", width = 30, height = 20, units = "cm")

    } else if(input$indicador_salud_r %in% lista_vunico & input$salud_r_corte != "Departamento") {
      
      req(input$salud_r_corte, input$indicador_salud_r)
      
        dat_plot <- dat_salud_r() %>%
          filter(corte == input$salud_r_corte) %>%
          janitor::remove_empty("cols")
        
        if(input$salud_r_corte == "Total"){
          
          plot_salud_corte <- ggplot(dat_plot,
                                   aes_string(x = "fecha_cat", y = "Valor")) +
            geom_col(position = "dodge", width = .4, alpha = .8, fill = color_defecto) +
            geom_text(aes(label = Valor), vjust = -0.4, fontface = "bold", size = 5) +
            theme_bdd(base_size = 12) +
            theme(axis.text.x=element_blank(),
                  legend.position = "bottom") +
            labs(x = "",  y = "",
                 title = wrapit(paste(input$indicador_salud_r,
                                      "según",
                                      tolower(input$salud_r_corte),
                                      "en",
                                      unique(dat_plot$fecha_cat))),
                 caption = wrapit(unique(dat_plot$cita))) 
          
          print(plot_salud_corte)
          ggsave("www/indicador salud r.png", width = 30, height = 20, units = "cm")
          
          
        } else {
          
          salud_r_corte_var <- rlang::sym(to_varname(input$salud_r_corte))
          
          plot_salud_corte <- ggplot(dat_plot,
                                   aes_string(x = "fecha_cat", y = "Valor",
                                              fill = salud_r_corte_var)) +
            geom_col(position = "dodge", width = .7, alpha = .8) +
            theme_bdd(base_size = 12) +
            theme(axis.text.x=element_blank(),
                  legend.position = "bottom") +
            labs(x = "",  y = "",
                 title = wrapit(paste(input$indicador_salud_r,
                                      "según",
                                      tolower(input$salud_r_corte),
                                      "en",
                                      unique(dat_plot$fecha_cat))),
                 caption = wrapit(unique(dat_plot$cita))) +
            scale_fill_brewer(name = "", palette = "Paired") 
          
          print(plot_salud_corte)
          ggsave("www/indicador salud r.png", width = 30, height = 20, units = "cm")
          
        }
        
        print(plot_salud_corte)
        ggsave("www/indicador salud r.png", width = 30, height = 20, units = "cm")
      
      } else if(input$indicador_salud_r %in% lista_ind_2) {
        
        req(input$salud_r_corte, input$salud_r_corte_2,
            input$fecha_salud_r, input$checkbox_salud_r)
        
        salud_r_corte_var <- rlang::sym(to_varname(input$salud_r_corte))
        
        dat_plot <- dat_salud_r() %>%
          filter(ano >= input$fecha_salud_r[1] &
                   ano <= input$fecha_salud_r[2]) %>%
          filter(corte == input$salud_r_corte) %>%
          filter(!!salud_r_corte_var %in% input$checkbox_salud_r)
        
        if(input$salud_r_corte_2 == "Total"){
          
          dat_plot <- dat_plot %>% 
            filter(corte_2 == "Total")
          
          plot_salud_corte <- ggplot(dat_plot,
                                   aes_string(x = "fecha", y = "Valor", colour = salud_r_corte_var)) +
            geom_line(size = 1, alpha = 0.5) +
            geom_point(size = 3) +
            theme_bdd(base_size = 12) +
            theme(axis.text.x = element_text(angle = 0),
                  legend.position = "bottom") +
            labs(x = "",  y = "",
                 title = wrapit(paste(input$indicador_salud_r,
                                      "según",
                                      tolower(input$salud_r_corte))),
                 caption = wrapit(unique(dat_plot$cita))) +
            scale_colour_manual(name = "", values = paleta_expandida) 
          
        } else if(input$salud_r_corte_2 != "Total") {
          
          salud_r_corte_var_2 <- rlang::sym(to_varname(input$salud_r_corte_2))
          
          dat_plot <- dat_plot %>%
            filter(corte_2 == input$salud_r_corte_2)  
            
          plot_salud_corte <- ggplot(dat_plot,
                                   aes_string(x = "fecha", y = "Valor", colour = salud_r_corte_var)) +
            geom_line(size = 1, alpha = 0.5) +
            geom_point(size = 3) +
            theme_bdd(base_size = 12) +
            theme(axis.text.x = element_text(angle = 0),
                  legend.position = "bottom") +
            labs(x = "",  y = "",
                 title = wrapit(paste(input$indicador_salud_r,
                                      "según",
                                      tolower(input$salud_r_corte))),
                 caption = wrapit(unique(dat_plot$cita))) +
            scale_colour_manual(name = "", values = paleta_expandida) + 
            facet_wrap(as.formula(paste("~", salud_r_corte_var_2)))
          
        }
        
        print(plot_salud_corte)
        ggsave("www/indicador salud r.png", width = 30, height = 20, units = "cm")
        
      
      } else if(input$salud_r_corte == "Total") {
            
            req(input$indicador_salud_r, input$fecha_salud_r)
            
            dat_plot <- dat_salud_r() %>% 
                filter(ano >= input$fecha_salud_r[1] &
                           ano <= input$fecha_salud_r[2]) %>% 
                filter(corte == "Total")
            
            plot_salud <- ggplot(dat_plot,
                               aes(x = fecha, y = Valor)) +
                geom_line(size = 1, alpha = 0.5, colour = color_defecto) +
                geom_point(size = 3, colour = color_defecto) +
                theme_bdd(base_size = 12) +
                theme(axis.text.x = element_text(angle = 0),
                      legend.position = "bottom") +
                labs(x = "",  y = "",
                     title = wrapit(input$indicador_salud_r),
                     caption = wrapit(unique(dat_plot$cita))) 
            
            print(plot_salud)
            ggsave("www/indicador salud r.png", width = 30, height = 20, units = "cm")
            
            
        } else if(input$salud_r_corte == "Departamento" & 
                  input$indicador_salud_r %notin% lista_ind_2 ) {
                                              
            req(input$indicador_salud_r, input$fecha_dpto_salud_r)
            
            dat_plot <- dat_salud_r() %>%
                filter(corte == "Departamento") %>%
                filter(ano == input$fecha_dpto_salud_r) %>% 
                select(departamento, Valor, fuente, cita)
            
            dep_j <- dep %>%
                left_join(dat_plot, by = c("nombre" = "departamento"))
            
            plot_salud_dpto <- geouy::plot_geouy(dep_j,
                                               "Valor",
                                               viri_opt = "plasma",
                                               l = "n") +
                labs(x = "",  y = "",
                     title = wrapit(paste(input$indicador_salud_r, 
                                   "en",
                                   input$fecha_dpto_salud_r), w = 80),
                     caption = wrapit(unique(dat_plot$cita), w = 80)) +
                theme_bdd(base_size = 14)
            
            print(plot_salud_dpto)
            ggsave("www/indicador salud r.png", width = 30, height = 20, units = "cm")
            
            
        } else if(input$salud_r_corte != "Total") {
            
            req(input$salud_r_corte, input$indicador_salud_r, 
                input$fecha_salud_r, input$checkbox_salud_r)
            
            dat_plot <- dat_salud_r() %>%
                filter(ano >= input$fecha_salud_r[1] &
                           ano <= input$fecha_salud_r[2]) %>%
                filter(corte == input$salud_r_corte) %>%
                janitor::remove_empty("cols")
            
            salud_r_corte_var <- rlang::sym(to_varname(input$salud_r_corte))
            
            dat_plot <- filter(dat_plot,
                               !!salud_r_corte_var %in% input$checkbox_salud_r)
            
            plot_salud_corte <- ggplot(dat_plot,
                                     aes_string(x = "fecha", y = "Valor", colour = salud_r_corte_var)) +
                geom_line(size = 1, alpha = 0.5) +
                geom_point(size = 3) +
                theme_bdd(base_size = 12) +
                theme(axis.text.x = element_text(angle = 0),
                      legend.position = "bottom") +
                labs(x = "",  y = "",
                     title = wrapit(paste(input$indicador_salud_r,
                                   "según",
                                   tolower(input$salud_r_corte))),
                     caption = wrapit(unique(dat_plot$cita))) +
                scale_colour_manual(name = "", values = paleta_expandida) 
            
            print(plot_salud_corte)
            ggsave("www/indicador salud r.png", width = 30, height = 20, units = "cm")
            
        }
        
    })
    
    # 4.2.4. Descarga gráficos   =============================================
    
    output$baja_p_salud_r <- downloadHandler(
        filename <- function() {
            paste("indicador salud r", "png", sep = ".")
        },
        
        content <- function(file) {
            file.copy("www/indicador salud r.png", file)
        },
        contentType = "www/indicador salud r"
    )
    
    
    # 4.2.5. Tablas   ========================================================
    
    # Data
    salud_r_tab <- reactive({
        
      if(input$indicador_salud_r %in% lista_vunico & input$salud_r_corte == "Total"){
        
        req(input$salud_r_corte, input$indicador_salud_r)
        
        dat_salud_r() %>%
          filter(corte == "Total") %>% 
          select(Fecha, Valor) %>%
          arrange(desc(Fecha))
        
      } else if(input$indicador_salud_r %in% lista_vunico & input$salud_r_corte != "Total") {
        
        req(input$salud_r_corte, input$indicador_salud_r)
        
        salud_r_corte_var <- rlang::sym(to_varname(input$salud_r_corte))
        
        dat_cut <- dat_salud_r() %>%
          filter(corte == input$salud_r_corte) %>%
          janitor::remove_empty("cols") 
        
        dat_cut %>%     
          select(Fecha, salud_r_corte_var, Valor) %>%
          arrange(desc(Fecha)) %>% 
          pivot_wider(values_from = "Valor",
                      names_from = salud_r_corte_var)
        
        
      } else if(input$indicador_salud_r %in% lista_ind_2) {
      
      req(input$salud_r_corte, input$indicador_salud_r, input$fecha_salud_r)
        
        salud_r_corte_var <- rlang::sym(to_varname(input$salud_r_corte))
        
        dat_cut <- dat_salud_r() %>%
        filter(corte == input$salud_r_corte) %>%
        filter(!!salud_r_corte_var %in% input$checkbox_salud_r)
      
      if(input$salud_r_corte_2 == "Total"){
        
        dat_cut %>%
          filter(ano >= input$fecha_salud_r[1] &
                   ano <= input$fecha_salud_r[2]) %>%
          filter(corte_2 == "Total") %>% 
          select(Fecha, salud_r_corte_var, Valor) %>%
          arrange(desc(Fecha)) %>%
          pivot_wider(values_from = "Valor",
                      names_from = salud_r_corte_var)
        
      } else if(input$salud_r_corte_2 != "Total") {
        
        salud_r_corte_var_2 <- rlang::sym(to_varname(input$salud_r_corte_2))
        
        dat_cut %>%
          filter(ano >= input$fecha_salud_r[1] &
                   ano <= input$fecha_salud_r[2]) %>%
          filter(corte_2 == input$salud_r_corte_2) %>% 
          select(Fecha, salud_r_corte_var, salud_r_corte_var_2, Valor) %>%
          arrange(desc(Fecha)) %>%
          pivot_wider(values_from = "Valor",
                      names_from = salud_r_corte_var) 
        
      }
          
      } else if(input$salud_r_corte == "Total") {
            
            req(input$salud_r_corte, input$indicador_salud_r, input$fecha_salud_r)
            
            dat_salud_r() %>%
                filter(corte == "Total") %>% 
                filter(ano >= input$fecha_salud_r[1] &
                           ano <= input$fecha_salud_r[2]) %>%
                select(Fecha, Valor) %>%
                arrange(desc(Fecha))
            
        } else if(input$salud_r_corte != "Total") {
            
            req(input$salud_r_corte, input$indicador_salud_r, input$fecha_salud_r)
            
          salud_r_corte_var <- rlang::sym(to_varname(input$salud_r_corte))
          
            dat_cut <- dat_salud_r() %>%
                filter(corte == input$salud_r_corte) %>%
                janitor::remove_empty("cols") 
            
            dat_cut %>%     
                filter(ano >= input$fecha_salud_r[1] &
                           ano <= input$fecha_salud_r[2]) %>% 
                select(Fecha, salud_r_corte_var, Valor) %>%
                arrange(desc(Fecha)) %>% 
                pivot_wider(values_from = "Valor",
                            names_from = salud_r_corte_var)
            
        }
    })
    
    # Metadata 
    salud_r_meta <- reactive({
        
        dat_salud_r() %>%
          select(nomindicador, derecho, conindicador, tipoind, definicion, calculo, cita) %>% 
          mutate(`Mirador DESCA - UMAD/FCS – INDDHH` = " ") %>% 
          distinct() %>% 
          gather(key = "", value = " ")
        
    })
    
    # Excel
    list_salud_r <- reactive({
        list_salud_r <- list("Data" = salud_r_tab(),
                           "Metadata" = salud_r_meta())
    })
    
    # Render
    output$table_salud_r <- renderDT({
        
        DT::datatable(salud_r_tab(),
                      rownames = FALSE,
                      caption = htmltools::tags$caption(
                          input$indicador_salud_r,
                          style = "color:black; font-size:110%;")
        ) 
        
    })
    
    # 4.2.6. Descarga tablas   ================================================
    
    output$dwl_tab_salud_r <- downloadHandler(
        
        filename = function() {
            paste("pp-", input$indicador_salud_r, ".xlsx", sep = "")
        },
        content = function(file) {
            
            openxlsx::write.xlsx(list_salud_r(), file)
            
        }
    )
    
    
    
    ### 5.1. Seguridad Social Políticas   ==============================================
    
    # 5.1.1. Data reactiva   =================================================
    
    dat_ssocial_pp <- reactive({
      
      req(input$indicador_ssocial_pp)
      
      dat %>%
        filter(nomindicador == input$indicador_ssocial_pp) 
      
    })
    
    output$selector_ssocial_pp_corte <- renderUI({
      
      selectInput(
        inputId = "ssocial_pp_corte",
        label = "Seleccione corte:",
        choices = dat_ssocial_pp() %>% 
          select(corte) %>%
          arrange(corte) %>% 
          unique() %>% 
          pull(),
        selected = dat_ssocial_pp() %>% 
          filter(jerarquia == "1") %>%  
          distinct(corte) %>% 
          pull()
      )
      
    })
    
    output$selector_ssocial_pp_corte_2 <- renderUI({
      
      if(input$indicador_ssocial_pp %in% lista_ind_2){
        
        selectInput(
          inputId = "ssocial_pp_corte_2",
          label = "Seleccione primer corte:",
          choices = dat_ssocial_pp() %>% 
            select(corte_2) %>%
            arrange(corte_2) %>% 
            unique() %>% 
            pull(),
          selected = dat_ssocial_pp() %>% 
            filter(jerarquia == "1") %>%  
            distinct(corte_2) %>% 
            pull()
        )
        
      } else {
        
        NULL
      }
      
    })
    
    # Selector de fecha
    output$s_ssocial_pp_fecha <- renderUI({
      
      if(input$ssocial_pp_corte == "Departamento" & input$indicador_ssocial_pp %notin% lista_ind_2) {
        
        req(input$ssocial_pp_corte, input$indicador_ssocial_pp)
        
        req(nrow(dat_ssocial_pp()) > 0)
        
        selectInput(
          inputId = "fecha_dpto_ssocial_pp",
          label = "Seleccione año:",
          choices = dat_ssocial_pp() %>% 
            filter(nomindicador == input$indicador_ssocial_pp) %>%
            drop_na(Valor) %>%
            select(ano) %>%
            arrange(desc(ano)) %>% 
            unique() %>% 
            pull(),
          selected = 2019
        )
        
      } else if (input$indicador_ssocial_pp %in% lista_vunico){
        
        return(NULL)
        
      } else  {
        
        req(input$ssocial_pp_corte, input$indicador_ssocial_pp)
        req(nrow(dat_ssocial_pp()) > 0)
        
        tagList(
          # tags$style(type = 'text/css', 
          #            '#big_slider .irs-grid-text {font-size: 12px; transform: rotate(-90deg) translate(-10px);} ,.irs-grid-pol.large {height: 0px;}'),
          # div(id = 'big_slider',
          
          sliderInput("fecha_ssocial_pp", 
                      label = "Rango de tiempo", 
                      sep = "",
                      dragRange = T,
                      min = min(dat_ssocial_pp()$ano), 
                      max = max(dat_ssocial_pp()$ano), 
                      value = c(min(dat_ssocial_pp()$ano), 
                                max(dat_ssocial_pp()$ano))
          )
        )
        
      }
    })
    
    
    output$chbox_ssocial_pp <- renderUI({
      
      if(input$ssocial_pp_corte %notin% c("Total", "Departamento") & input$indicador_ssocial_pp %notin% lista_vunico) {
        
        ssocial_pp_corte_var <- rlang::sym(to_varname(input$ssocial_pp_corte))
        
        checkboxGroupInput(inputId = "checkbox_ssocial_pp",
                           label = "Seleccione categorías",
                           inline = TRUE,
                           choices =  dat_ssocial_pp() %>%
                             filter(corte == input$ssocial_pp_corte) %>% 
                             distinct(!!ssocial_pp_corte_var) %>%
                             pull(),
                           selected = dat_ssocial_pp() %>%
                             filter(corte == input$ssocial_pp_corte) %>% 
                             filter(jerarquia_cat == "1") %>%
                             distinct(!!ssocial_pp_corte_var) %>%
                             pull()
        )
        
      } else {
        
        return(NULL)
      }
      
    })
    
    # # Selector de corte según categoría y data temporal
    # dat_ssocial_pp <- reactive({
    #   
    #   req(input$ssocial_pp_corte)
    #   
    #   if(input$indicador_ssocial_pp %in% lista_ind_2){
    #     
    #     dat_salarios <- dat_ssocial_pp() %>%
    #       filter(corte_2 == input$ssocial_pp_corte_2) %>% 
    #       filter(corte == input$ssocial_pp_corte) %>%
    #       janitor::remove_empty("cols")
    #     
    #   } else {
    #     
    #     dat_ssocial_pp() %>%
    #       filter(corte == input$ssocial_pp_corte) %>%
    #       janitor::remove_empty("cols")
    #     
    #   }
    # })
    
    # 5.1.2. Metadata   ======================================================
    
    # Title
    output$title_ssocial_pp <- renderUI({ 
      helpText(HTML(unique(dat_ssocial_pp()$nomindicador)))
    })
    
    # Subtitle
    output$subtitle_ssocial_pp <- renderUI({ 
      helpText(HTML(unique(dat_ssocial_pp()$definicion)))
    })
    
    # Calculo
    output$calculo_ssocial_pp <- renderUI({ 
      helpText(HTML(paste("<b> Forma de cálculo:</b>", unique(dat_ssocial_pp()$calculo))))
    })
    
    
    # 5.1.3. Gráficos   ======================================================
    
    output$plot_ssocial_pp <- renderPlot({
      
      req(input$indicador_ssocial_pp)
      
      if(input$indicador_ssocial_pp %in% lista_vunico & input$indicador_ssocial_pp %in% lista_ind_2){
        
        req(input$ssocial_pp_corte, input$indicador_ssocial_pp)
        
        ssocial_pp_corte_var <- rlang::sym(to_varname(input$ssocial_pp_corte))
        ssocial_pp_corte_var_2 <- rlang::sym(to_varname(input$ssocial_pp_corte_2))
        
        dat_plot <- dat_ssocial_pp() %>%
          filter(ano >= input$fecha_ssocial_pp[1] &
                   ano <= input$fecha_ssocial_pp[2]) %>%
          filter(corte == input$ssocial_pp_corte) %>%
          filter(!!ssocial_pp_corte_var %in% input$checkbox_ssocial_pp) %>% 
          filter(corte_2 == input$ssocial_pp_corte_2)  
        
        plot_salud_corte <- ggplot(dat_plot,
                                   aes_string(x = "fecha_cat", y = "Valor",
                                              fill = ssocial_pp_corte_var)) +
          geom_col(position = "dodge", width = .7, alpha = .8) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x=element_blank(),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_ssocial_pp,
                                    "según",
                                    tolower(input$ssocial_pp_corte),
                                    "en",
                                    unique(dat_plot$fecha_cat))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_fill_brewer(name = "", palette = "Paired") +
          facet_wrap(as.formula(paste("~", ssocial_pp_corte_var_2)))
        
        print(plot_salud_corte)
        ggsave("www/indicador salud pp.png", width = 30, height = 20, units = "cm")
        
      } else if(input$indicador_ssocial_pp %in% lista_vunico & input$ssocial_pp_corte != "Departamento") {
        
        req(input$ssocial_pp_corte, input$indicador_ssocial_pp)
        
        dat_plot <- dat_ssocial_pp() %>%
          filter(corte == input$ssocial_pp_corte) %>%
          janitor::remove_empty("cols")
        
        if(input$ssocial_pp_corte == "Total"){
          
          plot_salud_corte <- ggplot(dat_plot,
                                     aes_string(x = "fecha_cat", y = "Valor")) +
            geom_col(position = "dodge", width = .4, alpha = .8, fill = color_defecto) +
            geom_text(aes(label = Valor), vjust = -0.4, fontface = "bold", size = 5) +
            theme_bdd(base_size = 12) +
            theme(axis.text.x=element_blank(),
                  legend.position = "bottom") +
            labs(x = "",  y = "",
                 title = wrapit(paste(input$indicador_ssocial_pp,
                                      "según",
                                      tolower(input$ssocial_pp_corte),
                                      "en",
                                      unique(dat_plot$fecha_cat))),
                 caption = wrapit(unique(dat_plot$cita))) 
          
          print(plot_salud_corte)
          ggsave("www/indicador salud pp.png", width = 30, height = 20, units = "cm")
          
          
        } else {
          
          ssocial_pp_corte_var <- rlang::sym(to_varname(input$ssocial_pp_corte))
          
          plot_salud_corte <- ggplot(dat_plot,
                                     aes_string(x = "fecha_cat", y = "Valor",
                                                fill = ssocial_pp_corte_var)) +
            geom_col(position = "dodge", width = .7, alpha = .8) +
            theme_bdd(base_size = 12) +
            theme(axis.text.x=element_blank(),
                  legend.position = "bottom") +
            labs(x = "",  y = "",
                 title = wrapit(paste(input$indicador_ssocial_pp,
                                      "según",
                                      tolower(input$ssocial_pp_corte),
                                      "en",
                                      unique(dat_plot$fecha_cat))),
                 caption = wrapit(unique(dat_plot$cita))) +
            scale_fill_brewer(name = "", palette = "Paired") 
          
          print(plot_salud_corte)
          ggsave("www/indicador salud pp.png", width = 30, height = 20, units = "cm")
          
        }
        
        print(plot_salud_corte)
        ggsave("www/indicador salud pp.png", width = 30, height = 20, units = "cm")
        
      } else  if(input$indicador_ssocial_pp %in% lista_ind_2) {
        
        req(input$ssocial_pp_corte, input$ssocial_pp_corte_2,
            input$fecha_ssocial_pp, input$checkbox_ssocial_pp)
        
        ssocial_pp_corte_var <- rlang::sym(to_varname(input$ssocial_pp_corte))
        
        dat_plot <- dat_ssocial_pp() %>%
          filter(ano >= input$fecha_ssocial_pp[1] &
                   ano <= input$fecha_ssocial_pp[2]) %>%
          filter(corte == input$ssocial_pp_corte) %>%
          filter(!!ssocial_pp_corte_var %in% input$checkbox_ssocial_pp)
        
        if(input$ssocial_pp_corte_2 == "Total"){
          
          dat_plot <- dat_plot %>% 
            filter(corte_2 == "Total")
          
          plot_ssocial_corte <- ggplot(dat_plot,
                                   aes_string(x = "fecha", y = "Valor", colour = ssocial_pp_corte_var)) +
            geom_line(size = 1, alpha = 0.5) +
            geom_point(size = 3) +
            theme_bdd(base_size = 12) +
            theme(axis.text.x = element_text(angle = 0),
                  legend.position = "bottom") +
            labs(x = "",  y = "",
                 title = wrapit(paste(input$indicador_ssocial_pp,
                                      "según",
                                      tolower(input$ssocial_pp_corte))),
                 caption = wrapit(unique(dat_plot$cita))) +
            scale_colour_manual(name = "", values = paleta_expandida) 
          
        } else if(input$ssocial_pp_corte_2 != "Total") {
          
          ssocial_pp_corte_var_2 <- rlang::sym(to_varname(input$ssocial_pp_corte_2))
          
          dat_plot <- dat_plot %>%
            filter(corte_2 == input$ssocial_pp_corte_2)  
          
          plot_ssocial_corte <- ggplot(dat_plot,
                                   aes_string(x = "fecha", y = "Valor", colour = ssocial_pp_corte_var)) +
            geom_line(size = 1, alpha = 0.5) +
            geom_point(size = 3) +
            theme_bdd(base_size = 12) +
            theme(axis.text.x = element_text(angle = 0),
                  legend.position = "bottom") +
            labs(x = "",  y = "",
                 title = wrapit(paste(input$indicador_ssocial_pp,
                                      "según",
                                      tolower(input$ssocial_pp_corte))),
                 caption = wrapit(unique(dat_plot$cita))) +
            scale_colour_manual(name = "", values = paleta_expandida) + 
            facet_wrap(as.formula(paste("~", ssocial_pp_corte_var_2)))
          
        }
        
        print(plot_ssocial_corte)
        ggsave("www/indicador ssocial pp.png", width = 30, height = 20, units = "cm")
        
        
      } else if(input$ssocial_pp_corte == "Total") {
        
        req(input$indicador_ssocial_pp, input$fecha_ssocial_pp)
        
        dat_plot <- dat_ssocial_pp() %>% 
          filter(ano >= input$fecha_ssocial_pp[1] &
                   ano <= input$fecha_ssocial_pp[2]) %>% 
          filter(corte == "Total")
        
        plot_ssocial <- ggplot(dat_plot,
                           aes(x = fecha, y = Valor)) +
          geom_line(size = 1, alpha = 0.5, colour = color_defecto) +
          geom_point(size = 3, colour = color_defecto) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(input$indicador_ssocial_pp),
               caption = wrapit(unique(dat_plot$cita))) 
        
        print(plot_ssocial)
        ggsave("www/indicador ssocial pp.png", width = 30, height = 20, units = "cm")
        
        
      } else if(input$ssocial_pp_corte == "Departamento" & 
                input$indicador_ssocial_pp %notin% lista_ind_2 ) {
        
        req(input$indicador_ssocial_pp, input$fecha_dpto_ssocial_pp)
        
        dat_plot <- dat_ssocial_pp() %>%
          filter(corte == "Departamento") %>%
          filter(ano == input$fecha_dpto_ssocial_pp) %>% 
          select(departamento, Valor, fuente, cita)
        
        dep_j <- dep %>%
          left_join(dat_plot, by = c("nombre" = "departamento"))
        
        plot_ssocial_dpto <- geouy::plot_geouy(dep_j,
                                           "Valor",
                                           viri_opt = "plasma",
                                           l = "n") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_ssocial_pp, 
                                    "en",
                                    input$fecha_dpto_ssocial_pp), w = 80),
               caption = wrapit(unique(dat_plot$cita), w = 80)) +
          theme_bdd(base_size = 14)
        
        print(plot_ssocial_dpto)
        ggsave("www/indicador ssocial pp.png", width = 30, height = 20, units = "cm")
        
        
      } else if(input$ssocial_pp_corte != "Total") {
        
        req(input$ssocial_pp_corte, input$indicador_ssocial_pp, 
            input$fecha_ssocial_pp, input$checkbox_ssocial_pp)
        
        dat_plot <- dat_ssocial_pp() %>%
          filter(ano >= input$fecha_ssocial_pp[1] &
                   ano <= input$fecha_ssocial_pp[2]) %>%
          filter(corte == input$ssocial_pp_corte) %>%
          janitor::remove_empty("cols")
        
        ssocial_pp_corte_var <- rlang::sym(to_varname(input$ssocial_pp_corte))
        
        dat_plot <- filter(dat_plot,
                           !!ssocial_pp_corte_var %in% input$checkbox_ssocial_pp)
        
        plot_ssocial_corte <- ggplot(dat_plot,
                                 aes_string(x = "fecha", y = "Valor", colour = ssocial_pp_corte_var)) +
          geom_line(size = 1, alpha = 0.5) +
          geom_point(size = 3) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_ssocial_pp,
                                    "según",
                                    tolower(input$ssocial_pp_corte))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_colour_manual(name = "", values = paleta_expandida) 
        
        print(plot_ssocial_corte)
        ggsave("www/indicador ssocial pp.png", width = 30, height = 20, units = "cm")
        
      }
      
    })
    
    # 5.1.4. Descarga gráficos   =============================================
    
    output$baja_p_ssocial_pp <- downloadHandler(
      filename <- function() {
        paste("indicador ssocial pp", "png", sep = ".")
      },
      
      content <- function(file) {
        file.copy("www/indicador ssocial pp.png", file)
      },
      contentType = "www/indicador ssocial pp"
    )
    
    
    # 5.1.5. Tablas   ========================================================
    
    # Data
    ssocial_pp_tab <- reactive({
      
      if(input$indicador_ssocial_pp %in% lista_vunico & input$ssocial_pp_corte == "Total"){
        
        req(input$ssocial_pp_corte, input$indicador_ssocial_pp)
        
        dat_ssocial_pp() %>%
          filter(corte == "Total") %>% 
          select(Fecha, Valor) %>%
          arrange(desc(Fecha))
        
      } else if(input$indicador_ssocial_pp %in% lista_vunico & input$ssocial_pp_corte != "Total") {
        
        req(input$ssocial_pp_corte, input$indicador_ssocial_pp)
        
        ssocial_pp_corte_var <- rlang::sym(to_varname(input$ssocial_pp_corte))
        
        dat_cut <- dat_ssocial_pp() %>%
          filter(corte == input$ssocial_pp_corte) %>%
          janitor::remove_empty("cols") 
        
        dat_cut %>%     
          select(Fecha, ssocial_pp_corte_var, Valor) %>%
          arrange(desc(Fecha)) %>% 
          pivot_wider(values_from = "Valor",
                      names_from = ssocial_pp_corte_var)
        
        
      } else if(input$indicador_ssocial_pp %in% lista_ind_2) {
        
        req(input$ssocial_pp_corte, input$indicador_ssocial_pp, input$fecha_ssocial_pp)
        
        ssocial_pp_corte_var <- rlang::sym(to_varname(input$ssocial_pp_corte))
        
        dat_cut <- dat_ssocial_pp() %>%
          filter(corte == input$ssocial_pp_corte) %>%
          filter(!!ssocial_pp_corte_var %in% input$checkbox_ssocial_pp)
        
        if(input$ssocial_pp_corte_2 == "Total"){
          
          dat_cut %>%
            filter(ano >= input$fecha_ssocial_pp[1] &
                     ano <= input$fecha_ssocial_pp[2]) %>%
            filter(corte_2 == "Total") %>% 
            select(Fecha, ssocial_pp_corte_var, Valor) %>%
            arrange(desc(Fecha)) %>%
            pivot_wider(values_from = "Valor",
                        names_from = ssocial_pp_corte_var)
          
        } else if(input$ssocial_pp_corte_2 != "Total") {
          
          ssocial_pp_corte_var_2 <- rlang::sym(to_varname(input$ssocial_pp_corte_2))
          
          dat_cut %>%
            filter(ano >= input$fecha_ssocial_pp[1] &
                     ano <= input$fecha_ssocial_pp[2]) %>%
            filter(corte_2 == input$ssocial_pp_corte_2) %>% 
            select(Fecha, ssocial_pp_corte_var, ssocial_pp_corte_var_2, Valor) %>%
            arrange(desc(Fecha)) %>%
            pivot_wider(values_from = "Valor",
                        names_from = ssocial_pp_corte_var) 
          
        }
        
      } else if(input$ssocial_pp_corte == "Total") {
        
        req(input$ssocial_pp_corte, input$indicador_ssocial_pp, input$fecha_ssocial_pp)
        
        dat_ssocial_pp() %>%
          filter(corte == "Total") %>% 
          filter(ano >= input$fecha_ssocial_pp[1] &
                   ano <= input$fecha_ssocial_pp[2]) %>%
          select(Fecha, Valor) %>%
          arrange(desc(Fecha))
        
      } else if(input$ssocial_pp_corte != "Total") {
        
        req(input$ssocial_pp_corte, input$indicador_ssocial_pp, input$fecha_ssocial_pp)
        
        ssocial_pp_corte_var <- rlang::sym(to_varname(input$ssocial_pp_corte))
        
        dat_cut <- dat_ssocial_pp() %>%
          filter(corte == input$ssocial_pp_corte) %>%
          janitor::remove_empty("cols") 
        
        dat_cut %>%     
          filter(ano >= input$fecha_ssocial_pp[1] &
                   ano <= input$fecha_ssocial_pp[2]) %>% 
          select(Fecha, ssocial_pp_corte_var, Valor) %>%
          arrange(desc(Fecha)) %>% 
          pivot_wider(values_from = "Valor",
                      names_from = ssocial_pp_corte_var)
        
      }
    })
    
    # Metadata 
    ssocial_pp_meta <- reactive({
      
      dat_ssocial_pp() %>%
        select(nomindicador, derecho, conindicador, tipoind, definicion, calculo, cita) %>% 
        mutate(`Mirador DESCA - UMAD/FCS – INDDHH` = " ") %>% 
        distinct() %>% 
        gather(key = "", value = " ")
      
    })
    
    # Excel
    list_ssocial_pp <- reactive({
      list_ssocial_pp <- list("Data" = ssocial_pp_tab(),
                            "Metadata" = ssocial_pp_meta())
    })
    
    # Render
    output$table_ssocial_pp <- renderDT({
      
      DT::datatable(ssocial_pp_tab(),
                    rownames = FALSE,
                    caption = htmltools::tags$caption(
                      input$indicador_ssocial_pp,
                      style = "color:black; font-size:110%;")
      ) 
      
    })
    
    # 5.1.6. Descarga tablas   ================================================
    
    output$dwl_tab_ssocial_pp <- downloadHandler(
      
      filename = function() {
        paste("resultados-", input$indicador_ssocial_pp, ".xlsx", sep = "")
      },
      content = function(file) {
        
        openxlsx::write.xlsx(list_ssocial_pp(), file)
        
      }
    )
    
  ### 5.2. Seguridad Social Resultados   ===================================
    
    # 5.2.1. Data reactiva   =================================================
    
    dat_ssocial_r <- reactive({
      
      req(input$indicador_ssocial_r)
      
      dat %>%
        filter(nomindicador == input$indicador_ssocial_r) 
      
    })
    
    output$selector_ssocial_r_corte <- renderUI({
      
      selectInput(
        inputId = "ssocial_r_corte",
        label = "Seleccione corte:",
        choices = dat_ssocial_r() %>% 
          select(corte) %>%
          arrange(corte) %>% 
          unique() %>% 
          pull(),
        selected = dat_ssocial_r() %>% 
          filter(jerarquia == "1") %>%  
          distinct(corte) %>% 
          pull()
      )
      
    })
    
    output$selector_ssocial_r_corte_2 <- renderUI({
      
      if(input$indicador_ssocial_r %in% lista_ind_2){
        
        selectInput(
          inputId = "ssocial_r_corte_2",
          label = "Seleccione primer corte:",
          choices = dat_ssocial_r() %>% 
            select(corte_2) %>%
            arrange(corte_2) %>% 
            unique() %>% 
            pull(),
          selected = dat_ssocial_r() %>% 
            filter(jerarquia == "1") %>%  
            distinct(corte_2) %>% 
            pull()
        )
        
      } else {
        
        NULL
      }
      
    })
    
    # Selector de fecha
    output$s_ssocial_r_fecha <- renderUI({
      
      if(input$ssocial_r_corte == "Departamento" & input$indicador_ssocial_r %notin% lista_ind_2) {
        
        req(input$ssocial_r_corte, input$indicador_ssocial_r)
        
        req(nrow(dat_ssocial_r()) > 0)
        
        selectInput(
          inputId = "fecha_dpto_ssocial_r",
          label = "Seleccione año:",
          choices = dat_ssocial_r() %>% 
            filter(nomindicador == input$indicador_ssocial_r) %>%
            drop_na(Valor) %>%
            select(ano) %>%
            arrange(desc(ano)) %>% 
            unique() %>% 
            pull(),
          selected = 2019
        )
        
      } else if (input$indicador_ssocial_r %in% lista_vunico){
        
        return(NULL)
        
      } else  {
        
        req(input$ssocial_r_corte, input$indicador_ssocial_r)
        req(nrow(dat_ssocial_r()) > 0)
        
        tagList(
          # tags$style(type = 'text/css', 
          #            '#big_slider .irs-grid-text {font-size: 12px; transform: rotate(-90deg) translate(-10px);} ,.irs-grid-pol.large {height: 0px;}'),
          # div(id = 'big_slider',
          
          sliderInput("fecha_ssocial_r", 
                      label = "Rango de tiempo", 
                      sep = "",
                      dragRange = T,
                      min = min(dat_ssocial_r()$ano), 
                      max = max(dat_ssocial_r()$ano), 
                      value = c(min(dat_ssocial_r()$ano), 
                                max(dat_ssocial_r()$ano))
          )
        )
        
      }
    })
    
    
    output$chbox_ssocial_r <- renderUI({
      
      if(input$ssocial_r_corte %notin% c("Total", "Departamento") & input$indicador_ssocial_r %notin% lista_vunico) {
        
        ssocial_r_corte_var <- rlang::sym(to_varname(input$ssocial_r_corte))
        
        checkboxGroupInput(inputId = "checkbox_ssocial_r",
                           label = "Seleccione categorías",
                           inline = TRUE,
                           choices =  dat_ssocial_r() %>%
                             filter(corte == input$ssocial_r_corte) %>% 
                             distinct(!!ssocial_r_corte_var) %>%
                             pull(),
                           selected = dat_ssocial_r() %>%
                             filter(corte == input$ssocial_r_corte) %>% 
                             filter(jerarquia_cat == "1") %>%
                             distinct(!!ssocial_r_corte_var) %>%
                             pull()
        )
        
      } else {
        
        return(NULL)
      }
      
    })
    
    # # Selector de corte según categoría y data temporal
    # dat_ssocial_r <- reactive({
    #   
    #   req(input$ssocial_r_corte)
    #   
    #   if(input$indicador_ssocial_r %in% lista_ind_2){
    #     
    #     dat_salarios <- dat_ssocial_r() %>%
    #       filter(corte_2 == input$ssocial_r_corte_2) %>% 
    #       filter(corte == input$ssocial_r_corte) %>%
    #       janitor::remove_empty("cols")
    #     
    #   } else {
    #     
    #     dat_ssocial_r() %>%
    #       filter(corte == input$ssocial_r_corte) %>%
    #       janitor::remove_empty("cols")
    #     
    #   }
    # })
    
    # 5.2.2. Metadata   ======================================================
    
    # Title
    output$title_ssocial_r <- renderUI({ 
      helpText(HTML(unique(dat_ssocial_r()$nomindicador)))
    })
    
    # Subtitle
    output$subtitle_ssocial_r <- renderUI({ 
      helpText(HTML(unique(dat_ssocial_r()$definicion)))
    })
    
    # Calculo
    output$calculo_ssocial_r <- renderUI({ 
      helpText(HTML(paste("<b> Forma de cálculo:</b>", unique(dat_ssocial_r()$calculo))))
    })
    
    
    # 5.2.3. Gráficos   ======================================================
    
    output$plot_ssocial_r <- renderPlot({
      
      req(input$indicador_ssocial_r)
      
      # Indicador especial (tiene solo una fecha entonces va con barras)
      
      if(input$indicador_ssocial_r %in% lista_vunico & input$indicador_ssocial_r %in% lista_ind_2){
        
        req(input$ssocial_r_corte, input$indicador_ssocial_r)
        
        ssocial_r_corte_var <- rlang::sym(to_varname(input$ssocial_r_corte))
        ssocial_r_corte_var_2 <- rlang::sym(to_varname(input$ssocial_r_corte_2))
        
        dat_plot <- dat_ssocial_r() %>%
          filter(ano >= input$fecha_ssocial_r[1] &
                   ano <= input$fecha_ssocial_r[2]) %>%
          filter(corte == input$ssocial_r_corte) %>%
          filter(!!ssocial_r_corte_var %in% input$checkbox_ssocial_r) %>% 
          filter(corte_2 == input$ssocial_r_corte_2)  
        
        plot_ssocial_corte <- ggplot(dat_plot,
                                     aes_string(x = "fecha_cat", y = "Valor",
                                                fill = ssocial_r_corte_var)) +
          geom_col(position = "dodge", width = .7, alpha = .8) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x=element_blank(),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_ssocial_r,
                                    "según",
                                    tolower(input$ssocial_r_corte),
                                    "en",
                                    unique(dat_plot$fecha_cat))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_fill_brewer(name = "", palette = "Paired") +
          facet_wrap(as.formula(paste("~", ssocial_r_corte_var_2)))
        
        print(plot_ssocial_corte)
        ggsave("www/indicador ssocial r.png", width = 30, height = 20, units = "cm")
        
      } else if(input$indicador_ssocial_r %in% lista_vunico & input$ssocial_r_corte != "Departamento") {
        
        req(input$ssocial_r_corte, input$indicador_ssocial_r)
        
        dat_plot <- dat_ssocial_r() %>%
          filter(corte == input$ssocial_r_corte) %>%
          janitor::remove_empty("cols")
        
        if(input$ssocial_r_corte == "Total"){
          
          plot_ssocial_corte <- ggplot(dat_plot,
                                       aes_string(x = "fecha_cat", y = "Valor")) +
            geom_col(position = "dodge", width = .4, alpha = .8, fill = color_defecto) +
            geom_text(aes(label = Valor), vjust = -0.4, fontface = "bold", size = 5) +
            theme_bdd(base_size = 12) +
            theme(axis.text.x=element_blank(),
                  legend.position = "bottom") +
            labs(x = "",  y = "",
                 title = wrapit(paste(input$indicador_ssocial_r,
                                      "según",
                                      tolower(input$ssocial_r_corte),
                                      "en",
                                      unique(dat_plot$fecha_cat))),
                 caption = wrapit(unique(dat_plot$cita))) 
          
          print(plot_ssocial_corte)
          ggsave("www/indicador ssocial r.png", width = 30, height = 20, units = "cm")
          
          
        } else {
          
          ssocial_r_corte_var <- rlang::sym(to_varname(input$ssocial_r_corte))
          
          plot_ssocial_corte <- ggplot(dat_plot,
                                       aes_string(x = "fecha_cat", y = "Valor",
                                                  fill = ssocial_r_corte_var)) +
            geom_col(position = "dodge", width = .7, alpha = .8) +
            theme_bdd(base_size = 12) +
            theme(axis.text.x=element_blank(),
                  legend.position = "bottom") +
            labs(x = "",  y = "",
                 title = wrapit(paste(input$indicador_ssocial_r,
                                      "según",
                                      tolower(input$ssocial_r_corte),
                                      "en",
                                      unique(dat_plot$fecha_cat))),
                 caption = wrapit(unique(dat_plot$cita))) +
            scale_fill_brewer(name = "", palette = "Paired") 
          
          print(plot_ssocial_corte)
          ggsave("www/indicador ssocial r.png", width = 30, height = 20, units = "cm")
          
        }
        
        print(plot_ssocial_corte)
        ggsave("www/indicador ssocial r.png", width = 30, height = 20, units = "cm")
        
      } else if(input$indicador_ssocial_r %in% lista_ind_2) {
        
        req(input$ssocial_r_corte, input$ssocial_r_corte_2,
            input$fecha_ssocial_r, input$checkbox_ssocial_r)
        
        ssocial_r_corte_var <- rlang::sym(to_varname(input$ssocial_r_corte))
        
        dat_plot <- dat_ssocial_r() %>%
          filter(ano >= input$fecha_ssocial_r[1] &
                   ano <= input$fecha_ssocial_r[2]) %>%
          filter(corte == input$ssocial_r_corte) %>%
          filter(!!ssocial_r_corte_var %in% input$checkbox_ssocial_r)
        
        if(input$ssocial_r_corte_2 == "Total"){
          
          dat_plot <- dat_plot %>% 
            filter(corte_2 == "Total")
          
          plot_ssocial_corte <- ggplot(dat_plot,
                                   aes_string(x = "fecha", y = "Valor", colour = ssocial_r_corte_var)) +
            geom_line(size = 1, alpha = 0.5) +
            geom_point(size = 3) +
            theme_bdd(base_size = 12) +
            theme(axis.text.x = element_text(angle = 0),
                  legend.position = "bottom") +
            labs(x = "",  y = "",
                 title = wrapit(paste(input$indicador_ssocial_r,
                                      "según",
                                      tolower(input$ssocial_r_corte))),
                 caption = wrapit(unique(dat_plot$cita))) +
            scale_colour_manual(name = "", values = paleta_expandida) 
          
        } else if(input$ssocial_r_corte_2 != "Total") {
          
          ssocial_r_corte_var_2 <- rlang::sym(to_varname(input$ssocial_r_corte_2))
          
          dat_plot <- dat_plot %>%
            filter(corte_2 == input$ssocial_r_corte_2)  
          
          plot_ssocial_corte <- ggplot(dat_plot,
                                   aes_string(x = "fecha", y = "Valor", colour = ssocial_r_corte_var)) +
            geom_line(size = 1, alpha = 0.5) +
            geom_point(size = 3) +
            theme_bdd(base_size = 12) +
            theme(axis.text.x = element_text(angle = 0),
                  legend.position = "bottom") +
            labs(x = "",  y = "",
                 title = wrapit(paste(input$indicador_ssocial_r,
                                      "según",
                                      tolower(input$ssocial_r_corte))),
                 caption = wrapit(unique(dat_plot$cita))) +
            scale_colour_manual(name = "", values = paleta_expandida) + 
            facet_wrap(as.formula(paste("~", ssocial_r_corte_var_2)))
          
        }
        
        print(plot_ssocial_corte)
        ggsave("www/indicador ssocial r.png", width = 30, height = 20, units = "cm")
        
        
      } else if(input$ssocial_r_corte == "Total") {
        
        req(input$indicador_ssocial_r, input$fecha_ssocial_r)
        
        dat_plot <- dat_ssocial_r() %>% 
          filter(ano >= input$fecha_ssocial_r[1] &
                   ano <= input$fecha_ssocial_r[2]) %>% 
          filter(corte == "Total")
        
        plot_ssocial <- ggplot(dat_plot,
                           aes(x = fecha, y = Valor)) +
          geom_line(size = 1, alpha = 0.5, colour = color_defecto) +
          geom_point(size = 3, colour = color_defecto) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(input$indicador_ssocial_r),
               caption = wrapit(unique(dat_plot$cita))) 
        
        print(plot_ssocial)
        ggsave("www/indicador ssocial r.png", width = 30, height = 20, units = "cm")
        
        
      } else if(input$ssocial_r_corte == "Departamento" & 
                input$indicador_ssocial_r %notin% lista_ind_2 ) {
        
        req(input$indicador_ssocial_r, input$fecha_dpto_ssocial_r)
        
        dat_plot <- dat_ssocial_r() %>%
          filter(corte == "Departamento") %>%
          filter(ano == input$fecha_dpto_ssocial_r) %>% 
          select(departamento, Valor, fuente, cita)
        
        dep_j <- dep %>%
          left_join(dat_plot, by = c("nombre" = "departamento"))
        
        plot_ssocial_dpto <- geouy::plot_geouy(dep_j,
                                           "Valor",
                                           viri_opt = "plasma",
                                           l = "n") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_ssocial_r, 
                                    "en",
                                    input$fecha_dpto_ssocial_r), w = 80),
               caption = wrapit(unique(dat_plot$cita), w = 80)) +
          theme_bdd(base_size = 14)
        
        print(plot_ssocial_dpto)
        ggsave("www/indicador ssocial r.png", width = 30, height = 20, units = "cm")
        
        
      } else if(input$ssocial_r_corte != "Total") {
        
        req(input$ssocial_r_corte, input$indicador_ssocial_r, 
            input$fecha_ssocial_r, input$checkbox_ssocial_r)
        
        dat_plot <- dat_ssocial_r() %>%
          filter(ano >= input$fecha_ssocial_r[1] &
                   ano <= input$fecha_ssocial_r[2]) %>%
          filter(corte == input$ssocial_r_corte) %>%
          janitor::remove_empty("cols")
        
        ssocial_r_corte_var <- rlang::sym(to_varname(input$ssocial_r_corte))
        
        dat_plot <- filter(dat_plot,
                           !!ssocial_r_corte_var %in% input$checkbox_ssocial_r)
        
        plot_ssocial_corte <- ggplot(dat_plot,
                                 aes_string(x = "fecha", y = "Valor", colour = ssocial_r_corte_var)) +
          geom_line(size = 1, alpha = 0.5) +
          geom_point(size = 3) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_ssocial_r,
                                    "según",
                                    tolower(input$ssocial_r_corte))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_colour_manual(name = "", values = paleta_expandida) 
        
        print(plot_ssocial_corte)
        ggsave("www/indicador ssocial r.png", width = 30, height = 20, units = "cm")
        
      }
      
    })
    
    # 5.2.4. Descarga gráficos   =============================================
    
    output$baja_p_ssocial_r <- downloadHandler(
      filename <- function() {
        paste("indicador ssocial r", "png", sep = ".")
      },
      
      content <- function(file) {
        file.copy("www/indicador ssocial r.png", file)
      },
      contentType = "www/indicador ssocial r"
    )
    
    
    # 5.2.5. Tablas   ========================================================
    
    # Data
    ssocial_r_tab <- reactive({
      
      if(input$indicador_ssocial_r %in% lista_vunico & input$ssocial_r_corte == "Total"){
        
        req(input$ssocial_r_corte, input$indicador_ssocial_r)
        
        dat_ssocial_r() %>%
          filter(corte == "Total") %>% 
          select(Fecha, Valor) %>%
          arrange(desc(Fecha))
        
      } else if(input$indicador_ssocial_r %in% lista_vunico & input$ssocial_r_corte != "Total") {
        
        req(input$ssocial_r_corte, input$indicador_ssocial_r)
        
        ssocial_r_corte_var <- rlang::sym(to_varname(input$ssocial_r_corte))
        
        dat_cut <- dat_ssocial_r() %>%
          filter(corte == input$ssocial_r_corte) %>%
          janitor::remove_empty("cols") 
        
        dat_cut %>%     
          select(Fecha, ssocial_r_corte_var, Valor) %>%
          arrange(desc(Fecha)) %>% 
          pivot_wider(values_from = "Valor",
                      names_from = ssocial_r_corte_var)
        
        
      } else if(input$indicador_ssocial_r %in% lista_ind_2) {
        
        req(input$ssocial_r_corte, input$indicador_ssocial_r, input$fecha_ssocial_r)
        
        ssocial_r_corte_var <- rlang::sym(to_varname(input$ssocial_r_corte))
        
        dat_cut <- dat_ssocial_r() %>%
          filter(corte == input$ssocial_r_corte) %>%
          filter(!!ssocial_r_corte_var %in% input$checkbox_ssocial_r)
        
        if(input$ssocial_r_corte_2 == "Total"){
          
          dat_cut %>%
            filter(ano >= input$fecha_ssocial_r[1] &
                     ano <= input$fecha_ssocial_r[2]) %>%
            filter(corte_2 == "Total") %>% 
            select(Fecha, ssocial_r_corte_var, Valor) %>%
            arrange(desc(Fecha)) %>%
            pivot_wider(values_from = "Valor",
                        names_from = ssocial_r_corte_var)
          
        } else if(input$ssocial_r_corte_2 != "Total") {
          
          ssocial_r_corte_var_2 <- rlang::sym(to_varname(input$ssocial_r_corte_2))
          
          dat_cut %>%
            filter(ano >= input$fecha_ssocial_r[1] &
                     ano <= input$fecha_ssocial_r[2]) %>%
            filter(corte_2 == input$ssocial_r_corte_2) %>% 
            select(Fecha, ssocial_r_corte_var, ssocial_r_corte_var_2, Valor) %>%
            arrange(desc(Fecha)) %>%
            pivot_wider(values_from = "Valor",
                        names_from = ssocial_r_corte_var) 
          
        }
        
      } else if(input$ssocial_r_corte == "Total") {
        
        req(input$ssocial_r_corte, input$indicador_ssocial_r, input$fecha_ssocial_r)
        
        dat_ssocial_r() %>%
          filter(corte == "Total") %>% 
          filter(ano >= input$fecha_ssocial_r[1] &
                   ano <= input$fecha_ssocial_r[2]) %>%
          select(Fecha, Valor) %>%
          arrange(desc(Fecha))
        
      } else if(input$ssocial_r_corte != "Total") {
        
        req(input$ssocial_r_corte, input$indicador_ssocial_r, input$fecha_ssocial_r)
        
        ssocial_r_corte_var <- rlang::sym(to_varname(input$ssocial_r_corte))
        
        dat_cut <- dat_ssocial_r() %>%
          filter(corte == input$ssocial_r_corte) %>%
          janitor::remove_empty("cols") 
        
        dat_cut %>%     
          filter(ano >= input$fecha_ssocial_r[1] &
                   ano <= input$fecha_ssocial_r[2]) %>% 
          select(Fecha, ssocial_r_corte_var, Valor) %>%
          arrange(desc(Fecha)) %>% 
          pivot_wider(values_from = "Valor",
                      names_from = ssocial_r_corte_var)
        
      }
    })
    
    # Metadata 
    ssocial_r_meta <- reactive({
      
      dat_ssocial_r() %>%
        select(nomindicador, derecho, conindicador, tipoind, definicion, calculo, cita) %>% 
        mutate(`Mirador DESCA - UMAD/FCS – INDDHH` = " ") %>% 
        distinct() %>% 
        gather(key = "", value = " ")
      
    })
    
    # Excel
    list_ssocial_r <- reactive({
      list_ssocial_r <- list("Data" = ssocial_r_tab(),
                              "Metadata" = ssocial_r_meta())
    })
    
    # Render
    output$table_ssocial_r <- renderDT({
      
      DT::datatable(ssocial_r_tab(),
                    rownames = FALSE,
                    caption = htmltools::tags$caption(
                      input$indicador_ssocial_r,
                      style = "color:black; font-size:110%;")
      ) 
      
    })
    
    # 5.2.6. Descarga tablas   ================================================
    
    output$dwl_tab_ssocial_r <- downloadHandler(
      
      filename = function() {
        paste("pp-", input$indicador_ssocial_r, ".xlsx", sep = "")
      },
      content = function(file) {
        
        openxlsx::write.xlsx(list_ssocial_r(), file)
        
      }
    )
    
    
    
    
    
    
    
    
    ### 6.1. Vivienda Políticas   ===============================================
    
    # 6.1.1. Data reactiva   =================================================
    
    dat_vivienda_pp <- reactive({
      
      req(input$indicador_vivienda_pp)
      
      dat %>%
        filter(nomindicador == input$indicador_vivienda_pp) 
      
    })
    
    output$selector_vivienda_pp_corte <- renderUI({
      
      selectInput(
        inputId = "vivienda_pp_corte",
        label = "Seleccione corte:",
        choices = dat_vivienda_pp() %>% 
          select(corte) %>%
          arrange(corte) %>% 
          unique() %>% 
          pull(),
        selected = dat_vivienda_pp() %>% 
          filter(jerarquia == "1") %>%  
          distinct(corte) %>% 
          pull()
      )
      
    })
    
    output$selector_vivienda_pp_corte_2 <- renderUI({
      
      if(input$indicador_vivienda_pp %in% lista_ind_2){
        
        selectInput(
          inputId = "vivienda_pp_corte_2",
          label = "Seleccione primer corte:",
          choices = dat_vivienda_pp() %>% 
            select(corte_2) %>%
            arrange(corte_2) %>% 
            unique() %>% 
            pull(),
          selected = dat_vivienda_pp() %>% 
            filter(jerarquia == "1") %>%  
            distinct(corte_2) %>% 
            pull()
        )
        
      } else {
        
        NULL
      }
      
    })
    
    # Selector de fecha
    output$s_vivienda_pp_fecha <- renderUI({
      
      if(input$vivienda_pp_corte == "Departamento" & input$indicador_vivienda_pp %notin% lista_ind_2) {
        
        req(input$vivienda_pp_corte, input$indicador_vivienda_pp)
        
        req(nrow(dat_vivienda_pp()) > 0)
        
        selectInput(
          inputId = "fecha_dpto_vivienda_pp",
          label = "Seleccione año:",
          choices = dat_vivienda_pp() %>% 
            filter(nomindicador == input$indicador_vivienda_pp) %>%
            drop_na(Valor) %>%
            select(ano) %>%
            arrange(desc(ano)) %>% 
            unique() %>% 
            pull(),
          selected = 2019
        )
        
      } else if (input$indicador_vivienda_pp %in% lista_vunico){
        
        return(NULL)
        
      } else  {
        
        req(input$vivienda_pp_corte, input$indicador_vivienda_pp)
        req(nrow(dat_vivienda_pp()) > 0)
        
        tagList(
          # tags$style(type = 'text/css', 
          #            '#big_slider .irs-grid-text {font-size: 12px; transform: rotate(-90deg) translate(-10px);} ,.irs-grid-pol.large {height: 0px;}'),
          # div(id = 'big_slider',
          
          sliderInput("fecha_vivienda_pp", 
                      label = "Rango de tiempo", 
                      sep = "",
                      dragRange = T,
                      min = min(dat_vivienda_pp()$ano), 
                      max = max(dat_vivienda_pp()$ano), 
                      value = c(min(dat_vivienda_pp()$ano), 
                                max(dat_vivienda_pp()$ano))
          )
        )
        
      }
    })
    
    
    output$chbox_vivienda_pp <- renderUI({
      
      if(input$vivienda_pp_corte %notin% c("Total", "Departamento") & input$indicador_vivienda_pp %notin% lista_vunico) {
        
        vivienda_pp_corte_var <- rlang::sym(to_varname(input$vivienda_pp_corte))
        
        checkboxGroupInput(inputId = "checkbox_vivienda_pp",
                           label = "Seleccione categorías",
                           inline = TRUE,
                           choices =  dat_vivienda_pp() %>%
                             filter(corte == input$vivienda_pp_corte) %>% 
                             distinct(!!vivienda_pp_corte_var) %>%
                             pull(),
                           selected = dat_vivienda_pp() %>%
                             filter(corte == input$vivienda_pp_corte) %>% 
                             filter(jerarquia_cat == "1") %>%
                             distinct(!!vivienda_pp_corte_var) %>%
                             pull()
        )
        
      } else {
        
        return(NULL)
      }
      
    })
    
    # # Selector de corte según categoría y data temporal
    # dat_vivienda_pp <- reactive({
    #   
    #   req(input$vivienda_pp_corte)
    #   
    #   if(input$indicador_vivienda_pp %in% lista_ind_2){
    #     
    #     dat_salarios <- dat_vivienda_pp() %>%
    #       filter(corte_2 == input$vivienda_pp_corte_2) %>% 
    #       filter(corte == input$vivienda_pp_corte) %>%
    #       janitor::remove_empty("cols")
    #     
    #   } else {
    #     
    #     dat_vivienda_pp() %>%
    #       filter(corte == input$vivienda_pp_corte) %>%
    #       janitor::remove_empty("cols")
    #     
    #   }
    # })
    
    # 6.1.2. Metadata   ======================================================
    
    # Title
    output$title_vivienda_pp <- renderUI({ 
      helpText(HTML(unique(dat_vivienda_pp()$nomindicador)))
    })
    
    # Subtitle
    output$subtitle_vivienda_pp <- renderUI({ 
      helpText(HTML(unique(dat_vivienda_pp()$definicion)))
    })
    
    # Calculo
    output$calculo_vivienda_pp <- renderUI({ 
      helpText(HTML(paste("<b> Forma de cálculo:</b>", unique(dat_vivienda_pp()$calculo))))
    })
    
    
    # 6.1.3. Gráficos   ======================================================
    
    output$plot_vivienda_pp <- renderPlot({
      
      req(input$indicador_vivienda_pp)
      
      if(input$indicador_vivienda_pp %in% lista_vunico & input$indicador_vivienda_pp %in% lista_ind_2){
        
        req(input$vivienda_pp_corte, input$indicador_vivienda_pp)
        
        vivienda_pp_corte_var <- rlang::sym(to_varname(input$vivienda_pp_corte))
        vivienda_pp_corte_var_2 <- rlang::sym(to_varname(input$vivienda_pp_corte_2))
        
        dat_plot <- dat_vivienda_pp() %>%
          filter(ano >= input$fecha_vivienda_pp[1] &
                   ano <= input$fecha_vivienda_pp[2]) %>%
          filter(corte == input$vivienda_pp_corte) %>%
          filter(!!vivienda_pp_corte_var %in% input$checkbox_vivienda_pp) %>% 
          filter(corte_2 == input$vivienda_pp_corte_2)  
        
        plot_vivienda_corte <- ggplot(dat_plot,
                                     aes_string(x = "fecha_cat", y = "Valor",
                                                fill = vivienda_pp_corte_var)) +
          geom_col(position = "dodge", width = .7, alpha = .8) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x=element_blank(),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_vivienda_pp,
                                    "según",
                                    tolower(input$vivienda_pp_corte),
                                    "en",
                                    unique(dat_plot$fecha_cat))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_fill_brewer(name = "", palette = "Paired") +
          facet_wrap(as.formula(paste("~", vivienda_pp_corte_var_2)))
        
        print(plot_vivienda_corte)
        ggsave("www/indicador vivienda pp.png", width = 30, height = 20, units = "cm")
        
      } else if(input$indicador_vivienda_pp %in% lista_vunico & input$vivienda_pp_corte != "Departamento") {
        
        req(input$vivienda_pp_corte, input$indicador_vivienda_pp)
        
        dat_plot <- dat_vivienda_pp() %>%
          filter(corte == input$vivienda_pp_corte) %>%
          janitor::remove_empty("cols")
        
        if(input$vivienda_pp_corte == "Total"){
          
          plot_vivienda_corte <- ggplot(dat_plot,
                                       aes_string(x = "fecha_cat", y = "Valor")) +
            geom_col(position = "dodge", width = .4, alpha = .8, fill = color_defecto) +
            geom_text(aes(label = Valor), vjust = -0.4, fontface = "bold", size = 5) +
            theme_bdd(base_size = 12) +
            theme(axis.text.x=element_blank(),
                  legend.position = "bottom") +
            labs(x = "",  y = "",
                 title = wrapit(paste(input$indicador_vivienda_pp,
                                      "según",
                                      tolower(input$vivienda_pp_corte),
                                      "en",
                                      unique(dat_plot$fecha_cat))),
                 caption = wrapit(unique(dat_plot$cita))) 
          
          print(plot_vivienda_corte)
          ggsave("www/indicador vivienda pp.png", width = 30, height = 20, units = "cm")
          
          
        } else {
          
          vivienda_pp_corte_var <- rlang::sym(to_varname(input$vivienda_pp_corte))
          
          plot_vivienda_corte <- ggplot(dat_plot,
                                       aes_string(x = "fecha_cat", y = "Valor",
                                                  fill = vivienda_pp_corte_var)) +
            geom_col(position = "dodge", width = .7, alpha = .8) +
            theme_bdd(base_size = 12) +
            theme(axis.text.x=element_blank(),
                  legend.position = "bottom") +
            labs(x = "",  y = "",
                 title = wrapit(paste(input$indicador_vivienda_pp,
                                      "según",
                                      tolower(input$vivienda_pp_corte),
                                      "en",
                                      unique(dat_plot$fecha_cat))),
                 caption = wrapit(unique(dat_plot$cita))) +
            scale_fill_brewer(name = "", palette = "Paired") 
          
          print(plot_vivienda_corte)
          ggsave("www/indicador vivienda pp.png", width = 30, height = 20, units = "cm")
          
        }
        
        print(plot_vivienda_corte)
        ggsave("www/indicador vivienda pp.png", width = 30, height = 20, units = "cm")
        
      } else if(input$indicador_vivienda_pp %in% lista_ind_2) {
        
        req(input$vivienda_pp_corte, input$vivienda_pp_corte_2,
            input$fecha_vivienda_pp, input$checkbox_vivienda_pp)
        
        vivienda_pp_corte_var <- rlang::sym(to_varname(input$vivienda_pp_corte))
        
        dat_plot <- dat_vivienda_pp() %>%
          filter(ano >= input$fecha_vivienda_pp[1] &
                   ano <= input$fecha_vivienda_pp[2]) %>%
          filter(corte == input$vivienda_pp_corte) %>%
          filter(!!vivienda_pp_corte_var %in% input$checkbox_vivienda_pp)
        
        if(input$vivienda_pp_corte_2 == "Total"){
          
          dat_plot <- dat_plot %>% 
            filter(corte_2 == "Total")
          
          plot_vivienda_corte <- ggplot(dat_plot,
                                   aes_string(x = "fecha", y = "Valor", colour = vivienda_pp_corte_var)) +
            geom_line(size = 1, alpha = 0.5) +
            geom_point(size = 3) +
            theme_bdd(base_size = 12) +
            theme(axis.text.x = element_text(angle = 0),
                  legend.position = "bottom") +
            labs(x = "",  y = "",
                 title = wrapit(paste(input$indicador_vivienda_pp,
                                      "según",
                                      tolower(input$vivienda_pp_corte))),
                 caption = wrapit(unique(dat_plot$cita))) +
            scale_colour_manual(name = "", values = paleta_expandida) 
          
        } else if(input$vivienda_pp_corte_2 != "Total") {
          
          vivienda_pp_corte_var_2 <- rlang::sym(to_varname(input$vivienda_pp_corte_2))
          
          dat_plot <- dat_plot %>%
            filter(corte_2 == input$vivienda_pp_corte_2)  
          
          plot_vivienda_corte <- ggplot(dat_plot,
                                   aes_string(x = "fecha", y = "Valor", colour = vivienda_pp_corte_var)) +
            geom_line(size = 1, alpha = 0.5) +
            geom_point(size = 3) +
            theme_bdd(base_size = 12) +
            theme(axis.text.x = element_text(angle = 0),
                  legend.position = "bottom") +
            labs(x = "",  y = "",
                 title = wrapit(paste(input$indicador_vivienda_pp,
                                      "según",
                                      tolower(input$vivienda_pp_corte))),
                 caption = wrapit(unique(dat_plot$cita))) +
            scale_colour_manual(name = "", values = paleta_expandida) + 
            facet_wrap(as.formula(paste("~", vivienda_pp_corte_var_2)))
          
        }
        
        print(plot_vivienda_corte)
        ggsave("www/indicador vivienda pp.png", width = 30, height = 20, units = "cm")
        
        
      } else if(input$vivienda_pp_corte == "Total") {
        
        req(input$indicador_vivienda_pp, input$fecha_vivienda_pp)
        
        dat_plot <- dat_vivienda_pp() %>% 
          filter(ano >= input$fecha_vivienda_pp[1] &
                   ano <= input$fecha_vivienda_pp[2]) %>% 
          filter(corte == "Total")
        
        plot_vivienda <- ggplot(dat_plot,
                           aes(x = fecha, y = Valor)) +
          geom_line(size = 1, alpha = 0.5, colour = color_defecto) +
          geom_point(size = 3, colour = color_defecto) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(input$indicador_vivienda_pp),
               caption = wrapit(unique(dat_plot$cita))) 
        
        print(plot_vivienda)
        ggsave("www/indicador vivienda pp.png", width = 30, height = 20, units = "cm")
        
        
      } else if(input$vivienda_pp_corte == "Departamento" & 
                input$indicador_vivienda_pp %notin% lista_ind_2 ) {
        
        req(input$indicador_vivienda_pp, input$fecha_dpto_vivienda_pp)
        
        dat_plot <- dat_vivienda_pp() %>%
          filter(corte == "Departamento") %>%
          filter(ano == input$fecha_dpto_vivienda_pp) %>% 
          select(departamento, Valor, fuente, cita)
        
        dep_j <- dep %>%
          left_join(dat_plot, by = c("nombre" = "departamento"))
        
        plot_vivienda_dpto <- geouy::plot_geouy(dep_j,
                                           "Valor",
                                           viri_opt = "plasma",
                                           l = "n") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_vivienda_pp, 
                                    "en",
                                    input$fecha_dpto_vivienda_pp), w = 80),
               caption = wrapit(unique(dat_plot$cita), w = 80)) +
          theme_bdd(base_size = 14)
        
        print(plot_vivienda_dpto)
        ggsave("www/indicador vivienda pp.png", width = 30, height = 20, units = "cm")
        
        
      } else if(input$vivienda_pp_corte != "Total") {
        
        req(input$vivienda_pp_corte, input$indicador_vivienda_pp, 
            input$fecha_vivienda_pp, input$checkbox_vivienda_pp)
        
        dat_plot <- dat_vivienda_pp() %>%
          filter(ano >= input$fecha_vivienda_pp[1] &
                   ano <= input$fecha_vivienda_pp[2]) %>%
          filter(corte == input$vivienda_pp_corte) %>%
          janitor::remove_empty("cols")
        
        vivienda_pp_corte_var <- rlang::sym(to_varname(input$vivienda_pp_corte))
        
        dat_plot <- filter(dat_plot,
                           !!vivienda_pp_corte_var %in% input$checkbox_vivienda_pp)
        
        plot_vivienda_corte <- ggplot(dat_plot,
                                 aes_string(x = "fecha", y = "Valor", colour = vivienda_pp_corte_var)) +
          geom_line(size = 1, alpha = 0.5) +
          geom_point(size = 3) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_vivienda_pp,
                                    "según",
                                    tolower(input$vivienda_pp_corte))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_colour_manual(name = "", values = paleta_expandida) 
        
        print(plot_vivienda_corte)
        ggsave("www/indicador vivienda pp.png", width = 30, height = 20, units = "cm")
        
      }
      
    })
    
    # 6.1.4. Descarga gráficos   =============================================
    
    output$baja_p_vivienda_pp <- downloadHandler(
      filename <- function() {
        paste("indicador vivienda pp", "png", sep = ".")
      },
      
      content <- function(file) {
        file.copy("www/indicador vivienda pp.png", file)
      },
      contentType = "www/indicador vivienda pp"
    )
    
    
    # 6.1.5. Tablas   ========================================================
    
    # Data
    vivienda_pp_tab <- reactive({
      
      if(input$indicador_vivienda_pp %in% lista_vunico & input$vivienda_pp_corte == "Total"){
        
        req(input$vivienda_pp_corte, input$indicador_vivienda_pp)
        
        dat_vivienda_pp() %>%
          filter(corte == "Total") %>% 
          select(Fecha, Valor) %>%
          arrange(desc(Fecha))
        
      } else if(input$indicador_vivienda_pp %in% lista_vunico & input$vivienda_pp_corte != "Total") {
        
        req(input$vivienda_pp_corte, input$indicador_vivienda_pp)
        
        vivienda_pp_corte_var <- rlang::sym(to_varname(input$vivienda_pp_corte))
        
        dat_cut <- dat_vivienda_pp() %>%
          filter(corte == input$vivienda_pp_corte) %>%
          janitor::remove_empty("cols") 
        
        dat_cut %>%     
          select(Fecha, vivienda_pp_corte_var, Valor) %>%
          arrange(desc(Fecha)) %>% 
          pivot_wider(values_from = "Valor",
                      names_from = vivienda_pp_corte_var)
        
        
      } else if(input$indicador_vivienda_pp %in% lista_ind_2) {
        
        req(input$vivienda_pp_corte, input$indicador_vivienda_pp, input$fecha_vivienda_pp)
        
        vivienda_pp_corte_var <- rlang::sym(to_varname(input$vivienda_pp_corte))
        
        dat_cut <- dat_vivienda_pp() %>%
          filter(corte == input$vivienda_pp_corte) %>%
          filter(!!vivienda_pp_corte_var %in% input$checkbox_vivienda_pp)
        
        if(input$vivienda_pp_corte_2 == "Total"){
          
          dat_cut %>%
            filter(ano >= input$fecha_vivienda_pp[1] &
                     ano <= input$fecha_vivienda_pp[2]) %>%
            filter(corte_2 == "Total") %>% 
            select(Fecha, vivienda_pp_corte_var, Valor) %>%
            arrange(desc(Fecha)) %>%
            pivot_wider(values_from = "Valor",
                        names_from = vivienda_pp_corte_var)
          
        } else if(input$vivienda_pp_corte_2 != "Total") {
          
          vivienda_pp_corte_var_2 <- rlang::sym(to_varname(input$vivienda_pp_corte_2))
          
          dat_cut %>%
            filter(ano >= input$fecha_vivienda_pp[1] &
                     ano <= input$fecha_vivienda_pp[2]) %>%
            filter(corte_2 == input$vivienda_pp_corte_2) %>% 
            select(Fecha, vivienda_pp_corte_var, vivienda_pp_corte_var_2, Valor) %>%
            arrange(desc(Fecha)) %>%
            pivot_wider(values_from = "Valor",
                        names_from = vivienda_pp_corte_var) 
          
        }
        
      } else if(input$vivienda_pp_corte == "Total") {
        
        req(input$vivienda_pp_corte, input$indicador_vivienda_pp, input$fecha_vivienda_pp)
        
        dat_vivienda_pp() %>%
          filter(corte == "Total") %>% 
          filter(ano >= input$fecha_vivienda_pp[1] &
                   ano <= input$fecha_vivienda_pp[2]) %>%
          select(Fecha, Valor) %>%
          arrange(desc(Fecha))
        
      } else if(input$vivienda_pp_corte != "Total") {
        
        req(input$vivienda_pp_corte, input$indicador_vivienda_pp, input$fecha_vivienda_pp)
        
        vivienda_pp_corte_var <- rlang::sym(to_varname(input$vivienda_pp_corte))
        
        dat_cut <- dat_vivienda_pp() %>%
          filter(corte == input$vivienda_pp_corte) %>%
          janitor::remove_empty("cols") 
        
        dat_cut %>%     
          filter(ano >= input$fecha_vivienda_pp[1] &
                   ano <= input$fecha_vivienda_pp[2]) %>% 
          select(Fecha, vivienda_pp_corte_var, Valor) %>%
          arrange(desc(Fecha)) %>% 
          pivot_wider(values_from = "Valor",
                      names_from = vivienda_pp_corte_var)
        
      }
    })
    
    # Metadata 
    vivienda_pp_meta <- reactive({
      
      dat_vivienda_pp() %>%
        select(nomindicador, derecho, conindicador, tipoind, definicion, calculo, cita) %>% 
        mutate(`Mirador DESCA - UMAD/FCS – INDDHH` = " ") %>% 
        distinct() %>% 
        gather(key = "", value = " ")
      
    })
    
    # Excel
    list_vivienda_pp <- reactive({
      list_vivienda_pp <- list("Data" = vivienda_pp_tab(),
                              "Metadata" = vivienda_pp_meta())
    })
    
    # Render
    output$table_vivienda_pp <- renderDT({
      
      DT::datatable(vivienda_pp_tab(),
                    rownames = FALSE,
                    caption = htmltools::tags$caption(
                      input$indicador_vivienda_pp,
                      style = "color:black; font-size:110%;")
      ) 
      
    })
    
    # 6.1.6. Descarga tablas   ================================================
    
    output$dwl_tab_vivienda_pp <- downloadHandler(
      
      filename = function() {
        paste("pp-", input$indicador_vivienda_pp, ".xlsx", sep = "")
      },
      content = function(file) {
        
        openxlsx::write.xlsx(list_vivienda_pp(), file)
        
      }
    )
    
    
    
    ### 6.2. Vivienda Resultados   =============================================
    
    # 6.2.1. Data reactiva   =================================================
    
    dat_vivienda_r <- reactive({
      
      req(input$indicador_vivienda_r)
      
      dat %>%
        filter(nomindicador == input$indicador_vivienda_r) 
      
    })
    
    output$selector_vivienda_r_corte <- renderUI({
      
      selectInput(
        inputId = "vivienda_r_corte",
        label = "Seleccione corte:",
        choices = dat_vivienda_r() %>% 
          select(corte) %>%
          arrange(corte) %>% 
          unique() %>% 
          pull(),
        selected = dat_vivienda_r() %>% 
          filter(jerarquia == "1") %>%  
          distinct(corte) %>% 
          pull()
      )
      
    })
    
    output$selector_vivienda_r_corte_2 <- renderUI({
      
      if(input$indicador_vivienda_r %in% lista_ind_2){
        
        selectInput(
          inputId = "vivienda_r_corte_2",
          label = "Seleccione primer corte:",
          choices = dat_vivienda_r() %>% 
            select(corte_2) %>%
            arrange(corte_2) %>% 
            unique() %>% 
            pull(),
          selected = dat_vivienda_r() %>% 
            filter(jerarquia == "1") %>%  
            distinct(corte_2) %>% 
            pull()
        )
        
      } else {
        
        NULL
      }
      
    })
    
    # Selector de fecha
    output$s_vivienda_r_fecha <- renderUI({
      
      if(input$vivienda_r_corte == "Departamento" & input$indicador_vivienda_r %notin% lista_ind_2) {
        
        req(input$vivienda_r_corte, input$indicador_vivienda_r)
        
        req(nrow(dat_vivienda_r()) > 0)
        
        selectInput(
          inputId = "fecha_dpto_vivienda_r",
          label = "Seleccione año:",
          choices = dat_vivienda_r() %>% 
            filter(nomindicador == input$indicador_vivienda_r) %>%
            drop_na(Valor) %>%
            select(ano) %>%
            arrange(desc(ano)) %>% 
            unique() %>% 
            pull(),
          selected = 2019
        )
        
      } else if (input$indicador_vivienda_r %in% lista_vunico){
        
        return(NULL)
        
      } else  {
        
        req(input$vivienda_r_corte, input$indicador_vivienda_r)
        req(nrow(dat_vivienda_r()) > 0)
        
        tagList(
          # tags$style(type = 'text/css', 
          #            '#big_slider .irs-grid-text {font-size: 12px; transform: rotate(-90deg) translate(-10px);} ,.irs-grid-pol.large {height: 0px;}'),
          # div(id = 'big_slider',
          
          sliderInput("fecha_vivienda_r", 
                      label = "Rango de tiempo", 
                      sep = "",
                      dragRange = T,
                      min = min(dat_vivienda_r()$ano), 
                      max = max(dat_vivienda_r()$ano), 
                      value = c(min(dat_vivienda_r()$ano), 
                                max(dat_vivienda_r()$ano))
          )
        )
        
      }
    })
    
    
    output$chbox_vivienda_r <- renderUI({
      
      if(input$vivienda_r_corte %notin% c("Total", "Departamento") & input$indicador_vivienda_r %notin% lista_vunico) {
        
        vivienda_r_corte_var <- rlang::sym(to_varname(input$vivienda_r_corte))
        
        checkboxGroupInput(inputId = "checkbox_vivienda_r",
                           label = "Seleccione categorías",
                           inline = TRUE,
                           choices =  dat_vivienda_r() %>%
                             filter(corte == input$vivienda_r_corte) %>% 
                             distinct(!!vivienda_r_corte_var) %>%
                             pull(),
                           selected = dat_vivienda_r() %>%
                             filter(corte == input$vivienda_r_corte) %>% 
                             filter(jerarquia_cat == "1") %>%
                             distinct(!!vivienda_r_corte_var) %>%
                             pull()
        )
        
      } else {
        
        return(NULL)
      }
      
    })
    
    # # Selector de corte según categoría y data temporal
    # dat_vivienda_r <- reactive({
    #   
    #   req(input$vivienda_r_corte)
    #   
    #   if(input$indicador_vivienda_r %in% lista_ind_2){
    #     
    #     dat_salarios <- dat_vivienda_r() %>%
    #       filter(corte_2 == input$vivienda_r_corte_2) %>% 
    #       filter(corte == input$vivienda_r_corte) %>%
    #       janitor::remove_empty("cols")
    #     
    #   } else {
    #     
    #     dat_vivienda_r() %>%
    #       filter(corte == input$vivienda_r_corte) %>%
    #       janitor::remove_empty("cols")
    #     
    #   }
    # })
    
    # 6.2.2. Metadata   ======================================================
    
    # Title
    output$title_vivienda_r <- renderUI({ 
      helpText(HTML(unique(dat_vivienda_r()$nomindicador)))
    })
    
    # Subtitle
    output$subtitle_vivienda_r <- renderUI({ 
      helpText(HTML(unique(dat_vivienda_r()$definicion)))
    })
    
    # Calculo
    output$calculo_vivienda_r <- renderUI({ 
      helpText(HTML(paste("<b> Forma de cálculo:</b>", unique(dat_vivienda_r()$calculo))))
    })
    
    
    # 6.2.3. Gráficos   ======================================================
    
    output$plot_vivienda_r <- renderPlot({
      
      req(input$indicador_vivienda_r)
      
      if(input$indicador_vivienda_r %in% lista_vunico & input$indicador_vivienda_r %in% lista_ind_2){
        
        req(input$vivienda_r_corte, input$indicador_vivienda_r)
        
        vivienda_r_corte_var <- rlang::sym(to_varname(input$vivienda_r_corte))
        vivienda_r_corte_var_2 <- rlang::sym(to_varname(input$vivienda_r_corte_2))
        
        dat_plot <- dat_vivienda_r() %>%
          filter(ano >= input$fecha_vivienda_r[1] &
                   ano <= input$fecha_vivienda_r[2]) %>%
          filter(corte == input$vivienda_r_corte) %>%
          filter(!!vivienda_r_corte_var %in% input$checkbox_vivienda_r) %>% 
          filter(corte_2 == input$vivienda_r_corte_2)  
        
        plot_vivienda_corte <- ggplot(dat_plot,
                                      aes_string(x = "fecha_cat", y = "Valor",
                                                 fill = vivienda_r_corte_var)) +
          geom_col(position = "dodge", width = .7, alpha = .8) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x=element_blank(),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_vivienda_r,
                                    "según",
                                    tolower(input$vivienda_r_corte),
                                    "en",
                                    unique(dat_plot$fecha_cat))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_fill_brewer(name = "", palette = "Paired") +
          facet_wrap(as.formula(paste("~", vivienda_r_corte_var_2)))
        
        print(plot_vivienda_corte)
        ggsave("www/indicador vivienda r.png", width = 30, height = 20, units = "cm")
        
      } else if(input$indicador_vivienda_r %in% lista_vunico & input$vivienda_r_corte != "Departamento") {
        
        req(input$vivienda_r_corte, input$indicador_vivienda_r)
        
        dat_plot <- dat_vivienda_r() %>%
          filter(corte == input$vivienda_r_corte) %>%
          janitor::remove_empty("cols")
        
        if(input$vivienda_r_corte == "Total"){
          
          plot_vivienda_corte <- ggplot(dat_plot,
                                        aes_string(x = "fecha_cat", y = "Valor")) +
            geom_col(position = "dodge", width = .4, alpha = .8, fill = color_defecto) +
            geom_text(aes(label = Valor), vjust = -0.4, fontface = "bold", size = 5) +
            theme_bdd(base_size = 12) +
            theme(axis.text.x=element_blank(),
                  legend.position = "bottom") +
            labs(x = "",  y = "",
                 title = wrapit(paste(input$indicador_vivienda_r,
                                      "según",
                                      tolower(input$vivienda_r_corte),
                                      "en",
                                      unique(dat_plot$fecha_cat))),
                 caption = wrapit(unique(dat_plot$cita))) 
          
          print(plot_vivienda_corte)
          ggsave("www/indicador vivienda r.png", width = 30, height = 20, units = "cm")
          
          
        } else {
          
          vivienda_r_corte_var <- rlang::sym(to_varname(input$vivienda_r_corte))
          
          plot_vivienda_corte <- ggplot(dat_plot,
                                        aes_string(x = "fecha_cat", y = "Valor",
                                                   fill = vivienda_r_corte_var)) +
            geom_col(position = "dodge", width = .7, alpha = .8) +
            theme_bdd(base_size = 12) +
            theme(axis.text.x=element_blank(),
                  legend.position = "bottom") +
            labs(x = "",  y = "",
                 title = wrapit(paste(input$indicador_vivienda_r,
                                      "según",
                                      tolower(input$vivienda_r_corte),
                                      "en",
                                      unique(dat_plot$fecha_cat))),
                 caption = wrapit(unique(dat_plot$cita))) +
            scale_fill_brewer(name = "", palette = "Paired") 
          
          print(plot_vivienda_corte)
          ggsave("www/indicador vivienda r.png", width = 30, height = 20, units = "cm")
          
        }
        
        print(plot_vivienda_corte)
        ggsave("www/indicador vivienda r.png", width = 30, height = 20, units = "cm")
        
      } else if(input$indicador_vivienda_r %in% lista_ind_2) {
        
        req(input$vivienda_r_corte, input$vivienda_r_corte_2,
            input$fecha_vivienda_r, input$checkbox_vivienda_r)
        
        vivienda_r_corte_var <- rlang::sym(to_varname(input$vivienda_r_corte))
        
        dat_plot <- dat_vivienda_r() %>%
          filter(ano >= input$fecha_vivienda_r[1] &
                   ano <= input$fecha_vivienda_r[2]) %>%
          filter(corte == input$vivienda_r_corte) %>%
          filter(!!vivienda_r_corte_var %in% input$checkbox_vivienda_r)
        
        if(input$vivienda_r_corte_2 == "Total"){
          
          dat_plot <- dat_plot %>% 
            filter(corte_2 == "Total")
          
          plot_vivienda_corte <- ggplot(dat_plot,
                                   aes_string(x = "fecha", y = "Valor", colour = vivienda_r_corte_var)) +
            geom_line(size = 1, alpha = 0.5) +
            geom_point(size = 3) +
            theme_bdd(base_size = 12) +
            theme(axis.text.x = element_text(angle = 0),
                  legend.position = "bottom") +
            labs(x = "",  y = "",
                 title = wrapit(paste(input$indicador_vivienda_r,
                                      "según",
                                      tolower(input$vivienda_r_corte))),
                 caption = wrapit(unique(dat_plot$cita))) +
            scale_colour_manual(name = "", values = paleta_expandida) 
          
        } else if(input$vivienda_r_corte_2 != "Total") {
          
          vivienda_r_corte_var_2 <- rlang::sym(to_varname(input$vivienda_r_corte_2))
          
          dat_plot <- dat_plot %>%
            filter(corte_2 == input$vivienda_r_corte_2)  
          
          plot_vivienda_corte <- ggplot(dat_plot,
                                   aes_string(x = "fecha", y = "Valor", colour = vivienda_r_corte_var)) +
            geom_line(size = 1, alpha = 0.5) +
            geom_point(size = 3) +
            theme_bdd(base_size = 12) +
            theme(axis.text.x = element_text(angle = 0),
                  legend.position = "bottom") +
            labs(x = "",  y = "",
                 title = wrapit(paste(input$indicador_vivienda_r,
                                      "según",
                                      tolower(input$vivienda_r_corte))),
                 caption = wrapit(unique(dat_plot$cita))) +
            scale_colour_manual(name = "", values = paleta_expandida) + 
            facet_wrap(as.formula(paste("~", vivienda_r_corte_var_2)))
          
        }
        
        print(plot_vivienda_corte)
        ggsave("www/indicador vivienda r.png", width = 30, height = 20, units = "cm")
        
        
      } else if(input$vivienda_r_corte == "Total") {
        
        req(input$indicador_vivienda_r, input$fecha_vivienda_r)
        
        dat_plot <- dat_vivienda_r() %>% 
          filter(ano >= input$fecha_vivienda_r[1] &
                   ano <= input$fecha_vivienda_r[2]) %>% 
          filter(corte == "Total")
        
        plot_vivienda <- ggplot(dat_plot,
                           aes(x = fecha, y = Valor)) +
          geom_line(size = 1, alpha = 0.5, colour = color_defecto) +
          geom_point(size = 3, colour = color_defecto) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(input$indicador_vivienda_r),
               caption = wrapit(unique(dat_plot$cita))) 
        
        print(plot_vivienda)
        ggsave("www/indicador vivienda r.png", width = 30, height = 20, units = "cm")
        
        
      } else if(input$vivienda_r_corte == "Departamento" & 
                input$indicador_vivienda_r %notin% lista_ind_2 ) {
        
        req(input$indicador_vivienda_r, input$fecha_dpto_vivienda_r)
        
        dat_plot <- dat_vivienda_r() %>%
          filter(corte == "Departamento") %>%
          filter(ano == input$fecha_dpto_vivienda_r) %>% 
          select(departamento, Valor, fuente, cita)
        
        dep_j <- dep %>%
          left_join(dat_plot, by = c("nombre" = "departamento"))
        
        plot_vivienda_dpto <- geouy::plot_geouy(dep_j,
                                           "Valor",
                                           viri_opt = "plasma",
                                           l = "n") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_vivienda_r, 
                                    "en",
                                    input$fecha_dpto_vivienda_r), w = 80),
               caption = wrapit(unique(dat_plot$cita), w = 80)) +
          theme_bdd(base_size = 14)
        
        print(plot_vivienda_dpto)
        ggsave("www/indicador vivienda r.png", width = 30, height = 20, units = "cm")
        
        
      } else if(input$vivienda_r_corte != "Total") {
        
        req(input$vivienda_r_corte, input$indicador_vivienda_r, 
            input$fecha_vivienda_r, input$checkbox_vivienda_r)
        
        dat_plot <- dat_vivienda_r() %>%
          filter(ano >= input$fecha_vivienda_r[1] &
                   ano <= input$fecha_vivienda_r[2]) %>%
          filter(corte == input$vivienda_r_corte) %>%
          janitor::remove_empty("cols")
        
        vivienda_r_corte_var <- rlang::sym(to_varname(input$vivienda_r_corte))
        
        dat_plot <- filter(dat_plot,
                           !!vivienda_r_corte_var %in% input$checkbox_vivienda_r)
        
        plot_vivienda_corte <- ggplot(dat_plot,
                                 aes_string(x = "fecha", y = "Valor", colour = vivienda_r_corte_var)) +
          geom_line(size = 1, alpha = 0.5) +
          geom_point(size = 3) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_vivienda_r,
                                    "según",
                                    tolower(input$vivienda_r_corte))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_colour_manual(name = "", values = paleta_expandida) 
        
        print(plot_vivienda_corte)
        ggsave("www/indicador vivienda r.png", width = 30, height = 20, units = "cm")
        
      }
      
    })
    
    # 6.2.4. Descarga gráficos   =============================================
    
    output$baja_p_vivienda_r <- downloadHandler(
      filename <- function() {
        paste("indicador vivienda r", "png", sep = ".")
      },
      
      content <- function(file) {
        file.copy("www/indicador vivienda r.png", file)
      },
      contentType = "www/indicador vivienda r"
    )
    
    
    # 6.2.5. Tablas   ========================================================
    
    # Data
    vivienda_r_tab <- reactive({
      
      if(input$indicador_vivienda_r %in% lista_vunico & input$vivienda_r_corte == "Total"){
        
        req(input$vivienda_r_corte, input$indicador_vivienda_r)
        
        dat_vivienda_r() %>%
          filter(corte == "Total") %>% 
          select(Fecha, Valor) %>%
          arrange(desc(Fecha))
        
      } else if(input$indicador_vivienda_r %in% lista_vunico & input$vivienda_r_corte != "Total") {
        
        req(input$vivienda_r_corte, input$indicador_vivienda_r)
        
        vivienda_r_corte_var <- rlang::sym(to_varname(input$vivienda_r_corte))
        
        dat_cut <- dat_vivienda_r() %>%
          filter(corte == input$vivienda_r_corte) %>%
          janitor::remove_empty("cols") 
        
        dat_cut %>%     
          select(Fecha, vivienda_r_corte_var, Valor) %>%
          arrange(desc(Fecha)) %>% 
          pivot_wider(values_from = "Valor",
                      names_from = vivienda_r_corte_var)
        
        
      } else  if(input$indicador_vivienda_r %in% lista_ind_2) {
        
        req(input$vivienda_r_corte, input$indicador_vivienda_r, input$fecha_vivienda_r)
        
        vivienda_r_corte_var <- rlang::sym(to_varname(input$vivienda_r_corte))
        
        dat_cut <- dat_vivienda_r() %>%
          filter(corte == input$vivienda_r_corte) %>%
          filter(!!vivienda_r_corte_var %in% input$checkbox_vivienda_r)
        
        if(input$vivienda_r_corte_2 == "Total"){
          
          dat_cut %>%
            filter(ano >= input$fecha_vivienda_r[1] &
                     ano <= input$fecha_vivienda_r[2]) %>%
            filter(corte_2 == "Total") %>% 
            select(Fecha, vivienda_r_corte_var, Valor) %>%
            arrange(desc(Fecha)) %>%
            pivot_wider(values_from = "Valor",
                        names_from = vivienda_r_corte_var)
          
        } else if(input$vivienda_r_corte_2 != "Total") {
          
          vivienda_r_corte_var_2 <- rlang::sym(to_varname(input$vivienda_r_corte_2))
          
          dat_cut %>%
            filter(ano >= input$fecha_vivienda_r[1] &
                     ano <= input$fecha_vivienda_r[2]) %>%
            filter(corte_2 == input$vivienda_r_corte_2) %>% 
            select(Fecha, vivienda_r_corte_var, vivienda_r_corte_var_2, Valor) %>%
            arrange(desc(Fecha)) %>%
            pivot_wider(values_from = "Valor",
                        names_from = vivienda_r_corte_var) 
          
        }
        
      } else if(input$vivienda_r_corte == "Total") {
        
        req(input$vivienda_r_corte, input$indicador_vivienda_r, input$fecha_vivienda_r)
        
        dat_vivienda_r() %>%
          filter(corte == "Total") %>% 
          filter(ano >= input$fecha_vivienda_r[1] &
                   ano <= input$fecha_vivienda_r[2]) %>%
          select(Fecha, Valor) %>%
          arrange(desc(Fecha))
        
      } else if(input$vivienda_r_corte != "Total") {
        
        req(input$vivienda_r_corte, input$indicador_vivienda_r, input$fecha_vivienda_r)
        
        vivienda_r_corte_var <- rlang::sym(to_varname(input$vivienda_r_corte))
        
        dat_cut <- dat_vivienda_r() %>%
          filter(corte == input$vivienda_r_corte) %>%
          janitor::remove_empty("cols") 
        
        dat_cut %>%     
          filter(ano >= input$fecha_vivienda_r[1] &
                   ano <= input$fecha_vivienda_r[2]) %>% 
          select(Fecha, vivienda_r_corte_var, Valor) %>%
          arrange(desc(Fecha)) %>% 
          pivot_wider(values_from = "Valor",
                      names_from = vivienda_r_corte_var)
        
      }
    })
    
    # Metadata 
    vivienda_r_meta <- reactive({
      
      dat_vivienda_r() %>%
        select(nomindicador, derecho, conindicador, tipoind, definicion, calculo, cita) %>% 
        mutate(`Mirador DESCA - UMAD/FCS – INDDHH` = " ") %>% 
        distinct() %>% 
        gather(key = "", value = " ")
      
    })
    
    # Excel
    list_vivienda_r <- reactive({
      list_vivienda_r <- list("Data" = vivienda_r_tab(),
                               "Metadata" = vivienda_r_meta())
    })
    
    # Render
    output$table_vivienda_r <- renderDT({
      
      DT::datatable(vivienda_r_tab(),
                    rownames = FALSE,
                    caption = htmltools::tags$caption(
                      input$indicador_vivienda_r,
                      style = "color:black; font-size:110%;")
      ) 
      
    })
    
    # 6.2.6. Descarga tablas   ================================================
    
    output$dwl_tab_vivienda_r <- downloadHandler(
      
      filename = function() {
        paste("resultados-", input$indicador_vivienda_r, ".xlsx", sep = "")
      },
      content = function(file) {
        
        openxlsx::write.xlsx(list_vivienda_r(), file)
        
      }
    )

    ### 7.1. Trabajo Políticas   ===============================================
    
    # 7.1.1. Data reactiva   =================================================
    
    dat_trabajo_pp <- reactive({
      
      req(input$indicador_trabajo_pp)
      
      dat %>%
        filter(nomindicador == input$indicador_trabajo_pp) 
      
    })
    
    output$selector_trabajo_pp_corte <- renderUI({
      
      selectInput(
        inputId = "trabajo_pp_corte",
        label = "Seleccione corte:",
        choices = dat_trabajo_pp() %>% 
          select(corte) %>%
          arrange(corte) %>% 
          unique() %>% 
          pull(),
        selected = dat_trabajo_pp() %>% 
          filter(jerarquia == "1") %>%  
          distinct(corte) %>% 
          pull()
      )
      
    })
    
    output$selector_trabajo_pp_corte_2 <- renderUI({
      
      if(input$indicador_trabajo_pp %in% lista_ind_2){
        
        selectInput(
          inputId = "trabajo_pp_corte_2",
          label = "Seleccione primer corte:",
          choices = dat_trabajo_pp() %>% 
            select(corte_2) %>%
            arrange(corte_2) %>% 
            unique() %>% 
            pull(),
          selected = dat_trabajo_pp() %>% 
            filter(jerarquia == "1") %>%  
            distinct(corte_2) %>% 
            pull()
        )
        
      } else {
        
        NULL
      }
      
    })
    
    # Selector de fecha
    output$s_trabajo_pp_fecha <- renderUI({
      
      if(input$trabajo_pp_corte == "Departamento" & input$indicador_trabajo_pp %notin% lista_ind_2) {
        
        req(input$trabajo_pp_corte, input$indicador_trabajo_pp)
        
        req(nrow(dat_trabajo_pp()) > 0)
        
        selectInput(
          inputId = "fecha_dpto_trabajo_pp",
          label = "Seleccione año:",
          choices = dat_trabajo_pp() %>% 
            filter(nomindicador == input$indicador_trabajo_pp) %>%
            drop_na(Valor) %>%
            select(ano) %>%
            arrange(desc(ano)) %>% 
            unique() %>% 
            pull(),
          selected = 2019
        )
        
      } else if (input$indicador_trabajo_pp %in% lista_vunico){
        
        return(NULL)
        
      } else  {
        
        req(input$trabajo_pp_corte, input$indicador_trabajo_pp)
        req(nrow(dat_trabajo_pp()) > 0)
        
        tagList(
          # tags$style(type = 'text/css', 
          #            '#big_slider .irs-grid-text {font-size: 12px; transform: rotate(-90deg) translate(-10px);} ,.irs-grid-pol.large {height: 0px;}'),
          # div(id = 'big_slider',
          
          sliderInput("fecha_trabajo_pp", 
                      label = "Rango de tiempo", 
                      sep = "",
                      dragRange = T,
                      min = min(dat_trabajo_pp()$ano), 
                      max = max(dat_trabajo_pp()$ano), 
                      value = c(min(dat_trabajo_pp()$ano), 
                                max(dat_trabajo_pp()$ano))
          )
        )
        
      }
    })
    
    
    output$chbox_trabajo_pp <- renderUI({
      
      if(input$trabajo_pp_corte %notin% c("Total", "Departamento") & input$indicador_trabajo_pp %notin% lista_vunico) {
        
        trabajo_pp_corte_var <- rlang::sym(to_varname(input$trabajo_pp_corte))
        
        checkboxGroupInput(inputId = "checkbox_trabajo_pp",
                           label = "Seleccione categorías",
                           inline = TRUE,
                           choices =  dat_trabajo_pp() %>%
                             filter(corte == input$trabajo_pp_corte) %>% 
                             distinct(!!trabajo_pp_corte_var) %>%
                             pull(),
                           selected = dat_trabajo_pp() %>%
                             filter(corte == input$trabajo_pp_corte) %>% 
                             filter(jerarquia_cat == "1") %>%
                             distinct(!!trabajo_pp_corte_var) %>%
                             pull()
        )
        
      } else {
        
        return(NULL)
      }
      
    })
    
    # # Selector de corte según categoría y data temporal
    # dat_trabajo_pp <- reactive({
    #   
    #   req(input$trabajo_pp_corte)
    #   
    #   if(input$indicador_trabajo_pp %in% lista_ind_2){
    #     
    #     dat_salarios <- dat_trabajo_pp() %>%
    #       filter(corte_2 == input$trabajo_pp_corte_2) %>% 
    #       filter(corte == input$trabajo_pp_corte) %>%
    #       janitor::remove_empty("cols")
    #     
    #   } else {
    #     
    #     dat_trabajo_pp() %>%
    #       filter(corte == input$trabajo_pp_corte) %>%
    #       janitor::remove_empty("cols")
    #     
    #   }
    # })
    
    # 7.1.2. Metadata   ======================================================
    
    # Title
    output$title_trabajo_pp <- renderUI({ 
      helpText(HTML(unique(dat_trabajo_pp()$nomindicador)))
    })
    
    # Subtitle
    output$subtitle_trabajo_pp <- renderUI({ 
      helpText(HTML(unique(dat_trabajo_pp()$definicion)))
    })
    
    # Calculo
    output$calculo_trabajo_pp <- renderUI({ 
      helpText(HTML(paste("<b> Forma de cálculo:</b>", unique(dat_trabajo_pp()$calculo))))
    })
    
    
    # 7.1.3. Gráficos   ======================================================
    
    output$plot_trabajo_pp <- renderPlot({
      
      req(input$indicador_trabajo_pp)
      
      if(input$indicador_trabajo_pp %in% lista_vunico & input$indicador_trabajo_pp %in% lista_ind_2){
        
        req(input$trabajo_pp_corte, input$indicador_trabajo_pp)
        
        trabajo_pp_corte_var <- rlang::sym(to_varname(input$trabajo_pp_corte))
        trabajo_pp_corte_var_2 <- rlang::sym(to_varname(input$trabajo_pp_corte_2))
        
        dat_plot <- dat_trabajo_pp() %>%
          filter(ano >= input$fecha_trabajo_pp[1] &
                   ano <= input$fecha_trabajo_pp[2]) %>%
          filter(corte == input$trabajo_pp_corte) %>%
          filter(!!trabajo_pp_corte_var %in% input$checkbox_trabajo_pp) %>% 
          filter(corte_2 == input$trabajo_pp_corte_2)  
        
        plot_trabajo_corte <- ggplot(dat_plot,
                                      aes_string(x = "fecha_cat", y = "Valor",
                                                 fill = trabajo_pp_corte_var)) +
          geom_col(position = "dodge", width = .7, alpha = .8) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x=element_blank(),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_trabajo_pp,
                                    "según",
                                    tolower(input$trabajo_pp_corte),
                                    "en",
                                    unique(dat_plot$fecha_cat))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_fill_brewer(name = "", palette = "Paired") +
          facet_wrap(as.formula(paste("~", trabajo_pp_corte_var_2)))
        
        print(plot_trabajo_corte)
        ggsave("www/indicador trabajo pp.png", width = 30, height = 20, units = "cm")
        
      } else if(input$indicador_trabajo_pp %in% lista_vunico & input$trabajo_pp_corte != "Departamento") {
        
        req(input$trabajo_pp_corte, input$indicador_trabajo_pp)
        
        dat_plot <- dat_trabajo_pp() %>%
          filter(corte == input$trabajo_pp_corte) %>%
          janitor::remove_empty("cols")
        
        if(input$trabajo_pp_corte == "Total"){
          
          plot_trabajo_corte <- ggplot(dat_plot,
                                        aes_string(x = "fecha_cat", y = "Valor")) +
            geom_col(position = "dodge", width = .4, alpha = .8, fill = color_defecto) +
            geom_text(aes(label = Valor), vjust = -0.4, fontface = "bold", size = 5) +
            theme_bdd(base_size = 12) +
            theme(axis.text.x=element_blank(),
                  legend.position = "bottom") +
            labs(x = "",  y = "",
                 title = wrapit(paste(input$indicador_trabajo_pp,
                                      "según",
                                      tolower(input$trabajo_pp_corte),
                                      "en",
                                      unique(dat_plot$fecha_cat))),
                 caption = wrapit(unique(dat_plot$cita))) 
          
          print(plot_trabajo_corte)
          ggsave("www/indicador trabajo pp.png", width = 30, height = 20, units = "cm")
          
          
        } else {
          
          trabajo_pp_corte_var <- rlang::sym(to_varname(input$trabajo_pp_corte))
          
          plot_trabajo_corte <- ggplot(dat_plot,
                                        aes_string(x = "fecha_cat", y = "Valor",
                                                   fill = trabajo_pp_corte_var)) +
            geom_col(position = "dodge", width = .7, alpha = .8) +
            theme_bdd(base_size = 12) +
            theme(axis.text.x=element_blank(),
                  legend.position = "bottom") +
            labs(x = "",  y = "",
                 title = wrapit(paste(input$indicador_trabajo_pp,
                                      "según",
                                      tolower(input$trabajo_pp_corte),
                                      "en",
                                      unique(dat_plot$fecha_cat))),
                 caption = wrapit(unique(dat_plot$cita))) +
            scale_fill_brewer(name = "", palette = "Paired") 
          
          print(plot_trabajo_corte)
          ggsave("www/indicador trabajo pp.png", width = 30, height = 20, units = "cm")
          
        }
        
        print(plot_trabajo_corte)
        ggsave("www/indicador trabajo pp.png", width = 30, height = 20, units = "cm")
        
      } else if(input$indicador_trabajo_pp %in% lista_ind_2) {
        
        req(input$trabajo_pp_corte, input$trabajo_pp_corte_2,
            input$fecha_trabajo_pp, input$checkbox_trabajo_pp)
        
        trabajo_pp_corte_var <- rlang::sym(to_varname(input$trabajo_pp_corte))
        
        dat_plot <- dat_trabajo_pp() %>%
          filter(ano >= input$fecha_trabajo_pp[1] &
                   ano <= input$fecha_trabajo_pp[2]) %>%
          filter(corte == input$trabajo_pp_corte) %>%
          filter(!!trabajo_pp_corte_var %in% input$checkbox_trabajo_pp)
        
        if(input$trabajo_pp_corte_2 == "Total"){
          
          dat_plot <- dat_plot %>% 
            filter(corte_2 == "Total")
          
          plot_trabajo_corte <- ggplot(dat_plot,
                                   aes_string(x = "fecha", y = "Valor", colour = trabajo_pp_corte_var)) +
            geom_line(size = 1, alpha = 0.5) +
            geom_point(size = 3) +
            theme_bdd(base_size = 12) +
            theme(axis.text.x = element_text(angle = 0),
                  legend.position = "bottom") +
            labs(x = "",  y = "",
                 title = wrapit(paste(input$indicador_trabajo_pp,
                                      "según",
                                      tolower(input$trabajo_pp_corte))),
                 caption = wrapit(unique(dat_plot$cita))) +
            scale_colour_manual(name = "", values = paleta_expandida) 
          
        } else if(input$trabajo_pp_corte_2 != "Total") {
          
          trabajo_pp_corte_var_2 <- rlang::sym(to_varname(input$trabajo_pp_corte_2))
          
          dat_plot <- dat_plot %>%
            filter(corte_2 == input$trabajo_pp_corte_2)  
          
          plot_trabajo_corte <- ggplot(dat_plot,
                                   aes_string(x = "fecha", y = "Valor", colour = trabajo_pp_corte_var)) +
            geom_line(size = 1, alpha = 0.5) +
            geom_point(size = 3) +
            theme_bdd(base_size = 12) +
            theme(axis.text.x = element_text(angle = 0),
                  legend.position = "bottom") +
            labs(x = "",  y = "",
                 title = wrapit(paste(input$indicador_trabajo_pp,
                                      "según",
                                      tolower(input$trabajo_pp_corte))),
                 caption = wrapit(unique(dat_plot$cita))) +
            scale_colour_manual(name = "", values = paleta_expandida) + 
            facet_wrap(as.formula(paste("~", trabajo_pp_corte_var_2)))
          
        }
        
        print(plot_trabajo_corte)
        ggsave("www/indicador trabajo pp.png", width = 30, height = 20, units = "cm")
        
        
      } else if(input$trabajo_pp_corte == "Total") {
        
        req(input$indicador_trabajo_pp, input$fecha_trabajo_pp)
        
        dat_plot <- dat_trabajo_pp() %>% 
          filter(ano >= input$fecha_trabajo_pp[1] &
                   ano <= input$fecha_trabajo_pp[2]) %>% 
          filter(corte == "Total")
        
        plot_trabajo <- ggplot(dat_plot,
                           aes(x = fecha, y = Valor)) +
          geom_line(size = 1, alpha = 0.5, colour = color_defecto) +
          geom_point(size = 3, colour = color_defecto) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(input$indicador_trabajo_pp),
               caption = wrapit(unique(dat_plot$cita))) 
        
        print(plot_trabajo)
        ggsave("www/indicador trabajo pp.png", width = 30, height = 20, units = "cm")
        
        
      } else if(input$trabajo_pp_corte == "Departamento" & 
                input$indicador_trabajo_pp %notin% lista_ind_2 ) {
        
        req(input$indicador_trabajo_pp, input$fecha_dpto_trabajo_pp)
        
        dat_plot <- dat_trabajo_pp() %>%
          filter(corte == "Departamento") %>%
          filter(ano == input$fecha_dpto_trabajo_pp) %>% 
          select(departamento, Valor, fuente, cita)
        
        dep_j <- dep %>%
          left_join(dat_plot, by = c("nombre" = "departamento"))
        
        plot_trabajo_dpto <- geouy::plot_geouy(dep_j,
                                           "Valor",
                                           viri_opt = "plasma",
                                           l = "n") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_trabajo_pp, 
                                    "en",
                                    input$fecha_dpto_trabajo_pp), w = 80),
               caption = wrapit(unique(dat_plot$cita), w = 80)) +
          theme_bdd(base_size = 14)
        
        print(plot_trabajo_dpto)
        ggsave("www/indicador trabajo pp.png", width = 30, height = 20, units = "cm")
        
        
      } else if(input$trabajo_pp_corte != "Total") {
        
        req(input$trabajo_pp_corte, input$indicador_trabajo_pp, 
            input$fecha_trabajo_pp, input$checkbox_trabajo_pp)
        
        dat_plot <- dat_trabajo_pp() %>%
          filter(ano >= input$fecha_trabajo_pp[1] &
                   ano <= input$fecha_trabajo_pp[2]) %>%
          filter(corte == input$trabajo_pp_corte) %>%
          janitor::remove_empty("cols")
        
        trabajo_pp_corte_var <- rlang::sym(to_varname(input$trabajo_pp_corte))
        
        dat_plot <- filter(dat_plot,
                           !!trabajo_pp_corte_var %in% input$checkbox_trabajo_pp)
        
        plot_trabajo_corte <- ggplot(dat_plot,
                                 aes_string(x = "fecha", y = "Valor", colour = trabajo_pp_corte_var)) +
          geom_line(size = 1, alpha = 0.5) +
          geom_point(size = 3) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_trabajo_pp,
                                    "según",
                                    tolower(input$trabajo_pp_corte))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_colour_manual(name = "", values = paleta_expandida) 
        
        print(plot_trabajo_corte)
        ggsave("www/indicador trabajo pp.png", width = 30, height = 20, units = "cm")
        
      }
      
    })
    
    # 7.1.4. Descarga gráficos   =============================================
    
    output$baja_p_trabajo_pp <- downloadHandler(
      filename <- function() {
        paste("indicador trabajo pp", "png", sep = ".")
      },
      
      content <- function(file) {
        file.copy("www/indicador trabajo pp.png", file)
      },
      contentType = "www/indicador trabajo pp"
    )
    
    
    # 7.1.5. Tablas   ========================================================
    
    # Data
    trabajo_pp_tab <- reactive({
      
      if(input$indicador_trabajo_pp %in% lista_vunico & input$trabajo_pp_corte == "Total"){
        
        req(input$trabajo_pp_corte, input$indicador_trabajo_pp)
        
        dat_trabajo_pp() %>%
          filter(corte == "Total") %>% 
          select(Fecha, Valor) %>%
          arrange(desc(Fecha))
        
      } else if(input$indicador_trabajo_pp %in% lista_vunico & input$trabajo_pp_corte != "Total") {
        
        req(input$trabajo_pp_corte, input$indicador_trabajo_pp)
        
        trabajo_pp_corte_var <- rlang::sym(to_varname(input$trabajo_pp_corte))
        
        dat_cut <- dat_trabajo_pp() %>%
          filter(corte == input$trabajo_pp_corte) %>%
          janitor::remove_empty("cols") 
        
        dat_cut %>%     
          select(Fecha, trabajo_pp_corte_var, Valor) %>%
          arrange(desc(Fecha)) %>% 
          pivot_wider(values_from = "Valor",
                      names_from = trabajo_pp_corte_var)
        
        
      } else if(input$indicador_trabajo_pp %in% lista_ind_2) {
        
        req(input$trabajo_pp_corte, input$indicador_trabajo_pp, input$fecha_trabajo_pp)
        
        trabajo_pp_corte_var <- rlang::sym(to_varname(input$trabajo_pp_corte))
        
        dat_cut <- dat_trabajo_pp() %>%
          filter(corte == input$trabajo_pp_corte) %>%
          filter(!!trabajo_pp_corte_var %in% input$checkbox_trabajo_pp)
        
        if(input$trabajo_pp_corte_2 == "Total"){
          
          dat_cut %>%
            filter(ano >= input$fecha_trabajo_pp[1] &
                     ano <= input$fecha_trabajo_pp[2]) %>%
            filter(corte_2 == "Total") %>% 
            select(Fecha, trabajo_pp_corte_var, Valor) %>%
            arrange(desc(Fecha)) %>%
            pivot_wider(values_from = "Valor",
                        names_from = trabajo_pp_corte_var)
          
        } else if(input$trabajo_pp_corte_2 != "Total") {
          
          trabajo_pp_corte_var_2 <- rlang::sym(to_varname(input$trabajo_pp_corte_2))
          
          dat_cut %>%
            filter(ano >= input$fecha_trabajo_pp[1] &
                     ano <= input$fecha_trabajo_pp[2]) %>%
            filter(corte_2 == input$trabajo_pp_corte_2) %>% 
            select(Fecha, trabajo_pp_corte_var, trabajo_pp_corte_var_2, Valor) %>%
            arrange(desc(Fecha)) %>%
            pivot_wider(values_from = "Valor",
                        names_from = trabajo_pp_corte_var) 
          
        }
        
      } else if(input$trabajo_pp_corte == "Total") {
        
        req(input$trabajo_pp_corte, input$indicador_trabajo_pp, input$fecha_trabajo_pp)
        
        dat_trabajo_pp() %>%
          filter(corte == "Total") %>% 
          filter(ano >= input$fecha_trabajo_pp[1] &
                   ano <= input$fecha_trabajo_pp[2]) %>%
          select(Fecha, Valor) %>%
          arrange(desc(Fecha))
        
      } else if(input$trabajo_pp_corte != "Total") {
        
        req(input$trabajo_pp_corte, input$indicador_trabajo_pp, input$fecha_trabajo_pp)
        
        trabajo_pp_corte_var <- rlang::sym(to_varname(input$trabajo_pp_corte))
        
        dat_cut <- dat_trabajo_pp() %>%
          filter(corte == input$trabajo_pp_corte) %>%
          janitor::remove_empty("cols") 
        
        dat_cut %>%     
          filter(ano >= input$fecha_trabajo_pp[1] &
                   ano <= input$fecha_trabajo_pp[2]) %>% 
          select(Fecha, trabajo_pp_corte_var, Valor) %>%
          arrange(desc(Fecha)) %>% 
          pivot_wider(values_from = "Valor",
                      names_from = trabajo_pp_corte_var)
        
      }
    })
    
    # Metadata 
    trabajo_pp_meta <- reactive({
      
      dat_trabajo_pp() %>%
        select(nomindicador, derecho, conindicador, tipoind, definicion, calculo, cita) %>% 
        mutate(`Mirador DESCA - UMAD/FCS – INDDHH` = " ") %>% 
        distinct() %>% 
        gather(key = "", value = " ")
      
    })
    
    # Excel
    list_trabajo_pp <- reactive({
      list_trabajo_pp <- list("Data" = trabajo_pp_tab(),
                               "Metadata" = trabajo_pp_meta())
    })
    
    # Render
    output$table_trabajo_pp <- renderDT({
      
      DT::datatable(trabajo_pp_tab(),
                    rownames = FALSE,
                    caption = htmltools::tags$caption(
                      input$indicador_trabajo_pp,
                      style = "color:black; font-size:110%;")
      ) 
      
    })
    
    # 7.1.6. Descarga tablas   ================================================
    
    output$dwl_tab_trabajo_pp <- downloadHandler(
      
      filename = function() {
        paste("pp-", input$indicador_trabajo_pp, ".xlsx", sep = "")
      },
      content = function(file) {
        
        openxlsx::write.xlsx(list_trabajo_pp(), file)
        
      }
    )
    
    
    
    ### 7.2. Trabajo Resultados   =============================================
    
    # 7.2.1. Data reactiva   =================================================
    
    dat_trabajo_r <- reactive({
      
      req(input$indicador_trabajo_r)
      
      dat %>%
        filter(nomindicador == input$indicador_trabajo_r) 
      
    })
    
    output$selector_trabajo_r_corte <- renderUI({
      
      selectInput(
        inputId = "trabajo_r_corte",
        label = "Seleccione corte:",
        choices = dat_trabajo_r() %>% 
          select(corte) %>%
          arrange(corte) %>% 
          unique() %>% 
          pull(),
        selected = dat_trabajo_r() %>% 
          filter(jerarquia == "1") %>%  
          distinct(corte) %>% 
          pull()
      )
      
    })
    
    output$selector_trabajo_r_corte_2 <- renderUI({
      
      if(input$indicador_trabajo_r %in% lista_ind_2){
        
        selectInput(
          inputId = "trabajo_r_corte_2",
          label = "Seleccione primer corte:",
          choices = dat_trabajo_r() %>% 
            select(corte_2) %>%
            arrange(corte_2) %>% 
            unique() %>% 
            pull(),
          selected = dat_trabajo_r() %>% 
            filter(jerarquia == "1") %>%  
            distinct(corte_2) %>% 
            pull()
        )
        
      } else {
        
        NULL
      }
      
    })
    
    # Selector de fecha
    output$s_trabajo_r_fecha <- renderUI({
      
      if(input$trabajo_r_corte == "Departamento" & input$indicador_trabajo_r %notin% lista_ind_2) {
        
        req(input$trabajo_r_corte, input$indicador_trabajo_r)
        
        req(nrow(dat_trabajo_r()) > 0)
        
        selectInput(
          inputId = "fecha_dpto_trabajo_r",
          label = "Seleccione año:",
          choices = dat_trabajo_r() %>% 
            filter(nomindicador == input$indicador_trabajo_r) %>%
            drop_na(Valor) %>%
            select(ano) %>%
            arrange(desc(ano)) %>% 
            unique() %>% 
            pull(),
          selected = 2019
        )
        
      } else if (input$indicador_trabajo_r %in% lista_vunico){
        
        return(NULL)
        
      } else  {
        
        req(input$trabajo_r_corte, input$indicador_trabajo_r)
        req(nrow(dat_trabajo_r()) > 0)
        
        tagList(
          # tags$style(type = 'text/css', 
          #            '#big_slider .irs-grid-text {font-size: 12px; transform: rotate(-90deg) translate(-10px);} ,.irs-grid-pol.large {height: 0px;}'),
          # div(id = 'big_slider',
          
          sliderInput("fecha_trabajo_r", 
                      label = "Rango de tiempo", 
                      sep = "",
                      dragRange = T,
                      min = min(dat_trabajo_r()$ano), 
                      max = max(dat_trabajo_r()$ano), 
                      value = c(min(dat_trabajo_r()$ano), 
                                max(dat_trabajo_r()$ano))
          )
        )
        
      }
    })
    
    
    output$chbox_trabajo_r <- renderUI({
      
      if(input$trabajo_r_corte %notin% c("Total", "Departamento") & input$indicador_trabajo_r %notin% lista_vunico) {
        
        trabajo_r_corte_var <- rlang::sym(to_varname(input$trabajo_r_corte))
        
        checkboxGroupInput(inputId = "checkbox_trabajo_r",
                           label = "Seleccione categorías",
                           inline = TRUE,
                           choices =  dat_trabajo_r() %>%
                             filter(corte == input$trabajo_r_corte) %>% 
                             distinct(!!trabajo_r_corte_var) %>%
                             pull(),
                           selected = dat_trabajo_r() %>%
                             filter(corte == input$trabajo_r_corte) %>% 
                             filter(jerarquia_cat == "1") %>%
                             distinct(!!trabajo_r_corte_var) %>%
                             pull()
        )
        
      } else {
        
        return(NULL)
      }
      
    })
    
    # # Selector de corte según categoría y data temporal
    # dat_trabajo_r <- reactive({
    #   
    #   req(input$trabajo_r_corte)
    #   
    #   if(input$indicador_trabajo_r %in% lista_ind_2){
    #     
    #     dat_salarios <- dat_trabajo_r() %>%
    #       filter(corte_2 == input$trabajo_r_corte_2) %>% 
    #       filter(corte == input$trabajo_r_corte) %>%
    #       janitor::remove_empty("cols")
    #     
    #   } else {
    #     
    #     dat_trabajo_r() %>%
    #       filter(corte == input$trabajo_r_corte) %>%
    #       janitor::remove_empty("cols")
    #     
    #   }
    # })
    
    # 7.2.2. Metadata   ======================================================
    
    # Title
    output$title_trabajo_r <- renderUI({ 
      helpText(HTML(unique(dat_trabajo_r()$nomindicador)))
    })
    
    # Subtitle
    output$subtitle_trabajo_r <- renderUI({ 
      helpText(HTML(unique(dat_trabajo_r()$definicion)))
    })
    
    # Calculo
    output$calculo_trabajo_r <- renderUI({ 
      helpText(HTML(paste("<b> Forma de cálculo:</b>", unique(dat_trabajo_r()$calculo))))
    })
    
    
    # 7.2.3. Gráficos   ======================================================
    
    output$plot_trabajo_r <- renderPlot({
      
      req(input$indicador_trabajo_r)
      
      if(input$indicador_trabajo_r %in% lista_vunico & input$indicador_trabajo_r %in% lista_ind_2){
        
        req(input$trabajo_r_corte, input$indicador_trabajo_r)
        
        trabajo_r_corte_var <- rlang::sym(to_varname(input$trabajo_r_corte))
        trabajo_r_corte_var_2 <- rlang::sym(to_varname(input$trabajo_r_corte_2))
        
        dat_plot <- dat_trabajo_r() %>%
          filter(ano >= input$fecha_trabajo_r[1] &
                   ano <= input$fecha_trabajo_r[2]) %>%
          filter(corte == input$trabajo_r_corte) %>%
          filter(!!trabajo_r_corte_var %in% input$checkbox_trabajo_r) %>% 
          filter(corte_2 == input$trabajo_r_corte_2)  
        
        plot_trabajo_corte <- ggplot(dat_plot,
                                     aes_string(x = "fecha_cat", y = "Valor",
                                                fill = trabajo_r_corte_var)) +
          geom_col(position = "dodge", width = .7, alpha = .8) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x=element_blank(),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_trabajo_r,
                                    "según",
                                    tolower(input$trabajo_r_corte),
                                    "en",
                                    unique(dat_plot$fecha_cat))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_fill_brewer(name = "", palette = "Paired") +
          facet_wrap(as.formula(paste("~", trabajo_r_corte_var_2)))
        
        print(plot_trabajo_corte)
        ggsave("www/indicador trabajo r.png", width = 30, height = 20, units = "cm")
        
      } else if(input$indicador_trabajo_r %in% lista_vunico & input$trabajo_r_corte != "Departamento") {
        
        req(input$trabajo_r_corte, input$indicador_trabajo_r)
        
        dat_plot <- dat_trabajo_r() %>%
          filter(corte == input$trabajo_r_corte) %>%
          janitor::remove_empty("cols")
        
        if(input$trabajo_r_corte == "Total"){
          
          plot_trabajo_corte <- ggplot(dat_plot,
                                       aes_string(x = "fecha_cat", y = "Valor")) +
            geom_col(position = "dodge", width = .4, alpha = .8, fill = color_defecto) +
            geom_text(aes(label = Valor), vjust = -0.4, fontface = "bold", size = 5) +
            theme_bdd(base_size = 12) +
            theme(axis.text.x=element_blank(),
                  legend.position = "bottom") +
            labs(x = "",  y = "",
                 title = wrapit(paste(input$indicador_trabajo_r,
                                      "según",
                                      tolower(input$trabajo_r_corte),
                                      "en",
                                      unique(dat_plot$fecha_cat))),
                 caption = wrapit(unique(dat_plot$cita))) 
          
          print(plot_trabajo_corte)
          ggsave("www/indicador trabajo r.png", width = 30, height = 20, units = "cm")
          
          
        } else {
          
          trabajo_r_corte_var <- rlang::sym(to_varname(input$trabajo_r_corte))
          
          plot_trabajo_corte <- ggplot(dat_plot,
                                       aes_string(x = "fecha_cat", y = "Valor",
                                                  fill = trabajo_r_corte_var)) +
            geom_col(position = "dodge", width = .7, alpha = .8) +
            theme_bdd(base_size = 12) +
            theme(axis.text.x=element_blank(),
                  legend.position = "bottom") +
            labs(x = "",  y = "",
                 title = wrapit(paste(input$indicador_trabajo_r,
                                      "según",
                                      tolower(input$trabajo_r_corte),
                                      "en",
                                      unique(dat_plot$fecha_cat))),
                 caption = wrapit(unique(dat_plot$cita))) +
            scale_fill_brewer(name = "", palette = "Paired") 
          
          print(plot_trabajo_corte)
          ggsave("www/indicador trabajo r.png", width = 30, height = 20, units = "cm")
          
        }
        
        print(plot_trabajo_corte)
        ggsave("www/indicador trabajo r.png", width = 30, height = 20, units = "cm")
        
      } else if(input$indicador_trabajo_r %in% lista_ind_2) {
        
        req(input$trabajo_r_corte, input$trabajo_r_corte_2,
            input$fecha_trabajo_r, input$checkbox_trabajo_r)
        
        trabajo_r_corte_var <- rlang::sym(to_varname(input$trabajo_r_corte))
        
        dat_plot <- dat_trabajo_r() %>%
          filter(ano >= input$fecha_trabajo_r[1] &
                   ano <= input$fecha_trabajo_r[2]) %>%
          filter(corte == input$trabajo_r_corte) %>%
          filter(!!trabajo_r_corte_var %in% input$checkbox_trabajo_r)
        
        if(input$trabajo_r_corte_2 == "Total"){
          
          dat_plot <- dat_plot %>% 
            filter(corte_2 == "Total")
          
          plot_trabajo_corte <- ggplot(dat_plot,
                                   aes_string(x = "fecha", y = "Valor", colour = trabajo_r_corte_var)) +
            geom_line(size = 1, alpha = 0.5) +
            geom_point(size = 3) +
            theme_bdd(base_size = 12) +
            theme(axis.text.x = element_text(angle = 0),
                  legend.position = "bottom") +
            labs(x = "",  y = "",
                 title = wrapit(paste(input$indicador_trabajo_r,
                                      "según",
                                      tolower(input$trabajo_r_corte))),
                 caption = wrapit(unique(dat_plot$cita))) +
            scale_colour_manual(name = "", values = paleta_expandida) 
          
        } else if(input$trabajo_r_corte_2 != "Total") {
          
          trabajo_r_corte_var_2 <- rlang::sym(to_varname(input$trabajo_r_corte_2))
          
          dat_plot <- dat_plot %>%
            filter(corte_2 == input$trabajo_r_corte_2)  
          
          plot_trabajo_corte <- ggplot(dat_plot,
                                   aes_string(x = "fecha", y = "Valor", colour = trabajo_r_corte_var)) +
            geom_line(size = 1, alpha = 0.5) +
            geom_point(size = 3) +
            theme_bdd(base_size = 12) +
            theme(axis.text.x = element_text(angle = 0),
                  legend.position = "bottom") +
            labs(x = "",  y = "",
                 title = wrapit(paste(input$indicador_trabajo_r,
                                      "según",
                                      tolower(input$trabajo_r_corte))),
                 caption = wrapit(unique(dat_plot$cita))) +
            scale_colour_manual(name = "", values = paleta_expandida) +
            facet_wrap(as.formula(paste("~", trabajo_r_corte_var_2)))
          
        }
        
        print(plot_trabajo_corte)
        ggsave("www/indicador trabajo r.png", width = 30, height = 20, units = "cm")
        
        
      } else if(input$trabajo_r_corte == "Total") {
        
        req(input$indicador_trabajo_r, input$fecha_trabajo_r)
        
        dat_plot <- dat_trabajo_r() %>% 
          filter(ano >= input$fecha_trabajo_r[1] &
                   ano <= input$fecha_trabajo_r[2]) %>% 
          filter(corte == "Total")
        
        plot_trabajo <- ggplot(dat_plot,
                           aes(x = fecha, y = Valor)) +
          geom_line(size = 1, alpha = 0.5, colour = color_defecto) +
          geom_point(size = 3, colour = color_defecto) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(input$indicador_trabajo_r),
               caption = wrapit(unique(dat_plot$cita))) 
        
        print(plot_trabajo)
        ggsave("www/indicador trabajo r.png", width = 30, height = 20, units = "cm")
        
        
      } else if(input$trabajo_r_corte == "Departamento" & 
                input$indicador_trabajo_r %notin% lista_ind_2 ) {
        
        req(input$indicador_trabajo_r, input$fecha_dpto_trabajo_r)
        
        dat_plot <- dat_trabajo_r() %>%
          filter(corte == "Departamento") %>%
          filter(ano == input$fecha_dpto_trabajo_r) %>% 
          select(departamento, Valor, fuente, cita)
        
        dep_j <- dep %>%
          left_join(dat_plot, by = c("nombre" = "departamento"))
        
        plot_trabajo_dpto <- geouy::plot_geouy(dep_j,
                                           "Valor",
                                           viri_opt = "plasma",
                                           l = "n") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_trabajo_r, 
                                    "en",
                                    input$fecha_dpto_trabajo_r), w = 80),
               caption = wrapit(unique(dat_plot$cita), w = 80)) +
          theme_bdd(base_size = 14)
        
        print(plot_trabajo_dpto)
        ggsave("www/indicador trabajo r.png", width = 30, height = 20, units = "cm")
        
        
      } else if(input$trabajo_r_corte != "Total") {
        
        req(input$trabajo_r_corte, input$indicador_trabajo_r, 
            input$fecha_trabajo_r, input$checkbox_trabajo_r)
        
        dat_plot <- dat_trabajo_r() %>%
          filter(ano >= input$fecha_trabajo_r[1] &
                   ano <= input$fecha_trabajo_r[2]) %>%
          filter(corte == input$trabajo_r_corte) %>%
          janitor::remove_empty("cols")
        
        trabajo_r_corte_var <- rlang::sym(to_varname(input$trabajo_r_corte))
        
        dat_plot <- filter(dat_plot,
                           !!trabajo_r_corte_var %in% input$checkbox_trabajo_r)
        
        plot_trabajo_corte <- ggplot(dat_plot,
                                 aes_string(x = "fecha", y = "Valor", colour = trabajo_r_corte_var)) +
          geom_line(size = 1, alpha = 0.5) +
          geom_point(size = 3) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_trabajo_r,
                                    "según",
                                    tolower(input$trabajo_r_corte))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_colour_manual(name = "", values = paleta_expandida) 
        
        print(plot_trabajo_corte)
        ggsave("www/indicador trabajo r.png", width = 30, height = 20, units = "cm")
        
      }
      
    })
    
    # 7.2.4. Descarga gráficos   =============================================
    
    output$baja_p_trabajo_r <- downloadHandler(
      filename <- function() {
        paste("indicador trabajo r", "png", sep = ".")
      },
      
      content <- function(file) {
        file.copy("www/indicador trabajo r.png", file)
      },
      contentType = "www/indicador trabajo r"
    )
    
    
    # 7.2.5. Tablas   ========================================================
    
    # Data
    trabajo_r_tab <- reactive({
      
        if(input$indicador_trabajo_r %in% lista_vunico & input$trabajo_r_corte == "Total"){
          
          req(input$trabajo_r_corte, input$indicador_trabajo_r)
          
          dat_trabajo_r() %>%
            filter(corte == "Total") %>% 
            select(Fecha, Valor) %>%
            arrange(desc(Fecha))
          
        } else if(input$indicador_trabajo_r %in% lista_vunico & input$trabajo_r_corte != "Total") {
          
          req(input$trabajo_r_corte, input$indicador_trabajo_r)
          
          trabajo_r_corte_var <- rlang::sym(to_varname(input$trabajo_r_corte))
          
          dat_cut <- dat_trabajo_r() %>%
            filter(corte == input$trabajo_r_corte) %>%
            janitor::remove_empty("cols") 
          
          dat_cut %>%     
            select(Fecha, trabajo_r_corte_var, Valor) %>%
            arrange(desc(Fecha)) %>% 
            pivot_wider(values_from = "Valor",
                        names_from = trabajo_r_corte_var)
          
          
        } else if(input$indicador_trabajo_r %in% lista_ind_2) {
        
        req(input$trabajo_r_corte, input$indicador_trabajo_r, input$fecha_trabajo_r)
        
        trabajo_r_corte_var <- rlang::sym(to_varname(input$trabajo_r_corte))
        
        dat_cut <- dat_trabajo_r() %>%
          filter(corte == input$trabajo_r_corte) %>%
          filter(!!trabajo_r_corte_var %in% input$checkbox_trabajo_r)
        
        if(input$trabajo_r_corte_2 == "Total"){
          
          dat_cut %>%
            filter(ano >= input$fecha_trabajo_r[1] &
                     ano <= input$fecha_trabajo_r[2]) %>%
            filter(corte_2 == "Total") %>% 
            select(Fecha, trabajo_r_corte_var, Valor) %>%
            arrange(desc(Fecha)) %>%
            pivot_wider(values_from = "Valor",
                        names_from = trabajo_r_corte_var)
          
        } else if(input$trabajo_r_corte_2 != "Total") {
          
          trabajo_r_corte_var_2 <- rlang::sym(to_varname(input$trabajo_r_corte_2))
          
          dat_cut %>%
            filter(ano >= input$fecha_trabajo_r[1] &
                     ano <= input$fecha_trabajo_r[2]) %>%
            filter(corte_2 == input$trabajo_r_corte_2) %>% 
            select(Fecha, trabajo_r_corte_var, trabajo_r_corte_var_2, Valor) %>%
            arrange(desc(Fecha)) %>%
            pivot_wider(values_from = "Valor",
                        names_from = trabajo_r_corte_var) 
          
        }
        
      } else if(input$trabajo_r_corte == "Total") {
        
        req(input$trabajo_r_corte, input$indicador_trabajo_r, input$fecha_trabajo_r)
        
        dat_trabajo_r() %>%
          filter(corte == "Total") %>% 
          filter(ano >= input$fecha_trabajo_r[1] &
                   ano <= input$fecha_trabajo_r[2]) %>%
          select(Fecha, Valor) %>%
          arrange(desc(Fecha))
        
      } else if(input$trabajo_r_corte != "Total") {
        
        req(input$trabajo_r_corte, input$indicador_trabajo_r, input$fecha_trabajo_r)
        
        trabajo_r_corte_var <- rlang::sym(to_varname(input$trabajo_r_corte))
        
        dat_cut <- dat_trabajo_r() %>%
          filter(corte == input$trabajo_r_corte) %>%
          janitor::remove_empty("cols") 
        
        dat_cut %>%     
          filter(ano >= input$fecha_trabajo_r[1] &
                   ano <= input$fecha_trabajo_r[2]) %>% 
          select(Fecha, trabajo_r_corte_var, Valor) %>%
          arrange(desc(Fecha)) %>% 
          pivot_wider(values_from = "Valor",
                      names_from = trabajo_r_corte_var)
        
      }
    })
    
    # Metadata 
    trabajo_r_meta <- reactive({
      
      dat_trabajo_r() %>%
        select(nomindicador, derecho, conindicador, tipoind, definicion, calculo, cita) %>% 
        mutate(`Mirador DESCA - UMAD/FCS – INDDHH` = " ") %>% 
        distinct() %>% 
        gather(key = "", value = " ")
      
    })
    
    # Excel
    list_trabajo_r <- reactive({
      list_trabajo_r <- list("Data" = trabajo_r_tab(),
                              "Metadata" = trabajo_r_meta())
    })
    
    # Render
    output$table_trabajo_r <- renderDT({
      
      DT::datatable(trabajo_r_tab(),
                    rownames = FALSE,
                    caption = htmltools::tags$caption(
                      input$indicador_trabajo_r,
                      style = "color:black; font-size:110%;")
      ) 
      
    })
    
    # 7.2.6. Descarga tablas   ================================================
    
    output$dwl_tab_trabajo_r <- downloadHandler(
      
      filename = function() {
        paste("resultados-", input$indicador_trabajo_r, ".xlsx", sep = "")
      },
      content = function(file) {
        
        openxlsx::write.xlsx(list_trabajo_r(), file)
        
      }
    )

    ### 8.1. Ambiente Políticas   ===============================================
    
    # 8.1.1. Data reactiva   =================================================
    
    dat_ambiente_pp <- reactive({
      
      req(input$indicador_ambiente_pp)
      
      dat %>%
        filter(nomindicador == input$indicador_ambiente_pp) 
      
    })
    
    output$selector_ambiente_pp_corte <- renderUI({
      
      selectInput(
        inputId = "ambiente_pp_corte",
        label = "Seleccione corte:",
        choices = dat_ambiente_pp() %>% 
          select(corte) %>%
          arrange(corte) %>% 
          unique() %>% 
          pull(),
        selected = dat_ambiente_pp() %>% 
          filter(jerarquia == "1") %>%  
          distinct(corte) %>% 
          pull()
      )
      
    })
    
    output$selector_ambiente_pp_corte_2 <- renderUI({
      
      if(input$indicador_ambiente_pp %in% lista_ind_2){
        
        selectInput(
          inputId = "ambiente_pp_corte_2",
          label = "Seleccione primer corte:",
          choices = dat_ambiente_pp() %>% 
            select(corte_2) %>%
            arrange(corte_2) %>% 
            unique() %>% 
            pull(),
          selected = dat_ambiente_pp() %>% 
            filter(jerarquia == "1") %>%  
            distinct(corte_2) %>% 
            pull()
        )
        
      } else {
        
        NULL
      }
      
    })
    
    # Selector de fecha
    output$s_ambiente_pp_fecha <- renderUI({
      
      if(input$ambiente_pp_corte == "Departamento" & input$indicador_ambiente_pp %notin% lista_ind_2) {
        
        req(input$ambiente_pp_corte, input$indicador_ambiente_pp)
        
        req(nrow(dat_ambiente_pp()) > 0)
        
        selectInput(
          inputId = "fecha_dpto_ambiente_pp",
          label = "Seleccione año:",
          choices = dat_ambiente_pp() %>% 
            filter(nomindicador == input$indicador_ambiente_pp) %>%
            drop_na(Valor) %>%
            select(ano) %>%
            arrange(desc(ano)) %>% 
            unique() %>% 
            pull(),
          selected = 2019
        )
        
      } else if (input$indicador_ambiente_pp %in% lista_vunico){
        
        return(NULL)
        
      } else  {
        
        req(input$ambiente_pp_corte, input$indicador_ambiente_pp)
        req(nrow(dat_ambiente_pp()) > 0)
        
        tagList(
          # tags$style(type = 'text/css', 
          #            '#big_slider .irs-grid-text {font-size: 12px; transform: rotate(-90deg) translate(-10px);} ,.irs-grid-pol.large {height: 0px;}'),
          # div(id = 'big_slider',
          
          sliderInput("fecha_ambiente_pp", 
                      label = "Rango de tiempo", 
                      sep = "",
                      dragRange = T,
                      min = min(dat_ambiente_pp()$ano), 
                      max = max(dat_ambiente_pp()$ano), 
                      value = c(min(dat_ambiente_pp()$ano), 
                                max(dat_ambiente_pp()$ano))
          )
        )
        
      }
    })
    
    
    output$chbox_ambiente_pp <- renderUI({
      
      if(input$ambiente_pp_corte %notin% c("Total", "Departamento") & input$indicador_ambiente_pp %notin% lista_vunico) {
        
        ambiente_pp_corte_var <- rlang::sym(to_varname(input$ambiente_pp_corte))
        
        checkboxGroupInput(inputId = "checkbox_ambiente_pp",
                           label = "Seleccione categorías",
                           inline = TRUE,
                           choices =  dat_ambiente_pp() %>%
                             filter(corte == input$ambiente_pp_corte) %>% 
                             distinct(!!ambiente_pp_corte_var) %>%
                             pull(),
                           selected = dat_ambiente_pp() %>%
                             filter(corte == input$ambiente_pp_corte) %>% 
                             filter(jerarquia_cat == "1") %>%
                             distinct(!!ambiente_pp_corte_var) %>%
                             pull()
        )
        
      } else {
        
        return(NULL)
      }
      
    })
    
    # # Selector de corte según categoría y data temporal
    # dat_ambiente_pp <- reactive({
    #   
    #   req(input$ambiente_pp_corte)
    #   
    #   if(input$indicador_ambiente_pp %in% lista_ind_2){
    #     
    #     dat_salarios <- dat_ambiente_pp() %>%
    #       filter(corte_2 == input$ambiente_pp_corte_2) %>% 
    #       filter(corte == input$ambiente_pp_corte) %>%
    #       janitor::remove_empty("cols")
    #     
    #   } else {
    #     
    #     dat_ambiente_pp() %>%
    #       filter(corte == input$ambiente_pp_corte) %>%
    #       janitor::remove_empty("cols")
    #     
    #   }
    # })
    
    # 8.1.2. Metadata   ======================================================
    
    # Title
    output$title_ambiente_pp <- renderUI({ 
      helpText(HTML(unique(dat_ambiente_pp()$nomindicador)))
    })
    
    # Subtitle
    output$subtitle_ambiente_pp <- renderUI({ 
      helpText(HTML(unique(dat_ambiente_pp()$definicion)))
    })
    
    # Calculo
    output$calculo_ambiente_pp <- renderUI({ 
      helpText(HTML(paste("<b> Forma de cálculo:</b>", unique(dat_ambiente_pp()$calculo))))
    })
    
    
    # 8.1.3. Gráficos   ======================================================
    
    output$plot_ambiente_pp <- renderPlot({
      
      req(input$indicador_ambiente_pp)
      
      if(input$indicador_ambiente_pp %in% lista_vunico & input$indicador_ambiente_pp %in% lista_ind_2){
        
        req(input$ambiente_pp_corte, input$indicador_ambiente_pp)
        
        ambiente_pp_corte_var <- rlang::sym(to_varname(input$ambiente_pp_corte))
        ambiente_pp_corte_var_2 <- rlang::sym(to_varname(input$ambiente_pp_corte_2))
        
        dat_plot <- dat_ambiente_pp() %>%
          filter(ano >= input$fecha_ambiente_pp[1] &
                   ano <= input$fecha_ambiente_pp[2]) %>%
          filter(corte == input$ambiente_pp_corte) %>%
          filter(!!ambiente_pp_corte_var %in% input$checkbox_ambiente_pp) %>% 
          filter(corte_2 == input$ambiente_pp_corte_2)  
        
        plot_ambiente_corte <- ggplot(dat_plot,
                                     aes_string(x = "fecha_cat", y = "Valor",
                                                fill = ambiente_pp_corte_var)) +
          geom_col(position = "dodge", width = .7, alpha = .8) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x=element_blank(),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_ambiente_pp,
                                    "según",
                                    tolower(input$ambiente_pp_corte),
                                    "en",
                                    unique(dat_plot$fecha_cat))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_fill_brewer(name = "", palette = "Paired") +
          facet_wrap(as.formula(paste("~", ambiente_pp_corte_var_2)))
        
        print(plot_ambiente_corte)
        ggsave("www/indicador ambiente pp.png", width = 30, height = 20, units = "cm")
        
      } else if(input$indicador_ambiente_pp %in% lista_vunico & input$ambiente_pp_corte != "Departamento") {
        
        req(input$ambiente_pp_corte, input$indicador_ambiente_pp)
        
        dat_plot <- dat_ambiente_pp() %>%
          filter(corte == input$ambiente_pp_corte) %>%
          janitor::remove_empty("cols")
        
        if(input$ambiente_pp_corte == "Total"){
          
          plot_ambiente_corte <- ggplot(dat_plot,
                                       aes_string(x = "fecha_cat", y = "Valor")) +
            geom_col(position = "dodge", width = .4, alpha = .8, fill = color_defecto) +
            geom_text(aes(label = Valor), vjust = -0.4, fontface = "bold", size = 5) +
            theme_bdd(base_size = 12) +
            theme(axis.text.x=element_blank(),
                  legend.position = "bottom") +
            labs(x = "",  y = "",
                 title = wrapit(paste(input$indicador_ambiente_pp,
                                      "según",
                                      tolower(input$ambiente_pp_corte),
                                      "en",
                                      unique(dat_plot$fecha_cat))),
                 caption = wrapit(unique(dat_plot$cita))) 
          
          print(plot_ambiente_corte)
          ggsave("www/indicador ambiente pp.png", width = 30, height = 20, units = "cm")
          
          
        } else {
          
          ambiente_pp_corte_var <- rlang::sym(to_varname(input$ambiente_pp_corte))
          
          plot_ambiente_corte <- ggplot(dat_plot,
                                       aes_string(x = "fecha_cat", y = "Valor",
                                                  fill = ambiente_pp_corte_var)) +
            geom_col(position = "dodge", width = .7, alpha = .8) +
            theme_bdd(base_size = 12) +
            theme(axis.text.x=element_blank(),
                  legend.position = "bottom") +
            labs(x = "",  y = "",
                 title = wrapit(paste(input$indicador_ambiente_pp,
                                      "según",
                                      tolower(input$ambiente_pp_corte),
                                      "en",
                                      unique(dat_plot$fecha_cat))),
                 caption = wrapit(unique(dat_plot$cita))) +
            scale_fill_brewer(name = "", palette = "Paired") 
          
          print(plot_ambiente_corte)
          ggsave("www/indicador ambiente pp.png", width = 30, height = 20, units = "cm")
          
        }
        
        print(plot_ambiente_corte)
        ggsave("www/indicador ambiente pp.png", width = 30, height = 20, units = "cm")
        
      } else if(input$indicador_ambiente_pp %in% lista_ind_2) {
        
        req(input$ambiente_pp_corte, input$ambiente_pp_corte_2,
            input$fecha_ambiente_pp, input$checkbox_ambiente_pp)
        
        ambiente_pp_corte_var <- rlang::sym(to_varname(input$ambiente_pp_corte))
        
        dat_plot <- dat_ambiente_pp() %>%
          filter(ano >= input$fecha_ambiente_pp[1] &
                   ano <= input$fecha_ambiente_pp[2]) %>%
          filter(corte == input$ambiente_pp_corte) %>%
          filter(!!ambiente_pp_corte_var %in% input$checkbox_ambiente_pp)
        
        if(input$ambiente_pp_corte_2 == "Total"){
          
          dat_plot <- dat_plot %>% 
            filter(corte_2 == "Total")
          
          plot_ambiente_corte <- ggplot(dat_plot,
                                   aes_string(x = "fecha", y = "Valor", colour = ambiente_pp_corte_var)) +
            geom_line(size = 1, alpha = 0.5) +
            geom_point(size = 3) +
            theme_bdd(base_size = 12) +
            theme(axis.text.x = element_text(angle = 0),
                  legend.position = "bottom") +
            labs(x = "",  y = "",
                 title = wrapit(paste(input$indicador_ambiente_pp,
                                      "según",
                                      tolower(input$ambiente_pp_corte))),
                 caption = wrapit(unique(dat_plot$cita))) +
            scale_colour_manual(name = "", values = paleta_expandida) 
          
        } else if(input$ambiente_pp_corte_2 != "Total") {
          
          ambiente_pp_corte_var_2 <- rlang::sym(to_varname(input$ambiente_pp_corte_2))
          
          dat_plot <- dat_plot %>%
            filter(corte_2 == input$ambiente_pp_corte_2)  
          
          plot_ambiente_corte <- ggplot(dat_plot,
                                   aes_string(x = "fecha", y = "Valor", colour = ambiente_pp_corte_var)) +
            geom_line(size = 1, alpha = 0.5) +
            geom_point(size = 3) +
            theme_bdd(base_size = 12) +
            theme(axis.text.x = element_text(angle = 0),
                  legend.position = "bottom") +
            labs(x = "",  y = "",
                 title = wrapit(paste(input$indicador_ambiente_pp,
                                      "según",
                                      tolower(input$ambiente_pp_corte))),
                 caption = wrapit(unique(dat_plot$cita))) +
            scale_colour_manual(name = "", values = paleta_expandida) + 
          facet_wrap(as.formula(paste("~", ambiente_pp_corte_var_2)))
          
        }
        
        print(plot_ambiente_corte)
        ggsave("www/indicador ambiente pp.png", width = 30, height = 20, units = "cm")
        
        
      } else if(input$ambiente_pp_corte == "Total") {
        
        req(input$indicador_ambiente_pp, input$fecha_ambiente_pp)
        
        dat_plot <- dat_ambiente_pp() %>% 
          filter(ano >= input$fecha_ambiente_pp[1] &
                   ano <= input$fecha_ambiente_pp[2]) %>% 
          filter(corte == "Total")
        
        plot_ambiente <- ggplot(dat_plot,
                           aes(x = fecha, y = Valor)) +
          geom_line(size = 1, alpha = 0.5, colour = color_defecto) +
          geom_point(size = 3, colour = color_defecto) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(input$indicador_ambiente_pp),
               caption = wrapit(unique(dat_plot$cita))) 
        
        print(plot_ambiente)
        ggsave("www/indicador ambiente pp.png", width = 30, height = 20, units = "cm")
        
        
      } else if(input$ambiente_pp_corte == "Departamento" & 
                input$indicador_ambiente_pp %notin% lista_ind_2 ) {
        
        req(input$indicador_ambiente_pp, input$fecha_dpto_ambiente_pp)
        
        dat_plot <- dat_ambiente_pp() %>%
          filter(corte == "Departamento") %>%
          filter(ano == input$fecha_dpto_ambiente_pp) %>% 
          select(departamento, Valor, fuente, cita)
        
        dep_j <- dep %>%
          left_join(dat_plot, by = c("nombre" = "departamento"))
        
        plot_ambiente_dpto <- geouy::plot_geouy(dep_j,
                                           "Valor",
                                           viri_opt = "plasma",
                                           l = "n") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_ambiente_pp, 
                                    "en",
                                    input$fecha_dpto_ambiente_pp), w = 80),
               caption = wrapit(unique(dat_plot$cita), w = 80)) +
          theme_bdd(base_size = 14)
        
        print(plot_ambiente_dpto)
        ggsave("www/indicador ambiente pp.png", width = 30, height = 20, units = "cm")
        
        
      } else if(input$ambiente_pp_corte != "Total") {
        
        req(input$ambiente_pp_corte, input$indicador_ambiente_pp, 
            input$fecha_ambiente_pp, input$checkbox_ambiente_pp)
        
        dat_plot <- dat_ambiente_pp() %>%
          filter(ano >= input$fecha_ambiente_pp[1] &
                   ano <= input$fecha_ambiente_pp[2]) %>%
          filter(corte == input$ambiente_pp_corte) %>%
          janitor::remove_empty("cols")
        
        ambiente_pp_corte_var <- rlang::sym(to_varname(input$ambiente_pp_corte))
        
        dat_plot <- filter(dat_plot,
                           !!ambiente_pp_corte_var %in% input$checkbox_ambiente_pp)
        
        plot_ambiente_corte <- ggplot(dat_plot,
                                 aes_string(x = "fecha", y = "Valor", colour = ambiente_pp_corte_var)) +
          geom_line(size = 1, alpha = 0.5) +
          geom_point(size = 3) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_ambiente_pp,
                                    "según",
                                    tolower(input$ambiente_pp_corte))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_colour_manual(name = "", values = paleta_expandida) 
        
        print(plot_ambiente_corte)
        ggsave("www/indicador ambiente pp.png", width = 30, height = 20, units = "cm")
        
      }
      
    })
    
    # 8.1.4. Descarga gráficos   =============================================
    
    output$baja_p_ambiente_pp <- downloadHandler(
      filename <- function() {
        paste("indicador ambiente pp", "png", sep = ".")
      },
      
      content <- function(file) {
        file.copy("www/indicador ambiente pp.png", file)
      },
      contentType = "www/indicador ambiente p"
    )
    
    
    # 8.1.5. Tablas   ========================================================
    
    # Data
    ambiente_pp_tab <- reactive({
      
      if(input$indicador_ambiente_pp %in% lista_vunico & input$ambiente_pp_corte == "Total"){
        
        req(input$ambiente_pp_corte, input$indicador_ambiente_pp)
        
        dat_ambiente_pp() %>%
          filter(corte == "Total") %>% 
          select(Fecha, Valor) %>%
          arrange(desc(Fecha))
        
      } else if(input$indicador_ambiente_pp %in% lista_vunico & input$ambiente_pp_corte != "Total") {
        
        req(input$ambiente_pp_corte, input$indicador_ambiente_pp)
        
        ambiente_pp_corte_var <- rlang::sym(to_varname(input$ambiente_pp_corte))
        
        dat_cut <- dat_ambiente_pp() %>%
          filter(corte == input$ambiente_pp_corte) %>%
          janitor::remove_empty("cols") 
        
        dat_cut %>%     
          select(Fecha, ambiente_pp_corte_var, Valor) %>%
          arrange(desc(Fecha)) %>% 
          pivot_wider(values_from = "Valor",
                      names_from = ambiente_pp_corte_var)
        
        
      } else if(input$indicador_ambiente_pp %in% lista_ind_2) {
        
        req(input$ambiente_pp_corte, input$indicador_ambiente_pp, input$fecha_ambiente_pp)
        
        ambiente_pp_corte_var <- rlang::sym(to_varname(input$ambiente_pp_corte))
        
        dat_cut <- dat_ambiente_pp() %>%
          filter(corte == input$ambiente_pp_corte) %>%
          filter(!!ambiente_pp_corte_var %in% input$checkbox_ambiente_pp)
        
        if(input$ambiente_pp_corte_2 == "Total"){
          
          dat_cut %>%
            filter(ano >= input$fecha_ambiente_pp[1] &
                     ano <= input$fecha_ambiente_pp[2]) %>%
            filter(corte_2 == "Total") %>% 
            select(Fecha, ambiente_pp_corte_var, Valor) %>%
            arrange(desc(Fecha)) %>%
            pivot_wider(values_from = "Valor",
                        names_from = ambiente_pp_corte_var)
          
        } else if(input$ambiente_pp_corte_2 != "Total") {
          
          ambiente_pp_corte_var_2 <- rlang::sym(to_varname(input$ambiente_pp_corte_2))
          
          dat_cut %>%
            filter(ano >= input$fecha_ambiente_pp[1] &
                     ano <= input$fecha_ambiente_pp[2]) %>%
            filter(corte_2 == input$ambiente_pp_corte_2) %>% 
            select(Fecha, ambiente_pp_corte_var, ambiente_pp_corte_var_2, Valor) %>%
            arrange(desc(Fecha)) %>%
            pivot_wider(values_from = "Valor",
                        names_from = ambiente_pp_corte_var) 
          
        }
        
      } else if(input$ambiente_pp_corte == "Total") {
        
        req(input$ambiente_pp_corte, input$indicador_ambiente_pp, input$fecha_ambiente_pp)
        
        dat_ambiente_pp() %>%
          filter(corte == "Total") %>% 
          filter(ano >= input$fecha_ambiente_pp[1] &
                   ano <= input$fecha_ambiente_pp[2]) %>%
          select(Fecha, Valor) %>%
          arrange(desc(Fecha))
        
      } else if(input$ambiente_pp_corte != "Total") {
        
        req(input$ambiente_pp_corte, input$indicador_ambiente_pp, input$fecha_ambiente_pp)
        
        ambiente_pp_corte_var <- rlang::sym(to_varname(input$ambiente_pp_corte))
        
        dat_cut <- dat_ambiente_pp() %>%
          filter(corte == input$ambiente_pp_corte) %>%
          janitor::remove_empty("cols") 
        
        dat_cut %>%     
          filter(ano >= input$fecha_ambiente_pp[1] &
                   ano <= input$fecha_ambiente_pp[2]) %>% 
          select(Fecha, ambiente_pp_corte_var, Valor) %>%
          arrange(desc(Fecha)) %>% 
          pivot_wider(values_from = "Valor",
                      names_from = ambiente_pp_corte_var)
        
      }
    })
    
    # Metadata 
    ambiente_pp_meta <- reactive({
      
      dat_ambiente_pp() %>%
        select(nomindicador, derecho, conindicador, tipoind, definicion, calculo, cita) %>% 
        mutate(`Mirador DESCA - UMAD/FCS – INDDHH` = " ") %>% 
        distinct() %>% 
        gather(key = "", value = " ")
      
    })
    
    # Excel
    list_ambiente_pp <- reactive({
      list_ambiente_pp <- list("Data" = ambiente_pp_tab(),
                               "Metadata" = ambiente_pp_meta())
    })
    
    # Render
    output$table_ambiente_pp <- renderDT({
      
      DT::datatable(ambiente_pp_tab(),
                    rownames = FALSE,
                    caption = htmltools::tags$caption(
                      input$indicador_ambiente_pp,
                      style = "color:black; font-size:110%;")
      ) 
      
    })
    
    # 8.1.6. Descarga tablas   ================================================
    
    output$dwl_tab_ambiente_pp <- downloadHandler(
      
      filename = function() {
        paste("pp-", input$indicador_ambiente_pp, ".xlsx", sep = "")
      },
      content = function(file) {
        
        openxlsx::write.xlsx(list_ambiente_pp(), file)
        
      }
    )
    
    
    
    ### 8.2. Ambiente Resultados   =============================================
    
    # 8.2.1. Data reactiva   =================================================
    
    dat_ambiente_r <- reactive({
      
      req(input$indicador_ambiente_r)
      
      dat %>%
        filter(nomindicador == input$indicador_ambiente_r) 
      
    })
    
    output$selector_ambiente_r_corte <- renderUI({
      
      selectInput(
        inputId = "ambiente_r_corte",
        label = "Seleccione corte:",
        choices = dat_ambiente_r() %>% 
          select(corte) %>%
          arrange(corte) %>% 
          unique() %>% 
          pull(),
        selected = dat_ambiente_r() %>% 
          filter(jerarquia == "1") %>%  
          distinct(corte) %>% 
          pull()
      )
      
    })
    
    output$selector_ambiente_r_corte_2 <- renderUI({
      
      if(input$indicador_ambiente_r %in% lista_ind_2){
        
        selectInput(
          inputId = "ambiente_r_corte_2",
          label = "Seleccione primer corte:",
          choices = dat_ambiente_r() %>% 
            select(corte_2) %>%
            arrange(corte_2) %>% 
            unique() %>% 
            pull(),
          selected = dat_ambiente_r() %>% 
            filter(jerarquia == "1") %>%  
            distinct(corte_2) %>% 
            pull()
        )
        
      } else {
        
        NULL
      }
      
    })
    
    # Selector de fecha
    output$s_ambiente_r_fecha <- renderUI({
      
      if(input$ambiente_r_corte == "Departamento" & input$indicador_ambiente_r %notin% lista_ind_2) {
        
        req(input$ambiente_r_corte, input$indicador_ambiente_r)
        
        req(nrow(dat_ambiente_r()) > 0)
        
        selectInput(
          inputId = "fecha_dpto_ambiente_r",
          label = "Seleccione año:",
          choices = dat_ambiente_r() %>% 
            filter(nomindicador == input$indicador_ambiente_r) %>%
            drop_na(Valor) %>%
            select(ano) %>%
            arrange(desc(ano)) %>% 
            unique() %>% 
            pull(),
          selected = 2019
        )
        
      } else if (input$indicador_ambiente_r %in% lista_vunico){
        
        return(NULL)
        
      } else  {
        
        req(input$ambiente_r_corte, input$indicador_ambiente_r)
        req(nrow(dat_ambiente_r()) > 0)
        
        tagList(
          # tags$style(type = 'text/css', 
          #            '#big_slider .irs-grid-text {font-size: 12px; transform: rotate(-90deg) translate(-10px);} ,.irs-grid-pol.large {height: 0px;}'),
          # div(id = 'big_slider',
          
          sliderInput("fecha_ambiente_r", 
                      label = "Rango de tiempo", 
                      sep = "",
                      dragRange = T,
                      min = min(dat_ambiente_r()$ano), 
                      max = max(dat_ambiente_r()$ano), 
                      value = c(min(dat_ambiente_r()$ano), 
                                max(dat_ambiente_r()$ano))
          )
        )
        
      }
    })
    
    
    output$chbox_ambiente_r <- renderUI({
      
      if(input$ambiente_r_corte %notin% c("Total", "Departamento") & input$indicador_ambiente_r %notin% lista_vunico) {
        
        ambiente_r_corte_var <- rlang::sym(to_varname(input$ambiente_r_corte))
        
        checkboxGroupInput(inputId = "checkbox_ambiente_r",
                           label = "Seleccione categorías",
                           inline = TRUE,
                           choices =  dat_ambiente_r() %>%
                             filter(corte == input$ambiente_r_corte) %>% 
                             distinct(!!ambiente_r_corte_var) %>%
                             pull(),
                           selected = dat_ambiente_r() %>%
                             filter(corte == input$ambiente_r_corte) %>% 
                             filter(jerarquia_cat == "1") %>%
                             distinct(!!ambiente_r_corte_var) %>%
                             pull()
        )
        
      } else {
        
        return(NULL)
      }
      
    })
    
    # # Selector de corte según categoría y data temporal
    # dat_ambiente_r <- reactive({
    #   
    #   req(input$ambiente_r_corte)
    #   
    #   if(input$indicador_ambiente_r %in% lista_ind_2){
    #     
    #     dat_salarios <- dat_ambiente_r() %>%
    #       filter(corte_2 == input$ambiente_r_corte_2) %>% 
    #       filter(corte == input$ambiente_r_corte) %>%
    #       janitor::remove_empty("cols")
    #     
    #   } else {
    #     
    #     dat_ambiente_r() %>%
    #       filter(corte == input$ambiente_r_corte) %>%
    #       janitor::remove_empty("cols")
    #     
    #   }
    # })
    
    # 8.2.2. Metadata   ======================================================
    
    # Title
    output$title_ambiente_r <- renderUI({ 
      helpText(HTML(unique(dat_ambiente_r()$nomindicador)))
    })
    
    # Subtitle
    output$subtitle_ambiente_r <- renderUI({ 
      helpText(HTML(unique(dat_ambiente_r()$definicion)))
    })
    
    # Calculo
    output$calculo_ambiente_r <- renderUI({ 
      helpText(HTML(paste("<b> Forma de cálculo:</b>", unique(dat_ambiente_r()$calculo))))
    })
    
    
    # 8.2.3. Gráficos   ======================================================
    
    output$plot_ambiente_r <- renderPlot({
      
      req(input$indicador_ambiente_r)
      
      if(input$indicador_ambiente_r %in% lista_vunico & input$indicador_ambiente_r %in% lista_ind_2){
        
        req(input$ambiente_r_corte, input$indicador_ambiente_r)
        
        ambiente_r_corte_var <- rlang::sym(to_varname(input$ambiente_r_corte))
        ambiente_r_corte_var_2 <- rlang::sym(to_varname(input$ambiente_r_corte_2))
        
        dat_plot <- dat_ambiente_r() %>%
          filter(ano >= input$fecha_ambiente_r[1] &
                   ano <= input$fecha_ambiente_r[2]) %>%
          filter(corte == input$ambiente_r_corte) %>%
          filter(!!ambiente_r_corte_var %in% input$checkbox_ambiente_r) %>% 
          filter(corte_2 == input$ambiente_r_corte_2)  
        
        plot_ambiente_corte <- ggplot(dat_plot,
                                      aes_string(x = "fecha_cat", y = "Valor",
                                                 fill = ambiente_r_corte_var)) +
          geom_col(position = "dodge", width = .7, alpha = .8) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x=element_blank(),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_ambiente_r,
                                    "según",
                                    tolower(input$ambiente_r_corte),
                                    "en",
                                    unique(dat_plot$fecha_cat))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_fill_brewer(name = "", palette = "Paired") +
          facet_wrap(as.formula(paste("~", ambiente_r_corte_var_2)))
        
        print(plot_ambiente_corte)
        ggsave("www/indicador ambiente r.png", width = 30, height = 20, units = "cm")
        
      } else if(input$indicador_ambiente_r %in% lista_vunico & input$ambiente_r_corte != "Departamento") {
        
        req(input$ambiente_r_corte, input$indicador_ambiente_r)
        
        dat_plot <- dat_ambiente_r() %>%
          filter(corte == input$ambiente_r_corte) %>%
          janitor::remove_empty("cols")
        
        if(input$ambiente_r_corte == "Total"){
          
          plot_ambiente_corte <- ggplot(dat_plot,
                                        aes_string(x = "fecha_cat", y = "Valor")) +
            geom_col(position = "dodge", width = .4, alpha = .8, fill = color_defecto) +
            geom_text(aes(label = Valor), vjust = -0.4, fontface = "bold", size = 5) +
            theme_bdd(base_size = 12) +
            theme(axis.text.x=element_blank(),
                  legend.position = "bottom") +
            labs(x = "",  y = "",
                 title = wrapit(paste(input$indicador_ambiente_r,
                                      "según",
                                      tolower(input$ambiente_r_corte),
                                      "en",
                                      unique(dat_plot$fecha_cat))),
                 caption = wrapit(unique(dat_plot$cita))) 
          
          print(plot_ambiente_corte)
          ggsave("www/indicador ambiente r.png", width = 30, height = 20, units = "cm")
          
          
        } else {
          
          ambiente_r_corte_var <- rlang::sym(to_varname(input$ambiente_r_corte))
          
          plot_ambiente_corte <- ggplot(dat_plot,
                                        aes_string(x = "fecha_cat", y = "Valor",
                                                   fill = ambiente_r_corte_var)) +
            geom_col(position = "dodge", width = .7, alpha = .8) +
            theme_bdd(base_size = 12) +
            theme(axis.text.x=element_blank(),
                  legend.position = "bottom") +
            labs(x = "",  y = "",
                 title = wrapit(paste(input$indicador_ambiente_r,
                                      "según",
                                      tolower(input$ambiente_r_corte),
                                      "en",
                                      unique(dat_plot$fecha_cat))),
                 caption = wrapit(unique(dat_plot$cita))) +
            scale_fill_brewer(name = "", palette = "Paired") 
          
          print(plot_ambiente_corte)
          ggsave("www/indicador ambiente r.png", width = 30, height = 20, units = "cm")
          
        }
        
        print(plot_ambiente_corte)
        ggsave("www/indicador ambiente r.png", width = 30, height = 20, units = "cm")
        
      } else if(input$indicador_ambiente_r %in% lista_ind_2) {
        
        req(input$ambiente_r_corte, input$ambiente_r_corte_2,
            input$fecha_ambiente_r, input$checkbox_ambiente_r)
        
        ambiente_r_corte_var <- rlang::sym(to_varname(input$ambiente_r_corte))
        
        dat_plot <- dat_ambiente_r() %>%
          filter(ano >= input$fecha_ambiente_r[1] &
                   ano <= input$fecha_ambiente_r[2]) %>%
          filter(corte == input$ambiente_r_corte) %>%
          filter(!!ambiente_r_corte_var %in% input$checkbox_ambiente_r)
        
        if(input$ambiente_r_corte_2 == "Total"){
          
          dat_plot <- dat_plot %>% 
            filter(corte_2 == "Total")
          
          plot_ambiente_corte <- ggplot(dat_plot,
                                   aes_string(x = "fecha", y = "Valor", colour = ambiente_r_corte_var)) +
            geom_line(size = 1, alpha = 0.5) +
            geom_point(size = 3) +
            theme_bdd(base_size = 12) +
            theme(axis.text.x = element_text(angle = 0),
                  legend.position = "bottom") +
            labs(x = "",  y = "",
                 title = wrapit(paste(input$indicador_ambiente_r,
                                      "según",
                                      tolower(input$ambiente_r_corte))),
                 caption = wrapit(unique(dat_plot$cita))) +
            scale_colour_manual(name = "", values = paleta_expandida) 
          
        } else if(input$ambiente_r_corte_2 != "Total") {
          
          ambiente_r_corte_var_2 <- rlang::sym(to_varname(input$ambiente_r_corte_2))
          
          dat_plot <- dat_plot %>%
            filter(corte_2 == input$ambiente_r_corte_2)  
          
          plot_ambiente_corte <- ggplot(dat_plot,
                                   aes_string(x = "fecha", y = "Valor", colour = ambiente_r_corte_var)) +
            geom_line(size = 1, alpha = 0.5) +
            geom_point(size = 3) +
            theme_bdd(base_size = 12) +
            theme(axis.text.x = element_text(angle = 0),
                  legend.position = "bottom") +
            labs(x = "",  y = "",
                 title = wrapit(paste(input$indicador_ambiente_r,
                                      "según",
                                      tolower(input$ambiente_r_corte))),
                 caption = wrapit(unique(dat_plot$cita))) +
            scale_colour_manual(name = "", values = paleta_expandida) +
          facet_wrap(as.formula(paste("~", ambiente_r_corte_var_2)))
          
        }
        
        print(plot_ambiente_corte)
        ggsave("www/indicador ambiente r.png", width = 30, height = 20, units = "cm")
        
        
      } else if(input$ambiente_r_corte == "Total") {
        
        req(input$indicador_ambiente_r, input$fecha_ambiente_r)
        
        dat_plot <- dat_ambiente_r() %>% 
          filter(ano >= input$fecha_ambiente_r[1] &
                   ano <= input$fecha_ambiente_r[2]) %>% 
          filter(corte == "Total")
        
        plot_ambiente <- ggplot(dat_plot,
                           aes(x = fecha, y = Valor)) +
          geom_line(size = 1, alpha = 0.5, colour = color_defecto) +
          geom_point(size = 3, colour = color_defecto) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(input$indicador_ambiente_r),
               caption = wrapit(unique(dat_plot$cita))) 
        
        print(plot_ambiente)
        ggsave("www/indicador ambiente r.png", width = 30, height = 20, units = "cm")
        
        
      } else if(input$ambiente_r_corte == "Departamento" & 
                input$indicador_ambiente_r %notin% lista_ind_2 ) {
        
        req(input$indicador_ambiente_r, input$fecha_dpto_ambiente_r)
        
        dat_plot <- dat_ambiente_r() %>%
          filter(corte == "Departamento") %>%
          filter(ano == input$fecha_dpto_ambiente_r) %>% 
          select(departamento, Valor, fuente, cita)
        
        dep_j <- dep %>%
          left_join(dat_plot, by = c("nombre" = "departamento"))
        
        plot_ambiente_dpto <- geouy::plot_geouy(dep_j,
                                           "Valor",
                                           viri_opt = "plasma",
                                           l = "n") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_ambiente_r, 
                                    "en",
                                    input$fecha_dpto_ambiente_r), w = 80),
               caption = wrapit(unique(dat_plot$cita), w = 80)) +
          theme_bdd(base_size = 14)
        
        print(plot_ambiente_dpto)
        ggsave("www/indicador ambiente r.png", width = 30, height = 20, units = "cm")
        
        
      } else if(input$ambiente_r_corte != "Total") {
        
        req(input$ambiente_r_corte, input$indicador_ambiente_r, 
            input$fecha_ambiente_r, input$checkbox_ambiente_r)
        
        dat_plot <- dat_ambiente_r() %>%
          filter(ano >= input$fecha_ambiente_r[1] &
                   ano <= input$fecha_ambiente_r[2]) %>%
          filter(corte == input$ambiente_r_corte) %>%
          janitor::remove_empty("cols")
        
        ambiente_r_corte_var <- rlang::sym(to_varname(input$ambiente_r_corte))
        
        dat_plot <- filter(dat_plot,
                           !!ambiente_r_corte_var %in% input$checkbox_ambiente_r)
        
        plot_ambiente_corte <- ggplot(dat_plot,
                                 aes_string(x = "fecha", y = "Valor", colour = ambiente_r_corte_var)) +
          geom_line(size = 1, alpha = 0.5) +
          geom_point(size = 3) +
          theme_bdd(base_size = 12) +
          theme(axis.text.x = element_text(angle = 0),
                legend.position = "bottom") +
          labs(x = "",  y = "",
               title = wrapit(paste(input$indicador_ambiente_r,
                                    "según",
                                    tolower(input$ambiente_r_corte))),
               caption = wrapit(unique(dat_plot$cita))) +
          scale_colour_manual(name = "", values = paleta_expandida) 
        
        print(plot_ambiente_corte)
        ggsave("www/indicador ambiente r.png", width = 30, height = 20, units = "cm")
        
      }
      
    })
    
    # 8.2.4. Descarga gráficos   =============================================
    
    output$baja_p_ambiente_r <- downloadHandler(
      filename <- function() {
        paste("indicador ambiente r", "png", sep = ".")
      },
      
      content <- function(file) {
        file.copy("www/indicador ambiente r.png", file)
      },
      contentType = "www/indicador ambiente r"
    )
    
    
    # 8.2.5. Tablas   ========================================================
    
    # Data
    ambiente_r_tab <- reactive({
      
      if(input$indicador_ambiente_pp %in% lista_vunico & input$ambiente_r_corte == "Total"){
        
        req(input$ambiente_r_corte, input$indicador_ambiente_r)
        
        dat_ambiente_r() %>%
          filter(corte == "Total") %>% 
          select(Fecha, Valor) %>%
          arrange(desc(Fecha))
        
      } else if(input$indicador_ambiente_r %in% lista_vunico & input$ambiente_r_corte != "Total") {
        
        req(input$ambiente_r_corte, input$indicador_ambiente_r)
        
        ambiente_r_corte_var <- rlang::sym(to_varname(input$ambiente_r_corte))
        
        dat_cut <- dat_ambiente_r() %>%
          filter(corte == input$ambiente_r_corte) %>%
          janitor::remove_empty("cols") 
        
        dat_cut %>%     
          select(Fecha, ambiente_r_corte_var, Valor) %>%
          arrange(desc(Fecha)) %>% 
          pivot_wider(values_from = "Valor",
                      names_from = ambiente_r_corte_var)
        
        
      } else if(input$indicador_ambiente_r %in% lista_ind_2) {
        
        req(input$ambiente_r_corte, input$indicador_ambiente_r, input$fecha_ambiente_r)
        
        ambiente_r_corte_var <- rlang::sym(to_varname(input$ambiente_r_corte))
        
        dat_cut <- dat_ambiente_r() %>%
          filter(corte == input$ambiente_r_corte) %>%
          filter(!!ambiente_r_corte_var %in% input$checkbox_ambiente_r)
        
        if(input$ambiente_r_corte_2 == "Total"){
          
          dat_cut %>%
            filter(ano >= input$fecha_ambiente_r[1] &
                     ano <= input$fecha_ambiente_r[2]) %>%
            filter(corte_2 == "Total") %>% 
            select(Fecha, ambiente_r_corte_var, Valor) %>%
            arrange(desc(Fecha)) %>%
            pivot_wider(values_from = "Valor",
                        names_from = ambiente_r_corte_var)
          
        } else if(input$ambiente_r_corte_2 != "Total") {
          
          ambiente_r_corte_var_2 <- rlang::sym(to_varname(input$ambiente_r_corte_2))
          
          dat_cut %>%
            filter(ano >= input$fecha_ambiente_r[1] &
                     ano <= input$fecha_ambiente_r[2]) %>%
            filter(corte_2 == input$ambiente_r_corte_2) %>% 
            select(Fecha, ambiente_r_corte_var, ambiente_r_corte_var_2, Valor) %>%
            arrange(desc(Fecha)) %>%
            pivot_wider(values_from = "Valor",
                        names_from = ambiente_r_corte_var) 
          
        }
        
      } else if(input$ambiente_r_corte == "Total") {
        
        req(input$ambiente_r_corte, input$indicador_ambiente_r, input$fecha_ambiente_r)
        
        dat_ambiente_r() %>%
          filter(corte == "Total") %>% 
          filter(ano >= input$fecha_ambiente_r[1] &
                   ano <= input$fecha_ambiente_r[2]) %>%
          select(Fecha, Valor) %>%
          arrange(desc(Fecha))
        
      } else if(input$ambiente_r_corte != "Total") {
        
        req(input$ambiente_r_corte, input$indicador_ambiente_r, input$fecha_ambiente_r)
        
        ambiente_r_corte_var <- rlang::sym(to_varname(input$ambiente_r_corte))
        
        dat_cut <- dat_ambiente_r() %>%
          filter(corte == input$ambiente_r_corte) %>%
          janitor::remove_empty("cols") 
        
        dat_cut %>%     
          filter(ano >= input$fecha_ambiente_r[1] &
                   ano <= input$fecha_ambiente_r[2]) %>% 
          select(Fecha, ambiente_r_corte_var, Valor) %>%
          arrange(desc(Fecha)) %>% 
          pivot_wider(values_from = "Valor",
                      names_from = ambiente_r_corte_var)
        
      }
    })
    
    # Metadata 
    ambiente_r_meta <- reactive({
      
      dat_ambiente_r() %>%
        select(nomindicador, derecho, conindicador, tipoind, definicion, calculo, cita) %>% 
        mutate(`Mirador DESCA - UMAD/FCS – INDDHH` = " ") %>% 
        distinct() %>% 
        gather(key = "", value = " ")
      
    })
    
    # Excel
    list_ambiente_r <- reactive({
      list_ambiente_r <- list("Data" = ambiente_r_tab(),
                              "Metadata" = ambiente_r_meta())
    })
    
    # Render
    output$table_ambiente_r <- renderDT({
      
      DT::datatable(ambiente_r_tab(),
                    rownames = FALSE,
                    caption = htmltools::tags$caption(
                      input$indicador_ambiente_r,
                      style = "color:black; font-size:110%;")
      ) 
      
    })
    
    # 8.2.6. Descarga tablas   ================================================
    
    output$dwl_tab_ambiente_r <- downloadHandler(
      
      filename = function() {
        paste("resultados-", input$indicador_ambiente_r, ".xlsx", sep = "")
      },
      content = function(file) {
        
        openxlsx::write.xlsx(list_ambiente_r(), file)
        
      }
    )
}

# Run the application
shinyApp(ui = ui, server = server)
