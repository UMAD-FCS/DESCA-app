
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
        
        
    )
)


##  3.  SERVER  =============================================================

server <- function(input, output) {
    

### 3.1. Educación Políticas   ==============================================
    
    # 3.1.1. Data reactiva   ================================================
    
    dat_edu_pp <- reactive({
        
        req(input$indicador_edu_pp)
        
        dat %>%
            filter(nomindicador == input$indicador_edu_pp) 
        
    })
    
    # Selector de corte
    output$selector_edu_pp_corte <- renderUI({
        
        req(nrow(dat_edu_pp()) > 0)
        
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
                pull())
    })
    
    # Selector de fecha
    output$s_edu_pp_fecha <- renderUI({
        
        if(input$edu_pp_corte == "Departamento") {
            
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
            # )
            
        }
    })

    # Selector de corte según categoría y data temporal
    dat_edu_pp_temp <- reactive({

    req(input$edu_pp_corte)
        
    dat_edu_pp() %>%
        filter(corte == input$edu_pp_corte) %>%
        janitor::remove_empty("cols")
        
    })
    
    output$chbox_edu_pp <- renderUI({
        
        if(input$edu_pp_corte %in% c("Departamento", "Total")) {
            
            return(NULL)
            
            } else if(input$edu_pp_corte != "Total") {

            checkboxGroupInput(inputId = "checkbox_edu_pp",
                               label = "Seleccione categorías",
                               inline = TRUE,
                               choices =  dat_edu_pp_temp() %>%
                                   distinct(get(names(dat_edu_pp_temp()[,ncol(dat_edu_pp_temp())]))) %>%
                                   pull(),
                               selected = dat_edu_pp_temp() %>%
                                   distinct(get(names(dat_edu_pp_temp()[,ncol(dat_edu_pp_temp())]))) %>%
                                   pull() 
                               )
            }
        
        })


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
        
         if(input$edu_pp_corte == "Total") {

            req(input$indicador_edu_pp, input$fecha_edu_pp)
            
             req(nrow(dat_edu_pp()) > 0)
             
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
            ggsave("www/indicador educacion pp.png", width = 30, height = 20, units = "cm")
            
 
             } else if(input$edu_pp_corte == "Departamento") {

            req(input$indicador_edu_pp, input$fecha_dpto_edu_pp)
            
            req(nrow(dat_edu_pp()) > 0)
                 
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
            ggsave("www/indicador educacion pp.png", width = 30, height = 20, units = "cm")
        
        } else if(input$edu_pp_corte != "Total") {
        
            req(input$edu_pp_corte, input$indicador_edu_pp,
                input$fecha_edu_pp, input$checkbox_edu_pp)
            
            req(nrow(dat_edu_pp()) > 0)
            
            dat_plot <- dat_edu_pp() %>%
                filter(ano >= input$fecha_edu_pp[1] &
                           ano <= input$fecha_edu_pp[2]) %>%
                filter(corte == input$edu_pp_corte) %>%
                janitor::remove_empty("cols") 
            
            dat_plot <- filter(dat_plot,
                               get(names(dat_plot[,ncol(dat_plot)])) %in% input$checkbox_edu_pp)
            
            plot_edu_corte <- ggplot(dat_plot,
                               aes_string(x = "fecha", y = "Valor", colour = names(dat_plot[,ncol(dat_plot)]))) +
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
            ggsave("www/indicador educacion pp.png", width = 30, height = 20, units = "cm")
         
        }
        
        })
    
    # 3.1.4. Descarga gráficos   =============================================
    
    output$baja_p_edu_pp <- downloadHandler(
        filename <- function() {
            paste("indicador educacion pp", "png", sep = ".")
        },
        
        content <- function(file) {
            file.copy("www/indicador educacion pp.png", file)
        },
        contentType = "www/indicador educacion pp"
    )
    
    
    # 3.1.5. Tablas   ========================================================
    
    # Data
    edu_pp_tab <- reactive({
        
        if(input$edu_pp_corte == "Total") {
        
            req(input$edu_pp_corte, input$indicador_edu_pp, input$fecha_edu_pp)
            
            dat_edu_pp() %>%
                filter(corte == "Total") %>% 
                filter(ano >= input$fecha_edu_pp[1] &
                           ano <= input$fecha_edu_pp[2]) %>%
                select(Fecha, Valor) %>%
                arrange(desc(Fecha))
            
            } else if(input$edu_pp_corte != "Total") {
                
                req(input$edu_pp_corte, input$indicador_edu_pp, input$fecha_edu_pp)
                
                dat_cut <- dat_edu_pp() %>%
                    filter(corte == input$edu_pp_corte) %>%
                    janitor::remove_empty("cols") 

                dat_cut %>%     
                    filter(ano >= input$fecha_edu_pp[1] &
                               ano <= input$fecha_edu_pp[2]) %>% 
                    select(Fecha, names(dat_cut[,ncol(dat_cut)]), Valor) %>%
                    arrange(desc(Fecha)) %>% 
                    pivot_wider(values_from = "Valor",
                                names_from = names(dat_cut[,ncol(dat_cut)]))
                
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
            paste("resultados-", input$indicador_edu_pp, ".xlsx", sep = "")
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
                pull())
    })
    
    # Selector de fecha
    output$s_edu_r_fecha <- renderUI({
        
        if(input$edu_r_corte == "Departamento") {
            
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
            
        } else if (input$indicador_edu_r == "Porcentaje de estudiantes de tercer año de educación media con bajo desempeño en prueba Aristas (Niveles Bajo 1, Nivel 1, Nivel 2)"){
          
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
            # )
            
        }
    })
    
    # Selector de corte según categoría y data temporal
    dat_edu_r_temp <- reactive({
        
        req(input$edu_r_corte)
        
        dat_edu_r() %>%
            filter(corte == input$edu_r_corte) %>%
            janitor::remove_empty("cols")
        
    })
    
    output$chbox_edu_r <- renderUI({
        
        if(input$edu_r_corte %in% c("Departamento", "Total")) {
            
            return(NULL)
          
        } else if (input$indicador_edu_r == "Porcentaje de estudiantes de tercer año de educación media con bajo desempeño en prueba Aristas (Niveles Bajo 1, Nivel 1, Nivel 2)"){
          
          return(NULL)  
            
        } else if(input$edu_r_corte != "Total") {
            
            checkboxGroupInput(inputId = "checkbox_edu_r",
                               label = "Seleccione categorías",
                               inline = TRUE,
                               choices =  dat_edu_r_temp() %>%
                                   distinct(get(names(dat_edu_r_temp()[,ncol(dat_edu_r_temp())]))) %>%
                                   pull(),
                               selected = dat_edu_r_temp() %>%
                                   distinct(get(names(dat_edu_r_temp()[,ncol(dat_edu_r_temp())]))) %>%
                                   pull() 
            )
        }
        
    })
    
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
        
        if(input$edu_r_corte == "Total") {
            
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
            
        } else if(input$indicador_edu_r == "Porcentaje de estudiantes de tercer año de educación media con bajo desempeño en prueba Aristas (Niveles Bajo 1, Nivel 1, Nivel 2)") {
          
          req(input$edu_r_corte, input$indicador_edu_r)
          
          dat_plot <- dat_edu_r() %>%
            filter(corte == input$edu_r_corte) %>%
            janitor::remove_empty("cols")
          
          plot_edu_corte <- ggplot(dat_plot,
                                   aes_string(x = "fecha", y = "Valor", fill = names(dat_plot[,ncol(dat_plot)]))) +
            geom_col(position = "dodge", width = .7, alpha = .8) +
            theme_bdd(base_size = 12) +
            theme(axis.text.x=element_blank(),
                  legend.position = "bottom") +
            labs(x = "",  y = "",
                 title = wrapit(paste(input$indicador_edu_r,
                                      "según",
                                      tolower(input$edu_r_corte))),
                 caption = wrapit(unique(dat_plot$cita))) +
            scale_fill_brewer(name = "", palette = "Paired") 
          
          print(plot_edu_corte)
          ggsave("www/indicador edu r.png", width = 30, height = 20, units = "cm")
          
            
        } else if(input$edu_r_corte == "Departamento") {
            
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

            dat_plot <- filter(dat_plot,
                               get(names(dat_plot[,ncol(dat_plot)])) %in% input$checkbox_edu_r)
            
            plot_edu_corte <- ggplot(dat_plot,
                                     aes_string(x = "fecha", y = "Valor", colour = names(dat_plot[,ncol(dat_plot)]))) +
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
        
        if(input$edu_r_corte == "Total") {
            
            req(input$edu_r_corte, input$indicador_edu_r, input$fecha_edu_r)
            
            dat_edu_r() %>%
                filter(corte == "Total") %>% 
                filter(ano >= input$fecha_edu_r[1] &
                           ano <= input$fecha_edu_r[2]) %>%
                select(Fecha, Valor) %>%
                arrange(desc(Fecha))
            
        } else if(input$edu_r_corte != "Total") {
            
            req(input$edu_r_corte, input$indicador_edu_r, input$fecha_edu_r)
            
            dat_cut <- dat_edu_r() %>%
                filter(corte == input$edu_r_corte) %>%
                janitor::remove_empty("cols") 
            
            dat_cut %>%     
                filter(ano >= input$fecha_edu_r[1] &
                           ano <= input$fecha_edu_r[2]) %>% 
                select(Fecha, names(dat_cut[,ncol(dat_cut)]), Valor) %>%
                arrange(desc(Fecha)) %>% 
                pivot_wider(values_from = "Valor",
                            names_from = names(dat_cut[,ncol(dat_cut)]))
            
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
            paste("resultados-", input$indicador_edu_r, ".xlsx", sep = "")
        },
        content = function(file) {
            
            openxlsx::write.xlsx(list_edu_r(), file)
            
        }
    )

        
### 4.1. Salud Políticas   ==============================================
    
    # 4.1.1. Data reactiva   ================================================
    
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
                pull())
    })
    
    
    # Selector de fecha
    output$s_salud_pp_fecha <- renderUI({
        
        if(input$salud_pp_corte == "Departamento") {
            
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
            # )
            
        }
    })
    
    # Selector de corte según categoría y data temporal
    dat_salud_pp_temp <- reactive({
        
        req(input$salud_pp_corte)
        
        dat_salud_pp() %>%
            filter(corte == input$salud_pp_corte) %>%
            janitor::remove_empty("cols")
        
    })
    
    output$chbox_salud_pp <- renderUI({
        
        if(input$salud_pp_corte %in% c("Departamento", "Total")) {
            
            return(NULL)
            
        } else if(input$salud_pp_corte != "Total") {
            
            checkboxGroupInput(inputId = "checkbox_salud_pp",
                               label = "Seleccione categorías",
                               inline = TRUE,
                               choices =  dat_salud_pp_temp() %>%
                                   distinct(get(names(dat_salud_pp_temp()[,ncol(dat_salud_pp_temp())]))) %>%
                                   pull(),
                               selected = dat_salud_pp_temp() %>%
                                   distinct(get(names(dat_salud_pp_temp()[,ncol(dat_salud_pp_temp())]))) %>%
                                   pull() 
            )
        }
        
    })
    
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
        
        if(input$salud_pp_corte == "Total") {
            
            req(input$indicador_salud_pp, input$fecha_salud_pp)
            
            dat_plot <- dat_salud_pp() %>% 
                filter(ano >= input$fecha_salud_pp[1] &
                           ano <= input$fecha_salud_pp[2]) %>% 
                filter(corte == "Total")
            
            plot_edu <- ggplot(dat_plot,
                               aes(x = fecha, y = Valor)) +
                geom_line(size = 1, alpha = 0.5, colour = color_defecto) +
                geom_point(size = 3, colour = color_defecto) +
                theme_bdd(base_size = 12) +
                theme(axis.text.x = element_text(angle = 0),
                      legend.position = "bottom") +
                labs(x = "",  y = "",
                     title = wrapit(input$indicador_salud_pp),
                     caption = wrapit(unique(dat_plot$cita))) 
            
            print(plot_edu)
            ggsave("www/indicador salud pp.png", width = 30, height = 20, units = "cm")
            
            
        } else if(input$salud_pp_corte == "Departamento") {
            
            req(input$indicador_salud_pp, input$fecha_dpto_salud_pp)
            
            dat_plot <- dat_salud_pp() %>%
                filter(corte == "Departamento") %>%
                filter(ano == input$fecha_dpto_salud_pp) %>% 
              select(departamento, Valor, fuente, cita)
              
            dep_j <- dep %>%
                left_join(dat_plot, by = c("nombre" = "departamento"))
            
            plot_edu_dpto <- geouy::plot_geouy(dep_j,
                                               "Valor",
                                               viri_opt = "plasma",
                                               l = "n") +
                labs(x = "",  y = "",
                     title = wrapit(paste(input$indicador_salud_pp, 
                                   "en",
                                   input$fecha_dpto_salud_pp), w = 80),
                     caption = wrapit(unique(dat_plot$cita), w = 80)) +
                theme_bdd(base_size = 14)
            
            print(plot_edu_dpto)
            ggsave("www/indicador salud pp.png", width = 30, height = 20, units = "cm")
            
        } else if(input$salud_pp_corte != "Total") {
            
            req(input$salud_pp_corte, input$indicador_salud_pp, 
                input$fecha_salud_pp, input$checkbox_salud_pp)
            
            dat_plot <- dat_salud_pp() %>%
                filter(ano >= input$fecha_salud_pp[1] &
                           ano <= input$fecha_salud_pp[2]) %>%
                filter(corte == input$salud_pp_corte) %>%
                janitor::remove_empty("cols")
            
            dat_plot <- filter(dat_plot,
                               get(names(dat_plot[,ncol(dat_plot)])) %in% input$checkbox_salud_pp)
            
            plot_edu_corte <- ggplot(dat_plot,
                                     aes_string(x = "fecha", y = "Valor", colour = names(dat_plot[,ncol(dat_plot)]))) +
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
            
            print(plot_edu_corte)
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
        
        if(input$salud_pp_corte == "Total") {
            
            req(input$salud_pp_corte, input$indicador_salud_pp, input$fecha_salud_pp)
            
            dat_salud_pp() %>%
                filter(corte == "Total") %>% 
                filter(ano >= input$fecha_salud_pp[1] &
                           ano <= input$fecha_salud_pp[2]) %>%
                select(Fecha, Valor) %>%
                arrange(desc(Fecha))
            
        } else if(input$salud_pp_corte != "Total") {
            
            req(input$salud_pp_corte, input$indicador_salud_pp, input$fecha_salud_pp)
            
            dat_cut <- dat_salud_pp() %>%
                filter(corte == input$salud_pp_corte) %>%
                janitor::remove_empty("cols") 
            
            dat_cut %>%     
                filter(ano >= input$fecha_salud_pp[1] &
                           ano <= input$fecha_salud_pp[2]) %>% 
                select(Fecha, names(dat_cut[,ncol(dat_cut)]), Valor) %>%
                arrange(desc(Fecha)) %>% 
                pivot_wider(values_from = "Valor",
                            names_from = names(dat_cut[,ncol(dat_cut)]))
            
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
            paste("resultados-", input$indicador_salud_pp, ".xlsx", sep = "")
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
                pull())
    })
    
    
    # Selector de fecha
    output$s_salud_r_fecha <- renderUI({
        
        if(input$salud_r_corte == "Departamento" & input$indicador_salud_r != "Distribución porcentual de personas según institución prestadora en la cual declaran tener cobertura vigente") {
            
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
            
        } else if (input$indicador_salud_r == "Porcentaje de usuarios que ha recibido alguna información respecto a sus derechos y obligaciones (promedio por tipo de institución prestadora del SNIS)"){
          
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
            # )
            
        }
    })
    
    # Selector de corte según categoría y data temporal
    dat_salud_r_temp <- reactive({
        
        req(input$salud_r_corte)
        
        dat_salud_r() %>%
            filter(corte == input$salud_r_corte) %>%
            janitor::remove_empty("cols")
        
    })
    
    output$chbox_salud_r <- renderUI({
        
        if(input$indicador_salud_r == "Distribución porcentual de personas según institución prestadora en la cual declaran tener cobertura vigente"){
            
            checkboxGroupInput(inputId = "checkbox_prestador",
                               label = "Seleccione prestadores de salud",
                               inline = TRUE,
                               choices = dat_salud_r() %>%
                                   distinct(prestador) %>%
                                   pull(),
                               selected = c("IAMC", "Público (ASSE)")
            )
          
        } else if (input$indicador_salud_r == "Porcentaje de usuarios que ha recibido alguna información respecto a sus derechos y obligaciones (promedio por tipo de institución prestadora del SNIS)"){
            
          return(NULL)
          
        } else if(input$salud_r_corte %in% c("Departamento", "Total")) {
            
            return(NULL)
            
        } else if(input$salud_r_corte != "Total") {
            
            checkboxGroupInput(inputId = "checkbox_salud_r",
                               label = "Seleccione categorías",
                               inline = TRUE,
                               choices =  dat_salud_r_temp() %>%
                                   distinct(get(names(dat_salud_r_temp()[,ncol(dat_salud_r_temp())]))) %>%
                                   pull(),
                               selected = dat_salud_r_temp() %>%
                                   distinct(get(names(dat_salud_r_temp()[,ncol(dat_salud_r_temp())]))) %>%
                                   pull() 
            )
        }
        
    })
    
    
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
        
        if(input$salud_r_corte == "Total") {
            
            req(input$indicador_salud_r, input$fecha_salud_r)
            
            dat_plot <- dat_salud_r() %>% 
                filter(ano >= input$fecha_salud_r[1] &
                           ano <= input$fecha_salud_r[2]) %>% 
                filter(corte == "Total")
            
            plot_edu <- ggplot(dat_plot,
                               aes(x = fecha, y = Valor)) +
                geom_line(size = 1, alpha = 0.5, colour = color_defecto) +
                geom_point(size = 3, colour = color_defecto) +
                theme_bdd(base_size = 12) +
                theme(axis.text.x = element_text(angle = 0),
                      legend.position = "bottom") +
                labs(x = "",  y = "",
                     title = wrapit(input$indicador_salud_r),
                     caption = wrapit(unique(dat_plot$cita))) 
            
            print(plot_edu)
            ggsave("www/indicador salud r.png", width = 30, height = 20, units = "cm")
            
            # Indicador especial (tiene solo una fecha entonces va con barras)
        } else if(input$indicador_salud_r == "Porcentaje de usuarios que ha recibido alguna información respecto a sus derechos y obligaciones (promedio por tipo de institución prestadora del SNIS)") {

          req(input$salud_r_corte, input$indicador_salud_r)
          
          dat_plot <- dat_salud_r() %>%
            filter(corte == input$salud_r_corte) %>%
            janitor::remove_empty("cols")
          
          plot_edu_corte <- ggplot(dat_plot,
                                   aes_string(x = "fecha", y = "Valor", fill = names(dat_plot[,ncol(dat_plot)]))) +
            geom_col(position = "dodge", width = .7, alpha = .8) +
            theme_bdd(base_size = 12) +
            theme(axis.text.x=element_blank(),
                  legend.position = "bottom") +
            labs(x = "",  y = "",
                 title = wrapit(paste(input$indicador_salud_r,
                                      "según",
                                      tolower(input$salud_r_corte))),
                 caption = wrapit(unique(dat_plot$cita))) +
            scale_fill_brewer(name = "", palette = "Paired") 
          
          print(plot_edu_corte)
          ggsave("www/indicador salud r.png", width = 30, height = 20, units = "cm")
          
          # Indicador especial (por prestador tiene dos cortes)
        } else if(input$indicador_salud_r == "Distribución porcentual de personas según institución prestadora en la cual declaran tener cobertura vigente" & 
                  input$salud_r_corte %notin% c("Prestador")) {
            
            req(input$salud_r_corte, input$indicador_salud_r, 
                input$fecha_salud_r, input$checkbox_prestador)
            
            dat_plot <- dat_salud_r() %>%
                filter(ano >= input$fecha_salud_r[1] &
                           ano <= input$fecha_salud_r[2]) %>%
                filter(corte == input$salud_r_corte) %>%
                janitor::remove_empty("cols") %>% 
                filter(prestador %in% input$checkbox_prestador)
            
            plot_edu_corte <- ggplot(dat_plot,
                                     aes_string(x = "fecha", y = "Valor", colour = names(dat_plot[,ncol(dat_plot)]))) +
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
                scale_colour_brewer(palette = "Dark2") +
                facet_wrap(as.formula(paste("~", names(dat_plot[,ncol(dat_plot)-1]))))
            
            print(plot_edu_corte)
            ggsave("www/indicador salud r.png", width = 30, height = 20, units = "cm")
            
        } else if(input$indicador_salud_r == "Distribución porcentual de personas según institución prestadora en la cual declaran tener cobertura vigente" & 
                  input$salud_r_corte == "Prestador") {
            
            req(input$salud_r_corte, input$indicador_salud_r, 
                input$fecha_salud_r, input$checkbox_prestador)
            
            dat_plot <- dat_salud_r() %>%
                filter(ano >= input$fecha_salud_r[1] &
                           ano <= input$fecha_salud_r[2]) %>%
                filter(corte == input$salud_r_corte) %>%
                janitor::remove_empty("cols") %>% 
                filter(prestador %in% input$checkbox_prestador)
            
            plot_edu_corte <- ggplot(dat_plot,
                                     aes_string(x = "fecha", y = "Valor", colour = names(dat_plot[,ncol(dat_plot)]))) +
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
            
            print(plot_edu_corte)
            ggsave("www/indicador salud r.png", width = 30, height = 20, units = "cm")
            
        } else if(input$salud_r_corte == "Departamento" & 
                  input$indicador_salud_r != "Distribución porcentual de personas según institución prestadora en la cual declaran tener cobertura vigente") {
                                              
            req(input$indicador_salud_r, input$fecha_dpto_salud_r)
            
            dat_plot <- dat_salud_r() %>%
                filter(corte == "Departamento") %>%
                filter(ano == input$fecha_dpto_salud_r) %>% 
                select(departamento, Valor, fuente, cita)
            
            dep_j <- dep %>%
                left_join(dat_plot, by = c("nombre" = "departamento"))
            
            plot_edu_dpto <- geouy::plot_geouy(dep_j,
                                               "Valor",
                                               viri_opt = "plasma",
                                               l = "n") +
                labs(x = "",  y = "",
                     title = wrapit(paste(input$indicador_salud_r, 
                                   "en",
                                   input$fecha_dpto_salud_r), w = 80),
                     caption = wrapit(unique(dat_plot$cita), w = 80)) +
                theme_bdd(base_size = 14)
            
            print(plot_edu_dpto)
            ggsave("www/indicador salud r.png", width = 30, height = 20, units = "cm")
            
            
        } else if(input$salud_r_corte != "Total") {
            
            req(input$salud_r_corte, input$indicador_salud_r, 
                input$fecha_salud_r, input$checkbox_salud_r)
            
            dat_plot <- dat_salud_r() %>%
                filter(ano >= input$fecha_salud_r[1] &
                           ano <= input$fecha_salud_r[2]) %>%
                filter(corte == input$salud_r_corte) %>%
                janitor::remove_empty("cols")
            
            dat_plot <- filter(dat_plot,
                               get(names(dat_plot[,ncol(dat_plot)])) %in% input$checkbox_salud_r)
            
            
            plot_edu_corte <- ggplot(dat_plot,
                                     aes_string(x = "fecha", y = "Valor", colour = names(dat_plot[,ncol(dat_plot)]))) +
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
            
            print(plot_edu_corte)
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
        
        if(input$salud_r_corte == "Total") {
            
            req(input$salud_r_corte, input$indicador_salud_r, input$fecha_salud_r)
            
            dat_salud_r() %>%
                filter(corte == "Total") %>% 
                filter(ano >= input$fecha_salud_r[1] &
                           ano <= input$fecha_salud_r[2]) %>%
                select(Fecha, Valor) %>%
                arrange(desc(Fecha))
            
        } else if(input$indicador_salud_r == "Distribución porcentual de personas según institución prestadora en la cual declaran tener cobertura vigente" &
                  input$salud_r_corte == "Prestador") {
            
            req(input$salud_r_corte, input$indicador_salud_r, input$fecha_salud_r)
            
            dat_cut <- dat_salud_r() %>%
                filter(corte == input$salud_r_corte) %>%
                janitor::remove_empty("cols") %>% 
                filter(prestador %in% input$checkbox_prestador)
            
            dat_cut %>%
                filter(ano >= input$fecha_salud_r[1] &
                           ano <= input$fecha_salud_r[2]) %>%
                select(Fecha, names(dat_cut[,ncol(dat_cut)]), Valor) %>%
                arrange(desc(Fecha)) %>%
                pivot_wider(values_from = "Valor",
                            names_from = names(dat_cut[,ncol(dat_cut)]))
            
        } else if(input$indicador_salud_r == "Distribución porcentual de personas según institución prestadora en la cual declaran tener cobertura vigente" &
                  input$salud_r_corte %notin% c("Prestador")) {
            
            req(input$salud_r_corte, input$indicador_salud_r, input$fecha_salud_r)
            
            dat_cut <- dat_salud_r() %>%
                filter(corte == input$salud_r_corte) %>%
                janitor::remove_empty("cols") %>% 
                filter(prestador %in% input$checkbox_prestador)
            
            dat_cut %>%
                filter(ano >= input$fecha_salud_r[1] &
                           ano <= input$fecha_salud_r[2]) %>%
                select(Fecha, names(dat_cut[,ncol(dat_cut)]), names(dat_cut[,ncol(dat_cut)-1]), Valor) %>%
                arrange(desc(Fecha)) %>%
                pivot_wider(values_from = "Valor",
                            names_from = names(dat_cut[,ncol(dat_cut)]))
        } else if(input$salud_r_corte != "Total") {
            
            req(input$salud_r_corte, input$indicador_salud_r, input$fecha_salud_r)
            
            dat_cut <- dat_salud_r() %>%
                filter(corte == input$salud_r_corte) %>%
                janitor::remove_empty("cols") 
            
            dat_cut %>%     
                filter(ano >= input$fecha_salud_r[1] &
                           ano <= input$fecha_salud_r[2]) %>% 
                select(Fecha, names(dat_cut[,ncol(dat_cut)]), Valor) %>%
                arrange(desc(Fecha)) %>% 
                pivot_wider(values_from = "Valor",
                            names_from = names(dat_cut[,ncol(dat_cut)]))
            
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
            paste("resultados-", input$indicador_salud_r, ".xlsx", sep = "")
        },
        content = function(file) {
            
            openxlsx::write.xlsx(list_salud_r(), file)
            
        }
    )
    
### 5.1. Seguridad Social Políticas   ==============================================
    
    # 5.1.1. Data reactiva   ================================================
    
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
                pull())
    })
    
    
    # Selector de fecha
    output$s_ssocial_pp_fecha <- renderUI({
        
        if(input$ssocial_pp_corte == "Departamento") {
            
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
            # )
            
        }
    })
    
    # Selector de corte según categoría y data temporal
    dat_ssocial_pp_temp <- reactive({
        
        req(input$ssocial_pp_corte)
        
        dat_ssocial_pp() %>%
            filter(corte == input$ssocial_pp_corte) %>%
            janitor::remove_empty("cols")
        
    })
    
    output$chbox_ssocial_pp <- renderUI({
        
        if(input$ssocial_pp_corte %in% c("Departamento", "Total")) {
            
            return(NULL)
            
        } else if(input$ssocial_pp_corte != "Total") {
            
            checkboxGroupInput(inputId = "checkbox_ssocial_pp",
                               label = "Seleccione categorías",
                               inline = TRUE,
                               choices =  dat_ssocial_pp_temp() %>%
                                   distinct(get(names(dat_ssocial_pp_temp()[,ncol(dat_ssocial_pp_temp())]))) %>%
                                   pull(),
                               selected = dat_ssocial_pp_temp() %>%
                                   distinct(get(names(dat_ssocial_pp_temp()[,ncol(dat_ssocial_pp_temp())]))) %>%
                                   pull() 
            )
        }
        
    })
    
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
        
        if(input$ssocial_pp_corte == "Total") {
            
            req(input$indicador_ssocial_pp, input$fecha_ssocial_pp)
            
            dat_plot <- dat_ssocial_pp() %>% 
                filter(ano >= input$fecha_ssocial_pp[1] &
                           ano <= input$fecha_ssocial_pp[2]) %>% 
                filter(corte == "Total")
            
            plot_edu <- ggplot(dat_plot,
                               aes(x = fecha, y = Valor)) +
                geom_line(size = 1, alpha = 0.5, colour = color_defecto) +
                geom_point(size = 3, colour = color_defecto) +
                theme_bdd(base_size = 12) +
                theme(axis.text.x = element_text(angle = 0),
                      legend.position = "bottom") +
                labs(x = "",  y = "",
                     title = wrapit(input$indicador_ssocial_pp),
                     caption = wrapit(unique(dat_plot$cita))) 
            
            print(plot_edu)
            ggsave("www/indicador seguridad social pp.png", width = 30, height = 20, units = "cm")
            
            
        } else if(input$ssocial_pp_corte == "Departamento") {
            
            req(input$indicador_ssocial_pp, input$fecha_dpto_ssocial_pp)
            
            dat_plot <- dat_ssocial_pp() %>%
                filter(corte == "Departamento") %>%
                filter(ano == input$fecha_dpto_ssocial_pp) %>% 
                select(departamento, Valor, fuente, cita)
            
            dep_j <- dep %>%
                left_join(dat_plot, by = c("nombre" = "departamento"))
            
            plot_edu_dpto <- geouy::plot_geouy(dep_j,
                                               "Valor",
                                               viri_opt = "plasma",
                                               l = "n") +
                labs(x = "",  y = "",
                     title = wrapit(paste(input$indicador_ssocial_pp, 
                                   "en",
                                   input$fecha_dpto_ssocial_pp), w = 80),
                     caption = wrapit(unique(dat_plot$cita), w = 80)) +
                theme_bdd(base_size = 14)
            
            print(plot_edu_dpto)
            ggsave("www/indicador seguridad social pp.png", width = 30, height = 20, units = "cm")
            
        } else if(input$ssocial_pp_corte != "Total") {
            
            req(input$ssocial_pp_corte, input$indicador_ssocial_pp,
                input$fecha_ssocial_pp, input$checkbox_ssocial_pp)
            
            dat_plot <- dat_ssocial_pp() %>%
                filter(ano >= input$fecha_ssocial_pp[1] &
                           ano <= input$fecha_ssocial_pp[2]) %>%
                filter(corte == input$ssocial_pp_corte) %>%
                janitor::remove_empty("cols")
            
            dat_plot <- filter(dat_plot,
                               get(names(dat_plot[,ncol(dat_plot)])) %in% input$checkbox_ssocial_pp)
            
            plot_edu_corte <- ggplot(dat_plot,
                                     aes_string(x = "fecha", y = "Valor", colour = names(dat_plot[,ncol(dat_plot)]))) +
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
            
            print(plot_edu_corte)
            ggsave("www/indicador seguridad social pp.png", width = 30, height = 20, units = "cm")
            
        }
        
    })
    
    # 5.1.4. Descarga gráficos   =============================================
    
    output$baja_p_ssocial_pp <- downloadHandler(
        filename <- function() {
            paste("indicador seguridad social pp", "png", sep = ".")
        },
        
        content <- function(file) {
            file.copy("www/indicador seguridad social pp.png", file)
        },
        contentType = "www/indicador seguridad social pp"
    )
    
    
    # 5.1.5. Tablas   ========================================================
    
    # Data
    ssocial_pp_tab <- reactive({
        
        if(input$ssocial_pp_corte == "Total") {
            
            req(input$ssocial_pp_corte, input$indicador_ssocial_pp, input$fecha_ssocial_pp)
            
            dat_ssocial_pp() %>%
                filter(corte == "Total") %>% 
                filter(ano >= input$fecha_ssocial_pp[1] &
                           ano <= input$fecha_ssocial_pp[2]) %>%
                select(Fecha, Valor) %>%
                arrange(desc(Fecha))
            
        } else if(input$ssocial_pp_corte != "Total") {
            
            req(input$ssocial_pp_corte, input$indicador_ssocial_pp, input$fecha_ssocial_pp)
            
            dat_cut <- dat_ssocial_pp() %>%
                filter(corte == input$ssocial_pp_corte) %>%
                janitor::remove_empty("cols") 
            
            dat_cut %>%     
                filter(ano >= input$fecha_ssocial_pp[1] &
                           ano <= input$fecha_ssocial_pp[2]) %>% 
                select(Fecha, names(dat_cut[,ncol(dat_cut)]), Valor) %>%
                arrange(desc(Fecha)) %>% 
                pivot_wider(values_from = "Valor",
                            names_from = names(dat_cut[,ncol(dat_cut)]))
            
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
    
    
### 5.2. Seguridad Social Resultados   =============================================
    
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
                pull())
    })
    
    
    # Selector de fecha
    output$s_ssocial_r_fecha <- renderUI({
        
        if(input$ssocial_r_corte == "Departamento") {
            
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
            # )
            
        }
    })
    
    
    # Selector de corte según categoría y data temporal
    dat_ssocial_r_temp <- reactive({
        
        req(input$ssocial_r_corte)
        
        dat_ssocial_r() %>%
            filter(corte == input$ssocial_r_corte) %>%
            janitor::remove_empty("cols")
        
    })
    
    output$chbox_ssocial_r <- renderUI({
        
        if(input$ssocial_r_corte %in% c("Departamento", "Total")) {
            
            return(NULL)
            
        } else if(input$ssocial_r_corte != "Total") {
            
            checkboxGroupInput(inputId = "checkbox_ssocial_r",
                               label = "Seleccione categorías",
                               inline = TRUE,
                               choices =  dat_ssocial_r_temp() %>%
                                   distinct(get(names(dat_ssocial_r_temp()[,ncol(dat_ssocial_r_temp())]))) %>%
                                   pull(),
                               selected = dat_ssocial_r_temp() %>%
                                   distinct(get(names(dat_ssocial_r_temp()[,ncol(dat_ssocial_r_temp())]))) %>%
                                   pull() 
            )
        }
        
    })
    
    
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
        
        if(input$ssocial_r_corte == "Total") {
            
            req(input$indicador_ssocial_r, input$fecha_ssocial_r)
            
            dat_plot <- dat_ssocial_r() %>% 
                filter(ano >= input$fecha_ssocial_r[1] &
                           ano <= input$fecha_ssocial_r[2]) %>% 
                filter(corte == "Total")
            
            plot_edu <- ggplot(dat_plot,
                               aes(x = fecha, y = Valor)) +
                geom_line(size = 1, alpha = 0.5, colour = color_defecto) +
                geom_point(size = 3, colour = color_defecto) +
                theme_bdd(base_size = 12) +
                theme(axis.text.x = element_text(angle = 0),
                      legend.position = "bottom") +
                labs(x = "",  y = "",
                     title = wrapit(input$indicador_ssocial_r),
                     caption = wrapit(unique(dat_plot$cita))) 
            
            print(plot_edu)
            ggsave("www/indicador seguridad social r.png", width = 30, height = 20, units = "cm")
            
            
        } else if(input$ssocial_r_corte == "Departamento") {
            
            req(input$indicador_ssocial_r, input$fecha_dpto_ssocial_r)
            
            dat_plot <- dat_ssocial_r() %>%
                filter(corte == "Departamento") %>%
                filter(ano == input$fecha_dpto_ssocial_r) %>% 
                select(departamento, Valor, fuente, cita)
            
            dep_j <- dep %>%
                left_join(dat_plot, by = c("nombre" = "departamento"))
            
            plot_edu_dpto <- geouy::plot_geouy(dep_j,
                                               "Valor",
                                               viri_opt = "plasma",
                                               l = "n") +
                labs(x = "",  y = "",
                     title = wrapit(paste(input$indicador_ssocial_r, 
                                   "en",
                                   input$fecha_dpto_ssocial_r),w = 80),
                     caption = wrapit(unique(dat_plot$cita), w = 80)) +
                theme_bdd(base_size = 14)
            
            print(plot_edu_dpto)
            ggsave("www/indicador seguridad social r.png", width = 30, height = 20, units = "cm")
            
        } else if(input$ssocial_r_corte != "Total") {
            
            req(input$ssocial_r_corte, input$indicador_ssocial_r, 
                input$fecha_ssocial_r, input$checkbox_ssocial_r)
            
            dat_plot <- dat_ssocial_r() %>%
                filter(ano >= input$fecha_ssocial_r[1] &
                           ano <= input$fecha_ssocial_r[2]) %>%
                filter(corte == input$ssocial_r_corte) %>%
                janitor::remove_empty("cols")
            
            dat_plot <- filter(dat_plot,
                               get(names(dat_plot[,ncol(dat_plot)])) %in% input$checkbox_ssocial_r)
            
            plot_edu_corte <- ggplot(dat_plot,
                                     aes_string(x = "fecha", y = "Valor", colour = names(dat_plot[,ncol(dat_plot)]))) +
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
            
            print(plot_edu_corte)
            ggsave("www/indicador seguridad social r.png", width = 30, height = 20, units = "cm")
            
        }
        
    })
    
    # 5.2.4. Descarga gráficos   =============================================
    
    output$baja_p_ssocial_r <- downloadHandler(
        filename <- function() {
            paste("indicador seguridad social r", "png", sep = ".")
        },
        
        content <- function(file) {
            file.copy("www/indicador seguridad social r.png", file)
        },
        contentType = "www/indicador seguridad social r"
    )
    
    
    # 5.2.5. Tablas   ========================================================
    
    # Data
    ssocial_r_tab <- reactive({
        
        if(input$ssocial_r_corte == "Total") {
            
            req(input$ssocial_r_corte, input$indicador_ssocial_r, input$fecha_ssocial_r)
            
            dat_ssocial_r() %>%
                filter(corte == "Total") %>% 
                filter(ano >= input$fecha_ssocial_r[1] &
                           ano <= input$fecha_ssocial_r[2]) %>%
                select(Fecha, Valor) %>%
                arrange(desc(Fecha))
            
        } else if(input$ssocial_r_corte != "Total") {
            
            req(input$ssocial_r_corte, input$indicador_ssocial_r, input$fecha_ssocial_r)
            
            dat_cut <- dat_ssocial_r() %>%
                filter(corte == input$ssocial_r_corte) %>%
                janitor::remove_empty("cols") 
            
            dat_cut %>%     
                filter(ano >= input$fecha_ssocial_r[1] &
                           ano <= input$fecha_ssocial_r[2]) %>% 
                select(Fecha, names(dat_cut[,ncol(dat_cut)]), Valor) %>%
                arrange(desc(Fecha)) %>% 
                pivot_wider(values_from = "Valor",
                            names_from = names(dat_cut[,ncol(dat_cut)]))
            
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
            paste("resultados-", input$indicador_ssocial_r, ".xlsx", sep = "")
        },
        content = function(file) {
            
            openxlsx::write.xlsx(list_ssocial_r(), file)
            
        }
    )
    
    
### 6.1. Vivienda Políticas   ===============================================
    
    # 6.1.1. Data reactiva   ================================================
    
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
                pull())
    })
    
    
    # Selector de fecha
    output$s_vivienda_pp_fecha <- renderUI({
        
        if(input$vivienda_pp_corte == "Departamento") {
            
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
            # )
            
        }
    })
    
    
    # Selector de corte según categoría y data temporal
    dat_vivienda_pp_temp <- reactive({
        
        req(input$vivienda_pp_corte)
        
        dat_vivienda_pp() %>%
            filter(corte == input$vivienda_pp_corte) %>%
            janitor::remove_empty("cols")
        
    })
    
    output$chbox_vivienda_pp <- renderUI({
        
        if(input$vivienda_pp_corte %in% c("Departamento", "Total")) {
            
            return(NULL)
            
        } else if(input$vivienda_pp_corte != "Total") {
            
            checkboxGroupInput(inputId = "checkbox_vivienda_pp",
                               label = "Seleccione categorías",
                               inline = TRUE,
                               choices =  dat_vivienda_pp_temp() %>%
                                   distinct(get(names(dat_vivienda_pp_temp()[,ncol(dat_vivienda_pp_temp())]))) %>%
                                   pull(),
                               selected = dat_vivienda_pp_temp() %>%
                                   distinct(get(names(dat_vivienda_pp_temp()[,ncol(dat_vivienda_pp_temp())]))) %>%
                                   pull() 
            )
        }
        
    })
    
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
        
        if(input$vivienda_pp_corte == "Total") {
            
            req(input$indicador_vivienda_pp, input$fecha_vivienda_pp)
            
            dat_plot <- dat_vivienda_pp() %>% 
                filter(ano >= input$fecha_vivienda_pp[1] &
                           ano <= input$fecha_vivienda_pp[2]) %>% 
                filter(corte == "Total")
            
            plot_edu <- ggplot(dat_plot,
                               aes(x = fecha, y = Valor)) +
                geom_line(size = 1, alpha = 0.5, colour = color_defecto) +
                geom_point(size = 3, colour = color_defecto) +
                theme_bdd(base_size = 12) +
                theme(axis.text.x = element_text(angle = 0),
                      legend.position = "bottom") +
                labs(x = "",  y = "",
                     title = wrapit(input$indicador_vivienda_pp),
                     caption = wrapit(unique(dat_plot$cita))) 
            
            print(plot_edu)
            ggsave("www/indicador vivienda pp.png", width = 30, height = 20, units = "cm")
            
            
        } else if(input$vivienda_pp_corte == "Departamento") {
            
            req(input$indicador_vivienda_pp, input$fecha_dpto_vivienda_pp)
            
            dat_plot <- dat_vivienda_pp() %>%
                filter(corte == "Departamento") %>%
                filter(ano == input$fecha_dpto_vivienda_pp) %>% 
                select(departamento, Valor, fuente, cita)
            
            dep_j <- dep %>%
                left_join(dat_plot, by = c("nombre" = "departamento"))
            
            plot_edu_dpto <- geouy::plot_geouy(dep_j,
                                               "Valor",
                                               viri_opt = "plasma",
                                               l = "n") +
                labs(x = "",  y = "",
                     title = wrapit(paste(input$indicador_vivienda_pp, 
                                   "en",
                                   input$fecha_dpto_vivienda_pp), w = 80),
                     caption = wrapit(unique(dat_plot$cita), w = 80)) +
                theme_bdd(base_size = 14)
            
            print(plot_edu_dpto)
            ggsave("www/indicador vivienda pp.png", width = 30, height = 20, units = "cm")
            
        } else if(input$vivienda_pp_corte != "Total") {
            
            req(input$vivienda_pp_corte, input$indicador_vivienda_pp, 
                input$fecha_vivienda_pp, input$checkbox_vivienda_pp)
            
            dat_plot <- dat_vivienda_pp() %>%
                filter(ano >= input$fecha_vivienda_pp[1] &
                           ano <= input$fecha_vivienda_pp[2]) %>%
                filter(corte == input$vivienda_pp_corte) %>%
                janitor::remove_empty("cols")
            
            dat_plot <- filter(dat_plot,
                               get(names(dat_plot[,ncol(dat_plot)])) %in% input$checkbox_vivienda_pp)
            
            plot_edu_corte <- ggplot(dat_plot,
                                     aes_string(x = "fecha", y = "Valor", colour = names(dat_plot[,ncol(dat_plot)]))) +
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
            
            print(plot_edu_corte)
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
        
        if(input$vivienda_pp_corte == "Total") {
            
            req(input$vivienda_pp_corte, input$indicador_vivienda_pp, input$fecha_vivienda_pp)
            
            dat_vivienda_pp() %>%
                filter(corte == "Total") %>% 
                filter(ano >= input$fecha_vivienda_pp[1] &
                           ano <= input$fecha_vivienda_pp[2]) %>%
                select(Fecha, Valor) %>%
                arrange(desc(Fecha))
            
        } else if(input$vivienda_pp_corte != "Total") {
            
            req(input$vivienda_pp_corte, input$indicador_vivienda_pp, input$fecha_vivienda_pp)
            
            dat_cut <- dat_vivienda_pp() %>%
                filter(corte == input$vivienda_pp_corte) %>%
                janitor::remove_empty("cols") 
            
            dat_cut %>%     
                filter(ano >= input$fecha_vivienda_pp[1] &
                           ano <= input$fecha_vivienda_pp[2]) %>% 
                select(Fecha, names(dat_cut[,ncol(dat_cut)]), Valor) %>%
                arrange(desc(Fecha)) %>% 
                pivot_wider(values_from = "Valor",
                            names_from = names(dat_cut[,ncol(dat_cut)]))
            
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
            paste("resultados-", input$indicador_vivienda_pp, ".xlsx", sep = "")
        },
        content = function(file) {
            
            openxlsx::write.xlsx(list_vivienda_pp(), file)
            
        }
    )
    
    
    ### 6.2. Salud Resultados   =============================================
    
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
                pull())
    })
    
    
    # Selector de fecha
    output$s_vivienda_r_fecha <- renderUI({
        
        if(input$vivienda_r_corte == "Departamento") {
            
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
            # )
            
        }
    })
    
    # Selector de corte según categoría y data temporal
    dat_vivienda_r_temp <- reactive({
        
        req(input$vivienda_r_corte)
        
        dat_vivienda_r() %>%
            filter(corte == input$vivienda_r_corte) %>%
            janitor::remove_empty("cols")
        
    })
    
    output$chbox_vivienda_r <- renderUI({
        
        if(input$vivienda_r_corte %in% c("Departamento", "Total")) {
            
            return(NULL)
            
        } else if(input$vivienda_r_corte != "Total") {
            
            checkboxGroupInput(inputId = "checkbox_vivienda_r",
                               label = "Seleccione categorías",
                               inline = TRUE,
                               choices =  dat_vivienda_r_temp() %>%
                                   distinct(get(names(dat_vivienda_r_temp()[,ncol(dat_vivienda_r_temp())]))) %>%
                                   pull(),
                               selected = dat_vivienda_r_temp() %>%
                                   distinct(get(names(dat_vivienda_r_temp()[,ncol(dat_vivienda_r_temp())]))) %>%
                                   pull() 
            )
        }
        
    })
    
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
        
        if(input$vivienda_r_corte == "Total") {
            
            req(input$indicador_vivienda_r, input$fecha_vivienda_r)
            
            dat_plot <- dat_vivienda_r() %>% 
                filter(ano >= input$fecha_vivienda_r[1] &
                           ano <= input$fecha_vivienda_r[2]) %>% 
                filter(corte == "Total")
            
            plot_edu <- ggplot(dat_plot,
                               aes(x = fecha, y = Valor)) +
                geom_line(size = 1, alpha = 0.5, colour = color_defecto) +
                geom_point(size = 3, colour = color_defecto) +
                theme_bdd(base_size = 12) +
                theme(axis.text.x = element_text(angle = 0),
                      legend.position = "bottom") +
                labs(x = "",  y = "",
                     title = wrapit(input$indicador_vivienda_r),
                     caption = wrapit(unique(dat_plot$cita))) 
            
            print(plot_edu)
            ggsave("www/indicador vivienda r.png", width = 30, height = 20, units = "cm")
            
            
        } else if(input$vivienda_r_corte == "Departamento") {
            
            req(input$indicador_vivienda_r, input$fecha_dpto_vivienda_r)
            
            dat_plot <- dat_vivienda_r() %>%
                filter(corte == "Departamento") %>%
                filter(ano == input$fecha_dpto_vivienda_r) %>% 
               select(departamento, Valor, fuente, cita)
            
            dep_j <- dep %>%
                left_join(dat_plot, by = c("nombre" = "departamento"))
            
            plot_edu_dpto <- geouy::plot_geouy(dep_j,
                                               "Valor",
                                               viri_opt = "plasma",
                                               l = "n") +
                labs(x = "",  y = "",
                     title = wrapit(paste(input$indicador_vivienda_r, 
                                   "en",
                                   input$fecha_dpto_vivienda_r), w = 80),
                     caption = wrapit(unique(dat_plot$cita), w = 80)) +
                theme_bdd(base_size = 14)
            
            print(plot_edu_dpto)
            ggsave("www/indicador vivienda r.png", width = 30, height = 20, units = "cm")
            
        } else if(input$vivienda_r_corte != "Total") {
            
            req(input$vivienda_r_corte, input$indicador_vivienda_r,
                input$fecha_vivienda_r, input$checkbox_vivienda_r)
            
            dat_plot <- dat_vivienda_r() %>%
                filter(ano >= input$fecha_vivienda_r[1] &
                           ano <= input$fecha_vivienda_r[2]) %>%
                filter(corte == input$vivienda_r_corte) %>%
                janitor::remove_empty("cols")
            
            dat_plot <- filter(dat_plot,
                               get(names(dat_plot[,ncol(dat_plot)])) %in% input$checkbox_vivienda_r)
            
            plot_edu_corte <- ggplot(dat_plot,
                                     aes_string(x = "fecha", y = "Valor", colour = names(dat_plot[,ncol(dat_plot)]))) +
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
            
            print(plot_edu_corte)
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
        
        if(input$vivienda_r_corte == "Total") {
            
            req(input$vivienda_r_corte, input$indicador_vivienda_r, input$fecha_vivienda_r)
            
            dat_vivienda_r() %>%
                filter(corte == "Total") %>% 
                filter(ano >= input$fecha_vivienda_r[1] &
                           ano <= input$fecha_vivienda_r[2]) %>%
                select(Fecha, Valor) %>%
                arrange(desc(Fecha))
            
        } else if(input$vivienda_r_corte != "Total") {
            
            req(input$vivienda_r_corte, input$indicador_vivienda_r, input$fecha_vivienda_r)
            
            dat_cut <- dat_vivienda_r() %>%
                filter(corte == input$vivienda_r_corte) %>%
                janitor::remove_empty("cols") 
            
            dat_cut %>%     
                filter(ano >= input$fecha_vivienda_r[1] &
                           ano <= input$fecha_vivienda_r[2]) %>% 
                select(Fecha, names(dat_cut[,ncol(dat_cut)]), Valor) %>%
                arrange(desc(Fecha)) %>% 
                pivot_wider(values_from = "Valor",
                            names_from = names(dat_cut[,ncol(dat_cut)]))
            
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
}

# Run the application
shinyApp(ui = ui, server = server)
