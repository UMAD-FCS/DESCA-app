set.seed(1989)

import_titillium_web <- function() {
  
  tw_font_dir <- system.file("fonts", "titillium-web", package = "hrbrthemes")
  
  suppressWarnings(suppressMessages(extrafont::font_import(tw_font_dir, prompt = FALSE)))
  
  message(
    sprintf(
      "You will likely need to install these fonts on your system as well.\n\nYou can find them in [%s]",
      tw_font_dir)
  )
  
}

# theme_bdd

theme_bdd <- function(
  base_family = "Poppins", 
  base_size = 14,
  plot_title_family = "Poppins",
  plot_title_size = 16,
  plot_title_face = "bold",
  plot_title_margin = 12,
  subtitle_family = "Poppins",
  subtitle_size = 13,
  subtitle_face = "plain",
  subtitle_margin = 15,
  strip_text_family = base_family,
  strip_text_size = 12,
  strip_text_face = "plain",
  caption_family = "Poppins",
  caption_size = 10,
  caption_face = "plain", 
  caption_margin = 10,
  axis_text_size = base_size,
  axis_title_family = base_family,
  axis_title_size = 10,
  axis_title_face = "plain",
  axis_title_just = "rt",
  plot_margin = margin(30, 30, 30, 30)) {
  
  ggplot2::theme_minimal(base_family = base_family, base_size = base_size)

}

blue_bdd <- "#0075BE"
blue_bdd2 <- "#3290cb"
blue_bdd3 <- "#4c9ed1"

theme_ggparliament_bdd <- function(legend = TRUE,
                                   background_colour = FALSE,
                                   border = FALSE) {
  basic_theme <- theme_void(base_family = "Titillium Web", base_size = 12)
  
  
  if (legend == TRUE) {
    basic_theme <- basic_theme + ggplot2::theme(legend.position = "bottom")
  } else {
    basic_theme <- basic_theme + ggplot2::theme(legend.position = "none")
  }
  
  
  
  if (!background_colour) {
    basic_theme <- basic_theme
  } else {
    basic_theme <- basic_theme + 
      ggplot2::theme(panel.background = ggplot2::element_rect(fill = "#F5F5F5", colour = NA)) # white smoke fill
  }
  
  
  if (!border) {
    basic_theme <- basic_theme
  } else {
    basic_theme <- basic_theme + 
      ggplot2::theme(panel.border = ggplot2::element_rect(colour = "#F5F5F5", fill = NA)) # white smoke colour
  }
  
  basic_theme
}  


coloriza_partidos <- function(vector) {
  levs <- if(is.factor(vector)) 
    levels(vector) 
  else 
    levels(factor(vector))
  
  predefinidos <- c("FA", "PN", "PC", "CA", "PI", "UP", "Cifra", "Equipos", 
                    "Factum", "Interconsult", "Opcion", "Radar", "OtrosP.", "VB/VA",
                    "Frente Amplio", "Partido Nacional", "Partido Colorado", "Cabildo Abierto", "Partido Independiente", "Unidad Popular",
                    "Aprueba", "Desaprueba", "Ni aprueba ni desaprueba", "NS/NC", "Saldo",
                    "Sanguinetti 2", "Lacalle", "Batlle", "Vazquez 1", "Mujica", "Vazquez 2", "Lacalle Pou")
  
  pal <- c("#013197", "#99ccff", "#BA0200", "#F8BC1E", "#663399", "#00913C", "#4C9ED1", 
           "#5C8DCC", "#737AC0", "#8865AC", "#984E91", "#9F3770", "grey35", "grey75",
           "#013197", "#99ccff", "#BA0200", "#F8BC1E", "#663399", "#00913C",
           "#4c9ed1", "#BA0200", "#808080", "#D3D3D3", "#4c9ed1",
           "#BA0200", "#4c9ed1", "#BA0200", "#013197", "#013197", "#013197", "#4c9ed1")
  
  pal <- pal[match(levs, predefinidos)]
  
  blanks <- which(is.na(pal))
  
  pal[blanks] <- sample(colours(100), length(blanks))
  
  pal
}

# creamos una función para graficar parlamento

par_uy <- function(datos, titulo, mayoria, ...){
  
  ggplot(datos, aes(x, y, colour = party_long)) +
    geom_parliament_seats() + 
    geom_highlight_government(government == 1) +
    draw_majoritythreshold(n = mayoria, label = FALSE, type = 'semicircle') +
    #geom_parliament_bar(colour = datos$colour, party = datos$party_long) + 
    theme_ggparliament_bdd() +
    labs(colour = NULL, 
         title = titulo,
         subtitle = "") +
    scale_colour_manual(values = datos$colour,
                        limits = datos$party_long)
}

wrapit <- function(x, w = 120) {
  wtext <- paste(strwrap(x, w), collapse=" \n ")
  return(wtext)
}

`%notin%` <- Negate(`%in%`)


addUnits <- function(n) {
  labels <- ifelse(n >= 0 & n < 1000, n,  # si es menos de 1000 less than thousands
                   ifelse(n >= 1000 & n < 1e6, paste0(round(n/1e3), 'K'),  # si es menor a un millon in thousands
                          ifelse(n >= 1e6 & n < 1e12, paste0(round(n/1e6), 'M'),  # si es menor a 1000 millones in millions
                                 ifelse(n >= 1e12 & n < 1e15, paste0(round(n/1e12), 'B'), # in billions
                                       ifelse(n < 0 & n > -1000, n,  # si es menos de 1000 less than thousands
                                              ifelse(n < -1000 & n > -1000000, paste0(round(n/1e3), 'K'),  # si es menor a un millon in thousands
                                                     ifelse(n < -1000000 & n  > -1000000000000, paste0(round(n/1e6), 'M'),  # si es menor a 1000 millones in millions
                                                            ifelse(n < -1000000000000 & n > -1000000000000000, paste0(round(n/1e12), 'B'), # in billions
                                                                   "error"))))))))
  return(labels)
}


theme_m <- function(base_size = 13,
                    base_family = "Georgia",
                    base_line_size = base_size / 170,
                    base_rect_size = base_size / 170){
  
  theme_minimal(base_size = base_size, 
                base_family = base_family,
                base_line_size = base_line_size) %+replace%
    
    theme(plot.title = element_text(       
      family = base_family,            
      size = 16,                
      face = 'bold',            
      hjust = 0,
      vjust = 4),
      
      plot.subtitle = element_text(      
        family = base_family,            
        size = 14,
        hjust = 0,                
        vjust = 2),               
      
      plot.caption = element_text(
        family = base_family,     
        size = 10,                
        hjust = 1),               
      
      axis.title = element_text(  
        family = base_family,     
        size = 14,
        face="bold"),               
      
      axis.text = element_text(    
        family = base_family,      
        size = 12),                
      
      axis.text.x = element_text(           
        margin=margin(5, b = 10)),
      
      axis.line = element_line(colour = "grey50", size = 1),
      
      panel.grid.major.y = element_line(colour = "grey90", size = .75),
      panel.grid.minor.y = element_blank(),   
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),   
      
      plot.margin=unit(c(2,2,2.5,2.2),"cm"),
      
      strip.text = element_text(size=14, face="bold", family = base_family)
      
    )
}


# Function to change from varnames to proper_names
to_proper_name <- function(list){
  
  metadata <- readxl::read_excel("data/keys.xlsx")
  
  metadata %>%
    filter(names_var %in% list) %>%
    pull(names_proper)
  
}


# Function to change from proper_names to varnames
to_varname <- function(list){
  
  metadata <- readxl::read_excel("data/keys.xlsx")
  
  metadata %>%
    filter(names_proper %in% list) %>%
    pull(names_var)
  
}

firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}


simpleCap <- function(x,abr) {
  s <- strsplit(x, " ")[[1]]
  loc = which(!s %in% abr)
  loc_abr = which(s %in% abr)
  tmp_s = s[!s %in% abr]
  
  paste(toupper(substring(tmp_s, 1,1)), tolower(substring(tmp_s, 2)),
        sep="", collapse=" ")
  
  result = character(length(s))
  result[loc] = strsplit(paste(toupper(substring(tmp_s, 1,1)), tolower(substring(tmp_s, 2)),
                               sep="", collapse=" ")," ")[[1]]
  result[loc_abr] = abr
  result = paste(result,collapse = " ")
  return(result)
}
