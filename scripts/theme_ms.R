##Theme base
#Theme Options

library(tidyverse)
library(ggplot2)

# colors
axis_color <- "#000F08"
highlight_color <- "#D7263D"
background_color <- "white"

# font
number_font <- "Arial"
axes_text_size <- 10
axes_title_font <- "Arial"
axes_title_size <- 12
title_size <- 12

theme_emo <- theme(
  line = element_line(colour = axis_color, size = 0.5, linetype = 1), 
  rect = element_rect(fill = background_color, colour = axis_color, size = 0.5, linetype = 1), 
  text = element_text(family = axes_title_font, size = axes_text_size), 
  
  axis.text = element_text(family = number_font,size = rel(0.8), colour = "black", margin = unit(0.1, "cm")),
  axis.text.x = element_text(vjust = 0), 
  axis.text.y = element_text(hjust = 1), 
  axis.ticks = element_line(colour = "black"), 
  axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), angle = 90), 
  axis.ticks.length = unit(0.15, "cm"),
  
  strip.text = element_text(size = rel(0.8)), 
  strip.background = element_rect(fill = background_color, colour = NA, size = 0.5, linetype = 1),
  
  plot.background = element_rect(fill = background_color, color = NA),
  
  legend.background = element_rect(fill=background_color,colour = background_color), 
  legend.spacing = unit(0.2, "cm"), 
  legend.key = element_rect(fill = background_color, colour = NA), 
  legend.key.size = unit(1, "lines"), 
  legend.key.height = NULL, 
  legend.key.width = NULL, 
  legend.text = element_text(size = rel(0.6)), 
  legend.text.align = NULL, 
  legend.title = element_text(size = rel(0.6), hjust = 0), 
  legend.title.align = NULL, 
  legend.position = "right", 
  legend.direction = NULL, 
  legend.justification = "center", 
  legend.box = NULL, 
  
  panel.background = element_rect(fill = background_color, colour = NA),  
  panel.grid.major = element_line(colour = NA),
  panel.grid.minor = element_blank(), 
  panel.spacing = unit(1, "lines"), 
  panel.margin.x = NULL, 
  panel.margin.y = NULL)

ggplot2::theme(strip.background = element_blank(),
               axis.text.y = ggplot2::element_text(size = 8, angle = 90, hjust = 0.5),
               axis.title.x = ggplot2::element_text(size = 8, face = "bold", color = "black", vjust = -0.3), 
               axis.title.y = ggplot2::element_text(size = 8, face = "bold", color = "black"), 
               strip.text.x = ggplot2::element_text(size = 8, face = "bold", color = "black"), 
               strip.text.y = ggplot2::element_text(size = 8, face = "bold", color = "black"), 
               plot.title = ggplot2::element_text(size = 8, face = "bold", vjust = 1), 
               panel.background = ggplot2::element_rect(color = "black", size = 0.5),
               legend.position = "none") 

##################

### Aesthetics ###

rood <- brewer.pal(n = 5, name = 'Reds')
blauw <- brewer.pal(n = 5, name = 'Blues')
groen <-brewer.pal(n = 5, name = 'Greens')

straincol = c("BZ142" = blauw[3], 
              "CB4932" = blauw[4], 
              "DL238" = "cadetblue3", 
              "JU2586" = blauw[3], 
              "JU751" = rood[4],
              "JU782" = groen[3],
              "N2" = "orange",
              "NIC258" = rood[3],
              "NIC265" = groen[5],
              "NIC271" = groen[4],
              "NM1968" = groen[3],
              "WN2001" = rood[5])

xlabel = c( "0" = "0", "0.0098"= "9.8", "0.0196" = "19.6", "0.0391" = "39.1", "0.078125" = "78.1", "0.15625"= "156.3", "0.3125" = "312.5") 

tweaking_nolab = theme(strip.background = element_blank(),
                       axis.text.y = ggplot2::element_text(size = 8, angle = 0), 
                       axis.title.y = ggplot2::element_text(size = 12, face = "bold", color = "black", vjust = -0.5),
                       strip.text.x = ggplot2::element_text(size = 8, face = "bold", color = "black"), 
                       strip.text.y = ggplot2::element_text(size = 8, face = "bold", color = "black"), 
                       panel.border = ggplot2::element_rect(fill = NA, color = "black", size = 0.5),
                       legend.position = "none" ,
                       axis.title.x = element_blank(),
                       axis.text.x = element_blank())

tweaking_lab = theme(strip.background = element_blank(),
                     axis.text.x = ggplot2::element_text(size = 8, angle =90), #element_blank(), , hjust = -0.2
                     axis.text.y = ggplot2::element_text(size = 8, angle = 0), #, hjust = 0.5
                     axis.title.x.bottom = ggplot2::element_text(size = 12, face = "bold", color = "black", vjust = 0.4),
                     axis.title.y = ggplot2::element_text(size = 12, face = "bold", color = "black", vjust = -0.5),
                     strip.text.x = ggplot2::element_text(size = 8, face = "bold", color = "black"), 
                     strip.text.y = ggplot2::element_text(size = 8, face = "bold", color = "black"), 
                     panel.border = ggplot2::element_rect(fill = NA, color = "black", size = 0.5),
                     legend.position = "none")

tweaking_legend = theme(strip.background = element_blank(),
                        axis.text.y = ggplot2::element_text(size = 8, angle = 0),
                        axis.title.y = ggplot2::element_text(size = 12, face = "bold", color = "black", vjust = -0.5),
                        strip.text.x = ggplot2::element_text(size = 8, face = "bold", color = "black"), 
                        strip.text.y = ggplot2::element_text(size = 8, face = "bold", color = "black"), 
                        panel.border = ggplot2::element_rect(fill = NA, color = "black", size = 0.5),
                        legend.position = c(.05, .05),
                        legend.justification = c("left", "bottom"),
                        axis.title.x = element_blank(),
                        axis.text.x = element_blank(),
                        legend.title = element_blank(),
                        legend.text = element_text( 
                          size = 10,
                          face = "bold",
                          colour = "black"))

#############################
