library(tidyverse)
library(ggplot2)
library(cowplot)
library(egg)
library(grid)
library(ggplotify)
library(gridExtra)
library(ggpubr)
library(RColorBrewer)
library(rstatix)

# EMODEPSIDE MANUSCRIPT FIGURES #

###############################
# IMPORTANT!!!!!
# Download emodepside_manuscrip folder and as set working directory
setwd("~/Desktop/emodepside_manuscript/")
###############################

#################

### Load data ###

assign("all_data",get(load("data/wild_strains_data.RData")))
assign("mutant_data",get(load("data/mutant_data.RData")))
assign("EXTTOF",get(load("data/EXT_TOF_dose.RData")))

source("scripts/theme_ms.R")

######################

###### FIGURE 2 ######

#### DOSE RESPONSE PLOTS ####

## Calculate ##
plot_mutant <- mutant_data %>%
  dplyr::mutate(strain = as.character(strain)) %>%
  dplyr::mutate(strain = factor(strain, levels = c("N2","BZ142","NM1968"))) %>%
  dplyr::group_by(strain, trait, dose) %>%
  dplyr::summarise(avg = mean(norm_pheno), st_dev = sd(norm_pheno))

## Plot ##
k <- 1
mutant_plot_list = list()
for(plot_trait in c("norm.n","mean.TOF","median.norm.EXT")){
  if(plot_trait == "norm.n"){
    plot_aest <-  tweaking_nolab
    title <- "Brood size"
    breaking <- c(-75,-50,-25,0,25,50)
    breaklabels = c(-75,-50,-25,0,25,50)
    breaklimit <- c(-80,55)
  }
  if(plot_trait == "mean.TOF"){
    plot_aest <-  tweaking_nolab
    title <- "Animal length"
    breaking <- c(-100,-50,0,50) 
    breaklabels = c(-100,-50,0,50)
    breaklimit <- c(-110,55)
  }
  if(plot_trait == "median.norm.EXT"){
    plot_aest <-  tweaking_lab
    title <- "Optical density"
    breaking <- c(-0.8,-0.4,0,0.4,0.8) 
    breaklabels = c(-0.8,-0.4,0,0.4,0.8)
    breaklimit <- c(-0.85,0.85)
  }
  mutant_plot_list[[k]] <- plot_mutant %>% 
    dplyr::filter(trait == plot_trait) %>%
    ggplot(aes(x=dose, y=avg, group = strain, color = strain)) +
    geom_errorbar(width=.01, aes(ymin=avg-st_dev, ymax=avg+st_dev)) +
    geom_line(size = 0.5)+
    scale_colour_manual(values = straincol)+
    theme_emo +
    plot_aest +
    scale_x_continuous(breaks = c(0,0.0098,0.0196,0.0391,0.078125,0.156240,0.312500), guide = guide_axis(angle = 90), 
                       labels = xlabel, 
                       "Emodepside concentration (nM)", limits = c(0,0.32)) + 
    scale_y_continuous(title, breaks = breaking, labels = breaklabels, limits = breaklimit, expand = expand_scale(mult = c(0.005, 0.05)))
  k=k+1
}

## Making figure ##
m1 <- ggplotGrob(mutant_plot_list[[1]])
m2 <- ggplotGrob(mutant_plot_list[[2]])
m3 <- ggplotGrob(mutant_plot_list[[3]])

mutant_graphs <-  rbind(m1,m2,m3, size = "first")
mutant_graphs$widths <- unit.pmax(m1$widths,m2$widths,m3$widths)
grid.newpage()
mutants <- as_ggplot(mutant_graphs) +
  draw_plot_label(label = c("A", "B", "C"), size = 12, x = c(0.015, 0.015, 0.015), y = c(0.997, 0.687, 0.374))
#ggsave2("~/Desktop/Figure2.png",mutants, height = 6.5, width = 3.75)

######################

###### FIGURE 3 ######

## EXT--TOF PLOTS ##

s <- 1
strain_plot_list = list()

for(plot_strain in c("N2","CB4856","DL238")){
  if(plot_strain == "N2"){
    lab <- tweaking_nolab 
    name <- 'atop(bold("N2"))'
  }
  if(plot_strain == "CB4856"){
    lab <- tweaking_nolab 
    name <- 'atop(bold("CB4856"))'
  }
  if(plot_strain == "DL238"){
    lab <- tweaking_lab 
    name <- 'atop(bold("DL238"))'
  }
  strain_plot_list[[s]] <-  ggplot(EXTTOF[EXTTOF$strain == plot_strain & EXTTOF$condition == "emodepside00781",], aes(x=TOF, y=EXT)) +
    geom_point(data= EXTTOF[EXTTOF$strain == plot_strain & EXTTOF$condition == "DMSO",], aes(TOF, y=EXT), colour = "darkgrey", size =0.8) +
    geom_point(size=0.8) +
    theme_emo +
    lab +
    annotate("text", x = 1000, y = 300, label = name, parse = TRUE, size=3.5) +
    scale_x_continuous(expression(bold(paste("Animal length (",mu,"m)"))), limits = c(0,1100), expand = expand_scale(mult = c(0.005,0.05))) +
    scale_y_continuous("Optical density", limits = c(0, 4500), expand = expand_scale(mult = c(0.005, 0.05)))
  s= s+1
}

strain1 <- ggplotGrob(strain_plot_list[[1]])
strain2 <- ggplotGrob(strain_plot_list[[2]])
strain3 <- ggplotGrob(strain_plot_list[[3]])

all_strains <-  rbind(strain1,strain2,strain3, size = "first")
all_strains$widths <- unit.pmax(strain1$widths,strain2$widths,strain3$widths)
grid.newpage()
strains <- as_ggplot(all_strains) +
  draw_plot_label(label = c("A", "B", "C"), size = 12, x = 0.015, y = c(0.997, 0.687, 0.374))
#ggsave2("~/Desktop/Figure3.png",strains,  height = 6.5, width = 3.75)


######################

###### FIGURE 4 ######

#### DOSE RESPONSE PLOTS ####

## Calculate ##
dose_avg <- all_data %>%
  dplyr::mutate(strain = factor(strain, levels = c("DL238","JU2586","CB4932","JU782","NIC271","NIC265","NIC258","JU751","WN2001","N2"))) %>%
  dplyr::group_by(strain, trait, dose) %>%
  dplyr::summarise(avg = mean(norm_pheno), st_dev = sd(norm_pheno))

## Plot ## 
l <- 1
curve_plot_list = list()
for(plot_trait in c("norm.n","mean.TOF","median.norm.EXT")){
  if(plot_trait == "norm.n"){
    plot_aest <-  tweaking_nolab
    title <- "Brood size"
    rechthoek <- geom_point(mapping=aes(x=c(0.0781), y=-49.3), size = 2 , shape =24, fill = "darkgrey", color = "darkgrey")
    breaking <- c(-50,-25,0,25,50) 
    breaklabels = c(-50,-25,0,25,50)
    breaklimit <- c(-50,50)   
  }
  if(plot_trait == "mean.TOF"){
    plot_aest <-  tweaking_nolab
    title <- "Animal length"
    rechthoek <- geom_point(mapping=aes(x=c(0.0781), y=-158), size = 2 , shape =24, fill = "darkgrey", color = "darkgrey")
    breaking <- c(-150,-100,-50,0,50,100) 
    breaklabels = c(-150,-100,-50,0,50,100)
    breaklimit <- c(-160,115)
  }
  if(plot_trait == "median.norm.EXT"){
    plot_aest <-  tweaking_lab
    title <- "Optical density"
    rechthoek <- geom_point(mapping=aes(x=c(0.0781), y=-1.085), size = 2 , shape =24, fill = "darkgrey", color = "darkgrey")
    breaking <- c(-1.0,-0.5,0,0.5,1) 
    breaklabels = c(-1.0,-0.5,0,0.5,1)
    breaklimit <- c(-1.1,1.1)
  }
  curve_plot_list[[l]] <-   dose_avg %>% 
    dplyr::filter(trait == plot_trait) %>%
    ggplot(aes(x=dose, y=avg, group = strain, color = strain)) +
    geom_errorbar(width=.01, aes(ymin=avg-st_dev, ymax=avg+st_dev)) +
    geom_line(size = 0.5)+
    scale_colour_manual(values = straincol)+
    theme_emo +
    plot_aest +
    rechthoek +
    scale_x_continuous(breaks = c(0,0.0098,0.0196,0.0391,0.078125,0.156240,0.312500), guide = guide_axis(angle = 90), 
                       labels = xlabel, 
                       "Emodepside concentration (nM)", limits = c(0,0.32)) + #labels = c("0","0.0098","0.0196","0.0391","0.078125","0.156240","0.312500")
    scale_y_continuous(title, breaks = breaking, labels = breaklabels, limits = breaklimit,expand = expand_scale(mult = c(0.00, 0.0)))
  l=l+1
}

####################

#### EC50 PLOTS ####

## Calculate ##
max <- all_data %>% ## determine highest phenotypic response per assay/strain/trait
  dplyr::mutate(ID = paste(strain,trait,sep="_")) %>%
  dplyr::group_by(strain,trait,dose) %>%
  dplyr::mutate(avg = mean(norm_pheno)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(strain,trait) %>%
  dplyr::mutate(max = max(avg)) %>%
  dplyr::filter(avg == max) %>%
  dplyr::summarise(maxID = first(ID), peak = first(dose)) %>%
  dplyr::ungroup() %>%
  dplyr::select(maxID,peak)

EC50_avg <- all_data %>%
  dplyr::mutate(maxID = paste(strain,trait,sep="_")) %>%
  dplyr::full_join(max, by = "maxID") %>%
  dplyr::select(!maxID)  %>%
  dplyr::filter(dose >= peak)%>%
  dplyr::group_by(trait,strain,assay) %>%
  dplyr::summarise(intercept_a = (lm(norm_pheno ~ dose))$coefficients[1],
                   dose_b = (lm(norm_pheno ~ dose))$coefficients[2],
                   average = mean(norm_pheno))%>% 
  dplyr::group_by(trait,strain) %>%
  dplyr::mutate(y0 = intercept_a/-dose_b) %>%
  dplyr::mutate(EC50 = (average-intercept_a)/dose_b) %>%
  dplyr::summarise(EC50avg = mean(EC50),
                   st_dev = sd(EC50))

## Plot ##
m <- 1
EC50_plot_list = list()
for(plot_trait in c("norm.n","mean.TOF","median.norm.EXT")){
  if(plot_trait == "norm.n"){
    plot_aest <-  tweaking_nolab
  }
  if(plot_trait == "mean.TOF"){
    plot_aest <-  tweaking_nolab
  }
  if(plot_trait == "median.norm.EXT"){
    plot_aest <-  tweaking_lab
  }
  EC50_plot_list[[m]] <-
    EC50_avg %>%
    dplyr::filter(trait == plot_trait) %>% 
    dplyr::mutate(strain = replace(strain, strain == "N2_1", "N2")) %>%
    dplyr::mutate(strain = replace(strain, strain == "JU2580", "JU2586")) %>% 
    dplyr::filter(! strain %in% c("N2_2","BZ142","NM1968")) %>%
    dplyr::mutate(strain = factor(strain, levels = c("DL238","JU2586","CB4932","JU782","NIC271","NIC265","NIC258","JU751","WN2001","N2"))) %>%
    ggplot(aes(strain,EC50avg, fill = strain)) +
    geom_errorbar(aes( ymax = EC50avg + st_dev, ymin = EC50avg - st_dev), size = 0.5, width = 0.5)+
    geom_col() + 
    coord_flip() +
    scale_color_manual(values= straincol,
                       aesthetics = c("colour", "fill")) +
    theme_emo +
    plot_aest +
    scale_x_discrete(NULL,expand = expand_scale(mult = c(0.1, 0.1))) +
    scale_y_continuous(breaks = c(0,0.0098,0.0196,0.0391,0.078125,0.156240,0.312500), guide = guide_axis(angle = 90), 
                       labels = xlabel, 
                       "EC50 (nM)", limits = c(0,0.23))
  m=m+1
  
}

## Making figure ##
p1 <- ggplotGrob(curve_plot_list[[1]])
p2 <- ggplotGrob(curve_plot_list[[2]])
p3 <- ggplotGrob(curve_plot_list[[3]])
p4 <- ggplotGrob(EC50_plot_list[[1]])
p5 <- ggplotGrob(EC50_plot_list[[2]])
p6 <- ggplotGrob(EC50_plot_list[[3]])

curve_graphs <-  rbind(p1,p2,p3, size = "first")
curve_graphs$widths <- unit.pmax(p1$widths,p2$widths,p3$widths)
ec50_graphs <-  rbind(p4,p5,p6, size = "first")
ec50_graphs$widths <- unit.pmax(p1$widths,p2$widths,p3$widths)
grid.newpage()

curves <- as_ggplot(curve_graphs) +
  draw_plot_label(label = c("A", "B", "C"), size = 12, x = c(0.015, 0.015, 0.015), y =  c(0.997, 0.689, 0.38))
ec50 <- as_ggplot(ec50_graphs) +
  draw_plot_label(label = c("D", "E", "F"), size = 12, x = c(0.015, 0.015, 0.015), y = c(0.997, 0.689, 0.38))
#plot_grid(curves,ec50,  ncol = 2) +  ggsave2("~/Desktop/Figure4.png", height = 6.5, width = 7.5)

############

### DONE ###

############



