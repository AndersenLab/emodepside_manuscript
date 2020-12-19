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

assign("mutant_data",get(load("data/mutant_data.RData")))
assign("dose_one",get(load("data/dose_one_data.RData")))

source("scripts/theme_ms.R")

####################################

###### SUPPLEMENTARY FIGURE 2 ######

## Stats ##

plot_DMSO <- mutant_data %>%
  dplyr::filter(dose == 0) %>%
  dplyr::group_by(trait,strain,assay) %>%
  dplyr::summarise(avg = mean(phenotype)) %>%
  dplyr::mutate(strain = factor(strain, levels = c("N2","BZ142","NM1968"))) %>%
  dplyr::mutate(trait = factor(trait, levels = c("norm.n","mean.TOF","median.norm.EXT")))

stats <- mutant_data %>%
  dplyr::filter(dose == 0) %>%
  dplyr::group_by(trait,strain,assay) %>%
  dplyr::summarise(avg = mean(phenotype)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(trait)  %>%
  wilcox_test(avg ~ strain, p.adjust.method = "bonferroni", ref.group = "N2")

starplot <- stats %>%
  dplyr::mutate(strain = group2 ) %>%
  dplyr::select(-group2,-group1) %>%
  dplyr::mutate(avg = ifelse(trait == "norm.n", 37,
                             ifelse(trait == "mean.TOF", 60,0.35)))%>%
  dplyr::mutate(BF = ifelse(p.adj.signif == "ns","","*"))

## Plot ##
a <- 1
sup_plot_list = list()
for(plot_trait in c("norm.n","mean.TOF","median.norm.EXT")){
  if(plot_trait == "norm.n"){
    title <- "Brood size"
    breaking <- c(-25,0,25)
    breaklabels = c(-25,0,25)
    breaklimit <- c(-30,40)
    label_strain <- theme( axis.text.x = element_blank())}
  if(plot_trait == "mean.TOF"){
    title <- "Animal length"
    breaking <- c(-40,-20,0,20,40,60)
    breaklabels = c(-40,-20,0,20,40,60)
    breaklimit <- c(-45,65)
    label_strain <- theme( axis.text.x = element_blank())}
  if(plot_trait == "median.norm.EXT"){
    title <-"Optical density"
    breaking <- c(-0.2,0,0.2)
    breaklabels = c(-0.2,0,0.2)
    breaklimit <- c(-.25,0.38)
    label_strain <- theme( axis.text.x = ggplot2::element_text(size = 12, angle = 315, face = "bold"))}
  sup_plot_list[[a]] <- plot_DMSO[plot_DMSO$trait == plot_trait,] %>%
    dplyr::mutate(strain = factor(strain, levels = c("N2","BZ142","NM1968"))) %>%
    dplyr::mutate(trait = factor(trait, levels = c("norm.n","mean.TOF","median.norm.EXT"))) %>% 
    ggplot(aes(x=strain, y= avg, color = strain)) +
    geom_boxplot(outlier.shape = NA) +
    geom_jitter(size=1, colour = "darkgrey") +
    scale_colour_manual(values = straincol)+
    theme_emo +
    tweaking_nolab +
    label_strain +
    geom_text(data = starplot[starplot$trait == plot_trait,],aes(label = BF), size = 8 , color = "black") +
    scale_y_continuous(title, breaks = breaking, labels = breaklabels, limits = breaklimit, expand = expand_scale(mult = c(0.005, 0.05)))
  a = a+1
}

## Making figure ##

pa <- ggplotGrob(sup_plot_list[[1]])
pb <- ggplotGrob(sup_plot_list[[2]]) 
pc <- ggplotGrob(sup_plot_list[[3]]) 

sup_graphs <-  rbind(pa,pb,pc, size = "first")
sup_graphs$widths <- unit.pmax(pa$widths,pb$widths,pc$widths)
grid.newpage()

sup_two <- as_ggplot(arrangeGrob(sup_graphs)) + 
  draw_plot_label(label = c("A", "B", "C"), size = 12, x = 0.015, y = c(0.997, 0.693, 0.387))
#ggsave2("~/Desktop/Sup_Fig2.png", sup_two, height = 6.5, width = 3.75, units = "in") 

####################################

###### SUPPLEMENTARY FIGURE 3 ######

## Plot ##
straincol_dose = c("N2" = "orange",
              "CB4856" = "blue",
              "DL238" = "cadetblue3")

plot_DOSE <- dose_one %>%
  dplyr::group_by(strain, trait, dose) %>%
  dplyr::summarise(avg = mean(norm_pheno), st_dev = sd(norm_pheno))

b <- 1
dose_plot_list = list()

for(plot_trait in c("norm.n","mean.TOF","median.norm.EXT")){
  if(plot_trait == "norm.n"){
    title <- "Brood size"
    lab = tweaking_legend 
  }
  if(plot_trait == "mean.TOF"){
    title <- "Animal length"
    lab = tweaking_nolab
  }
  if(plot_trait == "median.norm.EXT"){
    title <- "Optical density"
    lab = tweaking_lab 
  }
  dose_plot_list[[b]] <-  plot_DOSE[plot_DOSE$trait == plot_trait,] %>% 
    ggplot(aes(x=dose, y=avg, group = strain, color = strain))+
    geom_errorbar(width=.01, aes(ymin=avg-st_dev, ymax=avg+st_dev)) +
    geom_line(size = 0.5) +
    scale_colour_manual(values = straincol_dose)+
    theme_emo +
    lab +
    scale_x_continuous(breaks = c(0,0.0391,0.078125,0.156240,0.3125), guide = guide_axis(angle = 90), 
                       labels = c("0","0.0391","0.0781","0.1562","0.3125"),
                       expression(bold(paste("Emodepside concentration (",mu,"M)"))), limits = c(-0.015,0.33)) + #labels = c("0","0.0098","0.0196","0.0391","0.078125","0.156240","0.312500")
    scale_y_continuous(title,expand = expand_scale(mult = c(0.005, 0.05))) 
  b=b+1 }

dose1 <- ggplotGrob(dose_plot_list[[1]])
dose2 <- ggplotGrob(dose_plot_list[[2]])
dose3 <- ggplotGrob(dose_plot_list[[3]])


all_graphs <-  rbind(dose1,dose2,dose3, size = "first")
all_graphs$widths <- unit.pmax(dose1$widths,dose2$widths,dose3$widths)
grid.newpage()
doses <- as_ggplot(all_graphs) +
  draw_plot_label(label = c("A", "B", "C"), size = 12, x = 0.015, y = c(0.997, 0.693, 0.387))
#ggsave2("~/Desktop/SubFig3.png",doses,  height = 6.5, width = 3.75)

############

### DONE ###

############



