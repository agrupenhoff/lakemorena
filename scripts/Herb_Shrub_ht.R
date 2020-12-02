library(dplyr)
library(corpcor)
library(tidyverse)
library(tinytex)
library(tibble)
library(dplyr)
library(ggplot2)
library(vegan)
library(knitr)
library(kableExtra)
library(rio)
library(ape)
library(devtools)
library(Hotelling)
library(corpcor)
library(wesanderson)
library(ggpubr)

LM_height <- read_csv("C:/Users/ashle/Documents/R/lakemorena/lakemorena/data/clean/LM_height_clean.csv")

#LM_height <- LM_height %>% 
#    rename(Herb = avg_herb_ht,
#         Shrub = avg_shrub_ht) %>% 
#  unite("plot_trt",plot_id,treatment,time) %>% 
#  pivot_longer(cols= -plot_trt, names_to = "lifeform", values_to = "Height_cm") 
  
#LM_height <- LM_height %>% 
#  separate(plot_trt, c("plot_id","treatment","time"), sep="_", extra= "merge" )

LM_height$time <- factor(LM_height$time, levels = c("pregoat","postgoat"))

export(LM_height, "C:/Users/ashle/Documents/R/lakemorena/lakemorena/data/clean/LM_height_clean.csv")

#only pregoat data 

LM_pregoat_ht <- LM_height %>% 
  filter(time =="pregoat") 

#add p-values comparing groups; specify comparisons I want
my_comparisons <- list(c("fuelbreak","control"))


LM_pregoat_ht_plot <- ggplot(data=LM_pregoat_ht, mapping = aes(x=treatment, y=Height_cm, fill=treatment))+
  geom_boxplot()+
  scale_fill_manual(values=wes_palette("Moonrise2"))+
  facet_wrap(~ lifeform, scales = "free_y")+
  stat_compare_means(aes(label = ..p.signif..))+
  theme(axis.title=element_text(size=14,face="bold"),
        axis.text=element_text(size=12))+
  theme_Publication()+
  theme(axis.title.x=element_blank())
LM_pregoat_ht_plot


ggsave("C:/Users/ashle/Documents/R/lakemorena/lakemorena/images/PREGOAT_height.png" ,LM_pregoat_ht_plot)


###### IMPACTS OF GOAT GRAZING

LM_goats_ht <- LM_height %>% 
  filter(treatment =="fuelbreak")

#add p-values comparing groups; specify comparisons I want
my_comparisons <- list(c("pregoat","postgoat"))


#facet label names for cover/richness


LM_goat_ht_plot <- ggplot(data=LM_goats_ht, mapping = aes(x=time, y=Height_cm, fill=time))+
  geom_boxplot()+
  facet_wrap(~ lifeform, scales = "free_y")+
  scale_fill_manual(values=wes_palette("Moonrise2"))+
  stat_compare_means(aes(label = ..p.signif..))+
  theme(axis.title=element_text(size=14,face="bold"),
        axis.text=element_text(size=12))+
  theme_Publication()+
  theme(axis.title.x=element_blank())
LM_goat_ht_plot


ggsave("C:/Users/ashle/Documents/R/lakemorena/lakemorena/images/GOAT_height.png" ,LM_goat_ht_plot)



