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




LM_cover <- read_csv("C:/Users/ashle/Documents/R/lakemorena/lakemorena/data/raw/cover_lifeform_raw.csv")
LM_richness <- read_csv("C:/Users/ashle/Documents/R/lakemorena/lakemorena/data/raw/richness_lifeform_raw.csv")
LM_transect <- read_csv("C:/Users/ashle/Documents/R/lakemorena/lakemorena/data/clean/Transect_LM.csv")


## organize data all and combine

LM_cover <- LM_cover %>% 
  unite("plot_trt",plot_id,treatment,time) %>% 
  pivot_longer(cols= -plot_trt, names_to = "lifeform", values_to = "cover") 
LM_cover <- LM_cover %>% 
  unite("plot_trt_life",plot_trt, lifeform)
  

LM_richness <- LM_richness %>% 
  unite("plot_trt",plot_id,treatment,time) %>% 
  pivot_longer(cols= -plot_trt, names_to = "lifeform", values_to = "richness") 
LM_richness <- LM_richness %>% 
  unite("plot_trt_life",plot_trt, lifeform)

LM_rich_cover <-   left_join(LM_cover, LM_richness, by="plot_trt_life") 
LM_rich_cover <- LM_rich_cover %>% 
  pivot_longer(cols=-plot_trt_life, names_to = "cover_richness", values_to = "value") %>% 
  separate(plot_trt_life, c("plot_id","treatment","time","lifeform"), sep="_", extra= "merge" )

export(LM_rich_cover, "C:/Users/ashle/Documents/R/lakemorena/lakemorena/data/clean/LM_rich_cover_all.csv")


##PLOT PREGOAT DATA

LM_pregoat <- LM_rich_cover %>% 
  filter(time =="pregoat")


LM_pregoat_plot <- ggplot(data=LM_pregoat, mapping = aes(x=lifeform, y=value, fill=treatment))+
  geom_boxplot()+
  facet_wrap(~ cover_richness, scales = "free_y") +
  scale_fill_manual(values=wes_palette("Moonrise2"))+
  stat_compare_means(aes(label = ..p.signif..))
LM_pregoat_plot

LM_pregoat_plot 

#add p-values comparing groups; specify comparisons I want
my_comparisons <- list(c("goats","control"))

ggsave("C:/Users/ashle/Documents/R/lakemorena/lakemorena/images/PREGOAT_rich_cover.png" ,LM_pregoat_plot)




