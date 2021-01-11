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





#LM_cover <- read_csv("data/raw/cover_lifeform_raw.csv")
#LM_richness <- read_csv("data/raw/richness_lifeform_raw.csv")
#LM_transect <- read_csv("data/clean/Transect_LM.csv")

#LM_richness <- LM_richness %>% 
#  mutate(total = rowSums(.[4:9]))
#LM_cover <- LM_cover %>% 
#  mutate(total = rowSums(.[4:8]))

#export(LM_richness,"data/raw/richness_lifeform_raw.csv" )
#export(LM_cover,"data/raw/cover_lifeform_raw.csv" )

## organize data all and combine

#LM_cover <- LM_cover %>% 
# unite("plot_trt",plot_id,treatment,time) %>% 
#pivot_longer(cols= -plot_trt, names_to = "lifeform", values_to = "cover") 
#LM_cover <- LM_cover %>% 
#  unite("plot_trt_life",plot_trt, lifeform)
  

#LM_richness <- LM_richness %>% 
#  unite("plot_trt",plot_id,treatment,time) %>% 
 # pivot_longer(cols= -plot_trt, names_to = "lifeform", values_to = "richness") 
#LM_richness <- LM_richness %>% 
 # unite("plot_trt_life",plot_trt, lifeform)

#LM_rich_cover <-   left_join(LM_cover, LM_richness, by="plot_trt_life") 
#LM_rich_cover <- LM_rich_cover %>% 
#  pivot_longer(cols=-plot_trt_life, names_to = "cover_richness", values_to = "value") %>% 
 # separate(plot_trt_life, c("plot_id","treatment","time","lifeform"), sep="_", extra= "merge" )

#rename lifeform
#LM_rich_cover$lifeform <- recode(LM_rich_cover$lifeform,
  #                               tree_l ="LiveTree",
  #                               shrub_l = "LiveShrub",
  #                               shrub_d = "DeadShrub",
  #                               native_herb= "NativeHerb",
  #                               exotic_herb= "NonnativeHerb",
  #                               total_herb= "TotalHerb",
 #                                total="Total")
#LM_rich_cover$lifeform <- factor(LM_rich_cover$lifeform, levels = c("LiveTree","LiveShrub","DeadShrub","NativeHerb","NonnativeHerb","TotalHerb", "Total"))
#LM_rich_cover$time <- factor(LM_rich_cover$time, levels = c("pregoat","postgoat"))

#replace goats with fuel break
#LM_rich_cover <- LM_rich_cover %>% 
#  mutate(treatment = replace(treatment, which(treatment == "goats"),"fuelbreak"))


#export(LM_rich_cover, "data/clean/LM_rich_cover_all.csv")

LM_rich_cover <- import("data/clean/LM_rich_cover_all.csv")



##PLOT PREGOAT DATA COVER AND RICHNESS


#only pregoat data and remove total herb
LM_pregoat <- LM_rich_cover %>% 
  filter(time =="pregoat",
         lifeform == "DeadShrub" |
          lifeform == "NonnativeHerb"|
           lifeform == "LiveShrub"|
           lifeform == "NativeHerb"|
           lifeform == "LiveTree"|
           lifeform == "Total") 



#add p-values comparing groups; specify comparisons I want
my_comparisons <- list(c("fuelbreak","control"))


#facet label names for cover/richness
pregoat.labs <- c("Cover (%)", "Richness (count)")
names(pregoat.labs) <- c("cover","richness")

LM_pregoat_plot <- ggplot(data=LM_pregoat, mapping = aes(x=lifeform, y=value, fill=treatment))+
  geom_boxplot()+
  facet_wrap(~ cover_richness, scales = "free_y",
             labeller = labeller(cover_richness= pregoat.labs)) +
  scale_fill_manual(values=wes_palette("Moonrise2"))+
  stat_compare_means(aes(label = ..p.signif..))+
  theme(axis.title=element_text(size=14,face="bold"),
        axis.text=element_text(size=12))+
  theme_Publication()
LM_pregoat_plot


ggsave(LM_pregoat_plot,"C:/Users/ashle/Documents/R/lakemorena/lakemorena/images/PREGOAT_richcover.png")


###### IMPACTS OF GOAT GRAZING

LM_goats <- LM_rich_cover %>% 
  filter(treatment =="fuelbreak",
         lifeform == "DeadShrub" |
           lifeform == "NonnativeHerb"|
           lifeform == "LiveShrub"|
           lifeform == "NativeHerb"|
           lifeform == "LiveTree" |
           lifeform == "Total") 

#make sure pregoat comes first
LM_goats$time <- factor(LM_goats$time, levels = c("pregoat","postgoat"))
#add p-values comparing groups; specify comparisons I want
my_comparisons <- list(c("pregoat","postgoat"))


#facet label names for cover/richness
goat.labs <- c("Cover (%)", "Richness (count)")
names(goat.labs) <- c("cover","richness")

LM_goat_plot <- ggplot(data=LM_goats, mapping = aes(x=lifeform, y=value, fill=time))+
  geom_boxplot()+
  facet_wrap(~ cover_richness, scales = "free_y",
             labeller = labeller(cover_richness= goat.labs)) +
  scale_fill_manual(values=wes_palette("Moonrise2"))+
  stat_compare_means(aes(label = ..p.signif..))+
  theme(axis.title=element_text(size=14,face="bold"),
        axis.text=element_text(size=12))+
  theme_Publication()
LM_goat_plot


ggsave("C:/Users/ashle/Documents/R/lakemorena/lakemorena/images/GOAT_rich_cover.png" ,LM_goat_plot)


