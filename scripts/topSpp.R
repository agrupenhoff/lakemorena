
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


LM_transect <- read_csv("C:/Users/ashle/Documents/R/lakemorena/lakemorena/data/clean/Transect_LM.csv")
LM_lifeform <- read_csv("C:/Users/ashle/Documents/R/lakemorena/lakemorena/data/raw/Species_lifeform.csv")

#LM_transect_longer <- LM_transect %>% 
#  pivot_longer(cols= -plot_time, names_to = "species", values_to = "count") %>% 
#  rename("Species" = "species")

#LM_transect_spp <- left_join(LM_transect_longer, LM_lifeform, by="Species")

#export(LM_transect_spp, "C:/Users/ashle/Documents/R/lakemorena/lakemorena/data/clean/LM_transect_species.csv")

#LM_transect_spp <- read_csv("C:/Users/ashle/Documents/R/lakemorena/lakemorena/data/clean/LM_transect_species.csv")


#aggregate mean count values for each species by treatment and time

#LM_topspp <- LM_transect_spp %>% 
#  separate(plot_time, c("plot_id","treatment","time"), sep="_", extra= "merge" ) %>% 
#  group_by(treatment, time, Species) %>% 
#  summarise(sum_count = sum(count)) 

#LM_topspp <- LM_topspp %>% 
#  mutate(Spp_percent = (sum_count/800)*100) %>% 
#  mutate(treatment = replace(treatment, which(treatment == "goats"),"fuelbreak"))

#export(LM_topspp, "C:/Users/ashle/Documents/R/lakemorena/lakemorena/data/clean/LM_topspp.csv")

LM_topspp <- read_csv("C:/Users/ashle/Documents/R/lakemorena/lakemorena/data/clean/LM_topspp.csv")

# Control

LM_topspp_control <- LM_topspp %>% 
  filter(treatment == "contol")%>% 
  select(time, Species, Spp_percent) %>% 
  pivot_wider(names_from = time, values_from = Spp_percent) %>% 
  mutate(change = (pregoat-postgoat))


#TARGETED BY GOATS, fuelbreak 

LM_topspp_fuelbreak <- LM_topspp %>% 
  filter(treatment == "fuelbreak")%>% 
  select(time, Species, Spp_percent) %>% 
  pivot_wider(names_from = time, values_from = Spp_percent) %>% 
  mutate(change = (pregoat-postgoat))

LM_topspp_fuelbreak <- left_join(LM_topspp_fuelbreak, LM_lifeform, by= "Species")





