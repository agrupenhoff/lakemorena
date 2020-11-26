library(vegan)
library(ggplot2)
library(grid)
library(phyloseq)

#NMDS CREATION PREGOAT: fuel break vs. adjacent wildlands

#import abundance data
Transect_LM <- import("C:/Users/ashle/Documents/R/lakemorena/lakemorena/data/clean/Transect_LM.csv", header=TRUE)

Transect_separate <- Transect_LM %>% 
  separate(plot_time, c("plot", "trt","time"),"_")
  
Transect_pregoat <- Transect_separate %>% 
  filter(time == "pregoat")



#make community matrix on which to base ordination
pregoat = Transect_pregoat[,4:ncol(Transect_pregoat)]
#columns that contain descriptive data
pregoat.env = Transect_pregoat[,1:3]

#remove samples that have 0s across all plots
good_samples <- colnames(pregoat[(colSums(decostand(pregoat,"pa")) >= 1)])     # decostand(x,"pa") counts presence/absence
pregoat = pregoat[,good_samples]


#ordination by NMDS
set.seed(123) #reproduce same results
NMDS <- metaMDS(pregoat, distance="bray", k=2) #no transformation of species data is made here prior to bray curtis dissimilarities being calculated. 
NMDS
            #stress = 0.1857329

MDS1 = NMDS$points[,1]
MDS2 = NMDS$points[,2]
NMDS = data.frame(MDS1 = MDS1, MDS2 = MDS2, plot=pregoat.env$plot, trt = pregoat.env$trt)
head(NMDS)



#species data
stems<-colSums(pregoat) #total abundances for each species
spps <- data.frame(scores(meta.nmds.pregoat, display = "species")) #dataframe of species scoes for plotting
spps$species <- row.names(spps) # making a column with species names
spps$colsums <- stems #adding the colSums from above
spps<-spps[!is.na(spps$NMDS1) & !is.na(spps$NMDS2),] #removes NAs
spps.colmedian <- median(spps$colsums) #create an object that is the median of the abundance of the measured species
spps.colmean <- mean(spps$colsums) #creates a mean instead if you wish to use
spps2 <- subset(spps,spps$colsums > spps.colmean) #select the most abundant species. Could discard fewer by going something like - spps$colsums>(spps.colmedian/2) instead
spps2$species <- factor(spps2$species) #otherwise factor doesn't drop unused levels and it will throw an error

#Add

#plot nice with ggplot - difference between control and goats
              
              #plot
              NMDS_treatment <- ggplot(data=NMDS, aes(x=MDS1, y=MDS2)) +
                geom_point(aes(MDS1,MDS2,color=factor(trt),shape = factor(trt)),size=3)+
                coord_fixed()+
                theme_classic()+
                theme(panel.background = element_rect(fill = NA, colour = "black", size = 1, linetype = "solid"))+
                labs(colour = "Treatment", shape = "Treatment")+ # add legend labels for Management and Landuse
                theme(legend.position = "right", legend.text = element_text(size = 12), legend.title = element_text(size = 12), axis.text = element_text(size = 10))+ # add legend at right of plot
                labs(title = "Ordination plot")+
                stat_ellipse(aes(color=trt),type="norm") +
                geom_text(data=spps2, aes(x=NMDS1, y=NMDS2,label=species),alpha=0.5)  #add species label of most abundant species
                
              NMDS_treatment
              
                
              
              ggsave( "C:/Users/ashle/Documents/R/lakemorena/lakemorena/images/NMDS_treatment.jpg",NMDS_treatment)
                            
              
              
  
              
              #Bootstrapping and testing for differences between the groups 
              #Permanova test
              fit <- adonis(com.pregoat ~ trt, data=Env.pregoat, permutations=999, method="bray")
              fit
              
              #p-value = 0.001 therefore we can assume these are different
              
              #####################
              #Check assumption of homogeneity of multivariate dispersion
              distances_data <- vegdist(com.pregoat)
              anova(betadisper(distances_data, Env.pregoat$trt))
              
              
              
              
              
#########################################################################################################
################################################################################################              #########
##again but for ALL
              
              Transect_all <- Transect_separate %>% 
                unite("treatment", trt, time)
              
              #make community matrix on which to base ordination
              com.alltrt = Transect_all[,3:ncol(Transect_all)]
              #columns that contain descriptive data
              Env.alltrt = Transect_all[,1:2]
              
              #remove samples that have 0s across all plots
              good_samples <- colnames(com.alltrt[(colSums(decostand(com.alltrt,"pa")) >= 1)])     # decostand(x,"pa") counts presence/absence
              com.alltrt = com.alltrt[,good_samples]
              
              
              #ordination by NMDS
              set.seed(123)
              NMDS <- metaMDS(com.alltrt, distance="bray", k=2)
              NMDS
              
              #stress = 0.2313909
              
              
              #plot nice with ggplot - difference between control and goats
              
              MDS1 = NMDS$points[,1]
              MDS2 = NMDS$points[,2]
              NMDS = data.frame(MDS1 = MDS1, MDS2 = MDS2, plot=Env.alltrt$plot, trt = Env.alltrt$treatment)
              head(NMDS)
              
              
              
              #plot
              NMDS_all <- ggplot(NMDS, aes(x=MDS1, y=MDS2, col=trt)) +
                geom_point() +
                stat_ellipse() +
                theme_bw() +
                labs(title = "NMDS Plot")
              NMDS_all
              #### run if "invalid graphic state error" occurs: dev.off()              
              
              ggsave( "C:/Users/ashle/Documents/R/lakemorena/lakemorena/images/NMDS_treatment.jpg",NMDS_treatment)
              
              
          
              
              #####################
              #Bootstrapping and testing for differences between the groups 
              #Permanova test
              fit <- adonis(com ~ time, data=data2, permutations=999, method="bray")
              fit
              
              #p-value = 0.001 therefore we can assume these are different
              
              #####################
              #Check assumption of homogeneity of multivariate dispersion
              distances_data <- vegdist(com)
              anova(betadisper(distances_data, data2$time)) #0.52 homogeneity ASSUMED! yaya


