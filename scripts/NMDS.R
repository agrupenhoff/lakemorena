library(wesanderson)



#NMDS CREATION

Transect_LM <- import("C:/Users/ashle/Documents/R/lakemorena/lakemorena/data/clean/Transect_LM.csv")

Transect_separate <- Transect_LM %>% 
  separate(plot_time, c("plot", "trt","time"),"_") 


#make community matrix
com = Transect_separate[,4:ncol(Transect_separate)]

#turn abundance data into matrix
m_com = as.matrix(com)

#set sed before metaMDS command to obtain same results each time you run nmds plot with dataset
set.seed(123)
nmds = metaMDS(m_com, distance = "bray")
nmds
            #stress = 0.2313909

#plot simple
plot(nmds)

#plot nice with ggplot

                            #obtain coordinates for NMDS1 and NMDS2 axes (x,y coordinates)
                            data.scores = as.data.frame(scores(nmds))
                            
                            #add columns to data frame 
                            data.scores$plot = Transect_separate$plot
                            data.scores$trt = Transect_separate$trt
                            data.scores$time = Transect_separate$time
                            
                            head(data.scores)
                            
                            library(ggplot2)
                            
                            xx = ggplot(data.scores, aes(x = NMDS1, y = NMDS2)) + 
                              geom_point(size = 4, aes( shape = trt, colour = time))+ 
                              theme(axis.text.y = element_text(colour = "black", size = 12, face = "bold"), 
                                    axis.text.x = element_text(colour = "black", face = "bold", size = 12), 
                                    legend.text = element_text(size = 12, face ="bold", colour ="black"), 
                                    legend.position = "right", axis.title.y = element_text(face = "bold", size = 14), 
                                    axis.title.x = element_text(face = "bold", size = 14, colour = "black"), 
                                    legend.title = element_text(size = 14, colour = "black", face = "bold"), 
                                    panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
                                    legend.key=element_blank()) + 
                              labs(x = "NMDS1", colour = "time", y = "NMDS2", shape = "trt")  + 
                              scale_colour_manual(values = wes_palette(name ="IsleofDogs1")) 
                            
                            xx
                            
                            ggsave(xx, "C:/Users/ashle/Documents/R/lakemorena/lakemorena/images/NMDS_simple.jpg")
                            
                            

  #add ellipses
                            
                            ordiplot(nmds, type = "n", main = "ellipses")
                            orditorp(nmds, display = "sites", labels = F, pch = c(16, 8, 17, 18) [as.numeric(Transect_separate$trt)], col = c("green", "blue") [as.numeric(Transect_separate$time)], cex = 1)
                            ordiellipse(nmds, groups =  Transect_separate$trt, draw = "polygon", lty = 1, col = "grey90")



