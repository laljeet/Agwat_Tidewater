Github_loc<-"F:/My Drive/Tidewater/Tidewater Data Analysis"
dat_loc<-paste0(Github_loc,"/Trial_2021/")
plot_loc<-paste0(Github_loc,"/Plot_2021/")
options(scipen = 9999)
setwd(Github_loc)
 library(stargazer)
library(tidyverse)
 library(Rmisc)
options(scipen=999)
 library(predictmeans)
# library(agricolae)
# library(doebioresearch)
library(lmerTest)
# library(AICcmodavg)
###################################################################################
#### Data Preparation ##############################################################
###################################################################################

corn_stats_21 <- read.csv(paste0(dat_loc, "corn_stats.csv"))
colnames(corn_stats_21)[1]<- "Rep"
corn_stats_21 <- corn_stats_21[-c(2)]
corn_stats_21<-dplyr::mutate(corn_stats_21,  Rep = case_when(
  Rep ==1  ~ 5,
  Rep ==2  ~ 6,
  Rep ==3  ~ 7,
  Rep ==4  ~ 8,
))

corn_stats_21 <- corn_stats_21[,-c(5,6)]

# corn_stats_21[,c(5:ncol(corn_stats_21))] <- sapply(corn_stats_21[,c(5:ncol(corn_stats_21))], function(x) as.numeric(as.character(x)))  

dat_loc2<-paste0(Github_loc,"/Data_2020/")
corn_stats_20 <- read.csv(paste0(dat_loc2, "Corn2020_dat.csv"))
corn_stats_20 <- corn_stats_20[-c(5:7)]


source('F:/My Drive/Tidewater/Tidewater Data Analysis/NUE_WUE_FUNCTION.R', echo=TRUE)

Corn_data_2020 <- Corn_data(2020)
Corn_data_2021 <- Corn_data(2021)

Corn_data_2020$Year = 2020
Corn_data_2021$Year = 2021
Corn_data_2021 <- filter(Corn_data_2021, Nuptake < 390)  ##########Remove outliers

rm(corn_stats_21,corn_stats_20)

#############################################################################################################################
# Summary Tables
Yield_stats_20 <- summarySE(Corn_data_2020, measurevar="Yield", groupvars=c("Nrate","Irrigation","Treatment"))
Yield_stats_21 <- summarySE(Corn_data_2021, measurevar="Yield", groupvars=c("Nrate","Irrigation","Treatment"))

NUE_stats_20 <- summarySE(Corn_data_2020_NUE, measurevar="NUE", groupvars=c("Nrate","Irrigation","Treatment"))
NUE_stats_21 <- summarySE(Corn_data_2021, measurevar="NUE", groupvars=c("Nrate","Irrigation","Treatment"))

irrWUE_stats_20 <- summarySE(Corn_data_2020, measurevar="irrWUE", groupvars=c("Nrate","Irrigation","Treatment"))
irrWUE_stats_21 <- summarySE(Corn_data_2021, measurevar="irrWUE", groupvars=c("Nrate","Irrigation","Treatment"))

############################################################################################################################
#Generate Models for both years

Corn_data_2020$Nrate <- factor(Corn_data_2020$Nrate)
Corn_data_2020$Irrigation <- factor(Corn_data_2020$Irrigation)
Corn_data_2021$Nrate <- factor(Corn_data_2021$Nrate)
Corn_data_2021$Irrigation <- factor(Corn_data_2021$Irrigation)
# Corn_data_2020$Year =1


#########################################################################################
# Get all the models

# My thoughts here:
# 1. Don't merge the years Results between the years varies a lot for yield 
# 2. Removed outliers by combining the years. In this way we are less aggressive in removing them

  
  m1 = lmer(Yield ~ Nrate * Irrigation + (1 | Rep), data = Corn_data_2020)
  m2 = lmer(Yield ~ Nrate * Irrigation + (1 | Rep), data = Corn_data_2021)
  
  Corn_data_2020_NUE <- Corn_data_2020 %>% 
    filter(Nrate != "0")
  Corn_data_2021_NUE <- Corn_data_2021 %>% 
    filter(Nrate != "0")
  
  m3 = lmer(NUE ~ Nrate * Irrigation +(1 | Rep), data = Corn_data_2020_NUE)
  m4 = lmer(NUE ~ Nrate * Irrigation +(1 | Rep), data = Corn_data_2021_NUE)
  
  Corn_data_2020_WUE <- Corn_data_2020 %>% 
    filter(Irrigation != "None")
  Corn_data_2021_WUE <- Corn_data_2021 %>% 
    filter(Irrigation != "None")
  
  m5 = lmer(irrWUE ~ Nrate * Irrigation +(1 | Rep), data = Corn_data_2020_WUE)
  m6 = lmer(irrWUE ~ Nrate * Irrigation +(1 | Rep), data = Corn_data_2021_WUE)
  
 
  
##########################
# Analysis of models
# 
# 1. Check ANOVA to see if the interaction is significant else process to main effects
# 2. Apply predictmeans function for lsd and other summary 

  a1 <-   anova(m1)  # Nrate significant
  a2 <-   anova(m2)  # Nrate significant
  a3 <-   anova(m3)  # Interaction significant
  a4 <-   anova(m4)  # None
  a5 <-   anova(m5)  # None
  a6 <-   anova(m6)  # None
  
  library(emmeans)
  TK1 <- emmeans(m1, list(pairwise ~ Nrate), adjust = "tukey")
  TK2 <- emmeans(m2, list(pairwise ~ Nrate), adjust = "tukey")
  TK3 <- emmeans(m3, list(pairwise ~ Nrate * Irrigation), adjust = "tukey")
  # TK4 <- emmeans(m4, list(pairwise ~ Nrate), adjust = "tukey")
  # TK5 <- emmeans(m5, list(pairwise ~ Irrigation), adjust = "tukey")
  # TK6 <- emmeans(m6, list(pairwise ~ Irrigation), adjust = "tukey")
 
  
  library(multcomp)
  cld(TK1)

  p_m1 <- predictmeans(m1, "Nrate", adj="tukey",pairwise=TRUE, barplot=FALSE, plot = FALSE)  
  p_m1
  
  p_m1$mean_table
  
  # ______________________________________________________________________________________________
  cld(TK2)
  
  p_m2 <- predictmeans(m2, "Nrate", adj="tukey",pairwise=TRUE, barplot=FALSE, plot = FALSE)  
  p_m2
  
  p_m2$`Pairwise p-value`
  
  # ______________________________________________________________________________________________
  cld(TK3)
  
  p_m3 <- predictmeans(m3, "Nrate:Irrigation", adj="tukey", barplot=FALSE, plot = FALSE)  
  p_m3
  
  p_m3$mean_table
  
  # # ______________________________________________________________________________________________
  # cld(TK4)
  # 
  # p_m4 <- predictmeans(m4, "Nrate", adj="tukey", barplot=FALSE, plot = FALSE)  
  # p_m4
  # 
  # p_m4$mean_table
  # 
  # # ______________________________________________________________________________________________
  # cld(TK5)
  # 
  # p_m5 <- predictmeans(m5, "Irrigation", adj="tukey", barplot=FALSE, plot = FALSE)  
  # p_m5
  # 
  # p_m5$mean_table
  # 
  # # ______________________________________________________________________________________________
  # cld(TK6)
  # 
  # p_m6 <- predictmeans(m6, "Irrigation", adj="tukey", barplot=FALSE, plot = FALSE)  
  # p_m6
  # 
  # p_m6$mean_table
  # 
  # __________________________________________________________________________________________________
 


 
 Yield_20_sign <- cld(TK1)
 Yield_20_sign$.group<- gsub("3", "a",Yield_20_sign$.group)
 Yield_20_sign$.group<- gsub("2", "b",Yield_20_sign$.group)
 Yield_20_sign$.group<- gsub("1", "c",Yield_20_sign$.group)

 Yield_21_sign <- cld(TK2)
 Yield_21_sign$.group<- gsub("4", "a",Yield_21_sign$.group)
 Yield_21_sign$.group<- gsub("3", "b",Yield_21_sign$.group)
 Yield_21_sign$.group<- gsub("2", "c",Yield_21_sign$.group)
 Yield_21_sign$.group<- gsub("1", "d",Yield_21_sign$.group)
 
 NUE_20_sign <- cld(TK3)

 NUE_20_sign$.group<- gsub("2", "a",NUE_20_sign$.group)
 NUE_20_sign$.group<- gsub("1", "b",NUE_20_sign$.group)
 
 # save(Yield_20_sign,Yield_21_sign,NUE_20_sign, file = "Corn_signs.Rdata")
 
 #######################################
 # Do manual
 
 # WUE_20_sign <- cld(TK5)
 # WUE_20_sign$.group<- gsub("2", "a",WUE_20_sign$.group)
 # WUE_20_sign$.group<- gsub("1", "b",WUE_20_sign$.group)
 # 
 # WUE_21_sign <- cld(TK6)
 # WUE_21_sign$.group<- gsub("2", "a",WUE_21_sign$.group)
 # WUE_21_sign$.group<- gsub("1", "b",WUE_21_sign$.group)
 
 
 #________________________________________________________________________________________________
  #############################################################################################################################
  # Summary Tables
Yield_stats_20 <- summarySE(Corn_data_2020, measurevar="Yield", groupvars=c("Nrate"))
Yield_stats_20 <-left_join(Yield_stats_20,Yield_20_sign[,c(1,7)], by = "Nrate")
  
Yield_stats_21 <- summarySE(Corn_data_2021, measurevar="Yield", groupvars=c("Nrate"))
Yield_stats_21 <-left_join(Yield_stats_21,Yield_21_sign[,c(1,7)], by = "Nrate")
 
 
NUE_stats_20 <- summarySE(Corn_data_2020_NUE, measurevar="NUE", groupvars=c("Nrate","Irrigation"))
NUE_stats_20 <-left_join(NUE_stats_20,NUE_20_sign[,c(1,2,8)], by = c("Nrate"= "Nrate","Irrigation"= "Irrigation"))


# NUE_stats_21 <- summarySE(Corn_data_2021, measurevar="NUE", groupvars=c("Nrate"))
# NUE_stats_21 <-left_join(NUE_stats_21,NUE_21_sign[,c(1,7)], by = "Nrate")
# 
# 
#   irrWUE_stats_20 <- summarySE(Corn_data_2020, measurevar="irrWUE", groupvars=c("Irrigation"))
#   irrWUE_stats_20 <-left_join(irrWUE_stats_20,WUE_20_sign[,c(1,7)], by = "Irrigation")
#   
#   irrWUE_stats_21 <- summarySE(Corn_data_2021, measurevar="irrWUE", groupvars=c("Irrigation"))
#   irrWUE_stats_21 <-left_join(irrWUE_stats_21,WUE_21_sign[,c(1,7)], by = "Irrigation")


###############################################################################
  # Significance plot
  
  # Yield plots
  
  
  
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # Yield

  Yield_plot_CORN_2021<-Yield_stats_21 %>% 
    # filter(Irrigation != "3No Irrigation") %>%
    ggplot(aes(x=as.factor(Nrate), y=Yield))+
    geom_bar(stat = "identity", position=position_dodge(), colour="black", width = 0.75, alpha = 0.5, fill = "#9ecae1")+
    geom_errorbar(aes(ymin=Yield-se, ymax=Yield+se),
                  size=0.5,
                  width=0.15,                    # Width of the error bars
                  position=position_dodge(.9))+
    # geom_line(size=1)+
    theme_bw()+
    # scale_x_continuous(breaks=seq(80, 240, 80))+
    scale_y_continuous(breaks=seq(0, 250, 50))+
    theme(plot.title = element_text(size=12),
          legend.position="top",
          legend.title=element_blank(),
          legend.box = "horizontal",
          legend.background = element_rect(fill="white",
                                           size=0.5, linetype="solid",
                                           colour ="white"),
          legend.text=element_text(size=12),
          axis.text=element_text(size=12, colour="black"),
          axis.title=element_text(size=12, colour="black"),
          axis.line = element_line(colour = "black",
                                   size = 0.5, linetype = "solid"),
          axis.ticks = element_line(colour="black"),
          panel.grid.major=element_line(colour = "light grey"),
          panel.grid.minor=element_blank())+
    scale_fill_manual(values=c("#bdbdbd", "#a6bddb", "#fee0d2"), 
                      breaks=c("1Full Irrigation", "2Precision Irrigation", "3No Irrigation"),
                      labels=c("Full Irrigation", "Precision Irrigation", "No Irrigation"))+
    guides(colour = guide_legend(override.aes = list(size=5)))+
    labs(title="Corn Yield (2021)",
         x="N Application (lb/acre)", y = "Yield  (bu/ac)",
         color=('Irrigation Treatment'),
         # shape=('Irrigation Treatment'),
         # linetype=('Irrigation Treatment')
    )+
    geom_text(aes(label=.group), position = position_dodge(0.90), size = 5,
              vjust=-0.8, hjust=-0.5, colour = "gray25")
  
  
  Yield_plot_CORN_2021
  #ggsave(paste0(plot_loc,"Sig_Yield_plot_CORN_2021.png"),Yield_plot_CORN_2021, width = 7, height = 5, units="in", dpi = 600)
  
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #
  
  Yield_plot_CORN_2020<-Yield_stats_20 %>% 
    # filter(Irrigation != "3No Irrigation") %>%
    ggplot(aes(x=as.factor(Nrate), y=Yield))+
    geom_bar(stat = "identity", position=position_dodge(), colour="black",width = 0.75, alpha = 0.5, fill = "#9ecae1")+
    geom_errorbar(aes(ymin=Yield-se, ymax=Yield+se),
                  size=0.5,
                  width=0.15,                    # Width of the error bars
                  position=position_dodge(.9))+
    # geom_line(size=1)+
    theme_bw()+
    # scale_x_continuous(breaks=seq(80, 240, 80))+
    scale_y_continuous(limits = c(0,250),breaks=seq(0, 250, 50))+
    theme(plot.title = element_text(size=12),
          legend.position="top",
          legend.title=element_blank(),
          legend.box = "horizontal",
          legend.background = element_rect(fill="white",
                                           size=0.5, linetype="solid",
                                           colour ="white"),
          legend.text=element_text(size=12),
          axis.text=element_text(size=12, colour="black"),
          axis.title=element_text(size=12, colour="black"),
          axis.line = element_line(colour = "black",
                                   size = 0.5, linetype = "solid"),
          axis.ticks = element_line(colour="black"),
          panel.grid.major=element_line(colour = "light grey"),
          panel.grid.minor=element_blank())+
    scale_fill_manual(values=c("#bdbdbd", "#a6bddb", "#fee0d2"), 
                      breaks=c("1Full Irrigation", "2Precision Irrigation", "3No Irrigation"),
                      labels=c("Full Irrigation", "Precision Irrigation", "No Irrigation"))+
    guides(colour = guide_legend(override.aes = list(size=5)))+
    labs(title="Corn Yield (2020)",
         x="N Application (lb/acre)", y = "Yield  (bu/ac)",
         color=('Irrigation Treatment'),
         # shape=('Irrigation Treatment'),
         # linetype=('Irrigation Treatment')
    )+
    geom_text(aes(label=.group), position = position_dodge(0.90), size = 5,
              vjust=-0.8, hjust=-0.5, colour = "gray25")
  
  
  Yield_plot_CORN_2020
  #ggsave(paste0(plot_loc,"Sig_Yield_plot_CORN_2020.png"),Yield_plot_CORN_2021, width = 7, height = 5, units="in", dpi = 600)
  # ========================================================================================================================================
  
  NUE_plot_dat<-dplyr::mutate(NUE_stats_20,  Irrigation = case_when(
    Irrigation == "None" ~ "3No Irrigation",
    Irrigation == "Full" ~ "1Full Irrigation",
    Irrigation == "Environmental "  ~ "2Precision Irrigation",
  ))
  
  nue_plot_CORN_2020<-NUE_plot_dat %>% 
    filter(Nrate != 0) %>% 
    ggplot(aes(x=as.factor(Nrate), y=NUE*100, fill= Irrigation))+
    geom_bar(stat = "identity", position=position_dodge(), colour="black",width = 0.75,  alpha = 0.5)+
    geom_errorbar(aes(ymin=NUE*100-se*100, ymax=NUE*100+se*100),
                  size=0.5,
                  width=0.15,                    # Width of the error bars
                  position=position_dodge(.9))+
    # geom_line(size=1)+
    theme_bw()+
     scale_y_continuous(limits = c(0,100),breaks=seq(0, 100, 25))+
    theme(plot.title = element_text(size=12),
          legend.position="top",
          legend.title=element_blank(),
          legend.box = "horizontal",
          legend.background = element_rect(fill="white",
                                           size=0.5, linetype="solid",
                                           colour ="white"),
          legend.text=element_text(size=12),
          axis.text=element_text(size=12, colour="black"),
          axis.title=element_text(size=12, colour="black"),
          axis.line = element_line(colour = "black",
                                   size = 0.5, linetype = "solid"),
          axis.ticks = element_line(colour="black"),
          panel.grid.major=element_line(colour = "light grey"),
          panel.grid.minor=element_blank())+
    scale_fill_manual(values=c("#bdbdbd", "#a6bddb", "#fee0d2"), 
                      breaks=c("1Full Irrigation", "2Precision Irrigation", "3No Irrigation"),
                      labels=c("Full Irrigation", "Precision Irrigation", "No Irrigation"))+
    guides(colour = guide_legend(override.aes = list(size=5)))+
    labs(title="Corn Nitrogen Use Efficiency (2020)",
         x="N Application (lb/acre)", y = "NUE (%)",
         color=('Irrigation Treatment'),
         # shape=('Irrigation Treatment'),
         # linetype=('Irrigation Treatment')
    )+
    geom_text(aes(label=.group), position = position_dodge(0.9), size = 5, 
              vjust=-2.75, hjust=0.75, colour = "gray25")
  
  
  nue_plot_CORN_2020
   #ggsave(paste0(plot_loc,"Sig_nue_plot_CORN_2020_Updated.png"),nue_plot_CORN_2020, width = 7, height = 5, units="in", dpi = 600)
   library(ggpubr)
  p<-  ggarrange(nue_plot_CORN_2020,
   ggarrange(Yield_plot_CORN_2020, Yield_plot_CORN_2021), 
             
             # labels = c("A", "B", "C"),
             ncol = 1, nrow = 2)
  p

   
#####################################################################################################
   # ANOVA TABLES

  
  ##################################
  # All ANOVA tables
  final <- list(a1,a2,a3,a4,a5,a6)
  
  aov.table = rlist::list.rbind(final)
  aov.table <- aov.table[,-c(3,5,7,9,11)]

  aov.table$sign[aov.table$`Pr(>F)` < 0.05] <- "*" 
  aov.table$sign[aov.table$`Pr(>F)` < 0.01] <- "**"
  aov.table$sign[aov.table$`Pr(>F)` < 0.001] <- "***"
  aov.table$sign[aov.table$`Pr(>F)` > 0.05] <- ""
  
  aov.table$`Pr(>F)` <- round(aov.table$`Pr(>F)` ,3)
  
  aov.table[[4]] = paste(aov.table[[4]], 
                          ifelse(is.na(aov.table[[4]]), " ", aov.table[[5]]))
  
  
  # colnames(aov.table)[2:7] <- m_names
  colnames(aov.table)[1] <- "DF"

  write.csv(aov.table, "All_Anova_Corn.csv")
#############################################################################################
  # Based on significance proceed for post ad hoc 
  
  # p_m1 <- predictmeans(m1, "Nrate", adj="bonferroni",pairwise=TRUE, barplot=FALSE, plot = FALSE)                 # 
  # 
  # p_m2 <- predictmeans(m2, "Nrate", adj="bonferroni",pairwise=TRUE, barplot=FALSE, plot = FALSE)
  # 
  # p_m3 <- predictmeans(m3, "Nrate:Irrigation", adj="bonferroni",pairwise=TRUE, barplot=FALSE, plot = FALSE)
  # 
  # p_m4 <- predictmeans(m4, "Nrate", adj="bonferroni",pairwise=TRUE, barplot=FALSE, plot = FALSE)
  # 
  # p_m5 <- predictmeans(m5, "Irrigation", adj="bonferroni",pairwise=TRUE, barplot=FALSE, plot = FALSE)
  # 
  # p_m6 <- predictmeans(m6, "Irrigation", adj="bonferroni",pairwise=TRUE, barplot=FALSE, plot = FALSE)
  
  
#################################################################################################

  

  residplot(m1, group = "none", level = 1, slope = FALSE, id = FALSE, newwd=TRUE,ask=FALSE)
  residplot(m2, group = "none", level = 1, slope = FALSE, id = FALSE, newwd=TRUE, ask=FALSE)
  residplot(m3, group = "none", level = 1, slope = FALSE, id = FALSE, newwd=TRUE, ask=FALSE)
  residplot(m4, group = "none", level = 1, slope = FALSE, id = FALSE, newwd=TRUE, ask=FALSE)
  residplot(m5, group = "none", level = 1, slope = FALSE, id = FALSE, newwd=TRUE, ask=FALSE)
  residplot(m6, group = "none", level = 1, slope = FALSE, id = FALSE, newwd=TRUE, ask=FALSE)
  

###################################################################################################
  # Cooks distance
  
# CookD(m1, group=NULL, plot=TRUE, idn=5, newwd=TRUE)
# 
# cooksd <- cooks.distance(m1)
# sample_size <- nrow(Corn_data_2020)
# plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
# abline(h = 4/sample_size, col="red")  # add cutoff line
# text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4/sample_size, names(cooksd),""), col="red")  # add labels




