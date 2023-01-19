Github_loc<-"F:/My Drive/Tidewater/Tidewater Data Analysis"
dat_loc<-paste0(Github_loc,"/Trial_2021/")
plot_loc<-paste0(Github_loc,"/Plot_2021/")

setwd(Github_loc)

library(tidyverse)
library(Rmisc)
options(scipen=999)

###################################################################################
#### Data Preparation ##############################################################
###################################################################################

Cotton_data_2021 <- read.csv(paste0(dat_loc, "Cotton_stats.csv"))
colnames(Cotton_data_2021)[1]<- "Rep"
# Cotton_data_2021 <- Cotton_data_2021[-c(2)]
# Cotton_data_2021$Nrate <- as.factor(Cotton_data_2021$Nrate)

# Cotton_data_2021[,c(5:ncol(Cotton_data_2021))] <- sapply(Cotton_data_2021[,c(5:ncol(Cotton_data_2021))], function(x) as.numeric(as.character(x)))  

dat_loc2<-paste0(Github_loc,"/Data_2020/")
Cotton_data_2020 <- read.csv(paste0(dat_loc2, "Cotton2020_dat.csv"))
# Cotton_data_2020_NUE <- Cotton_data_2020 %>% 
#   filter(Rep <4)
Cotton_data_2020$Irrigation[Cotton_data_2020$Irrigation =="Full  "] <- "Full"
Cotton_data_2020$Irrigation[Cotton_data_2020_NUE$Irrigation =="Full  "] <- "Full"

Year = 2021

cotton_data <- function(Year){
  Irrigation_dat <- as.data.frame(unique(Cotton_data_2021$Irrigation))
  if (Year == 2021) {
    Irrigation_dat$Irrigation_amt <- c(0,2.2,1.5) 
    dat = Cotton_data_2021
  }else{
    Irrigation_dat$Irrigation_amt <- c(0,6.4,4.1)
    dat = Cotton_data_2020
  }
  
  colnames(Irrigation_dat)[1] <- "Irrigation"
  
  dat<- left_join(dat, Irrigation_dat, by = "Irrigation")
  rm(Irrigation_dat)
  
  Non_fertilized_summary<-dat %>% 
    filter(Nrate == 0) %>% 
    dplyr::group_by(Irrigation)%>%
    dplyr::summarise(Nuptake_mean0= mean(Nuptake,na.rm=TRUE),
                     Yield_mean0 = mean(Yield))
  
  Non_irrigated_summary<-dat %>% 
    filter(Irrigation == "None") %>% 
    dplyr::group_by(Nrate)%>%
    dplyr::summarise(Nuptake_mean0 = mean(Nuptake, na.rm=TRUE),
                     Yield_mean0 = mean(Yield))
  
  
  
  dat<- left_join(dat, Non_fertilized_summary[,c(1,2)], by = "Irrigation") 
  dat<- left_join(dat, Non_irrigated_summary[,c(1,3)], by = "Nrate") 
  dat$NUE <- round(ifelse(dat$Nrate == 0, 0, (dat$Nuptake - dat$Nuptake_mean0)/dat$Nrate),2)
  
  dat$irrWUE <- round(ifelse(dat$Irrigation == "None", 0, (dat$Yield - dat$Yield_mean0)/dat$Irrigation_amt),2)
  
  return(dat)
}

Cotton_data_2020 <- cotton_data(2020)
Cotton_data_2021 <- cotton_data(2021)

Cotton_data_2020$Year = 2020
Cotton_data_2021$Year = 2021

Combined_data <- rbind.data.frame(Cotton_data_2020,Cotton_data_2021)




############################################################################################################################
#Generate Models for both years

Cotton_data_2020$Nrate <- factor(Cotton_data_2020$Nrate)
Cotton_data_2020$Irrigation <- factor(Cotton_data_2020$Irrigation)
Cotton_data_2021$Nrate <- factor(Cotton_data_2021$Nrate)
Cotton_data_2021$Irrigation <- factor(Cotton_data_2021$Irrigation)
# Cotton_data_2020$Year =1


#########################################################################################
# Get all the models

# My thoughts here:
# 1. Don't merge the years Results between the years varies a lot for yield 
# 2. Removed outliers by combining the years. In this way we are less aggressive in removing them

Cotton_data_2020_NUE <- Cotton_data_2020 %>% 
  filter(Nrate != 0 & Rep < 4 )
Cotton_data_2021_NUE <- Cotton_data_2021 %>% 
  filter(Nrate != "0")

Cotton_data_2020_WUE <- Cotton_data_2020 %>% 
  filter(Irrigation != "None")
Cotton_data_2021_WUE <- Cotton_data_2021 %>% 
  filter(Irrigation != "None")


#############################################################################################################################
# Sumamry Tables
Yield_stats_20 <- summarySE(Cotton_data_2020, measurevar="Yield", groupvars=c("Nrate","Irrigation","Treatment"))
Yield_stats_21 <- summarySE(Cotton_data_2021, measurevar="Yield", groupvars=c("Nrate","Irrigation","Treatment"))

NUE_stats_20 <- summarySE(Cotton_data_2020_NUE, measurevar="NUE", groupvars=c("Nrate","Irrigation","Treatment"))
NUE_stats_21 <- summarySE(Cotton_data_2021, measurevar="NUE", groupvars=c("Nrate","Irrigation","Treatment"))

irrWUE_stats_20 <- summarySE(Cotton_data_2020, measurevar="irrWUE", groupvars=c("Nrate","Irrigation","Treatment"))
irrWUE_stats_21 <- summarySE(Cotton_data_2021, measurevar="irrWUE", groupvars=c("Nrate","Irrigation","Treatment"))
##################################################################################################################################


m1 = lmer(Yield ~ Nrate * Irrigation + (1 | Rep), data = Cotton_data_2020)
m2 = lmer(Yield ~ Nrate * Irrigation + (1 | Rep), data = Cotton_data_2021)
m3 = lmer(NUE ~ Nrate * Irrigation +(1 | Rep), data = Cotton_data_2020_NUE)
m4 = lmer(NUE ~ Nrate * Irrigation +(1 | Rep), data = Cotton_data_2021_NUE)
m5 = lmer(irrWUE ~ Nrate * Irrigation +(1 | Rep), data = Cotton_data_2020_WUE)
m6 = lmer(irrWUE ~ Nrate * Irrigation +(1 | Rep), data = Cotton_data_2021_WUE)



##########################
# Analysis of models
# 
# 1. Check ANOVA to see if the interaction is significant esle process to main effects
# 2. Apply predictmeans function for lsd and other summary 

a1 <-   anova(m1)  # Nrate and Irrigation significant (Main Effects)
a2 <-   anova(m2)  # Nrate significant
a3 <-   anova(m3)  # None significant
a4 <-   anova(m4)  # None significant
a5 <-   anova(m5)  # None significant
a6 <-   anova(m6)  # Nrate significant


a1
a2
a3
a4
a5
a6

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
write.csv(aov.table, "anova_Cotton.csv")

############################################

library(emmeans)
TK1 <- emmeans(m1, list(pairwise ~ Nrate), adjust = "tukey")
TK1_1 <- emmeans(m1, list(pairwise ~ Irrigation), adjust = "tukey")

TK2 <- emmeans(m2, list(pairwise ~ Nrate), adjust = "tukey")
TK3<- emmeans(m6, list(pairwise ~ Nrate), adjust = "tukey")

library(multcomp)


p_m1 <- predictmeans(m1, "Nrate", adj="tukey",pairwise=TRUE, barplot=FALSE, plot = FALSE)  
p_m1

p_m1$mean_table
cld(TK1)




p_m1 <- predictmeans(m1, "Irrigation", adj="tukey",pairwise=TRUE, barplot=FALSE, plot = FALSE)  
p_m1

p_m1$mean_table

cld(TK1_1)

# ______________________________________________________________________________________________


p_m2 <- predictmeans(m2, "Nrate", adj="tukey",pairwise=TRUE, barplot=FALSE, plot = FALSE)  
p_m2

p_m2$`Pairwise p-value`
cld(TK2)
# ______________________________________________________________________________________________


p_m3 <- predictmeans(m3, "Nrate", adj="tukey", barplot=FALSE, plot = FALSE)  
p_m3

p_m3$mean_table
cld(TK3)
################################################ 
Yield_20_sign <- cld(TK1)
Yield_20_sign$.group<- gsub("3", "a",Yield_20_sign$.group)
Yield_20_sign$.group<- gsub("2", "b",Yield_20_sign$.group)
Yield_20_sign$.group<- gsub("1", "c",Yield_20_sign$.group)

Yield_20_sign2 <- cld(TK1_1)

Yield_20_sign2$.group<- gsub("2", "a",Yield_20_sign2$.group)
Yield_20_sign2$.group<- gsub("1", "b",Yield_20_sign2$.group)



Yield_21_sign <- cld(TK2)
Yield_21_sign$.group<- gsub("3", "a",Yield_21_sign$.group)
Yield_21_sign$.group<- gsub("2", "b",Yield_21_sign$.group)
Yield_21_sign$.group<- gsub("1", "c",Yield_21_sign$.group)

WUE_21_sign <- cld(TK3)

WUE_21_sign$.group<- gsub("2", "a",WUE_21_sign$.group)
WUE_21_sign$.group<- gsub("1", "b",WUE_21_sign$.group)

######################################################
Yield_stats_20 <- summarySE(Cotton_data_2020, measurevar="Yield", groupvars=c("Nrate"))
Yield_stats_20 <-left_join(Yield_stats_20,Yield_20_sign[,c(1,7)], by = "Nrate")



Yield_stats_20_1 <- summarySE(Cotton_data_2020, measurevar="Yield", groupvars=c("Irrigation"))
Yield_stats_20_1 <-left_join(Yield_stats_20_1,Yield_20_sign2[,c(1,7)], by = "Irrigation")



Yield_stats_21 <- summarySE(Cotton_data_2021, measurevar="Yield", groupvars=c("Nrate"))
Yield_stats_21 <-left_join(Yield_stats_21,Yield_21_sign[,c(1,7)], by = "Nrate")


WUE_stats_21 <- summarySE(Cotton_data_2021_WUE, measurevar="irrWUE", groupvars=c("Nrate"))
WUE_stats_21 <-left_join(WUE_stats_21,WUE_21_sign[,c(1,7)], by = c("Nrate"= "Nrate"))


# Yield plots



# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Yield

Yield_plot_Cotton_2021<-Yield_stats_21 %>% 
  # filter(Irrigation != "3No Irrigation") %>%
  ggplot(aes(x=as.factor(Nrate), y=Yield))+
  geom_bar(stat = "identity", position=position_dodge(), colour="black", alpha = 0.5, fill = "#9ecae1")+
  geom_errorbar(aes(ymin=Yield-se, ymax=Yield+se),
                size=0.5,
                width=0.15,                    # Width of the error bars
                position=position_dodge(.9))+
  # geom_line(size=1)+
  theme_bw()+
  # scale_x_continuous(breaks=seq(80, 240, 80))+
  # scale_y_continuous(breaks=seq(0, 250, 50))+
  theme(plot.title = element_text(size=14),
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
  labs(title="Cotton Yield (2021)",
       x="Nitrogen Application (lb/acre)", y = "Yield  (bu/ac)",
       color=('Irrigation Treatment'),
       # shape=('Irrigation Treatment'),
       # linetype=('Irrigation Treatment')
  )+
  geom_text(aes(label=.group), position = position_dodge(0.90), size = 5,
            vjust=-0.8, hjust=-0.5, colour = "gray25")


Yield_plot_Cotton_2021
ggsave(paste0(plot_loc,"Sig_Yield_plot_Cotton_2021.png"),Yield_plot_Cotton_2021, width = 7, height = 5, units="in", dpi = 600)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#

Yield_plot_Cotton_2020<-Yield_stats_20 %>% 
  # filter(Irrigation != "3No Irrigation") %>%
  ggplot(aes(x=as.factor(Nrate), y=Yield))+
  geom_bar(stat = "identity", position=position_dodge(), colour="black",width = 0.75,  alpha = 0.5, fill = "#9ecae1")+
  geom_errorbar(aes(ymin=Yield-se, ymax=Yield+se),
                size=0.5,
                width=0.15,                    # Width of the error bars
                position=position_dodge(.9))+
  # geom_line(size=1)+
  theme_bw()+
  # scale_x_continuous(breaks=seq(80, 240, 80))+
   scale_y_continuous(limits= c(0,2000),breaks=seq(0, 2000, 500))+
  theme(plot.title = element_text(size=14),
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
  labs(title="Cotton Yield (2020)",
       x="Nitrogen Application (lb/acre)", y = "Yield  (bu/ac)",
       color=('Irrigation Treatment'),
       # shape=('Irrigation Treatment'),
       # linetype=('Irrigation Treatment')
  )+
  geom_text(aes(label=.group), position = position_dodge(0.90), size = 5,
            vjust=-0.8, hjust=-0.5, colour = "gray25")


Yield_plot_Cotton_2020
ggsave(paste0(plot_loc,"Sig_Yield_plot_Cotton_2020.png"),Yield_plot_Cotton_2021, width = 7, height = 5, units="in", dpi = 600)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Irrigation Significance

Irr_plot_dat<-dplyr::mutate(Yield_stats_20_1,  Irrigation = case_when(
  Irrigation == "None" ~ "No Irrigation",
  Irrigation == "Full" ~ "Full Irrigation",
  Irrigation == "Environmental "  ~ "Weather Informed\n Irrigation",
))


Yield_Irr_plot_Cotton_2020<-Irr_plot_dat %>% 
  # filter(Irrigation != "3No Irrigation") %>%
  ggplot(aes(x=as.factor(Irrigation), y=Yield))+
  geom_bar(stat = "identity", position=position_dodge(), colour="black", width = 0.75, alpha = 0.5, fill = "#9ecae1")+
  geom_errorbar(aes(ymin=Yield-se, ymax=Yield+se),
                size=0.5,
                width=0.15,                    # Width of the error bars
                position=position_dodge(.9))+
  # geom_line(size=1)+
  theme_bw()+
  # scale_x_continuous(breaks=seq(80, 240, 80))+
  scale_y_continuous(limits= c(0,2000),breaks=seq(0, 2000, 500))+
  theme(plot.title = element_text(size=14),
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
  labs(title="Cotton Yield (2020)",
        x="Irrigation Application",
       y = "Yield  (bu/ac)",
       color=('Irrigation Treatment'),
       # shape=('Irrigation Treatment'),
       # linetype=('Irrigation Treatment')
  )+
  geom_text(aes(label=.group), position = position_dodge(0.90), size = 5,
            vjust=-0.8, hjust=-0.5, colour = "gray25")


Yield_Irr_plot_Cotton_2020
ggsave(paste0(plot_loc,"Irr_Sig_Yield_plot_Cotton_2020.png"),Yield_Irr_plot_Cotton_2020, width = 7, height = 5, units="in", dpi = 600)

# ========================================================================================================================================
WUE_plot_Cotton_2021<-WUE_stats_21 %>% 
  # filter(Irrigation != "3No Irrigation") %>%
  ggplot(aes(x=as.factor(Nrate), y=irrWUE))+
  geom_bar(stat = "identity", position=position_dodge(), colour="black",width = 0.75,  alpha = 0.5, fill = "#9ecae1")+
  geom_errorbar(aes(ymin=irrWUE-se, ymax=irrWUE+se),
                size=0.5,
                width=0.15,                    # Width of the error bars
                position=position_dodge(.9))+
  # geom_line(size=1)+
  theme_bw()+
  # scale_x_continuous(breaks=seq(80, 240, 80))+
  # scale_y_continuous(breaks=seq(0, 20, 5))+
  theme(plot.title = element_text(size=14),
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
  labs(title="Cotton Water Use Efficiency (2021)",
       x="Nitrogen Application (lb/acre)", y = "Water Use Efficiency (bu/ac-in)",
       color=('Irrigation Treatment'),
       # shape=('Irrigation Treatment'),
       # linetype=('Irrigation Treatment')
  )+
  geom_text(aes(label=.group), position = position_dodge(0.90), size = 5,
            vjust=-0.35, hjust=-0.5, colour = "gray25")


WUE_plot_Cotton_2021
ggsave(paste0(plot_loc,"Sig_wue_plot_Cotton_2021.png"),WUE_plot_Cotton_2021, width = 7, height = 5, units="in", dpi = 600)

library(ggpubr)
p1<-   ggarrange(Yield_plot_Cotton_2020, Yield_Irr_plot_Cotton_2020,Yield_plot_Cotton_2021,WUE_plot_Cotton_2021, 
               
               # labels = c("A", "B", "C"),
               ncol = 2, nrow = 2)
p1

ggsave(paste0(plot_loc,"Sig_all_Cotton.png"),p1, width = 10, height = 8, units="in", dpi = 600)


####################################################################################################################################
# DATA PLOTS ALL

#######################################################################################################################################




#################################################################################################################################################
# Plots 2021

NUE_plot_dat<-dplyr::mutate(NUE_stats_21,  Irrigation = case_when(
  Treatment <=4 ~ "3No Irrigation",
  Treatment <=8 ~ "1Full Irrigation",
  Treatment <=12 ~ "2Precision Irrigation",
))


nue_plot_Cotton_2021<-NUE_plot_dat %>% 
  filter(Nrate != 0) %>% 
  ggplot(aes(x=as.factor(Nrate), y=NUE*100, fill= Irrigation))+
  geom_bar(stat = "identity", position=position_dodge(), colour="black", alpha = 0.5)+
  geom_errorbar(aes(ymin=NUE*100-se*100, ymax=NUE*100+se*100),
                size=0.5,
                width=0.15,                    # Width of the error bars
                position=position_dodge(.9))+
  # geom_line(size=1)+
  theme_bw()+
  # scale_x_continuous(breaks=seq(80, 240, 80))+
  theme(plot.title = element_text(size=14),
        legend.position="top",
        legend.title=element_blank(),
        legend.box = "horizontal",
        legend.background = element_rect(fill="white",
                                         size=0.5, linetype="solid",
                                         colour ="white"),
        legend.text=element_text(size=12),
        axis.text=element_text(size=14, colour="black"),
        axis.title=element_text(size=14, colour="black"),
        axis.line = element_line(colour = "black",
                                 size = 0.5, linetype = "solid"),
        axis.ticks = element_line(colour="black"),
        panel.grid.major=element_line(colour = "light grey"),
        panel.grid.minor=element_blank())+
  scale_fill_manual(values=c("#bdbdbd", "#a6bddb", "#fee0d2"), 
                    breaks=c("1Full Irrigation", "2Precision Irrigation", "3No Irrigation"),
                    labels=c("Full Irrigation", "Precision Irrigation", "No Irrigation"))+
  guides(colour = guide_legend(override.aes = list(size=5)))+
  labs(title="NUE (2021)",
       x="N Application (lb/acre)", y = "NUE (%)",
       color=('Irrigation Treatment'),
       # shape=('Irrigation Treatment'),
       # linetype=('Irrigation Treatment')
  )


nue_plot_Cotton_2021
# #ggsave(paste0(plot_loc,"nue_plot_Cotton_2021_Updated.png"),nue_plot_Cotton_2021, width = 7, height = 5, units="in", dpi = 600)

WUE_plot_dat<-dplyr::mutate(irrWUE_stats_21,  Irrigation = case_when(
  Treatment <=4 ~ "3No Irrigation",
  Treatment <=8 ~ "1Full Irrigation",
  Treatment <=12 ~ "2Precision Irrigation",
))


WUE_plot_Cotton_2021<-WUE_plot_dat %>% 
  filter(Irrigation != "3No Irrigation") %>%
  ggplot(aes(x=as.factor(Nrate), y=irrWUE, fill= Irrigation))+
  geom_bar(stat = "identity", position=position_dodge(), colour="black")+
  geom_errorbar(aes(ymin=irrWUE-se, ymax=irrWUE+se),
                size=0.5,
                width=0.15,                    # Width of the error bars
                position=position_dodge(.9))+
  # geom_line(size=1)+
  theme_bw()+
  # scale_x_continuous(breaks=seq(80, 240, 80))+
  # scale_y_continuous(breaks=seq(0, 20, 5))+
  theme(plot.title = element_text(size=14),
        legend.position="top",
        legend.title=element_blank(),
        legend.box = "horizontal",
        legend.background = element_rect(fill="white",
                                         size=0.5, linetype="solid",
                                         colour ="white"),
        legend.text=element_text(size=12),
        axis.text=element_text(size=14, colour="black"),
        axis.title=element_text(size=14, colour="black"),
        axis.line = element_line(colour = "black",
                                 size = 0.5, linetype = "solid"),
        axis.ticks = element_line(colour="black"),
        panel.grid.major=element_line(colour = "light grey"),
        panel.grid.minor=element_blank())+
  scale_fill_manual(values=c("#bdbdbd", "#a6bddb", "#fee0d2"), 
                    breaks=c("1Full Irrigation", "2Precision Irrigation", "3No Irrigation"),
                    labels=c("Full Irrigation", "Precision Irrigation", "No Irrigation"))+
  guides(colour = guide_legend(override.aes = list(size=5)))+
  labs(title="WUE (2021)",
       x="N Application (lb/acre)", y = "WUE (bu/ac-in)",
       color=('Irrigation Treatment'),
       # shape=('Irrigation Treatment'),
       # linetype=('Irrigation Treatment')
  )


WUE_plot_Cotton_2021
#ggsave(paste0(plot_loc,"wue_plot_Cotton_2021.png"),WUE_plot_Cotton_2021, width = 7, height = 5, units="in", dpi = 600)


Yield_plot_dat<-dplyr::mutate(Yield_stats_21,  Irrigation = case_when(
  Treatment <=4 ~ "3No Irrigation",
  Treatment <=8 ~ "1Full Irrigation",
  Treatment <=12 ~ "2Precision Irrigation",
))


Yield_plot_Cotton_2021<-Yield_plot_dat %>% 
  # filter(Irrigation != "3No Irrigation") %>%
  ggplot(aes(x=as.factor(Nrate), y=Yield, fill= Irrigation))+
  geom_bar(stat = "identity", position=position_dodge(), colour="black")+
  geom_errorbar(aes(ymin=Yield-se, ymax=Yield+se),
                size=0.5,
                width=0.15,                    # Width of the error bars
                position=position_dodge(.9))+
  # geom_line(size=1)+
  theme_bw()+
  # scale_x_continuous(breaks=seq(80, 240, 80))+
  # scale_y_continuous(breaks=seq(0, 250, 50))+
  theme(plot.title = element_text(size=14),
        legend.position="top",
        legend.title=element_blank(),
        legend.box = "horizontal",
        legend.background = element_rect(fill="white",
                                         size=0.5, linetype="solid",
                                         colour ="white"),
        legend.text=element_text(size=12),
        axis.text=element_text(size=14, colour="black"),
        axis.title=element_text(size=14, colour="black"),
        axis.line = element_line(colour = "black",
                                 size = 0.5, linetype = "solid"),
        axis.ticks = element_line(colour="black"),
        panel.grid.major=element_line(colour = "light grey"),
        panel.grid.minor=element_blank())+
  scale_fill_manual(values=c("#bdbdbd", "#a6bddb", "#fee0d2"), 
                    breaks=c("1Full Irrigation", "2Precision Irrigation", "3No Irrigation"),
                    labels=c("Full Irrigation", "Precision Irrigation", "No Irrigation"))+
  guides(colour = guide_legend(override.aes = list(size=5)))+
  labs(title="Yield (2021)",
       x="N Application (lb/acre)", y = "Yield  (bu/ac)",
       color=('Irrigation Treatment'),
       # shape=('Irrigation Treatment'),
       # linetype=('Irrigation Treatment')
  )


Yield_plot_Cotton_2021
#ggsave(paste0(plot_loc,"Yield_plot_Cotton_2021.png"),Yield_plot_Cotton_2021, width = 7, height = 5, units="in", dpi = 600)


#################################################################################################################################################
# Plots 2020

NUE_plot_dat<-dplyr::mutate(NUE_stats_20,  Irrigation = case_when(
  Treatment <=4 ~ "3No Irrigation",
  Treatment <=8 ~ "1Full Irrigation",
  Treatment <=12 ~ "2Precision Irrigation",
))


nue_plot_Cotton_2020<-NUE_plot_dat %>% 
  filter(Nrate != 0) %>% 
  ggplot(aes(x=as.factor(Nrate), y=NUE*100, fill= Irrigation))+
  geom_bar(stat = "identity", position=position_dodge(), colour="black")+
  geom_errorbar(aes(ymin=NUE*100-se*100, ymax=NUE*100+se*100),
                size=0.5,
                width=0.15,                    # Width of the error bars
                position=position_dodge(.9))+
  # geom_line(size=1)+
  theme_bw()+
  # scale_y_continuous(breaks=seq(0, 100, 20))+
  theme(plot.title = element_text(size=14),
        legend.position="top",
        legend.title=element_blank(),
        legend.box = "horizontal",
        legend.background = element_rect(fill="white",
                                         size=0.5, linetype="solid",
                                         colour ="white"),
        legend.text=element_text(size=12),
        axis.text=element_text(size=14, colour="black"),
        axis.title=element_text(size=14, colour="black"),
        axis.line = element_line(colour = "black",
                                 size = 0.5, linetype = "solid"),
        axis.ticks = element_line(colour="black"),
        panel.grid.major=element_line(colour = "light grey"),
        panel.grid.minor=element_blank())+
  scale_fill_manual(values=c("#bdbdbd", "#a6bddb", "#fee0d2"), 
                    breaks=c("1Full Irrigation", "2Precision Irrigation", "3No Irrigation"),
                    labels=c("Full Irrigation", "Precision Irrigation", "No Irrigation"))+
  guides(colour = guide_legend(override.aes = list(size=5)))+
  labs(title="NUE (2020)",
       x="N Application (lb/acre)", y = "NUE (%)",
       color=('Irrigation Treatment'),
       # shape=('Irrigation Treatment'),
       # linetype=('Irrigation Treatment')
  )


nue_plot_Cotton_2020
#ggsave(paste0(plot_loc,"nue_plot_Cotton_2020_Updated.png"),nue_plot_Cotton_2020, width = 7, height = 5, units="in", dpi = 600)

WUE_plot_dat<-dplyr::mutate(irrWUE_stats_20,  Irrigation = case_when(
  Treatment <=4 ~ "3No Irrigation",
  Treatment <=8 ~ "1Full Irrigation",
  Treatment <=12 ~ "2Precision Irrigation",
))


WUE_plot_Cotton_2020<-WUE_plot_dat %>% 
  filter(Irrigation != "3No Irrigation") %>%
  ggplot(aes(x=as.factor(Nrate), y=irrWUE, fill= Irrigation))+
  geom_bar(stat = "identity", position=position_dodge(), colour="black")+
  geom_errorbar(aes(ymin=irrWUE-se, ymax=irrWUE+se),
                size=0.5,
                width=0.15,                    # Width of the error bars
                position=position_dodge(.9))+
  # geom_line(size=1)+
  theme_bw()+
  # scale_x_continuous(breaks=seq(80, 240, 80))+
   scale_y_continuous(limits = c(-200,150),breaks=seq(-200, 150, 100))+
  theme(plot.title = element_text(size=14),
        legend.position="top",
        legend.title=element_blank(),
        legend.box = "horizontal",
        legend.background = element_rect(fill="white",
                                         size=0.5, linetype="solid",
                                         colour ="white"),
        legend.text=element_text(size=12),
        axis.text=element_text(size=14, colour="black"),
        axis.title=element_text(size=14, colour="black"),
        axis.line = element_line(colour = "black",
                                 size = 0.5, linetype = "solid"),
        axis.ticks = element_line(colour="black"),
        panel.grid.major=element_line(colour = "light grey"),
        panel.grid.minor=element_blank())+
  scale_fill_manual(values=c("#bdbdbd", "#a6bddb", "#fee0d2"), 
                    breaks=c("1Full Irrigation", "2Precision Irrigation", "3No Irrigation"),
                    labels=c("Full Irrigation", "Precision Irrigation", "No Irrigation"))+
  guides(colour = guide_legend(override.aes = list(size=5)))+
  labs(title="WUE(2020)",
       x="N Application (lb/acre)", y = "WUE (bu/ac-in)",
       color=('Irrigation Treatment'),
       # shape=('Irrigation Treatment'),
       # linetype=('Irrigation Treatment')
  )


WUE_plot_Cotton_2020
#ggsave(paste0(plot_loc,"wue_plot_Cotton_2020.png"),WUE_plot_Cotton_2020, width = 7, height = 5, units="in", dpi = 600)


Yield_plot_dat<-dplyr::mutate(Yield_stats_20,  Irrigation = case_when(
  Treatment <=4 ~ "3No Irrigation",
  Treatment <=8 ~ "1Full Irrigation",
  Treatment <=12 ~ "2Precision Irrigation",
))


Yield_plot_Cotton_2020<-Yield_plot_dat %>% 
  # filter(Irrigation != "3No Irrigation") %>%
  ggplot(aes(x=as.factor(Nrate), y=Yield, fill= Irrigation))+
  geom_bar(stat = "identity", position=position_dodge(), colour="black")+
  geom_errorbar(aes(ymin=Yield-se, ymax=Yield+se),
                size=0.5,
                width=0.15,                    # Width of the error bars
                position=position_dodge(.9))+
  # geom_line(size=1)+
  theme_bw()+
  # scale_x_continuous(breaks=seq(80, 240, 80))+
   scale_y_continuous(limits= c(0,2000),breaks=seq(0, 2000, 500))+
  theme(plot.title = element_text(size=14),
        legend.position="top",
        legend.title=element_blank(),
        legend.box = "horizontal",
        legend.background = element_rect(fill="white",
                                         size=0.5, linetype="solid",
                                         colour ="white"),
        legend.text=element_text(size=12),
        axis.text=element_text(size=14, colour="black"),
        axis.title=element_text(size=14, colour="black"),
        axis.line = element_line(colour = "black",
                                 size = 0.5, linetype = "solid"),
        axis.ticks = element_line(colour="black"),
        panel.grid.major=element_line(colour = "light grey"),
        panel.grid.minor=element_blank())+
  scale_fill_manual(values=c("#bdbdbd", "#a6bddb", "#fee0d2"), 
                    breaks=c("1Full Irrigation", "2Precision Irrigation", "3No Irrigation"),
                    labels=c("Full Irrigation", "Precision Irrigation", "No Irrigation"))+
  guides(colour = guide_legend(override.aes = list(size=5)))+
  labs(title="Yield (2020)",
       x="N Application (lb/acre)", y = "Yield  (bu/ac)",
       color=('Irrigation Treatment'),
       # shape=('Irrigation Treatment'),
       # linetype=('Irrigation Treatment')
  )


Yield_plot_Cotton_2020
#ggsave(paste0(plot_loc,"Yield_plot_Cotton_2020.png"),Yield_plot_Cotton_2020, width = 7, height = 5, units="in", dpi = 600)

library("patchwork")

ggp_all <- (Yield_plot_Cotton_2020 + Yield_plot_Cotton_2021) / (nue_plot_Cotton_2020+nue_plot_Cotton_2021) / (WUE_plot_Cotton_2020+ WUE_plot_Cotton_2021) +    # Create grid of plots with title
  plot_annotation(title = "COTTON RESULTS")&
  theme(plot.title = element_text(size = 16,hjust = 0.5))
ggp_all

ggsave(paste0(plot_loc,"All-Cotton.png"),ggp_all, width = 12, height = 12, units="in", dpi = 600)
