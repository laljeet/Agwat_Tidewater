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

##################################################################################
# SI UNIT CONVERSIONS

corn_stats_20$Yield  <- round(corn_stats_20$Yield * 63,2)
corn_stats_20$Nuptake  <- round(corn_stats_20$Nuptake * 1.12085,2)
corn_stats_20$Nrate  <- round(corn_stats_20$Nrate * 1.12085,digits = -1)


corn_stats_21$Yield  <- round(corn_stats_21$Yield * 63,2)
corn_stats_21$Nuptake  <- round(corn_stats_21$Nuptake * 1.12085,2)

corn_stats_21$Nrate <- round((corn_stats_21$Nrate * 1.12085),digits = -1)

source('F:/My Drive/Tidewater/Tidewater Data Analysis/NUE_WUE_FUNCTION_SI.R', echo=TRUE)

Corn_data_2020 <- Corn_data(2020)
Corn_data_2021 <- Corn_data(2021)

Corn_data_2020<-dplyr::mutate(Corn_data_2020,  Irrigation = case_when(
  Treatment <=4 ~ "No Irrigation",
  Treatment <=8 ~ "Full Irrigation",
  Treatment <=12 ~ "Weather Informed Irrigation",
))

Corn_data_2021<-dplyr::mutate(Corn_data_2021,  Irrigation = case_when(
  Treatment <=4 ~ "No Irrigation",
  Treatment <=8 ~ "Full Irrigation",
  Treatment <=12 ~ "Weather Informed Irrigation",
))


Corn_data_2020$Year = 2020
Corn_data_2021$Year = 2021
 Corn_data_2021_NUE <- filter(Corn_data_2021, Nuptake < 415)  ##########Remove outliers

rm(corn_stats_21,corn_stats_20)


Corn_data_2020_WUE <- Corn_data_2020 %>%
  filter(Irrigation != "None")
Corn_data_2021_WUE <- Corn_data_2021 %>%
  filter(Irrigation != "None")

# Corn_data_2020_NUE <- Corn_data_2020 %>%
#   filter(Nrate != "0")
# Corn_data_2021_NUE <- Corn_data_2021_NUE %>%
#   filter(Nrate != "0")





# Yield_Summary_table <- rbind.data.frame(Yield_stats_20,Yield_stats_21)
# NUE_Summary_table <- rbind.data.frame(NUE_stats_20,NUE_stats_21)
# WUE_Summary_table <- rbind.data.frame(irrWUE_stats_20,irrWUE_stats_21)

 # write.csv(Yield_Summary_table, "Corn_Yield_summary_SI.csv")
 # write.csv(NUE_Summary_table, "Corn_NUE_summary_SI.csv")
 # write.csv(WUE_Summary_table, "Corn_WUE_summary_SI.csv")

Yield_stats_20 <- summarySE(Corn_data_2020, measurevar="Yield", groupvars=c("Nrate"))
Yield_stats_21 <- summarySE(Corn_data_2021, measurevar="Yield", groupvars=c("Nrate"))

NUE_stats_20 <- summarySE(Corn_data_2020, measurevar="NUE", groupvars=c("Nrate"))
NUE_stats_21 <- summarySE(Corn_data_2021_NUE, measurevar="NUE", groupvars=c("Nrate"))

irrWUE_stats_20 <- summarySE(Corn_data_2020_WUE, measurevar="irrWUE", groupvars=c("Nrate"))
irrWUE_stats_21 <- summarySE(Corn_data_2021_WUE, measurevar="irrWUE", groupvars=c("Nrate"))

#################################################################################################################################

Yield_stats_20 <- summarySE(Corn_data_2020, measurevar="Yield", groupvars=c("Irrigation"))
Yield_stats_21 <- summarySE(Corn_data_2021, measurevar="Yield", groupvars=c("Irrigation"))

NUE_stats_20 <- summarySE(Corn_data_2020, measurevar="NUE", groupvars=c("Irrigation"))
NUE_stats_21 <- summarySE(Corn_data_2021_NUE, measurevar="NUE", groupvars=c("Irrigation"))

irrWUE_stats_20 <- summarySE(Corn_data_2020, measurevar="irrWUE", groupvars=c("Irrigation"))
irrWUE_stats_21 <- summarySE(Corn_data_2021, measurevar="irrWUE", groupvars=c("Irrigation"))
# ===================================================================================================================================================


#############################################################################################################################
# Interaction Summary Tables
Yield_stats_20 <- summarySE(Corn_data_2020, measurevar="Yield", groupvars=c("Nrate","Irrigation","Treatment"))
Yield_stats_21 <- summarySE(Corn_data_2021, measurevar="Yield", groupvars=c("Nrate","Irrigation","Treatment"))

NUE_stats_20 <- summarySE(Corn_data_2020, measurevar="NUE", groupvars=c("Nrate","Irrigation","Treatment"))
NUE_stats_21 <- summarySE(Corn_data_2021_NUE, measurevar="NUE", groupvars=c("Nrate","Irrigation","Treatment"))
NUE_stats_21$NUE <- round(NUE_stats_21$NUE,2)
irrWUE_stats_20 <- summarySE(Corn_data_2020_WUE, measurevar="irrWUE", groupvars=c("Nrate","Irrigation","Treatment"))
irrWUE_stats_21 <- summarySE(Corn_data_2021_WUE, measurevar="irrWUE", groupvars=c("Nrate","Irrigation","Treatment"))

NUE_stats_20$NUE <- round(NUE_stats_20$NUE,2)
# CORN
#################################################################################################################################################
# Plots 2021 


####

NUE_plot_dat<-dplyr::mutate(NUE_stats_21,  Irrigation = case_when(
  Treatment <=4 ~ "3No Irrigation",
  Treatment <=8 ~ "1Full Irrigation",
  Treatment <=12 ~ "2Weather Informed",
))


nue_plot_CORN_2021<-NUE_plot_dat %>% 
  filter(Nrate != 0) %>% 
  ggplot(aes(x=as.factor(Nrate), y=NUE*100, fill= Irrigation))+
  geom_bar(stat = "identity", position=position_dodge(), colour="black", alpha = 0.5)+
  geom_errorbar(aes(ymin=NUE*100-se*100, ymax=NUE*100+se*100),
                linewidth=0.5,
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
        legend.text=element_text(size=14),
        axis.text=element_text(size=14, colour="black"),
        axis.title=element_text(size=14, colour="black"),
        axis.line = element_line(colour = "black",
                                 size = 0.5, linetype = "solid"),
        axis.ticks = element_line(colour="black"),
        panel.grid.major=element_line(colour = "light grey"),
        panel.grid.minor=element_blank())+
  scale_fill_manual(values=c("#bdbdbd", "#a6bddb", "#fee0d2"), 
                    breaks=c("1Full Irrigation", "2Weather Informed", "3No Irrigation"),
                    labels=c("Full Irrigation", "Weather Informed", "No Irrigation"))+
  guides(colour = guide_legend(override.aes = list(size=5)))+
  labs(title="NUE (2021)",
       # x="N Application (lb/acre)",
         x= element_blank(),
       y = "NUE (%)",
       color=('Irrigation Treatment'),
       # shape=('Irrigation Treatment'),
       # linetype=('Irrigation Treatment')
  )

nue_plot_CORN_2021
#ggsave(paste0(plot_loc,"Sept_nue_plot_CORN_2021_Updated.png"),nue_plot_CORN_2021, width = 7, height = 5, units="in", dpi = 600)

WUE_plot_dat<-dplyr::mutate(irrWUE_stats_21,  Irrigation = case_when(
  Treatment <=4 ~ "3No Irrigation",
  Treatment <=8 ~ "1Full Irrigation",
  Treatment <=12 ~ "2Weather Informed",
))


WUE_plot_CORN_2021<-WUE_plot_dat %>% 
  filter(Irrigation != "3No Irrigation") %>%
  ggplot(aes(x=as.factor(Nrate), y=irrWUE, fill= Irrigation))+
  geom_bar(stat = "identity", position=position_dodge(), colour="black")+
  geom_errorbar(aes(ymin=irrWUE-se, ymax=irrWUE+se),
                linewidth=0.5,
                width=0.15,                    # Width of the error bars
                position=position_dodge(.9))+
  # geom_line(size=1)+
  theme_bw()+
  # scale_x_continuous(breaks=seq(80, 240, 80))+
  scale_y_continuous(breaks=seq(-10, 50, 10))+
  theme(plot.title = element_text(size=14),
        legend.position="top",
        legend.title=element_blank(),
        legend.box = "horizontal",
        legend.background = element_rect(fill="white",
                                         size=0.5, linetype="solid",
                                         colour ="white"),
        legend.text=element_text(size=14),
        axis.text=element_text(size=14, colour="black"),
        axis.title=element_text(size=14, colour="black"),
        axis.line = element_line(colour = "black",
                                 size = 0.5, linetype = "solid"),
        axis.ticks = element_line(colour="black"),
        panel.grid.major=element_line(colour = "light grey"),
        panel.grid.minor=element_blank())+
  scale_fill_manual(values=c("#bdbdbd", "#a6bddb", "#fee0d2"), 
                    breaks=c("1Full Irrigation", "2Weather Informed", "3No Irrigation"),
                    labels=c("Full Irrigation", "Weather Informed", "No Irrigation"))+
  guides(colour = guide_legend(override.aes = list(size=5)))+
  labs(title="WUE (2021)",
       x="N Application (kg N/ha)", y = "WUE (kg/ha) /mm",
       color=('Irrigation Treatment'),
       # shape=('Irrigation Treatment'),
       # linetype=('Irrigation Treatment')
  )


WUE_plot_CORN_2021
#ggsave(paste0(plot_loc,"Sept_wue_plot_CORN_2021.png"),WUE_plot_CORN_2021, width = 7, height = 5, units="in", dpi = 600)


Yield_plot_dat<-dplyr::mutate(Yield_stats_21,  Irrigation = case_when(
  Treatment <=4 ~ "3No Irrigation",
  Treatment <=8 ~ "1Full Irrigation",
  Treatment <=12 ~ "2Weather Informed",
))


Yield_plot_CORN_2021<-Yield_plot_dat %>% 
  # filter(Irrigation != "3No Irrigation") %>%
  ggplot(aes(x=as.factor(Nrate), y=Yield, fill= Irrigation))+
  geom_bar(stat = "identity", position=position_dodge(), colour="black")+
  geom_errorbar(aes(ymin=Yield-se, ymax=Yield+se),
                linewidth=0.5,
                width=0.15,                    # Width of the error bars
                position=position_dodge(.9))+
  # geom_line(size=1)+
  theme_bw()+
  # scale_x_continuous(breaks=seq(80, 240, 80))+
  scale_y_continuous(breaks=seq(0, 16000, 4000))+
  theme(plot.title = element_text(size=14),
        legend.position="top",
        legend.title=element_blank(),
        legend.box = "horizontal",
        legend.background = element_rect(fill="white",
                                         size=0.5, linetype="solid",
                                         colour ="white"),
        legend.text=element_text(size=14),
        axis.text=element_text(size=14, colour="black"),
        axis.title=element_text(size=14, colour="black"),
        axis.line = element_line(colour = "black",
                                 size = 0.5, linetype = "solid"),
        axis.ticks = element_line(colour="black"),
        panel.grid.major=element_line(colour = "light grey"),
        panel.grid.minor=element_blank())+
  scale_fill_manual(values=c("#bdbdbd", "#a6bddb", "#fee0d2"), 
                    breaks=c("1Full Irrigation", "2Weather Informed", "3No Irrigation"),
                    labels=c("Full Irrigation", "Weather Informed", "No Irrigation"))+
  guides(colour = guide_legend(override.aes = list(size=5)))+
  labs(title="Yield (2021)",
       # x="N Application (lb/acre)", 
         x= element_blank(),
        y = "Yield  (kg/ha)",
       color=('Irrigation Treatment'),
       # shape=('Irrigation Treatment'),
       # linetype=('Irrigation Treatment')
  )


Yield_plot_CORN_2021
#ggsave(paste0(plot_loc,"Sept_Yield_plot_CORN_2021.png"),Yield_plot_CORN_2021, width = 7, height = 5, units="in", dpi = 600)


#################################################################################################################################################
# Plots 2020
# Load sign file

load("F:/My Drive/Tidewater/Tidewater Data Analysis/Corn_signs.Rdata")
NUE_20_sign<-dplyr::mutate(NUE_20_sign,  Irrigation = case_when(
  Irrigation == "None" ~ "3No Irrigation",
  Irrigation == "Full" ~ "1Full Irrigation",
  Irrigation == "Environmental "  ~ "2Weather Informed",
))
NUE_20_sign$Nrate <- as.numeric(as.character(NUE_20_sign$Nrate))
NUE_20_sign$Nrate <- round((NUE_20_sign$Nrate * 1.12085),digits = -1)

NUE_stats_20<-dplyr::mutate(NUE_stats_20,  Irrigation = case_when(
  Treatment <=4 ~ "3No Irrigation",
  Treatment <=8 ~ "1Full Irrigation",
  Treatment <=12 ~ "2Weather Informed",
))



NUE_stats_20$Nrate = as.factor(NUE_stats_20$Nrate)
NUE_20_sign$Nrate <- as.factor(NUE_20_sign$Nrate)

NUE_plot_dat <- left_join(NUE_stats_20,NUE_20_sign[,c(1,2,8)] , by = c("Nrate", "Irrigation"))

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
  scale_y_continuous(limits = c(0,150),breaks=seq(0, 150, 50))+
  theme(plot.title = element_text(size=14),
        legend.position="top",
        legend.title=element_blank(),
        legend.box = "horizontal",
        legend.background = element_rect(fill="white",
                                         size=0.5, linetype="solid",
                                         colour ="white"),
        legend.text=element_text(size=14),
        axis.text=element_text(size=14, colour="black"),
        axis.title=element_text(size=14, colour="black"),
        axis.line = element_line(colour = "black",
                                 size = 0.5, linetype = "solid"),
        axis.ticks = element_line(colour="black"),
        panel.grid.major=element_line(colour = "light grey"),
        panel.grid.minor=element_blank())+
  scale_fill_manual(values=c("#bdbdbd", "#a6bddb", "#fee0d2"), 
                    breaks=c("1Full Irrigation", "2Weather Informed", "3No Irrigation"),
                    labels=c("Full Irrigation", "Weather Informed", "No Irrigation"))+
  guides(colour = guide_legend(override.aes = list(size=5)))+
  labs(title="NUE (2020)",
       # x="N Application (lb/acre)",
       x= element_blank(),
       y = "NUE (%)",
       color=('Irrigation Treatment'),
       # shape=('Irrigation Treatment'),
       # linetype=('Irrigation Treatment')
  )+
  geom_text(aes(label=.group), position = position_dodge(0.9), size = 5, 
            vjust=-2.75, hjust=0.75, colour = "gray25")


nue_plot_CORN_2020
#ggsave(paste0(plot_loc,"Sept_nue_plot_CORN_2020_Updated.png"),nue_plot_CORN_2020, width = 7, height = 5, units="in", dpi = 600)

WUE_plot_dat<-dplyr::mutate(irrWUE_stats_20,  Irrigation = case_when(
  Treatment <=4 ~ "3No Irrigation",
  Treatment <=8 ~ "1Full Irrigation",
  Treatment <=12 ~ "2Weather Informed",
))


WUE_plot_CORN_2020<-WUE_plot_dat %>% 
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
  scale_y_continuous(limits = c(-10,50) ,breaks=seq(-10, 50, 10))+
  theme(plot.title = element_text(size=14),
        legend.position="top",
        legend.title=element_blank(),
        legend.box = "horizontal",
        legend.background = element_rect(fill="white",
                                         size=0.5, linetype="solid",
                                         colour ="white"),
        legend.text=element_text(size=14),
        axis.text=element_text(size=14, colour="black"),
        axis.title=element_text(size=14, colour="black"),
        axis.line = element_line(colour = "black",
                                 size = 0.5, linetype = "solid"),
        axis.ticks = element_line(colour="black"),
        panel.grid.major=element_line(colour = "light grey"),
        panel.grid.minor=element_blank())+
  scale_fill_manual(values=c("#bdbdbd", "#a6bddb", "#fee0d2"), 
                    breaks=c("1Full Irrigation", "2Weather Informed", "3No Irrigation"),
                    labels=c("Full Irrigation", "Weather Informed", "No Irrigation"))+
  guides(colour = guide_legend(override.aes = list(size=5)))+
  labs(title="WUE (2020)",
       x="N Application (kg N/ha)", y = "WUE (kg/ha) /mm",
       color=('Irrigation Treatment'),
       # shape=('Irrigation Treatment'),
       # linetype=('Irrigation Treatment')
  )


WUE_plot_CORN_2020
#ggsave(paste0(plot_loc,"Sept_wue_plot_CORN_2020.png"),WUE_plot_CORN_2020, width = 7, height = 5, units="in", dpi = 600)


Yield_plot_dat<-dplyr::mutate(Yield_stats_20,  Irrigation = case_when(
  Treatment <=4 ~ "3No Irrigation",
  Treatment <=8 ~ "1Full Irrigation",
  Treatment <=12 ~ "2Weather Informed",
))


Yield_plot_CORN_2020<-Yield_plot_dat %>% 
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
  scale_y_continuous(limits = c(0,16000) ,breaks=seq(0, 16000, 4000))+
  theme(plot.title = element_text(size=14),
        legend.position="top",
        legend.title=element_blank(),
        legend.box = "horizontal",
        legend.background = element_rect(fill="white",
                                         size=0.5, linetype="solid",
                                         colour ="white"),
        legend.text=element_text(size=14),
        axis.text=element_text(size=14, colour="black"),
        axis.title=element_text(size=14, colour="black"),
        axis.line = element_line(colour = "black",
                                 size = 0.5, linetype = "solid"),
        axis.ticks = element_line(colour="black"),
        panel.grid.major=element_line(colour = "light grey"),
        panel.grid.minor=element_blank())+
  scale_fill_manual(values=c("#bdbdbd", "#a6bddb", "#fee0d2"), 
                    breaks=c("1Full Irrigation", "2Weather Informed", "3No Irrigation"),
                    labels=c("Full Irrigation", "Weather Informed", "No Irrigation"))+
  guides(colour = guide_legend(override.aes = list(size=5)))+
  labs(title="Yield (2020)",
       x=element_blank(),
       y = "Yield  (kg/ha)",
       color=('Irrigation Treatment'),
       # shape=('Irrigation Treatment'),
       # linetype=('Irrigation Treatment')
  )


Yield_plot_CORN_2020
#ggsave(paste0(plot_loc,"Sept_Yield_plot_CORN_2020.png"),Yield_plot_CORN_2020, width = 7, height = 5, units="in", dpi = 600)

# library(patchwork)
# ggp_all <- (Yield_plot_CORN_2020 + Yield_plot_CORN_2021) / (nue_plot_CORN_2020+nue_plot_CORN_2021) / (WUE_plot_CORN_2020+ WUE_plot_CORN_2021) + 
#   # plot_layout(guides="collect")# Create grid of plots with title
#   plot_annotation(title = "Corn Results")&
#   theme(plot.title = element_text(size = 16,hjust = 0.5))
# ggp_all

  # ggsave(paste0(plot_loc,"Interactionplot_CORN.png"),ggp_all, width = 12, height = 12, units="in", dpi = 600)
library(ggpubr)
p <- ggarrange(Yield_plot_CORN_2020 , Yield_plot_CORN_2021,
          nue_plot_CORN_2020 ,nue_plot_CORN_2021,
          WUE_plot_CORN_2020, WUE_plot_CORN_2021
          ,ncol=2, nrow=3, common.legend = TRUE, legend="bottom")
  
ggp_all <- annotate_figure(p, top = text_grob("Corn results"
               , face = "bold", size = 18))
ggp_all
 ggsave(paste0(plot_loc,"22SI_Interactionplot_CORN.png"),ggp_all, width = 12, height = 12, units="in", dpi = 600)


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

###################################################################

# Nrate
rm(list = ls())
load(("F:/My Drive/Tidewater/Tidewater Data Analysis/Corn_stats_significance.RData"))


Yield_stats_20 <- summarySE(Corn_data_2020, measurevar="Yield", groupvars=c("Nrate"))
Yield_stats_20 <-left_join(Yield_stats_20,Yield_20_sign[,c(1,7)], by = "Nrate")

Yield_stats_21 <- summarySE(Corn_data_2021, measurevar="Yield", groupvars=c("Nrate"))
Yield_stats_21 <-left_join(Yield_stats_21,Yield_21_sign[,c(1,7)], by = "Nrate")

NUE_stats_20 <- summarySE(Corn_data_2020, measurevar="NUE", groupvars=c("Nrate"))
NUE_stats_20 <-NUE_stats_20
NUE_stats_20$.group <- NA


NUE_stats_21 <- summarySE(Corn_data_2021, measurevar="NUE", groupvars=c("Nrate"))
NUE_stats_21 <-NUE_stats_21
NUE_stats_21$.group <- NA


irrWUE_stats_20 <- summarySE(Corn_data_2020, measurevar="irrWUE", groupvars=c("Nrate"))

irrWUE_stats_20$.group <- NA

irrWUE_stats_21 <- summarySE(Corn_data_2021, measurevar="irrWUE", groupvars=c("Nrate"))

irrWUE_stats_21$.group <- NA



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


NUE_plot_CORN_2021<-NUE_stats_21 %>% 
  filter(Nrate != "0") %>%
  ggplot(aes(x=as.factor(Nrate), y=NUE*100))+
  geom_bar(stat = "identity", position=position_dodge(), colour="black", width = 0.75, alpha = 0.5, fill = "#9ecae1")+
  geom_errorbar(aes(ymin=100*(NUE-se), ymax=100*(NUE+se)),
                size=0.5,
                width=0.15,                    # Width of the error bars
                position=position_dodge(.9))+
  # geom_line(size=1)+
  theme_bw()+
  # scale_x_continuous(breaks=seq(80, 240, 80))+
  scale_y_continuous(breaks=seq(0, 150, 25))+
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
  labs(title="NUE (2021)",
       x="N Application (lb/acre)", y = "NUE  (bu/ac)",
       color=('Irrigation Treatment'),
       # shape=('Irrigation Treatment'),
       # linetype=('Irrigation Treatment')
  )+
  geom_text(aes(label=.group), position = position_dodge(0.90), size = 5,
            vjust=-0.8, hjust=-0.5, colour = "gray25")


NUE_plot_CORN_2021

NUE_plot_CORN_2020<-NUE_stats_20 %>% 
  filter(Nrate != "0") %>%
  ggplot(aes(x=as.factor(Nrate), y=NUE*100))+
  geom_bar(stat = "identity", position=position_dodge(), colour="black", width = 0.75, alpha = 0.5, fill = "#9ecae1")+
  geom_errorbar(aes(ymin=100*(NUE-se), ymax=100*(NUE+se)),
                size=0.5,
                width=0.15,                    # Width of the error bars
                position=position_dodge(.9))+
  # geom_line(size=1)+
  theme_bw()+
  # scale_x_continuous(breaks=seq(80, 240, 80))+
  scale_y_continuous(breaks=seq(0, 150, 25))+
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
  labs(title="NUE (2020)",
       x="N Application (lb/acre)", y = "NUE  (bu/ac)",
       color=('Irrigation Treatment'),
       # shape=('Irrigation Treatment'),
       # linetype=('Irrigation Treatment')
  )+
  geom_text(aes(label=.group), position = position_dodge(0.90), size = 5,
            vjust=-0.8, hjust=-0.5, colour = "gray25")


NUE_plot_CORN_2020

irrWUE_plot_CORN_2021<-irrWUE_stats_21 %>% 
  # filter(Nrate != "0") %>%
  ggplot(aes(x=as.factor(Nrate), y=irrWUE))+
  geom_bar(stat = "identity", position=position_dodge(), colour="black", width = 0.75, alpha = 0.5, fill = "#9ecae1")+
  geom_errorbar(aes(ymin=(irrWUE-se), ymax=(irrWUE+se)),
                size=0.5,
                width=0.15,                    # Width of the error bars
                position=position_dodge(.9))+
  # geom_line(size=1)+
  theme_bw()+
  # scale_x_continuous(breaks=seq(80, 240, 80))+
  # scale_y_continuous(breaks=seq(0, 250, 50))+
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
  labs(title="WUE (2021)",
       x="N Application (lb/acre)", y = "WUE  (bu/ac)",
       color=('Irrigation Treatment'),
       # shape=('Irrigation Treatment'),
       # linetype=('Irrigation Treatment')
  )+
  geom_text(aes(label=.group), position = position_dodge(0.90), size = 5,
            vjust=-0.8, hjust=-0.5, colour = "gray25")


irrWUE_plot_CORN_2021

irrWUE_plot_CORN_2020<-irrWUE_stats_20 %>% 
  # filter(Nrate != "0") %>%
  ggplot(aes(x=as.factor(Nrate), y=irrWUE))+
  geom_bar(stat = "identity", position=position_dodge(), colour="black", width = 0.75, alpha = 0.5, fill = "#9ecae1")+
  geom_errorbar(aes(ymin=(irrWUE-se), ymax=(irrWUE+se)),
                size=0.5,
                width=0.15,                    # Width of the error bars
                position=position_dodge(.9))+
  # geom_line(size=1)+
  theme_bw()+
  # scale_x_continuous(breaks=seq(80, 240, 80))+
  # scale_y_continuous(breaks=seq(0, 250, 50))+
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
  labs(title="WUE (2020)",
       x="N Application (lb/acre)", y = "WUE  (bu/ac)",
       color=('Irrigation Treatment'),
       # shape=('Irrigation Treatment'),
       # linetype=('Irrigation Treatment')
  )+
  geom_text(aes(label=.group), position = position_dodge(0.90), size = 5,
            vjust=-0.8, hjust=-0.5, colour = "gray25")


irrWUE_plot_CORN_2020


ggp_all <- (Yield_plot_CORN_2020 + Yield_plot_CORN_2021) / (NUE_plot_CORN_2020+NUE_plot_CORN_2021) / (irrWUE_plot_CORN_2020+ irrWUE_plot_CORN_2021) + 
  # plot_layout(guides="collect")# Create grid of plots with title
  plot_annotation(title = "Impacts of N rate")&
  theme(plot.title = element_text(size = 16,hjust = 0.5))
ggp_all


ggsave(paste0(plot_loc,"NUE _main_CORN.png"),ggp_all, width = 10, height = 13, units="in", dpi = 600)

#################################################################################
# Irrigation significance



NUE_stats_21<-dplyr::mutate(NUE_stats_21,  Irrigation = case_when(
  Irrigation == "None" ~ "3No Irrigation",
  Irrigation == "Full" ~ "1Full Irrigation",
  Irrigation == "Environmental "  ~ "2Weather Informed",
))

NUE_plot_CORN_2021<-NUE_stats_21 %>% 
  # filter(Nrate != "0") %>%
  ggplot(aes(x=as.factor(Irrigation), y=NUE*100))+
  geom_bar(stat = "identity", position=position_dodge(), colour="black", width = 0.75, alpha = 0.5, fill = "#9ecae1")+
  geom_errorbar(aes(ymin=100*(NUE-se), ymax=100*(NUE+se)),
                size=0.5,
                width=0.15,                    # Width of the error bars
                position=position_dodge(.9))+
  # geom_line(size=1)+
  theme_bw()+
  # scale_x_continuous(breaks=seq(80, 240, 80))+
  scale_y_continuous(breaks=seq(0, 250, 25))+
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
                    breaks=c("1Full Irrigation", "2Weather Informed", "3No Irrigation"),
                    labels=c("Full Irrigation", "Weather Informed", "No Irrigation"))+
  guides(colour = guide_legend(override.aes = list(size=5)))+
  labs(title="NUE (2021)",
       x="N Application (lb/acre)", y = "NUE  (bu/ac)",
       color=('Irrigation Treatment'),
       # shape=('Irrigation Treatment'),
       # linetype=('Irrigation Treatment')
  )+
  geom_text(aes(label=.group), position = position_dodge(0.90), size = 5,
            vjust=-0.8, hjust=-0.5, colour = "gray25")


NUE_plot_CORN_2021
