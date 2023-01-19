# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

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
# Cotton_data_2020$Irrigation[Cotton_data_2020_NUE$Irrigation =="Full  "] <- "Full"


##################################################################################
# SI UNIT CONVERSIONS

Cotton_data_2020$Yield  <- round(Cotton_data_2020$Yield * 1.12085,2)
Cotton_data_2020$Nuptake  <- round(Cotton_data_2020$Nuptake * 1.12085,2)
Cotton_data_2020$Nrate  <- round(Cotton_data_2020$Nrate * 1.12085,0)


Cotton_data_2021$Yield  <- round(Cotton_data_2021$Yield * 1.12085,2)
Cotton_data_2021$Nuptake  <- round(Cotton_data_2021$Nuptake * 1.12085,2)

Cotton_data_2021$Nrate <- round((Cotton_data_2021$Nrate * 1.12085),0)


# Year = 2020
cotton_data <- function(Year){
  Irrigation_dat <- as.data.frame(unique(Cotton_data_2021$Irrigation))
  if (Year == 2021) {
    Irrigation_dat$Irrigation_amt <- c(0,2.2,1.5) 
    dat = Cotton_data_2021
  }else{
    Irrigation_dat$Irrigation_amt <- c(0,6.4,4.1)
    dat = Cotton_data_2020
  }
  
  Irrigation_dat$Irrigation_amt  <- Irrigation_dat$Irrigation_amt * 25.4
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

# Cotton_data_2020<-dplyr::mutate(Cotton_data_2020,  Nrate = case_when(
#   Nrate ==0 ~ "CTN1",
#   Nrate ==40 ~ "CTN2",
#   Nrate ==80 ~ "CTN3",
#   Nrate ==120 ~ "CTN4",
# ))
# 
# Cotton_data_2021<-dplyr::mutate(Cotton_data_2021,  Nrate = case_when(
#   Nrate ==0 ~ "CTN1",
#   Nrate ==40 ~ "CTN2",
#   Nrate ==80 ~ "CTN3",
#   Nrate ==120 ~ "CTN4",
# ))

Cotton_data_2020<-dplyr::mutate(Cotton_data_2020,  Irrigation = case_when(
  Treatment <=4 ~ "No Irrigation",
  Treatment <=8 ~ "Full Irrigation",
  Treatment <=12 ~ "Weather Informed Irrigation",
))

Cotton_data_2021<-dplyr::mutate(Cotton_data_2021,  Irrigation = case_when(
  Treatment <=4 ~ "No Irrigation",
  Treatment <=8 ~ "Full Irrigation",
  Treatment <=12 ~ "Weather Informed Irrigation",
))


Cotton_data_2020$Nrate <- factor(Cotton_data_2020$Nrate)
Cotton_data_2020$Irrigation <- factor(Cotton_data_2020$Irrigation)
Cotton_data_2021$Nrate <- factor(Cotton_data_2021$Nrate)
Cotton_data_2021$Irrigation <- factor(Cotton_data_2021$Irrigation)

Cotton_data_2020_NUE <- Cotton_data_2020 %>%
  filter( Rep < 4 )
Cotton_data_2021_NUE <- Cotton_data_2021 

Cotton_data_2020_WUE <- Cotton_data_2020 %>%
  filter( Rep < 4 )
Cotton_data_2021_WUE <- Cotton_data_2021 

#############################################################################################################################
# Summary Tables


Yield_Summary_table <- rbind.data.frame(Yield_stats_20,Yield_stats_21)
NUE_Summary_table <- rbind.data.frame(NUE_stats_20,NUE_stats_21)
WUE_Summary_table <- rbind.data.frame(irrWUE_stats_20,irrWUE_stats_21)

 # write.csv(Yield_Summary_table, "SI_Cotton_Yield_summary.csv")
 # write.csv(NUE_Summary_table, "SI_Cotton_NUE_summary.csv")
 # write.csv(WUE_Summary_table, "SI_Cotton_WUE_summary.csv")

Yield_stats_20 <- summarySE(Cotton_data_2020, measurevar="Yield", groupvars=c("Nrate"))
Yield_stats_21 <- summarySE(Cotton_data_2021, measurevar="Yield", groupvars=c("Nrate"))

NUE_stats_20 <- summarySE(Cotton_data_2020_NUE, measurevar="NUE", groupvars=c("Nrate"))
NUE_stats_21 <- summarySE(Cotton_data_2021_NUE, measurevar="NUE", groupvars=c("Nrate"))

irrWUE_stats_20 <- summarySE(Cotton_data_2020_WUE, measurevar="irrWUE", groupvars=c("Nrate"))
irrWUE_stats_21 <- summarySE(Cotton_data_2021_WUE, measurevar="irrWUE", groupvars=c("Nrate"))

#################################################################################################################################

Yield_stats_20 <- summarySE(Cotton_data_2020, measurevar="Yield", groupvars=c("Irrigation"))
Yield_stats_21 <- summarySE(Cotton_data_2021, measurevar="Yield", groupvars=c("Irrigation"))

NUE_stats_20 <- summarySE(Cotton_data_2020_NUE, measurevar="NUE", groupvars=c("Irrigation"))
NUE_stats_21 <- summarySE(Cotton_data_2021_NUE, measurevar="NUE", groupvars=c("Irrigation"))

irrWUE_stats_20 <- summarySE(Cotton_data_2020_WUE, measurevar="irrWUE", groupvars=c("Irrigation"))
irrWUE_stats_21 <- summarySE(Cotton_data_2021_WUE, measurevar="irrWUE", groupvars=c("Irrigation"))

##################################################################################################################################
Yield_stats_20 <- summarySE(Cotton_data_2020, measurevar="Yield", groupvars=c("Nrate","Irrigation","Treatment"))
Yield_stats_21 <- summarySE(Cotton_data_2021, measurevar="Yield", groupvars=c("Nrate","Irrigation","Treatment"))

NUE_stats_20 <- summarySE(Cotton_data_2020_NUE, measurevar="NUE", groupvars=c("Nrate","Irrigation","Treatment"))
NUE_stats_21 <- summarySE(Cotton_data_2021_NUE, measurevar="NUE", groupvars=c("Nrate","Irrigation","Treatment"))

irrWUE_stats_20 <- summarySE(Cotton_data_2020_WUE, measurevar="irrWUE", groupvars=c("Nrate","Irrigation","Treatment"))
irrWUE_stats_21 <- summarySE(Cotton_data_2021_WUE, measurevar="irrWUE", groupvars=c("Nrate","Irrigation","Treatment"))
# ===================================================================================================================================================
# Cotton
#################################################################################################################################################
# Plots 2021 

NUE_plot_dat<-dplyr::mutate(NUE_stats_21,  Irrigation = case_when(
  Treatment <=4 ~ "3No Irrigation",
  Treatment <=8 ~ "1Full Irrigation",
  Treatment <=12 ~ "2Weather Informed",
))


nue_plot_Cotton_2021<-NUE_plot_dat %>% 
  filter(Nrate != 0) %>% 
  ggplot(aes(x=as.factor(Nrate), y=NUE*100, fill= Irrigation))+
  geom_bar(stat = "identity", position=position_dodge(), colour="black")+
  geom_errorbar(aes(ymin=NUE*100-se*100, ymax=NUE*100+se*100),
                size=0.5,
                width=0.15,                    # Width of the error bars
                position=position_dodge(.9))+
  # geom_line(size=1)+
  theme_bw()+
  # scale_x_continuous(breaks=seq(80, 240, 80))+
    scale_y_continuous(breaks=seq(0, 100, 20))+
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
                    breaks=c("1Full Irrigation", "2Weather Informed", "3No Irrigation"),
                    labels=c("Full Irrigation", "Weather Informed", "No Irrigation"))+
  guides(colour = guide_legend(override.aes = list(size=5)))+
  labs(title="NUE (2021)",
       # x="Nitrogen Application (kg/ha)",
            x=element_blank(),
       y = "NUE (%)",
       color=('Irrigation Treatment'),
       # shape=('Irrigation Treatment'),
       # linetype=('Irrigation Treatment')
  )

nue_plot_Cotton_2021
# ggsave(paste0(plot_loc,"Sept_nue_plot_Cotton_2021_Updated.png"),nue_plot_Cotton_2021, width = 7, height = 5, units="in", dpi = 600)

WUE_plot_dat<-dplyr::mutate(irrWUE_stats_21,  Irrigation = case_when(
  Treatment <=4 ~ "3No Irrigation",
  Treatment <=8 ~ "1Full Irrigation",
  Treatment <=12 ~ "2Weather Informed",
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
   scale_y_continuous(limits = c(-10,10), breaks=seq(-10,10, 5))+
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
                    breaks=c("1Full Irrigation", "2Weather Informed", "3No Irrigation"),
                    labels=c("Full Irrigation", "Weather Informed", "No Irrigation"))+
  guides(colour = guide_legend(override.aes = list(size=5)))+
  labs(title="WUE (2021)",
       x="Nitrogen Application (kg N /ha)", y = "WUE (kg/ha) /mm",
       color=('Irrigation Treatment'),
       # shape=('Irrigation Treatment'),
       # linetype=('Irrigation Treatment')
  )


WUE_plot_Cotton_2021
# ggsave(paste0(plot_loc,"Sept_wue_plot_Cotton_2021.png"),WUE_plot_Cotton_2021, width = 7, height = 5, units="in", dpi = 600)


Yield_plot_dat<-dplyr::mutate(Yield_stats_21,  Irrigation = case_when(
  Treatment <=4 ~ "3No Irrigation",
  Treatment <=8 ~ "1Full Irrigation",
  Treatment <=12 ~ "2Weather Informed",
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
                    breaks=c("1Full Irrigation", "2Weather Informed", "3No Irrigation"),
                    labels=c("Full Irrigation", "Weather Informed", "No Irrigation"))+
  guides(colour = guide_legend(override.aes = list(size=5)))+
  labs(title="Yield (2021)",
       # x="Nitrogen Application (kg/ha)",
            x=element_blank(),
       y = "Yield  (kg/ha)",
       color=('Irrigation Treatment'),
       # shape=('Irrigation Treatment'),
       # linetype=('Irrigation Treatment')
  )


Yield_plot_Cotton_2021
# ggsave(paste0(plot_loc,"Sept_Yield_plot_Cotton_2021.png"),Yield_plot_Cotton_2021, width = 7, height = 5, units="in", dpi = 600)


#################################################################################################################################################
# Plots 2020

NUE_plot_dat<-dplyr::mutate(NUE_stats_20,  Irrigation = case_when(
  Treatment <=4 ~ "3No Irrigation",
  Treatment <=8 ~ "1Full Irrigation",
  Treatment <=12 ~ "2Weather Informed",
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
  scale_y_continuous(breaks=seq(0, 100, 20))+
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
                    breaks=c("1Full Irrigation", "2Weather Informed", "3No Irrigation"),
                    labels=c("Full Irrigation", "Weather Informed", "No Irrigation"))+
  guides(colour = guide_legend(override.aes = list(size=5)))+
  labs(title="NUE (2020)",
            x=element_blank(),
       # x="Nitrogen Application (kg/ha)",
       y = "NUE (%)",
       color=('Irrigation Treatment'),
       # shape=('Irrigation Treatment'),
       # linetype=('Irrigation Treatment')
  )


nue_plot_Cotton_2020
# ggsave(paste0(plot_loc,"Sept_nue_plot_Cotton_2020_Updated.png"),nue_plot_Cotton_2020, width = 7, height = 5, units="in", dpi = 600)

WUE_plot_dat<-dplyr::mutate(irrWUE_stats_20,  Irrigation = case_when(
  Treatment <=4 ~ "3No Irrigation",
  Treatment <=8 ~ "1Full Irrigation",
  Treatment <=12 ~ "2Weather Informed",
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
  scale_y_continuous(limits = c(-10,10), breaks=seq(-10,10, 5))+
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
                    breaks=c("1Full Irrigation", "2Weather Informed", "3No Irrigation"),
                    labels=c("Full Irrigation", "Weather Informed", "No Irrigation"))+
  guides(colour = guide_legend(override.aes = list(size=5)))+
  labs(title="WUE (2020)",
       x="Nitrogen Application (kg N /ha)", y = "WUE (kg/ha) /mm",
       color=('Irrigation Treatment'),
       # shape=('Irrigation Treatment'),
       # linetype=('Irrigation Treatment')
  )


WUE_plot_Cotton_2020
# ggsave(paste0(plot_loc,"Sept_wue_plot_Cotton_2020.png"),WUE_plot_Cotton_2020, width = 7, height = 5, units="in", dpi = 600)


Yield_plot_dat<-dplyr::mutate(Yield_stats_20,  Irrigation = case_when(
  Treatment <=4 ~ "3No Irrigation",
  Treatment <=8 ~ "1Full Irrigation",
  Treatment <=12 ~ "2Weather Informed",
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
  scale_y_continuous(limits= c(0,2100),breaks=seq(0, 2000, 500))+
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
                    breaks=c("1Full Irrigation", "2Weather Informed", "3No Irrigation"),
                    labels=c("Full Irrigation", "Weather Informed", "No Irrigation"))+
  guides(colour = guide_legend(override.aes = list(size=5)))+
  labs(title="Yield (2020)",
       # x="Nitrogen Application (kg N /ha)",
            x=element_blank(),
       y = "Yield  (kg/ha)",
       color=('Irrigation Treatment'),
       # shape=('Irrigation Treatment'),
       # linetype=('Irrigation Treatment')
  )


Yield_plot_Cotton_2020
# ggsave(paste0(plot_loc,"Sept_Yield_plot_Cotton_2020.png"),Yield_plot_Cotton_2020, width = 7, height = 5, units="in", dpi = 600)
library(ggpubr)
p <- ggarrange(Yield_plot_Cotton_2020 , Yield_plot_Cotton_2021,
          nue_plot_Cotton_2020 ,nue_plot_Cotton_2021,
          WUE_plot_Cotton_2020, WUE_plot_Cotton_2021
          ,ncol=2, nrow=3, common.legend = TRUE, legend="bottom")
  
ggp_all <- annotate_figure(p, top = text_grob("Cotton results"
               , face = "bold", size = 18))
ggp_all

 ggsave(paste0(plot_loc,"22SI_Cotton.png"),ggp_all, width = 12, height = 12, units="in", dpi = 600)

