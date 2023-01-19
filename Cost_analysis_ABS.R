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

corn_stats_21 <- read.csv(paste0(dat_loc, "corn_stats.csv"))
colnames(corn_stats_21)[1]<- "Rep"
corn_stats_21 <- corn_stats_21[-c(2)]
corn_stats_21<-dplyr::mutate(corn_stats_21,  Rep = case_when(
  Rep ==1  ~ 5,
  Rep ==2  ~ 6,
  Rep ==3  ~ 7,
  Rep ==4  ~ 8,
))
# corn_stats_21[,c(5:ncol(corn_stats_21))] <- sapply(corn_stats_21[,c(5:ncol(corn_stats_21))], function(x) as.numeric(as.character(x)))  

dat_loc2<-paste0(Github_loc,"/Data_2020/")
corn_stats_20 <- read.csv(paste0(dat_loc2, "Corn2020_dat.csv"))
corn_stats_20 <- corn_stats_20[-c(5:7)]

# Year = 2021

Corn_data <- function(Year){
Irrigation_dat <- as.data.frame(unique(corn_stats_21$Irrigation))
if (Year == 2021) {
  Irrigation_dat$Irrigation_amt <- c(0,2.2,1.5) 
  dat = corn_stats_21
}else{
  Irrigation_dat$Irrigation_amt <- c(0,5.2,3.5)
  dat = corn_stats_20
}

colnames(Irrigation_dat)[1] <- "Irrigation"

dat<- left_join(dat, Irrigation_dat, by = "Irrigation")
rm(Irrigation_dat)

Non_fertilized_summary<-dat %>% 
  filter(Nrate == 0) %>% 
  dplyr::group_by(Irrigation)%>%
  dplyr::summarise(Nuptake_mean0= mean(Nuptake),
            Yield_mean0 = mean(Yield))

Non_irrigated_summary<-dat %>% 
  filter(Irrigation == "None") %>% 
  dplyr::group_by(Nrate)%>%
  dplyr::summarise(Nuptake_mean0 = mean(Nuptake),
                   Yield_mean0 = mean(Yield))


 
dat<- left_join(dat, Non_fertilized_summary[,c(1,2)], by = "Irrigation") 
dat<- left_join(dat, Non_irrigated_summary[,c(1,3)], by = "Nrate") 
dat$NUE <- round(ifelse(dat$Nrate == 0, 0, (dat$Nuptake - dat$Nuptake_mean0)/dat$Nrate),2)

dat$irrWUE <- round(ifelse(dat$Irrigation == "None", 0, (dat$Yield - dat$Yield_mean0)/dat$Irrigation_amt),2)

return(dat)
}

corn_stats_20 <- Corn_data(2020)
corn_stats_21 <- Corn_data(2021)





#####################################


####################################################################


# https://www.eia.gov/dnav/pet/hist/LeafHandler.ashx?n=PET&s=EMD_EPD2D_PTE_NUS_DPG&f=M
# 

Cost_analysis <- function(Year, Irrigtaion_Type,Price_condition){

  if (Year == 2020 & Irrigtaion_Type == "Full") {
    Irri = "Full"
    dat = corn_stats_20
    Annual_Acre_inch = 5.2
    Avg_Application_rate = 0.87
  }else if(Year == 2020 & Irrigtaion_Type == "Environmental"){
    Irri= "Environmental "
    dat = corn_stats_20
    Annual_Acre_inch = 3.5
    Avg_Application_rate = 0.875
  }else if(Year == 2020 & Irrigtaion_Type == "None"){
    Irri= "None"
    dat = corn_stats_20
    Annual_Acre_inch = 0
    Avg_Application_rate = 0
    }else if (Year == 2021 & Irrigtaion_Type == "Full"){
    Irri= "Full"
    dat = corn_stats_21
    Annual_Acre_inch = 2.2
    Avg_Application_rate = 0.73
  }else if (Year == 2021 & Irrigtaion_Type == "Environmental"){
    Irri= "Environmental "
    dat = corn_stats_21
    Annual_Acre_inch = 1.5
    Avg_Application_rate = 0.5
  }else if (Year == 2021 & Irrigtaion_Type == "None"){
    Irri= "None"
    dat = corn_stats_21
    Annual_Acre_inch = 0
    Avg_Application_rate = 0
  }

if (Price_condition == "recent") {
  Fertilizer_cost <- 1.14 #https://www.dtnpf.com/agriculture/web/ag/crops/article/2022/04/13/fertilizers-look-set-price-records
  Fuel_Cost <-  5.5
  Horsepower<- 60
  Irrigated_Area <- 1           
  T_coverage<- 0.67   #0.67 hours (65 acres take 43.5 hours)
  Corn_price <- 7.8  #$ per bushel
}else if(Price_condition == "old"){
  Fertilizer_cost <- 0.47  # https://www.dtnpf.com/agriculture/web/ag/crops/article/2019/01/16/fertilizer-prices-continue-rise#:~:text=N%20and%20UAN32%20%240.47%2Flb.,-N.
  Fuel_Cost <-  3   # https://www.eia.gov/dnav/pet/hist/LeafHandler.ashx?n=PET&s=EMD_EPD2D_PTE_NUS_DPG&f=M
  Horsepower<- 60
  Irrigated_Area <- 1           
  T_coverage<- 0.67   #0.67 hours (65 acres take 43.5 hours)
  Corn_price <- 3.5  #$ per bushel  #https://www.macrotrends.net/2532/corn-prices-historical-chart-data
}

  
  
  
  dat <- dat %>%
    filter(Irrigation == Irri) %>% 
    mutate(Operating_cost = 
             (0.044*Horsepower*Fuel_Cost*T_coverage)*(Irrigation_amt/Avg_Application_rate)) %>% 
    mutate(Fertilizer_cost = Nrate * Fertilizer_cost) %>% 
    mutate(Total_input_cost= Operating_cost+ Fertilizer_cost) %>% 
    mutate(Total_selling_price= Yield*Corn_price) %>% 
    mutate(P_return = Total_selling_price- Total_input_cost) 



  dat$Treatment <- as.factor(dat$Treatment)
   dat$Year = Year
 return(dat)
  
}


Full_21 <- Cost_analysis(2021,"Full","recent")
Full_20 <- Cost_analysis(2020,"Full","recent")
Env_21<- Cost_analysis(2021,"Environmental","recent")
Env_20 <- Cost_analysis(2020,"Environmental","recent")
None_21<- Cost_analysis(2021,"None","recent")
None_20 <- Cost_analysis(2020,"None","recent")

Recent_Corn_Cost <- rbind.data.frame(Full_20[,c(1,2,3,4,5,12:17)], Full_21[,c(1,2,3,4,7,12:17)],Env_20[,c(1,2,3,4,5,12:17)], Env_21[,c(1,2,3,4,7,12:17)],
                                                                                                    None_20[,c(1,2,3,4,5,12:17)], None_21[,c(1,2,3,4,7,12:17)])

Recent_Corn_Cost$Total_input_cost<-ifelse(is.nan(Recent_Corn_Cost$Operating_cost),Recent_Corn_Cost$Fertilizer_cost,Recent_Corn_Cost$Total_input_cost)
Recent_Corn_Cost$P_return<-ifelse(is.nan(Recent_Corn_Cost$Operating_cost),Recent_Corn_Cost$Total_selling_price- Recent_Corn_Cost$Fertilizer_cost,Recent_Corn_Cost$P_return)

Recent_Corn_Cost$Nrate <- as.factor(Recent_Corn_Cost$Nrate)


########################################################################################################
# Recent 2020
Recent_Corn_Summary_2020 <- Recent_Corn_Cost %>% 
  dplyr::filter(Year == 2020) %>% 
  dplyr::group_by(Irrigation,Nrate,Treatment) %>% 
  dplyr::summarise(Year = first(Year),
    Corn_P_return = mean(P_return))

Recent_Corn_Summary_2020$Cost_Type = "Recent"

NI_data <- Recent_Corn_Summary_2020 %>% 
  filter(Irrigation =="None")

Recent_Corn_Summary_2020 <- Recent_Corn_Summary_2020 %>% 
  filter(Irrigation !="None")

Recent_Corn_Summary_2020$Change = NA

Recent_Corn_Summary_2020$Change <- ifelse (Recent_Corn_Summary_2020$Nrate == 0, ((Recent_Corn_Summary_2020$Corn_P_return- NI_data$Corn_P_return[NI_data$Nrate == "0"])),
        ifelse(Recent_Corn_Summary_2020$Nrate == 80, ((Recent_Corn_Summary_2020$Corn_P_return- NI_data$Corn_P_return[NI_data$Nrate == "80"])),
               ifelse(Recent_Corn_Summary_2020$Nrate == 160,  ((Recent_Corn_Summary_2020$Corn_P_return- NI_data$Corn_P_return[NI_data$Nrate == "160"])),
                      ifelse(Recent_Corn_Summary_2020$Nrate == 240,((Recent_Corn_Summary_2020$Corn_P_return- NI_data$Corn_P_return[NI_data$Nrate == "240"])),0 )
                      ))
        ) 

########################################################################################################
# Recent 2021

Recent_Corn_Summary_2021 <- Recent_Corn_Cost %>% 
  dplyr::filter(Year == 2021) %>% 
  dplyr::group_by(Irrigation,Nrate,Treatment) %>% 
  dplyr::summarise(Year = first(Year),
                   Corn_P_return = mean(P_return))

Recent_Corn_Summary_2021$Cost_Type = "Recent"

NI_data <- Recent_Corn_Summary_2021 %>% 
  filter(Irrigation =="None")

Recent_Corn_Summary_2021 <- Recent_Corn_Summary_2021 %>% 
  filter(Irrigation !="None")

Recent_Corn_Summary_2021$Change = NA

Recent_Corn_Summary_2021$Change <- ifelse (Recent_Corn_Summary_2021$Nrate == 0, ((Recent_Corn_Summary_2021$Corn_P_return- NI_data$Corn_P_return[NI_data$Nrate == "0"])),
                                           ifelse(Recent_Corn_Summary_2021$Nrate == 80, ((Recent_Corn_Summary_2021$Corn_P_return- NI_data$Corn_P_return[NI_data$Nrate == "80"])),
                                                  ifelse(Recent_Corn_Summary_2021$Nrate == 160,  ((Recent_Corn_Summary_2021$Corn_P_return- NI_data$Corn_P_return[NI_data$Nrate == "160"])),
                                                         ifelse(Recent_Corn_Summary_2021$Nrate == 240,((Recent_Corn_Summary_2021$Corn_P_return- NI_data$Corn_P_return[NI_data$Nrate == "240"])),0 )
                                                  ))
) 

Recent_Corn_Summary_2020$Change  <- Recent_Corn_Summary_2020$Change *2.47105
Recent_Corn_Summary_2021$Change  <- Recent_Corn_Summary_2021$Change *2.47105

Recent_Corn_Summary_2020$Nrate<- as.numeric(as.character(Recent_Corn_Summary_2020$Nrate))
Recent_Corn_Summary_2020$Nrate <- round(Recent_Corn_Summary_2020$Nrate * 1.12085,digits = -1)
Recent_Corn_Summary_2020$Nrate <- as.factor(Recent_Corn_Summary_2020$Nrate)

Recent_Corn_Summary_2021$Nrate<- as.numeric(as.character(Recent_Corn_Summary_2021$Nrate))
Recent_Corn_Summary_2021$Nrate <- round(Recent_Corn_Summary_2021$Nrate * 1.12085,digits = -1)
Recent_Corn_Summary_2021$Nrate <- as.factor(Recent_Corn_Summary_2021$Nrate)


plot_dat2 <- Recent_Corn_Summary_2020 
plot_dat2<-dplyr::mutate(plot_dat2,  Irrigation = case_when(
      Irrigation == "Full" ~ "Full Irrigation",
      Irrigation == "Environmental "  ~ "Weather Informed",
    ))

p1 <- plot_dat2 %>%    filter(Nrate != 0) %>%
  ggplot(aes(x=as.factor(Nrate), y=Change, fill= Irrigation))+
  geom_bar(stat = "identity", position=position_dodge(), colour="black")+
  
  # geom_errorbar(aes(ymin=Change-se, ymax=Change+se),
  #               size=0.5,
  #               width=0.15,                    # Width of the error bars
  #               position=position_dodge(.9))+
  # geom_line(size=1)+
  theme_bw()+
  
  scale_y_continuous(limits= c(-200,400),breaks=seq(-200, 400, 100))+
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
  # scale_fill_manual(values=c("#bdbdbd", "#a6bddb", "#fee0d2"), 
  #                   breaks=c("1Full Irrigation", "2Weather Informed", "3No Irrigation"),
  #                   labels=c("Full Irrigation", "Weather Informed", "No Irrigation"))+
  guides(colour = guide_legend(override.aes = list(size=5)))+
  labs(title="Corn 2020",
       # x="N Application (lb/acre)",
       x= element_blank(),
       y = " ($/ha)",
       color=('Irrigation Treatment'),
       # shape=('Irrigation Treatment'),
       # linetype=('Irrigation Treatment')
  )

p1
#####################################################################################
plot_dat2 <- Recent_Corn_Summary_2021 
plot_dat2<-dplyr::mutate(plot_dat2,  Irrigation = case_when(
  Irrigation == "Full" ~ "Full Irrigation",
  Irrigation == "Environmental "  ~ "Weather Informed",
))

p2 <- plot_dat2 %>%    filter(Nrate != 0) %>%
  ggplot(aes(x=as.factor(Nrate), y=Change, fill= Irrigation))+
  geom_bar(stat = "identity", position=position_dodge(), colour="black")+
  
  # geom_errorbar(aes(ymin=Change-se, ymax=Change+se),
  #               size=0.5,
  #               width=0.15,                    # Width of the error bars
  #               position=position_dodge(.9))+
  # geom_line(size=1)+
  theme_bw()+
  
  scale_y_continuous(limits= c(-200,400),breaks=seq(-200, 400, 100))+
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
  # scale_fill_manual(values=c("#bdbdbd", "#a6bddb", "#fee0d2"), 
  #                   breaks=c("1Full Irrigation", "2Weather Informed", "3No Irrigation"),
  #                   labels=c("Full Irrigation", "Weather Informed", "No Irrigation"))+
  guides(colour = guide_legend(override.aes = list(size=5)))+
  labs(title="Corn 2021",
        x="N Application (kg N/ha)",
       x= element_blank(),
       y = " ($/ha)",
       color=('Irrigation Treatment'),
       # shape=('Irrigation Treatment'),
       # linetype=('Irrigation Treatment')
  )

p2

###############################################################################################
# Old Prices

Full_21 <- Cost_analysis(2021,"Full","old")
Full_20 <- Cost_analysis(2020,"Full","old")
Env_21<- Cost_analysis(2021,"Environmental","old")
Env_20 <- Cost_analysis(2020,"Environmental","old")
None_21<- Cost_analysis(2021,"None","old")
None_20 <- Cost_analysis(2020,"None","old")

old_Corn_Cost <- rbind.data.frame(Full_20[,c(1,2,3,4,5,12:17)], Full_21[,c(1,2,3,4,7,12:17)],Env_20[,c(1,2,3,4,5,12:17)], Env_21[,c(1,2,3,4,7,12:17)],
                                     None_20[,c(1,2,3,4,5,12:17)], None_21[,c(1,2,3,4,7,12:17)])

old_Corn_Cost$Total_input_cost<-ifelse(is.nan(old_Corn_Cost$Operating_cost),old_Corn_Cost$Fertilizer_cost,old_Corn_Cost$Total_input_cost)
old_Corn_Cost$P_return<-ifelse(is.nan(old_Corn_Cost$Operating_cost),old_Corn_Cost$Total_selling_price- old_Corn_Cost$Fertilizer_cost,old_Corn_Cost$P_return)

old_Corn_Cost$Nrate <- as.factor(old_Corn_Cost$Nrate)


########################################################################################################
# old 2020
old_Corn_Summary_2020 <- old_Corn_Cost %>% 
  dplyr::filter(Year == 2020) %>% 
  dplyr::group_by(Irrigation,Nrate,Treatment) %>% 
  dplyr::summarise(Year = first(Year),
                   Corn_P_return = mean(P_return),
                   )

old_Corn_Summary_2020$Cost_Type = "old"

NI_data <- old_Corn_Summary_2020 %>% 
  filter(Irrigation =="None")

old_Corn_Summary_2020 <- old_Corn_Summary_2020 %>% 
  filter(Irrigation !="None")

old_Corn_Summary_2020$Change = NA

old_Corn_Summary_2020$Change <-  ifelse (old_Corn_Summary_2020$Nrate == 0, ((old_Corn_Summary_2020$Corn_P_return- NI_data$Corn_P_return[NI_data$Nrate == "0"])),
                                           ifelse(old_Corn_Summary_2020$Nrate == 80, ((old_Corn_Summary_2020$Corn_P_return- NI_data$Corn_P_return[NI_data$Nrate == "80"])),
                                                  ifelse(old_Corn_Summary_2020$Nrate == 160,  ((old_Corn_Summary_2020$Corn_P_return- NI_data$Corn_P_return[NI_data$Nrate == "160"])),
                                                         ifelse(old_Corn_Summary_2020$Nrate == 240,((old_Corn_Summary_2020$Corn_P_return- NI_data$Corn_P_return[NI_data$Nrate == "240"])),0 )
                                                  ))
)  

########################################################################################################
# old 2021

old_Corn_Summary_2021 <- old_Corn_Cost %>% 
  dplyr::filter(Year == 2021) %>% 
  dplyr::group_by(Irrigation,Nrate,Treatment) %>% 
  dplyr::summarise(Year = first(Year),
                   Corn_P_return = mean(P_return))

old_Corn_Summary_2021$Cost_Type = "old"

NI_data <- old_Corn_Summary_2021 %>% 
  filter(Irrigation =="None")

old_Corn_Summary_2021 <- old_Corn_Summary_2021 %>% 
  filter(Irrigation !="None")

old_Corn_Summary_2021$Change = NA

old_Corn_Summary_2021$Change <- ifelse (old_Corn_Summary_2021$Nrate == 0, ((old_Corn_Summary_2021$Corn_P_return- NI_data$Corn_P_return[NI_data$Nrate == "0"])),
                                           ifelse(old_Corn_Summary_2021$Nrate == 80, ((old_Corn_Summary_2021$Corn_P_return- NI_data$Corn_P_return[NI_data$Nrate == "80"])),
                                                  ifelse(old_Corn_Summary_2021$Nrate == 160,  ((old_Corn_Summary_2021$Corn_P_return- NI_data$Corn_P_return[NI_data$Nrate == "160"])),
                                                         ifelse(old_Corn_Summary_2021$Nrate == 240,((old_Corn_Summary_2021$Corn_P_return- NI_data$Corn_P_return[NI_data$Nrate == "240"])),0 )
                                                  ))
) 

# save(old_Corn_Summary_2021,old_Corn_Summary_2020,Recent_Corn_Summary_2020,Recent_Corn_Summary_2021,
#      file = "Corn_financial_data.RData")
# 

####################################################
plot_dat2 <- old_Corn_Summary_2020 
plot_dat2<-dplyr::mutate(plot_dat2,  Irrigation = case_when(
  Irrigation == "Full" ~ "Full Irrigation",
  Irrigation == "Environmental "  ~ "Weather Informed",
))

p3 <- plot_dat2 %>%    filter(Nrate != 0) %>%
  ggplot(aes(x=as.factor(Nrate), y=Change, fill= Irrigation))+
  geom_bar(stat = "identity", position=position_dodge(), colour="black")+
  
  # geom_errorbar(aes(ymin=Change-se, ymax=Change+se),
  #               size=0.5,
  #               width=0.15,                    # Width of the error bars
  #               position=position_dodge(.9))+
  # geom_line(size=1)+
  theme_bw()+
  
  scale_y_continuous(limits= c(-100,200),breaks=seq(-100, 200, 50))+
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
  # scale_fill_manual(values=c("#bdbdbd", "#a6bddb", "#fee0d2"), 
  #                   breaks=c("1Full Irrigation", "2Weather Informed", "3No Irrigation"),
  #                   labels=c("Full Irrigation", "Weather Informed", "No Irrigation"))+
  guides(colour = guide_legend(override.aes = list(size=5)))+
  labs(title="Corn 2020",
       # x="N Application (lb/acre)",
       x= element_blank(),
       y = "($/acre)",
       color=('Irrigation Treatment'),
       # shape=('Irrigation Treatment'),
       # linetype=('Irrigation Treatment')
  )

p3



#####################################################################################
plot_dat2 <- old_Corn_Summary_2021 
plot_dat2<-dplyr::mutate(plot_dat2,  Irrigation = case_when(
  Irrigation == "Full" ~ "Full Irrigation",
  Irrigation == "Environmental "  ~ "Weather Informed",
))

p4 <- plot_dat2 %>%    filter(Nrate != 0) %>%
  ggplot(aes(x=as.factor(Nrate), y=Change, fill= Irrigation))+
  geom_bar(stat = "identity", position=position_dodge(), colour="black")+
  
  # geom_errorbar(aes(ymin=Change-se, ymax=Change+se),
  #               size=0.5,
  #               width=0.15,                    # Width of the error bars
  #               position=position_dodge(.9))+
  # geom_line(size=1)+
  theme_bw()+
  
 scale_y_continuous(limits= c(-100,200),breaks=seq(-100, 200, 50))+
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
  # scale_fill_manual(values=c("#bdbdbd", "#a6bddb", "#fee0d2"), 
  #                   breaks=c("1Full Irrigation", "2Weather Informed", "3No Irrigation"),
  #                   labels=c("Full Irrigation", "Weather Informed", "No Irrigation"))+
  guides(colour = guide_legend(override.aes = list(size=5)))+
  labs(title="Corn 2021",
        x="N Application (lb/acre)",
       x= element_blank(),
       y = " ($/acre)",
       color=('Irrigation Treatment'),
       # shape=('Irrigation Treatment'),
       # linetype=('Irrigation Treatment')
  )

p4
 
library(ggpubr)     
 
p <- ggarrange(p1,p2, nrow = 2, common.legend = TRUE ) 

p <- annotate_figure(p, top = text_grob("Financial benefit relative to NI \nRecent prices", 
                                      color = "black", face = "bold", size = 14))


p

ggsave(paste0(plot_loc,"ABS_Corn_profit_oct_recent.png"),p, width = 10, height = 8, units="in", dpi = 600)


p_1 <- ggarrange(p3,p4, nrow = 2, common.legend = TRUE ) 


p_1 <- annotate_figure(p_1, top = text_grob("Historic prices", 
                                        color = "black", face = "bold", size = 14))


p_1

ggsave(paste0(plot_loc,"ABS_Corn_profit_oct_old.png"),p_1, width = 10, height = 8, units="in", dpi = 600)

######################################################################################################

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

Cotton_stats_21 <- read.csv(paste0(dat_loc, "Cotton_stats.csv"))
colnames(Cotton_stats_21)[1]<- "Rep"
# Cotton_stats_21 <- Cotton_stats_21[-c(2)]
# Cotton_stats_21$Nrate <- as.factor(Cotton_stats_21$Nrate)

# Cotton_stats_21[,c(5:ncol(Cotton_stats_21))] <- sapply(Cotton_stats_21[,c(5:ncol(Cotton_stats_21))], function(x) as.numeric(as.character(x)))  
Cotton_stats_21<-dplyr::mutate(Cotton_stats_21,  Rep = case_when(
  Rep ==1  ~ 5,
  Rep ==2  ~ 6,
  Rep ==3  ~ 7,
  Rep ==4  ~ 8,
))

dat_loc2<-paste0(Github_loc,"/Data_2020/")
Cotton_stats_20 <- read.csv(paste0(dat_loc2, "Cotton2020_dat.csv"))
# Cotton_stats_20_NUE <- Cotton_stats_20 %>% 
#   filter(Rep <4)
Cotton_stats_20$Irrigation[Cotton_stats_20$Irrigation =="Full  "] <- "Full"
Cotton_stats_20$Irrigation[Cotton_stats_20$Irrigation =="Full  "] <- "Full"

# Year = 2021

cotton_data <- function(Year){
  Irrigation_dat <- as.data.frame(unique(Cotton_stats_21$Irrigation))
  if (Year == 2021) {
    Irrigation_dat$Irrigation_amt <- c(0,2.2,1.5) 
    dat = Cotton_stats_21
  }else{
    Irrigation_dat$Irrigation_amt <- c(0,6.4,4.1)
    dat = Cotton_stats_20
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

Cotton_stats_20 <- cotton_data(2020)
Cotton_stats_21 <- cotton_data(2021)

# Cotton_stats_20$Year = 2020
# Cotton_stats_21$Year = 2021

Combined_data <- rbind.data.frame(Cotton_stats_20,Cotton_stats_21)










Cost_analysis <- function(Year, Irrigtaion_Type,Price_condition){
  
  if (Year == 2020 & Irrigtaion_Type == "Full") {
    Irri = "Full"
    dat = Cotton_stats_20
    Annual_Acre_inch = 6.4
    Avg_Application_rate = 1.06
  }else if(Year == 2020 & Irrigtaion_Type == "Environmental"){
    Irri= "Environmental "
    dat = Cotton_stats_20
    Annual_Acre_inch = 4.1
    Avg_Application_rate = 1.02
  }else if(Year == 2020 & Irrigtaion_Type == "None"){
    Irri= "None"
    dat = Cotton_stats_20
    Annual_Acre_inch = 0
    Avg_Application_rate = 0
  }else if (Year == 2021 & Irrigtaion_Type == "Full"){
    Irri= "Full"
    dat = Cotton_stats_21
    Annual_Acre_inch = 2.2
    Avg_Application_rate = 0.73
  }else if (Year == 2021 & Irrigtaion_Type == "Environmental"){
    Irri= "Environmental "
    dat = Cotton_stats_21
    Annual_Acre_inch = 1.2
    Avg_Application_rate = 0.4
  }else if (Year == 2021 & Irrigtaion_Type == "None"){
    Irri= "None"
    dat = Cotton_stats_21
    Annual_Acre_inch = 0
    Avg_Application_rate = 0
  }
  
  if (Price_condition == "recent") {
    Fertilizer_cost <- 1.14 #https://www.dtnpf.com/agriculture/web/ag/crops/article/2022/04/13/fertilizers-look-set-price-records
    Fuel_Cost <-  5.5
    Horsepower<- 60
    Irrigated_Area <- 1           
    T_coverage<- 0.67   #0.67 hours (65 acres take 43.5 hours)
    Cotton_price <- 1.4  #$ per lb
  }else if(Price_condition == "old"){
    Fertilizer_cost <- 0.47  # https://www.dtnpf.com/agriculture/web/ag/crops/article/2019/01/16/fertilizer-prices-continue-rise#:~:text=N%20and%20UAN32%20%240.47%2Flb.,-N.
    Fuel_Cost <-  3   # https://www.eia.gov/dnav/pet/hist/LeafHandler.ashx?n=PET&s=EMD_EPD2D_PTE_NUS_DPG&f=M
    Horsepower<- 60
    Irrigated_Area <- 1           
    T_coverage<- 0.67   #0.67 hours (65 acres take 43.5 hours)
    Cotton_price <- 0.7  #$ per bushel  #https://www.macrotrends.net/2532/Cotton-prices-historical-chart-data
  }
  
  
  
  
  dat <- dat %>%
    filter(Irrigation == Irri) %>% 
    mutate(Operating_cost = 
             (0.044*Horsepower*Fuel_Cost*T_coverage)*(Irrigation_amt/Avg_Application_rate)) %>% 
    mutate(Fertilizer_cost = Nrate * Fertilizer_cost) %>% 
    mutate(Total_input_cost= Operating_cost+ Fertilizer_cost) %>% 
    mutate(Total_selling_price= Yield*Cotton_price) %>% 
    mutate(P_return = Total_selling_price- Total_input_cost) 
  
  
  
  # dat$Treatment <- as.factor(dat$Treatment)
   dat$Year = Year
  return(dat)
  
}


Full_21 <- Cost_analysis(2021,"Full","recent")
Full_20 <- Cost_analysis(2020,"Full","recent")
Env_21<- Cost_analysis(2021,"Environmental","recent")
Env_20 <- Cost_analysis(2020,"Environmental","recent")
None_21<- Cost_analysis(2021,"None","recent")
None_20 <- Cost_analysis(2020,"None","recent")

Recent_Cotton_Cost <- rbind.data.frame(Full_20[,c(1,2,3,4,7,12:17)], Full_21[,c(1,2,3,4,7,12:17)],Env_20[,c(1,2,3,4,7,12:17)], Env_21[,c(1,2,3,4,7,12:17)],
                                       None_20[,c(1,2,3,4,7,12:17)], None_21[,c(1,2,3,4,7,12:17)])

Recent_Cotton_Cost$Total_input_cost<-ifelse(is.nan(Recent_Cotton_Cost$Operating_cost),Recent_Cotton_Cost$Fertilizer_cost,Recent_Cotton_Cost$Total_input_cost)
Recent_Cotton_Cost$P_return<-ifelse(is.nan(Recent_Cotton_Cost$Operating_cost),Recent_Cotton_Cost$Total_selling_price- Recent_Cotton_Cost$Fertilizer_cost,Recent_Cotton_Cost$P_return)

Recent_Cotton_Cost$Nrate <- as.factor(Recent_Cotton_Cost$Nrate)


########################################################################################################
# Recent 2020
Recent_Cotton_Summary_2020 <- Recent_Cotton_Cost %>% 
  dplyr::filter(Year == 2020) %>% 
  dplyr::group_by(Irrigation,Nrate,Treatment) %>% 
  dplyr::summarise(Year = first(Year),
                   Cotton_P_return = mean(P_return))

Recent_Cotton_Summary_2020$Cost_Type = "Recent"

NI_data <- Recent_Cotton_Summary_2020 %>% 
  filter(Irrigation =="None")

Recent_Cotton_Summary_2020 <- Recent_Cotton_Summary_2020 %>% 
  filter(Irrigation !="None")

Recent_Cotton_Summary_2020$Change = NA

Recent_Cotton_Summary_2020$Change <- ifelse (Recent_Cotton_Summary_2020$Nrate == 0,  ((Recent_Cotton_Summary_2020$Cotton_P_return- NI_data$Cotton_P_return[NI_data$Nrate == "0"])),
                                           ifelse(Recent_Cotton_Summary_2020$Nrate == 40,  ((Recent_Cotton_Summary_2020$Cotton_P_return- NI_data$Cotton_P_return[NI_data$Nrate == "40"])),
                                                  ifelse(Recent_Cotton_Summary_2020$Nrate == 80,   ((Recent_Cotton_Summary_2020$Cotton_P_return- NI_data$Cotton_P_return[NI_data$Nrate == "80"])),
                                                         ifelse(Recent_Cotton_Summary_2020$Nrate == 120, ((Recent_Cotton_Summary_2020$Cotton_P_return- NI_data$Cotton_P_return[NI_data$Nrate == "120"])),0 )
                                                  ))
) 
Recent_Cotton_Summary_2020$Change <- Recent_Cotton_Summary_2020$Change *2.47105
########################################################################################################
# Recent 2021

Recent_Cotton_Summary_2021 <- Recent_Cotton_Cost %>% 
  dplyr::filter(Year == 2021) %>% 
  dplyr::group_by(Irrigation,Nrate,Treatment) %>% 
  dplyr::summarise(Year = first(Year),
                   Cotton_P_return = mean(P_return))

Recent_Cotton_Summary_2021$Cost_Type = "Recent"

NI_data <- Recent_Cotton_Summary_2021 %>% 
  filter(Irrigation =="None")

Recent_Cotton_Summary_2021 <- Recent_Cotton_Summary_2021 %>% 
  filter(Irrigation !="None")

Recent_Cotton_Summary_2021$Change = NA

Recent_Cotton_Summary_2021$Change <- ifelse (Recent_Cotton_Summary_2021$Nrate == 0,  ((Recent_Cotton_Summary_2021$Cotton_P_return- NI_data$Cotton_P_return[NI_data$Nrate == "0"])),
                                             ifelse(Recent_Cotton_Summary_2021$Nrate == 40,  ((Recent_Cotton_Summary_2021$Cotton_P_return- NI_data$Cotton_P_return[NI_data$Nrate == "40"])),
                                                    ifelse(Recent_Cotton_Summary_2021$Nrate == 80,   ((Recent_Cotton_Summary_2021$Cotton_P_return- NI_data$Cotton_P_return[NI_data$Nrate == "80"])),
                                                           ifelse(Recent_Cotton_Summary_2021$Nrate == 120, ((Recent_Cotton_Summary_2021$Cotton_P_return- NI_data$Cotton_P_return[NI_data$Nrate == "120"])),0 )
                                                    ))
) 

Recent_Cotton_Summary_2021$Change <- Recent_Cotton_Summary_2021$Change*2.47105

Recent_Cotton_Summary_2020$Nrate<- as.numeric(as.character(Recent_Cotton_Summary_2020$Nrate))
Recent_Cotton_Summary_2020$Nrate <- round(Recent_Cotton_Summary_2020$Nrate * 1.12085,0)
Recent_Cotton_Summary_2020$Nrate <- as.factor(Recent_Cotton_Summary_2020$Nrate)

Recent_Cotton_Summary_2021$Nrate<- as.numeric(as.character(Recent_Cotton_Summary_2021$Nrate))
Recent_Cotton_Summary_2021$Nrate <- round(Recent_Cotton_Summary_2021$Nrate * 1.12085,0)
Recent_Cotton_Summary_2021$Nrate <- as.factor(Recent_Cotton_Summary_2021$Nrate)


plot_dat2 <- Recent_Cotton_Summary_2020 
plot_dat2<-dplyr::mutate(plot_dat2,  Irrigation = case_when(
  Irrigation == "Full" ~ "Full Irrigation",
  Irrigation == "Environmental "  ~ "Weather Informed",
))

p5 <- plot_dat2 %>%    filter(Nrate != 0) %>%
  ggplot(aes(x=as.factor(Nrate), y=Change, fill= Irrigation))+
  geom_bar(stat = "identity", position=position_dodge(), colour="black")+
  
  # geom_errorbar(aes(ymin=Change-se, ymax=Change+se),
  #               size=0.5,
  #               width=0.15,                    # Width of the error bars
  #               position=position_dodge(.9))+
  # geom_line(size=1)+
  theme_bw()+
  
  scale_y_continuous(limits= c(-1800,600),breaks=seq(-1800,600, 300))+
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
  # scale_fill_manual(values=c("#bdbdbd", "#a6bddb", "#fee0d2"), 
  #                   breaks=c("1Full Irrigation", "2Weather Informed", "3No Irrigation"),
  #                   labels=c("Full Irrigation", "Weather Informed", "No Irrigation"))+
  guides(colour = guide_legend(override.aes = list(size=5)))+
  labs(title="Cotton 2020",
       # x="N Application (lb/acre)",
       x= element_blank(),
       y = "($/ha)",
       color=('Irrigation Treatment'),
       # shape=('Irrigation Treatment'),
       # linetype=('Irrigation Treatment')
  )

p5
#####################################################################################
plot_dat2 <- Recent_Cotton_Summary_2021 
plot_dat2<-dplyr::mutate(plot_dat2,  Irrigation = case_when(
  Irrigation == "Full" ~ "Full Irrigation",
  Irrigation == "Environmental "  ~ "Weather Informed",
))

p6 <- plot_dat2 %>%    filter(Nrate != 0) %>%
  ggplot(aes(x=as.factor(Nrate), y=Change, fill= Irrigation))+
  geom_bar(stat = "identity", position=position_dodge(), colour="black")+
  
  # geom_errorbar(aes(ymin=Change-se, ymax=Change+se),
  #               size=0.5,
  #               width=0.15,                    # Width of the error bars
  #               position=position_dodge(.9))+
  # geom_line(size=1)+
  theme_bw()+
  
   scale_y_continuous(limits= c(-1800,500),breaks=seq(-1800,6600, 300))+
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
  # scale_fill_manual(values=c("#bdbdbd", "#a6bddb", "#fee0d2"), 
  #                   breaks=c("1Full Irrigation", "2Weather Informed", "3No Irrigation"),
  #                   labels=c("Full Irrigation", "Weather Informed", "No Irrigation"))+
  guides(colour = guide_legend(override.aes = list(size=5)))+
  labs(title="Cotton 2021",
       x="N Application (kg N/ha)",
       x= element_blank(),
       y = "($/ha)",
       color=('Irrigation Treatment'),
       # shape=('Irrigation Treatment'),
       # linetype=('Irrigation Treatment')
  )

p6

###############################################################################################
# Old Prices

Full_21 <- Cost_analysis(2021,"Full","old")
Full_20 <- Cost_analysis(2020,"Full","old")
Env_21<- Cost_analysis(2021,"Environmental","old")
Env_20 <- Cost_analysis(2020,"Environmental","old")
None_21<- Cost_analysis(2021,"None","old")
None_20 <- Cost_analysis(2020,"None","old")

old_Cotton_Cost <- rbind.data.frame(Full_20[,c(1,2,3,4,7,12:17)], Full_21[,c(1,2,3,4,7,12:17)],Env_20[,c(1,2,3,4,7,12:17)], Env_21[,c(1,2,3,4,7,12:17)],
                                       None_20[,c(1,2,3,4,7,12:17)], None_21[,c(1,2,3,4,7,12:17)])

old_Cotton_Cost$Total_input_cost<-ifelse(is.nan(old_Cotton_Cost$Operating_cost),old_Cotton_Cost$Fertilizer_cost,old_Cotton_Cost$Total_input_cost)
old_Cotton_Cost$P_return<-ifelse(is.nan(old_Cotton_Cost$Operating_cost),old_Cotton_Cost$Total_selling_price- old_Cotton_Cost$Fertilizer_cost,old_Cotton_Cost$P_return)

old_Cotton_Cost$Nrate <- as.factor(old_Cotton_Cost$Nrate)


########################################################################################################
# old 2020
old_Cotton_Summary_2020 <- old_Cotton_Cost %>% 
  dplyr::filter(Year == 2020) %>% 
  dplyr::group_by(Irrigation,Nrate,Treatment) %>% 
  dplyr::summarise(Year = first(Year),
                   Cotton_P_return = mean(P_return))

old_Cotton_Summary_2020$Cost_Type = "old"

NI_data <- old_Cotton_Summary_2020 %>% 
  filter(Irrigation =="None")

old_Cotton_Summary_2020 <- old_Cotton_Summary_2020 %>% 
  filter(Irrigation !="None")

old_Cotton_Summary_2020$Change = NA

old_Cotton_Summary_2020$Change <- ifelse (old_Cotton_Summary_2020$Nrate == 0,  ((old_Cotton_Summary_2020$Cotton_P_return- NI_data$Cotton_P_return[NI_data$Nrate == "0"])),
                                             ifelse(old_Cotton_Summary_2020$Nrate == 40,  ((old_Cotton_Summary_2020$Cotton_P_return- NI_data$Cotton_P_return[NI_data$Nrate == "40"])),
                                                    ifelse(old_Cotton_Summary_2020$Nrate == 80,   ((old_Cotton_Summary_2020$Cotton_P_return- NI_data$Cotton_P_return[NI_data$Nrate == "80"])),
                                                           ifelse(old_Cotton_Summary_2020$Nrate == 120, ((old_Cotton_Summary_2020$Cotton_P_return- NI_data$Cotton_P_return[NI_data$Nrate == "120"])),0 )
                                                    ))
) 

########################################################################################################
# old 2021

old_Cotton_Summary_2021 <- old_Cotton_Cost %>% 
  dplyr::filter(Year == 2021) %>% 
  dplyr::group_by(Irrigation,Nrate,Treatment) %>% 
  dplyr::summarise(Year = first(Year),
                   Cotton_P_return = mean(P_return))

old_Cotton_Summary_2021$Cost_Type = "old"

NI_data <- old_Cotton_Summary_2021 %>% 
  filter(Irrigation =="None")

old_Cotton_Summary_2021 <- old_Cotton_Summary_2021 %>% 
  filter(Irrigation !="None")

old_Cotton_Summary_2021$Change = NA

old_Cotton_Summary_2021$Change <- ifelse (old_Cotton_Summary_2021$Nrate == 0,  ((old_Cotton_Summary_2021$Cotton_P_return- NI_data$Cotton_P_return[NI_data$Nrate == "0"])),
                                             ifelse(old_Cotton_Summary_2021$Nrate == 40,  ((old_Cotton_Summary_2021$Cotton_P_return- NI_data$Cotton_P_return[NI_data$Nrate == "40"])),
                                                    ifelse(old_Cotton_Summary_2021$Nrate == 80,   ((old_Cotton_Summary_2021$Cotton_P_return- NI_data$Cotton_P_return[NI_data$Nrate == "80"])),
                                                           ifelse(old_Cotton_Summary_2021$Nrate == 120, ((old_Cotton_Summary_2021$Cotton_P_return- NI_data$Cotton_P_return[NI_data$Nrate == "120"])),0 )
                                                    ))
) 

plot_dat2 <- old_Cotton_Summary_2020 
plot_dat2<-dplyr::mutate(plot_dat2,  Irrigation = case_when(
  Irrigation == "Full" ~ "Full Irrigation",
  Irrigation == "Environmental "  ~ "Weather Informed",
))

p7 <- plot_dat2 %>%    filter(Nrate != 0) %>%
  ggplot(aes(x=as.factor(Nrate), y=Change, fill= Irrigation))+
  geom_bar(stat = "identity", position=position_dodge(), colour="black")+
  
  # geom_errorbar(aes(ymin=Change-se, ymax=Change+se),
  #               size=0.5,
  #               width=0.15,                    # Width of the error bars
  #               position=position_dodge(.9))+
  # geom_line(size=1)+
  theme_bw()+
  
  scale_y_continuous(limits= c(-1100,500),breaks=seq(-1000,500, 300))+
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
  # scale_fill_manual(values=c("#bdbdbd", "#a6bddb", "#fee0d2"), 
  #                   breaks=c("1Full Irrigation", "2Weather Informed", "3No Irrigation"),
  #                   labels=c("Full Irrigation", "Weather Informed", "No Irrigation"))+
  guides(colour = guide_legend(override.aes = list(size=5)))+
  labs(title="Cotton 2020",
       # x="N Application (lb/acre)",
       x= element_blank(),
       y = "($/ha)",
       color=('Irrigation Treatment'),
       # shape=('Irrigation Treatment'),
       # linetype=('Irrigation Treatment')
  )

p7
#####################################################################################
plot_dat2 <- old_Cotton_Summary_2021 
plot_dat2<-dplyr::mutate(plot_dat2,  Irrigation = case_when(
  Irrigation == "Full" ~ "Full Irrigation",
  Irrigation == "Environmental "  ~ "Weather Informed",
))

 
p8 <- plot_dat2 %>%    filter(Nrate != 0) %>%   
  ggplot(aes(x=as.factor(Nrate), y=Change, fill= Irrigation))+
  geom_bar(stat = "identity", position=position_dodge(), colour="black")+
  
  # geom_errorbar(aes(ymin=Change-se, ymax=Change+se),
  #               size=0.5,
  #               width=0.15,                    # Width of the error bars
  #               position=position_dodge(.9))+
  # geom_line(size=1)+
  theme_bw()+
  
  scale_y_continuous(limits= c(-700,300),breaks=seq(-800,200, 200))+
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
  # scale_fill_manual(values=c("#bdbdbd", "#a6bddb", "#fee0d2"), 
  #                   breaks=c("1Full Irrigation", "2Weather Informed", "3No Irrigation"),
  #                   labels=c("Full Irrigation", "Weather Informed", "No Irrigation"))+
  guides(colour = guide_legend(override.aes = list(size=5)))+
  labs(title="Cotton 2021",
       x="N Application (lb/acre)",
       x= element_blank(),
       y = "($/acre)",
       color=('Irrigation Treatment'),
       # shape=('Irrigation Treatment'),
       # linetype=('Irrigation Treatment')
  )

p8

library(ggpubr)     

p <- ggarrange(p5,p6, nrow = 2, common.legend = TRUE ) 

p <- annotate_figure(p, top = text_grob("Financial benefit relative to NI \n Recent prices", 
                                        color = "black", face = "bold", size = 14))


p

ggsave(paste0(plot_loc,"ABS_Cotton_profit_oct_recent.png"),p, width = 10, height = 8, units="in", dpi = 600)


p_1 <- ggarrange(p7,p8, nrow = 2, common.legend = TRUE ) 


p_1 <- annotate_figure(p_1, top = text_grob("Financial benefit relative to NI \n Historic prices", 
                                            color = "black", face = "bold", size = 14))


p_1

ggsave(paste0(plot_loc,"ABSCotton_profit_oct_old.png"),p_1, width = 10, height = 8, units="in", dpi = 600)


##################
# Year 2021

### Recent

p_1 <- ggarrange(p1,p5,p2,p6, nrow = 2,ncol=2, common.legend = TRUE ) 
p_1

p_1<- annotate_figure(p_1, top = text_grob("Financial benefit relative to NI \n (Recent prices)", 
                                        color = "black", face = "bold", size = 14))


p_1

ggsave(paste0(plot_loc,"2SI_ABS_Profit_recent.png"),p_1, width = 10, height = 8,  bg = 'white',units="in", dpi = 600)


p_1 <- ggarrange(p3,p7,p4,p8, nrow = 2,ncol=2, common.legend = TRUE ) 
p_1

p_1<- annotate_figure(p_1, top = text_grob("Financial benefit relative to NI \n (Historic prices)", 
                                        color = "black", face = "bold", size = 14))


p_1

ggsave(paste0(plot_loc,"ABS_Profit_historic.png"),p_1, width = 10, height = 8, bg = 'white', units="in", dpi = 600)
