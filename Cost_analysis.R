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

Recent_Corn_Summary_2020$Change <- ifelse (Recent_Corn_Summary_2020$Nrate == 0, 100* ((Recent_Corn_Summary_2020$Corn_P_return- NI_data$Corn_P_return[NI_data$Nrate == "0"])/
                                                                                      NI_data$Corn_P_return[NI_data$Nrate == "0"]),
        ifelse(Recent_Corn_Summary_2020$Nrate == 80, 100* ((Recent_Corn_Summary_2020$Corn_P_return- NI_data$Corn_P_return[NI_data$Nrate == "80"])/
                                                                                               NI_data$Corn_P_return[NI_data$Nrate == "80"]),
               ifelse(Recent_Corn_Summary_2020$Nrate == 160,  100* ((Recent_Corn_Summary_2020$Corn_P_return- NI_data$Corn_P_return[NI_data$Nrate == "160"])/
                                                                                                       NI_data$Corn_P_return[NI_data$Nrate == "160"]),
                      ifelse(Recent_Corn_Summary_2020$Nrate == 240,100* ((Recent_Corn_Summary_2020$Corn_P_return- NI_data$Corn_P_return[NI_data$Nrate == "240"])/
                                                                           NI_data$Corn_P_return[NI_data$Nrate == "240"]),0 )
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

Recent_Corn_Summary_2021$Change <- ifelse (Recent_Corn_Summary_2021$Nrate == 0, 100* ((Recent_Corn_Summary_2021$Corn_P_return- NI_data$Corn_P_return[NI_data$Nrate == "0"])/
                                                                                        NI_data$Corn_P_return[NI_data$Nrate == "0"]),
                                           ifelse(Recent_Corn_Summary_2021$Nrate == 80, 100* ((Recent_Corn_Summary_2021$Corn_P_return- NI_data$Corn_P_return[NI_data$Nrate == "80"])/
                                                                                                NI_data$Corn_P_return[NI_data$Nrate == "80"]),
                                                  ifelse(Recent_Corn_Summary_2021$Nrate == 160,  100* ((Recent_Corn_Summary_2021$Corn_P_return- NI_data$Corn_P_return[NI_data$Nrate == "160"])/
                                                                                                         NI_data$Corn_P_return[NI_data$Nrate == "160"]),
                                                         ifelse(Recent_Corn_Summary_2021$Nrate == 240,100* ((Recent_Corn_Summary_2021$Corn_P_return- NI_data$Corn_P_return[NI_data$Nrate == "240"])/
                                                                                                              NI_data$Corn_P_return[NI_data$Nrate == "240"]),0 )
                                                  ))
) 

plot_dat2 <- Recent_Corn_Summary_2020 
plot_dat2<-dplyr::mutate(plot_dat2,  Irrigation = case_when(
      Irrigation == "Full" ~ "Full Irrigation",
      Irrigation == "Environmental "  ~ "Weather Informed",
    ))

p1 <- plot_dat2 %>% 
  ggplot(aes(x=as.factor(Nrate), y=Change, fill= Irrigation))+
  geom_bar(stat = "identity", position=position_dodge(), colour="black")+
  
  # geom_errorbar(aes(ymin=Change-se, ymax=Change+se),
  #               size=0.5,
  #               width=0.15,                    # Width of the error bars
  #               position=position_dodge(.9))+
  # geom_line(size=1)+
  theme_bw()+
  
  scale_y_continuous(limits= c(-10,15),breaks=seq(-10, 15, 5))+
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
       y = "Financial benefit relative to NI \n ($/acre)",
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

p2 <- plot_dat2 %>% 
  ggplot(aes(x=as.factor(Nrate), y=Change, fill= Irrigation))+
  geom_bar(stat = "identity", position=position_dodge(), colour="black")+
  
  # geom_errorbar(aes(ymin=Change-se, ymax=Change+se),
  #               size=0.5,
  #               width=0.15,                    # Width of the error bars
  #               position=position_dodge(.9))+
  # geom_line(size=1)+
  theme_bw()+
  
  scale_y_continuous(limits= c(-10,15),breaks=seq(-10, 15, 5))+
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
       y = "Financial benefit relative to NI \n ($/acre)",
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

old_Corn_Summary_2020$Change <- ifelse (old_Corn_Summary_2020$Nrate == 0, 100* ((old_Corn_Summary_2020$Corn_P_return- NI_data$Corn_P_return[NI_data$Nrate == "0"])/
                                                                                        NI_data$Corn_P_return[NI_data$Nrate == "0"]),
                                           ifelse(old_Corn_Summary_2020$Nrate == 80, 100* ((old_Corn_Summary_2020$Corn_P_return- NI_data$Corn_P_return[NI_data$Nrate == "80"])/
                                                                                                NI_data$Corn_P_return[NI_data$Nrate == "80"]),
                                                  ifelse(old_Corn_Summary_2020$Nrate == 160,  100* ((old_Corn_Summary_2020$Corn_P_return- NI_data$Corn_P_return[NI_data$Nrate == "160"])/
                                                                                                         NI_data$Corn_P_return[NI_data$Nrate == "160"]),
                                                         ifelse(old_Corn_Summary_2020$Nrate == 240,100* ((old_Corn_Summary_2020$Corn_P_return- NI_data$Corn_P_return[NI_data$Nrate == "240"])/
                                                                                                              NI_data$Corn_P_return[NI_data$Nrate == "240"]),0 )
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

old_Corn_Summary_2021$Change <- ifelse (old_Corn_Summary_2021$Nrate == 0, 100* ((old_Corn_Summary_2021$Corn_P_return- NI_data$Corn_P_return[NI_data$Nrate == "0"])/
                                                                                        NI_data$Corn_P_return[NI_data$Nrate == "0"]),
                                           ifelse(old_Corn_Summary_2021$Nrate == 80, 100* ((old_Corn_Summary_2021$Corn_P_return- NI_data$Corn_P_return[NI_data$Nrate == "80"])/
                                                                                                NI_data$Corn_P_return[NI_data$Nrate == "80"]),
                                                  ifelse(old_Corn_Summary_2021$Nrate == 160,  100* ((old_Corn_Summary_2021$Corn_P_return- NI_data$Corn_P_return[NI_data$Nrate == "160"])/
                                                                                                         NI_data$Corn_P_return[NI_data$Nrate == "160"]),
                                                         ifelse(old_Corn_Summary_2021$Nrate == 240,100* ((old_Corn_Summary_2021$Corn_P_return- NI_data$Corn_P_return[NI_data$Nrate == "240"])/
                                                                                                              NI_data$Corn_P_return[NI_data$Nrate == "240"]),0 )
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

p3 <- plot_dat2 %>% 
  ggplot(aes(x=as.factor(Nrate), y=Change, fill= Irrigation))+
  geom_bar(stat = "identity", position=position_dodge(), colour="black")+
  
  # geom_errorbar(aes(ymin=Change-se, ymax=Change+se),
  #               size=0.5,
  #               width=0.15,                    # Width of the error bars
  #               position=position_dodge(.9))+
  # geom_line(size=1)+
  theme_bw()+
  
  scale_y_continuous(limits= c(-10,15),breaks=seq(-10, 15, 5))+
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
       y = "Financial benefit relative to NI \n ($/acre)",
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

p4 <- plot_dat2 %>% 
  ggplot(aes(x=as.factor(Nrate), y=Change, fill= Irrigation))+
  geom_bar(stat = "identity", position=position_dodge(), colour="black")+
  
  # geom_errorbar(aes(ymin=Change-se, ymax=Change+se),
  #               size=0.5,
  #               width=0.15,                    # Width of the error bars
  #               position=position_dodge(.9))+
  # geom_line(size=1)+
  theme_bw()+
  
  scale_y_continuous(limits= c(-10,15),breaks=seq(-10, 15, 5))+
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
       y = "Financial benefit relative to NI \n ($/acre)",
       color=('Irrigation Treatment'),
       # shape=('Irrigation Treatment'),
       # linetype=('Irrigation Treatment')
  )

p4
 
library(ggpubr)     
 
p <- ggarrange(p1,p2, nrow = 2, common.legend = TRUE ) 

p <- annotate_figure(p, top = text_grob("Recent prices", 
                                      color = "black", face = "bold", size = 14))


p

ggsave(paste0(plot_loc,"Corn_profit_oct+_recent.png"),p, width = 10, height = 8, units="in", dpi = 600)


p_1 <- ggarrange(p3,p4, nrow = 2, common.legend = TRUE ) 


p_1 <- annotate_figure(p_1, top = text_grob("Historic prices", 
                                        color = "black", face = "bold", size = 14))


p_1

ggsave(paste0(plot_loc,"Corn_profit_oct_old.png"),p_1, width = 10, height = 8, units="in", dpi = 600)



################################
# OLD before oct 18
# plot_dat <- left_join(old_Corn_Cost_Summary[,c(1,2,6)], Recent_Corn_Summary_2020[,c(1,2,6)], by = c("Nrate", "Irrigation"))
# colnames(plot_dat)[c(3,4)] <- c( "Historic prices", "Recent prices")
# 
# plot_dat2 <- pivot_longer(plot_dat,cols = c(3,4))
# 
#   plot_dat2<-dplyr::mutate(plot_dat2,  Irrigation = case_when(
#     Irrigation == "None" ~ "No Irrigation",
#     Irrigation == "Full" ~ "Full Irrigation",
#     Irrigation == "Environmental "  ~ "Weather Informed",
#   ))
# 
# p<- ggplot(plot_dat2, aes(x=Nrate, y=value , group = Irrigation)) +
#   geom_line(aes(linetype=Irrigation))+
#   geom_point(aes(shape=Irrigation))+
#   facet_grid(. ~ name)+
#   theme_bw()+
#   scale_y_continuous(limits= c(-10,310), breaks=c(-10,0,50,100,150,200,250,300))+
#   labs(title="Corn",
#        x="Nitrogen Application (lb/acre)", y = "% Change",
#        # color=('Irrigation Treatment'),
#        # shape=('Irrigation Treatment'),
#        # linetype=('Irrigation Treatment')
#   )+
# theme(plot.title = element_text(size=18),
#         legend.position="top",
#         legend.title=element_blank(),
#         legend.box = "horizontal",
#         legend.background = element_rect(fill="white",
#                                          size=0.5, linetype="solid",
#                                          colour ="white"),
#         legend.text=element_text(size=14),
#     
#         axis.text=element_text(size=14, colour="black"),
#         axis.title=element_text(size=14, colour="black"),
#         axis.line = element_line(colour = "black",
#                                  size = 0.5, linetype = "solid"),
#         axis.ticks = element_line(colour="black"),
#         panel.grid.major=element_line(colour = "light grey"),
#         panel.grid.minor=element_blank(),
#        strip.text = element_text(size = 14, color = "black" ),
#         strip.background = element_rect(
#           color="black", fill ="#FFE4C4", size=1, linetype="solid"))
#   
# p
# ggsave(paste0(plot_loc,"Corn_profit_plot.png"),p, width = 7, height = 5, units="in", dpi = 600)
#   ###Both Years

# Plot_Recent_Corn_Cost <- summarySE(Recent_Corn_Cost, measurevar="P_return", groupvars=c("Nrate","Irrigation","Treatment"))
# Plot_Old_Corn_Cost <- summarySE(old_Corn_Cost, measurevar="P_return", groupvars=c("Nrate","Irrigation","Treatment"))
# 
# Return_plot_dat<-dplyr::mutate(Plot_Old_Corn_Cost,  Irrigation = case_when(
#   Treatment <=4 ~ "3No Irrigation",
#   Treatment <=8 ~ "1Full Irrigation",
#   Treatment <=12 ~ "2Precision Irrigation",
# ))
# 
# Return_plot_corn_old<-Return_plot_dat %>% 
#    ggplot(aes(x=Nrate, y=P_return, fill= Irrigation))+
#   geom_bar(stat = "identity", position=position_dodge(), colour="black")+
#   geom_errorbar(aes(ymin=P_return-se, ymax=P_return+se),
#                 size=0.5,
#                 width=0.15,                    # Width of the error bars
#                 position=position_dodge(.9))+
#   # geom_line(size=1)+
#   theme_bw()+
#   # scale_x_continuous(breaks=seq(80, 240, 80))+
#   
#   theme(plot.title = element_text(size=18),
#         legend.position="top",
#         legend.title=element_blank(),
#         legend.box = "horizontal",
#         legend.background = element_rect(fill="white",
#                                          size=0.5, linetype="solid",
#                                          colour ="white"),
#         legend.text=element_text(size=14),
#         axis.text=element_text(size=14, colour="black"),
#         axis.title=element_text(size=14, colour="black"),
#         axis.line = element_line(colour = "black",
#                                  size = 0.5, linetype = "solid"),
#         axis.ticks = element_line(colour="black"),
#         panel.grid.major=element_line(colour = "light grey"),
#         panel.grid.minor=element_blank())+
#   scale_y_continuous(breaks=seq(0, 1600, 400))+
#   scale_fill_manual(values=c("#bdbdbd", "#a6bddb", "#fee0d2"), 
#                     breaks=c("1Full Irrigation", "2Precision Irrigation", "3No Irrigation"),
#                     labels=c("Full Irrigation", "Precision Irrigation", "No Irrigation"))+
#   guides(colour = guide_legend(override.aes = list(size=5)))+
#   labs(title="Cotton Yield (2020)",
#        x="Nitrogen Application (lb/acre)", y = "Yield  (lb/ac)",
#        color=('Irrigation Treatment'),
#        # shape=('Irrigation Treatment'),
#        # linetype=('Irrigation Treatment')
#   )
# Return_plot_corn_old
# 
# # ggsave(paste0(plot_loc,"P_return_Corn.png"),plot_P_return, width = 7, height = 5, units="in", dpi = 600)
# # 
# ###########################################################################################################################
# ################################################################################
# #
# Price_dat_21 <- Recent_Corn_Cost %>% 
#   filter(Rep > 4)
# Price_dat_20 <- Recent_Corn_Cost %>% 
#   filter(Rep < 5)
# 
# Plot_Price_dat_21 <- summarySE(Price_dat_21, measurevar="P_return", groupvars=c("Nrate","Irrigation","Treatment"))
# Plot_Price_dat_20 <- summarySE(Price_dat_20, measurevar="P_return", groupvars=c("Nrate","Irrigation","Treatment"))
# 
# Return_plot_dat_21<-dplyr::mutate(Plot_Price_dat_21,  Irrigation = case_when(
#   Treatment <=4 ~ "3No Irrigation",
#   Treatment <=8 ~ "1Full Irrigation",
#   Treatment <=12 ~ "2Precision Irrigation",
# ))
# 
# Return_plot_dat_20<-dplyr::mutate(Plot_Price_dat_20,  Irrigation = case_when(
#   Treatment <=4 ~ "3No Irrigation",
#   Treatment <=8 ~ "1Full Irrigation",
#   Treatment <=12 ~ "2Precision Irrigation",
# ))
# 
# p1<-Return_plot_dat_21 %>% 
#   ggplot(aes(x=Nrate, y=P_return, fill= Irrigation))+
#   geom_bar(stat = "identity", position=position_dodge(), colour="black")+
#   geom_errorbar(
#     aes(ymin=P_return-se, ymax=P_return+se),
#     size=0.5,
#     width=0.15,                    # Width of the error bars
#     position=position_dodge(.9))+
#   
#   # geom_line(size=1)+
#   theme_bw()+
#   # scale_y_continuous(breaks=seq(80, 240, 80))+
#   
#   
#   theme(plot.title = element_text(size=18),
#         legend.position="top",
#         legend.title=element_blank(),
#         legend.box = "horizontal",
#         legend.background = element_rect(fill="white",
#                                          size=0.5, linetype="solid",
#                                          colour ="white"),
#         legend.text=element_text(size=14),
#         axis.text=element_text(size=14, colour="black"),
#         axis.title=element_text(size=14, colour="black"),
#         axis.line = element_line(colour = "black",
#                                  size = 0.5, linetype = "solid"),
#         axis.ticks = element_line(colour="black"),
#         panel.grid.major=element_line(colour = "light grey"),
#         panel.grid.minor=element_blank())+
#   scale_y_continuous(limits = c(0,1800),
#                      breaks=seq(0,2000, 500))+
#   scale_fill_manual(values=c("#bdbdbd", "#a6bddb", "#fee0d2"), 
#                     breaks=c("1Full Irrigation", "2Precision Irrigation", "3No Irrigation"),
#                     labels=c("Full Irrigation", "Precision Irrigation", "No Irrigation"))+
#   guides(colour = guide_legend(override.aes = list(size=5)))+
#   labs(title="2021 Corn Returns (Recent prices)",
#        x="Nitrogen Application (lb/acre)", y = "Return  ($/acre)",
#        color=('Irrigation Treatment'),
#        # shape=('Irrigation Treatment'),
#        # linetype=('Irrigation Treatment')
#   )
# 
# p1
# 
# p2<-Return_plot_dat_20 %>% 
#   ggplot(aes(x=Nrate, y=P_return, fill= Irrigation))+
#   geom_bar(stat = "identity", position=position_dodge(), colour="black")+
#   geom_errorbar(
#     aes(ymin=P_return-se, ymax=P_return+se),
#     size=0.5,
#     width=0.15,                    # Width of the error bars
#     position=position_dodge(.9))+
#   
#   # geom_line(size=1)+
#   theme_bw()+
#   # scale_y_continuous(breaks=seq(80, 240, 80))+
#   
#   
#   theme(plot.title = element_text(size=18),
#         legend.position="top",
#         legend.title=element_blank(),
#         legend.box = "horizontal",
#         legend.background = element_rect(fill="white",
#                                          size=0.5, linetype="solid",
#                                          colour ="white"),
#         legend.text=element_text(size=14),
#         axis.text=element_text(size=14, colour="black"),
#         axis.title=element_text(size=14, colour="black"),
#         axis.line = element_line(colour = "black",
#                                  size = 0.5, linetype = "solid"),
#         axis.ticks = element_line(colour="black"),
#         panel.grid.major=element_line(colour = "light grey"),
#         panel.grid.minor=element_blank())+
#   scale_y_continuous(limits = c(0,1800),
#     breaks=seq(0,2000, 500))+
#   scale_fill_manual(values=c("#bdbdbd", "#a6bddb", "#fee0d2"), 
#                     breaks=c("1Full Irrigation", "2Precision Irrigation", "3No Irrigation"),
#                     labels=c("Full Irrigation", "Precision Irrigation", "No Irrigation"))+
#   guides(colour = guide_legend(override.aes = list(size=5)))+
#   labs(title="2020 Corn Returns (Recent prices)",
#        x="Nitrogen Application (lb/acre)", y = "Return  ($/acre)",
#        color=('Irrigation Treatment'),
#        # shape=('Irrigation Treatment'),
#        # linetype=('Irrigation Treatment')
#   )
# 
# p2
# 
# #############################################################
# ### Old Prices
# Price_dat_21 <- old_Corn_Cost %>% 
#   filter(Rep > 4)
# Price_dat_20 <- old_Corn_Cost %>% 
#   filter(Rep < 5)
# 
# Plot_Price_dat_21 <- summarySE(Price_dat_21, measurevar="P_return", groupvars=c("Nrate","Irrigation","Treatment"))
# Plot_Price_dat_20 <- summarySE(Price_dat_20, measurevar="P_return", groupvars=c("Nrate","Irrigation","Treatment"))
# 
# Return_plot_dat_21<-dplyr::mutate(Plot_Price_dat_21,  Irrigation = case_when(
#   Treatment <=4 ~ "3No Irrigation",
#   Treatment <=8 ~ "1Full Irrigation",
#   Treatment <=12 ~ "2Precision Irrigation",
# ))
# 
# Return_plot_dat_20<-dplyr::mutate(Plot_Price_dat_20,  Irrigation = case_when(
#   Treatment <=4 ~ "3No Irrigation",
#   Treatment <=8 ~ "1Full Irrigation",
#   Treatment <=12 ~ "2Precision Irrigation",
# ))
# 
# p3<-Return_plot_dat_21 %>% 
#   ggplot(aes(x=Nrate, y=P_return, fill= Irrigation))+
#   geom_bar(stat = "identity", position=position_dodge(), colour="black")+
#   geom_errorbar(
#     aes(ymin=P_return-se, ymax=P_return+se),
#     size=0.5,
#     width=0.15,                    # Width of the error bars
#     position=position_dodge(.9))+
#   
#   # geom_line(size=1)+
#   theme_bw()+
#   # scale_y_continuous(breaks=seq(80, 240, 80))+
#   
#   
#   theme(plot.title = element_text(size=18),
#         legend.position="top",
#         legend.title=element_blank(),
#         legend.box = "horizontal",
#         legend.background = element_rect(fill="white",
#                                          size=0.5, linetype="solid",
#                                          colour ="white"),
#         legend.text=element_text(size=14),
#         axis.text=element_text(size=14, colour="black"),
#         axis.title=element_text(size=14, colour="black"),
#         axis.line = element_line(colour = "black",
#                                  size = 0.5, linetype = "solid"),
#         axis.ticks = element_line(colour="black"),
#         panel.grid.major=element_line(colour = "light grey"),
#         panel.grid.minor=element_blank())+
#   scale_y_continuous(limits = c(0,1800),
#                      breaks=seq(0,2000, 500))+
#   scale_fill_manual(values=c("#bdbdbd", "#a6bddb", "#fee0d2"), 
#                     breaks=c("1Full Irrigation", "2Precision Irrigation", "3No Irrigation"),
#                     labels=c("Full Irrigation", "Precision Irrigation", "No Irrigation"))+
#   guides(colour = guide_legend(override.aes = list(size=5)))+
#   labs(title="2021 Corn Returns (Historic prices)",
#        x="Nitrogen Application (lb/acre)", y = "Return  ($/acre)",
#        color=('Irrigation Treatment'),
#        # shape=('Irrigation Treatment'),
#        # linetype=('Irrigation Treatment')
#   )
# 
# p3
# 
# p4<-Return_plot_dat_20 %>% 
#   ggplot(aes(x=Nrate, y=P_return, fill= Irrigation))+
#   geom_bar(stat = "identity", position=position_dodge(), colour="black")+
#   geom_errorbar(
#     aes(ymin=P_return-se, ymax=P_return+se),
#     size=0.5,
#     width=0.15,                    # Width of the error bars
#     position=position_dodge(.9))+
#   
#   # geom_line(size=1)+
#   theme_bw()+
#   # scale_y_continuous(breaks=seq(80, 240, 80))+
#   
#   
#   theme(plot.title = element_text(size=18),
#         legend.position="top",
#         legend.title=element_blank(),
#         legend.box = "horizontal",
#         legend.background = element_rect(fill="white",
#                                          size=0.5, linetype="solid",
#                                          colour ="white"),
#         legend.text=element_text(size=14),
#         axis.text=element_text(size=14, colour="black"),
#         axis.title=element_text(size=14, colour="black"),
#         axis.line = element_line(colour = "black",
#                                  size = 0.5, linetype = "solid"),
#         axis.ticks = element_line(colour="black"),
#         panel.grid.major=element_line(colour = "light grey"),
#         panel.grid.minor=element_blank())+
#   scale_y_continuous(limits = c(0,1800),
#                      breaks=seq(0,2000, 500))+
#   scale_fill_manual(values=c("#bdbdbd", "#a6bddb", "#fee0d2"), 
#                     breaks=c("1Full Irrigation", "2Precision Irrigation", "3No Irrigation"),
#                     labels=c("Full Irrigation", "Precision Irrigation", "No Irrigation"))+
#   guides(colour = guide_legend(override.aes = list(size=5)))+
#   labs(title="2020 Corn Returns (Historic prices)",
#        x="Nitrogen Application (lb/acre)", y = "Return  ($/acre)",
#        color=('Irrigation Treatment'),
#        # shape=('Irrigation Treatment'),
#        # linetype=('Irrigation Treatment')
#   )
# 
# p4
# 
# library("gridExtra")
# p <- grid.arrange(p1,p2,p3,p4, 
#              ncol = 2, nrow = 2)
# ggsave("Proftablity.png", plot = p, width = 12, height = 8, units = "in")
# ####################################################################################################################################
# 
# 
# # Cost_analysis(2021,"Precision")
# # Fertilizer_cost <- 0.32
# # Fuel_Cost <-  5.5
# # Horsepower<- 60
# # Irrigated_Area <- 1           # 1 acre
# # T_coverage<- 0.67               # 0.67 hours (65 acres take 43.5 hours) 0.0231 hours (~1.5 min, calculation made from Georgia, equations are available to do it more complex)
# # # Avg_Application_rate <- 0.75
# # Annual_Acre_inch <- 1.5          # This will change. 
# 
# # 
# # This is test run
# ####################################################################
# # Operating Cost per acre Inch 
# # Assumption: Fuel Pump
# 
# # Cost per gallon: 5.5$
# # Horsepower:60
# # Time for full coverage:43.5
# # Average Application Rate:1
# # Annual Acre Inch applied:16
# 
# # Operating_cost <- Horsepower*Fuel_Cost*T_coverage*Annual_Acre_inch/Avg_Application_rate*0.044
# # Operating_cost_per_acre <- Operating_cost/Irrigated_Area
# # Operating_cost_per_acre_Inch <- Operating_cost_per_acre/Annual_Acre_inch
# # 
# # Operating_cost
# # Operating_cost_per_acre
# # Operating_cost_per_acre_Inch
# 
# # Back to data
# #####################################################################
# # dat$Operating_cost <- Horsepower*Fuel_Cost*T_coverage*dat$Irrigation_amt/Avg_Application_rate*0.044
# # dat$Operating_cost <- dat$Operating_cost/Irrigated_Area  # Cost to irrigate 1.5 and 2.2 inches in sensor and full irrigation
# # 
# # dat$Fertilizer_cost <- dat$Nrate * Fertilizer_cost
# # 
# # dat$Total_input_cost <- dat$Operating_cost+dat$Fertilizer_cost
# # #################
# # # skipping labor and management cost (small and will be same for both cases)
# # 
# # ######################################################
# # # Selling price
# # 
# # Corn_price <- 7.8  #$ per bushel
# # dat$Total_selling_price <- dat$Yield*Corn_price
# # 
# # dat$P_return <- dat$Total_selling_price-dat$Total_input_cost
# # 
# # dat$Treatment <- as.factor(dat$Treatment)
# 
# #################################
# # PLOT
# ####
# # summary <- dat %>% 
# #   dplyr::group_by(Treatment) %>% 
# #   dplyr::summarise(Irrigation=first(Irrigation),
# #                    Nrate=first(Nrate),
# #                    NUE = mean(NUE),
# #                    WUE = mean(irrWUE),
# #                    P_return = mean(P_return))
# # 
# # 
# # 
# # dat$Nrate <- as.factor(dat$Nrate)
# # P_return_stats <- summarySE(dat, measurevar="P_return", groupvars=c("Nrate","Irrigation","Treatment"))
# # 
# # P_return_stats$Treatment <- as.numeric(as.character(P_return_stats$Treatment))
# # plot_dat<-dplyr::mutate(P_return_stats,  Irrigation = case_when(
# #   Treatment <=4 ~ "3No Irrigation",
# #   Treatment <=8 ~ "1Full Irrigation",
# #   Treatment <=12 ~ "2Precision Irrigation",
# # ))
# # 
# # plot_P_return<-plot_dat %>% 
# #   # filter(Nrate != 0) %>% 
# #   ggplot(aes(x=Nrate, y=P_return, fill= Irrigation))+
# #   geom_bar(stat = "identity", position=position_dodge(), colour="black")+
# #   geom_errorbar(aes(ymin=P_return-se, ymax=P_return+se),
# #                 size=0.5,
# #                 width=0.15,                    # Width of the error bars
# #                 position=position_dodge(.9))+
# #   # geom_line(size=1)+
# #   theme_bw()+
# #   scale_y_continuous(breaks=seq(0, 2000, 200))+
# #   theme(plot.title = element_text(size=18),
# #         legend.position="top",
# #         legend.title=element_blank(),
# #         legend.box = "horizontal",
# #         legend.background = element_rect(fill="white",
# #                                          size=0.5, linetype="solid",
# #                                          colour ="white"),
# #         legend.text=element_text(size=14),
# #         axis.text=element_text(size=14, colour="black"),
# #         axis.title=element_text(size=14, colour="black"),
# #         axis.line = element_line(colour = "black",
# #                                  size = 0.5, linetype = "solid"),
# #         axis.ticks = element_line(colour="black"),
# #         panel.grid.major=element_line(colour = "light grey"),
# #         panel.grid.minor=element_blank())+
# #   scale_fill_manual(values=c("#bdbdbd", "#a6bddb", "#fee0d2"), 
# #                     breaks=c("1Full Irrigation", "2Precision Irrigation", "3No Irrigation"),
# #                     labels=c("Full Irrigation", "Precision Irrigation", "No Irrigation"))+
# #   guides(colour = guide_legend(override.aes = list(size=5)))+
# #   labs(title="Corn P_return",
# #        x="Nitrogen Application (lb/acre)", y = "P_return ($)",
# #        color=('Irrigation Treatment'),
# #        # shape=('Irrigation Treatment'),
# #        # linetype=('Irrigation Treatment')
# #   )
# # plot_P_return
# # 
# # ggsave(paste0(plot_loc,"P_return_Corn.png"),plot_P_return, width = 7, height = 5, units="in", dpi = 600)
# # 
# # 
# # 
# # 
# # 
# # 
