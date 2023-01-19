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


Year = 2020
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


Yield_stats_20 <- summarySE(Cotton_data_2020, measurevar="Yield", groupvars=c("Nrate"))
Yield_stats_21 <- summarySE(Cotton_data_2021, measurevar="Yield", groupvars=c("Nrate"))

irrWUE_stats_21 <- summarySE(Cotton_data_2021, measurevar="irrWUE", groupvars=c("Nrate"))
irrWUE_stats_20 <- summarySE(Cotton_data_2020, measurevar="irrWUE", groupvars=c("Nrate"))




Cotton_data_2020_NUE <- Cotton_data_2020 %>% 
  filter( Rep < 4 )

NUE_stats_20 <- summarySE(Cotton_data_2020_NUE, measurevar="NUE", groupvars=c("Nrate"))



NUE_stats_21 <- summarySE(Cotton_data_2021, measurevar="NUE", groupvars=c("Nrate"))


#######################################################
# Irrigation None is significant

Yield_stats_20 <- summarySE(Cotton_data_2020, measurevar="Yield", groupvars=c("Irrigation"))

# Yield_stats_20$.group <- NA

Yield_stats_21 <- summarySE(Cotton_data_2021, measurevar="Yield", groupvars=c("Irrigation"))
Yield_stats_21 <-Yield_stats_21[,c(1,3)]
Yield_stats_21$.group <- NA

NUE_stats_20 <- summarySE(Cotton_data_2020_NUE, measurevar="NUE", groupvars=c("Irrigation"))
NUE_stats_20 <-NUE_stats_20[,c(1,3)]
NUE_stats_20$.group <- NA


NUE_stats_21 <- summarySE(Cotton_data_2021, measurevar="NUE", groupvars=c("Irrigation"))
NUE_stats_21 <-NUE_stats_21[,c(1,3)]
NUE_stats_21$.group <- NA


irrWUE_stats_20 <- summarySE(Cotton_data_2020, measurevar="irrWUE", groupvars=c("Irrigation"))
irrWUE_stats_20 <-irrWUE_stats_20[,c(1,3)]
irrWUE_stats_20$.group <- NA

irrWUE_stats_21 <- summarySE(Cotton_data_2021, measurevar="irrWUE", groupvars=c("Irrigation"))
irrWUE_stats_21 <-irrWUE_stats_21[,c(1,3)]
irrWUE_stats_21$.group <- NA

Irrigation_summary  <- cbind.data.frame(Yield_stats_20,Yield_stats_21,NUE_stats_20,NUE_stats_21,irrWUE_stats_20,irrWUE_stats_21)





