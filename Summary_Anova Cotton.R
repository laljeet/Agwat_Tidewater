load(("F:/My Drive/Tidewater/Tidewater Data Analysis/Cotton_stats_significance.RData"))


Github_loc<-"F:/My Drive/Tidewater/Tidewater Data Analysis"
dat_loc<-paste0(Github_loc,"/Trial_2021/")
plot_loc<-paste0(Github_loc,"/Plot_2021/")
options(scipen = 9999)
setwd(Github_loc)
library(stargazer)
library(tidyverse)
library(Rmisc)
options(scipen=999)

###################################################################

# Nrate

Yield_stats_20 <- summarySE(Cotton_data_2020, measurevar="Yield", groupvars=c("Nrate"))
Yield_stats_20 <-left_join(Yield_stats_20[,c(1,3)],Yield_20_sign[,c(1,7)], by = "Nrate")

Yield_stats_21 <- summarySE(Cotton_data_2021, measurevar="Yield", groupvars=c("Nrate"))
Yield_stats_21 <-left_join(Yield_stats_21[,c(1,3)],Yield_21_sign[,c(1,7)], by = "Nrate")


irrWUE_stats_21 <- summarySE(Cotton_data_2021, measurevar="irrWUE", groupvars=c("Nrate"))
irrWUE_stats_21 <-left_join(irrWUE_stats_21[,c(1,3)],WUE_21_sign[,c(1,7)], by = "Nrate")

Cotton_data_2020_NUE <- Cotton_data_2020 %>% 
  filter( Rep < 4 )

NUE_stats_20 <- summarySE(Cotton_data_2020_NUE, measurevar="NUE", groupvars=c("Nrate"))
NUE_stats_20 <-NUE_stats_20[,c(1,3)]
NUE_stats_20$.group <- NA


NUE_stats_21 <- summarySE(Cotton_data_2021, measurevar="NUE", groupvars=c("Nrate"))
NUE_stats_21 <-NUE_stats_21[,c(1,3)]
NUE_stats_21$.group <- NA


irrWUE_stats_20 <- summarySE(Cotton_data_2020, measurevar="irrWUE", groupvars=c("Nrate"))
irrWUE_stats_20 <-irrWUE_stats_20[,c(1,3)]
irrWUE_stats_20$.group <- NA



Nrate_summary  <- cbind.data.frame(Yield_stats_20,Yield_stats_21,NUE_stats_20,NUE_stats_21,irrWUE_stats_20,irrWUE_stats_21)

write.csv(Nrate_summary, "Cotton_Nrate_summary.csv")
#######################################################################################################################
# Irrigation None is significant

Yield_stats_20 <- summarySE(Cotton_data_2020, measurevar="Yield", groupvars=c("Irrigation"))
Yield_stats_20 <-left_join(Yield_stats_20[,c(1,3)],Yield_20_sign2[,c(1,7)], by = "Irrigation")
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

write.csv(Irrigation_summary, "Cotton_Irrigation_summary.csv")

##############################################################################################################################
# Interaction Effect


Yield_stats_20 <- summarySE(Cotton_data_2020, measurevar="Yield", groupvars=c("Nrate","Irrigation"))
Yield_stats_20 <-Yield_stats_20[,c(1,2,4)]
Yield_stats_20$.group <- NA

Yield_stats_21 <- summarySE(Cotton_data_2021, measurevar="Yield", groupvars=c("Nrate","Irrigation"))
Yield_stats_21 <-Yield_stats_21[,c(1,2,4)]
Yield_stats_21$.group <- NA

NUE_stats_20 <- summarySE(Cotton_data_2020_NUE, measurevar="NUE", groupvars=c("Nrate","Irrigation"))
NUE_stats_20 <-NUE_stats_20[,c(1,2,4)]
NUE_stats_20$.group <- NA



NUE_stats_21 <- summarySE(Cotton_data_2021, measurevar="NUE", groupvars=c("Nrate","Irrigation"))
NUE_stats_21 <-NUE_stats_21[,c(1,2,4)]
NUE_stats_21$.group <- NA


irrWUE_stats_20 <- summarySE(Cotton_data_2020, measurevar="irrWUE", groupvars=c("Nrate","Irrigation"))
irrWUE_stats_20 <-irrWUE_stats_20[,c(1,2,4)]
irrWUE_stats_20$.group <- NA

irrWUE_stats_21 <- summarySE(Cotton_data_2021, measurevar="irrWUE", groupvars=c("Nrate","Irrigation"))
irrWUE_stats_21 <-irrWUE_stats_21[,c(1,2,4)]
irrWUE_stats_21$.group <- NA

Interaction_summary  <- cbind.data.frame(Yield_stats_20,Yield_stats_21,NUE_stats_20,NUE_stats_21,irrWUE_stats_20,irrWUE_stats_21)

write.csv(Interaction_summary, "Cotton_Interaction_summary.csv")





