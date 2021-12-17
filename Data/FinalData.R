#This file contains simulated data used in final figures and analysis. 

#Fishing mortality lists for all sims
FmxList <- seq(0.0,1.0,by=0.1)
FmyList <- seq(0.0,1.0,by=0.1)

#Perretti two species model data
Perretti_sim <- generateTradeOff_Perretti(FmxList,FmyList,0.4,0.04,3.8,0.844,1,0.1,0.1)


#Ricker two-species model single time series to plot population dynamics
Ricker2_02_1_nofish <- simulateRicker_noise(0.5,0.05,2.0,0.78,1.0,0.1,0.2,0.2,0,0)
Ricker2_02_1_fish <- simulateRicker_noise(0.5,0.05,2.0,0.78,1.0,0.1,0.2,0.2,0.5,0.3)
#Ricker two species model data
Ricker2_02 <- generateTradeOff_Ricker(FmxList,FmyList,0.5,0.05,2.0,0.78,1.0,0.1,0.2,0.2,50,50)