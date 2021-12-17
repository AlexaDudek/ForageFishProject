# File with all of data, functions, and calls for creating Figures
library(rEDM)

set.seed(8)
# Perretti parameters
x0_perretti = 0.4
y0_perretti = 0.4
rx_perretti = 3.8
ry_perretti = 3.7
Kx_perretti = 1
Ky_perretti = 1
c_perretti = 0.1


#Fishing mortality lists for all sims
FmxList <- seq(0.0,1.0,by=0.1)
FmyList <- seq(0.0,1.0,by=0.1)

#Perretti two species model data
Perretti_sim <- generateTradeOff_Perretti(FmxList,FmyList,0.4,0.04,3.8,0.844,1,0.1,0.1)

# Trade Off Parameters 
x0 = 0.5
y0 = 0.05
rx = 2.0
ry = 0.78
Kx = 1
Ky = 0.1

# Ricker time series without fishing
tsRicker_noFishing_data = simulateRicker_noise(x0, y0, rx, ry, Kx, Ky, 
                                                cx = 0.2, cy = 0.2, fx = 0, fy = 0, 
                                                numTimeSteps = 50)
tsRicker_noFishing_plots = plotTests(tsRicker_noFishing_data)


# Ricker time series with fishing
tsRicker_Fishing_data = simulateRicker_noise(x0, y0, rx, ry, Kx, Ky,
                                              cx = 0.2, cy = 0.2, fx = 0.4, fy = 0.4,
                                              numTimeSteps = 50)
tsRicker_Fishing_plots = plotTests(tsRicker_Fishing_data)


### Trade Off Analyses 

# Single Species Fmsy
FF_SSmsy = Fmsy(rx, Kx)
P_SSmsy = Fmsy(ry, Ky)

FF_2Smsy = 0.55
P_2Smsy = 0.3

FmList = seq(0, 1, by = 0.05)

# Trade Off c = 0.2
cx_low = 0.2
cy_low = 0.2

toRicker_lowC_data = generateTradeOff_Ricker(FmList, FmList, x0, y0, rx, ry, Kx, Ky,
                                        cx_low, cy_low, numTimeSteps = 50, numMeanYears = 50)

# Trade Off c = 0.5
cx_high = 0.5
cy_high = 0.5 

toRicker_highC_data = generateTradeOff_Ricker(FmList, FmList, x0, y0, rx, ry, Kx, Ky,
                                              cx_high, cy_high, numTimeSteps = 50, numMeanYears = 50)

# Determining consistent plotting breaks for each 'z' variable
maxXYield = max(max(toRicker_highC_data$xYield), max(toRicker_lowC_data$xYield))
maxYYield = max(max(toRicker_highC_data$yYield), max(toRicker_lowC_data$yYield))
maxYBiomass = max(max(toRicker_highC_data$yBiomass), max(toRicker_lowC_data$yBiomass))

FFbreaks = seq(0, maxXYield, length.out = 9)
PYbreaks = seq(0, maxYYield, length.out = 9)
PBbreaks = seq(0, maxYBiomass, length.out = 9)

colorsFF <- function(x){
  colors = colorRampPalette(c("mediumpurple4", "khaki"))( 8 )
  colors[1:x]
}

colorsP <- function(x){
  colors = colorRampPalette(c("dodgerblue4", "honeydew"))( 8 )
  colors[1:x]
}

colorsPY <- function(x){
  colors = colorRampPalette(c("dodgerblue4", "yellow"))( 8 )
  colors[1:x]
}

breaklabelFF <- function(x){
  labels = paste0(round(FFbreaks[1:8], 3), "-", round(FFbreaks[2:9],3))
  labels[1:x]
}

breaklabelPB <- function(x){
  labels = paste0(round(PBbreaks[1:8], 3), "-", round(PBbreaks[2:9],3))
  labels[1:x]
}

breaklabelPY <- function(x){
  labels = paste0(round(PYbreaks[1:8], 3), "-", round(PYbreaks[2:9],3))
  labels[1:x] 
}

to_lowC_FFYield <- ggplot(toRicker_lowC_data, aes(x = FmX, y = FmY, z = xYield)) +
  geom_contour_filled(breaks = FFbreaks, show.legend = TRUE) + 
  scale_fill_manual(palette=colorsFF, values = breaklabelFF(8), name="Forage Fish Yield", drop=FALSE) +
  theme(legend.position = "right") +
  geom_contour(breaks = FFbreaks, color="grey")+
  geom_hline(yintercept = P_SSmsy, color = "red") +
  geom_hline(yintercept = P_2Smsy, linetype = "dashed", color = "red") +
  geom_vline(xintercept = FF_SSmsy, color = "red") +
  geom_vline(xintercept = FF_2Smsy, linetype = "dashed", color = "red") +
  labs(x = "Forage Fish Fishing Mortality", y = "Predator Fishing Mortality", 
       title = paste("Trade Off Contour Analysis - Forage Fish Yield, c =", cx_low))

to_lowC_PBiomass <- ggplot(toRicker_lowC_data, aes(x = FmX, y = FmY, z = yBiomass)) +
  geom_contour_filled(breaks = PBbreaks, show.legend = TRUE) +
  scale_fill_manual(palette=colorsP, values = breaklabelPB(8), name="Predator Biomass", drop=FALSE) +
  theme(legend.position = "right") +
  geom_contour(breaks = PBbreaks, color="grey")+
  geom_hline(yintercept = P_SSmsy, color = "red") +
  geom_hline(yintercept = P_2Smsy, linetype = "dashed", color = "red") +
  geom_vline(xintercept = FF_SSmsy, color = "red") +
  geom_vline(xintercept = FF_2Smsy, linetype = "dashed", color = "red") +
  labs(x = "Forage Fish Fishing Mortality", y = "Predator Fishing Mortality", 
       title = paste("Trade Off Contour Analysis - Predator Biomass, c =", cx_low))

to_lowC_PYield <- ggplot(toRicker_lowC_data, aes(x = FmX, y = FmY, z = yYield)) +
  geom_contour_filled(breaks = PYbreaks, show.legend = TRUE) +
  scale_fill_manual(palette=colorsPY, values = breaklabelPY(8), name="Predator Yield", drop=FALSE) +
  theme(legend.position = "right") +
  geom_contour(breaks = PYbreaks, color="grey")+
  geom_hline(yintercept = P_SSmsy, color = "red") +
  geom_hline(yintercept = P_2Smsy, linetype = "dashed", color = "red") +
  geom_vline(xintercept = FF_SSmsy, color = "red") +
  geom_vline(xintercept = FF_2Smsy, linetype = "dashed", color = "red") +
  labs(x = "Forage Fish Fishing Mortality", y = "Predator Fishing Mortality", 
       title = paste("Trade Off Contour Analysis - Predator Yield, c =", cx_low))




to_highC_FFYield <- ggplot(toRicker_highC_data, aes(x = FmX, y = FmY, z = xYield)) +
  geom_contour_filled(breaks = FFbreaks, show.legend = TRUE) +
  scale_fill_manual(palette=colorsFF, values = breaklabelFF(8), name="Forage Fish Yield", drop=FALSE) +
  theme(legend.position = "right") +
  geom_contour(breaks = FFbreaks, color="grey")+
  geom_hline(yintercept = P_SSmsy, color = "red") +
  geom_hline(yintercept = P_2Smsy, linetype = "dashed", color = "red") +
  geom_vline(xintercept = FF_SSmsy, color = "red") +
  geom_vline(xintercept = FF_2Smsy, linetype = "dashed", color = "red") +
  labs(x = "Forage Fish Fishing Mortality", y = "Predator Fishing Mortality", 
       title = paste("Trade Off Contour Analysis - Forage Fish Yield, c =", cx_high))

to_highC_PBiomass <- ggplot(toRicker_highC_data, aes(x = FmX, y = FmY, z = yBiomass)) +
  geom_contour_filled(breaks = PBbreaks, show.legend = TRUE) + 
  scale_fill_manual(palette=colorsP, values = breaklabelPB(8), name="Predator Biomass", drop=FALSE) +
  theme(legend.position = "right") +
  geom_contour(breaks = PBbreaks, color="grey")+
  geom_hline(yintercept = P_SSmsy, color = "red") +
  geom_hline(yintercept = P_2Smsy, linetype = "dashed", color = "red") +
  geom_vline(xintercept = FF_SSmsy, color = "red") +
  geom_vline(xintercept = FF_2Smsy, linetype = "dashed", color = "red") +
  labs(x = "Forage Fish Fishing Mortality", y = "Predator Fishing Mortality", 
       title = paste("Trade Off Contour Analysis - Predator Yield, c =", cx_high))

to_highC_PYield <- ggplot(toRicker_highC_data, aes(x = FmX, y = FmY, z = yYield)) +
  geom_contour_filled(breaks = PYbreaks, show.legend = TRUE) +
  scale_fill_manual(palette=colorsPY, values = breaklabelPY(8), name="Predator Yield", drop=FALSE) +
  theme(legend.position = "right") +
  geom_contour(breaks = PYbreaks, color="grey")+
  geom_hline(yintercept = P_SSmsy, color = "red") +
  geom_hline(yintercept = P_2Smsy, linetype = "dashed", color = "red") +
  geom_vline(xintercept = FF_SSmsy, color = "red") +
  geom_vline(xintercept = FF_2Smsy, linetype = "dashed", color = "red") +
  labs(x = "Forage Fish Fishing Mortality", y = "Predator Fishing Mortality", 
       title = paste("Trade Off Contour Analysis - Predator Yield, c =", cx_high))





### CCM 

# Nonlinear CCM Parameters
x0_nl = 1
y0_nl = 0.05
rx_nl = 3
ry_nl = 3
Kx_nl = 1
Ky_nl = 0.1
cx_nl = 0.01
cy_nl = 0.2
fx_nl = 0.1
fy_nl = 0.1
numTimeSteps_nl = 500


# Ricker time series without fishing
tsRicker_nl_data = simulateRicker_noise_nl(x0_nl, y0_nl, rx_nl, ry_nl, Kx_nl, Ky_nl, 
                                               cx_nl, cy_nl, fx_nl, fy_nl, 
                                               numTimeSteps_nl)
tsRicker_nl_plots = plotTests(tsRicker_nl_data)

# Trade Off with Chaotic Data
to_nl = generateTradeOff_Ricker_nl(FmList, FmList, x0_nl, y0_nl, rx_nl, ry_nl, Kx_nl, Ky_nl, 
                                       cx_nl, cy_nl, 200, numMeanYears = 100)

to_nl_plot <-  ggplot(to_nl, aes(x = FmX, y = FmY, z = yYield)) +
  geom_contour_filled() + 
  geom_contour(color="grey")+
  labs(x = "Forage Fish Fishing Mortality", y = "Predator Fishing Mortality", 
       title = "Trade Off Contour Analysis - Predator Yield with Non-Linear Dynamics")


CCMdata = tsRicker_nl_data[-c(1:100),]

Edata <- EmbedDimension(dataFrame = CCMdata, Tp = 0, columns = "yYield", target = "xYield", lib="1 250",pred="1 250", showPlot = FALSE)
E = Edata$E[Edata$rho == max(Edata$rho)]
CCM_output = CCM(dataFrame = CCMdata, E = E, Tp = 0, columns = "yYield", target = "xYield", 
                 libSizes = "397 397 1", sample = 100, showPlot = TRUE)
CCM = CCM_output$`yYield:xYield`

cList = seq(0, 1, 0.1)
cVSccm = data.frame(c = numeric(length(cList)), predictability = numeric(length(cList)))

for(i in 1:length(cList)){
  c = cList[i]
  cVSccm$c[i] = c
  CCMdata = simulateRicker_noise_nl(x0 = 1.0, y0 = 0.05, rx = 3.0, ry = 3, Kx = 1, Ky = 0.1, cx = 0.01, cy = c, fx=0.1, fy=0.1, 500)
  CCMdata <- CCMdata[-c(1:100),]
  Edata <- EmbedDimension(dataFrame = CCMdata, Tp = 0, columns = "yYield", target = "xYield", lib="1 250",pred="1 250", showPlot = FALSE)
  E = Edata$E[Edata$rho == max(Edata$rho)]
  CCM_output = CCM(dataFrame = CCMdata, E = E, Tp = 0, columns = "yYield", target = "xYield", 
                   libSizes = "390 390 1", sample = 100, showPlot = FALSE)
  CCM = CCM_output$`yYield:xYield`
  cVSccm$predictability[i] = CCM
}

cVSccm_no0 = cVSccm[2:11,]

ggplot(data = cVSccm, aes(x = c, y = predictability)) +
  geom_point(size = 2) +
  geom_smooth(data =cVSccm_no0, aes(x = c, y = predictability), method="lm", formula = y~log(x), fill="blue", fullrange=TRUE) +
  theme_bw() + 
  theme(plot.title = element_text(face="bold")) +
  labs(x = "Coupling Factor", y = "Predictability", 
       title = "Linear Model Coupling Factor vs \n Non-Parametric Convergent Cross Mapping Predictability")
  

  














