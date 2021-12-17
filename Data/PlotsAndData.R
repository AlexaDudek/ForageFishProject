


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
# cList = c(0.1)
cVSccm = data.frame(c = numeric(length(cList)), predictability = numeric(length(cList)))

for(i in 1:length(cList)){
  c = cList[i]
  cVSccm$c[i] = c
  CCMdata = simulateRicker_noise_nl(x0 = 1.0, y0 = 0.05, rx = 3.0, ry = 3, Kx = 1, Ky = 0.1, cx = 0.01, cy = c, fx=0.1, fy=0.1, 500)
  CCMdata <- CCMdata[-c(1:100),]
  Edata <- EmbedDimension(dataFrame = CCMdata, Tp = 0, columns = "yYield", target = "xYield", lib="1 250",pred="1 250", showPlot = FALSE)
  E = Edata$E[Edata$rho == max(Edata$rho)]
  CCM_output = CCM(dataFrame = CCMdata, E = E, Tp = 0, columns = "yYield", target = "xYield", 
                   libSizes = "390 390 1", sample = 1, showPlot = FALSE)
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
  

  














