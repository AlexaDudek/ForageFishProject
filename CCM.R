# CCM function

forageFishRicker_noise_nl <- function(x0, y0, r, K, c, f){
  
  #population without fishing
  x1 = ((x0*exp(r*(1-(x0/K)))) - (c*x0*y0))*rlnorm(1, meanlog=0, sdlog=0.1)
  #population after fishing
  x1f = x1 - (f*x1)
  
  #if population becomes negative, set it to 0
  if(x1f < 0){
    x1f <- 0
  }
  
  return(x1f) #returns population next year after fishing
}


predatorRicker_noise_nl <- function(y0, x0, r, K, c, f){
  
  #population without fishing
  y1 = ((y0*exp(r*(1-(y0/K)))) + ((c*x0*y0)))*rlnorm(1, meanlog=0, sdlog=0.1)
  #population after fishing
  y1f = y1 - (f*y1)
  
  return(y1f) #returns population next year after fishing
}


simulateRicker_noise_nl <- function(x0, y0, rx, ry, Kx, Ky, cx, cy, fx, fy, numTimeSteps = 50){
  populations =  data.frame(time = 1:numTimeSteps, x = numeric(numTimeSteps), y = numeric(numTimeSteps), 
                            xYield = numeric(numTimeSteps), yYield = numeric(numTimeSteps))
  
  for(i in 1:numTimeSteps){
    
    fx_i <- mean(rbinom(1000,1,fx))
    fy_i <- mean(rbinom(1000,1,fy))
    
    populations$x[i] = forageFishRicker_noise_nl(x0, y0, rx, Kx, cx, fx_i)
    populations$y[i] = predatorRicker_noise_nl(y0, x0, ry, Ky, cy, fy_i)
    
    x0 = populations$x[i]
    y0 = populations$y[i]
    
    populations$xYield[i] = fx_i * x0
    populations$yYield[i] = fy_i * y0
  }
  return(populations) #returns time series with data for both populations in a data frame
}

CCMdata = simulateRicker_noise_nl(x0 = 1.0, y0 = 0.05, rx = 3.0, ry = 3, Kx = 1, Ky = 0.1, cx = 0.01, cy = 0.2, fx=0.1, fy=0.1, 500)
CCMdata <- CCMdata[-c(1:100),]

Edata <- EmbedDimension(dataFrame = CCMdata, Tp = 0, columns = "yYield", target = "xYield", lib="1 250",pred="1 250", showPlot = FALSE)
E = Edata$E[Edata$rho == max(Edata$rho)]
CCM_output = CCM(dataFrame = CCMdata, E = E, Tp = 0, columns = "yYield", target = "xYield", 
                 libSizes = "397 397 1", sample = 100, showPlot = TRUE)
CCM = CCM_output$`yYield:xYield`

cList = seq(0, 1, 0.1)
cVSccm = data.frame(c = numeric(length(cList)), predictability = numeric(length(cList)))

for(i in 1:length(cList)){
  c = cList[i]
  print(c)
  cVSccm$c[i] = c
  CCMdata = simulateRicker_noise_nl(x0 = 1.0, y0 = 0.05, rx = 3.0, ry = 3, Kx = 1, Ky = 0.1, cx = 0.01, cy = c, fx=0.1, fy=0.1, 500)
  CCMdata <- CCMdata[-c(1:100),]
  Edata <- EmbedDimension(dataFrame = CCMdata, Tp = 0, columns = "yYield", target = "xYield", lib="1 250",pred="1 250", showPlot = FALSE)
  E = Edata$E[Edata$rho == max(Edata$rho)]
  CCM_output = CCM(dataFrame = CCMdata, E = E, Tp = 0, columns = "yYield", target = "xYield", 
                   libSizes = "390 390 1", sample = 100, showPlot = TRUE)
  CCM = CCM_output$`yYield:xYield`
  cVSccm$predictability[i] = CCM
}

ggplot(data = cVSccm, aes(x = c, y = predictability)) +
  geom_point()
