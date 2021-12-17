# CCM function
library(rEDM)

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