#This file contains functions to create a time series of population data. The time series represent
#one population of each species over a period of time specified by the parameter numTimeSteps, 
#which is set to 50 as a default. The first function uses Perretti equations without noise 
#and fishing mortality.The second function uses Perretti equations with those factors added. 
#It has two additional parameters for fishing mortality of both species. The third equation uses the
#Ricker model with noise and fishing mortality added. 



simulatePerretti <- function(x0, y0, rx, ry, Kx, Ky, c, numTimeSteps = 50){
  populations =  data.frame(time = 1:numTimeSteps, x = numeric(numTimeSteps), y = numeric(numTimeSteps))
  for(i in 1:50){
    populations$x[i] = forageFishPerretti(x0, y0, rx, Kx, c)
    populations$y[i] = predatorPerretti(y0, x0, ry, Ky, c)
    
    x0 = populations$x[i]
    y0 = populations$y[i]
  }
  return(populations) #returns a time series with data for both populations in a data frame
}

#fx <- forage fish fishing mortality
#fy <- predator fish fishing mortality 

#From non-fishery data (Bradley CE et al.)
fx <- 0.105
fy <- 0.108

simulatePerretti_noise <- function(x0, y0, rx, ry, Kx, Ky, c, fx, fy, numTimeSteps = 50){
  populations =  data.frame(time = 1:numTimeSteps, x = numeric(numTimeSteps), y = numeric(numTimeSteps))
  for(i in 1:50){
    populations$x[i] = forageFishPerretti_noise(x0, y0, rx, Kx, c, fx)
    populations$y[i] = predatorPerretti_noise(y0, x0, ry, Ky, c, fy)
    
    x0 = populations$x[i]
    y0 = populations$y[i]
  }
  return(populations) #returns time series with data for both populations in a data frame
} 

#testing without noise, graphed in PlottingThings.Rmd
testPopulations1 = simulatePerretti(x0, y0, rx, ry, Kx, Ky, c, 50)

#Ricker

simulateRicker_noise <- function(x0, y0, rx, ry, Kx, Ky, c, fx, fy, numTimeSteps = 50){
  populations =  data.frame(time = 1:numTimeSteps, x = numeric(numTimeSteps), y = numeric(numTimeSteps))
  for(i in 1:50){
    populations$x[i] = forageFishRicker_noise(x0, y0, rx, Kx, c, fx)
    populations$y[i] = predatorRicker_noise(y0, x0, ry, Ky, c, fy)
    
    x0 = populations$x[i]
    y0 = populations$y[i]
  }
  return(populations) #returns time series with data for both populations in a data frame
} 









