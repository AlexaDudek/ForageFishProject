#This file contains functions to generate multiple time series of data. Each simulation has a different
#fishing mortality, specified by the FmListX and FmListY parameters. These functions use the previous
#simulatePerretti_noise  and simulateRicker_noise functions to create one time series. Data from a specified portion 
#of the time series (default set to last 20 years) is averaged for both species and added to a data frame. Yield
#is also calculated for each series' averages. This data is later plotted to show a trade-off relationship between 
#the two species.


#FmListX <- a list of fishing mortality to loop over for the forage fish
#FmListY <- a list of fishing mortality to loop over for the predator fish


#Perretti
generateTradeOff_Perretti <- function(FmListX, FmListY, x0, y0, rx, ry, Kx, Ky, c, numTimeSteps = 50, numMeanYears = 20){
  #creating grid for data to be stored in, adding fishing mortalities as columns
  tradeOffData = expand.grid(FmX = FmListX, FmY = FmListY)
  #adding columns for biomass data
  tradeOffData$xBiomass = NA
  tradeOffData$yBiomass = NA
  #adding columns for yield data
  tradeOffData$yYield = NA
  tradeOffData$xYield = NA
  
  numRows = nrow(tradeOffData)
  
  #loop to complete a simulation and add its data to the data frame
  for(row in 1:numRows){
    FmX = tradeOffData$FmX[row]
    FmY = tradeOffData$FmY[row]
    
    timeSeries = simulatePerretti_noise(x0, y0, rx, ry, Kx, Ky, c, FmX, FmY, numTimeSteps)
    
    meanX = mean(timeSeries$x[(numTimeSteps - (numMeanYears-1)):numTimeSteps])
    meanY = mean(timeSeries$y[(numTimeSteps - (numMeanYears-1)):numTimeSteps])
    
    tradeOffData$xBiomass[row] = meanX
    tradeOffData$yBiomass[row] = meanY
    tradeOffData$yYield[row] = meanY*FmY
    tradeOffData$xYield[row] = meanX*FmX
  }
  return(tradeOffData) #returning data frame with data from all simulations
}


#fishing mortality lists to loop over
FmListX = seq(0, 0.2, by = 0.01) 
FmListY = seq(0, 0.2, by = 0.01)

# test based on Perretti et al 2013 parameter values
testTradeOff1 <- generateTradeOff_Perretti(FmListX, FmListY, x0 = 0.4, y0 = 0.4, rx = 3.8, ry = 3.7, 
                                      Kx = 1, Ky = 1, c = 0.1)

# 200 time steps, averaged across all
testTradeOff2 <- generateTradeOff_Perretti(FmListX, FmListY, x0 = 0.4, y0 = 0.4, rx = 3.8, ry = 3.7, 
                                      Kx = 1, Ky = 1, c = 0.1, numTimeSteps = 200, numMeanYears = 200)


#Ricker

generateTradeOff_Ricker <- function(FmListX, FmListY, x0, y0, rx, ry, Kx, Ky, cx, cy, numTimeSteps = 50, numMeanYears = 20){
  #creating grid for data to be stored in, adding fishing mortalities as columns
  tradeOff = expand.grid(FmX = FmListX, FmY = FmListY)
  #adding columns for biomass data
  tradeOff$xBiomass = NA
  tradeOff$yBiomass = NA
  #adding columns for yield data
  tradeOff$yYield = NA
  tradeOff$xYield = NA
  
  numRows = nrow(tradeOff)
  
  #loop to complete a simulation
  for(row in 1:numRows){
    FmX = tradeOff$FmX[row]
    FmY = tradeOff$FmY[row]
    
    timeSeries = simulateRicker_noise(x0, y0, rx, ry, Kx, Ky, cx, cy, FmX, FmY, numTimeSteps)
    
    #finding averages of one simulation, for entire time series: enter the same number for 
    #numTimeSteps and numMeanYears parameters
    meanX = mean(timeSeries$x[(numTimeSteps - (numMeanYears-1)):numTimeSteps])
    meanY = mean(timeSeries$y[(numTimeSteps - (numMeanYears-1)):numTimeSteps])
    
    #adding data from this simulation to data frame
    tradeOff$xBiomass[row] = meanX
    tradeOff$yBiomass[row] = meanY
    tradeOff$yYield[row] = meanY*FmY
    tradeOff$xYield[row] = meanX*FmX
  }
  return(tradeOff) #returning data frame with data from all simulations
}

generateTradeOff_Ricker_nl <- function(FmListX, FmListY, x0, y0, rx, ry, Kx, Ky, cx, cy, numTimeSteps = 50, numMeanYears = 20){
  #creating grid for data to be stored in, adding fishing mortalities as columns
  tradeOff = expand.grid(FmX = FmListX, FmY = FmListY)
  #adding columns for biomass data
  tradeOff$xBiomass = NA
  tradeOff$yBiomass = NA
  #adding columns for yield data
  tradeOff$yYield = NA
  tradeOff$xYield = NA
  
  numRows = nrow(tradeOff)
  
  #loop to complete a simulation
  for(row in 1:numRows){
    FmX = tradeOff$FmX[row]
    FmY = tradeOff$FmY[row]
    
    timeSeries = simulateRicker_noise_nl(x0, y0, rx, ry, Kx, Ky, cx, cy, FmX, FmY, numTimeSteps)
    
    #finding averages of one simulation, for entire time series: enter the same number for 
    #numTimeSteps and numMeanYears parameters
    meanX = mean(timeSeries$x[(numTimeSteps - (numMeanYears-1)):numTimeSteps])
    meanY = mean(timeSeries$y[(numTimeSteps - (numMeanYears-1)):numTimeSteps])
    meanXyield = mean(timeSeries$xYield[(numTimeSteps - (numMeanYears-1)):numTimeSteps])
    meanYyield = mean(timeSeries$yYield[(numTimeSteps - (numMeanYears-1)):numTimeSteps])
    
    
    #adding data from this simulation to data frame
    tradeOff$xBiomass[row] = meanX
    tradeOff$yBiomass[row] = meanY
    tradeOff$yYield[row] = meanYyield
    tradeOff$xYield[row] = meanXyield
  }
  return(tradeOff) #returning data frame with data from all simulations
}



