#This file contains a function to generate multiple time series of data. Each simulation has a different
#fishing mortality, specified by the FmListX and FmListY parameters. It uses the previous
#simulatePopulations_noise function to create one time series. Data from the end of the time series
#(default set to last 20 years) is averaged for both species and added to a data frame. This
#data is then plotted to show a trade-off relationship between the two species. 


#FmListX <- a list of fishing mortalities to loop over for the forage fish
#FmListY <- a list of fishing mortalities to loop over for the predator fish

generateTradeOffData <- function(FmListX, FmListY, x0, y0, rx, ry, Kx, Ky, c, numTimeSteps = 50, numMeanYears = 20){
  #creating grid for data to be stored in, adding fishing mortalities as columns
  tradeOffData = expand.grid(FmX = FmListX, FmY = FmListY)
  #adding columns for biomass data to be entered in
  tradeOffData$xBiomass = NA
  tradeOffData$yBiomass = NA
  
  numRows = nrow(tradeOffData)
  
  #loop to complete a simulation and add its data to the data frame
  for(row in 1:numRows){
    FmX = tradeOffData$FmX[row]
    FmY = tradeOffData$FmY[row]
    
    timeSeries = simulatePopulations_noise(x0, y0, rx, ry, Kx, Ky, c, FmX, FmY, numTimeSteps)
    
    meanX = mean(timeSeries$x[(numTimeSteps - (numMeanYears-1)):numTimeSteps])
    meanY = mean(timeSeries$y[(numTimeSteps - (numMeanYears-1)):numTimeSteps])
    
    tradeOffData$xBiomass[row] = meanX
    tradeOffData$yBiomass[row] = meanY
  }
  return(tradeOffData) #returning data frame with data from all simulations
}



FmListX = seq(0, 0.2, by = 0.01) 
FmListY = seq(0, 0.2, by = 0.01)

# test based on Perretti et al 2013 parameter values
testTradeOff1 <- generateTradeOffData(FmListX, FmListY, x0 = 0.4, y0 = 0.4, rx = 3.8, ry = 3.7, 
                                      Kx = 1, Ky = 1, c = 0.1)

# 200 time steps, averaged across all
testTradeOff2 <- generateTradeOffData(FmListX, FmListY, x0 = 0.4, y0 = 0.4, rx = 3.8, ry = 3.7, 
                                      Kx = 1, Ky = 1, c = 0.1, numTimeSteps = 200, numMeanYears = 200)




