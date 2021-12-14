
generateTradeOffData <- function(FmListX, FmListY, x0, y0, rx, ry, Kx, Ky, c, numTimeSteps = 50, numMeanYears = 20){
  tradeOffData = expand.grid(FmX = FmListX, FmY = FmListY)
  tradeOffData$xBiomass = NA
  tradeOffData$yBiomass = NA
  
  numRows = nrow(tradeOffData)
  
  for(row in 1:numRows){
    FmX = tradeOffData$FmX[row]
    FmY = tradeOffData$FmY[row]
    
    timeSeries = simulatePopulations_noise(x0, y0, rx, ry, Kx, Ky, c, FmX, FmY)
    
    meanX = mean(timeSeries$x[(numTimeSteps - (numMeanYears-1)):numTimeSteps])
    meanY = mean(timeSeries$y[(numTimeSteps - (numMeanYears-1)):numTimeSteps])
    
    tradeOffData$xBiomass[row] = meanX
    tradeOffData$yBiomass[row] = meanY
  }
  return(tradeOffData)
}



FmListX = seq(0, 0.2, by = 0.01) 
FmListY = seq(0, 0.2, by = 0.01)

# test based on Perretti et al 2013 parameter values
testTradeOff1 <- generateTradeOffData(FmListX, FmListY, x0 = 0.4, y0 = 0.4, rx = 3.8, ry = 3.7, 
                                      Kx = 1, Ky = 1, c = 0.1)

# 200 time steps, averaged across all
testTradeOff2 <- generateTradeOffData(FmListX, FmListY, x0 = 0.4, y0 = 0.4, rx = 3.8, ry = 3.7, 
                                      Kx = 1, Ky = 1, c = 0.1, numTimeSteps = 200, numMeanYears = 200)




