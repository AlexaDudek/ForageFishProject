
generateTradeOffData <- function(FmListX, FmListY, x0, y0, rx, ry, Kx, Ky, c, numTimeSteps = 50, numMeanYears = 20){
  tradeOffData = expand.grid(FmX = FmListX, FmY = FmListY)
  tradeOffData$xBiomass = NA
  tradeOffData$yBiomass = NA
  
  numRows = nrow(tradeOffData)
  
  for(row in 1:numRows){
    FmX = tradeOffData$FmX[row]
    FmY = tradeOffData$FmY[row]
    
    timeSeries = simulatePopulations(x0, y0, rx, ry, Kx, Ky, c, numTimeSteps)
    
    meanX = timeSeries$x[(numTimeSteps - numMeanYears):numTimeSteps]
    meanY = timeSeries$y[(numTimeSteps - numMeanYears):numTimeSteps]
    
    tradeOffData$xBiomass[row] = meanX
    tradeOffData$yBiomass[row] = meanY
  }
  
  return(tradeOffData)
  
}