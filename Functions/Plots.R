#Creates plots of population dynamics. Takes data frame of simulated data from simulateRicker_noise or simulatePerretti_noise and plots populations over time, and populations against each other
plotTests <- function(dataframe) {
  
  #produces a graph of forage fish population over time
  forageFish <- ggplot(dataframe, aes(x = time, y = x)) + 
    geom_line() +
    geom_function(fun = function(x) mean(dataframe$x), color = "blue") +
    theme_bw() + 
    labs(x = "Time Step", y = "Population", title = "Forage Fish-Simulated Time Series")
  
  #produces a graph of predator population over time
  predator <- ggplot(dataframe, aes(x = time, y = y)) + 
    geom_line() +
    geom_function(fun = function(x) mean(dataframe$y), color = "blue") +
    theme_bw() + 
    labs(x = "Time Step", y = "Population", title = "Predator-Simulated Time Series")
  
  #produces a graph of forage fish population vs predator population over time
  both <- ggplot(dataframe, aes(x = x, y = y)) + 
    geom_line() +
    theme_bw() + 
    labs(x = "Forage Fish", y = "Predator", title = "Trade-Off-Relationship Between Populations")
  
  
  return(list(FF = forageFish, P = predator, B = both))
}

#plotTradeOff - takes a data frame as an input (data frame contains fishing mortalities of both forage fish and predator, biomass of forage fish and predator, and yield of forage fish and predator). The function returns three graphs: one showing contours of predator biomass, one showing contours of predator yield, and one showing contours of forage fish yield. 

plotTradeOff <- function(dataframe){
  plot <- ggplot(dataframe, aes(x = FmX, y = FmY, z = yBiomass)) +
    geom_contour_filled() + geom_contour(color="grey")+
    geom_contour(color="black",breaks=0.58)+
    labs(x = "Forage Fish Fishing Mortality", y = "Predator Fishing Mortality", title = "Trade Off Contour Analysis-Predator Biomass")

  
  plot2 <- ggplot(dataframe, aes(x = FmX, y = FmY, z = yYield)) +
    geom_contour_filled() + geom_contour(color="grey")+
    geom_contour(color="black",breaks=0.58)+
    labs(x = "Forage Fish Fishing Mortality", y = "Predator Fishing Mortality", title = "Trade Off Contour Analysis-Predator Yield")
  
  
  plot3 <- ggplot(dataframe, aes(x = FmX, y = FmY, z = xYield)) +
    geom_contour_filled() + geom_contour(color="grey")+
    geom_contour(color="black",breaks=0.58)+
    labs(x = "Forage Fish Fishing Mortality", y = "Predator Fishing Mortality", title = "Trade Off Contour Analysis-Forage Fish Yield")
  
  
  return(list(pBiomass = plot, pYield = plot2, ffYield = plot3))
}

