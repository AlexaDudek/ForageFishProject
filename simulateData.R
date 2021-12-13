

# Peretti Two-species model parameters:
x0 = 0.4
y0 = 0.4
rx = 3.8
ry = 3.7
Kx = 1
Ky = 1
c = 0.1

simulatePopulations <- function(x0, y0, rx, ry, Kx, Ky, c, numTimeSteps = 50){
  populations =  data.frame(time = 1:numTimeSteps, x = numeric(numTimeSteps), y = numeric(numTimeSteps))
  for(i in 1:50){
    populations$x[i] = forageFishEquation(x0, y0, rx, Kx, c)
    populations$y[i] = predatorEquation(y0, x0, ry, Ky, c)
    
    x0 = populations$x[i]
    y0 = populations$y[i]
  }
  return(populations)
}

testPopulations1 = simulatePopulations(x0, y0, rx, ry, Kx, Ky, c, 50)













