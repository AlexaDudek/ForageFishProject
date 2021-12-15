#Data to plot


#generateTradeOff_Ricker 
# a bunch of sims to make contours

#parameters for Ricker model
x0 <- 0.4
y0 <- 0.04
Kx <- 1.0
Ky <- 0.1
rx <- 3.8
ry <- 0.844
c <- 0.1

# simulateRicker_noise <- function(x0, y0, rx, ry, Kx, Ky, cx, cy, fx, fy, numTimeSteps = 50)

ts5 = simulateRicker_noise(x0 = 0.5, y0 = 0.05, rx = 2.0, ry = (3.5/4.5), Kx = 1, Ky = 0.1, cx = 0.2, cy = 0.2, 0, 0, 50)

ts4 = simulateRicker_noise(x0 = 0.5, y0 = 0.05, rx = 2.0, ry = (3.5/4.5), Kx = 1, Ky = 0.1, cx = 0.1, cy = 0.1, 0, 0, 50)

ts3 <- simulateRicker_noise(x0 = 0.5, y0 = 0.05, rx = 2.0, ry = (3.5/4.5), Kx = 1, Ky = 0.1, cx = 0.1, cy = 0.02, 0, 0, 50)

ts2 <- simulateRicker_noise(x0 = 0.5, y0 = 0.05, rx = 2.0, ry = (3.5/4.5), Kx = 1, Ky = 0.1, cx = 0, cy = 0, 0, 0, 50)

ts1 <- simulateRicker_noise(x0 = 0.4, y0 = 0.04, rx = 3.8, ry = 0.844, Kx = 1, Ky = 0.1, cx = 0, cy = 0, 0, 0, 50)




FmxList <- seq(0,0.4,by=0.01)
FmyList <- seq(0,0.4,by=0.01)

#creating variable saving Ricker trade off data, 200 year series

# generateTradeOff_Ricker <- function(FmListX, FmListY, x0, y0, rx, ry, Kx, Ky, 
# cx, cy, numTimeSteps = 50, numMeanYears = 20){

to5 = generateTradeOff_Ricker(FmxList,FmyList, x0 = 0.5, y0 = 0.05, rx = 2.0, ry = (3.5/4.5), Kx = 1, Ky = 0.1, cx = 0.2, cy = 0.2, 50, 50)

to4 = generateTradeOff_Ricker(FmxList,FmyList, x0 = 0.5, y0 = 0.05, rx = 2.0, ry = (3.5/4.5), Kx = 1, Ky = 0.1, cx = 0.1, cy = 0.1, 50, 50)

to3 <- generateTradeOff_Ricker(FmxList,FmyList, x0 = 0.5, y0 = 0.05, rx = 2.0, ry = (3.5/4.5), Kx = 1, Ky = 0.1, cx = 0.1, cy = 0.02, 50, 50)

to2 <- generateTradeOff_Ricker(FmxList,FmyList, x0 = 0.5, y0 = 0.05, rx = 2.0, ry = (3.5/4.5), Kx = 1, Ky = 0.1, cx = 0, cy =0, 50, 50)

#foragefish and predator
#fishing = 0 for both
#one time series



