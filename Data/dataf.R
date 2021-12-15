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

# simulateRicker_noise <- function(x0, y0, rx, ry, Kx, Ky, c, fx, fy, numTimeSteps = 50)


ts1 <- simulateRicker_noise(x0 = 0.4, y0 = 0.04, ,ry,Kx,Ky,c,0,0,50)

FmxList <- seq(0,0.4,by=0.01)
FmyList <- seq(0,0.4,by=0.01)

#creating variable saving Ricker trade off data, 200 year series
RickerTrade <- generateTradeOff_Ricker(FmxList,FmyList,x0,y0,rx,ry,Kx,Ky,c,50,50)

#foragefish and predator
#fishing = 0 for both
#one time series



