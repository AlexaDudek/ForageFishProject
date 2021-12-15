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

<<<<<<< HEAD
ts13 = simulateRicker_noise(x0 = 0.5, y0 = 0.05, rx = 2.0, ry = (3.5/4.5), Kx = 1, Ky = 0.1, cx = 1.0, cy = 1.0, 0, 0, 50)

ts12 = simulateRicker_noise(x0 = 0.5, y0 = 0.05, rx = 2.0, ry = (3.5/4.5), Kx = 1, Ky = 0.1, cx = 0.9, cy = 0.9, 0, 0, 50)

ts11 = simulateRicker_noise(x0 = 0.5, y0 = 0.05, rx = 2.0, ry = (3.5/4.5), Kx = 1, Ky = 0.1, cx = 0.8, cy = 0.8, 0, 0, 50)

ts10 = simulateRicker_noise(x0 = 0.5, y0 = 0.05, rx = 2.0, ry = (3.5/4.5), Kx = 1, Ky = 0.1, cx = 0.7, cy = 0.7, 0, 0, 50)

ts9 = simulateRicker_noise(x0 = 0.5, y0 = 0.05, rx = 2.0, ry = (3.5/4.5), Kx = 1, Ky = 0.1, cx = 0.6, cy = 0.6, 0, 0, 50)

ts8 = simulateRicker_noise(x0 = 0.5, y0 = 0.05, rx = 2.0, ry = (3.5/4.5), Kx = 1, Ky = 0.1, cx = 0.5, cy = 0.5, 0, 0, 50)

ts7 = simulateRicker_noise(x0 = 0.5, y0 = 0.05, rx = 2.0, ry = (3.5/4.5), Kx = 1, Ky = 0.1, cx = 0.4, cy = 0.4, 0, 0, 50)

ts6 = simulateRicker_noise(x0 = 0.5, y0 = 0.05, rx = 2.0, ry = (3.5/4.5), Kx = 1, Ky = 0.1, cx = 0.3, cy = 0.3, 0, 0, 50)
=======

>>>>>>> 801786105c9c973b6ce35e50529cdad7f0707ce9

ts5 = simulateRicker_noise(x0 = 0.5, y0 = 0.05, rx = 2.0, ry = (3.5/4.5), Kx = 1, Ky = 0.1, cx = 0.2, cy = 0.2, 0, 0, 50)

ts4 = simulateRicker_noise(x0 = 0.5, y0 = 0.05, rx = 2.0, ry = (3.5/4.5), Kx = 1, Ky = 0.1, cx = 0.1, cy = 0.1, 0, 0, 50)

ts3 <- simulateRicker_noise(x0 = 0.5, y0 = 0.05, rx = 2.0, ry = (3.5/4.5), Kx = 1, Ky = 0.1, cx = 0.0, cy = 0.0, 0, 0, 50)






FmxList <- seq(0,0.4,by=0.01)
FmyList <- seq(0,0.4,by=0.01)

#creating variable saving Ricker trade off data, 200 year series

# generateTradeOff_Ricker <- function(FmListX, FmListY, x0, y0, rx, ry, Kx, Ky, 
# cx, cy, numTimeSteps = 50, numMeanYears = 20){

to13 = generateTradeOff_Ricker(FmxList,FmyList, x0 = 0.5, y0 = 0.05, rx = 2.0, ry = (3.5/4.5), Kx = 1, Ky = 0.1, cx = 1.0, cy = 1.0, 50, 50)

to12 = generateTradeOff_Ricker(FmxList,FmyList, x0 = 0.5, y0 = 0.05, rx = 2.0, ry = (3.5/4.5), Kx = 1, Ky = 0.1, cx = 0.9, cy = 0.9, 50, 50)

to11 = generateTradeOff_Ricker(FmxList,FmyList, x0 = 0.5, y0 = 0.05, rx = 2.0, ry = (3.5/4.5), Kx = 1, Ky = 0.1, cx = 0.8, cy = 0.8, 50, 50)

to10 = generateTradeOff_Ricker(FmxList,FmyList, x0 = 0.5, y0 = 0.05, rx = 2.0, ry = (3.5/4.5), Kx = 1, Ky = 0.1, cx = 0.7, cy = 0.7, 50, 50)

to9 = generateTradeOff_Ricker(FmxList,FmyList, x0 = 0.5, y0 = 0.05, rx = 2.0, ry = (3.5/4.5), Kx = 1, Ky = 0.1, cx = 0.6, cy = 0.6, 50, 50)

to8 = generateTradeOff_Ricker(FmxList,FmyList, x0 = 0.5, y0 = 0.05, rx = 2.0, ry = (3.5/4.5), Kx = 1, Ky = 0.1, cx = 0.5, cy = 0.5, 50, 50)

to7 = generateTradeOff_Ricker(FmxList,FmyList, x0 = 0.5, y0 = 0.05, rx = 2.0, ry = (3.5/4.5), Kx = 1, Ky = 0.1, cx = 0.4, cy = 0.4, 50, 50)

to6 = generateTradeOff_Ricker(FmxList,FmyList, x0 = 0.5, y0 = 0.05, rx = 2.0, ry = (3.5/4.5), Kx = 1, Ky = 0.1, cx = 0.3, cy = 0.3, 50, 50)

to5 = generateTradeOff_Ricker(FmxList,FmyList, x0 = 0.5, y0 = 0.05, rx = 2.0, ry = (3.5/4.5), Kx = 1, Ky = 0.1, cx = 0.2, cy = 0.2, 50, 50)

to4 = generateTradeOff_Ricker(FmxList,FmyList, x0 = 0.5, y0 = 0.05, rx = 2.0, ry = (3.5/4.5), Kx = 1, Ky = 0.1, cx = 0.1, cy = 0.1, 50, 50)

to3 <- generateTradeOff_Ricker(FmxList,FmyList, x0 = 0.5, y0 = 0.05, rx = 2.0, ry = (3.5/4.5), Kx = 1, Ky = 0.1, cx = 0.0, cy = 0.0, 50, 50)



#foragefish and predator
#fishing = 0 for both
#one time series


RickerForage_one <- simulateRicker_noise(x0,y0,rx,ry,Kx,Ky,c,0,0,50)

#generateTradeOff_Perretti
#not used in final analysis but included as a step in our methods--part of the process
#for creating simulation and testing models

# Perretti Two-species model parameters, based on research and assumed ratios
x0 = 0.4
y0 = 0.04
rx = 3.8
ry = 0.844
Kx = 1
Ky = 0.1
c = 0.1

Perretti_sim <- generateTradeOff_Perretti(FmxList,FmyList,x0,y0,rx,ry,Kx,Ky,c)
save("Perretti_sim", file = "./Data/Perretti_tradeoff_data")

