#Data to plot


# https://stackoverflow.com/questions/29812633/cannot-coerce-type-closure-to-vector-of-type-character 


FFbreaks = seq(0, maxXYield, length.out = 9)
PYbreaks = seq(0, maxXYield, length.out = 9)
PBbreaks = seq(0, maxXYield, length.out = 9)

colorsFF <- function(x){
  colors = colorRampPalette(c("mediumpurple4", "khaki"))( 8 )
  colors[1:x]
}

colorsP <- function(x){
  colors = colorRampPalette(c("dodgerblue4", "honeydew"))( 8 )
  colors[1:x]
}

breaklabelFF <- function(x){
  labels = paste0(round(FFbreaks[1:8], 3), "-", round(FFbreaks[2:9],3))
  labels[1:x]
}

breaklabelPB <- function(x){
  labels = paste0(round(PBbreaks[1:8], 3), "-", round(PBbreaks[2:9],3))
  labels[1:x]
}

breaklabelPY <- function(x){
  labels = paste0(round(PYbreaks[1:8], 3), "-", round(PYbreaks[2:9],3))
  labels[1:x] 
}

testPlot_highC <- ggplot(toRicker_highC_data, aes(x = FmX, y = FmY, z = xYield)) +
  geom_contour_filled(breaks = FFbreaks, show.legend = TRUE) + 
  scale_fill_manual(palette=colorsFF, values = breaklabelFF(8), name="Value", drop=FALSE) +
  theme(legend.position = "right") +
  geom_contour(breaks = FFbreaks, color="grey")+
  geom_hline(yintercept = P_SSmsy, color = "red") +
  geom_hline(yintercept = P_2Smsy, linetype = "dashed", color = "red") +
  geom_vline(xintercept = FF_SSmsy, color = "red") +
  geom_vline(xintercept = FF_2Smsy, linetype = "dashed", color = "red") +
  labs(x = "Forage Fish Fishing Mortality", y = "Predator Fishing Mortality", 
       title = "Trade Off Contour Analysis - Predator Yield")
print(testPlot_highC)













testPlot_lowC <- ggplot(toRicker_lowC_data, aes(x = FmX, y = FmY, z = xYield)) +
  geom_contour_filled() + 
  geom_contour(color="grey")+
  geom_hline(yintercept = P_SSmsy, color = "red") +
  geom_hline(yintercept = P_2Smsy, linetype = "dashed", color = "red") +
  geom_vline(xintercept = FF_SSmsy, color = "red") +
  geom_vline(xintercept = FF_2Smsy, linetype = "dashed", color = "red") +
  labs(x = "Forage Fish Fishing Mortality", y = "Predator Fishing Mortality", 
       title = "Trade Off Contour Analysis - Predator Yield")












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

ts13 = simulateRicker_noise(x0 = 0.5, y0 = 0.05, rx = 2.0, ry = (3.5/4.5), Kx = 1, Ky = 0.1, cx = 1.0, cy = 1.0, 0, 0, 50)

ts12 = simulateRicker_noise(x0 = 0.5, y0 = 0.05, rx = 2.0, ry = (3.5/4.5), Kx = 1, Ky = 0.1, cx = 0.9, cy = 0.9, 0, 0, 50)

ts11 = simulateRicker_noise(x0 = 0.5, y0 = 0.05, rx = 2.0, ry = (3.5/4.5), Kx = 1, Ky = 0.1, cx = 0.8, cy = 0.8, 0, 0, 50)

ts10 = simulateRicker_noise(x0 = 0.5, y0 = 0.05, rx = 2.0, ry = (3.5/4.5), Kx = 1, Ky = 0.1, cx = 0.7, cy = 0.7, 0, 0, 50)

ts9 = simulateRicker_noise(x0 = 0.5, y0 = 0.05, rx = 2.0, ry = (3.5/4.5), Kx = 1, Ky = 0.1, cx = 0.6, cy = 0.6, 0, 0, 50)

ts8 = simulateRicker_noise(x0 = 0.5, y0 = 0.05, rx = 2.0, ry = (3.5/4.5), Kx = 1, Ky = 0.1, cx = 0.5, cy = 0.5, 0, 0, 50)

ts7 = simulateRicker_noise(x0 = 0.5, y0 = 0.05, rx = 2.0, ry = (3.5/4.5), Kx = 1, Ky = 0.1, cx = 0.4, cy = 0.4, 0, 0, 50)

ts6 = simulateRicker_noise(x0 = 0.5, y0 = 0.05, rx = 2.0, ry = (3.5/4.5), Kx = 1, Ky = 0.1, cx = 0.3, cy = 0.3, 0, 0, 50)

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

### Commented out UNUSED CODE
# BmsyNextS <- function(r, K){
#   msy = optim(K/2, fn = function(x) -singleSpeciesRicker(x, r, K), method="SANN")$par
#   return(msy) 
# }

# dataframeMSY <- function(S, r, K, numYear){
#   populations =  data.frame(Nt = numeric(numYear), NtNext = numeric(numYear))
#   for(row in 1:numYear){
#     populations$Nt[row] = S
#     nextS = S*exp(r*(1-(S/K))) - change
#     populations$NtNext[row] = nextS
#     S = nextS
#   }
#   return(populations)
# }

### This was me trying to figure out what was happening with MSY
# Everything is in random orders and not well defined
# I am very sorry
# I don't think there's a good way for me to organize my chaotic thinking, however
r = 2.0
K = 1.0

MSYtest = dataframeMSY(0.4, r, K, 50)

# These aren't even calling the right functions cuz I redefined them
BmsyFF = Bmsy(r, K)
YmsyFF = Ymsy(r, K)
FmsyFF = Fmsy(r, K)

print(paste("Bmsy =", BmsyFF))
print(paste("Ymsy =", YmsyFF))
print(paste("Fmsy =", FmsyFF))

# The meat of my relevant-discovery period
what = BmsyDS(r, K)
print(what)
change = singleSpeciesMSY(what, r, K)
print(change)

wtf = dataframeMSY(what, r, K, 50)
wtfNext = singleSpeciesRicker(what, r, K)
print(change/wtfNext)

# Plotting attempts to see what the functions are actually outputting
ggplot() +
  geom_function(fun = function(x) 0, color = "grey40") +
  geom_function(fun = function(x) singleSpeciesRicker(x, r, K)) + 
  xlim(0,2) +
  theme_bw() +
  labs(x = "Nt", y = "Nt+1")

ggplot() +
  geom_function(fun = function(x) 0, color = "grey") +
  geom_function(fun = function(x) singleSpeciesMSY(x, r, K)) + 
  xlim(0,2) +
  theme_bw() +
  labs(x = "Nt", y = "delta N")


# These values actually outputted pretty cool plots 
Fms = seq(0, 1, by = 0.1)
Fms2 = seq(0, 0.5, by = 0.05)
FmSBmsy = seq(0.25, 0.35, 0.01)
FmMmsy = seq(0.45, 0.65, 0.01)
FmMmsyFine = seq(0.54, 0.56, 0.001)

toMSYTest = generateTradeOff_Ricker(Fms, Fms, x0 = 0.5, y0 = 0.05, rx = 2.0, ry = (3.5/4.5), Kx = 1, Ky = 0.1, cx = 0.2, cy = 0.2, 50, 50)

toMSYTest2 = generateTradeOff_Ricker(Fms, Fms, x0 = 0.5, y0 = 0.05, rx = 2.0, ry = (3.5/4.5), Kx = 1, Ky = 0.1, cx = 0.2, cy = 0.04, 50, 50)

toMSYTest3 = generateTradeOff_Ricker(Fms, Fms, x0 = 0.5, y0 = 0.05, rx = 2.0, ry = (3.5/4.5), Kx = 1, Ky = 0.1, cx = 0.1, cy = 0.1, 50, 50)

toMSYTest4 = generateTradeOff_Ricker(Fms2, Fms2, x0 = 0.5, y0 = 0.05, rx = 2.0, ry = (3.5/4.5), Kx = 1, Ky = 0.1, cx = 0.1, cy = 0.1, 50, 50)

toMSYTest5 = generateTradeOff_Ricker(Fms, Fms, x0 = 0.5, y0 = 0.05, rx = 2.0, ry = (3.5/4.5), Kx = 1, Ky = 0.1, cx = 0.2, cy = 0.2, 50, 50)

toMSYTest6 =  generateTradeOff_Ricker(FmSBmsy, FmSBmsy, x0 = 0.5, y0 = 0.05, rx = 2.0, ry = (3.5/4.5), Kx = 1, Ky = 0.1, cx = 0.2, cy = 0.2, 50, 50)

toMSYTest7 = generateTradeOff_Ricker(FmMmsy, FmMmsy, x0 = 0.5, y0 = 0.05, rx = 2.0, ry = (3.5/4.5), Kx = 1, Ky = 0.1, cx = 0.2, cy = 0.2, 50, 50)

toFineMSYM = generateTradeOff_Ricker(FmMmsyFine, FmMmsyFine, x0 = 0.5, y0 = 0.05, rx = 2.0, ry = (3.5/4.5), Kx = 1, Ky = 0.1, cx = 0.2, cy = 0.2, 50, 50)


toMSY_SB_highC1 = generateTradeOff_Ricker(Fms, Fms, x0 = 0.5, y0 = 0.05, rx = 2.0, ry = 0.78, Kx = 1, Ky = 0.1, cx = 0.5, cy = 0.5, 50, 50)
