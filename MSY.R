
singleSpeciesRicker <- function(S, r, K){
  nextS = S*exp(r*(1-(S/K)))
  return(nextS)
}

singleSpeciesMSY <- function(S, r, K){
  dS = S*exp(r*(1-(S/K))) - S
  return(dS)
}


Bmsy <- function(r, K){
  msy = optim(K/2, fn = function(x) -singleSpeciesMSY(x, r, K), method="SANN")$par
}



Ymsy <- function(r, K){
  Bmsy = Bmsy(r, K)
  Ymsy = singleSpeciesRicker(Bmsy, r, K) - Bmsy
  return(Ymsy)
}

Fmsy <- function(r, K){
  Bmsy = Bmsy(r, K)
  Ymsy = Ymsy(r, K)
  Fmsy = Ymsy/singleSpeciesRicker(Bmsy, r, K)
  return(Fmsy)
}


FFmsy = Fmsy(r = 2, K = 1)
Pmsy = Fmsy(r = 3.5/4.5, K = 0.1)


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

# This was me trying to figure out what was happening
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
