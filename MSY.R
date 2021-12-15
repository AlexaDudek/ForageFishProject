
singleSpeciesRicker <- function(S, r, K){
  dS = S*exp(r*(1-(S/K))) - S
  return(dS)
}


Bmsy <- function(r, K){
  msy = optim(K/2, fn = function(x) -singleSpeciesRicker(x, r, K), method="SANN")$par
  return(msy)
}

Ymsy <- function(r, K){
  Bmsy = Bmsy(r, K)
  Ymsy = singleSpeciesRicker(Bmsy, r, K)
  return(Ymsy)
}

Fmsy <- function(r, K){
  Ymsy = Ymsy(r, K)
  
}



r = 2.0
K = 1.0

testOptim <- OptimSS()