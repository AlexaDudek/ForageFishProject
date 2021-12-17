
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
