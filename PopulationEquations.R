# Data generation functions

# Forage Fish Equation
forageFishEquation <- function(x0, y0, r, K, c){
  x1 = (x0*r*(1-(x0/K))) - (c*x0*y0)
  return(x1)
}

# Predator Fish Equation
predatorEquation <- function(y0, x0, r, K, c){
  y1 = (y0*r(1-(y0/K))) + (c*x0*y0)
}
