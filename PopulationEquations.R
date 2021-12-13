# Data generation functions
##Equations from Perretti et al. paper, two species approach
###These are used to provide the next year population size. Further in the project,
###these functions are used in loops to create simulated time series of predator and prey 
###relationships. 


#x0 <- initial forage fish pop size
#y0 <- initial predator fish pop size
#r <- growth rate
#K <- carrying capacity
#c <- coupling strength 

# Forage Fish Equation, no process noise
forageFishEquation <- function(x0, y0, r, K, c){
 
   x1 = (x0*r*(1-(x0/K))) - (c*x0*y0)
  
   return(x1) #returns pop size next year
}

# Predator Fish Equation, no process noise
predatorEquation <- function(y0, x0, r, K, c){
  
  y1 = (y0*r*(1-(y0/K))) + (c*x0*y0)
  
  return(y1) #returns pop size next year
}


#Fishing mortality -- We add fishing mortality by multiplying a parameter f by initial
#population size x0 or y0 and subtracting this value from the population size.

#Process noise -- We add process noise using the rlnorm() function. Rlnorm() provides e to the power
#of a value determined by a normal distribution. We used a mean of 0 and standard deviation of 0.005.
#These values were chosen based on the Perretti et al. paper. 

#Forage Fish Equation, process noise added and fishing mortality 
forageFishEquation_noise <- function(x0, y0, r, K, c, f){
  
  x1 = ((x0*r*(1-(x0/K))) - (c*x0*y0) - (f*x0))*rlnorm(1, meanlog=0, sdlog=0.005)
  
  return(x1) #returns pop size next year
}

#Predator Fish Equation, process noise added and fishing mortality 
predatorEquation_noise <- function(y0, x0, r, K, c, f){
  
  y1 = ((y0*r*(1-(y0/K))) + (c*x0*y0) - (f*x0))*rlnorm(1, meanlog=0, sdlog=0.005)
  
  return(y1) #returns pop size next year
}
