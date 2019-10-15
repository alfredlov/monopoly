##---------------------------------------------------------
## Monopoly Simulation
## Version 1.0
##---------------------------------------------------------

#import gameboard as dataset
board <- read.csv("../monopoly_data simple.csv")
currentPlayer <- 0 #aktuell spiller

initGame <- function(){
  N <<- 2 #spillere
  id <- c(1:N) #som vektor til data.frame
  strategy <- c(1, 2) #strategier for spillere, spiller 1 får første verdi som strategi etc..
  startCap <- 2000 #startkapital for spillere, 'Cap' for capital
  fortune <- rep(startCap, times=N)
  roundCap <- 200 #penger for å passere start
  version <- 1 #sette hvilken versjon av spillet å kjøre(?)
  active <- rep(1, times=N)
  players <<- data.frame(id, strategy, fortune, active)
}

