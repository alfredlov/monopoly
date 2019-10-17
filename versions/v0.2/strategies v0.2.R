##---------------------------------------------------------
## Monopoly Simulation - Strategies script
## Version 0.2
##---------------------------------------------------------



## runStrategy: Runs the current player's predefined strategy
runStrategy <- function(){
  strategyName <- paste("strategy", players$strategy[cur_player], sep="")
  ##Case: The strategy tells the player to buy the property by returning true.
  ##      Changes the owner variable for the property and deducts the price of 
  ##      the property from the fortune of the current player. 
  ##      Det er mulig å forkorte denne koden!!!!
  if(get(strategyName)() == TRUE){
    #kjøp
    #cat(sprintf("kjøp %s",Sys.time()))
    position <- players$position[cur_player]
    board$owner[position] <<- cur_player
    players$fortune[cur_player] <<- players$fortune[cur_player] - board$price[position]
  }else{
  }
  ##Case: The strategy tells the player not to buy the property. Do nothing!
  ##      Droppe denne else-setningen?? Den gjør vel ingenting. 
  else{
    #ikke kjøp
    #print("ikke kjøp")
  }
}

##-----------------------------------------------------------------------------------
##  Strategy 1: Greedy Naive
##  Simple naïve strategy which involves buying all properties the player lands on. 
##-----------------------------------------------------------------------------------
strategy1 <- function(){
  return(TRUE)
}

##-----------------------------------------------------------------------------------
##  Strategy 2: Probabilistic greedy naive
##  Simple strategy of buying all properties the player lands on with probability 0.5.
##-----------------------------------------------------------------------------------
strategy2 <- function(){
  if(sample(0:1, 1) == 1){
    return(TRUE)
  }else{
    return(FALSE)
  }
}


##-----------------------------------------------------------------------------------
##  Strategy 3: Simple conservative
##  Buys all properties as long as price < 70% of total income.
##-----------------------------------------------------------------------------------
strategy3 <- function(){
  if(sample(0:5, 1) == 1){
    return(TRUE)
  }else{
    return(FALSE)
  }
  #if property price/income <= 70%
  # buy, treturn true, else return false. 
  
}