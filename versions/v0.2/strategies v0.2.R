##---------------------------------------------------------
## Monopoly Simulation - Strategies script
## Version 0.2
##---------------------------------------------------------

## runStrategy: Runs the current player's predefined strategy
runStrategy <- function(){
  strategyName <- paste("strategy", players$strategy[cur_player], sep="")
  if(get(strategyName)() == TRUE){
    #cat(sprintf("kjøp %s",Sys.time()))
    position <- players$position[cur_player]
    board$owner[position] <<- cur_player
    players$fortune[cur_player] <<- players$fortune[cur_player] - board$price[position]
  }
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
  #if property price/income <= 70%
  # buy, treturn true, else return false. 
  
}