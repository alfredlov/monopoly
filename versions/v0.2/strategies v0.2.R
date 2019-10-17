##---------------------------------------------------------
## Monopoly Simulation - Strategies script
## Version 0.2
##---------------------------------------------------------

##--------------------------------------------------------------------------------
## runStrategy: Runs the current player's predefined strategy
##  - If the given player's strategy-function returns TRUE, 
##    set the owner variable of the property to TRUE. 
##--------------------------------------------------------------------------------

runStrategy <- function(){
  strategyName <- paste("strategy", players$strategy[cur_player], sep="")
  if(get(strategyName)() == TRUE){
    
    #SLETT??
    #cat(sprintf("kjøp %s",Sys.time()))
    position <- players$position[cur_player]
    board$owner[position] <<- cur_player
    players$fortune[cur_player] <<- players$fortune[cur_player] - board$price[position]
  }

  ##SLETT??
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
  if(sample(0:1, prob = c(0.5, 0.5), 1) == 1){
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
  

  if(board$price[players$position[cur_player]]/players$fortune[cur_player] <= 0.5){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

##-----------------------------------------------------------------------------------
##  Strategy 4: Middle of the road
##  Only buys regular properties on the 2nd and 3rd part of the board. 
## These are either purple, orange, red, orange...
##-----------------------------------------------------------------------------------
strategy4 <- function(){
  

  
  if(board$color[players$position[cur_player]] == 'purple' || board$color[players$position[cur_player]] == 'orange' || 
     board$color[players$position[cur_player]] == 'red' || board$color[players$position[cur_player]] == 'yellow'){
    return(TRUE)
  }else{
    return(FALSE)
  }
}


