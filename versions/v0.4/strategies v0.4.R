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
  propPrice <<- board$price[players$position[cur_player]]
  ##propPos <<- board$position[players$position[cur_player]]
  strategyName <- paste("strategy", players$strategy[cur_player], sep="")
  if(get(strategyName)() == TRUE){
    
    #SLETT??
    #cat(sprintf("kjøp %s",Sys.time()))
    position <- players$position[cur_player]
    board$owner[position] <<- cur_player
    players$fortune[cur_player] <<- players$fortune[cur_player] - board$price[position]
  }else{
    #####BUDRUNDE
    #BUDRUNDE FUNKER MEN STRATEGIENE ER IKKE SOFISTIKERTE NOK TIL Å HÅNDTERE DET
    #DE BRUKER OFTE MER ENN DE HAR OG TAPER
    #SKRU BUDRUNDER AV/PÅ I initGame()
    if(bid_Active == TRUE){    
      bid_over <- FALSE
      while (bid_over != TRUE) {
        interestedBuyers <<- c()
        for (i in 1:nrow(players)) {
          strategyName <- paste("strategy", players$strategy[i], sep="")
          interestedBuyers[i] <- get(strategyName)()
        }
        if(length(interestedBuyers[interestedBuyers==TRUE]) == 1){
          bidWinner <<- match(TRUE,interestedBuyers)
          bid_over <- TRUE
          position <- players$position[cur_player]
          board$owner[position] <<- bidWinner
          players$fortune[bidWinner] <<- players$fortune[bidWinner] - propPrice
          cat(sprintf("Player %s won auction of %s for %s",bidWinner, position, propPrice))
        }
        if(propPrice > board$price[players$position[cur_player]]*3){
          bidWinner <<- sample(1:length(interestedBuyers[interestedBuyers==TRUE]), 1)
          bid_over <- TRUE
          position <- players$position[cur_player]
          board$owner[position] <<- bidWinner
          players$fortune[bidWinner] <<- players$fortune[bidWinner] - propPrice
  
          cat(sprintf("Player %s won auction of %s on random for %s",bidWinner, position, propPrice))
        }else{
          propPrice <<- propPrice * 1.1
        }
      }
    }
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
  if(propPrice/players$fortune[cur_player] <= 0.5){
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
  
  ##FORENKLE??
  if(board$color[players$position[cur_player]] == 'purple' || board$color[players$position[cur_player]] == 'orange' || 
     board$color[players$position[cur_player]] == 'red' || board$color[players$position[cur_player]] == 'yellow'){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

##-----------------------------------------------------------------------------------
##  Strategy 5: Red & Orange
##-----------------------------------------------------------------------------------
strategy5 <- function(){
  
  ##FORENKLE??
  if(board$color[players$position[cur_player]] == 'orange' || board$color[players$position[cur_player]] == 'red'){
    return(TRUE)
  }else{
    return(FALSE)
  }
}



