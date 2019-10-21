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
        playersBidDf <- players %>%
          filter(active == 1 & fortune > propPrice)
        interested <- rep(0, times=nrow(playersBidDf))
        interestedBuyers <<- data.frame(playersBidDf$id, interested)
        if(nrow(playersBidDf) != 0){
          for (i in 1:nrow(playersBidDf)) {
            strategyName <- paste("strategy", playersBidDf$strategy[i], sep="")
            interestedBuyers$interested[i] <- get(strategyName)()
          }
          if(length(interestedBuyers$interested[interestedBuyers$interested==TRUE]) == 1){
            bidWinner <<- interestedBuyers$playersBidDf.id[interestedBuyers$interested==TRUE]
            bid_over <- TRUE
            position <- players$position[cur_player]
            board$owner[position] <<- bidWinner
            players$fortune[bidWinner] <<- players$fortune[bidWinner] - propPrice
            cat(sprintf("Player %s won auction of %s for %s",bidWinner, position, propPrice))
          }
          if(propPrice > board$price[players$position[cur_player]]*3){
            if(length(interestedBuyers$interested[interestedBuyers$interested > 0]) == 0){
              bid_over <- TRUE
            }else{
            bidWinner <<- interestedBuyers[sample(nrow(interestedBuyers), 1),]
            bid_over <- TRUE
            position <- players$position[cur_player]
            board$owner[position] <<- bidWinner$playersBidDf.id
            players$fortune[bidWinner$playersBidDf.id] <<- players$fortune[bidWinner$playersBidDf.id] - propPrice
    
            cat(sprintf("Player %s won auction of %s on random for %s",bidWinner, position, propPrice))
            }
          }else{
            propPrice <<- round(propPrice * 1.1)
          }
        }else{
          bid_over <- TRUE
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


##-----------------------------------------------------------------------------------
##  Strategy 6: Railroads
##-----------------------------------------------------------------------------------
strategy6 <- function(){
  if(board$prop[players$position[cur_player]] == 3){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

##-----------------------------------------------------------------------------------
##  Strategy 7: Utilities
##-----------------------------------------------------------------------------------
strategy7 <- function(){
  if(board$prop[players$position[cur_player]] == 2){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

##-----------------------------------------------------------------------------------
##  Strategy 8: Railroads + Utilities
##-----------------------------------------------------------------------------------
strategy8 <- function(){
  if(board$prop[players$position[cur_player]] == 2 | board$prop[players$position[cur_player]] == 3){
    return(TRUE)
  }else{
    return(FALSE)
  }
}


