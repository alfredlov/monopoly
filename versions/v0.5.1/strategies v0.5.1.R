##--------------------------------------------------------------------------------##
## Monopoly Simulation - Strategies script                                        ## 
## Version 0.5                                                                    ##
##--------------------------------------------------------------------------------##

source('ai strategies v0.5.1.R')


# function: runStrategy()
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
runStrategy <- function(){                                           # Helper function for handling the purchase of properties. 
  propName <<- as.character(board$name[players$position[cur_player]])
  propPrice <<- board$price[players$position[cur_player]]            # Gets price, type, colors and position of current property.
  propType <<- board$prop[players$position[cur_player]]
  propCol <<- board$color[players$position[cur_player]]
  propPos <<- board$position[players$position[cur_player]]           # Runs the strategy given for each player.
  strategyName <- paste("strategy", players$strategy[cur_player], sep="")
  
  if(get(strategyName)() == TRUE){                                   # If the strategy allows for purchasing property...
    position <- players$position[cur_player]              
    board$owner[position] <<- cur_player                             # Change owner of the property on the board to cur_player.
    gatherStat("street", 1, propName)
    updateBalance(cur_player, "minus", propPrice, "bought street")   # Pays the bank for the property.
    
    if(printGame == TRUE){
      cat(sprintf("Player %s bought property at position %s for %s. \n", cur_player, propPos, propPrice))
      }
    
  }else{                                                             # If the strategy returns FALSE...
    if(bid_Active == TRUE){                                          # ... and is bidding is turned on in settings. 
      bid_over <<- FALSE                                             # ... start bidding round. 
      playersBidDf <- players %>%                                    # ... without cur_player as he aready was able to buy property.
        filter(active == 1 & fortune > propPrice & id != cur_player)
      
      while (bid_over != TRUE) {                                     # While bidding round isn't over...
        interested <- rep(0, times=nrow(playersBidDf))               # Create array of interested buyers
        interestedBuyers <<- data.frame(playersBidDf$id, interested)
        
        if(nrow(playersBidDf) != 0){                                 # For each player, consult their strategies and register if they are interested buyers.           
          for (i in 1:nrow(playersBidDf)){
            strategyName <- paste("strategy", playersBidDf$strategy[playersBidDf$id == playersBidDf$id[i]], sep="")
            interestedBuyers$interested[i] <- get(strategyName)(playersBidDf$id[i])
          }
          
          # If only one person is interested, awards them the property, deduct the bidprice from their fortune and register them as the owner.
          if(length(interestedBuyers$interested[interestedBuyers$interested==TRUE]) == 1){    
            bidWinner <<- interestedBuyers$playersBidDf.id[interestedBuyers$interested==TRUE]
            bid_over <<- TRUE
            position <- players$position[cur_player]
            board$owner[position] <<- bidWinner
            updateBalance(bidWinner, "minus", propPrice, "bought street on auction")
            gatherStat("street", 1, propName)
            if(printGame==TRUE){
              cat(sprintf("Player %s won auction of property at position %s and paid $%s! \n",bidWinner, position, propPrice))
            }
          }
          
          # If auction price has reached three times the original price, award the property at random or end bidding if no player now is interested. 
          if(propPrice > board$price[players$position[cur_player]]*3){
            if(length(interestedBuyers$interested[interestedBuyers$interested > 0]) == 0){
              bid_over <<- TRUE
              if(printGame==TRUE){
                cat(sprintf("All players left the auction as price was too high. \n"))
              }
            }
            
            else{
              bidWinner <<- interestedBuyers[sample(nrow(interestedBuyers), 1),]
              bid_over <<- TRUE
              position <- players$position[cur_player]
              board$owner[position] <<- bidWinner$playersBidDf.id
              updateBalance(bidWinner$playersBidDf.id, "minus", propPrice, "bought street on auction")
              gatherStat("street", 1, propName)
              if(printGame==TRUE){
                cat(sprintf("Player %s won auction of property at position %s by random selection and paid  %s. \n",bidWinner, position, propPrice))
              }
    
            }
          }else{
            propPrice <<- round(propPrice * 1.1)
            playersBidDf <- playersBidDf %>%
              filter(active == 1 & fortune > propPrice & id != cur_player & id %in% interestedBuyers$playersBidDf.id[interestedBuyers$interested != 0])
          }
        }
        
        else{
          bid_over <<- TRUE
          
          if(printGame==TRUE){
            cat(sprintf("No players wanted or were able to participate in the auction. \n"))
          }
        }
      }
    }
  }
}


# function: runHouseStrategy()
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
runHouseStrategy <- function(){                           # Handles running of house strategies.
  housesAvailable <- TRUE
                          
  if(sum(board$houses[!is.na(board$houses)]) > 32){ # Checks to see if there are houses avaliable in bank. 
    housesAvailable <- FALSE
  }
  
  ownsAll <<- c()                                       # ... create list of properties where players owns all of the same color.
  for (i in 1:length(uniqueC)) {
    if(checkStreetPer(uniqueC[i], cur_player) == TRUE){
      ownsAll <<- c(ownsAll, uniqueC[i])
    }
  } 
  
  strategyName <- paste("strategy", players$houseStrategy[cur_player], sep="")
  
  if(length(ownsAll) > 0){                              # If the current player owns all properties of at least 1 color...
    considerBuy <<- TRUE                                # Initially set considering buying houses to TRUE. 
  }else{
    considerBuy <<- FALSE 
  }
  if(housesAvailable == TRUE && considerBuy == TRUE){     # If there are houses avaliable & player is interested
    if(sum(board$houses[!is.na(board$houses)]) > 32){ # Checks to see if there are houses avaliable in bank. 
      housesAvailable <- FALSE
    }
          placesToBuy <<- board %>%                       # ... find candidates for house buying.
            filter(owner == cur_player & color %in% ownsAll & housePrice < players$fortune[cur_player] & houses < 5 & mortaged != 1) %>%
            select(name, color, houses, housePrice)
          
        if(length(placesToBuy$name) == 0){                # If no properties fit the above criterea...
          considerBuy <<- FALSE                           # ... decline to buy houses.
        } else{
          placesToBuy <<- placesToBuy  %>%
            group_by(color) %>%
            filter(houses == min(houses)) %>%
            ungroup()
          
          houseToBuy <- get(strategyName)()               # Run house strategy for the player.
          
          if(houseToBuy != FALSE){                        # If the strategy allowed for buying a house, deduct house price and increment number of houses.
            housePrice <- board$housePrice[board$name == houseToBuy]
            if(housePrice)
            board$houses[board$name == houseToBuy] <<- board$houses[board$name == houseToBuy] + 1 
            players$fortune[cur_player] <<- players$fortune[cur_player] - housePrice
            gatherStat("house", 1, houseToBuy)
            if(printGame==TRUE){
              cat(sprintf("Player %s has bought a house!", cur_player))
            }
          }else{
            considerBuy <<- FALSE
          }
        }
      
  }
}


# function: runMortStrategy(x, y, z)
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
runMortStrategy <- function(x, y, z){                 # Mortgage strategy depends on player x, whether the player wants a 
  if(!missing(x)){                                    # variable y is not relevant for the final implementation.
    stratPlayer <<- x
  }else{
    stratPlayer <<- cur_player
  }
  
  countFreq(stratPlayer)
  
  if(missing(y)){                                     # If no cap-requirement is supplied the player mortgages until he has a fortune above 0. 
     if(sum(streetColFreq) > 0){                      
      if(M1(stratPlayer, "mortage") == FALSE){
        return(FALSE)
      }else{
        gatherStat("pantsatt", 1)                    # Gather AI-data. 
        return(TRUE)
      }
    }else{
      return(FALSE)
    }
  }else if (y == TRUE){
    #SLETT?! spilleren prøver å få z kapital
  }
}


# function: mayLiftMortage()
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
mayLiftMortage <- function(){                           # Function for the buying out of mortgaged properties.
  mortagedProps <- board %>%                            # Only consideres prooperties which the player can afford to unmortgage and are currently mortgaged.
    filter(owner == cur_player & mortaged == 1 & (mortageval*1.1) < players$fortune[cur_player])
  inConcideration <- c()
  
  if(length(mortagedProps$name) > 0){
      strategyName <- paste("strategy", players$strategy[cur_player], sep="")
      if(players$strategy[cur_player] > 100){                # If the AI deals with mortgage, run seperate code...
        inConcideration <- c(inConcideration, get(strategyName)(cur_player, "liftmortagestart"))
        for(i in 1:nrow(mortagedProps)){
          propPrice <<- mortagedProps$mortageval[i]*1.1
          propType <<- mortagedProps$prop[i]
          propCol <<- mortagedProps$color[i]
          propPos <<- mortagedProps$position[i]
          inConcideration <- c(inConcideration, get(strategyName)(cur_player, "liftmortage"))
        }
      }
      
      else{                                              # ... otherwise run "analog" mortgage strategy.
        for(i in 1:nrow(mortagedProps)){
          propPrice <<- mortagedProps$mortageval[i]*1.1
          propType <<- mortagedProps$prop[i]
          propCol <<- mortagedProps$color[i]
          propPos <<- mortagedProps$position[i]
          inConcideration <- c(inConcideration, get(strategyName)(cur_player, "liftmortage"))
        }
      }
    
    if(!is.null(inConcideration)){     
      strategyName <- paste("strategy", players$strategy[cur_player], sep="")
      if(players$strategy[cur_player] > 100){ 
        if(inConcideration[1] >= max(inConcideration)){
          
        }else{
          gatherStat("unpantsatt", 1)
          liftMort <- max(which(inConcideration == max(inConcideration)))
          posOfLiftMort <- mortagedProps$position[liftMort-1]
          board$mortaged[board$position == posOfLiftMort] <<- 0
          updateBalance(cur_player, "minus", board$mortageval[board$position == posOfLiftMort]*1.1, sprintf("lif-mortage of %s", posOfLiftMort))
          
        }
      }else{
        if(TRUE %in% inConcideration){                     # ... unmortgage possible properties.
          gatherStat("unpantsatt", 1)
          liftMort <- max(which(inConcideration == max(inConcideration)))
          posOfLiftMort <- mortagedProps$position[liftMort]
          board$mortaged[board$position == posOfLiftMort] <<- 0
          updateBalance(cur_player, "minus", board$mortageval[board$position == posOfLiftMort]*1.1, sprintf("lif-mortage of %s", posOfLiftMort))
        }
      }
    }
  }
}


# function: setPlayer()
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
setPlayer <- function(x){
  if(!missing(x)){
    stratPlayer <<- x
  }else{
    stratPlayer <<- cur_player
  }
}


#####################################################################################
#                             PROPERTY-STRATEGIES                                   #
#####################################################################################

##-----------------------------------------------------------------------------------
##  Strategy 1: Greedy Naive
##  Simple naïve strategy which involves buying all properties the player lands on. 
##-----------------------------------------------------------------------------------
strategy1 <- function(x, y){
  setPlayer(x)
  return(TRUE)
}

strategy12 <- function(x, y){
  setPlayer(x)
  if(!(propType %in% c(2,3))){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

##-----------------------------------------------------------------------------------
##  Strategy 2: Probabilistic greedy naive
##  Simple strategy of buying all properties the player lands on with probability 0.5.
##-----------------------------------------------------------------------------------
strategy2 <- function(x, y){
  setPlayer(x)
  if(sample(0:1, prob = c(0.5, 0.5), 1) == 1){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

##-----------------------------------------------------------------------------------
##  Strategy 3: Simple conservative
##  Buys all properties as long as price < 50% of total income.
##-----------------------------------------------------------------------------------
strategy3 <- function(x, y){
  setPlayer(x)
  playerLiq <- players$fortune[players$id==stratPlayer] + sum(board$price[board$owner==stratPlayer & board$mortaged == 0 & !is.na(board$owner)]*1/2)
  if(propPrice/playerLiq <= 0.3){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

##-----------------------------------------------------------------------------------
##  Strategy 4: Middle of the road
##  Only buys regular properties on the 2nd and 3rd part of the board (purple, red, orange, yellow properties).
##-----------------------------------------------------------------------------------
strategy4 <- function(x, y){
  if(propCol == 'purple' || propCol == 'orange' || propCol == 'red' || propCol == 'yellow'){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

##-----------------------------------------------------------------------------------
##  Strategy 5: Red & Orange
##-----------------------------------------------------------------------------------
strategy5 <- function(x, y){
  setPlayer(x)
  if(propCol == 'orange' || propCol == 'red'){
    return(TRUE)
  }else{
    return(FALSE)
  }
}


##-----------------------------------------------------------------------------------
##  Strategy 6: Railroads
##-----------------------------------------------------------------------------------
strategy6 <- function(x, y){
  setPlayer(x)
  if(propType == 3){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

##-----------------------------------------------------------------------------------
##  Strategy 7: Utilities
##-----------------------------------------------------------------------------------
strategy7 <- function(x, y){
  setPlayer(x)
  if(propType == 2){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

##-----------------------------------------------------------------------------------
##  Strategy 8: Railroads + Utilities
##-----------------------------------------------------------------------------------
strategy8 <- function(x, y){
  setPlayer(x)
  if(propType == 2 | propType== 3){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

##-----------------------------------------------------------------------------------
##  Strategy 9: Railroads + Utilities, then Buy-all
##-----------------------------------------------------------------------------------
strategy9 <- function(x, y){
  setPlayer(x)
  if(propType == 2 | propType == 3){
    return(TRUE)
  }else{
    if(sum(board$owner[board$prop==2 | board$prop==3] > 0) >=5){
      return(TRUE)
    }
    else{
      return(FALSE)
    }
  }
}


##-----------------------------------------------------------------------------------
##  Strategy 10: Solid
##-----------------------------------------------------------------------------------
strategy10 <- function(x, y){
  setPlayer(x)
  if(!missing(x)){
    stratPlayer <<- x
  }else{
    stratPlayer <<- cur_player
  }
  curFortune <- players$fortune[players$id == stratPlayer]
  currentThrow <- players$throws[players$id == stratPlayer]
  factor <- 1.001
  capitalReq <- 1500
  if((curFortune-propPrice)>=300){
    return(TRUE)
  }
  else if(curFortune-propPrice >= factor^(currentThrow)*capitalReq){

    return(TRUE)
    }
  else{
    return(FALSE)
  }
}

##-----------------------------------------------------------------------------------
##  Strategy 11: Best Practice
##-----------------------------------------------------------------------------------
strategy11 <- function(x, y){
  setPlayer(x)
  curFortune <- players$fortune[players$id == stratPlayer]
  currentThrow <- players$throws[players$id == stratPlayer]
  countFreq(stratPlayer)
  if(curFortune - propPrice < 200){
    return(FALSE)
  }else{
    if(propType %in% c(2,3)){
      return(FALSE)
    } else if(currentThrow < 7){
      return(TRUE)
    } else if(-7 %in% (players$position[players$id != stratPlayer] - propPos)){
      return(TRUE)
    } else if(propCol %in% streetColFreq){
      #print("lololol")
      if(propCol %in% streetColFreqOthers){
        return(FALSE)
      }else{
        return(TRUE)
      }
    } else if(propCol == "orange"){
      return(TRUE)
    } else{
      return(FALSE)
    }
  }
}

#####################################################################################
#                               HOUSE-STRATEGIES                                    #
#####################################################################################

##-----------------------------------------------------------------------------------
##  Strategy H1: Aggressive
##-----------------------------------------------------------------------------------

strategyH1 <- function(){
  setPlayer()
  return(placesToBuy[1,]$name)
}
  

##-----------------------------------------------------------------------------------
##  Strategy H2: Moderate
##-----------------------------------------------------------------------------------

strategyH2 <- function(){
  setPlayer()
  curFortune <- players$fortune[players$id==cur_player]
  if(players$fortune[players$id==cur_player]<1000){
    return(FALSE)
  }
  else{
    pickedHouse <- sample(1:length(placesToBuy[1,]))
    return(placesToBuy[1,]$name)
  }
}


##-----------------------------------------------------------------------------------
##  Strategy H3: Timid
##-----------------------------------------------------------------------------------

strategyH3 <- function(){
  setPlayer()
  return(placesToBuy[length(placesToBuy$name),]$name)
}


#####################################################################################
#                               MORTGAGE-STRATEGIES                                 #
#####################################################################################

M1 <- function(x,y){
  setPlayer(x)
  countFreq(stratPlayer)
  allOfCol <- c()
  for(i in 1:length(uniqueC)){
    total <- length(board$color[board$color == uniqueC[i] & !(is.na(board$owner))])
    allOfCol <- c(allOfCol, total)
  }
  normVec <- streetColFreq/allOfCol
  lowest <- min(normVec[normVec>0]) #over 0 fordi de med 0 inneholder 0 eiendommer...
  
  first <- min(which(normVec == lowest)) #første farge m/ færrest eiendommer
  colOfInd <- uniqueC[first]
  #pantsette eiendom med fargen colOfFirst:
  propsOfCol <- board$position[board$color == colOfInd & board$owner == stratPlayer & !(is.na(board$owner)) & board$mortaged != 1]
  firtStreet <- min(board$position[board$position %in% propsOfCol])
  if(is.na(sum(board$houses[board$position %in% propsOfCol])) | sum(board$houses[board$position %in% propsOfCol]) <= 0){
    #ingen hus -> pantsett
    if((bankMoney - board$mortageval[board$position == firtStreet]) > 0){
      updateBalance(stratPlayer, "pluss", board$mortageval[board$position == firtStreet], "Mortage")
      board$mortaged[board$position == firtStreet] <<- 1
      return(TRUE)
    }else{
      print("The bank can't afford mortgaging the property.")
      return(FALSE)
    }
  }else{ # The player attempts to sell the house 
    whereToSell <- max(board$position[board$position %in% propsOfCol & board$houses > 0])
    if(bankMoney - (board$housePrice[board$position == whereToSell])/2 > 0){        # ... to the bank.
      updateBalance(stratPlayer, "pluss", (board$housePrice[board$position == whereToSell])/2, "sold house")
      board$houses[board$position == whereToSell] <<- board$houses[board$position == whereToSell] - 1
      return(TRUE)                                                                 # If the bank can pay for the house, it's sold...
    }else{                                                                         # the players balance and the bank balance is updated.
      print("The bank can't afford to buy back houses.")                           # Otherwise nothing happens and event is printed.
      return(FALSE)
    }
  }
}

