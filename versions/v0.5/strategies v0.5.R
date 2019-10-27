##---------------------------------------------------------
## Monopoly Simulation - Strategies script
## Version 0.2
##---------------------------------------------------------

##--------------------------------------------------------------------------------
## runStrategy: Runs the current player's predefined strategy
##  - If the given player's strategy-function returns TRUE, 
##    set the owner variable of the property to TRUE. 
##--------------------------------------------------------------------------------
source('ai strategies v0.5.R')

gatherStat <- function(x, y){
  if(collectStats == TRUE){
    countFreq(cur_player)
    wola <- cur_player
    if(x == "house"){
      logForNN4temp <<- rbind(logForNN4temp, c(players$throws[cur_player],players$fortune[cur_player],streetColFreq, houseColFreq, 0, y, 0, sum(players$fortune[players$id != cur_player]),streetColFreqOthers,houseColFreqOthers, wola))
    }else if (x == "pantsatt"){
      logForNN4temp <<- rbind(logForNN4temp, c(players$throws[cur_player],players$fortune[cur_player],streetColFreq, houseColFreq, 0, 0, y, sum(players$fortune[players$id != cur_player]),streetColFreqOthers,houseColFreqOthers, wola))
    }else{
      logForNN4temp <<- rbind(logForNN4temp, c(players$throws[cur_player],players$fortune[cur_player],streetColFreq, houseColFreq, y, 0, 0, sum(players$fortune[players$id != cur_player]),streetColFreqOthers,houseColFreqOthers, wola))
    }
    colnames(logForNN4temp) <- length(c("throws", "fortune", as.character(uniqueC), as.character(paste(uniqueC, "houses", sep = '')), "buyStreet", "buyHouse", "fortuneOthers", as.character(paste(uniqueC, "Others", sep = '')), as.character(paste(uniqueC, "housesOthers", sep = '')), "id"))
  }
}

runStrategy <- function(){
  propPrice <<- board$price[players$position[cur_player]]
  propType <<- board$prop[players$position[cur_player]]
  propCol <<- board$color[players$position[cur_player]]
  propPos <<- board$position[players$position[cur_player]]
  
  ##propPos <<- board$position[players$position[cur_player]]
  strategyName <- paste("strategy", players$strategy[cur_player], sep="")
  if(get(strategyName)() == TRUE){
    #SLETT??
    #cat(sprintf("kjøp %s",Sys.time()))
    
    position <- players$position[cur_player]
    board$owner[position] <<- cur_player
    gatherStat("street", 1)
    updateBalance(cur_player, "minus", propPrice, "bought street")
    bankMoney <<- bankMoney + propPrice
  }else{
    gatherStat("street", 0)
    #####BUDRUNDE
    #SKRU BUDRUNDER AV/PÅ I initGame()
    if(bid_Active == TRUE){    
      bid_over <<- FALSE
      playersBidDf <- players %>%
        filter(active == 1 & fortune > propPrice & id != cur_player)
      while (bid_over != TRUE) {
        interested <- rep(0, times=nrow(playersBidDf))
        interestedBuyers <<- data.frame(playersBidDf$id, interested)
        if(nrow(playersBidDf) != 0){
          for (i in 1:nrow(playersBidDf)) {
            strategyName <- paste("strategy", playersBidDf$strategy[playersBidDf$id == playersBidDf$id[i]], sep="")
            #cat(sprintf("\nstrategy %s, player %s",strategyName, playersBidDf$id[i]))
            interestedBuyers$interested[i] <- get(strategyName)(playersBidDf$id[i])
          }
          if(length(interestedBuyers$interested[interestedBuyers$interested==TRUE]) == 1){
            bidWinner <<- interestedBuyers$playersBidDf.id[interestedBuyers$interested==TRUE]
            bid_over <<- TRUE
            position <- players$position[cur_player]
            board$owner[position] <<- bidWinner
            updateBalance(bidWinner, "minus", propPrice, "bought street on auction")
            bankMoney <<- bankMoney + propPrice
            #cat(sprintf("Player %s won auction of %s for %s",bidWinner, position, propPrice))
          }
          if(propPrice > board$price[players$position[cur_player]]*3){
            if(length(interestedBuyers$interested[interestedBuyers$interested > 0]) == 0){
              bid_over <<- TRUE
            }else{
            bidWinner <<- interestedBuyers[sample(nrow(interestedBuyers), 1),]
            bid_over <<- TRUE
            position <- players$position[cur_player]
            board$owner[position] <<- bidWinner$playersBidDf.id
            updateBalance(bidWinner$playersBidDf.id, "minus", propPrice, "bought street on auction")
            bankMoney <<- bankMoney + propPrice
    
            #cat(sprintf("Player %s won auction of %s on random for %s",bidWinner, position, propPrice))
            }
          }else{
            propPrice <<- round(propPrice * 1.1)
            playersBidDf <- playersBidDf %>%
              filter(active == 1 & fortune > propPrice & id != cur_player & id %in% interestedBuyers$playersBidDf.id[interestedBuyers$interested != 0])
          }
        }else{
          bid_over <<- TRUE
        }
      }
    }
    #ikke kjøp
    #print("ikke kjøp")
  }
}

runHouseStrategy <- function(){
  housesAvailable <- TRUE
  if(!is.na(sum(board$houses))){
    if(sum(board$houses) > 32){
      housesAvailable <- FALSE
    }
  }
  if(housesAvailable == TRUE){
  #check if player owns all of a color
  strategyName <- paste("strategy", players$houseStrategy[cur_player], sep="")
  uniqueC <- c(as.character(unique(board$color[board$color != "" & board$color != "white" & board$color != "grey"])))
  ownsAll <<- c() #liste over farger hvor cur_player eier alle, gitt av for løkken nedenfor
  for (i in 1:length(uniqueC)) {
    if(checkStreetPer(uniqueC[i], cur_player) == TRUE){
      ownsAll <<- c(ownsAll, uniqueC[i])
    }
  } 
  
  if(length(ownsAll) > 0){ #hvis en spiller eier alle av en farge/farger
    #print("ALFRED")
    #propPrice <<- board$housePrice[board$position == wTB]
    considerBuy <<- TRUE
    while(considerBuy == TRUE){
        placesToBuy <<- board %>%
          filter(owner == cur_player & color %in% ownsAll & housePrice < players$fortune[cur_player] & houses < 5) %>%
          select(name, color, houses, housePrice)
      if(length(placesToBuy$name) == 0){
        considerBuy <<- FALSE
      }else{
        placesToBuy <<- placesToBuy  %>%
          group_by(color) %>%
          filter(houses == min(houses)) %>%
          ungroup()
        houseToBuy <- get(strategyName)()
        if(houseToBuy != FALSE){ #KJØPER BARE HUS OM TRUE FRA STRATEGI
          board$houses[board$name == houseToBuy] <<- board$houses[board$name == houseToBuy] + 1 
          players$fortune[cur_player] <<- players$fortune[cur_player] - board$housePrice[board$name == houseToBuy]
          gatherStat("house", 1)
          #print("KJØPT HUS")
        }else{
          gatherStat("house", 0)
          considerBuy <<- FALSE
        }
      }
    }
  }
  }
}

runMortStrategy <- function(x, y, z){
  #--------------
  #x = spiller, y = TRUE/FALSE (om spilleren pantsetter for å få z kapital), z = ønsket kapital
  if(!missing(x)){
    stratPlayer <<- x
  }else{
    stratPlayer <<- cur_player
  }
  countFreq(stratPlayer)
  if(missing(y)){
    #her pantsettes det nok kun for å overleve i spillet
    #denne calles som regel fra checkPlayerLoss helt til spilleren eventuelt ikke har tapt
    if(sum(streetColFreq) > 0){
      #strategi M1 pantsetter kun de billigste eiendommene
      if(M1(stratPlayer) == FALSE){
        return(FALSE)
      }else{
        return(TRUE)
      }
    }else{
      return(FALSE)
    }
  }else if (y == TRUE){
    #spilleren prøver å få z kapital
  }
}

M1 <- function(x){
  if(!missing(x)){
    stratPlayer <<- x
  }else{
    stratPlayer <<- cur_player
  }
  #denne strategien prøver å pantsette minst verdifulle eiendommer
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
  if(is.na(sum(board$houses[board$position %in% propsOfCol])) | sum(board$houses[board$position %in% propsOfCol]) == 0){
    #ingen hus -> pantsett
    if((bankMoney - board$mortageval[board$position == firtStreet]) > 0){
      bankMoney <<- bankMoney - board$mortageval[board$position == firtStreet]
      updateBalance(stratPlayer, "pluss", board$mortageval[board$position == firtStreet], "Mortage")
      board$mortaged[board$position == firtStreet] <<- 1
      return(TRUE)
    }else{
      print("banken har ikke råd til pantsetting")
      return(FALSE)
    }
  }else{
    if(bankMoney - (board$housePrice[board$position == firtStreet])/2 > 0){
      bankMoney <<- bankMoney - (board$housePrice[board$position == firtStreet])/2
      updateBalance(stratPlayer, "pluss", (board$housePrice[board$position == firtStreet])/2, "sold house")
      board$houses[board$position == firtStreet] <<- board$houses[board$position == firtStreet] - 1
      return(TRUE)
    }else{
      print("banken har ikke råd til kjøpe hus")
      return(FALSE)
    }
    #selg hus til banken for halve prisen
  }
}

######################################################################################
#####  PROPERTY-STRATEGIES ###########################################################
######################################################################################

##-----------------------------------------------------------------------------------
##  Strategy 1: Greedy Naive
##  Simple naïve strategy which involves buying all properties the player lands on. 
##-----------------------------------------------------------------------------------
strategy1 <- function(x){
  return(TRUE)
}

##-----------------------------------------------------------------------------------
##  Strategy 2: Probabilistic greedy naive
##  Simple strategy of buying all properties the player lands on with probability 0.5.
##-----------------------------------------------------------------------------------
strategy2 <- function(x){
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
strategy3 <- function(x){
  if(!missing(x)){
    stratPlayer <<- x
  }else{
    stratPlayer <<- cur_player
  }
  if(propPrice/players$fortune[stratPlayer] <= 0.5){
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
strategy4 <- function(x){
  
  ##FORENKLE??
  if(propCol == 'purple' || propCol == 'orange' || propCol == 'red' || propCol == 'yellow'){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

##-----------------------------------------------------------------------------------
##  Strategy 5: Red & Orange
##-----------------------------------------------------------------------------------
strategy5 <- function(x){
  
  ##FORENKLE??
  if(propCol == 'orange' || propCol == 'red'){
    return(TRUE)
  }else{
    return(FALSE)
  }
}


##-----------------------------------------------------------------------------------
##  Strategy 6: Railroads
##-----------------------------------------------------------------------------------
strategy6 <- function(x){
  if(propType == 3){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

##-----------------------------------------------------------------------------------
##  Strategy 7: Utilities
##-----------------------------------------------------------------------------------
strategy7 <- function(x){
  if(propType == 2){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

##-----------------------------------------------------------------------------------
##  Strategy 8: Railroads + Utilities
##-----------------------------------------------------------------------------------
strategy8 <- function(x){
  if(propType == 2 | propType== 3){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

##-----------------------------------------------------------------------------------
##  Strategy 9: Railroads + Utilities, then Conservative
##-----------------------------------------------------------------------------------
strategy9 <- function(x){
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
strategy10 <- function(x){
  if(!missing(x)){
    stratPlayer <<- x
  }else{
    stratPlayer <<- cur_player
  }
  curFortune <- players$fortune[players$id == stratPlayer]
  currentThrow <- players$throws[players$id == stratPlayer]
  factor <- 1.001
  capitalReq <- 1500
  if((currentThrow<=5)&&((curFortune-propPrice)>=300)){
    # print("fine1!")
    # print("-----------")
    #
    # print(curFortune)
    # print(propPrice)
  }
  if((curFortune-propPrice)>=300){
    #print("lololol")
    return(TRUE)
  }
  else if(curFortune > 2500){
    #print("fine2!")

    return(TRUE)
  }
  else if(curFortune-propPrice >= factor^(currentThrow)*capitalReq){
    #print("--------")
    #print(factor^(currentThrow)*capitalReq)

    #print("fine3!")

    return(TRUE)
    }

  else{
    return(FALSE)
  }


}
##-----------------------------------------------------------------------------------
##  Strategy 11: Agressive(?)
##-----------------------------------------------------------------------------------

strategy11 <- function(x){
  if(!missing(x)){
    stratPlayer <<- x
  }else{
    stratPlayer <<- cur_player
  }
  curFortune <- players$fortune[players$id == stratPlayer]
  currentThrow <- players$throws[players$id == stratPlayer]
  countFreq(stratPlayer)
  if(curFortune - propPrice < 400){
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


######################################################################################
#####  HOUSE-STRATEGIES #############################################################
######################################################################################

##-----------------------------------------------------------------------------------
##  Strategy H1: Aggressive
##-----------------------------------------------------------------------------------

strategyH1 <- function(){
  return(placesToBuy[1,]$name)
}
  

##-----------------------------------------------------------------------------------
##  Strategy H2: Moderate
##-----------------------------------------------------------------------------------

strategyH2 <- function(){
  curFortune <- players$fortune[players$id==cur_player]
  #cat(sprintf("current player: %s \n",cur_player))
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
  
  
}

##-----------------------------------------------------------------------------------
##  Strategy HALFRED: Timid
##-----------------------------------------------------------------------------------

strategyHALFRED <- function(){
  # if(length(placesToBuy$name) == 1){
  #   return(placesToBuy[1,]$name)
  #   housesInCol <- board$houses[board$color == uniqueC[ownsAll[1]]]
  #   sQuery <- board$position[board$color == uniqueC[ownsAll[1]] & board$houses == min(housesInCol) & board$houses < 5]
  #   if(length(sQuery) != 0){
  #     wTB <- max(sQuery)
  #     
  #   }
  # }else{
  return(placesToBuy[length(placesToBuy$name),]$name)
  #}
 
  
  # if(length(ownsAll) > 1){ #hvis en spiller eier alle av en farge/farger
  #   colFocus <- sample(1:length(ownsAll), 1)
  #   housesInCol <- board$houses[board$color == uniqueC[ownsAll[colFocus]]]
  #   sQuery <- board$position[board$color == uniqueC[ownsAll[colFocus]] & board$houses == min(housesInCol) & board$houses < 5]
  #   if(length(sQuery) != 0){
  #     wTB <- max(sQuery)
  #     if(players$fortune[cur_player] - board$housePrice[board$position == wTB] > 0){
  #       propPrice <<- board$housePrice[board$position == wTB]
  #       strategyName <- paste("strategy", players$houseStrategy[cur_player], sep="")
  #       if(get(strategyName)() == TRUE){
  #         board$houses[board$position == wTB] <<- board$houses[board$position == wTB] + 1 
  #         players$fortune[cur_player] <<- players$fortune[cur_player] - board$housePrice[board$position == wTB]
  #         gatherStat("house", 1)
  #         #print("KJØPT HUS")
  #       }else{
  #         gatherStat("house", 0)
  #       }    
  #     }
  #   }
  # }
}



