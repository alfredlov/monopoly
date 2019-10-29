##--------------------------------------------------------------------------------
## FUNCTIONS v.0.5.R
## Contains the game-functions.
##--------------------------------------------------------------------------------

#Importing of libraries and associated scripts.
source('strategies v0.5.R')
library(dplyr)


# function: throwDice
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
throwDice <- function(){
  dice <- c(sample(1:6, size = 1, replace = TRUE), sample(1:6, size = 1, replace = TRUE))
  return(dice)
}

# function: move()
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
move <- function(x){
  cur_position <- players$position[cur_player]
  if(cur_position + x > nrow(board)){                 # If the player moves past 'Go'...
    y <- nrow(board) - cur_position                   # ... finds remaining tiles until 'Go'.
    players$position[cur_player] <<- x - y            # ... sets position to be x - remaining tiles until 'Go'.
    
    if(bankMoney > roundCap){                         # If the bank is solvent, credit current player.
      updateBalance(cur_player, "pluss", roundCap, "Start")
      if(printGame == TRUE){                          # Print event. 
        cat(sprintf("Player %s moved %s tiles to position %s, and passed Go.\n",cur_player, x, x-y))
      }
      
    }else{
      if(printGame == TRUE){                          # Print event. 
        cat(sprintf("Bank can't pay $%s for passing 'Go'.\n", roundCap))
      }
    }
  }
  
  else{                                               # The following code-snippet deals with the 
    if(players$jailDays[cur_player] == 1){            # number of days the player has to spend in jail.
      players$jailDays[cur_player] <<- players$jailDays[cur_player] - 1
      players$position[cur_player] <<- cur_position + x
      if(printGame == TRUE){                          # Print event. 
        cat(sprintf("Player %s got out after being in jail for three turns. \n",cur_player))
      }
    }
    if(players$jailDays[cur_player] %in% c(2,3)){
      players$jailDays[cur_player] <<- players$jailDays[cur_player] - 1
      if(printGame == TRUE){                          # Print event. 
        cat(sprintf("Player %s is in jail, and must wait another round. \n",cur_player))
      }
    }
    if(players$jailDays[cur_player] == 0){
      players$position[cur_player] <<- cur_position + x
    }
    if(printGame == TRUE){                          # Print event. 
      cat(sprintf("Player %s moved %s tiles to position %s. \n",cur_player, x, cur_position + x))
    }
  }
} 




# function: processPos()
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
processPos <- function(){                             # Hands off player to correct processing function based on where he landed. 
  position <<- players$position[cur_player]
  playerFortune <- players$fortune[cur_player]
  if(board$prop[position] %in% c(1,2,3)){             # If current tile is a buyable property...
      if(board$owner[position] == 0){                 # If no one currently owns the buyable property...
        if(board$price[position] <= playerFortune){   # If price of property is lower than playerFortune...
          runStrategy()                               # Run property strategy.
        }
        
      }else{                                          # ...if someone else owns the propertymove to pay-rent function.
        owner <<- board$owner[position]               # ...pay-rent function depends on whether the property is a normal propoerty, a train or a utility. 
        processNames <- c("Prop", "Util", "Train")
        processName <- paste("process", processNames[board$prop[position]], sep="")
        if(owner != cur_player){
          get(processName)()    
        }
      }
  }
  
  if(board$prop[position] %in% c(0,5,6)){             # If the player currently is at jail, 'Free Parking' or an automatic pay tile...
    processNames <- c("Jail", "Auto", "Free")         # ... hand of player to relevent process-function.
    processName <- paste("process", processNames[match(board$prop[position],c(0,5,6))], sep="")
    get(processName)()
  }
  
  runHouseStrategy()                                  # Runs the house strategy set in 'Settings'. 
} 

# function: processFree
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
processFree <- function(){                            # If the player lands on 'Free Parking'...
                                                      # ... nothing happens.
  if(printGame == TRUE){                            # Print event. 
    cat(sprintf("Player %s landed on 'Free Parking'. \n",cur_player))
  }
}                      


# function: processJail
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
processJail <- function(){                            # If the player lands on 'Go to Jail'...
  if(board$position[position] == 27){
    players$position[cur_player] <<- 9                # ... teleports player to 'Jail'...
    players$jailDays[cur_player] <<- 3                # ... sets days remaining in jail to 3.
    if(printGame == TRUE){                            # Print event. 
      cat(sprintf("Player %s moved to jail. \n",cur_player))
    }
  }
  
  if(board$position[position] == 9){                 # If the player is at jail...
    dice1 <- sample(1:6, size = 1, replace = TRUE)   # Throw two dice...
    dice2 <- sample(1:6, size = 1, replace = TRUE)
    if(dice1 == dice2){                              # If the dice show same result...
      players$jailDays[cur_player] <<- 0             # ... set remaining days in jail to 0.
      move(dice1 + dice2)                            # ... increment player position with result using move().
      if(printGame == TRUE){                         # Print event. 
        cat(sprintf("Player %s got out of jail by throw two dice with same face.\n",cur_player))
      }
    }
  }
}


# function: processAuto: 
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
processAuto <- function(){                            # Processing the player landing on an auto-pay tile (e.g. income tax).
  updateBalance(cur_player, "minus", board$autopay[position], "autopay")
  
  if(printGame==TRUE){                                # Print event.
    cat(sprintf("Player %s landed on an autopay tile and has to pay %s to the bank. \n", cur_player, board$autopay[position]))
  }
}

# function: processUtil: 
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
processUtil <- function(){                            # Processes the player landing on a utility property.
  numberOfUtilities <- nrow(board[board$prop  == "2" & board$owner == owner,])
  utilR <- c(4, 10)                                   # Rent multipliers depending on numberOfUtilities
  dice_res <- sum(throwDice())                        # Throw dice to calculate rent.
  updateBalance(owner, "pluss", dice_res*utilR[numberOfUtilities], "Util")
  updateBalance(cur_player, "minus", dice_res*utilR[numberOfUtilities], "Util")
                  
  if(printGame==TRUE){                                # Print event.
    cat(sprintf("Player %s landed on a utility property owned by %s and paid him %s. \n", cur_player, owner, dice_res*utilR[numberOfUtilities]))
  }
}

## function: processTrain()
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
processTrain <- function(){                           # Processes the player landing on a train property.
  numberOfTrains <- nrow(board[board$prop  == "3" & board$owner == owner,])
  rentToPay <- board$rent[position]*2^(numberOfTrains-1)
  updateBalance(cur_player, "minus", rentToPay, "Train")
  updateBalance(owner, "pluss", rentToPay, "Train")
  
  if(printGame==TRUE){                                # Print event.
    cat(sprintf("Player %s landed on a train owned by %s, and paid him %s. \n", cur_player, owner, rentToPay))
  }
}


# function: processProp()
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
processProp <- function(){                            # Processes the player landing on a regular property.
  color <<- as.character(board$color[position])       # Finds color of the street you landed on. 
  if(board$mortaged[position] == 0){                  # If the property is not mortaged...
    if(checkStreetPer(color, owner) == TRUE){         # ... and the owner owns all the streets of the same color...
      if(board$houses[position] > 0){                 # ... and the person has a number of houses on the property pay accordingly.
        housesFunc <- sprintf("rent%sh", board$houses[position])
        updateBalance(cur_player, "minus", board[[housesFunc]][position], "Husleie")
        updateBalance(owner, "pluss", board[[housesFunc]][position], "Husleie")
      }
      
      else{                                          # If the person owns all props of the same color, but has no houses pay accordingly.
        updateBalance(cur_player, "minus", board$rent[position]*2, "Dobbel Prop")
        updateBalance(owner, "pluss", board$rent[position]*2, "Dobbel Prop")
      }
      
    }
    
    else{                                            # If the property is owned by someone else, but the person isn't entitled to more than normal rent, pay accordingly.
      updateBalance(cur_player, "minus", board$rent[position], "Prop")
      updateBalance(owner, "pluss", board$rent[position], "Prop")
    }
  }
  else{                                             # When the property is mortgaged, pay no rent.
  }
}

# function: checkStreetPer(x, y)
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
checkStreetPer <- function(x, y){                   # Checks to see if player y also owns the other properties of the same color as x.
  NoC <- nrow(board[board$color  == x,])            # Number of properties with that color x.
  NoCo <- nrow(board[board$color  == x & board$owner == y,]) # Number of properties with that color x, owned by y.
  
  if(NoC == NoCo){
    return(TRUE)
  }else{
    return(FALSE)
  }
}


# function: countFreq(x)
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
countFreq <- function(x){                            # Finds the number of houses, and how many properties each player has of each color.
  streetColFreq <<- c()
  streetColFreqOthers <<- c()
  houseColFreq <<- c()
  houseColFreqOthers <<- c()
  
  for (i in 1:length(uniqueC)) {
    NoCo <- nrow(board[board$color  == uniqueC[i] & board$owner == x & !(is.na(board$owner)) & board$mortaged != 1,]) 
    streetColFreq <<- c(streetColFreq, NoCo)
    NoCo2 <- nrow(board[board$color  == uniqueC[i] & board$owner != x & board$owner != 0 & !(is.na(board$owner)) & board$mortaged != 1,]) 
    streetColFreqOthers <<- c(streetColFreqOthers, NoCo2)
    
    sumHouses <- sum(board$houses[(board$owner==x) & !(is.na(board$owner))  & !(is.na(board$houses)) & board$color  == uniqueC[i]])
    houseColFreq <<- c(houseColFreq, sumHouses)
    sumHouses2 <- sum(board$houses[(board$owner!=x) & (board$owner!=0) & !(is.na(board$owner))  & !(is.na(board$houses)) & board$color  == uniqueC[i]])
    houseColFreqOthers <<- c(houseColFreqOthers, sumHouses2)
  }
}

# function: updateBalance(x, y, z, what)
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
updateBalance <- function(x, y, z, what){             # Transfers money z from/to (depending on y) player x.
  if(y == "pluss"){
    players$fortune[x] <<- players$fortune[x] + z
    bankMoney <<- bankMoney - z
  }else{
    players$fortune[x] <<- players$fortune[x] - z
    bankMoney <<- bankMoney + z
  }
  
  if(enableTransLog == TRUE){
    cat(sprintf("\n %s %s to %s for %s", y, z, x, what))
  }
  
  if(printGame==TRUE){                                # Print event.
    if(y=="pluss"){
      cat(sprintf("%s was added to player %s's account from bank. \n", z, x))
    }
    if(y=="minus"){
      cat(sprintf("%s was deducted from player %s's account from bank. \n", z, x))
    }
  }
} 


# function: checkPlayerLoss()
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
checkPlayerLoss <- function(){                       # Checks to see if cur_player has lost.
  #SETTE INN IF MORT = TRUE FØRST...
  #Og an ennen if om at fortune < 0.
  
  #GENERELT DEKOMPONER WHILE SETNING...
  while(length(board$name[(board$owner==cur_player) & !(is.na(board$owner)) & !(is.na(board$houses)) & board$mortaged != 1]) > 0 & players$fortune[cur_player] < 0 & mort_Active == TRUE){
    if(runMortStrategy(cur_player) == FALSE){        # While the player has unmorta???????????
      break
    }
  }
  
  if(players$fortune[cur_player] < 0){                # If player fortune is negative, houses can't be sold and properties can't be mortgaged...
    players$active[cur_player] <<- 0                  # Set current player to be inactive.
    fortune <<- cbind(fortune, players$fortune)       # ?? må dette gjøres her??Add fortune data to
    board2 <<- board
    board <<- board %>%
      mutate(houses=replace(houses, owner==cur_player, 0)) %>%
      mutate(owner=replace(owner, owner==cur_player, 0)) %>%
      as.data.frame()
    
    if(printGame==TRUE){
      cat(sprintf("Player %s ran out of cash! \n", cur_player))
    }
    
    return(TRUE)        #Hvorfor returner den noe???
  }
  else{
    return(FALSE)
  }
}

# function: checkGameOver()
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
checkGameOver <- function(){                            # Checks to see if game is over, which happens if only one player is active.
  if(length(players$active[players$active==TRUE])==1){
    game_over <<- TRUE                                  # Declares game over. 
    roundWinner <<- players$id[players$active==TRUE]    # Finds round winner and his strategy.
    winnerStrategy <<- players$strategy[players$active==TRUE]
    countFreq(1)
    colnames(logForNN4temp) <- c("throws", "fortune", as.character(uniqueC), as.character(paste(uniqueC, "houses", sep = '')), "buyStreet", "buyHouse", "mortage", "liftmortage", "fortuneOthers", as.character(paste(uniqueC, "Others", sep = '')), as.character(paste(uniqueC, "housesOthers", sep = '')), "id")
    logForNN6temp <<- rbind(logForNN6temp, c(players$throws[players$id == 1],players$fortune[players$id == 1],streetColFreq, houseColFreq, sum(logForNN4temp$mortage[logForNN4temp$id == 1]), sum(logForNN4temp$liftmortage[logForNN4temp$id == 1]), sum(logForNN4temp$mortage[logForNN4temp$id != 1]),sum(logForNN4temp$liftmortage[logForNN4temp$id != 1]), sum(players$fortune[players$id != 1]),streetColFreqOthers,houseColFreqOthers, ifelse(roundWinner == 1, 1, 0)))
    print(roundWinner)
    if(printGame==TRUE){                                # Print event.
      cat(sprintf("Player %s won, using strategy %s. \n", roundWinner, winnerStrategy))
    }
  }
} 


# function: setNextPlayer()
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
setNextPlayer <- function(){                            # Changes current player before next turn. 
  if(cur_player == N){                                  # If current player is the last in the list, wrap around...
    cur_player <<- 1
  } else {                                              # ... else increment it by 1.
    cur_player <<- cur_player + 1
  }
  
  if(printGame==TRUE){                                  # Print event.
    cat(sprintf("It's no player %s's turn. \n",cur_player))
  }
}



