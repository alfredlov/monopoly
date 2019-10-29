##--------------------------------------------------------------------------------
## FUNCTIONS v.0.5.R
## Contains the game-functions.
##--------------------------------------------------------------------------------

#Importing of libraries and associated scripts.
source('strategies v0.5.R')
library(dplyr)

##--------------------------------------------------------------------------------
## throwDice: Simulates the throwing of two dice.
## output: A vector of two numbers in range 1-6.
##--------------------------------------------------------------------------------
throwDice <- function(){
  dice <- c(sample(1:6, size = 1, replace = TRUE), sample(1:6, size = 1, replace = TRUE))
  return(dice)
}

##--------------------------------------------------------------------------------
## move: Handles chaning player board position.
## input: x = number given by dice throw
## -  Increments position variable of player by x, and wraps around and credits fortune if 
#     position + x is more than length of board. 
##--------------------------------------------------------------------------------
move <- function(x){
  cur_position <- players$position[cur_player]
  if(cur_position + x > nrow(board)){                 # If the player moves past 'Go'...
    y <- nrow(board) - cur_position                   # ... finds remaining tiles until 'Go'.
    players$position[cur_player] <<- x - y            # ... sets position to be x - remaining tiles until 'Go'.
    
    if(bankMoney > roundCap){                         # If the bank is solvent, credit current player.
      updateBalance(cur_player, "pluss", roundCap, "Start")
      if(printGame == TRUE){                          # Print event. 
        cat(sprintf("Player %s moved %s tiles to position %s, and passed Go.",cur_player, x, x-y))
      }
      
    }else{
      if(printGame == TRUE){                          # Print event. 
        cat(sprintf("Bank can't pay $%s for passing 'Go'.", roundCap))
      }
    }
  }
  
  else{                                               # The following code-snippet deals with the 
    if(players$jailDays[cur_player] == 1){            # number of days the player has to spend in jail.
      players$jailDays[cur_player] <<- players$jailDays[cur_player] - 1
      players$position[cur_player] <<- cur_position + x
      if(printGame == TRUE){                          # Print event. 
        cat(sprintf("Player %s got out after being in jail for three turns.",cur_player))
      }
    }
    if(players$jailDays[cur_player] %in% c(2,3)){
      players$jailDays[cur_player] <<- players$jailDays[cur_player] - 1
      if(printGame == TRUE){                          # Print event. 
        cat(sprintf("Player %s is in jail, and must wait another round.",cur_player))
      }
    }
    if(players$jailDays[cur_player] == 0){
      players$position[cur_player] <<- cur_position + x
    }
    if(printGame == TRUE){                          # Print event. 
      cat(sprintf("Player %s moved %s tiles to position %s.",cur_player, x, cur_position + x))
    }
  }
} 




##--------------------------------------------------------------------------------
## processPos: Handles actions and consequences of moving to a tile.
## -  If the tile is not a property, the player will be subject to an automatic action 
##    (which can be no action at all if the tile is blank).
## -  If the property is owned by someone else the player that landed on the tile has to pay a toll. 
##    A seperate function deals with the specifics of paying. 
## -  If the person cannot afford a free property nothing happens and the turn moves to the next player.
## -  If the player lands on a tile, that is a property, is free and he can afford. 
##--------------------------------------------------------------------------------
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

##--------------------------------------------------------------------------------
## processFree: 
##--------------------------------------------------------------------------------
processFree <- function(){                            # If the player lands on 'Free Parking'...
}                                                     # ... nothing happens.

##--------------------------------------------------------------------------------
## processJail: 
##--------------------------------------------------------------------------------
processJail <- function(){                            # If the player lands on 'Go to Jail'...
  if(board$position[position] == 27){
    players$position[cur_player] <<- 9                # ... teleports player to 'Jail'...
    players$jailDays[cur_player] <<- 3                # ... sets days remaining in jail to 3.
    if(printGame == TRUE){                            # Print event. 
      cat(sprintf("Player %s moved to jail",cur_player))
    }
  }
  
  if(board$position[position] == 9){                 #
    dice1 <- sample(1:6, size = 1, replace = TRUE)
    dice2 <- sample(1:6, size = 1, replace = TRUE)
    if(dice1 == dice2){
      players$jailDays[cur_player] <<- 0
      move(dice1 + dice2)
      if(printGame == TRUE){                            # Print event. 
        cat(sprintf("Player %s KOM UT AV FENGSEL PGA TERNINGKAST",cur_player))
      }
    }
    #cat(sprintf("Player %s KOM ikkkkke UT AV FENGSEL PGA TERNINGKAST",cur_player))
     #teleporter til jail
    #for å holde spilleren i jail trenger vi et element i df for å se om just visiting eller i jail
    #  og for å se hvor mange kast spilleren har forsøkt å komme ut
  }
}

##--------------------------------------------------------------------------------
## processAuto: 
##--------------------------------------------------------------------------------
processAuto <- function(){
  updateBalance(cur_player, "minus", board$autopay[position], "autopay")
                                                        #players$fortune[cur_player] <<- players$fortune[cur_player] - board$autopay[position]

  #cat(sprintf("AUTOPAY, spiller %s betaler %s til %s", cur_player, board$autopay[position], board$name[position]))
}

##--------------------------------------------------------------------------------
## processUtil: 
##--------------------------------------------------------------------------------
processUtil <- function(){
  NoT <- nrow(board[board$prop  == "2" & board$owner == owner,]) #hvor mange tog eieren av dette toget eier
  utilR <- c(4, 10) #multipliers for landing on a util
  dice_res <- sum(throwDice()) #kast terning
  updateBalance(owner, "pluss", dice_res*utilR[NoT], "Util")
  updateBalance(cur_player, "minus", dice_res*utilR[NoT], "Util")
                  # players$fortune[owner] <<- players$fortune[owner] + dice_res*utilR[NoT] #formelen for tog-leie er 25*2^(x-1). D gir rekken 1,2,4,8. drd en ganger 25 m/ for å få leieprisene 25,50,100,200
                  # players$fortune[cur_player] <<- players$fortune[cur_player] - dice_res*utilR[NoT]
  # #cat(sprintf("UTILITY, spiller %s eier %s util, spiller %s kastet %s og betaler %s   ", owner, NoT, cur_player, dice_res, dice_res*utilR[NoT]))
}

##--------------------------------------------------------------------------------
## processTrain: 
##--------------------------------------------------------------------------------
processTrain <- function(){
  NoT <- nrow(board[board$prop  == "3" & board$owner == owner,]) #hvor mange tog eieren av dette toget eier
  updateBalance(owner, "pluss", board$rent[position]*2^(NoT-1), "Train")
  updateBalance(cur_player, "minus", board$rent[position]*2^(NoT-1), "Train")
                      # players$fortune[owner] <<- players$fortune[owner] + board$rent[position]*2^(NoT-1) #formelen for tog-leie er 25*2^(x-1). D gir rekken 1,2,4,8. drd en ganger 25 m/ for å få leieprisene 25,50,100,200
                      # players$fortune[cur_player] <<- players$fortune[cur_player] - board$rent[position]*2^(NoT-1)
  #cat(sprintf("TOOG, spiller %s eier %s tog, spiller %s betaler %s", owner, NoT, cur_player, board$rent[position]*2^(NoT-1)))
}

##--------------------------------------------------------------------------------
## processProp: 
##--------------------------------------------------------------------------------
processProp <- function(){
  #sjekke om den som eier gaten også eier alle i samme farge
  color <<- as.character(board$color[position])
  if(board$mortaged[position] == 0){
    if(checkStreetPer(color, owner) == TRUE){
      #en annen spiller en den som landet her eier alle av denne fargen, dobbel leie
      if(board$houses[position] > 0){
        housesFunc <- sprintf("rent%sh", board$houses[position])
        updateBalance(owner, "pluss", board[[housesFunc]][position], "Husleie")
        updateBalance(cur_player, "minus", board[[housesFunc]][position], "Husleie")
      }else{
        updateBalance(owner, "pluss", board$rent[position]*2, "Dobbel Prop")
        updateBalance(cur_player, "minus", board$rent[position]*2, "Dobbel Prop")
      }
    }else{
      updateBalance(owner, "pluss", board$rent[position], "Prop")
      updateBalance(cur_player, "minus", board$rent[position], "Prop")
    }
  }else{
    #mortaged, no rent
  }
}
#sjekk om alle farger blir eid av én spiller
checkStreetPer <- function(x, y){
  NoC <- nrow(board[board$color  == x,]) #hvor mange gater i den fargen
  NoCo <- nrow(board[board$color  == x & board$owner == y,]) 
  if(NoC == NoCo){
    return(TRUE)
  }else{
    return(FALSE)
  }
}
#sjekk hvor mange av en farge én spiller eier, og hvor mange hus per eiendom
countFreq <- function(x){
  #x = spiller å sjekke for 
  uniqueC <<- c(as.character(unique(board$color[board$color != "" & board$color != "grey"])))
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

updateBalance <- function(x, y, z, what){
  #x - spiller, y - legg til eller trekk fra, z - beløp, what - hva skjedde
  if(y == "pluss"){
    #legg til
    players$fortune[x] <<- players$fortune[x] + z
    bankMoney <<- bankMoney - z
  }else{
    #trekk fra
    players$fortune[x] <<- players$fortune[x] - z
    bankMoney <<- bankMoney + z
  }
  if(enableTransLog == TRUE){
    cat(sprintf("\n %s %s to %s for %s", y, z, x, what))
  }
} 


##--------------------------------------------------------------------------------
## checkPlayerLoss: Checks to see if player has lost by seeing if balance is negative. 
##--------------------------------------------------------------------------------
checkPlayerLoss <- function(){#sjekk hvis cur_player har tapt
  while(length(board$name[(board$owner==cur_player) & !(is.na(board$owner)) & !(is.na(board$houses)) & board$mortaged != 1]) > 0 & players$fortune[cur_player] < 0 & mort_Active == TRUE){
    if(runMortStrategy(cur_player) == FALSE){
      break
    }
  }
  if(players$fortune[cur_player] < 0){
    players$active[cur_player] <<- 0
    fortune <<- cbind(fortune, players$fortune)
    board2 <<- board
    board <<- board %>%
      mutate(houses=replace(houses, owner==cur_player, 0)) %>%
      mutate(owner=replace(owner, owner==cur_player, 0)) %>%
      as.data.frame()
    #cat(sprintf("Player %s ran out of cash!", cur_player))
    return(TRUE)
  }else{
    return(FALSE)
  }
}

##--------------------------------------------------------------------------------
## checkGameOver: Checks to see if game is over, by checking how many players remain in game. 
## -  Sets game_over to true and sets match_winner to be the only remaining player, also records player's strategy. 
##--------------------------------------------------------------------------------
checkGameOver <- function(){
  if(length(players$active[players$active==TRUE])==1){
    game_over <<- TRUE
    roundWinner <<- players$id[players$active==TRUE]
    winnerStrategy <<- players$strategy[players$active==TRUE]
    countFreq(1)
    colnames(logForNN4temp) <- c("throws", "fortune", as.character(uniqueC), as.character(paste(uniqueC, "houses", sep = '')), "buyStreet", "buyHouse", "mortage", "liftmortage", "fortuneOthers", as.character(paste(uniqueC, "Others", sep = '')), as.character(paste(uniqueC, "housesOthers", sep = '')), "id")
    
    logForNN6temp <<- rbind(logForNN6temp, c(players$throws[players$id == 1],players$fortune[players$id == 1],streetColFreq, houseColFreq, sum(logForNN4temp$mortage[logForNN4temp$id == 1]), sum(logForNN4temp$liftmortage[logForNN4temp$id == 1]), sum(logForNN4temp$mortage[logForNN4temp$id != 1]),sum(logForNN4temp$liftmortage[logForNN4temp$id != 1]), sum(players$fortune[players$id != 1]),streetColFreqOthers,houseColFreqOthers, ifelse(roundWinner == 1, 1, 0)))
    print(roundWinner)
    #cat(sprintf("Player %s won, using strategy %s", roundWinner, winnerStrategy))
  }
} 

##--------------------------------------------------------------------------------
## setNextPlayer: Handles player's turn-taking.
## -  Sets cur_player to next player in player-dataframe, unless we're at the last player, 
##    then circle back to player 1.
##--------------------------------------------------------------------------------
setNextPlayer <- function(){
  if(cur_player == N){
    cur_player <<- 1
  } else {
    cur_player <<- cur_player + 1
  }
  #cat(sprintf("Player %s's turn",cur_player))
}
