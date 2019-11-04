##---------------------------------------------------------
## Monopoly Simulation - Main script
## Version 0.5
##---------------------------------------------------------

#Importing of libraries and associated scripts.
library(ggplot2)
library(reshape2)


initGame <- function(i){
  #------------------------------  Settings  ------------------------------ 
  version <- 5                              # Sets game version.
  setwd("../v0.5")                          # Set working directory to correct version number
  strategy <- c(i, 107)                       # Set player strategies, first parameter sets strategy for player 1, etc...
  houseStrategy <- c(sample(c("H1", "H2"), 1), "H107")            # Set player house-buying strategies

  mortageStrategy <- c("M1", "M1")          # Set player mortgage strategies
  N <<- length(strategy)                    # N = Number of player
  startCap <<- 1500                         # Sets start capital for all players.
  roundCap <<- 200                          # Capital gained from passing 'Go'.
  bankMoney <<- 15140 - startCap*N          # Sets bank cash limit. 
  bid_Active <<- TRUE                       # Turn bidding on and off.
  mort_Active <<- TRUE                      # Turn mortage on and off.
  collectStats <<- TRUE                     # Turns collecting stats on and off. 
  printResult <<- FALSE                     # Turns printing result on and off.
  enableAiData <<- FALSE                    # Turn AI on/off.
  enableTransLog <<- FALSE                  # Turn transaction log on/off.
  printGame <<- FALSE                       # Turn printlog of game on/off.


  #---------------------------------------------------------------------------
  id <- c(1:N)                              # Creates unique player ID.
  throws <<- rep(0, times=N)                # Sets number of throws per player to initial value 0. 
  fortune <<- rep(startCap, times=N)        # Sets initial fortune for each player to startCap (e.g. 200)
  liquidity <<- rep(startCap, times=N)      # Sets initial liquidity for each player to startCap (e.g. 200)
  nHouses <<- rep(0, times=N)               # Sets inital number of houses to 0.
  nProps <<- rep(0, times=N)                # Sets initial number of properties to 0.
  active <- rep(1, times=N)                 # Initially sets all players to be active. 
  position <- rep(1, times=N)               # Sets start position for each player to 1 (i.e. 'Go'). 
  jailDays <- rep(0, times=N)               # Initializes remaining jail days variable to 0. 
  # Creates the players-dataframe fmor the variables listed above.
  players <<- data.frame(id, strategy, houseStrategy, fortune, active, position, jailDays, throws) 

  # Imports data about the game board from .csv file. 
  board <<- read.csv("monopoly_data v0.5.csv") 
  
  # Creates global vector containing all the propty colors.
  uniqueC <<- c(as.character(unique(board$color[board$color != "" & board$color != "grey"])))

  ##SLETT?!
  logForNN4temp <<- data.frame(matrix(NA, 0, 44))
  colnames(logForNN4temp) <- c("throws", "fortune", as.character(uniqueC), as.character(paste(uniqueC, "houses", sep = '')), "buyStreet", "buyHouse", "mortage", "liftmortage", "fortuneOthers", as.character(paste(uniqueC, "Others", sep = '')), as.character(paste(uniqueC, "housesOthers", sep = '')), "id")
  logForNN5temp <<- data.frame(matrix(NA, 0, 2))
  colnames(logForNN5temp) <- c("iS", "iiS")
  logForNN6temp <<- data.frame(matrix(NA, 0, 44))
  colnames(logForNN6temp) <- c("throws", "fortune", as.character(uniqueC), as.character(paste(uniqueC, "houses", sep = '')), "mortagedSelf", "liftMortageSelf" , "mortagedOther", "liftMortageOthers",  "fortuneOthers", as.character(paste(uniqueC, "Others", sep = '')), as.character(paste(uniqueC, "housesOthers", sep = '')), "win")

  
  streetNames <- as.character(board$name[board$name != "Start" & board$color != "" & board$color != "grey"])
  streetHouses <- paste(streetNames, "Houses")
  streetOther <- paste(streetNames, "Other")
  streetHousesOther <- paste(streetNames, "Other Houses")
  logForNN7temp <<- setNames(data.frame(matrix(0,ncol = 120, nrow = 1)), c("throws",
                                                                           "fortune", 
                                                                           streetNames, 
                                                                            streetHouses, 
                                                                            streetOther, 
                                                                            streetHousesOther,
                                                                           "mortagedSelf", 
                                                                           "liftMortageSelf", 
                                                                           "mortagedOther", 
                                                                           "liftMortageOthers",  
                                                                           "fortuneOthers", 
                                                                           "win"))
  purchasedLogDF <<- setNames(data.frame(matrix(NA,ncol = 112, nrow = 1)), c(streetNames, 
                                                                             streetHouses, 
                                                                             streetOther, 
                                                                             streetHousesOther))
  
  source('functions v0.5.R')
  source('ai training v0.5.R')
}

# Main-function.
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
startGame <- function(i){  
  initGame(i)                               # Resets the game (board and players dataframes).
  cur_player <<- sample(1:N, 1)             # Randomly selects first player. 
  game_over <<- FALSE                       # game_over initially set to FALSE.
  ptm <- Sys.time()                         # Timer used for timing out.
  while (game_over == FALSE) {              # Loop until game is over.
    av_dices <<- 1                          # Available dice throws.
    eqDicesCount <<- 0                      # Check number of times equal dice are thrown in the same round. 
    while (av_dices >= 1 & players$active[cur_player] == 1) {
      players$throws[cur_player] <<- players$throws[cur_player] + 1   # Increments throw number.
      av_dices <<- av_dices - 1
      dice_res <- throwDice()               # Throw dice.
      if(length(unique(dice_res)) == 1){    # If both dice show same number of eyes...  
        av_dices <<- av_dices + 1           # ... award new throw.
        eqDicesCount <<- eqDicesCount + 1   # ... increment number of equal dice thrown. 
        if(printGame==TRUE){                # Prints the event. 
          cat(sprintf("Player %s rolled two of the same face, %s. \n", cur_player, dice_res))
        }
      }
      
      if(eqDicesCount > 2){                 # If the player three times in a row has rolled two of the same face... 
        av_dices <<- 0                      # ... reset availiable dice throws and same-face counter to 0.
        eqDicesCount <<- 0              
        players$position[cur_player] <<- 9  # ... and move player to jail. 
        players$jailDays[cur_player] <<- 3  # ... and sets remaining jail days to 3. 
        if(printGame==TRUE){                # Prints the event. 
          cat(sprintf("Player %s rolled two of the same face three times and is moved to jail. \n", cur_player))
        }
        
      }else{
        move(sum(dice_res))                 # Changes the position of the player, and credits account if he passes 'Go'. 
        processPos()                        # Processes the position the player now sits on.
        mayLiftMortage()                    # Consider unleveraging properties.
      }
      
      if(checkPlayerLoss() == TRUE){        # Checks to see if player has went bankrupt...
        av_dices <<- 0                      # ... sets remaining dice throws to zero. 
      } 
    }
    
    collectRoundStatistics()                # Collects statistics on fortune, houses, etc. for the current round.
    checkGameOver()                         # Check to see if game is over. 
    setNextPlayer()                         # Changes current player before next round. 

    currentPlaytime <- Sys.time() - ptm     # Updates current playtime variable.
    if(currentPlaytime > 10){               # Checks to see if current playtime is longer than 10s.
      cat(sprintf("Time out, %s! Round took longer than 10 seconds. \n",Sys.time()))
      players$active <<- 0                  # Sets all players to inactive.
      game_over <- TRUE                     # Sets game to be over. 
      if(enableAiData == TRUE){             # Data collection for AI...
        logForNN4temp <<- data.frame(matrix(NA, 0, 42))
      }
      winnerStrategy <<- 0                  # Records 0 as winner strategy as no players won. 
      roundWinner <<- 0                     # Sets winner to be 0. 

    }
  }
  if(printResult==TRUE){                    # If in settings printResult is set to TRUE...
    printRoundResult()                      # ...prints graph containging development of fortune variable.
  }
}



# function: collectRoundStatistics()
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
collectRoundStatistics <- function(){                             # Collects statistice for each round of the game. 
  players$throws[cur_player] <<- players$throws[cur_player] + 1   # Increments throw number.
  curLiquidity <- rep(0, N)
  curProps <<- rep(0, N)
  curHouses <<- rep(0, N)
  
  curLiquidity <- players$fortune
  for (i in 1:length(curLiquidity)){
    curLiquidity[i] <- (as.numeric(curLiquidity[i]) + sum(board$price[board$owner==i & board$mortaged == 0 & !is.na(board$owner)]*1/2))
  }
  liquidity <<- cbind(liquidity, curLiquidity)                    # Appends new properties-data.
  
  for (i in 1:N) {
    curProps[i] <<- length(board$owner[(board$owner==i) & !(is.na(board$owner))])
    sumHouses <- sum(board$houses[(board$owner==i) & !(is.na(board$owner))  & !(is.na(board$houses))])
    curHouses[i] <<- sumHouses
  }
  
  fortune <<- cbind(fortune, players$fortune)                     # Appends new fortune-data.
  nProps <<- cbind(nProps, curProps)                              # Appends new properties-data.
  nHouses <<- cbind(nHouses, curHouses)                           # Appends new houses-data.
}

newVARS <- rbind(fortune, liquidity)


# function: printRoundResult()
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
printRoundResult <- function(){              # Creates ggplots og relevent post-game variables.
  fortune_data <- melt(fortune)
  fThrows <- length(fortune_data[,1])
  liq_data <- melt(liquidity)
  lThrows <- length(liq_data[,1])
  prop_data <- melt(nProps)
  pThrows <- length(prop_data[,1])
  house_data <- melt(nHouses)
  hThrows <- length(house_data[,1])
  
                                             # Plotting fortune development.
  ggplot(data=fortune_data, aes(x=1:fThrows, y=value, group=Var1, color=as.factor(Var1)))+
    geom_line()+
    theme_classic()+
    labs(title="Results", x="Throws", y="Fortune", color="Players", tag = 'For')+
    geom_hline(yintercept=0, linetype="dashed")
  
                                              # Plotting liquidity development.
  ggplot(data=liq_data, aes(x=1:lThrows, y=value, group=Var1, color=as.factor(Var1)))+
    geom_line()+
    theme_classic()+
    labs(title="Results", x="Throws", y="Fortune", color="Players", tag = 'Liq')+
    geom_hline(yintercept=0, linetype="dashed")
  
                                              # Plotting number of properties development.
  ggplot(data=prop_data, aes(x=1:pThrows, y=value, group=Var1, color=as.factor(Var1)))+
    geom_line()+
    theme_classic()+
    labs(title="Results", x="Throws", y="Fortune", color="Players", tag = 'Prop')+
    geom_hline(yintercept=0, linetype="dashed")
  
                                              # Plotting number of houses development.
  ggplot(data=house_data, aes(x=1:hThrows, y=value, group=Var1, color=as.factor(Var1)))+
    geom_line()+
    theme_classic()+
    labs(title="Results", x="Throws", y="Fortune", color="Players", tag = 'House')+
    geom_hline(yintercept=0, linetype="dashed")
}



################################################################
#                         TEST-SUITE                           #
################################################################

## SLETT FØR INNLEVERING

k <- 100
a <- 11
winners = 1:(k*a)*0
winners <- 1:15
numberOfRounds <- 1:(k*a)*0
z <- 0
for (i in 1:a) {
  for (j in 1:k) {
    z <- z + 1
    startGame(i)
    cat(sprintf("Round: %s, winnner %s", j, winnerStrategy))
    winners[z] <- winnerStrategy
  }
}

#hist(winners)
table(winners)
pbinom(179, 300, prob=0.5)

################################################################

startGame()
