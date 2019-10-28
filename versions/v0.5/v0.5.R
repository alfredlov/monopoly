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
  strategy <- c(1, 1)                       # Set player strategies, first parameter sets strategy for player 1, etc...
  houseStrategy <- c("H1", "H1")            # Set player house-buying strategies
  mortageStrategy <- c("M1", "M1")          # Set player mortgage strategies
  N <<- length(strategy)                    # N = Number of player
  startCap <<- 1500                         # Sets start capital for all players.
  roundCap <<- 200                          # Capital gained from passing 'Go'.
  bankMoney <<- 15140 - startCap*N          # Sets bank cash limit. 
  bid_Active <<- TRUE                       # Turn bidding on and off.
  mort_Active <<- TRUE                      # Turn mortage on and off.
  collectStats <<- TRUE                     # Turns collecting stats on and off. 
  printResult <<- FALSE                     # Turns printing result on and off.
  enableAiData <<- FALSE                    # Turn AI on/off
  enableTransLog <<- FALSE                  # Turn transaction log on/off
  #---------------------------------------------------------------------------
  id <- c(1:N)                              # Creates unique player ID.
  throws <<- rep(0, times=N)                # Sets number of throws per player to initial value 0. 
  fortune <<- rep(startCap, times=N)        # Sets initial fortune for each player to startCap (e.g. 200)
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
  
  # Creates log which is used for implementation of the Neural Network, AI. 
  logForNN4temp <<- data.frame(matrix(NA, 0, 42))
  colnames(logForNN4temp) <- c("throws", "fortune", as.character(uniqueC), as.character(paste(uniqueC, "houses", sep = '')), "buyStreet", "buyHouse", "fortuneOthers", as.character(paste(uniqueC, "Others", sep = '')), as.character(paste(uniqueC, "housesOthers", sep = '')), "id")
  
  # Sources associated scripts. 
  source('functions v0.5.R')
  source('ai training v0.5.R')
}

##---------------------------------------------------------
## Main-function.
##---------------------------------------------------------
startGame <- function(i){  
  initGame(i)                      # Reset verdier for spillet(start på nytt)
  cur_player <<- sample(1:N, 1)   # Selects initial player at random. Eliminates potential first-mover (dis)advantage.
  game_over <<- FALSE
  ptm <- Sys.time()  #Timer
  while (game_over == FALSE) { #loop så lenge ikke én er vinnner
    av_dices <<- 1 #available dice throws, pga to like = nytt kast..
    equalDicesQount <<- 0 #sjekke hvor mange ganger på rad to like, hvis over tre -> fengsel
    while (av_dices >= 1 & players$active[cur_player] == 1) {
      av_dices <<- av_dices - 1
      dice_res <- throwDice() #kast terning og lagre resultat
      if(length(unique(dice_res)) == 1){
        av_dices <<- av_dices + 1
        equalDicesQount <<- equalDicesQount + 1
        #print("SLO TO LIKE")
      }
      if(equalDicesQount > 2){
        av_dices <<- 0
        equalDicesQount <<- 0
        players$position[cur_player] <<- 9 #teleporter til jail
        players$jailDays[cur_player] <<- 3 #kommer ut på 3. runden
        #print("to like tre ganger = fengsel")
      }else{
        mayLiftMortage() #vurder å kjøpe tilbake eiendommer
        move(sum(dice_res)) #endre position for cur_player i players data.frame
        processPos() #håndter posisjon for spiller cur_player, leder til flere sub-funksjoner
      }
      if(checkPlayerLoss() == TRUE){
        av_dices <<- 0
      } #sjekk hvis cur_player har tapt
    }
    
    ################################################################
    ############# Samler inn statistikk for runden.  ###############
    ################################################################
    # Samler inn data om antall kast, formue og antall eiendommer, og hus.
    
    #Kast
    players$throws[cur_player] <<- players$throws[cur_player] + 1
    
    #Formue
    fortune <<- cbind(fortune, players$fortune)
    
    #Eiendom
    curProps <- rep(0, N)
    for (i in 1:N) {
      curProps[i] <- length(board$owner[(board$owner==i) & !(is.na(board$owner))])
    }
    nProps <<- cbind(nProps, curProps)
    
    # Hus
    curHouses <- rep(0, N)
    for (i in 1:N) {
      sumHouses <- sum(board$houses[(board$owner==i) & !(is.na(board$owner))  & !(is.na(board$houses))])
      curHouses[i] <- sumHouses
    }
    nHouses <<- cbind(nHouses, curHouses)
    
    ################################################################
    #############    Slutt: Statistikkinnsamling     ###############
    ################################################################
    
    checkGameOver() #sjekk om spillet er over
    setNextPlayer() #endre cur_player til neste
    
    ptm2 <- Sys.time() - ptm
    #timeout funskjon for å forhindre krasj
    if(ptm2 > 10){
      cat(sprintf("time out %s",Sys.time()))
      players$active[players$id != players$id[players$fortune == max(players$fortune)]] <<- 0
      players$active[players$id == players$id[players$fortune == max(players$fortune)]] <<- 0
      checkGameOver()
      game_over <- TRUE
      
      #logForNN4temp <<- data.frame(matrix(NA, 0, 42))
      winnerS <<- 0
      winner <<- 0
    }
  }
  if(printResult==TRUE){
    printRoundResult()
  }
}
startGame()

# #---------------
# #Mål hvor mange runder spillet går
# #---------------
# #lengde <<- c()
# #replicate(100, startGame())
# #mean(lengde)
# #lengde <- lengde[lengde<=200]
# #hist(lengde, breaks=20, xlim=c(0,360))
# lengde <<- c()
# replicate(100, startGame())
# hist(lengde, breaks=20, xlim=c(0,360), ylim = c(0,20))
# ## TESTING FOR Å FÅ UT VERDIER PÅ HVILKEN STRATEGI SOM ER BEST
#plot(nn)

################################################################
#############    PRINT ROUND RESULTS-FUNCTION    ###############
################################################################

printRoundResult <- function(){
  test_data <- melt(fortune)
  thisThrows <- length(test_data[,1])

  ggplot(data=test_data, aes(x=1:thisThrows, y=value, group=Var1, color=as.factor(Var1)))+
    geom_line()+
    theme_classic()+
    labs(title="Results", x="Throws", y="Fortune", color="Players")+
    geom_hline(yintercept=0, linetype="dashed")
}

################################################################
#############             TEST-SUITE             ###############
################################################################
k <- 50
a <- 0
#s=9
winners = 1:k*0
numberOfRounds <- 1:k*0
a <<- 0
for (j in 1:k) {
  a <<- a + 1
  cat(sprintf("Round: %s, winnner %s", j, winnerS))
  startGame()
  winners[j] <- winner
}


#hist(winners)
table(winners)
pbinom(25, 45, prob=0.5)
################################################################

startGame()
