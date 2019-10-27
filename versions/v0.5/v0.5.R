##---------------------------------------------------------
## Monopoly Simulation - Main script
## Version 0.5
##---------------------------------------------------------

#NB pass på variabler: globale = << og lokale = <
#FORSLAG:FLYTT ALT AV SOURCING OF IMPORTERING AV LIBRARIES OPP HIT!!
source('ai training v0.5.R')
library(reshape2)
initGame <- function(i){
  
  #------------------------------  Settings  ------------------------------ 
  setwd("../v0.5")              # Set working directory to correct version number
  N <<- 4                       # N = Number of player
  strategy <- c(1, 2, 3, 4)        # Set player strategies, first parameter sets strategy for player 1, etc...
  houseStrategy <- c("H1", "H1", "H1", "H1")  
  #strategy <- c(sample(1:9, 1), sample(1:9, 1), sample(1:9, 1), sample(1:9, 1))
  startCap <<- 1500               # Sets start capital for all players.
  roundCap <<- 200                # Capital gained frmo passing 'Go'.
  bankMoney <<- 15140 - startCap*N
  version <- 5                    # Sets game version.
  bid_Active <<- TRUE             # Turn bidding on and off.
  collectStats <<- TRUE           # Turns collecting stats on and off. 
  enableAiData <<- FALSE          # Turn AI on/off
  printResult <<- TRUE            # Turns printing result on and off. 
  #---------------------------------------------------------------------------
  
  #logForNN4temp <<- data.frame(matrix(NA, 0, 42))
  colnames(logForNN4temp) <- c("throws", "fortune", as.character(uniqueC), as.character(paste(uniqueC, "houses", sep = '')), "buyStreet", "buyHouse", "fortuneOthers", as.character(paste(uniqueC, "Others", sep = '')), as.character(paste(uniqueC, "housesOthers", sep = '')), "id")
  
  id <- c(1:N) #som vektor til data.frame
  throws <<- rep(0, times=N)
  fortune <<- rep(startCap, times=N)
  nHouses <<- rep(0, times=N)
  nProps <<- rep(0, times=N)
  active <- rep(1, times=N) #sett spillere som er aktive, alle v/ init
  position <- rep(1, times=N) 
  jailDays <- rep(0, times=N) 
  players <<- data.frame(id, strategy, houseStrategy, fortune, active, position, jailDays, throws) #data.frame m/ oversikt over spillerne
}

##---------------------------------------------------------
## Main-function.
##---------------------------------------------------------
startGame <- function(i){  
  initGame(i)                      # Reset verdier for spillet(start på nytt)
  source('functions v0.5.R')
  source('ai training v0.5.R')
  library(ggplot2)
  board <<- read.csv("monopoly_data v0.5.csv") #importer/reset gameboard som data.frame
  
  uniqueC <<- c(as.character(unique(board$color[board$color != "" & board$color != "grey"])))
  logForNN4temp <<- data.frame(matrix(NA, 0, 42))
  colnames(logForNN4temp) <- c("throws", "fortune", as.character(uniqueC), as.character(paste(uniqueC, "houses", sep = '')), "buyStreet", "buyHouse", "fortuneOthers", as.character(paste(uniqueC, "Others", sep = '')), as.character(paste(uniqueC, "housesOthers", sep = '')), "id")
  
  
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
    labs(title="Results", x="Throws", y="Fortune", color="Players")
}

################################################################
#############             TEST-SUITE             ###############
################################################################
# k <- 50
# a <- 0
# #s=9
# winners = 1:k*0
# numberOfRounds <- 1:k*0
# a <<- 0
# for (j in 1:k) {
#   a <<- a + 1
#   cat(sprintf("Round: %s, winnner %s", j, winnerS))
#   startGame()
#   winners[j] <- winnerS
# }
# 
# 
# #hist(winners)
# table(winners)
# pbinom(68, 100, prob=0.5)
################################################################

startGame()
