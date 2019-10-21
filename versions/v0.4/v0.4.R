##---------------------------------------------------------
## Monopoly Simulation - Main script
## Version 0.4
##---------------------------------------------------------

#NB pass på variabler: globale = << og lokale = <
#NB test enkeltfunksjoner fra dette dokumentet, ikke 'functions vx.x.R'


initGame <- function(){
  #------------------------- Settings --------------------------
  setwd("../v0.4")              # Set working directory to correct version number
  N <<- 4                       # N = Number of player
  strategy <- c(1, 3, 8, 9)     # Set player strategies, first parameter sets strategy for player 1, etc...
  startCap <- 1500              # Sets start capital for all players.
  roundCap <<- 200              # Capital gained frmo passing 'Go'.
  version <- 4                  # Sets game version.
  bid_Active <<- TRUE           # Turn bidding on and off.
  #-------------------------------------------------------------
  
  id <- c(1:N) #som vektor til data.frame
  fortune <<- rep(startCap, times=N)
  nProps <<- rep(0, times=N)
  active <- rep(1, times=N) #sett spillere som er aktive, alle v/ init
  position <- rep(1, times=N) #tile 1 er start
  jailDays <- rep(0, times=N) #tile 1 er start
  players <<- data.frame(id, strategy, fortune, active, position, jailDays) #data.frame m/ oversikt over spillerne
}

##---------------------------------------------------------
## Main-function.
##---------------------------------------------------------
startGame <- function(){
  source('functions v0.4.R')
  library(ggplot2)
  board <<- read.csv("monopoly_data v0.4.csv") #importer/reset gameboard som data.frame
  
  initGame()                      # Reset verdier for spillet(start på nytt)
  
  cur_player <<- sample(1:N, 1)   # Selects initial player at random. Eliminates potential first-mover (dis)advantage.
  
  game_over <<- FALSE
  
  ptm <- Sys.time()               #Timer
  while (game_over == FALSE) { #loop så lenge ikke én er vinnner
    av_dices <<- 1 #available dice throws, pga to like = nytt kast..
    equalDicesQount <<- 0 #sjekke hvor mange ganger på rad to like, hvis over tre -> fengsel
    while (av_dices >= 1 & players$active[cur_player] == 1) {
      av_dices <<- av_dices - 1
      dice_res <- throwDice() #kast terning og lagre resultat
      if(length(unique(dice_res)) == 1){
        av_dices <<- av_dices + 1
        equalDicesQount <<- equalDicesQount + 1
        print("SLO TO LIKE")
      }
      if(equalDicesQount > 2){
        av_dices <<- 0
        equalDicesQount <<- 0
        players$position[cur_player] <<- 9 #teleporter til jail
        players$jailDays[cur_player] <<- 3 #kommer ut på 3. runden
        print("to like tre ganger = fengsel")
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
    
    # Samler inn fortune-data.
    fortune <<- cbind(fortune, players$fortune)
    
    # Samler inn eiendoms-data.
    curProps <- rep(0, N)
    for (i in 1:N) {
      curProps[i] <- length(board$owner[(board$owner==i) & !(is.na(board$owner))])
    }
    nProps <<- cbind(nProps, curProps)
    
    ################################################################
    #############    Slutt: Statistikkinnsamling     ###############
    ################################################################
    
    checkGameOver() #sjekk om spillet er over
    setNextPlayer() #endre cur_player til neste
    ptm2 <- Sys.time() - ptm
    #timeout funskjon for å forhindre krasj
    if(ptm2 > 2){
      game_over <- TRUE
      cat(sprintf("time out %s",Sys.time()))

    }

    
  }


  
  
  ##PRINTFUNKSJON FOR FORTUNE-UTVIKING
  fortune <- t(data.frame(fortune))
  colnames(fortune)[1:N] <- paste(sprintf("player%s", 1:ncol(fortune)))

  

  ################################################################
  ############# PRINTFUNKSJON FOR FORTUNE-UTVIKING ###############
  #############               MED GGPLOT           ###############
  ################################################################

  ##SLETT??
  # fortune <- data.frame(fortune)
  # fortune <- t(data.frame(fortune))
  # colnames(fortune)[1:N] <- paste(sprintf("player%s", 1:ncol(fortune)))

  fortune <- data.frame(fortune)
  fortune %>% 
    ggplot() +
    geom_line(aes(x = 1:nrow(fortune), y = fortune[,1]), color="blue") +
    geom_line(aes(x = 1:nrow(fortune), y = fortune[,2]), color="red") +
    geom_line(aes(x = 1:nrow(fortune), y = fortune[,3]), color="orange") +
    geom_line(aes(x = 1:nrow(fortune), y = fortune[,4]), color="green") +
    scale_color_manual(values = c("#00AFBB", "#E7B800")) +
    theme_minimal()
  
  ################################################################
  #############        SLUTT FORTUNE-PRINT         ###############
  ################################################################
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
k=100
winners = 1:k*0
numberOfRounds <- 1:k*0
# 
for (i in 1:k) {
  startGame()
  winners[i] <- winner
}

hist(winners)
table(winners)
# 
# pbinom(73, size = 100, prob = 0.5)
# 
# ## Number of rounds
# hist(numberOfRounds, breaks=60)
# 
# ##Plot the results!!!
# plot(x=1:length(fortune1), y = fortune1, ylim=c(0, max(c(fortune1, fortune2))))
# lines(x=1:length(fortune2), y = fortune2)
# print("number of props")
# print(length(board$owner[board$owner==1]))
# print(length(board$owner[board$owner==2]))
