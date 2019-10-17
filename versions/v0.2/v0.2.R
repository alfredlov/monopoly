##---------------------------------------------------------
## Monopoly Simulation - Main script
## Version 0.2
##---------------------------------------------------------

source('functions v0.2.R') #importer funksjoner 
#NB pass på variabler: globale = << og lokale = <
#NB test enkeltfunksjoner fra dette dokumentet, ikke 'functions vx.x.R'
initGame <- function(){
  #---------------- Settings -------------------
  N <<- 2 #spillere
  strategy <- c(5, 1) #strategier for spillere, spiller 1 får første verdi som strategi etc..
  startCap <- 700 #startkapital for spillere, 'Cap' for capital
  roundCap <<- 10 #penger for å passere start
  version <- 2 #sette hvilken versjon av spillet å kjøre(?)
  #---------------------------------------------
  
  id <- c(1:N) #som vektor til data.frame
  fortune <- rep(startCap, times=N)
  active <- rep(1, times=N) #sett spillere som er aktive, alle v/ init
  position <- rep(1, times=N) #tile 1 er start
  players <<- data.frame(id, strategy, fortune, active, position) #data.frame m/ oversikt over spillerne
}

##---------------------------------------------------------
## Main-function.
##---------------------------------------------------------
startGame <- function(){
  fortune1 <<- c()
  fortune2 <<- c()
  board <<- read.csv("monopoly_data v0.2.csv") #importer/reset gameboard som data.frame
                                                    #tile 1 er start!
  initGame() #reset verdier for spillet(start på nytt)
  cur_player <<- sample(1:N, 1) #velg tilfeldig startspiller, neglesjere first-mover
  game_over <<- FALSE
  
  ptm <- Sys.time() #timer
  while (game_over == FALSE) { #loop så lenge ikke én er vinnner
    if(cur_player == 1){
    fortune1 <<- c(fortune1, players$fortune[1])
    }else{
    fortune2 <<- c(fortune2, players$fortune[2])
    }
    dice_res <- throwDice() #kast terning og lagre resultat
    move(dice_res) #endre position for cur_player i players data.frame
    processPos() #håndter posisjon for spiller cur_player, leder til flere sub-funksjoner
    checkPlayerLoss() #sjekk hvis cur_player har tapt
    checkGameOver() #sjekk om spillet er over
    setNextPlayer() #endre cur_player til neste
    ptm2 <- Sys.time() - ptm
    #timeour funskjon for å forhindre krasj
    if(ptm2 > 2){
      game_over <- TRUE
      cat(sprintf("time out %s",Sys.time()))
      fortune1 <<- c(fortune1, players$fortune[1])
      fortune2 <<- c(fortune2, players$fortune[2])
    }
  }
  #plot(x=1:length(fortune1), y=fortune1, type = "b", ylab="Fortune", xlab="Throws") +
  #  lines(x=1:length(fortune2), y=fortune2)
  
  #lengde <<- c(lengde, length(fortune1))
}

##startGame()

#---------------
#Mål hvor mange runder spillet går
#---------------
#lengde <<- c()
#replicate(100, startGame())
#mean(lengde)
#lengde <- lengde[lengde<=200]
#hist(lengde, breaks=20)

## TESTING FOR Å FÅ UT VERDIER PÅ HVILKEN STRAT SOM ER BEST
k=200
winners = 1:k*0
for (i in 1:k) {
  startGame()
  winners[i] <- winner
}

hist(winners)
table(winners)

pbinom(135, size = 200, prob = 0.5)

##Plot the results!!!
plot(x=1:length(fortune1), y = fortune1, ylim=c(0, max(c(fortune1, fortune2))))
lines(x=1:length(fortune2), y = fortune2)
print("number of props")
print(length(board$owner[board$owner==1]))
print(length(board$owner[board$owner==2]))
