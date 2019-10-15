##---------------------------------------------------------
## Monopoly Simulation
## Version 1.0
##---------------------------------------------------------

source('functions v0.1.R') #importer funksjoner 

#NB pass på variabler: globale = << og lokale = <
#NB test enkeltfunksjoner fra dette dokumentet, ikke 'functions vx.x.R'
initGame <- function(){
  #---------------- Settings -------------------
  N <<- 2 #spillere
  strategy <- c(1, 2) #strategier for spillere, spiller 1 får første verdi som strategi etc..
  startCap <- 2000 #startkapital for spillere, 'Cap' for capital
  roundCap <<- 200 #penger for å passere start
  version <- 1 #sette hvilken versjon av spillet å kjøre(?)
  #---------------------------------------------
  
  id <- c(1:N) #som vektor til data.frame
  fortune <- rep(startCap, times=N)
  active <- rep(1, times=N) #sett spillere som er aktive, alle v/ init
  position <- rep(1, times=N) #tile 1 er start
  players <<- data.frame(id, strategy, fortune, active, position) #data.frame m/ oversikt over spillerne
}

startGame <- function(){
  board <<- read.csv("../monopoly_data simple.csv") #importer/reset gameboard som data.frame
                                                    #tile 1 er start!
  initGame() #reset verdier for spillet(start på nytt)
  cur_player <<- sample(1:N, 1) #velg tilfeldig startspiller, neglesjere first-mover
  game_over <<- FALSE
  while (game_over == FALSE) { #loop så lenge ikke én er vinnner
    dice_res <- throwDice() #kast terning og lagre resultat
    move(dice_res) #endre position for cur_player i players data.frame
    processPos() #håndter posisjon for spiller cur_player, leder til flere sub-funksjoner
    checkPlayerLoss() #sjekk hvis cur_player har tapt
    checkGameOver() #sjekk om spillet er over
    setNextPlayer() #endre cur_player til neste
  }
  #LEGG TIL "print('spiller i(med strategi j) vant!')"
}

#start spillet
startGame()
