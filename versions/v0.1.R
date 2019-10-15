# Monopoly simulation

#import gameboard as dataset
board <- read.csv("../monopoly_data simple.csv")

n <- 2 #spillere
currentPlayer <- 0 #aktuell spiller

# loop game
  #init game 
    #definere data.frame for spillere
      #slik at alt resetes hver gang
      #definere én strategi pr spiller
      #velg hvem som starter (variabel i dataframe som har verdi)
  #start game, for løkke som går så lenge begge spillere har penger > 0
    #kast terning
    #kjør håndter ny posisjon 

