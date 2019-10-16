##--------------------------------------------------------------------------------
## FUNCTIONS v.0.1.R
## Constains the functions used in order to model the game. 
## The functions modelling the differing strategies are contained 
##  in a seperate script. 
##--------------------------------------------------------------------------------

##--------------------------------------------------------------------------------
## throwDice: Simulates the throwing of two dice.
## output: Returns an integer between 2 and 12. 
##--------------------------------------------------------------------------------
throwDice <- function(){
  dice <- sum(sample(1:6, size = 2, replace = TRUE))
  return(dice)
  
  ##Vurder å slette disse kommentarene!!
  #simuler kast m/ 2 terninger, se hist nedenfor for bevis 
  #hist(replicate(1000, sample(1:6, size = 1, replace = TRUE) + sample(1:6, size = 1, replace = TRUE)))
  #hist(replicate(100000, sum(sample(1:6, size = 2, replace = TRUE))))
  
}

##--------------------------------------------------------------------------------
## move: Handles the changing the position of the players on the board.
## Increments cur_position by what is given by the dices. Handles discontinuity 
## at Go and also handles adding $ to balance of players when passing Go. 
##--------------------------------------------------------------------------------
move <- function(x){#endre position for cur_player i players data.frame
  cur_position <- players$position[cur_player]
  if(cur_position + x > nrow(board)){
    y <- nrow(board) - cur_position
    players$position[cur_player] <<- x - y
    players$fortune[cur_player] <<- players$fortune[cur_player] + roundCap
    cat(sprintf("Player %s moved %s tiles to position %s, and passed Go.",cur_player, x, x-y))
  } else{
    players$position[cur_player] <<- cur_position + x
    cat(sprintf("Player %s moved %s tiles to position %s.",cur_player, x, cur_position + x))
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
processPos <- function(){#håndter posisjon for spiller cur_player, leder til flere sub-funksjoner
  if(board$prop[players$position[cur_player]] == 1){ #sjekk om bolig
    if(board$eier[players$position[cur_player]] == 0){ #sjekk om ledig
      if(board$price[players$position[cur_player]] < players$fortune[cur_player]){ #sjekk om råd
        
      }
      
      ##Her skjer det vel ingenting siden personen ikke kan kjøpe leiligheten. Den blir effektivt en blank tile. 
      
    }
    
    ## Må skrive her en funkjson som tar for seg å betale spillerem for å ha landet på deres eiendom
    
  }
  
  ## Må skrive her hva som skjer når det ikke er en eiendom.... 
} 

checkPlayerLoss <- function(){#sjekk hvis cur_player har tapt
  if(players$fortune[cur_player] < 0){
    players$active[cur_player] <<- 0
    cat(sprintf("Player %s ran out of cash!", cur_player))
  }
}

checkGameOver <- function(){#sjekk om spillet er over
  if(length(players$active[players$active==TRUE])==1){
    game_over <<- TRUE
    winner <- players$id[players$active==TRUE]
    winnerS <- players$strategy[players$active==TRUE]
    cat(sprintf("Player %s won, using strategy %s", winner, winnerS))
  }
} 

setNextPlayer <- function(){#endre cur_player til neste
  if(cur_player == N){
    cur_player <<- 1
  } else {
    cur_player <<- cur_player + 1
  }
  cat(sprintf("Player %s's turn",cur_player))
}
