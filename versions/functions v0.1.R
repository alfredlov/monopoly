##--------------------------------------------------------------------------------
## FUNCTIONS v.0.1.R
## Constains the functions used in order to model the game. 
## The functions modelling the differing strategies are contained 
##  in a seperate script. 
##--------------------------------------------------------------------------------

##--------------------------------------------------------------
## throwDice: Simulates the throwing of two dice.
## output: Returns an integer between 2 and 12. 
##--------------------------------------------------------------
throwDice <- function(){
  dice <- sum(sample(1:6, size = 2, replace = TRUE))
  return(dice)
  
  ##Vurder å slette disse kommentarene!!
  #simuler kast m/ 2 terninger, se hist nedenfor for bevis 
  #hist(replicate(1000, sample(1:6, size = 1, replace = TRUE) + sample(1:6, size = 1, replace = TRUE)))
  #hist(replicate(100000, sum(sample(1:6, size = 2, replace = TRUE))))
  
}

move <- function(x){#endre position for cur_player i players data.frame
  cur_position <- players$position[cur_player]
  if(cur_position + x > nrow(board)){
    y <- nrow(board) - cur_position
    players$position[cur_player] <<- x - y
    players$fortune[cur_player] <<- players$fortune[cur_player] + roundCap
    cat(sprintf("Player %s moved %s tiles to position %s, and passed Start",cur_player, x, x-y))
  } else{
    players$position[cur_player] <<- cur_position + x
    cat(sprintf("Player %s moved %s tiles to position %s",cur_player, x, cur_position + x))
  }
} 

processPos <- function(){#håndter posisjon for spiller cur_player, leder til flere sub-funksjoner
  if(board$prop[players$position[cur_player]] == 1){ #sjekk om bolig
    if(board$eier[players$position[cur_player]] == 0){ #sjekk om ledig
      if(board$price[players$position[cur_player]] < players$fortune[cur_player]){ #sjekk om råd
        
      }
    }
  }
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
