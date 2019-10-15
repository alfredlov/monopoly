#Funksjoner for main dokumentet

throwDice <- function(){#kast terning og lagre resultat
  dice <- sample(1:6, size = 1, replace = TRUE) + sample(1:6, size = 1, replace = TRUE)
  return(dice)
  #simuler kast m/ 2 terninger, se hist nedenfor for bevis 
  #hist(replicate(1000, sample(1:6, size = 1, replace = TRUE) + sample(1:6, size = 1, replace = TRUE)))
}

move <- function(x){#endre position for cur_player i players data.frame
  cur_position <- players$position[cur_player]
  if(cur_position + x > nrow(board)){
    y <- nrow(board) - cur_position
    players$position[cur_player] <<- x - y
    cat(sprintf("Player %s moved %s tiles to position %s, and passed Start",cur_player, x, x-y))
  } else{
    players$position[cur_player] <<- cur_position + x
    cat(sprintf("Player %s moved %s tiles to position %s",cur_player, x, cur_position + x))
  }
} 

processPos <- function(){#håndter posisjon for spiller cur_player, leder til flere sub-funksjoner
  
} 

checkPlayerLoss <- function(){#sjekk hvis cur_player har tapt
  if(players$fortune[cur_player] < 0){
    players$active[cur_player] <<- 0
    cat(sprintf("Player %s ran out of cash", cur_player))
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
