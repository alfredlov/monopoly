##--------------------------------------------------------------------------------
## FUNCTIONS v.0.2.R
## Constains the functions used in order to model the game. 
## The functions modelling the differing strategies are contained 
##  in a seperate script. 
##--------------------------------------------------------------------------------

##--------------------------------------------------------------------------------
## throwDice: Simulates the throwing of two dice.
## output: Returns an integer between 2 and 12. 
##--------------------------------------------------------------------------------
source('strategies v0.2.R')
library(dplyr)
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
## input: x = number given by dice throw
## Increments position variable of player by what is given by the dices. Handles discontinuity 
## at Go and also handles adding $ to balance of players when passing Go. 
##--------------------------------------------------------------------------------
move <- function(x){#endre position for cur_player i players data.frame
  cur_position <- players$position[cur_player]
  if(cur_position + x > nrow(board)){
    y <- nrow(board) - cur_position
    players$position[cur_player] <<- x - y
    players$fortune[cur_player] <<- players$fortune[cur_player] + roundCap
    #cat(sprintf("Player %s moved %s tiles to position %s, and passed Go.",cur_player, x, x-y))
  }else{
    players$position[cur_player] <<- cur_position + x
    #cat(sprintf("Player %s moved %s tiles to position %s",cur_player, x, cur_position + x))
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
  position <- players$position[cur_player]
  if(board$prop[position] == 1){ #sjekk om bolig
    processProp() #håndtere landet på bolig
  }
  if(board$prop[position] == 3){
    processTrain() #håndtere landet på tog
  }
  
  ## Må skrive her hva som skjer når det ikke er en eiendom.... 
} 

processTrain <- function(){
  
}

processProp <- function(){
  position <- players$position[cur_player]
  if(board$owner[position] == 0){ #sjekk om ledig
    if(board$price[position] <= players$fortune[cur_player]){ #sjekk om råd
      #kjør strategi
      runStrategy()
    }else{
      #ikke råd
    }
  }else{
    #betal leie
    #sjekke om en selv eier gaten
    owner <- board$owner[position]
    if(owner != cur_player){
      #sjekke om den som eier gaten også eier alle i samme farge
      NoC <- nrow(board[board$color  == board$color[position],]) #hvor mange gater i den fargen
      NoCo <- nrow(board[board$color  == board$color[position] & board$owner == owner1,]) #hvor mange gater i den fargen som blir eid av eieren av denne gaten
      #------------ <Alternativt> -------------------
                #colorOfPosition <- board$color[position]
                #NoC2 <- board %>%
                  #filter(color %in% colorOfPosition) %>%
                  #nrow()
              
                #NoCo2 <- board %>%
                  #filter(owner %in% owner1, color %in% colorOfPosition) %>%
                  #nrow()
      #------------ </Alternativt> ------------------
      if(NoC == NoCo){
        #en annen spiller en den som landet her eier alle av denne fargen, dobbel leie
        players$fortune[owner] <<- players$fortune[owner] + board$rent[position]*2
        players$fortune[cur_player] <<- players$fortune[cur_player] - board$rent[position]*2
        print("DOBBEL LEIE")
        cat(sprintf("DOBBEL, spiller %s eier hele %s", owner, board$color[position]))
      }else{
        players$fortune[owner] <<- players$fortune[owner] + board$rent[position]
        players$fortune[cur_player] <<- players$fortune[cur_player] - board$rent[position]
        print("LEIE")
      }
    }
  }
}

checkPlayerLoss <- function(){#sjekk hvis cur_player har tapt
  if(players$fortune[cur_player] < 0){
    players$active[cur_player] <<- 0
    #cat(sprintf("Player %s ran out of cash!", cur_player))
  }
}

checkGameOver <- function(){#sjekk om spillet er over
  if(length(players$active[players$active==TRUE])==1){
    game_over <<- TRUE
    winner <- players$id[players$active==TRUE]
    winnerS <- players$strategy[players$active==TRUE]
    #cat(sprintf("Player %s won, using strategy %s", winner, winnerS))
  }
} 

setNextPlayer <- function(){#endre cur_player til neste
  if(cur_player == N){
    cur_player <<- 1
  } else {
    cur_player <<- cur_player + 1
  }
  #cat(sprintf("Player %s's turn",cur_player))
}
