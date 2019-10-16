runStrategy <- function(){
  strategyName <- paste("strategy", players$strategy[cur_player], sep="")
  if(get(strategyName)() == TRUE){
    #kjøp
    #cat(sprintf("kjøp %s",Sys.time()))
    position <- players$position[cur_player]
    board$owner[position] <<- cur_player
    players$fortune[cur_player] <<- players$fortune[cur_player] - board$price[position]
  }else{
    #ikke kjøp
    #print("ikke kjøp")
  }
}

strategy1 <- function(){
  return(TRUE)
}

strategy2 <- function(){
  if(sample(0:1, 1) == 1){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

strategy3 <- function(){
  if(sample(0:5, 1) == 1){
    return(TRUE)
  }else{
    return(FALSE)
  }
}