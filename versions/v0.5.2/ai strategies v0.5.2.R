##--------------------------------------------------------------------------------##
## Monopoly Simulation - AI-Strategies set script                                 ##
## Version 0.5                                                                    ##
##--------------------------------------------------------------------------------##

mround <- function(x,base){
  base*round(x/base)
}


#####################################################################################
#                                  AI-STRATEGIES                                    #
#####################################################################################

##-----------------------------------------------------------------------------------
##  Strategy 100: AI-strategy 0
##-----------------------------------------------------------------------------------
strategy100 <- function(x){
  if(length(fortune) > 5){
    aprox.position <- mround(length(fortune[cur_player,])/5, 5)
  }else{
    prox.position <- 5
  }
  houses <- sum(board$houses[(board$owner==cur_player) & !(is.na(board$owner))  & !(is.na(board$houses))])
  balance1 <- players$fortune[cur_player]
  properties1 <- length(board$owner[(board$owner==cur_player) & !(is.na(board$owner))])
  if(!missing(x)){
    balance2 <- players$fortune[cur_player] - board$housePrice[players$position[cur_player]]
    houses2 <- sum(board$houses[(board$owner==cur_player) & !(is.na(board$owner))  & !(is.na(board$houses))])+ 1
    
    x5 <- c(aprox.position, aprox.position)
    x1500 <- c(balance1, balance2)
    x0 <- c(properties1, properties1)
    x0.1 <- c(houses, houses2)
    
    test=data.frame(x5,x1500, x0, x0.1)
    Predict=neuralnet::compute(nn,test)
    Predict$net.result
    
    for (i in 1:2) {
      if(is.na(Predict$net.result[i])){
        Predict$net.result[i] <- 0
      }
    }
    if(Predict$net.result[1] > Predict$net.result[2]){
      return(FALSE)
    }else{
      return(TRUE)
    }
  }else{
    balance2 <- players$fortune[cur_player] - propPrice
    properties2 <- length(board$owner[(board$owner==cur_player) & !(is.na(board$owner))]) + 1
    
    x5 <- c(aprox.position, aprox.position)
    x1500 <- c(balance1, balance2)
    x0 <- c(properties1, properties2)
    x0.1 <- c(houses, houses)
    
    test=data.frame(x5,x1500, x0, x0.1)
    Predict=neuralnet::compute(nn,test)
    Predict$net.result
    
    for (i in 1:2) {
      if(is.na(Predict$net.result[i])){
        Predict$net.result[i] <- 0
      }
    }
    if(Predict$net.result[1] > Predict$net.result[2]){
      return(FALSE)
    }else{
      return(TRUE)
    }
  }
}

##-----------------------------------------------------------------------------------
##  Strategy 101: AI-strategy 1
##-----------------------------------------------------------------------------------

strategy101 <- function(x){
  houses <- sum(board$houses[(board$owner==cur_player) & !(is.na(board$owner))  & !(is.na(board$houses))])
  properties1 <- length(board$owner[(board$owner==cur_player) & !(is.na(board$owner))])
  if(!missing(x)){
    houses2 <- sum(board$houses[(board$owner==cur_player) & !(is.na(board$owner))  & !(is.na(board$houses))])+ 1
    
    x0 <- c(properties1, properties1)
    x0.1 <- c(houses, houses2)
    
    test=data.frame(x0, x0.1)
    Predict=neuralnet::compute(nn,test)
    Predict$net.result
    
    for (i in 1:2) {
      if(is.na(Predict$net.result[i])){
        Predict$net.result[i] <- 0
      }
    }
    if(Predict$net.result[1] > Predict$net.result[2]){
      return(FALSE)
    }else{
      return(TRUE)
    }
  }else{
    properties2 <- length(board$owner[(board$owner==cur_player) & !(is.na(board$owner))]) + 1
    
    x0 <- c(properties1, properties2)
    x0.1 <- c(houses, houses)
    
    test=data.frame(x0, x0.1)
    Predict=neuralnet::compute(nn,test)
    Predict$net.result
    
    for (i in 1:2) {
      if(is.na(Predict$net.result[i])){
        Predict$net.result[i] <- 0
      }
    }
    if(Predict$net.result[1] > Predict$net.result[2]){
      return(FALSE)
    }else{
      return(TRUE)
    }
  }
}


##-----------------------------------------------------------------------------------
##  Strategy 102: AI-strategy 2
##-----------------------------------------------------------------------------------

strategy102 <- function(x){
  uniqueC <- unique(board$color[board$color != "" & board$color != "white" & board$color != "grey"])
  streetColFreq <<- c()
  houseColFreq <<- c()
  for (i in 1:length(uniqueC)) {
    NoC <- nrow(board[board$color  == uniqueC[i],]) 
    NoCo <- nrow(board[board$color  == uniqueC[i] & board$owner == cur_player & !(is.na(board$owner)),]) 
    streetColFreq <<- c(streetColFreq, NoCo)
    
    sumHouses <- sum(board2$houses[(board2$owner==y) & !(is.na(board2$owner))  & !(is.na(board2$houses)) & board2$color  == uniqueC[i]])
    houseColFreq <<- c(houseColFreq, sumHouses)
  }
  
  if(!missing(x)){
    hypStreet <<- houseColFreq
    hypStreet[which(uniqueC==board$color[players$position[cur_player]])] <<- hypStreet[which(uniqueC==board$color[players$position[cur_player]])] + 1
    
    test=data.frame()
    test<- rbind(test, c(streetColFreq, houseColFreq))
    test<- rbind(test, c(streetColFreq, hypStreet))
    colnames(test) <- c(as.character(uniqueC), as.character(paste(uniqueC, "houses", sep = '')))
    Predict=neuralnet::compute(nn,test)
    Predict$net.result
    for (i in 1:2) {
      if(is.na(Predict$net.result[i])){
        Predict$net.result[i] <- 0
      }
    }
    if(Predict$net.result[1] > Predict$net.result[2]){
      return(FALSE)
    }else{
      return(TRUE)
    }
  }else{
    hypStreet <<- streetColFreq
    hypStreet[which(uniqueC==board$color[players$position[cur_player]])] <<- hypStreet[which(uniqueC==board$color[players$position[cur_player]])] + 1
    
    test=data.frame()
    test<- rbind(test, c(streetColFreq, houseColFreq))
    test<- rbind(test, c(hypStreet, houseColFreq))
    colnames(test) <- c(as.character(uniqueC), as.character(paste(uniqueC, "houses", sep = '')))
    Predict=neuralnet::compute(nn,test)
    Predict$net.result
    
    for (i in 1:2) {
      if(is.na(Predict$net.result[i])){
        Predict$net.result[i] <- 0
      }
    }
    if(Predict$net.result[1] > Predict$net.result[2]){
      return(FALSE)
    }else{
      return(TRUE)
    }
  }
}


##-----------------------------------------------------------------------------------
##  Strategy 103: AI-strategy 3
##-----------------------------------------------------------------------------------

strategy103 <- function(x){
  pos <- players$position[cur_player]
  fort <- players$fortune[cur_player]
  uniqueC <- unique(board$color[board$color != "" & board$color != "grey"])
  streetColFreq <<- c()
  streetColFreqOthers <<- c()
  houseColFreq <<- c()
  houseColFreqOthers <<- c()
  for (i in 1:length(uniqueC)) {
    NoCo <- nrow(board[board$color  == uniqueC[i] & board$owner == cur_player & !(is.na(board$owner)),]) 
    streetColFreq <<- c(streetColFreq, NoCo)
    NoCo2 <- nrow(board[board$color  == uniqueC[i] & board$owner != cur_player & board$owner != 0 & !(is.na(board$owner)),]) 
    streetColFreqOthers <<- c(streetColFreq, NoCo2)
    
    sumHouses <- sum(board$houses[(board$owner==cur_player) & !(is.na(board$owner))  & !(is.na(board$houses)) & board$color  == uniqueC[i]])
    houseColFreq <<- c(houseColFreq, sumHouses)
    sumHouses2 <- sum(board$houses[(board$owner!=cur_player) & (board$owner!=0) & !(is.na(board$owner))  & !(is.na(board$houses)) & board$color  == uniqueC[i]])
    houseColFreqOthers <<- c(houseColFreq, sumHouses2)
  }
  if(!missing(x)){
    hypStreet <<- houseColFreq
    hypStreet[which(uniqueC==board$color[players$position[cur_player]])] <<- hypStreet[which(uniqueC==board$color[players$position[cur_player]])] + 1
    fort2 <- fort - board$housePrice[players$position[cur_player]]
    test=data.frame()
    test<- rbind(test, c(pos, fort, streetColFreq, houseColFreq, 0, 0, sum(players$fortune[players$id != cur_player]), streetColFreqOthers, houseColFreqOthers))
    test<- rbind(test, c(pos, fort2, streetColFreq, hypStreet, 0, 1, sum(players$fortune[players$id != cur_player]), streetColFreqOthers, houseColFreqOthers))
    colnames(test) <<- c("throws", "fortune", as.character(uniqueC), as.character(paste(uniqueC, "houses", sep = '')), "buyStreet", "buyHouse", "fortuneOthers", as.character(paste(uniqueC, "Others", sep = '')), as.character(paste(uniqueC, "housesOthers", sep = '')))
    
    Predict=neuralnet::compute(nn,test)
    Predict$net.result
    for (i in 1:2) {
      if(is.na(Predict$net.result[i])){
        Predict$net.result[i] <- 0
      }
    }
    if(Predict$net.result[1] > Predict$net.result[2]){
      return(FALSE)
    }else{
      return(TRUE)
    }
  }else{
    hypStreet <<- streetColFreq
    hypStreet[which(uniqueC==board$color[players$position[cur_player]])] <<- hypStreet[which(uniqueC==board$color[players$position[cur_player]])] + 1
    fort2 <- fort - propPrice
    
    
    test=data.frame()
    test<- rbind(test, c(pos, fort, streetColFreq, houseColFreq, 0, 0,0,0, sum(players$fortune[players$id != cur_player]), streetColFreqOthers, houseColFreqOthers))
    test<- rbind(test, c(pos, fort2, hypStreet, houseColFreq, 1, 0, 0,0,sum(players$fortune[players$id != cur_player]), streetColFreqOthers, houseColFreqOthers))
    colnames(test) <<- c("throws", "fortune", as.character(uniqueC), as.character(paste(uniqueC, "houses", sep = '')), "buyStreet", "buyHouse", "fortuneOthers", as.character(paste(uniqueC, "Others", sep = '')), as.character(paste(uniqueC, "housesOthers", sep = '')))
    Predict=neuralnet::compute(nn,test)
    Predict$net.result
    
    for (i in 1:2) {
      if(is.na(Predict$net.result[i])){
        Predict$net.result[i] <- 0
      }
    }
    if(Predict$net.result[1] > Predict$net.result[2]){
      return(FALSE)
    }else{
      return(TRUE)
    }
  }
}

##-----------------------------------------------------------------------------------
##  Strategy 104: AI-strategy 4
##-----------------------------------------------------------------------------------

strategy104 <- function(x, y){
  if(!missing(x)){
    stratPlayer <<- x
  }else{
    stratPlayer <<- cur_player
  }
  pos <- players$throws[stratPlayer]
  fort <- players$fortune[stratPlayer]
  countFreq(stratPlayer)
  fort2 <- fort - propPrice
  normalize2 <- function(x){
    return((x - min(logForNN4)) / (max(logForNN4) - min(logForNN4)))
  }
  predictFunc <- function(x){
    colnames(x) <- c("throws", "fortune", as.character(uniqueC), as.character(paste(uniqueC, "houses", sep = '')), "buyStreet", "buyHouse", "mortage", "liftmortage", "fortuneOthers", as.character(paste(uniqueC, "Others", sep = '')), as.character(paste(uniqueC, "housesOthers", sep = '')))
    
    testNORM <- as.data.frame(lapply(x, normalize2))
    Predict<<-neuralnet::compute(nn,testNORM)
    for (i in 1:length(Predict$net.result)) {
      if(is.na(Predict$net.result[i])){
        Predict$net.result[i] <<- 0
      }
    }
  }
  if(!missing(y)){
    if(y == "liftmortagestart"){
      test<-data.frame(matrix(NA, 0, 41))
      test<- rbind(test, c(pos, fort, streetColFreq, houseColFreq, 0, 0, 0, 0, sum(players$fortune[players$id != stratPlayer]), streetColFreqOthers, houseColFreqOthers))
      predictFunc(test)
      return(Predict$net.result)
    }else if(y == "liftmortage"){
      test<-data.frame(matrix(NA, 0, 41))
      hypStreet <- streetColFreq
      hypStreet[which(uniqueC==propCol)] <- hypStreet[which(uniqueC==propCol)] + 1
      test<- rbind(test, c(pos, fort2, hypStreet, houseColFreq, 0, 0, 0, 1, sum(players$fortune[players$id != stratPlayer]), streetColFreqOthers, houseColFreqOthers))
      predictFunc(test)
      return(Predict$net.result)
    }else if(y == "mortage"){
      test<-data.frame(matrix(NA, 0, 41))
      test<- rbind(test, c(pos, fort, streetColFreq, houseColFreq, 0, 0, 0, 0, sum(players$fortune[players$id != stratPlayer]), streetColFreqOthers, houseColFreqOthers))

      countFreq(stratPlayer)
      propsOfCol <- unique(board$color[board$owner == stratPlayer & !(is.na(board$owner)) & board$mortaged != 1])
      if(length(propsOfCol) > 0){
        for (i in 1:length(propsOfCol)) {
          hypStreet <- streetColFreq
          hypStreet[which(uniqueC==propCol)] <- hypStreet[which(uniqueC==propsOfCol[i])] + 1
          
          fort3 <- fort + board$mortageval[board$position == min(board$position[board$color == propsOfCol[i]])]
          test<- rbind(test, c(pos, fort3, hypStreet, houseColFreq, 0, 0, 1, 0, sum(players$fortune[players$id != stratPlayer]), streetColFreqOthers, houseColFreqOthers))
        }
        predictFunc(test)
        propColToMort <- propsOfCol[which(Predict$net.result == max(Predict$net.result))]
        posToMort <- min(board$position[board$color == propColToMort & board$owner == stratPlayer & !(is.na(board$owner)) & board$mortaged != 1])
        if(Predict$net.result[1] == max(Predict$net.result)){
          cat(sprintf("ai did not mortage %s", Predict$net.result))
          return(FALSE)
        }else{
          if(is.na(sum(board$houses[board$color %in% propColToMort])) | sum(board$houses[board$color %in% propColToMort]) == 0){
            if((bankMoney - board$mortageval[board$position == posToMort]) > 0){
              updateBalance(stratPlayer, "pluss", board$mortageval[board$position == posToMort], "Mortage")
              board$mortaged[board$position == posToMort] <<- 1
              return(TRUE)
            }else{
              return(FALSE)
            }
          }else{
            if(bankMoney - (board$housePrice[board$position == posToMort])/2 > 0){
              updateBalance(stratPlayer, "pluss", (board$housePrice[board$position == posToMort])/2, "sold house")
              board$houses[board$position == posToMort] <<- board$houses[board$position == posToMort] - 1
              return(TRUE)
            }else{
              return(FALSE)
            }
          }
          return(TRUE)
        }
      }else{
        return(FALSE)
      }
    }
  }else{
    #nn=neuralnet(win~throws+fortune+white+brown+lblue+purple+orange+red+yellow+green+blue+whitehouses+brownhouses+lbluehouses+purplehouses+orangehouses+redhouses+yellowhouses+greenhouses+bluehouses+buyStreet+buyHouse,data=filteredNN, act.fct = "tanh", linear.output = FALSE, stepmax=1e6, lifesign="full")
    hypStreet <- streetColFreq
    hypStreet[which(uniqueC==propCol)] <- hypStreet[which(uniqueC==propCol)] + 1
    
    test<-data.frame(matrix(NA, 0, 41))
    test<- rbind(test, c(pos, fort, streetColFreq, houseColFreq, 0, 0, 0, 0, sum(players$fortune[players$id != stratPlayer]), streetColFreqOthers, houseColFreqOthers))
    test<- rbind(test, c(pos, fort2, hypStreet, houseColFreq, 1, 0, 0, 0, sum(players$fortune[players$id != stratPlayer]), streetColFreqOthers, houseColFreqOthers))
    
    predictFunc(test)
    
    if(Predict$net.result[1] >= Predict$net.result[2]){
      return(FALSE)
    }else{
      return(TRUE)
    }
  }
}


##-----------------------------------------------------------------------------------
##  Strategy 105: AI-strategy 5
##-----------------------------------------------------------------------------------


strategy105 <- function(x, y){
  if(!missing(x)){
    stratPlayer <<- x
  }else{
    stratPlayer <<- cur_player
  }
  predictFunc <- function(x){
    colnames(x) <- c("iS", "iiS")
    Predict<<-neuralnet::compute(nn2,x)
    for (i in 1:length(Predict$net.result)) {
      if(is.na(Predict$net.result[i])){
        Predict$net.result[i] <<- 0
      }
    }
  }
  test<-data.frame(matrix(NA, 0, 41))
  for (i in 1:11) {
    test<- rbind(test, c(i, players$strategy[players$id != stratPlayer]))
  }
  predictFunc(test)
  
  stratToDo <- max(which(Predict$net.result == max(Predict$net.result)))
  strategyName <- paste("strategy", stratToDo, sep="")
  return(get(strategyName)())
}



##-----------------------------------------------------------------------------------
##  Strategy 106: AI-strategy 6
##-----------------------------------------------------------------------------------

strategy106 <- function(x, y){
  if(!missing(x)){
    stratPlayer <<- x
  }else{
    stratPlayer <<- cur_player
  }
  pos <- players$throws[stratPlayer]
  fort <- players$fortune[stratPlayer]
  countFreq(stratPlayer)
  fort2 <- fort - propPrice
  normalize2 <- function(x){
    return((x - min(logForNN6)) / (max(logForNN6) - min(logForNN6)))
  }
  colnames(logForNN4temp) <<- c("throws", "fortune", as.character(uniqueC), as.character(paste(uniqueC, "houses", sep = '')), "buyStreet", "buyHouse", "mortage", "liftmortage", "fortuneOthers", as.character(paste(uniqueC, "Others", sep = '')), as.character(paste(uniqueC, "housesOthers", sep = '')), "id")
  
  predictFunc <- function(x){
    colnames(x) <- c(as.character(uniqueC), as.character(paste(uniqueC, "houses", sep = '')), "mortagedSelf", "liftMortageSelf" , "mortagedOther", "liftMortageOthers", "fortuneOthers", as.character(paste(uniqueC, "Others", sep = '')), as.character(paste(uniqueC, "housesOthers", sep = '')))
    
    testNORM <- as.data.frame(lapply(x, normalize2))
    Predict<<-neuralnet::compute(nn,testNORM)
    for (i in 1:length(Predict$net.result)) {
      if(is.na(Predict$net.result[i])){
        Predict$net.result[i] <<- 0
      }
    }
  }
  if(!missing(y)){
    if(y == "liftmortagestart"){
      test<-data.frame(matrix(NA, 0, 41))
      test<- rbind(test, c(streetColFreq, houseColFreq,  sum(logForNN4temp$mortage[logForNN4temp$id == stratPlayer]), sum(logForNN4temp$liftmortage[logForNN4temp$id == stratPlayer]), sum(logForNN4temp$mortage[logForNN4temp$id != stratPlayer]),sum(logForNN4temp$liftmortage[logForNN4temp$id != stratPlayer]), sum(players$fortune[players$id != stratPlayer]), streetColFreqOthers, houseColFreqOthers))
      predictFunc(test)
      return(Predict$net.result)
    }else if(y == "liftmortage"){
      test<-data.frame(matrix(NA, 0, 41))
      hypStreet <- streetColFreq
      hypStreet[which(uniqueC==propCol)] <- hypStreet[which(uniqueC==propCol)] + 1
      test<- rbind(test, c(hypStreet, houseColFreq,  sum(logForNN4temp$mortage[logForNN4temp$id == stratPlayer]), sum(logForNN4temp$liftmortage[logForNN4temp$id == stratPlayer]) + 1, sum(logForNN4temp$mortage[logForNN4temp$id != stratPlayer]),sum(logForNN4temp$liftmortage[logForNN4temp$id != stratPlayer]), sum(players$fortune[players$id != stratPlayer]), streetColFreqOthers, houseColFreqOthers))
      predictFunc(test)
      return(Predict$net.result)
    }else if(y == "mortage"){
      test<-data.frame(matrix(NA, 0, 41))
      test<- rbind(test, c(streetColFreq, houseColFreq,  sum(logForNN4temp$mortage[logForNN4temp$id == stratPlayer]), sum(logForNN4temp$liftmortage[logForNN4temp$id == stratPlayer]), sum(logForNN4temp$mortage[logForNN4temp$id != stratPlayer]),sum(logForNN4temp$liftmortage[logForNN4temp$id != stratPlayer]), sum(players$fortune[players$id != stratPlayer]), streetColFreqOthers, houseColFreqOthers))
      countFreq(stratPlayer)
      propsOfCol <- unique(board$color[board$owner == stratPlayer & !(is.na(board$owner)) & board$mortaged != 1])
      if(length(propsOfCol) > 0){
        for (i in 1:length(propsOfCol)) {
          hypStreet <- streetColFreq
          hypStreet[which(uniqueC==propCol)] <- hypStreet[which(uniqueC==propsOfCol[i])] - 1
          
          fort3 <- fort + board$mortageval[board$position == min(board$position[board$color == propsOfCol[i]])]
          test<- rbind(test, c(hypStreet, houseColFreq,  sum(logForNN4temp$mortage[logForNN4temp$id == stratPlayer]) + 1, sum(logForNN4temp$liftmortage[logForNN4temp$id == stratPlayer]), sum(logForNN4temp$mortage[logForNN4temp$id != stratPlayer]),sum(logForNN4temp$liftmortage[logForNN4temp$id != stratPlayer]), sum(players$fortune[players$id != stratPlayer]), streetColFreqOthers, houseColFreqOthers))
        }
        predictFunc(test)
        if(Predict$net.result[1] == max(Predict$net.result)){
          cat(sprintf("ai did not mortage %s", Predict$net.result))
          return(FALSE)
        }else{
          propColToMort <- propsOfCol[which(Predict$net.result == max(Predict$net.result)) - 1]
          posToMort <- min(board$position[board$color == propColToMort & board$owner == stratPlayer & !(is.na(board$owner)) & board$mortaged != 1])
          if(is.na(sum(board$houses[board$color %in% propColToMort])) | sum(board$houses[board$color %in% propColToMort]) == 0){
   
            if((bankMoney - board$mortageval[board$position == posToMort]) > 0){
              updateBalance(stratPlayer, "pluss", board$mortageval[board$position == posToMort], "Mortage")
              board$mortaged[board$position == posToMort] <<- 1
              return(TRUE)
            }else{
              return(FALSE)
            }
          }else{
            if(bankMoney - (board$housePrice[board$position == posToMort])/2 > 0){
              updateBalance(stratPlayer, "pluss", (board$housePrice[board$position == posToMort])/2, "sold house")
              board$houses[board$position == posToMort] <<- board$houses[board$position == posToMort] - 1
              return(TRUE)
            }else{
              return(FALSE)
            }
          }
          return(TRUE)
        }
      }else{
        return(FALSE)
      }
    }
  }else{
    #nn=neuralnet(win~throws+fortune+white+brown+lblue+purple+orange+red+yellow+green+blue+whitehouses+brownhouses+lbluehouses+purplehouses+orangehouses+redhouses+yellowhouses+greenhouses+bluehouses+buyStreet+buyHouse,data=filteredNN, act.fct = "tanh", linear.output = FALSE, stepmax=1e6, lifesign="full")
    hypStreet <- streetColFreq
    hypStreet[which(uniqueC==propCol)] <- hypStreet[which(uniqueC==propCol)] + 1
    
    test<-data.frame(matrix(NA, 0, 41))
    test<- rbind(test, c(streetColFreq, houseColFreq,  sum(logForNN4temp$mortage[logForNN4temp$id == stratPlayer]), sum(logForNN4temp$liftmortage[logForNN4temp$id == stratPlayer]), sum(logForNN4temp$mortage[logForNN4temp$id != stratPlayer]),sum(logForNN4temp$liftmortage[logForNN4temp$id != stratPlayer]), sum(players$fortune[players$id != stratPlayer]), streetColFreqOthers, houseColFreqOthers))
    test<- rbind(test, c(hypStreet, houseColFreq,  sum(logForNN4temp$mortage[logForNN4temp$id == stratPlayer]), sum(logForNN4temp$liftmortage[logForNN4temp$id == stratPlayer]), sum(logForNN4temp$mortage[logForNN4temp$id != stratPlayer]),sum(logForNN4temp$liftmortage[logForNN4temp$id != stratPlayer]), sum(players$fortune[players$id != stratPlayer]), streetColFreqOthers, houseColFreqOthers))
    
    predictFunc(test)
    
    if(Predict$net.result[1] >= Predict$net.result[2]){
      return(FALSE)
    }else{
      return(TRUE)
    }
  }
}


##-----------------------------------------------------------------------------------
##  Strategy 107: AI-strategy 7
##-----------------------------------------------------------------------------------

strategy107 <- function(x, y){
  colnames(logForNN4temp) <<- c("throws", 
                                "fortune", 
                                as.character(uniqueC), 
                                as.character(paste(uniqueC, "houses", sep = '')),
                                "buyStreet", 
                                "buyHouse", 
                                "mortage", 
                                "liftmortage", 
                                "fortuneOthers", 
                                as.character(paste(uniqueC, "Others", sep = '')), 
                                as.character(paste(uniqueC, "housesOthers", sep = '')), 
                                "id")
  if(!missing(x)){
    stratPlayer <<- x
  }else{
    stratPlayer <<- cur_player
  }
  pos <- players$throws[stratPlayer]
  fort <- players$fortune[stratPlayer]
  countFreq(stratPlayer)
  fort2 <- fort - propPrice
  
  streetNames <- as.character(board$name[board$name != "Start" & board$color != "" & board$color != "grey"])
  streetHouses <- paste(streetNames, "Houses")
  streetOther <- paste(streetNames, "Other")
  streetHousesOther <- paste(streetNames, "Other Houses")
  
  test <- data.frame()
  
  predictFunc <- function(x){
    colnames(x) <-  c(#"throws",
                              "fortune", 
                              streetNames, 
                              streetHouses, 
                              streetOther, 
                              streetHousesOther,
                              "mortagedSelf", 
                              "liftMortageSelf", 
                              "mortagedOther", 
                              "liftMortageOthers",  
                              "fortuneOthers")
    n <- colnames(x)
    n <- gsub("&", "", n)
    n <- gsub(" ", ".", n)
    colnames(x) <- n
    x <- x %>%
      replace(., is.na(.), as.integer("0"))
    Predict<<-neuralnet::compute(nn,x)
    for (i in 1:length(Predict$net.result)) {
      if(is.na(Predict$net.result[i])){
        Predict$net.result[i] <<- 0
      }
    }
  }
  
  if(!missing(y)){
    if(y == "liftmortagestart"){
      test <- data.frame()
      test <- rbind(test, c(#players$throws[players$id == 1],
                              players$fortune[players$id == stratPlayer],
                              purchasedLogDF,
                              sum(logForNN4temp$mortage[logForNN4temp$id == stratPlayer]),
                              sum(logForNN4temp$liftmortage[logForNN4temp$id == stratPlayer]),
                              sum(logForNN4temp$mortage[logForNN4temp$id != stratPlayer]),
                              sum(logForNN4temp$liftmortage[logForNN4temp$id != stratPlayer]),
                              sum(players$fortune[players$id != stratPlayer])))
      predictFunc(test)
      return(Predict$net.result)
    }else if(y == "liftmortage"){
      test <- data.frame()
      test <- rbind(test, c(#players$throws[players$id == stratPlayer],
                     players$fortune[players$id == stratPlayer] - board$mortageval[board$position == propPos]*1.1,
                     purchasedLogDF,
                     sum(logForNN4temp$mortage[logForNN4temp$id == stratPlayer]),
                     sum(logForNN4temp$liftmortage[logForNN4temp$id == stratPlayer]) + 1,
                     sum(logForNN4temp$mortage[logForNN4temp$id != stratPlayer]),
                     sum(logForNN4temp$liftmortage[logForNN4temp$id != stratPlayer]),
                     sum(players$fortune[players$id != stratPlayer])))
      predictFunc(test)
      return(Predict$net.result)
    }
  }else{
    test <-data.frame()
    test <- rbind(test, c(players$fortune[players$id == stratPlayer],
                    purchasedLogDF,
                   sum(logForNN4temp$mortage[logForNN4temp$id == stratPlayer]),
                   sum(logForNN4temp$liftmortage[logForNN4temp$id == stratPlayer]),
                   sum(logForNN4temp$mortage[logForNN4temp$id != stratPlayer]),
                   sum(logForNN4temp$liftmortage[logForNN4temp$id != stratPlayer]),
                   sum(players$fortune[players$id != stratPlayer])))
    
    testPurchLog <- purchasedLogDF
    testPurchLog[[propName]] <- 1
    test <- unname(test)
    test <- rbind(test, c(players$fortune[players$id == stratPlayer] - board$price[board$name == propName],
                          testPurchLog,
                           sum(logForNN4temp$mortage[logForNN4temp$id == stratPlayer]),
                           sum(logForNN4temp$liftmortage[logForNN4temp$id == stratPlayer]),
                           sum(logForNN4temp$mortage[logForNN4temp$id != stratPlayer]),
                           sum(logForNN4temp$liftmortage[logForNN4temp$id != stratPlayer]),
                           sum(players$fortune[players$id != stratPlayer])))
    predictFunc(test)

    for (i in 1:length(Predict$net.result)) {
      if(is.na(Predict$net.result[i])){
        Predict$net.result[i] <- 0
      }
    }
    if(Predict$net.result[1] >= Predict$net.result[2]){
      return(FALSE)
    }else{
      return(TRUE)
    }
  }
}





#####################################################################################
#                             AI HOUSE-STRATEGIES                                   #
#####################################################################################


##-----------------------------------------------------------------------------------
##  Strategy H107: House AI-strategy 7
##-----------------------------------------------------------------------------------

strategyH107 <- function(x){
  if(!missing(x)){
    stratPlayer <<- x
  }else{
    stratPlayer <<- cur_player
  }
  streetNames <- as.character(board$name[board$name != "Start" & board$color != "" & board$color != "grey"])
  streetHouses <- paste(streetNames, "Houses")
  streetOther <- paste(streetNames, "Other")
  streetHousesOther <- paste(streetNames, "Other Houses")

  test <- data.frame()
  test <- rbind(test, c(#players$throws[players$id == stratPlayer],
    players$fortune[players$id == stratPlayer],
    purchasedLogDF,
    sum(logForNN4temp$mortage[logForNN4temp$id == stratPlayer]),
    sum(logForNN4temp$liftmortage[logForNN4temp$id == stratPlayer]),
    sum(logForNN4temp$mortage[logForNN4temp$id != stratPlayer]),
    sum(logForNN4temp$liftmortage[logForNN4temp$id != stratPlayer]),
    sum(players$fortune[players$id != stratPlayer])))
  
  streetNames2 <- as.character(board$name[board$name != "Start" & board$color != "" & board$color != "grey" & board$color != "white" & board$name %in% placesToBuy$name])

  for(i in 1:length(streetNames2)){
    testPurchLog <- purchasedLogDF
    testPurchLog <- testPurchLog %>%
      replace(., is.na(.), as.integer("0"))
    if(!is.na(streetNames2[i])){
      testPurchLog[[paste(propName, "Houses")]] <- testPurchLog[[paste(streetNames2[i], "Houses")]]+1
      test <- unname(test)
      test <- rbind(test, c(#players$throws[players$id == stratPlayer],
        players$fortune[players$id == stratPlayer]-board$housePrice[board$name == streetNames2[i]],
        testPurchLog,
        sum(logForNN4temp$mortage[logForNN4temp$id == stratPlayer]),
        sum(logForNN4temp$liftmortage[logForNN4temp$id == stratPlayer]),
        sum(logForNN4temp$mortage[logForNN4temp$id != stratPlayer]),
        sum(logForNN4temp$liftmortage[logForNN4temp$id != stratPlayer]),
        sum(players$fortune[players$id != stratPlayer])))
    }
  }
  
  colnames(test) <-  c(#"throws",
    "fortune", 
    streetNames, 
    streetHouses, 
    streetOther, 
    streetHousesOther,
    "mortagedSelf", 
    "liftMortageSelf", 
    "mortagedOther", 
    "liftMortageOthers",  
    "fortuneOthers")
  n <- colnames(test)
  n <- gsub("&", "", n)
  n <- gsub(" ", ".", n)
  colnames(test) <- n
  test <- test %>%
    replace(is.na(.), 0)
  Predict=neuralnet::compute(nn,test)
  Predict$net.result
  
  for (i in 1:length(Predict$net.result)) {
    if(is.na(Predict$net.result[i])){
      Predict$net.result[i] <- 0
    }
  }
  if(Predict$net.result[1] >= max(Predict$net.result)){
    return(FALSE)
  }else{
    colToBuy <- streetNames2[which(Predict$net.result == max(Predict$net.result))[1] - 1] #minus 1 pga predict har én mer rad enn placestobuy pga ikke kjøp alternativet
    placesToBuyTemp <- placesToBuy %>%
      filter(color == colToBuy)
    return(colToBuy)
  }
}


