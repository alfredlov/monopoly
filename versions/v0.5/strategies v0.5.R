##---------------------------------------------------------
## Monopoly Simulation - Strategies script
## Version 0.2
##---------------------------------------------------------

##--------------------------------------------------------------------------------
## runStrategy: Runs the current player's predefined strategy
##  - If the given player's strategy-function returns TRUE, 
##    set the owner variable of the property to TRUE. 
##--------------------------------------------------------------------------------
gatherStat <- function(x, y){
  if(collectStats == TRUE){
    uniqueC <- c(as.character(unique(board$color[board$color != "" & board$color != "grey"])))
    streetColFreq <<- c()
    streetColFreqOthers <<- c()
    houseColFreq <<- c()
    houseColFreqOthers <<- c()
    for (i in 1:length(uniqueC)) {
      NoCo <- nrow(board[board$color  == uniqueC[i] & board$owner == cur_player & !(is.na(board$owner)),]) 
      streetColFreq <<- c(streetColFreq, NoCo)
      NoCo2 <- nrow(board[board$color  == uniqueC[i] & board$owner != cur_player & board$owner != 0 & !(is.na(board$owner)),]) 
      streetColFreqOthers <<- c(streetColFreqOthers, NoCo2)
      
      sumHouses <- sum(board$houses[(board$owner==cur_player) & !(is.na(board$owner))  & !(is.na(board$houses)) & board$color  == uniqueC[i]])
      houseColFreq <<- c(houseColFreq, sumHouses)
      sumHouses2 <- sum(board$houses[(board$owner!=cur_player) & (board$owner!=0) & !(is.na(board$owner))  & !(is.na(board$houses)) & board$color  == uniqueC[i]])
      houseColFreqOthers <<- c(houseColFreqOthers, sumHouses2)
    }
    wola <- cur_player
    if(x == "house"){
      logForNN4temp <<- rbind(logForNN4temp, c(players$throws[cur_player],players$fortune[cur_player],streetColFreq, houseColFreq, 0, y, sum(players$fortune[players$id != cur_player]),streetColFreqOthers,houseColFreqOthers, wola))
    }else{
      logForNN4temp <<- rbind(logForNN4temp, c(players$throws[cur_player],players$fortune[cur_player],streetColFreq, houseColFreq, y, 0, sum(players$fortune[players$id != cur_player]),streetColFreqOthers,houseColFreqOthers, wola))
    }
    colnames(logForNN4temp) <- length(c("throws", "fortune", as.character(uniqueC), as.character(paste(uniqueC, "houses", sep = '')), "buyStreet", "buyHouse", "fortuneOthers", as.character(paste(uniqueC, "Others", sep = '')), as.character(paste(uniqueC, "housesOthers", sep = '')), "id"))
  }
}

runStrategy <- function(){
  propPrice <<- board$price[players$position[cur_player]]
  ##propPos <<- board$position[players$position[cur_player]]
  strategyName <- paste("strategy", players$strategy[cur_player], sep="")
  if(get(strategyName)() == TRUE){
    #SLETT??
    #cat(sprintf("kjøp %s",Sys.time()))
    position <- players$position[cur_player]
    board$owner[position] <<- cur_player
    players$fortune[cur_player] <<- players$fortune[cur_player] - board$price[position]
    gatherStat("street", 1)
  }else{
    gatherStat("street", 0)
    #####BUDRUNDE
    #BUDRUNDE FUNKER MEN STRATEGIENE ER IKKE SOFISTIKERTE NOK TIL Å HÅNDTERE DET
    #DE BRUKER OFTE MER ENN DE HAR OG TAPER
    #SKRU BUDRUNDER AV/PÅ I initGame()
    if(bid_Active == TRUE){    
      bid_over <- FALSE
      while (bid_over != TRUE) {
        playersBidDf <- players %>%
          filter(active == 1 & fortune > propPrice)
        interested <- rep(0, times=nrow(playersBidDf))
        interestedBuyers <<- data.frame(playersBidDf$id, interested)
        if(nrow(playersBidDf) != 0){
          for (i in 1:nrow(playersBidDf)) {
            strategyName <- paste("strategy", playersBidDf$strategy[i], sep="")
            interestedBuyers$interested[i] <- get(strategyName)()
          }
          if(length(interestedBuyers$interested[interestedBuyers$interested==TRUE]) == 1){
            bidWinner <<- interestedBuyers$playersBidDf.id[interestedBuyers$interested==TRUE]
            bid_over <- TRUE
            position <- players$position[cur_player]
            board$owner[position] <<- bidWinner
            players$fortune[bidWinner] <<- players$fortune[bidWinner] - propPrice
            #cat(sprintf("Player %s won auction of %s for %s",bidWinner, position, propPrice))
          }
          if(propPrice > board$price[players$position[cur_player]]*3){
            if(length(interestedBuyers$interested[interestedBuyers$interested > 0]) == 0){
              bid_over <- TRUE
            }else{
            bidWinner <<- interestedBuyers[sample(nrow(interestedBuyers), 1),]
            bid_over <- TRUE
            position <- players$position[cur_player]
            board$owner[position] <<- bidWinner$playersBidDf.id
            players$fortune[bidWinner$playersBidDf.id] <<- players$fortune[bidWinner$playersBidDf.id] - propPrice
    
            #cat(sprintf("Player %s won auction of %s on random for %s",bidWinner, position, propPrice))
            }
          }else{
            propPrice <<- round(propPrice * 1.1)
          }
        }else{
          bid_over <- TRUE
        }
      }
    }
    #ikke kjøp
    #print("ikke kjøp")
  }
}

runHouseStrategy <- function(){
  #check if player owns all of a color
  uniqueC <- c(as.character(unique(board$color[board$color != "" & board$color != "white" & board$color != "grey"])))
  ownsAll <<- c() #liste over farger hvor cur_player eier alle, gitt av for løkken nedenfor
  for (i in 1:length(uniqueC)) {
    if(checkStreetPer(uniqueC[i], cur_player) == TRUE){
      ownsAll <<- c(ownsAll, i)
    }
  }
  for (i in 1:length(ownsAll)) {
    if(!is.null(ownsAll)){
      if(ownsAll[i] == 1){
        if(top_n(board[board$color == uniqueC[i]], 1, wt = position)$housePrice > players$fortune[cur_player]){
          ownsAll[i] <<- 0
        }
      } 
    }
  }
  
  if(length(ownsAll) > 0){ #hvis en spiller eier alle av en farge/farger
    #print("ALFRED")
      propPrice <<- board$housePrice[board$position == wTB]
      strategyName <- paste("strategy", players$houseStrategy[cur_player], sep="")
      if(get(strategyName)() == TRUE){ #KJØPER BARE HUS OM TRUE FRA STRATEGI
        board$houses[board$position == wTB] <<- board$houses[board$position == wTB] + 1 
        players$fortune[cur_player] <<- players$fortune[cur_player] - board$housePrice[board$position == wTB]
        gatherStat("house", 1)
        #print("KJØPT HUS")
      }else{
        gatherStat("house", 0)
      }
  }
}

######################################################################################
#####  PROPERTY-STRATEGIES ###########################################################
######################################################################################

##-----------------------------------------------------------------------------------
##  Strategy 1: Greedy Naive
##  Simple naïve strategy which involves buying all properties the player lands on. 
##-----------------------------------------------------------------------------------
strategy1 <- function(x){
  return(TRUE)
}

##-----------------------------------------------------------------------------------
##  Strategy 2: Probabilistic greedy naive
##  Simple strategy of buying all properties the player lands on with probability 0.5.
##-----------------------------------------------------------------------------------
strategy2 <- function(x){
  if(sample(0:1, prob = c(0.5, 0.5), 1) == 1){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

##-----------------------------------------------------------------------------------
##  Strategy 3: Simple conservative
##  Buys all properties as long as price < 50% of total income.
##-----------------------------------------------------------------------------------
strategy3 <- function(x){
  if(propPrice/players$fortune[cur_player] <= 0.5){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

##-----------------------------------------------------------------------------------
##  Strategy 4: Middle of the road
##  Only buys regular properties on the 2nd and 3rd part of the board. 
## These are either purple, orange, red, orange...
##-----------------------------------------------------------------------------------
strategy4 <- function(x){
  
  ##FORENKLE??
  if(board$color[players$position[cur_player]] == 'purple' || board$color[players$position[cur_player]] == 'orange' || 
     board$color[players$position[cur_player]] == 'red' || board$color[players$position[cur_player]] == 'yellow'){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

##-----------------------------------------------------------------------------------
##  Strategy 5: Red & Orange
##-----------------------------------------------------------------------------------
strategy5 <- function(x){
  
  ##FORENKLE??
  if(board$color[players$position[cur_player]] == 'orange' || board$color[players$position[cur_player]] == 'red'){
    return(TRUE)
  }else{
    return(FALSE)
  }
}


##-----------------------------------------------------------------------------------
##  Strategy 6: Railroads
##-----------------------------------------------------------------------------------
strategy6 <- function(x){
  if(board$prop[players$position[cur_player]] == 3){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

##-----------------------------------------------------------------------------------
##  Strategy 7: Utilities
##-----------------------------------------------------------------------------------
strategy7 <- function(x){
  if(board$prop[players$position[cur_player]] == 2){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

##-----------------------------------------------------------------------------------
##  Strategy 8: Railroads + Utilities
##-----------------------------------------------------------------------------------
strategy8 <- function(x){
  if(board$prop[players$position[cur_player]] == 2 | board$prop[players$position[cur_player]] == 3){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

##-----------------------------------------------------------------------------------
##  Strategy 9: Railroads + Utilities, then Conservative
##-----------------------------------------------------------------------------------
strategy9 <- function(x){
  if(board$prop[players$position[cur_player]] == 2 | board$prop[players$position[cur_player]] == 3){
    return(TRUE)
  }else{
    if(sum(board$owner[board$prop==2 | board$prop==3] > 0) >=5){
      return(TRUE)
    }
    else{
      return(FALSE)
    }
  }
}


##-----------------------------------------------------------------------------------
##  Strategy 10: 
##-----------------------------------------------------------------------------------

######################################################################################
#####  HOUSE-STRATEGIES #############################################################
######################################################################################


##-----------------------------------------------------------------------------------
##  Strategy H1: Aggressive
##-----------------------------------------------------------------------------------

strategyH1 <- function(){
  
  
}
  

##-----------------------------------------------------------------------------------
##  Strategy H2: Moderate
##-----------------------------------------------------------------------------------

strategyH2 <- function(){
  
  
}

##-----------------------------------------------------------------------------------
##  Strategy H3: Timid
##-----------------------------------------------------------------------------------

strategyH3 <- function(){
  
  
}

##-----------------------------------------------------------------------------------
##  Strategy HALFRED: Timid
##-----------------------------------------------------------------------------------

strategyHALFRED <- function(){
  if(length(ownsAll) == 1){
    housesInCol <- board$houses[board$color == uniqueC[ownsAll[1]]]
    sQuery <- board$position[board$color == uniqueC[ownsAll[1]] & board$houses == min(housesInCol) & board$houses < 5]
    if(length(sQuery) != 0){
      wTB <- max(sQuery)
      
    }
  }
 
  
  # if(length(ownsAll) > 1){ #hvis en spiller eier alle av en farge/farger
  #   colFocus <- sample(1:length(ownsAll), 1)
  #   housesInCol <- board$houses[board$color == uniqueC[ownsAll[colFocus]]]
  #   sQuery <- board$position[board$color == uniqueC[ownsAll[colFocus]] & board$houses == min(housesInCol) & board$houses < 5]
  #   if(length(sQuery) != 0){
  #     wTB <- max(sQuery)
  #     if(players$fortune[cur_player] - board$housePrice[board$position == wTB] > 0){
  #       propPrice <<- board$housePrice[board$position == wTB]
  #       strategyName <- paste("strategy", players$houseStrategy[cur_player], sep="")
  #       if(get(strategyName)() == TRUE){
  #         board$houses[board$position == wTB] <<- board$houses[board$position == wTB] + 1 
  #         players$fortune[cur_player] <<- players$fortune[cur_player] - board$housePrice[board$position == wTB]
  #         gatherStat("house", 1)
  #         #print("KJØPT HUS")
  #       }else{
  #         gatherStat("house", 0)
  #       }    
  #     }
  #   }
  # }
}

######################################################################################
##  AI-Strategies
######################################################################################


##-----------------------------------------------------------------------------------
##  Strategy 100
##-----------------------------------------------------------------------------------
#strategy100()
#ai-strategi
mround <- function(x,base){
  base*round(x/base)
}
strategy100 <- function(x){
  #cur_player <- 2
  if(length(fortune) > 5){
    aprox.position <- mround(length(fortune[cur_player,])/5, 5)
  }else{
    prox.position <- 5
  }
  #scenario 1 - ikke kjøp
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
      #cat(sprintf("predrict %s %s", Predict$net.result[1], Predict$net.result[2]))
      return(FALSE)
    }else{
      #cat(sprintf("predrict %s %s", Predict$net.result[1], Predict$net.result[2]))
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

strategy101 <- function(x){
  #scenario 1 - ikke kjøp
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
      #cat(sprintf("predrict %s %s", Predict$net.result[1], Predict$net.result[2]))
      return(FALSE)
    }else{
      #cat(sprintf("predrict %s %s", Predict$net.result[1], Predict$net.result[2]))
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
strategy102 <- function(x){
  #scenario 1 - ikke kjøp
  uniqueC <- unique(board$color[board$color != "" & board$color != "white" & board$color != "grey"])
  streetColFreq <<- c()
  houseColFreq <<- c()
  for (i in 1:length(uniqueC)) {
    NoC <- nrow(board[board$color  == uniqueC[i],]) #hvor mange gater i den fargen
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
      #cat(sprintf("predrict %s %s", Predict$net.result[1], Predict$net.result[2]))
      return(FALSE)
    }else{
      #cat(sprintf("predrict %s %s", Predict$net.result[1], Predict$net.result[2]))
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
strategy103 <- function(x){
  #scenario 1 - ikke kjøp
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
  # filteredNN <- logForNN4 %>%
  #   filter(throws == pos)
  #nn=neuralnet(win~throws+fortune+white+brown+lblue+purple+orange+red+yellow+green+blue+whitehouses+brownhouses+lbluehouses+purplehouses+orangehouses+redhouses+yellowhouses+greenhouses+bluehouses+buyStreet+buyHouse,data=filteredNN, act.fct = "tanh", linear.output = FALSE, stepmax=1e6, lifesign="full")
  
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
      #cat(sprintf("predrict %s %s", Predict$net.result[1], Predict$net.result[2]))
      return(FALSE)
    }else{
      #cat(sprintf("predrict %s %s", Predict$net.result[1], Predict$net.result[2]))
      return(TRUE)
    }
  }else{
    hypStreet <<- streetColFreq
    hypStreet[which(uniqueC==board$color[players$position[cur_player]])] <<- hypStreet[which(uniqueC==board$color[players$position[cur_player]])] + 1
    fort2 <- fort - propPrice
    
    
    test=data.frame()
    test<- rbind(test, c(pos, fort, streetColFreq, houseColFreq, 0, 0, sum(players$fortune[players$id != cur_player]), streetColFreqOthers, houseColFreqOthers))
    test<- rbind(test, c(pos, fort2, hypStreet, houseColFreq, 1, 0, sum(players$fortune[players$id != cur_player]), streetColFreqOthers, houseColFreqOthers))
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

