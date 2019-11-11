######107
####Måler hvor lenge en spiller har eid en eiendom ut fra når i spillet den ble kjøp (målt i terningkast)
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
      #print("AI KJØPTE IKKKKKE GATE")
      return(FALSE)
    }else{
      #print("AI KJØPTE GATE")
      return(TRUE)
    }
  }
}

######################################################################################
##  AI - House-Strategy
######################################################################################
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
  #ikke kjøpe hus 
  test <- rbind(test, c(#players$throws[players$id == stratPlayer],
    players$fortune[players$id == stratPlayer],
    purchasedLogDF,
    sum(logForNN4temp$mortage[logForNN4temp$id == stratPlayer]),
    sum(logForNN4temp$liftmortage[logForNN4temp$id == stratPlayer]),
    sum(logForNN4temp$mortage[logForNN4temp$id != stratPlayer]),
    sum(logForNN4temp$liftmortage[logForNN4temp$id != stratPlayer]),
    sum(players$fortune[players$id != stratPlayer])))
  
  streetNames2 <- as.character(board$name[board$name != "Start" & board$color != "" & board$color != "grey" & board$color != "white" & board$name %in% placesToBuy$name])
  #for hver gate
  for(i in 1:length(streetNames2)){
    testPurchLog <- purchasedLogDF
    testPurchLog <- testPurchLog %>%
      replace(., is.na(.), as.integer("0"))
    if(!is.na(streetNames2[i])){
      #print("notna")
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
  
  #i tilfelle noen gir NA -> NA satt til 0
  for (i in 1:length(Predict$net.result)) {
    if(is.na(Predict$net.result[i])){
      Predict$net.result[i] <- 0
    }
  }
  #velger farge eller ikke kjøp
  if(Predict$net.result[1] >= max(Predict$net.result)){
    #ikke kjøp
    #print("AI KJØPTE IKKKKKE HUS")
    #cat(sprintf("%s", Predict$net.result))
    return(FALSE)
  }else{
    #print("AI KJØPTE HUS")
    colToBuy <- streetNames2[which(Predict$net.result == max(Predict$net.result))[1] - 1] #minus 1 pga predict har én mer rad enn placestobuy pga ikke kjøp alternativet
    placesToBuyTemp <- placesToBuy %>%
      filter(color == colToBuy)
    return(colToBuy) #kjøper hus på den eiendomen lengst ut ut i brettet av fargen den har valgt
  }
}

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
    #Predict$net.result
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

