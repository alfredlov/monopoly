##########################################################################
#### AI STUFF  ##########################################################
##########################################################################

#require(neuralnet)
#nn strategi 100, ser på sammenheng mellom seier og [posisjon, saldo, ant. hus, ant eiendommer]
#nn=neuralnet(X1~X5+X1500+X0+X0.1,data=logForNN, hidden=0,act.fct = "logistic", linear.output = FALSE, stepmax=1e6)
#nn strategi 101, ser på sammenheng mellom seier og [ant. hus, ant eiendommer]
#nn=neuralnet(X1~X3+X0,data=logForNN2, hidden=1,act.fct = "logistic", linear.output = FALSE, stepmax=1e6)
#nn strategi 102, ser på sammenheng mellom seier og [ant. hus pr farge, ant eiendommer pr farge]
#nn=neuralnet(win~brown+lblue+purple+orange+red+yellow+green+blue+brownhouses+lbluehouses+purplehouses+orangehouses+redhouses+yellowhouses+greenhouses+bluehouses,data=logForNN3, hidden=c(10,10, 10, 10, 10),act.fct = "logistic", linear.output = FALSE, stepmax=1e6)
#nn strategi 103, ser på sammenheng mellom seier og [ant. hus pr farge, ant eiendommer pr farge]
#replica_NN <- top_n(logForNN4, 50000, throws)

#nn=neuralnet(win~throws+fortune+white+brown+lblue+purple+orange+red+yellow+green+blue+whitehouses+brownhouses+lbluehouses+purplehouses+orangehouses+redhouses+yellowhouses+greenhouses+bluehouses+buyStreet+buyHouse,data=logForNN4, hidden=c(2),act.fct = "logistic", linear.output = FALSE, stepmax=1e6, lifesign="full")
#nn=neuralnet(win~throws+fortune+white+brown+lblue+purple+orange+red+yellow+green+blue+whitehouses+brownhouses+lbluehouses+purplehouses+orangehouses+redhouses+yellowhouses+greenhouses+bluehouses+buyStreet+buyHouse,data=replica_NN, hidden=c(1),act.fct = "tanh", linear.output = FALSE, stepmax=1e6, lifesign="full")
#nn=neuralnet(win~throws+fortune+white+brown+lblue+purple+orange+red+yellow+green+blue+whitehouses+brownhouses+lbluehouses+purplehouses+orangehouses+redhouses+yellowhouses+greenhouses+bluehouses+buyStreet+buyHouse+fortuneOthers+whiteOthers+brownOthers+lblueOthers+purpleOthers+orangeOthers+redOthers+yellowOthers+greenOthers+blueOthers+whitehousesOthers+brownhousesOthers+lbluehousesOthers+purplehousesOthers+orangehousesOthers+redhousesOthers+yellowhousesOthers+greenhousesOthers+bluehousesOthers,data=logForNN4, hidden=c(1),act.fct = "tanh", linear.output = FALSE, stepmax=1e6, lifesign="full")
#normalized
#nn=neuralnet(win~throws+fortune+white+brown+lblue+purple+orange+red+yellow+green+blue+brownhouses+lbluehouses+purplehouses+orangehouses+redhouses+yellowhouses+greenhouses+bluehouses+buyStreet+buyHouse+mortage+liftmortage+fortuneOthers+whiteOthers+brownOthers+lblueOthers+purpleOthers+orangeOthers+redOthers+yellowOthers+greenOthers+blueOthers+brownhousesOthers+lbluehousesOthers+purplehousesOthers+orangehousesOthers+redhousesOthers+yellowhousesOthers+greenhousesOthers+bluehousesOthers,data=logForNN4NORM, hidden=c(2,1), linear.output = FALSE, stepmax=1e6, lifesign="full")


#gwplot(x=nn, min=0)
#plot(nn)

################################################################
#############           START NN DATA             ###############
################################################################

#write.csv(logForNN4,'logForNN4.csv')
#logForNN <<- data.frame(matrix(NA, 0, 5))
#logForNN2 <<- data.frame(matrix(NA, 0, 5))
#logForNN3 <<- data.frame(matrix(NA, 0, 17))

#logForNN4 <<- data.frame(matrix(NA, 0, 45))
#logForNN5 <<- data.frame(matrix(NA, 0, 3))
#logForNN6 <<- data.frame(matrix(NA, 0, 44))


if(enableAiData == "Talfred"){

  #nn=neuralnet(win~throws+fortune+white+brown+lblue+purple+orange+red+yellow+green+blue+brownhouses+lbluehouses+purplehouses+orangehouses+redhouses+yellowhouses+greenhouses+bluehouses+buyStreet+buyHouse+mortage+liftmortage+fortuneOthers+whiteOthers+brownOthers+lblueOthers+purpleOthers+orangeOthers+redOthers+yellowOthers+greenOthers+blueOthers+brownhousesOthers+lbluehousesOthers+purplehousesOthers+orangehousesOthers+redhousesOthers+yellowhousesOthers+greenhousesOthers+bluehousesOthers,data=logForNN4NORM, 

  # nn=neuralnet(win~paste(board$name[board$name != "Start" & board$color != "" & board$color != "grey"],collapse="+")
  #              +paste(paste(board$name[board$name != "Start" & board$color != "" & board$color != "grey"], "Houses"),collapse="+")
  #              +mortagedSelf+liftMortageSelf+mortagedOther+liftMortageOthers
  #              +fortuneOthers
  #              +paste(paste(board$name[board$name != "Start" & board$color != "" & board$color != "grey"], "Others"),collapse="+")
  #              +paste(paste(board$name[board$name != "Start" & board$color != "" & board$color != "grey"], "Houses Others"),collapse="+"),
  #              data=logForNN7NORM, 
  #              hidden=c(2,2,2), 
  #              #act.fct = "logistic", 
  #              linear.output = FALSE, 
  #              stepmax=1e6, 
  #              lifesign="full", 
  #              #startweights = nn[["startweights"]],
  #              threshold=0.01)
  nn=neuralnet(win~white+brown+lblue+purple+orange+red+yellow+green+blue+brownhouses+lbluehouses+purplehouses+orangehouses+redhouses+yellowhouses+greenhouses+bluehouses+mortagedSelf+liftMortageSelf+mortagedOther+liftMortageOthers+fortuneOthers+whiteOthers+brownOthers+lblueOthers+purpleOthers+orangeOthers+redOthers+yellowOthers+greenOthers+blueOthers+brownhousesOthers+lbluehousesOthers+purplehousesOthers+orangehousesOthers+redhousesOthers+yellowhousesOthers+greenhousesOthers+bluehousesOthers,
               data=logForNN6NORM, 
               hidden=c(2), 
               #act.fct = "logistic", 
               linear.output = FALSE, 
               stepmax=1e6, 
               lifesign="full", 
               #startweights = nn[["startweights"]],
               threshold=0.01)
   replicate(200, buildDataBaseforNN())
  replicate(20, buildDataBaseforNN2())
  replicate(200, buildDataBaseforNN3())
  
  replicate(10, buildDataBaseforNN4())
  
  replicate(200, buildDataBaseforNN7())
  #normalize data for optimized learning 
  normalize <- function(x){
    return((x - min(x)) / (max(x) - min(x)))
  }
  logForNN4 <<- logForNN4 %>%
    mutate(win = ifelse(win == 1, 1, 0))
  
  logForNN6NORM <<- as.data.frame(lapply(logForNN6, normalize))
  logForNN6NORM <<- logForNN6NORM %>%
    replace(., is.na(.), as.integer("0"))
  
  hist(logForNN4$id[logForNN4$win == 1])
  
  buildDataBaseforNN <- function(){
    startGame()
    for(y in 1:N){
      #cat(sprintf("N: %s \n", y))
      bol <- as.numeric(fortune[y,][seq(1, length(fortune[y,]), 5)])
      dola <- as.numeric(nProps[y,][seq(1, length(nProps[y,]), 5)])
      aola <- as.numeric(nHouses[y,][seq(1, length(nHouses[y,]), 5)])
      cola <- c()
      wola <- c(rep(players$active[y], times=length(bol)))
      #cat(sprintf("length bol: %s \n", length(bol)))
      for(j in 1:length(bol)){
        #cat(sprintf("j: %s \n", j*5))
        logForNN <<- rbind(logForNN, c(j*5, bol[j], dola[j], aola[j], wola[j]))
      }
      #logForNN <<- matrix(cola, bol, dola, wola, ncol = 5, nrow = length(bol))
    }
    colnames(logForNN) <- c("throws", "saldo", "streets", "houses", "win")
  }
  
  buildDataBaseforNN2 <- function(){
    startGame()
    for(y in 1:N){
      #cat(sprintf("N: %s \n", y))
      #bol <- as.numeric(fortune[y,][seq(1, length(fortune[y,]), 5)])
      dola <- max(nProps[y,][seq(1, length(nProps[y,]), 5)])
      aola <- max(nHouses[y,][seq(1, length(nHouses[y,]), 5)])
      wola <- c(players$active[y])
      
      logForNN2 <<- rbind(logForNN2, c(dola, aola, wola))
      #logForNN <<- matrix(cola, bol, dola, wola, ncol = 5, nrow = length(bol))
    }
    #colnames(logForNN2) <- c("throws", "saldo", "streets", "houses", "win")
  }
  buildDataBaseforNN3 <- function(){
    startGame()
    for(y in 1:N){
      
      uniqueC <<- unique(board$color[board$color != "" & board$color != "white" & board$color != "grey"])
      streetColFreq <<- c()
      houseColFreq <<- c()
      for (i in 1:length(uniqueC)) {
        NoC <- nrow(board2[board$color  == uniqueC[i],]) #hvor mange gater i den fargen
        NoCo <- nrow(board2[board$color  == uniqueC[i] & board2$owner == y,]) 
        streetColFreq <<- c(streetColFreq, NoCo)
        
        sumHouses <- sum(board2$houses[(board2$owner==y) & !(is.na(board2$owner))  & !(is.na(board2$houses)) & board2$color  == uniqueC[i]])
        houseColFreq <<- c(houseColFreq, sumHouses)
      }
      wola <- c(players$active[y])
      
      logForNN3 <<- rbind(logForNN3, c(streetColFreq, houseColFreq, wola))
    }
    colnames(logForNN3) <- c(as.character(uniqueC), as.character(paste(uniqueC, "houses", sep = '')), "win")
  }
  
  buildDataBaseforNN4 <- function(){
    startGame()
    uniqueC <- unique(board$color[board$color != "" & board$color != "grey"])
    colnames(logForNN4temp) <<- c("throws", "fortune", as.character(uniqueC), as.character(paste(uniqueC, "houses", sep = '')), "buyStreet", "buyHouse", "fortuneOthers", as.character(paste(uniqueC, "Others", sep = '')), as.character(paste(uniqueC, "housesOthers", sep = '')), "id")
    
    if(length(logForNN4temp[,1]) != 0){
      logForNN4temp <<- logForNN4temp %>%
        mutate(win = ifelse(id == winner, 1, -1))
    }
    colnames(logForNN4) <- c("throws", "fortune", as.character(uniqueC), as.character(paste(uniqueC, "houses", sep = '')), "buyStreet", "buyHouse", "fortuneOthers", as.character(paste(uniqueC, "Others", sep = '')), as.character(paste(uniqueC, "housesOthers", sep = '')), "id", "win")
    
    logForNN4 <<- rbind(logForNN4, logForNN4temp)
    
  }
  
  
  buildDataBaseforNN5 <- function(){
    startGame()
    uniqueC <- unique(board$color[board$color != "" & board$color != "grey"])
    colnames(logForNN4temp) <- c("throws", "fortune", as.character(uniqueC), as.character(paste(uniqueC, "houses", sep = '')), "buyStreet", "buyHouse", "mortage", "liftmortage", "fortuneOthers", as.character(paste(uniqueC, "Others", sep = '')), as.character(paste(uniqueC, "housesOthers", sep = '')), "id")
    if(length(logForNN4temp[,1]) != 0){
      logForNN4temp <- logForNN4temp %>%
        mutate(win = ifelse(id == roundWinner, 1, 0))
    }
    colnames(logForNN4) <- c("throws", "fortune", as.character(uniqueC), as.character(paste(uniqueC, "houses", sep = '')), "buyStreet", "buyHouse", "mortage", "liftmortage", "fortuneOthers", as.character(paste(uniqueC, "Others", sep = '')), as.character(paste(uniqueC, "housesOthers", sep = '')), "id", "win")
    
    logForNN4 <<- rbind(logForNN4, logForNN4temp)
    logForNN5temp <<- rbind(logForNN5temp, c(players$strategy))
    buildDataBaseforNN6()
  }
  buildDataBaseforNN6 <- function(){
    startGame()
    logForNN5temp <- rbind(logForNN5temp, c(players$strategy))
    colnames(logForNN5temp) <- c("iS", "iiS")
    if(length(logForNN5temp[,1]) != 0){
      logForNN5temp <- logForNN5temp %>%
        mutate(win = ifelse(iS == winnerStrategy, 1, 0))
    }
    colnames(logForNN5) <- c("iS", "iiS", "win")
    logForNN5 <<- rbind(logForNN5, logForNN5temp)
  }
  buildDataBaseforNN7 <- function(){
    startGame()
    uniqueC <- unique(board$color[board$color != "" & board$color != "grey"])
    colnames(logForNN6temp) <- c("throws", "fortune", as.character(uniqueC), as.character(paste(uniqueC, "houses", sep = '')), "mortagedSelf", "liftMortageSelf" , "mortagedOther", "liftMortageOthers",  "fortuneOthers", as.character(paste(uniqueC, "Others", sep = '')), as.character(paste(uniqueC, "housesOthers", sep = '')), "win")
    colnames(logForNN6) <- c("throws", "fortune", as.character(uniqueC), as.character(paste(uniqueC, "houses", sep = '')), "mortagedSelf", "liftMortageSelf" , "mortagedOther", "liftMortageOthers", "fortuneOthers", as.character(paste(uniqueC, "Others", sep = '')), as.character(paste(uniqueC, "housesOthers", sep = '')), "win")
    logForNN6 <<- rbind(logForNN6, logForNN6temp)
  }
}
################################################################
#############          SLUTT NN DATA             ###############
################################################################
