##--------------------------------------------------------------------------------##
## Monopoly Simulation - AI-Training script                                       ##
## Version 0.5                                                                    ##
##--------------------------------------------------------------------------------##


require(neuralnet)
gatherStat <- function(x, y, z){                         # Collects game stats for use in AI-agent. 
  if(enableAiData == TRUE){
    countFreq(cur_player)
    wola <- cur_player
    if(x == "house"){
      logForNN4temp <<- rbind(logForNN4temp, 
                              c(players$throws[cur_player],
                                players$fortune[cur_player],
                                streetColFreq, 
                                houseColFreq, 
                                0, y, 0,0, 
                                sum(players$fortune[players$id != cur_player]),
                                streetColFreqOthers,
                                houseColFreqOthers, wola))
      
      nameOfPurchased <- z
      whoPurchased <- cur_player
      if(whoPurchased == 1){
        purchasedLogDF[[paste(nameOfPurchased, "Houses")]] <<- purchasedLogDF[[paste(nameOfPurchased, "Houses")]] + 1
      }else{
        purchasedLogDF[[paste(nameOfPurchased, "Other Houses")]] <<- purchasedLogDF[[paste(nameOfPurchased, "Other Houses")]] + 1
      }
      
    }else if (x == "pantsatt"){
      logForNN4temp <<- rbind(logForNN4temp, 
                              c(players$throws[cur_player],
                                players$fortune[cur_player],
                                streetColFreq, houseColFreq, 
                                0, 0, y,0, 
                                sum(players$fortune[players$id != cur_player]),
                                streetColFreqOthers,
                                houseColFreqOthers, 
                                wola))
    }else if (x == "unpantsatt"){
      logForNN4temp <<- rbind(logForNN4temp, 
                              c(players$throws[cur_player],
                                players$fortune[cur_player],
                                streetColFreq, 
                                houseColFreq, 
                                0, 0, 0, y, 
                                sum(players$fortune[players$id != cur_player]),
                                streetColFreqOthers,
                                houseColFreqOthers, 
                                wola))
    }else{
      nameOfPurchased <- z
      whoPurchased <- board$owner[board$name == nameOfPurchased]
      if(whoPurchased == 1){
        purchasedLogDF[[nameOfPurchased]] <<- 1
        #cat(sprintf("player %s at throw %s purchased %s \n", whoPurchased, players$throws[1], nameOfPurchased))
      }else{
        purchasedLogDF[[paste(nameOfPurchased, "Other")]] <<- 1
      }
      
      logForNN4temp <<- rbind(logForNN4temp, 
                              c(players$throws[cur_player],
                                players$fortune[cur_player],
                                streetColFreq, 
                                houseColFreq, 
                                y, 0, 0,0, 
                                sum(players$fortune[players$id != cur_player]),
                                streetColFreqOthers,
                                houseColFreqOthers, 
                                wola))
    }
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
  }
}

initAI <- function(){
  logForNN7 <- read_csv("logForNN7.csv") #either import data or build using buildDataBaseforNN8 function
  
  scale2 <- function(x, na.rm = FALSE) (ifelse(x > 0, 1, 0))
  
  logForNN7 <<- logForNN7 %>%
    replace(., is.na(.), as.integer("0"))
  
  logForNN72 <- logForNN7
  
  n <- colnames(logForNN72)
  n <- gsub("&", "", n)
  n <- gsub(" ", ".", n)
  colnames(logForNN72) <- n
  
  logForNN72 <- logForNN72 %>%
    mutate_at(n[!n %in% c("win", "fortune", "throws", "mortagedSelf", "liftMortageSelf", "liftMortageOthers", "fortuneOthers")], scale2) #change values for properties from throws bought @ to if bought or not
  
  a<-paste("win ~", paste(n[!n %in% c("win"
                                      #,"fortune"
                                      #,"throws"
                                      )], collapse = " + "))
  f <- as.formula(a)
  
  nn=neuralnet(f,data=logForNN72,hidden=c(117, 117, 117, 117, 117),
               linear.output = FALSE,
               stepmax=1e6,
               lifesign="full",
               threshold=1e-19)
  #write.csv(logForNN72,'logForNN72.csv')
  # plot(nn, rep = NULL, x.entry = NULL, x.out = NULL,
  #      radius = 0.15, arrow.length = 0.05, intercept = FALSE,
  #      intercept.factor = 100, information = TRUE, information.pos = 0.1,
  #      col.entry.synapse = "black", col.entry = "black",
  #      col.hidden = "black", col.hidden.synapse = "black",
  #      col.out = "black", col.out.synapse = "black",
  #      col.intercept = "blue", fontsize = 12, dimension = 20,
  #      show.weights = FALSE, file = NULL)
}

if(enableAiData == "TRUuE"){
  streetNames <- as.character(board$name[board$name != "Start" & board$color != "" & board$color != "grey"])
  streetHouses <- paste(streetNames, "Houses")
  streetOther <- paste(streetNames, "Other")
  streetHousesOther <- paste(streetNames, "Other Houses")

  replicate(5000, buildDataBaseforNN8()) 
  
  buildDataBaseforNN8 <- function(){
    startGame()
    colnames(logForNN7) <-  c("throws",
                              "fortune", 
                              streetNames, 
                              streetHouses, 
                              streetOther, 
                              streetHousesOther,
                              "mortagedSelf", 
                              "liftMortageSelf", 
                              "mortagedOther", 
                              "liftMortageOthers",  
                              "fortuneOthers", 
                              "win")
    logForNN7 <<- rbind(logForNN7, logForNN7temp)
  }
}
