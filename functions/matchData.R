function(matchesresults, selTourn, players, matchRows, bookies){
  if(is.null(matchRows)) return(as.data.frame(NULL))
  
  if(matchRows>nrow(matchesresults)) return(as.data.frame(NULL))
    
  data<-previousValues(matchesresults,
                       players,
                       bookies,
                       matchRows)
  
  
  dataT<-as.data.frame(data[c(29,22:28)])
  Probs<-fuzzyLogic(players[which(players$MatchId==as.character(dataT$MatchId)),])$FuzzyProb
  
  #played matches
  if(dataT$Status==1){
    dataT$GameStatus<-'Played'
    if(dataT$WonPlayer1==1) {
      dataT$Winner <- data[[1]]
      dataT$GamesDif<- dataT$DifGamesPlayer1
      dataT$BookieProb<-round(dataT$BookieProbP1,2)
      dataT$FuzzyProb<-round(100*Probs[1],2)
    }
    if(dataT$WonPlayer1!=1) {
      dataT$Winner <- data[[12]]
      dataT$GamesDif<- dataT$DifGamesPlayer1*(-1)
      dataT$BookieProb<-round(100-dataT$BookieProbP1,2)
      dataT$FuzzyProb<-round(100*Probs[2],2)
    }
    return(dataT[,c('Tournament', 'Date', 'Winner', 'NSets', 'GamesDif', 'FuzzyProb', 'BookieProb')])
  }
  
  #abandoned matches
  if(dataT$Status==3){
    dataT$GameStatus<-'Abandoned'
    if(dataT$WonPlayer1==1) {
      dataT$Winner <- data[[1]]
      dataT$BookieProb<-round(dataT$BookieProbP1,2)
      dataT$FuzzyProb<-round(100*Probs[1],2)
    }
    if(dataT$WonPlayer1!=1) {
      dataT$Winner <- data[[12]]
      dataT$BookieProb<-round(100-dataT$BookieProbP1,2)
      dataT$FuzzyProb<-round(100*Probs[2],2)
    }
    return(dataT[,c('Tournament', 'Date', 'Winner', 'GameStatus', 'FuzzyProb', 'BookieProb')])
  }
  
  #scheduled matches
  if(dataT$Status==2){
    dataT$GameStatus<-'Scheduled'

    dataT$BookieProbP1<-round(dataT$BookieProbP1,2)
    dataT$Fuzzy1ProbP1<-round(100*Probs[1],2)
    
    dataT$BookieProbP2<-round(100-dataT$BookieProbP1,2)
    dataT$Fuzzy1ProbP2<-round(100*Probs[2],2)
    
    return(dataT[,c('Tournament', 'Date', 'GameStatus', 'Fuzzy1ProbP1', 'BookieProbP1',
                    'Fuzzy1ProbP2','BookieProbP2')])
  }
  
  

}