function(matchesresults,players,bookies,rowNum){
  if(is.null(rowNum)){
    return(rep(NULL,9))
  }
  
  if(rowNum>nrow(matchesresults)) return(rep(NULL,9))
  

    book<-bookies[which(bookies$MatchId == matchesresults$MatchId[rowNum]),]
    playData<-players[which(players$MatchId == matchesresults$MatchId[rowNum]),]
    matchData<-matchesresults[rowNum,]
    if(matchData$Status=='Played') matchData$Status<-1
    if(matchData$Status=='Scheduled') matchData$Status<-2
    if(matchData$Status=='Abandoned') matchData$Status<-3
    
    #quines columnes retorno i en quin ordre
    colsp1<-c("Player","Technique","Physique","Psicology","Regularity","Experience",
              "Implication","Surface","Rest","Confidence","H2H")
    colsp2<-c("Player","Technique","Physique","Psicology","Regularity","Experience",
              "Implication","Surface","Rest","Confidence")
    colsmatch<-c("Tournament","Date","BookieProbP1","WonPlayer1",
                 "NSets","DifGamesPlayer1","Status","MatchId")
    colsbook<-c("OddsP1","OddsP2","HAP_Highest","HAP1_1","HAP1_2","HAP1_3",
                "HAP1_4","HAP1_5","HAP1_6","HAP1_7","HAP2_1","HAP2_2","HAP2_3",
                "HAP2_4","HAP2_5","HAP2_6","HAP2_7")
    
    playData<-as.list(c(playData[1,colsp1],playData[2,colsp2],matchData[1,colsmatch],book[1,colsbook]))
    
    return(playData)
  
}