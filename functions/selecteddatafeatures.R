function(matchesresults,players_data,matches_row){
  
  players_data$Player<-as.character(players_data$Player)
  matchesresults$Player1<-as.character(matchesresults$Player1)
  matchesresults$Player2<-as.character(matchesresults$Player2)
  
  if(is.null(matches_row)){
    ret<-as.data.frame(players_data[1:2,-c(1:2)])
    ret[1:2,]<-NA
    return(ret)
  }
  
  if(!is.null(matches_row)){
    w1<-which(players_data$Player == matchesresults$Player1[matches_row] &
              players_data$MatchId == matchesresults$MatchId[matches_row])
              
    w2<-which(players_data$Player == matchesresults$Player2[matches_row] &
                players_data$MatchId == matchesresults$MatchId[matches_row])
              
    ret<-as.data.frame(players_data[c(w1,w2),-c(1:2)])
    return(ret)
  }
}
