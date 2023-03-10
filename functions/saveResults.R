function(MatchId, Winner, NSets, GamesDif, Status){
  
  ret<-data.frame(MatchId=MatchId,
                  WonPlayer1=NA,
                  NSets=NA,
                  DifGamesPlayer1=NA,
                  Status='Scheduled')
  
  if(Status==1){
    ret$Status='Played'
    ret$WonPlayer1=Winner
    ret$NSets=NSets
    ret$DifGamesPlayer1=GamesDif
    if(as.numeric(Winner)==0) ret$DifGamesPlayer1<-GamesDif*(-1)
  }
  
  if(Status==3){
    ret$WonPlayer1=Winner
    ret$Status='Abandoned'
  }
  
  return(ret)
  
}