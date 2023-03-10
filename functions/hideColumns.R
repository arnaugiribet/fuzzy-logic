function(matchesresults){
  vec<-which(colnames(matchesresults) %in% c('MatchId','WonPlayer1','NSets','DifGamesPlayer1','Status'))-1
  return(vec)
}