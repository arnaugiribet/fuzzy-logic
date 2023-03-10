function(MatchesResults,Matches_rows_selected){
  
  if(!is.null(Matches_rows_selected)){
    w<-which(as.character(MatchesResults$MatchId) == as.character(MatchesResults$MatchId[Matches_rows_selected]))
    ret<-MatchesResults[w,]
    ret$FuzzyProb<-paste(round(ret$FuzzyProb*100,2),'%',sep='')
    ret$BookieProb<-paste(round(ret$BookieProb*100,2),'%',sep='')
    return(ret)
  }
  if(is.null(Matches_rows_selected)){
    ret<-MatchesResults[1:2,]
    ret[1:2,]<-NA
    return(ret)
  }
}
