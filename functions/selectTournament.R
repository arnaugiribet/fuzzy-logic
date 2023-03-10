function(MatchesResults,input){
  
  ret<-1:nrow(MatchesResults)
  if(!is.null(input)) {
    ret<-which(MatchesResults$Tournament==input)
    if(input=='All Tournaments') ret<-1:nrow(MatchesResults)
  }
  
  return(ret)
}

