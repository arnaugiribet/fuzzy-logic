function(matchesresults,matches_row){
  
  if(is.null(matches_row)) return(data.frame(1:2))
  
  r1<-as.character(matchesresults[matches_row,c('Player1')])
  r2<-as.character(matchesresults[matches_row,c('Player2')])
  
  ret<-as.data.frame(rbind(r1,r2))
  rownames(ret)<-NULL
  colnames(ret)<-'Player'
  return(ret)
}

