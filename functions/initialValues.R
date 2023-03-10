function(player,fuzzyData){
  if(sum(which(fuzzyData$Player == player))==0){
    return(rep(0.5,9))
  }
  
  if(sum(which(fuzzyData$Player == player))>0){
    playData<-fuzzyData[which(fuzzyData$Player == player),]
    return(as.numeric(playData[nrow(playData),3:11]))
  }
}