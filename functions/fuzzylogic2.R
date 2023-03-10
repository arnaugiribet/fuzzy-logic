function(fuzzyData){
  
  if(sum(!is.na(fuzzyData))==0) {
    return(data.frame(MatchId=NA,
                      Player=NA,
                      FuzzyProb=NA))
  }
  
  if(sum(is.na(fuzzyData[1:2,3:12]))>0){
    fuzzyP<-data.frame(MatchId=fuzzyData$MatchId,
                           Player=fuzzyData$Player,
                           FuzzyProb=NA,NA)
    
    rownames(fuzzyP)<-1:nrow(fuzzyP)
    return(fuzzyP)
  }
    
  if(sum(is.na(fuzzyData))==0){
    
    weights<-c(0.9,0,0.9,0,0,1.5,0,0,0,0)
    playersOpt<-as.matrix(fuzzyData[,-which(colnames(fuzzyData) %in% c('MatchId','Player'))])
    p1<-seq(1,nrow(playersOpt)-1,2)
    p2<-p1+1
    halfplayers<-cbind(playersOpt[p1,],playersOpt[p2,])
    
    fuzzyP<-fuzzyProbs(halfplayers, weights)
    fuzzyP2<-1-fuzzyP
    
    fuzzyP<-as.vector(t(data.frame(fuzzyP,fuzzyP2)))
    
    fuzzyP<-data.frame(MatchId=fuzzyData$MatchId,
                       Player=fuzzyData$Player,
                       FuzzyProb=fuzzyP)
    return(fuzzyP)
     
  }
  
}
