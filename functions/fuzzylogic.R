function(fuzzyData){
  
  if(sum(!is.na(fuzzyData))==0) {
    return(data.frame(MatchId=NA,
                      Player=NA,
                      FuzzyProb=NA))
  }
  
  if(sum(is.na(fuzzyData[1:2,3:12]))>0){
    fuzzyProbs<-data.frame(MatchId=fuzzyData$MatchId,
                           Player=fuzzyData$Player,
                           FuzzyProb=NA,NA)
    
    rownames(fuzzyProbs)<-1:nrow(fuzzyProbs)
    return(fuzzyProbs)
  }
    
  if(sum(is.na(fuzzyData))==0){
    
    
    weights<-c(1,0.8,0.7,0.7,0.4,0.8,0.8,0.6,0.8,0.4)
    playersOpt<-as.matrix(fuzzyData[,-which(colnames(fuzzyData) %in% c('MatchId','Player'))])
    p1<-seq(1,nrow(playersOpt)-1,2)
    p2<-p1+1
    halfplayers<-cbind(playersOpt[p1,],playersOpt[p2,])
    if(length(p1)==1) halfplayers<-t(as.matrix(c(playersOpt[p1,],playersOpt[p2,])))
    
    fuzzyP<-fuzzyProbs(halfplayers, weights)
    fuzzyP2<-1-fuzzyP
    
    fuzzyP<-as.vector(t(data.frame(fuzzyP,fuzzyP2)))
    
    fuzzyP<-data.frame(MatchId=fuzzyData$MatchId,
                       Player=fuzzyData$Player,
                       FuzzyProb=fuzzyP)
    return(fuzzyP)
    
    # 
    # weights<-data.frame(Technique=1,
    #                     Physique=0.8,
    #                     Psicology=0.7,
    #                     Regularity=0.7,
    #                     Experience=0.4,
    #                     Implication=0.8,
    #                     Surface=0.8,
    #                     Rest=0.6,
    #                     Confidence=0.8,
    #                     H2H=0.4)
    # 
    # weights<-weights/sum(weights)
    # 
    # hamilton<-function(x){
    #   hdists<-sweep(as.matrix(1-x[,colnames(weights)]),2,as.numeric(weights),`*`)
    #   hdists<-apply(hdists,1,sum)
    #   hdists<-1-hdists/sum(hdists)
    #   return(hdists)
    # } 
    # 
    # ea<-by(fuzzyData, fuzzyData$MatchId, function(x) hamilton(x))
    # ord<-order(as.numeric(substring(names(unlist(ea)),4)))
    # fuzzyProbs<-data.frame(MatchId=fuzzyData$MatchId,
    #                        Player=fuzzyData$Player,
    #                        FuzzyProb=unlist(ea)[ord])
    # 
    # rownames(fuzzyProbs)<-1:nrow(fuzzyProbs)
    # return(fuzzyProbs)
     
  }
  
}
