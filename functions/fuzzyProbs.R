function(halfplayers, weights){

    sweights<-sum(weights)
    
    if(nrow(halfplayers)>1){
      fuzzyProbs<-(sweights-apply(sweep(halfplayers[,11:20], MARGIN=2, weights, `*`),1,sum))/
        (2*sweights-apply(sweep(halfplayers[,1:10], MARGIN=2, weights, `*`),1,sum)-
           apply(sweep(halfplayers[,11:20], MARGIN=2, weights, `*`),1,sum))
      
    }
    
    if(nrow(halfplayers)==1){
      fuzzyProbs<-(sweights-sum(halfplayers[,11:20]*weights))/
        (2*sweights-sum(halfplayers[,1:10]*weights)-
           sum(halfplayers[,11:20]*weights))
      
    }
    
    
    return(fuzzyProbs)
}