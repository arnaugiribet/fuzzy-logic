function(matchesresults,players, nrow, bookies){
  
  retNA<-data.frame(Bet=NA,
                    P1=NA,
                    P2=NA,
                    Bet=NA)
  
  if(is.null(nrow)) return(retNA)
  
  if(nrow>nrow(matchesresults)) return(retNA)
  
  plays<-players$Player[which(players$MatchId == matchesresults$MatchId[nrow])]
  haps<-bookies[which(bookies$MatchId == matchesresults$MatchId[nrow]),]
  hv<-as.character(handicapVector(as.numeric(haps[2]),as.numeric(haps[3]),as.numeric(haps[4])))
  hv[which(hv>0)]<-paste('+',hv[which(hv>0)],sep='')
  
  cnames<-c('Bet',plays,'Bet')
  haleft<-c('Match',paste('HA',hv[1:7]))
  haright<-c('Match',paste('HA',hv[8:14]))
  oddsleft<-haps[,c(2,5:11)]
  oddsright<-haps[,c(3,12:18)]
  
  ret<-data.frame(Bet=haleft,
                  P1=as.numeric(oddsleft),
                  P2=as.numeric(oddsright),
                  Bet=haright)
  colnames(ret)<-cnames
  
  return(ret)
  
  }