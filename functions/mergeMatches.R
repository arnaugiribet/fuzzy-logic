function(matches,results,probs,players){

  MergedMatchesResults  <- merge(plyr::join(matches,results, by='MatchId', match='first'),
                                 probs,
                                 by.x=c('MatchId', 'Player1'),
                                 by.y=c('MatchId','Player'))
  
  fuzzyProbs2<-fuzzyLogic2(players)
  colnames(fuzzyProbs2)[3]<-'FuzzyProb2'
  p1<-seq(1,nrow(fuzzyProbs2)-1,2)
  MergedMatchesResults<-merge(MergedMatchesResults,fuzzyProbs2[p1,c('MatchId','FuzzyProb2')],all.x=T)
  
  MergedMatchesResults$FuzzyProb<-round(100*MergedMatchesResults$FuzzyProb,2)
  MergedMatchesResults$FuzzyProb2<-round(100*MergedMatchesResults$FuzzyProb2,2)
  MergedMatchesResults$BookieProb<-round(100*MergedMatchesResults$BookieProb,2)
  MergedMatchesResults$Value <- round(MergedMatchesResults$FuzzyProb-MergedMatchesResults$BookieProb,2)
  MergedMatchesResults$Value2 <- round(MergedMatchesResults$FuzzyProb2-MergedMatchesResults$BookieProb,2)
  
  colnames(MergedMatchesResults)[10:14]<-c('Fuzzy1ProbP1','BookieProbP1','Fuzzy2ProbP1','ValueF1P1','ValueF2P1')
  MergedMatchesResults$Fuzzy1ProbP2<-round(100-MergedMatchesResults$Fuzzy1ProbP1,2)
  MergedMatchesResults$BookieProbP2<-round(100-MergedMatchesResults$BookieProbP1,2)
  MergedMatchesResults$Fuzzy2ProbP2<-round(100-MergedMatchesResults$Fuzzy2ProbP1,2)
  MergedMatchesResults$ValueF1P2<- round(-MergedMatchesResults$ValueF1P1,2)
  MergedMatchesResults$ValueF2P2<- round(-MergedMatchesResults$ValueF2P1,2)
  
  MergedMatchesResults$MinCuotaF1P1<-ceiling(100*(1+MergedMatchesResults$Fuzzy1ProbP2/MergedMatchesResults$Fuzzy1ProbP1))/100
  MergedMatchesResults$MinCuotaF1P2<-ceiling(100*(1+MergedMatchesResults$Fuzzy1ProbP1/MergedMatchesResults$Fuzzy1ProbP2))/100
  
  MergedMatchesResults$MinCuotaF2P1<-ceiling(100*(1+MergedMatchesResults$Fuzzy2ProbP2/MergedMatchesResults$Fuzzy2ProbP1))/100
  MergedMatchesResults$MinCuotaF2P2<-ceiling(100*(1+MergedMatchesResults$Fuzzy2ProbP1/MergedMatchesResults$Fuzzy2ProbP2))/100
  
  MergedMatchesResults<-MergedMatchesResults[order(MergedMatchesResults$Date, decreasing=F),
                                             c('MatchId','Tournament','Date','BookieProbP1',
                                               'Fuzzy1ProbP1','Fuzzy2ProbP1','ValueF1P1','ValueF2P1','MinCuotaF1P1','MinCuotaF2P1','Player1',
                                               'Player2','MinCuotaF2P2','MinCuotaF1P2','ValueF2P2','ValueF1P2','Fuzzy2ProbP2','Fuzzy1ProbP2',
                                               "WonPlayer1","NSets","DifGamesPlayer1",'Status')]
  
  return(MergedMatchesResults)
}

