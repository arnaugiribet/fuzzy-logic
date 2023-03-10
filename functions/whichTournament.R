function(fuzzyMatches){
  options<-as.character(unique(fuzzyMatches$Tournament)[length(unique(fuzzyMatches$Tournament)):1])
  options<-c('All Tournaments',options)
  return(options)
}

