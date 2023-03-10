function(fuzzyMatches, fuzzyData, fuzzyProbs, Results, Bookies){
  
  pathdropbox <- "fuzzydata" #carpeta del dropbox
  namefile<-'fuzzy.RData' #nom fitxer
  filepathlocal <- file.path(namefile) #direccio local
  filepathdropbox <- file.path(pathdropbox, namefile) #direccio dropbox
  
  save(list=c("fuzzyMatches", "fuzzyData", "fuzzyProbs", "Results", "Bookies"), file=filepathlocal)
  
  if (file.exists(filepathlocal)) {
    drop_upload(filepathlocal, path = pathdropbox)
    #file.remove(filepathlocal)
  }
}