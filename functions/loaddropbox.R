function(){

  pathdropbox <- "fuzzydata" #carpeta del dropbox
  namefile<-'fuzzy.RData' #nom fitxer
  filepathlocal <- file.path(namefile) #direccio local
  filepathdropbox <- file.path(pathdropbox, namefile) #direccio dropbox
  
  drop_download(filepathdropbox, local_path = filepathlocal, overwrite = T)
 
  if (file.exists(filepathlocal)) {
    load(filepathlocal)
    file.remove(filepathlocal)
  }
  
  return(list(fuzzyMatches,fuzzyData,fuzzyProbs, Results, Bookies))
}