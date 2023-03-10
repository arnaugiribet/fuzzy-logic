function(data=values$fuzzyData,
                       p1row=NA,
                       p2row=NA){
  
  if(is.na(p1row) | is.na(p2row)){
    ret<-t(data[c(1,2),])
    ret[,1]<-NA
    ret[,2]<-NA
    return(ret)
  }
  
  if(!is.na(p2row)){
    return(t(data[c(p1row,p2row),]))
  }
}
