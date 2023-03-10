function(player1, player2) {
  
  if(is.null(player1) | is.null(player2)){
    title <- c('Not Selected','Neither')
  }
  
  if(!is.null(player1) & !is.null(player2)){
    title <- c(player1,player2)
    if(title[1] == title[2]) title[2]<-paste(title[2],' ',sep='')
  }
  
  code <- c(1,0)
  
  ret <- data.frame(title, code)
  
  ret <- as.list(setNames(ret$code, ret$title))
  return(ret)
}