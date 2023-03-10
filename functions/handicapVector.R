function(odds1, odds2, highest) {
  
  if(is.null(highest) | is.null(odds1) | is.null(odds2)) return(rep(NA,12))
  
  if(is.na(highest) | is.na(odds1) | is.na(odds2)) return(rep(NA,12))
  
  if(odds1==odds2) return(rep(NA,12))
  
  vec1<-seq(highest,highest-6,-1)
  vec2<-seq(highest,highest-6,-1)
  
  if(odds1 < odds2){
    vec1<-vec1*(-1)
  }
  
  if(odds1 > odds2){
    vec2<-vec2*(-1)
  }
  
  if(vec1[1]<vec1[7]){
    vec1<-vec1[7:1]
    vec2<-vec2[7:1]
  }
  
  return(c(vec1,vec2))
}