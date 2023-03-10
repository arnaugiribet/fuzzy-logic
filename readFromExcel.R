
rm(list=ls())

#####add new data from excel
library(rdrop2)
setwd('C:/Users/AGIRIBET/Desktop/shinyApp')

drop_auth(rdstoken = "droptoken.rds")
loadDropbox<-source('functions/loaddropbox.R',local=T)$value
saveDropbox<-source('functions/savedropbox.R',local=T)$value
fuzzyLogic<-source('functions/fuzzyLogic.R',local=T)$value
dataDropbox<-loadDropbox()

fuzzyMatches<-dataDropbox[[1]]
fuzzyData<-dataDropbox[[2]]
Results<-dataDropbox[[4]]
rm(dataDropbox,loadDropbox)

#players
newstats<-read.delim('clipboard',header=F) #h2h is last, confidence before this. dots as decimals
plays<-as.vector(t(read.delim('clipboard',header=F))) #players
matchId<-rep(paste('Ind', (nrow(fuzzyData)/2)+1:(nrow(newstats)/2),sep=''),each=2)
newstats<-data.frame(matchId,plays,newstats)
colnames(newstats)<-colnames(fuzzyData)
fuzzyData<-rbind(fuzzyData,newstats)

#matches
matchId<-paste('Ind', (nrow(fuzzyMatches)+1):((nrow(fuzzyMatches))+length(plays)/2),sep='')
tourn<-rep(fuzzyMatches$Tournament[1],length(matchId))
date<-rep(fuzzyMatches$Date[nrow(fuzzyMatches)]+lubridate::days(1),length(matchId))
p1<-plays[seq(1,length(plays),2)]
p2<-plays[seq(2,length(plays),2)]
m<-data.frame(matchId, tourn, date, p1, p2)
colnames(m)<-colnames(fuzzyMatches)
fuzzyMatches<-rbind(fuzzyMatches,m)

#probs
fuzzyProbs<-fuzzyLogic(fuzzyData)

#Results
res<-read.delim('clipboard',header=F) #copiar col1 nom, col2 nsets, col3 difjocs
res<-cbind(matchId,res)
won<-rep(0,nrow(res))
if(sum(as.character(res$V1) == as.character(m$Player1))>0){
  won[which(as.character(res$V1) == as.character(m$Player1))]<-1
}
res$V1<-won
if(sum(res$V1!=1)>0){
  res$V3[which(res$V1!=1)]<-res$V3[which(res$V1!=1)]*(-1)
}
res$V4<-'Finished'
colnames(res)<-colnames(Results)
Results<-rbind(Results,res)

#save on dropbox
saveDropbox(fuzzyMatches, fuzzyData, fuzzyProbs, Results)


