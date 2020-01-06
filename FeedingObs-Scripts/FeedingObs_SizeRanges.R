rm(list=ls()) # clears workspace
options(stringsAsFactors=F)
library(plyr)

dat<-read.csv('../FeedingObs_db-Data/FeedingObs-Obs-All.csv')


stats<-ddply(dat,.(Pred,Prey),summarize,PredMin=min(PredSize,na.rm=TRUE),PredMax=max(PredSize,na.rm=TRUE),PreyMin=min(PreySize,na.rm=TRUE),PreyMax=max(PreySize,na.rm=TRUE),n=length(PreySize))

stats<-subset(stats,Prey!='Not_Feeding')
stats<-arrange(stats,desc(Pred),desc(n))


write.csv(stats,'../FeedingObs_db-Output/FeedingObs-SizeRanges.csv')


pdf('../FeedingObs_db-Output/FeedingObs-SizeRanges.pdf',height=10,width=8)
par(mfrow=c(5,3),cex.main=0.8)
for(p in unique(dat$Pred)){
  tdat<-subset(dat,Pred==p)
  hist(tdat$PredSize,main=p,col='grey',xlab='Size (mm)',breaks=20)
  for(q in unique(tdat$Prey)){
    ttdat<-subset(tdat,Prey==q)
      n=nrow(ttdat)
      if(q!='Not_Feeding')
      hist(ttdat$PreySize,main=paste(paste(p,q,sep='-'),'\n n =',n),col='grey',xlab='Size (mm)',breaks=20)
}}
dev.off()


subset(dat,Pred=='Nucella_canaliculata' & PreySize>5 & Prey=='Balanus_glandula')
