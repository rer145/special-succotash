library(RODBC)
library(ggplot2)
library(dplyr)

cn<-odbcDriverConnect(connection="Driver={SQL Server Native Client 11.0};server=YOUR_SERVER_HERE;database=YOUR_DATABASE_HERE;uid=YOUR_USERID_HERE;pwd=YOUR_PASSWORD_HERE;")

handicaps<-sqlFetch(cn, 'r_handicap_history', colnames=FALSE)

ggplot()+
  geom_line(data=handicaps,aes(x=HandicapDate,y=HandicapIndex))+
  geom_point(data=handicaps,aes(x=HandicapDate,y=HandicapIndex))+
  xlab("Handicap Date")+
  ylab("Handicap Index")


driving<-sqlFetch(cn, 'r_driving_accuracy_history', colnames=FALSE)%>%
  filter(PlayedOn >= "2016-01-01")

ggplot()+
  geom_line(data=driving,aes(x=PlayedOn,y=FairwayHitPercent))+
  geom_point(data=driving,aes(x=PlayedOn,y=FairwayHitPercent))+
  xlab("Played On")+
  ylab("Fairways Hit (%)")


driving_gir<-sqlFetch(cn, 'r_driving_gir_history', colnames=FALSE)%>%
  filter(PlayedOn >= '2016-01-01')

ggplot()+
  geom_point(data=driving_gir, aes(x=FairwayHitPercent,y=GIRPercent))+
  xlab("Fairways Hit (%)")+
  ylab("Greens in Regulation (%)")
