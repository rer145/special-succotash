library(Lahman)
library(dplyr)
library(ggplot2)
library(ggiraph)

#dashboards using flexdashboard
library(flexdashboard)

#dashboards are great when visualizations are related


result<-Batting%>%
        filter(playerID=='ruthba01')%>%
        select(yearID,SO,HR)


#scatterplot
ggplot()+
  geom_point(data=result,aes(x=SO,y=HR),color="red")+
  xlab("Strikeouts")+
  ylab("Home Runs")


#histogram
ggplot()+
  geom_histogram(data=result,aes(x=HR),color="blue",fill="white",bins=5)+
  xlab("Home Runs")


#timeseries
ggplot()+
  geom_line(data=result,aes(x=yearID,y=HR),stat="identity",color="green")+
  geom_point(data=result,aes(x=yearID,y=HR),stat="identity",color="pink")+
  xlab("Year")+
  ylab("Home Runs")


