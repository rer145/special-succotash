#import libraries
library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)

#read in the data
deaths<-read.csv("KoreanConflict.csv", header=TRUE, stringsAsFactors=FALSE)

head(deaths)

fullDateRegEx = "^\\d{8}$"
yearRegEx = "^\\d{4}$"

df<-deaths%>%
  filter(str_detect(deaths$INCIDENT_DATE, fullDateRegEx)==TRUE)
count(df)

count(deaths)


#clean up data
for (i in 1:dim(deaths)) {
  #if INCIDENT_DATE is bad, but FATALITY is good, replace it
  if (!str_detect(deaths$INCIDENT_DATE[i], fullDateRegEx) & str_detect(deaths$FATALITY[i], fullDateRegEx)) {
    #print(paste(i, "There is a mistake here"))
    deaths$INCIDENT_DATE[i]<-deaths$FATALITY[i]
  }
}

#filter out bad incident date fields
deaths<-deaths %>%
  filter(str_detect(deaths$INCIDENT_DATE, fullDateRegEx) == TRUE)

count(deaths)

deathsDf<-deaths%>%
  group_by(INCIDENT_DATE)%>%
  summarize(num_deaths=n())%>%
  mutate(date=ymd(INCIDENT_DATE))%>%
  filter(date<='1953-07-27')%>%
  select(INCIDENT_DATE, date, num_deaths)

#look at number of deaths by date
ggplot()+
  geom_line(data=deathsDf, aes(x=date,y=num_deaths))+
  ggtitle("Korean War Deaths by Date")+
  xlab("Date")+
  ylab("Number of Deaths")+
  scale_x_date(date_breaks="3 months", date_labels="%b %y") 


#filter out bad birth year fields
deaths<-deaths %>%
  filter(str_detect(deaths$BIRTH_YEAR, yearRegEx) == TRUE)

#look at number of deaths by birth year
birthsDf<-deaths%>%
  filter(str_detect(deaths$BIRTH_YEAR, yearRegEx) == TRUE)%>%
  group_by(BIRTH_YEAR)%>%
  summarize(num_deaths=n())%>%
  mutate(date=BIRTH_YEAR)%>%
  select(BIRTH_YEAR, date, num_deaths)%>%
  arrange(desc(num_deaths))

ggplot()+
  geom_line(data=birthsDf,aes(x=date, y=num_deaths, group=1), stat="identity")+
  ggtitle("Korean War Deaths by Birth Year")+
  xlab("Birth Year")+
  ylab("Number of Deaths")

