#Load packages
library(tidyverse)
library(ggplot2)
library(plotly)
library(lubridate)

#Load datasets
alt<-read_csv("Dataset/Altitude.csv")

alt<-alt%>%
  separate(Date,
           into=c("date", "time"),
           sep=" ")%>%
  select(-time)  #To seperate dates and time and remain with dates. 


alt$date<-mdy(alt$date)
alt<-alt%>%
  rename(altitude=Altitude) #standardize dates and rename altitude

alt<-alt%>%
  filter(date>=today()- months(9))%>% #Keep data for the last 8 months only
  mutate(month=month(date,label=TRUE),
         year=year(date)) #Create a month column with labels


#alt_sum<-alt%>%
  group_by(month)%>%
  summarize(mean_altitude=mean(altitude))
