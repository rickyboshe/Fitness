#Load packages
library(tidyverse)
library(ggplot2)
library(plotly)
library(lubridate)
library(jsonlite)

#Load datasets
alt<-read_csv("Dataset/Altitude.csv")
dis<-read_csv("Dataset/Distance.csv")
hrt1<-read_csv("Dataset/Heartrate1.csv")
hrt2<-read_csv("Dataset/Heartrate2.csv")
lam<-read_csv("Dataset/Light active minutes.csv")
mam<-read_csv("Dataset/Moderate active minutes.csv")
sam<-read_csv("Dataset/Sedentary minutes.csv")
vam<-read_csv("Dataset/Very active minutes.csv")
slp<-read_csv("Dataset/Sleep.csv")
slp_sc<-read_csv("Dataset/sleep_score.csv")

#Separate date variable
alt<-alt%>%
  separate(Date,
           into=c("date", "time"),
           sep=" ")%>%
  select(-time)  #To seperate dates and time and remain with dates. 

dis<-dis%>%
  separate(Date,
           into=c("date", "time"),
           sep=" ")%>%
  select(-time)

hrt1<-hrt1%>%
  separate(Date,
           into=c("date", "time"),
           sep=" ")%>%
  select(-time)

hrt2<-hrt2%>%
  separate(Date,
           into=c("date", "time"),
           sep=" ")%>%
  select(-time)

lam<-lam%>%
  separate(Date,
           into=c("date", "time"),
           sep=" ")%>%
  select(-time)

mam<-mam%>%
  separate(Date,
           into=c("date", "time"),
           sep=" ")%>%
  select(-time)

sam<-sam%>%
  separate(Date,
           into=c("date", "time"),
           sep=" ")%>%
  select(-time)

vam<-vam%>%
  separate(Date,
           into=c("date", "time"),
           sep=" ")%>%
  select(-time)

slp_sc<-slp_sc%>%
  separate(timestamp,
           into=c("date", "time"),
           sep=" ")%>%
  select(-time)

#Standardize dates and rename variables

alt$date<-mdy(alt$date)
alt<-alt%>%
  rename(altitude=Altitude) 

dis$date<-mdy(dis$date)
dis<-dis%>%
  rename(distance=Distance) 


hrt1$date<-mdy(hrt1$date)
hrt1<-hrt1%>%
  rename(heartrate=Heartrate) 


hrt2$date<-mdy(hrt2$date)
hrt2<-hrt2%>%
  rename(heartrate=Heartrate) 


lam$date<-mdy(lam$date)
lam<-lam%>%
  rename(light_act_mins=`Light active minutes`) 


mam$date<-mdy(mam$date)
mam<-mam%>%
  rename(moderate_act_mins=`Moderate active minutes`) 

sam$date<-mdy(sam$date)
sam<-sam%>%
  rename(sedentary_mins=`Sedentary minutes`)

slp_sc$date<-ymd(slp_sc$date)

vam$date<-mdy(vam$date)
vam<-vam%>%
  rename(very_act_mins=`Very active minutes`) 


###Aggregate the data by month

alt<-aggregate(alt["altitude"], by=alt["date"], sum)

dis<-aggregate(dis["distance"], by=dis["date"], sum)

#Separate bpm and confidence columns
hrt<-rbind(hrt1,hrt2)
hrt<-hrt%>%
  separate(heartrate,
           into=c("bpm", "confidence"),
           sep=",")
hrt$bpm<-parse_number(hrt$bpm)
hrt$confidence<-parse_number(hrt$confidence)

#keep only high confidence recording (2 or 3)
hrt<-hrt%>%
  filter(confidence==2| confidence==3)
table(hrt$confidence)

#create min, max and avg bpm columns for each date

hrt<-hrt%>%
  group_by(date)%>%
  mutate(min_bpm=min(bpm),
         max_bpm=max(bpm),
         avg_bpm=mean(bpm))
hrt$avg_bpm<-as.integer(hrt$avg_bpm)

#remove bpm and confidence interval columns. Keep one observation per day

hrt<-hrt%>%
  select(-2:-3)%>%
  unique.data.frame()

#remove redundant columns
slp_sc<-slp_sc%>%
  select(-1)

#Import Sleep files Separate levels column
path <- "./Dataset/"
files <- dir(path, pattern = "*.json")

slp <- files %>%
  map_df(~fromJSON(file.path(path, .), flatten = TRUE))

#Remove redundant columns
slp$dateOfSleep<-ymd(slp$dateOfSleep)

slp<-slp%>%
  select(-1, -3:-6, -13:-16,-17, -19, -20, -22, -23, 
         -25, -26, -28, -29, -31, -33)

min(slp$minutesAsleep)

##Convert distance units from centimeters to meters

###

alt<-alt%>%
  filter(date>=today()- months(9))%>% 
  mutate(month=month(date,label=TRUE),
         year=year(date)) #Create a month column with labels


#alt_sum<-alt%>%
  group_by(month)%>%
  summarize(mean_altitude=mean(altitude))
