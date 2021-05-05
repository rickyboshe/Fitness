#Load packages
library(tidyverse)
library(ggplot2)
library(plotly)
library(lubridate)
library(jsonlite)
library(streamgraph)

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
  select(-time)   

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
  select(-time, -sleep_log_entry_id, -deep_sleep_in_minutes)

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

#Import Sleep files Separate levels column
path <- "./Dataset/"
files <- dir(path, pattern = "*.json")

slp <- files %>%
  map_df(~fromJSON(file.path(path, .), flatten = TRUE))

#Remove redundant columns
slp$dateOfSleep<-ymd(slp$dateOfSleep)

slp<-slp%>%
  select(-1, -3:-6, -13:-16,-17, -19, -20, -22, -23, 
         -25, -26, -28, -29, -31, -33)%>%
  rename(date=dateOfSleep)%>%
  as_tibble()

min(slp$minutesAsleep)

###Aggregate the data by month

alt<-aggregate(alt["altitude"], by=alt["date"], sum)

dis<-aggregate(dis["distance"], by=dis["date"], sum)


#Dealing with missing values to allow aggregation
colSums(is.na(slp))

slp<-slp%>%
  select(-type, -efficiency) #Not important

slp<-aggregate(. ~date, data = slp, sum, na.rm=TRUE, na.action=na.pass)

##Missing values are converted to zero, which is fine for those variables

max(slp$efficiency)

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

#Aggregate sleep score by date
slp_sc<-slp_sc%>%
  group_by(date)%>%
  summarize(across(everything(), mean))

sum(duplicated(slp_sc$date))


#Keep data for the last 9 months only

con_date<-function(df){
  df<-df%>%
    filter(date>=today()- months(9))
  return(df)
}
alt<-con_date(alt)
dis<-con_date(dis)
hrt<-con_date(hrt)
lam<-con_date(lam)
mam<-con_date(mam)
sam<-con_date(sam)
slp<-con_date(slp)
slp_sc<-con_date(slp_sc)
vam<-con_date(vam)


#Merge the files. left join to hrt dataset
merge<-join_all(list(hrt,alt,dis,lam,mam,sam,
                     vam,slp,slp_sc), by="date", type = "left")


sum(is.na(merge))


##Convert distance units from centimeters to meters
merge$distance<-merge$distance/100

##Pivot longer by bpm
#Streamgraph
merge_longer<-merge%>%
  pivot_longer(cols = c(avg_bpm, min_bpm, max_bpm),
               names_to="bpm",
               values_to="value")
merge_longer$bpm<-factor(merge_longer$bpm, levels = c("min_bpm", "avg_bpm", "max_bpm"))

fig1 <- streamgraph(merge_longer, key="bpm", value="value", date="date", 
                    offset="zero", interpolate="linear", height="300px", width="1000px")%>%
  sg_legend(show=TRUE, label="heartrates: ")%>%
  sg_fill_brewer("Pastel1")
fig1

#Line chart

fig2<-merge_longer%>%
  ggplot(aes(x=date, y=value, color=bpm))+
  geom_line()+
  geom_hline(yintercept = c(95, 162), col = "red", lty = 2, alpha = 0.7)

fig2

fig3<-merge_longer%>%
  ggplot( aes(x = date, y = value, color=bpm)) +
  geom_line() +
  geom_ribbon(aes(ymin=95,ymax=162), fill="green", color="green", alpha=.15)+
  geom_hline(yintercept = c(95, 162), col = "red", lty = 2, alpha = 0.7) + # move this under geom_ribbon for a nicer result
  theme_minimal()
fig3


fig4<-merge_longer%>%
  ggplot(aes(x=date, y=value, color=bpm))+
  geom_line()+
  geom_ribbon(aes(ymax=162, ymin=95), fill="pink", alpha=.2)
fig4

alt<-alt%>%
  filter(date>=today()- months(9))%>% 
  mutate(month=month(date,label=TRUE),
         year=year(date)) #Create a month column with labels

###Additional code
#alt_sum<-alt%>%
  group_by(month)%>%
  summarize(mean_altitude=mean(altitude))

AP.fit <- lm(merge_alt$altitude ~ merge_alt$month)
summary(AP.fit)
AP.ci <- predict(AP.fit, interval="confidence")
plot(AirPassengers)

#Check if there is significant different for the months
sum(is.na(merge_alt$altitude))
merge_alt<-merge_alt%>%
  drop_na(altitude)

#Test for normality
shapiro.test(merge_alt$altitude)

data <- data.frame(
  individual=paste( "Mister ", seq(1,60), sep=""),
  group=c( rep('A', 10), rep('B', 30), rep('C', 14), rep('D', 6)) ,
  value1=sample( seq(10,100), 60, replace=T),
  value2=sample( seq(10,100), 60, replace=T),
  value3=sample( seq(10,100), 60, replace=T)
)
max(merge_slp$minutesAsleep)
min(merge_slp$minutesAsleep)
rlang::last_error()
table(label_data$day)


#Calculate infograph stats
## Monthly distance
monthly_distance<-merge%>%
  mutate(weekday=wday(date, label=TRUE),
         month=month(date),
         year=year(date))

monthly_distance<-monthly_distance%>%
  mutate(period=as.yearmon(paste(year, month), "%Y %m"))
monthly_distance$period<-parse_date(monthly_distance$period)

monthly_distance<-monthly_distance%>%
  group_by(period)%>%
  summarize(avg_dis=sum(distance))%>%
  drop_na(avg_dis)%>%
  mutate(dis_km=avg_dis/1000)

mean(monthly_distance$dis_km)

#compound growth
growth_rate <- monthly_distance %>%
  arrange(period) %>%
  mutate(Diff_month = period - lag(period), 
         Diff_growth = dis_km - lag(dis_km), 
         Rate_percent = (Diff_growth / lag(dis_km)) * 100) 
Average_growth = mean(growth_rate$Rate_percent, na.rm = TRUE)


## Monthly active minutes
monthly_active<-merge%>%
  mutate(weekday=wday(date, label=TRUE),
         month=month(date),
         year=year(date))

monthly_active<-monthly_active%>%
  mutate(period=as.yearmon(paste(year, month), "%Y %m"))
monthly_active$period<-parse_date(monthly_active$period)

#Select only light, moderate and ver active minutes, sum them up
monthly_active$active<- rowSums(monthly_active[, c(7,8,10)], na.rm=TRUE)

#Convert active  minutes to hours and remove months with less than a week recording

monthly_active<-monthly_active%>%
  group_by(period)%>%
  summarize(avg_act=sum(active))%>%
  mutate(avg_act=avg_act/60)%>% 
  filter(period!="Sep 2020" & period!="Mar 2021") 

mean(monthly_active$avg_act)

#compound growth
growth_rate <- monthly_active %>%
  arrange(period) %>%
  mutate(Diff_month = period - lag(period), 
         Diff_growth = avg_act - lag(avg_act), 
         Rate_percent = (Diff_growth / lag(avg_act)) * 100) 
Average_growth = mean(growth_rate$Rate_percent, na.rm = TRUE)

## Average resting heart rate
mean(merge$resting_heart_rate, na.rm = TRUE)

## Average sleeping
daily_sleep<-merge%>%
  mutate(weekday=wday(date, label=TRUE),
         month=month(date),
         year=year(date))

daily_sleep<-daily_sleep%>%
  group_by(weekday)%>%
  summarize(avg_slp=mean(minutesAsleep, na.rm=TRUE)/60)

mean(daily_sleep$avg_slp)


## REM and Deep Sleep
monthly_rd<-merge%>%
  mutate(weekday=wday(date, label=TRUE),
         month=month(date),
         year=year(date))

monthly_rd<-monthly_rd%>%
  mutate(period=as.yearmon(paste(year, month), "%Y %m"))
monthly_rd$period<-parse_date(monthly_rd$period)

#Select only REM and deep sleep columns and sum them up
monthly_rd$sleep<- rowSums(monthly_rd[, c(15,18)], na.rm=TRUE)

#Convert Create a column that holds the percentage of sleep that was REM & Deep
monthly_rd<-monthly_rd%>%
  group_by(weekday)%>%
  summarize(rd_slp=sum(sleep),
            tot_slp=sum(minutesAsleep, na.rm = TRUE))%>%
  mutate(per_slp=(rd_slp/tot_slp)* 100)

mean(monthly_rd$per_slp)

