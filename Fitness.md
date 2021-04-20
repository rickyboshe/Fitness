Lockdown Fitness: An FitBit Insight
================
Fredrick Boshe
20/04/2021

# An analysis of how the lockdown in Germany may have affected Fitness levels.

Being in Germany the past 15 months means a state of endless lockdowns
to curb the spread of the virus. While these lockdowns have had some
positive results in keeping the waves of spread low in Germany, they
have undoubtedly taken physical and mental toll on people. The sense of
time, purpose and social behaviors have been negatively impacted. While
it would be hard to actively track of how one’s body reacts to
everything, technology has given us tools that can do this for us.

I am the proud owner of a FitBit Charge 2, which is roughly 5 years
old(!?) but still works like a charm. I graduated last year in
September, left my student working position, changed cities and
experienced my 3rd winter ever. All while going through a pandemic
lockdown. Personally, i felt it took its toll, but i wanted to get
better insights from my FitBit to analyze just how much the pattern was
disrupted.

Questions looking to be answered are:

1.  How did my daily fitness levels trend between October 2020 and March
    2021?
2.  How did my sleep quantity and quality change during the same time
    period? Is there any relationship between my fitness and my quality
    of sleep?
3.  How was my heart-rate/health throughout this period? Are the
    perceptions related to/influenced by the demographics and success
    metrics of the school?

I utilize my FitBit data that i
[exported](https://help.fitbit.com/articles/en_US/Help_article/1133.htm)
to try and answer these questions and provide context to key
observations. The data comes in JSON format, which can be tricky for
some people to parse through. You can uses this [Github
page](https://iccir919.github.io/fitbit-json-to-csv/) that helps convert
your JSON files to CSV.

``` r
#Loading packages
library(tidyverse)
library(ggplot2)
library(dplyr)
library(corrplot)
library(plotly)
library(ggcorrplot)
library(scales)
library(tufte)
library(lubridate)
library(streamgraph)
library(jsonlite)
library(emo)
library(zoo)
```

### Data: Importing and Cleaning

``` r
#Load datasets
## Method1: as CSV
alt<-read_csv("Dataset/Altitude.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   Date = col_character(),
    ##   Altitude = col_double()
    ## )

``` r
dis<-read_csv("Dataset/Distance.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   Date = col_character(),
    ##   Distance = col_double()
    ## )

``` r
hrt1<-read_csv("Dataset/Heartrate1.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   Date = col_character(),
    ##   Heartrate = col_character()
    ## )

``` r
hrt2<-read_csv("Dataset/Heartrate2.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   Date = col_character(),
    ##   Heartrate = col_character()
    ## )

``` r
lam<-read_csv("Dataset/Light active minutes.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   Date = col_character(),
    ##   `Light active minutes` = col_double()
    ## )

``` r
mam<-read_csv("Dataset/Moderate active minutes.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   Date = col_character(),
    ##   `Moderate active minutes` = col_double()
    ## )

``` r
sam<-read_csv("Dataset/Sedentary minutes.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   Date = col_character(),
    ##   `Sedentary minutes` = col_double()
    ## )

``` r
vam<-read_csv("Dataset/Very active minutes.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   Date = col_character(),
    ##   `Very active minutes` = col_double()
    ## )

``` r
slp_sc<-read_csv("Dataset/sleep_score.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   sleep_log_entry_id = col_double(),
    ##   timestamp = col_datetime(format = ""),
    ##   overall_score = col_double(),
    ##   composition_score = col_double(),
    ##   revitalization_score = col_double(),
    ##   duration_score = col_double(),
    ##   deep_sleep_in_minutes = col_double(),
    ##   resting_heart_rate = col_double(),
    ##   restlessness = col_double()
    ## )

``` r
## Method2: as JSON and turn it into a dataframe
path <- "./Dataset/Sleep/"
files <- dir(path, pattern = "*.json")

slp <- files %>%
  map_df(~fromJSON(file.path(path, .), flatten = TRUE))

#Date: Separate date and time (time is not relevant for this analysis)
#Separate date variable
alt<-alt%>%
  separate(Date,
           into=c("date", "time"),
           sep=" ")%>%
  select(-time)  

#Standardize dates and rename variables
alt$date<-mdy(alt$date)
alt<-alt%>%
  rename(altitude=Altitude) 

#Repeat process for all datasets. 
```

The “Date” column is designated as a character in all dataframes. It
needs to be converted into in a “Date” data type. Since time is not of
importance for this analysis, i decided to separate it, discard it and
then convert the dates into correct format. Remember to remove columns
you feel are not important to your analysis or duplicated across
dataframes e.g. sleep\_log\_entry\_id. Filter out low confidence
recordings for heartbeat (FitBit defines 0 1nd 1 as low confidence
recordings, times when FitBit did not record any values).

    ##                            date                   minutesAsleep 
    ##                               0                               0 
    ##                    minutesAwake              minutesAfterWakeup 
    ##                               0                               0 
    ##                       timeInBed                      efficiency 
    ##                               0                               0 
    ##                            type     levels.summary.deep.minutes 
    ##                               0                              72 
    ##     levels.summary.wake.minutes    levels.summary.light.minutes 
    ##                              72                              72 
    ##      levels.summary.rem.minutes levels.summary.restless.minutes 
    ##                              72                             150 
    ##    levels.summary.awake.minutes   levels.summary.asleep.minutes 
    ##                             150                             150

``` r
###Aggregate the data by month (example)
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
```

    ## 
    ##      2      3 
    ## 460516 645856

``` r
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
```

    ## [1] 0

``` r
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
merge<-hrt%>%
  left_join(alt, by="date")%>%
  left_join(dis, by="date")%>%
  left_join(lam, by="date")%>%
  left_join(mam, by="date")%>%
  left_join(sam, by="date")%>%
  left_join(vam, by="date")%>%
  left_join(slp, by="date")%>%
  left_join(slp_sc, by="date")

#Check for duplicated dates
sum(duplicated(merge$date))
```

    ## [1] 0

``` r
##Convert distance units from centimeters to meters
merge$distance<-merge$distance/100
```

## Day Insights: Activity levels

### Distance Covered

``` r
#create month and day columns
merge_dis<-merge%>%
  mutate(day=day(date),
         month=month(date),
         year=year(date))

#Create a column with month and year
merge_dis<-merge_dis%>%
  mutate(period=as.yearmon(paste(year, month), "%Y %m"))
merge_dis$period<-as.Date(merge_dis$period)#To allow mapping easier


#merge_dis_longer<-merge_dis%>%
  #pivot_longer(cols = c(day, month),
               #names_to="period",
               #values_to="value")

plot1<-merge_dis%>%
  drop_na(distance)%>%
  filter(month!=9)%>%
  ggplot(aes(x=day, y=period, fill=distance))+
  geom_tile(colour="white",size=0.25, na.rm = TRUE)+
  theme_minimal()+
  scale_y_date(date_breaks = 'months', date_labels = '%b-%Y', expand = c(0,0))+
  scale_fill_distiller(palette = "RdPu", direction=+1)+
  labs(x = "Day", 
       y = "Month",
       title = "Heatmap: Distance Covered by Date")+
  theme(plot.title = element_text(hjust = 0.5, size = 14, face="bold",
                                  margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 15, 
                                                    b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 0, r = 0, 
                                                    b = 15, l = 0)))


plot1
```

<img src="Fitness_files/figure-gfm/distance-1.png" width="100%" height="100%" />
