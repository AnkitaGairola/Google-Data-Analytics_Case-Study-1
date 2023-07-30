# Google-Data-Analytics_Case-Study-1
Case Study: How Does a Bike-Share Navigate Speedy Success 
## About the Company

In 2016, Cyclistic launched a successful bike-share offering. Since then, the program has grown to a fleet of 5,824 bicycles that are geotracked and locked into a network of 692 stations across Chicago. The bikes can be unlocked from one station and returned to any other station in the system anytime. Until now, Cyclistic’s marketing strategy relied on building general awareness and appealing to broad consumer segments. One approach that helped make these things possible was the flexibility of its pricing plans: single-ride passes, full-day passes, and annual memberships. Customers who purchase single-ride or full-day passes are referred to as casual riders. Customers who purchase annual memberships are Cyclistic members.Cyclistic’s finance analysts have concluded that annual members are much more profitable than casual riders. 

Although the pricing flexibility helps Cyclistic attract more customers, Moreno believes that maximizing the number of annual members will be key to future growth. Rather than creating a marketing campaign that targets all-new customers, Moreno believes there is a very good chance to convert casual riders into members. She notes that casual riders are already aware of the Cyclistic program and have chosen Cyclistic for their mobility needs.

## Goal of Case Study

1. How do annual members and casual riders use Cyclistic bikes differently?
2. Why would casual riders buy Cyclistic annual memberships?
3. How can Cyclistic use digital media to influence casual riders to become members?

Director has assigned first questions: How do annual members and casual riders use Cyclistic bikes differently?

I have to produce with the following deliverable:

1. A clear statement of the business task
2. A description of all data sources used
3. Documentation of any cleaning or manipulation of data
4. A summary of your analysis
5. Supporting visualizations and key findings
6. Your top three recommendations based on your analysis

### Ask

Business task : Maximise the number of annual membership

Key Stakeholder: The director of marketing, The marketing analysis team, and Cyclistic executive team.

### Prepare

Data set: Divvy's, a bike share programme based in Chicago, data from April 2020 - March 2021 

###### Set working directory

getwd()

setwd("E:/Capstone projects_Google/Case Study 1")

###### Required packages and read the data

install.packages("readxl")

install.packages("tidyverse")

install.packages("lubridate")

install.packages("ggplot2")

install.packages("dplyr")

install.packages("tidyr")

install.packages("geosphere")

install.packages("reprex")

install.packages("skimr")

```{r}
library("tidyverse")

library(lubridate)

library("ggplot2")

library("dplyr")

library("tidyr")

library("readxl")

library("geosphere")

library("reprex")

library("skimr")

April_2020 <- read.csv("E:/Capstone projects_Google/Case Study 1/working data/202004-divvy-tripdata.csv")

May_2020 <- read.csv("E:/Capstone projects_Google/Case Study 1/working data/202005-divvy-tripdata.csv")

June_2020 <- read.csv("E:/Capstone projects_Google/Case Study 1/working data/202006-divvy-tripdata.csv")

July_2020 <- read.csv("E:/Capstone projects_Google/Case Study 1/working data/202007-divvy-tripdata.csv")

August_2020 <- read.csv("E:/Capstone projects_Google/Case Study 1/working data/202008-divvy-tripdata.csv")

September_2020 <- read.csv("E:/Capstone projects_Google/Case Study 1/working data/202009-divvy-tripdata.csv")

October_2020 <- read.csv("E:/Capstone projects_Google/Case Study 1/working data/202010-divvy-tripdata.csv")

Novemeber_2020 <- read.csv("E:/Capstone projects_Google/Case Study 1/working data/202011-divvy-tripdata.csv")

December_2020 <- read.csv("E:/Capstone projects_Google/Case Study 1/working data/202012-divvy-tripdata.csv")

January_2021 <- read.csv("E:/Capstone projects_Google/Case Study 1/working data/202101-divvy-tripdata.csv")

Febraury_2021 <- read.csv("E:/Capstone projects_Google/Case Study 1/working data/202102-divvy-tripdata.csv")

March_2021 <- read.csv("E:/Capstone projects_Google/Case Study 1/working data/202103-divvy-tripdata.csv")

```
###### For data check

```{r}
colnames(April_2020)

colnames(May_2020)

colnames(June_2020)

colnames(July_2020)

colnames(August_2020)

colnames(September_2020)

colnames(October_2020)

colnames(Novemeber_2020)

colnames(December_2020)

colnames(January_2021)

colnames(Febraury_2021)

colnames(March_2021)

```
###### For data check

```{r}
str(April_2020)

str(May_2020)

str(June_2020)

str(July_2020)

str(August_2020)

str(September_2020)

str(October_2020)

str(Novemeber_2020)

str(December_2020)

str(January_2021)

str(Febraury_2021)

str(March_2021)

```
###### Convert data from double to character

```{r}
April_2020 <- mutate(April_2020, start_station_id = as.character(start_station_id), end_station_id = as.character(end_station_id))

May_2020 <- mutate(May_2020, start_station_id = as.character(start_station_id), end_station_id = as.character(end_station_id))

June_2020 <- mutate(June_2020, start_station_id = as.character(start_station_id), end_station_id = as.character(end_station_id))

July_2020 <- mutate(July_2020, start_station_id = as.character(start_station_id), end_station_id = as.character(end_station_id))

August_2020 <-mutate(August_2020, start_station_id = as.character(start_station_id), end_station_id = as.character(end_station_id))

September_2020 <-mutate(September_2020, start_station_id = as.character(start_station_id), end_station_id = as.character(end_station_id))

October_2020 <-mutate(October_2020, start_station_id = as.character(start_station_id), end_station_id = as.character(end_station_id))

Novemeber_2020 <- mutate(Novemeber_2020, start_station_id = as.character(start_station_id), end_station_id = as.character(end_station_id))

December_2020 <-mutate(December_2020, start_station_id = as.character(start_station_id), end_station_id = as.character(end_station_id))

January_2021 <-mutate(January_2021, start_station_id = as.character(start_station_id), end_station_id = as.character(end_station_id))

Febraury_2021 <-mutate(Febraury_2021, start_station_id = as.character(start_station_id), end_station_id = as.character(end_station_id))

March_2021 <-mutate(March_2021, start_station_id = as.character(start_station_id), end_station_id = as.character(end_station_id))


```

###### Merge all the data set

```{r}
All_Trip_Data <- bind_rows(April_2020,May_2020,June_2020,July_2020, August_2020, September_2020, October_2020, Novemeber_2020, December_2020, January_2021, Febraury_2021, March_2021)
```

### Process

Data cleaning before analysis

###### Number of column

```{r}
ncol(All_Trip_Data)
```

###### Number of rows

```{r}
nrow(All_Trip_Data)
```
###### Column name

```{r}
colnames(All_Trip_Data)
```
###### Dimensions of data

```{r}
dim(All_Trip_Data)
```
###### For first six rows of data

```{r}
head(All_Trip_Data)
```
###### Structure of data i.e (character, numeric, vectors, list etc)

```{r}
str(All_Trip_Data)
```
###### Statistical summary of data

```{r}
summary(All_Trip_Data)
```
###### Add columns of date, month, and year of the ride

```{r}
All_Trip_Data$date <- as.Date(All_Trip_Data$started_at)
All_Trip_Data$month <- format(as.Date(All_Trip_Data$date), "%m")
All_Trip_Data$day <- format(as.Date(All_Trip_Data$date), "%d")
All_Trip_Data$year <- format(as.Date(All_Trip_Data$date), "%Y")
All_Trip_Data$day_of_week <- format(as.Date(All_Trip_Data$date), "%A")

```

###### Add 'time' column after extract time from the 'started_at' column

```{r}
All_Trip_Data$time <- as.POSIXct(All_Trip_Data$started_at, format = "%Y-%m-%d %H:%M:%S")
All_Trip_Data$time <-format(All_Trip_Data$time, format = "%H:%M")
```

###### Add 'time2' column after extract time from the 'ended_at' column

```{r}
All_Trip_Data$time2 <- as.POSIXlt(All_Trip_Data$ended_at, format = "%Y-%m-%d %H:%M:%S")
All_Trip_Data$time2 <- format(All_Trip_Data$time2, format = "%H:%M")
```

###### For calculation of ride length

```{r}
All_Trip_Data$ride_length <- as.double(difftime(All_Trip_Data$ended_at, All_Trip_Data$started_at, units = "mins"))
```



###### For confirmation of additional column

```{r}
colnames(All_Trip_Data)
```

```{r}
skim(All_Trip_Data)
```
###### Convert 'ride length from double to numeric

```{r}
All_Trip_Data$ride_length <- as.numeric(as.character(All_Trip_Data$ride_length))
is.numeric(All_Trip_Data$ride_length)
```
###### New Data frame without records that have ride length <=zero minute or >1440 minutes

```{r}
All_Trip_Data_2 <- All_Trip_Data[!(All_Trip_Data$ride_length <=0 | All_Trip_Data$ride_length < 1440),]
```

###### Check the new data

```{r}
dim(All_Trip_Data_2)
summary(All_Trip_Data_2)
view(All_Trip_Data_2)
```
###### Remove Duplicate ride id
```{r}
All_Trip_Data_3 <- All_Trip_Data_2[!duplicated(All_Trip_Data_2$ride_id),]
dim(All_Trip_Data_3)
view(All_Trip_Data_3)

```

###### Calculate the ride distance
```{r}
All_Trip_Data_3$ride_distance <- distGeo(matrix(c(All_Trip_Data_3$start_lng, All_Trip_Data_3$start_lat), ncol = 2), matrix (c(All_Trip_Data_3$end_lng, All_Trip_Data_3$end_lat), ncol = 2))

view(All_Trip_Data_3)
```

```{r}
summary(All_Trip_Data_3)
```
### Analyze

###### Ride distribution day of week

```{r}
All_Trip_Data_3$day_of_week <- ordered(All_Trip_Data_3$day_of_week, levels = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'))
```


```{r}
All_Trip_Data_3 %>%
  group_by(member_casual, day_of_week) %>%
  summarise(number_of_ride = n(), .groups = 'drop') %>%
  arrange(day_of_week)
```
###### Ride distribution by month

```{r}
All_Trip_Data_3$month <- ordered(All_Trip_Data_3$month, level = c('05', '06', '07', '08', '09', '10', '11', '12', '01', '02', '03'))
```


###### Ride distribution by day of the month

```{r}
All_Trip_Data_3 %>%
  group_by(member_casual, day) %>%
  summarise(number_of_ride = n(),
            .groups = "drop")
``` 
###### whether ride_length can be different depends on rider type.

```{r}
aggregate(All_Trip_Data_3$ride_length ~ All_Trip_Data_3$member_casual + All_Trip_Data_3$day_of_week, FUN = mean)
```


```{r}
All_Trip_Data_3 %>%
  group_by(member_casual, month) %>%
  summarise(average_ride_length = mean(ride_length), groups = 'drop') %>%
  arrange(month)
```


###### The number of bike type users

```{r}
All_Trip_Data_3 %>%
  group_by(rideable_type) %>%
  summarise(number_of_ride = n())
```
###### Cycle type usage by user type

```{r}
All_Trip_Data_3 %>%
  group_by(member_casual, rideable_type) %>%
  summarise(number_of_ride = n(),
            .groups = "drop") %>%
  arrange(rideable_type)
```



###### Cycle type and average ride type

```{r}
All_Trip_Data_3 %>%
  group_by(rideable_type) %>%
  summarise(mean = mean(ride_length),
            .groups = "drop") %>%
  arrange(rideable_type)
```



###### Name of Unique Station

```{r}
stations <- All_Trip_Data_3 %>%
  gather(key, station_name, start_station_name, end_station_name) %>%
  distinct(station_name)
```


```{r}
print(paste("Number of station", nrow(stations)))
```

###### Most popular station

```{r}
popular_station <- All_Trip_Data_3 %>%
  gather(key, station_name, start_station_name, end_station_name) %>%
  group_by(station_name) %>%
  summarise(number_trip = n()/2) %>%
  arrange(desc(number_trip))

head(popular_station, 10)
```


### Share

###### Ride distribution day of week

```{r}
All_Trip_Data_3 %>%
  group_by(member_casual, day_of_week) %>%
  summarise(number_of_ride = n(),
            .group = "drop") %>%

ggplot(aes(x = day_of_week,y =  number_of_ride, fill = member_casual)) +
  geom_col(position = "dodge", start = "identity")
  labs (title = "The Number of rides by Weekday", x = "Weekday", y = "Number of rides")
```


###### Ride distribution by month

```{r}
All_Trip_Data_3 %>%
  group_by(member_casual, month) %>%
  summarise(number_of_ride = n(),
            .groups = 'drop')

All_Trip_Data_3 %>%
  group_by(member_casual, month) %>%
  summarise(number_of_ride = n(),
            .group = "drop") %>%
  
ggplot(aes(month, number_of_ride, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "The number of rides by month", x = "Month", y = "Number of Rides")
```


###### whether ride_length can be different depends on rider type.

```{r}
All_Trip_Data_3 %>%
  group_by(member_casual, month) %>%
  summarise(mean = mean(ride_length),
            .groups = "drop") %>%
  ggplot(aes(month, mean, fill = member_casual)) +
  geom_col(position = "dodge") +
  scale_y_continuous(breaks = seq(0, 28, 2)) +
  labs(title = "The average ride length by month", x = "month", y = "avarage(min)")
```

###### The number of bike type users

```{r}
All_Trip_Data_3 %>%
  group_by(rideable_type) %>%
  summarise(number_of_ride = n ()) %>%
  ggplot(aes(rideable_type, number_of_ride, fill = rideable_type)) +
  geom_col(position = "dodge", show.legend = FALSE) +
  scale_y_continuous(labels = scales::label_number_si(),
                     breaks = seq(0, 3000000, 1000000)) +
  labs(title = "The number of cycle type usages", x = "Cycle Type", Y = "Number of cycle usages")
```

###### Cycle type usage by user type

```{r}
All_Trip_Data_3 %>%
  group_by(member_casual, rideable_type) %>%
  summarise(number_of_ride = n(),
            .groups = "drop") %>%
  ggplot(aes(member_casual, number_of_ride, fill = rideable_type)) +
  geom_col(position = "dodge") +
  labs(title = "The number of cycle type usuage by user type", x = "User type", y = "Number of bike usages")
```


###### Cycle type and average ride type

```{r}
All_Trip_Data_3 %>%
  group_by(rideable_type) %>%
  summarise(mean = mean(ride_length),
            .groups = "drop") %>%
  ggplot(aes(rideable_type, mean)) +
  geom_col(position = "dodge") +
  labs(title = "The average ride length by cycle type", y = "Average Ride Length", x = "Cycle Type")
```


###### Most popular station

```{r}
popular_station %>%
  slice(1:10) %>%
  ggplot(aes(number_trip, reorder(station_name, number_trip))) +
  geom_col() +
  labs(title = "The most visited station", x = "Number of Trips", y = "Station Name")
```


