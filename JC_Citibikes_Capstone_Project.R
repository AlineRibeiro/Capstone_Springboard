#imports packages 
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(maps)
library(tidyverse)
library(plotly)
library(caTools)


#sets workig directory
setwd("C:/Users/aline/Desktop/Springboard/JC_Citi_Bikes_Capstone")

#sets dates to American English standards
Sys.setlocale("LC_TIME", "English_US")


#imports files individually
jc201509 <- read_csv("JC-201509-citibike-tripdata.csv")
jc201510 <- read_csv("JC-201510-citibike-tripdata.csv")
jc201511 <- read_csv("JC-201511-citibike-tripdata.csv")
jc201512 <- read_csv("JC-201512-citibike-tripdata.csv")

jc201601 <- read_csv("JC-201601-citibike-tripdata.csv")
jc201602 <- read_csv("JC-201602-citibike-tripdata.csv")
jc201603 <- read_csv("JC-201603-citibike-tripdata.csv")
jc201604 <- read_csv("JC-201604-citibike-tripdata.csv")
jc201605 <- read_csv("JC-201605-citibike-tripdata.csv")
jc201606 <- read_csv("JC-201606-citibike-tripdata.csv")
jc201607 <- read_csv("JC-201607-citibike-tripdata.csv")
jc201608 <- read_csv("JC-201608-citibike-tripdata.csv")
jc201609 <- read_csv("JC-201609-citibike-tripdata.csv")
jc201610 <- read_csv("JC-201610-citibike-tripdata.csv")
jc201611 <- read_csv("JC-201611-citibike-tripdata.csv")
jc201612 <- read_csv("JC-201612-citibike-tripdata.csv")

jc201701 <- read_csv("JC-201701-citibike-tripdata.csv")
jc201702 <- read_csv("JC-201702-citibike-tripdata.csv")
jc201703 <- read_csv("JC-201703-citibike-tripdata.csv")
jc201704 <- read_csv("JC-201704-citibike-tripdata.csv")
jc201705 <- read_csv("JC-201705-citibike-tripdata.csv")
jc201706 <- read_csv("JC-201706-citibike-tripdata.csv")
jc201707 <- read_csv("JC-201707-citibike-tripdata.csv")
jc201708 <- read_csv("JC-201708-citibike-tripdata.csv")
jc201709 <- read_csv("JC-201709-citibike-tripdata.csv")
jc201710 <- read_csv("JC-201710-citibike-tripdata.csv")
jc201711 <- read_csv("JC-201711-citibike-tripdata.csv")
jc201712 <- read_csv("JC-201712-citibike-tripdata.csv")


jc201801 <- read_csv("JC-201801-citibike-tripdata.csv")
jc201802 <- read_csv("JC-201802-citibike-tripdata.csv")
jc201803 <- read_csv("JC-201803-citibike-tripdata.csv")
jc201804 <- read_csv("JC-201804-citibike-tripdata.csv")
jc201805 <- read_csv("JC-201805-citibike-tripdata.csv")
jc201806 <- read_csv("JC-201806-citibike-tripdata.csv")
jc201807 <- read_csv("JC-201807-citibike-tripdata.csv")
jc201808 <- read_csv("JC-201808-citibike-tripdata.csv")




#converts birth year into integer, so that we can bind all data sets
jc201704$`birth year` <- as.integer(jc201704$`birth year`)
jc201705$`birth year` <- as.integer(jc201705$`birth year`)
jc201706$`birth year` <- as.integer(jc201706$`birth year`)
jc201707$`birth year` <- as.integer(jc201707$`birth year`)
jc201708$`birth year` <- as.integer(jc201708$`birth year`)
jc201709$`birth year` <- as.integer(jc201709$`birth year`)
jc201710$`birth year` <- as.integer(jc201710$`birth year`)
jc201711$`birth year` <- as.integer(jc201711$`birth year`)
jc201712$`birth year` <- as.integer(jc201712$`birth year`)

#creates list of variable names
colnames <- c("trip_duration", "start_time", "stop_time", "start_station_id", 
              "start_station_name", "start_station_latitude", "start_station_longitude", 
              "end_station_id", "end_station_name", "end_station_latitude", 
              "end_station_longitude", "bike_id", "user_type", "birth_year", "gender")


# creates a list of datasets, so that all variables are renamed at once
dfList <- list(jc201509 = jc201509, jc201510 = jc201510, 
               jc201511 = jc201511, jc201512 = jc201512,
               jc201601 = jc201601, jc201602 = jc201602, 
               jc201603 = jc201603, jc201604 = jc201604,
               jc201605 = jc201605, jc201606 = jc201606,
               jc201607 = jc201607, jc201608 = jc201608,
               jc201609 = jc201609, jc201610 = jc201610,
               jc201611 = jc201611, jc201612 = jc201612,
               jc201701 = jc201701, jc201702 = jc201702,
               jc201703 = jc201703, jc201704 = jc201704,
               jc201705 = jc201705, jc201706 = jc201706,
               jc201707 = jc201707, jc201708 = jc201708,
               jc201709 = jc201709, jc201710 = jc201710,
               jc201711 = jc201711, jc201712 = jc201712,
               jc201801 = jc201801, jc201802 = jc201802,
               jc201803 = jc201803, jc201804 = jc201804,
               jc201805 = jc201805, jc201806 = jc201806, 
               jc201807 = jc201807, jc201808 = jc201808)


#applies setNames on all data sets
list2env(lapply(dfList, setNames, colnames), .GlobalEnv)

#removes dfList to save space on global environment
remove(dfList)

#bind all data sets into one data frame
df <- bind_rows(jc201509, jc201510, jc201511, jc201512,
                jc201601, jc201602, jc201603, jc201604,
                jc201605, jc201606, jc201607, jc201608,
                jc201609, jc201610, jc201611, jc201612,
                jc201701, jc201702, jc201703, jc201704,
                jc201705, jc201706, jc201707, jc201708,
                jc201709, jc201710, jc201711, jc201712,
                jc201801, jc201802, jc201803, jc201804,
                jc201805, jc201806, jc201807, jc201808)


#adds month, year, day of week, hour of day  and week of year columns
df$month <- month(df$start_time)
df$year <- year(df$start_time)
df$year_month <- format(df$start_time, format = "%Y-%m")
df$day_of_week <- wday(df$start_time, label = TRUE)
df$hour_of_day <- hour(df$start_time)
df$week_of_year <- week(df$start_time)
df$day_of_month <- day(df$start_time)


#creates duration_minutes column   
df <- mutate(df, duration_minutes = round(trip_duration / 60)) 


#checks the data structure
str(df)
glimpse(df)
summary(df)



### seasonality


#counts number of trips per month 
trips_monthly <- df %>% group_by(year_month) %>% count()

trips_monthly

#plots number of trips per month
trips_monthly_p <- ggplot(df, aes(x = year_month)) +
  geom_bar(aes(fill=year_month))+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5), 
        legend.position="none", plot.title = element_text(hjust = 0.5))+
  scale_y_continuous("Trips count")+
  scale_x_discrete("Year and Month")+
  ggtitle("Trips count by year and month")
  

trips_monthly_p




#counts number of trips per day of the week
trips_daily <- df %>% group_by(day_of_week) %>% count()
trips_daily

#plots number of trips per day of the week
trips_daily_p <- ggplot(df, aes(x = day_of_week)) +
  geom_bar(aes(fill = day_of_week)) +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5))+
  scale_y_continuous("Trips count", breaks = seq(0, 200000, by = 40000)) +
  scale_x_discrete("User Type")+
  ggtitle("Trips count by day of the week")

trips_daily_p


#plots number of trips by hour on weekdays only 
trips_weekdays <- filter(df, day_of_week != "Sun" & day_of_week != "Sat")
trips_weekdays

trips_weekdays_plot <- ggplot(trips_weekdays, aes(x = factor(hour_of_day))) +
  geom_bar(aes(fill = factor(hour_of_day))) +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5))+
  scale_y_continuous("Trips count", breaks = seq(0, 130000, by = 20000)) +
  scale_x_discrete("Hour of the day")+
  ggtitle("Trips count by hour of the day on weekdays")

trips_weekdays_plot


#plots number of trips by hour on weekends only
trips_weekends <- filter(df, day_of_week == "Sun" | day_of_week == "Sat")

trips_weekends_plot <- ggplot(trips_weekends, aes(x = factor(hour_of_day))) +
  geom_bar(position = "dodge" ) +
  scale_y_continuous("Number of trips", breaks = seq(0, 13000, by = 2000)) +
  scale_x_discrete("Hour of day")

trips_weekends_plot


#creates variable Week and classifies days as weekend or weekday
df$week <- df$day_of_week
df$week <- gsub(pattern = "^S.*$", replacement = "weekend", df$week)
df$week <- gsub(pattern = "^[M,T,W,F].*$", replacement = "weekday", df$week)


#plots weekend and weekday on the same plot side by side
wkday_wkend_p <- ggplot(df, aes(x = factor(hour_of_day), fill = week)) +
  geom_bar(position = "dodge" ) +
  theme(legend.title=element_blank(), plot.title = element_text(hjust = 0.5))+
  scale_y_continuous("Trips count", breaks = seq(0, 1200000, by = 30000)) +
  scale_x_discrete("Hour of the day")+
  ggtitle("Trips count by hour of the day on weekdays and weekends")



wkday_wkend_p


  

###demographics


#replaces values under 1920 with NA in birth_year
df$birth_year <- replace(df$birth_year, df$birth_year < 1938, NA)
summary(df$birth_year)


#finds birth_year mode
mode_birth_year <- df %>%  group_by(birth_year) %>% count() %>%  arrange(desc(n))


#plots birth_year distribution
birth_year_p<- ggplot(df, aes(x = birth_year))+
  geom_bar()+ 
  #scale_y_continous("Number of trips", breaks = seq(0, 120000, by = 10000)) +
  scale_x_continuous("Birth Year", breaks = seq(1939, 2002, by = 2) )

birth_year_p

#makes a bar plot for gender (zero=unknown; 1=male; 2=female)
gender_distribuition <- df %>% group_by(gender) %>% count() %>%  arrange(desc(n))
gender_distribuition

gender_bar <- ggplot(df, aes(x = factor(gender), fill = factor(gender))) +
  geom_bar() +
  scale_x_discrete("Users' genders", breaks=c("0", "1", "2"),labels=c("Unknown", "Male", "Female")) +
  scale_fill_discrete("Gender", breaks=c("0", "1", "2"),labels=c("Unknown", "Male", "Female")) +
  scale_y_continuous("Number of trips")

gender_bar




####User type  


#counts number of trips made by user type (customers and subscribers)
user <- df %>% group_by(user_type) %>% tally() 
user


#plots user type distribution 
user_p <- ggplot(df, aes(x = user_type)) +
  geom_bar() +
  scale_y_continuous(breaks = seq(0, 900000, 50000))



#groups usertpe by gender
user_gender <- df %>% group_by(user_type, gender) %>% tally() %>%  arrange(n)

user_gender

#plots user gender and usertype
user_gender_plot <- ggplot(df, aes(x = user_type, fill = factor(gender))) +
  geom_bar(position = "dodge" ) +
  scale_fill_discrete("Gender", breaks=c("0", "1", "2"),labels=c("Unknown", "Male", "Female")) +
  scale_y_continuous("Number", breaks = seq(0, 900000, by = 50000)) +
  scale_x_discrete("User Type")

user_gender_plot


#plots a user_gender as a stacked bar plot
user_gender_stack <- ggplot(df, aes(x = user_type, fill = factor(gender))) +
  geom_bar(position = "stack" ) +
  scale_fill_discrete("Gender", breaks=c("0", "1", "2"),labels=c("Unknown", "Male", "Female")) +
  scale_y_continuous("Number", breaks = seq(0, 900000, by = 50000)) +
  scale_x_discrete("User Type")

user_gender_stack



####trips 

#checks which startstationname is more frequent
origin <- df %>%  group_by(start_station_name) %>% count() %>%  arrange(desc(n))
origin

#checks which endstation name is more frequent
destination <- df %>%  group_by(end_station_name) %>%  count %>%  arrange(desc(n))
destination


#checks which combination of start and station is more frequent
combination<- df %>% group_by(start_station_name, end_station_name) %>% tally() %>%  arrange(desc(n))
combination


#checks most popular journey by time of day
combination_hour<- df %>% group_by(start_station_name, end_station_name, hour_of_day) %>% tally() %>%  arrange(desc(n))
combination_hour


#plots an animates bubble chart for start station 

trips_origin <- df %>% group_by(start_station_name, start_station_longitude, 
                           start_station_latitude, hour_of_day) %>% summarise(n=n())


p <- ggplot(trips_origin, aes(x = start_station_longitude, y = start_station_latitude, 
                         col = start_station_name, size = n)) + 
  geom_point(aes(frame = hour_of_day))+ 
  theme(legend.position = "none")

p <- ggplotly(p)
         
p  



#add colums intervals 
df$intervals <- df$duration_minutes

#creates function to identify intervals for trip duration_minutes
my_fun <- function(x) {
  if (x <= 60) {
    if(x %% 2 == 1) {
      paste(x + 1)
    } else {
      paste(x)
    }
  } else if (x >= 61 ) {
    paste(61)
  }
  
}

df$intervals <- sapply(df$intervals, my_fun)
df$intervals <- as.integer(df$intervals)
glimpse(df)


#plots a histogram for intervals
#remember that te value at 61 means "Others"
duration_p <- ggplot(df, aes(x = intervals)) +
  geom_bar(aes(fill = factor(intervals)))+
  theme(legend.position = "none",  plot.title = element_text(hjust = 0.5))+
  scale_y_continuous("Trips count", breaks = seq(0, 250000, 50000 )) +
  scale_x_continuous("Trip duration up to x minutes", breaks = seq(0, 60, by = 2))+ 
  ggtitle("Trips count by trip duration")
  


duration_p


### Bike Ids

#counts bike_ids
n_distinct(df$bike_id)

#counts how many times each bikeid is displayed 
journeys_ind <- df %>% group_by(bike_id) %>% count() %>% arrange(desc(n))
journeys_ind

#calculates average number of trips made by each bike id
avg_trips <- mean(journeys_ind$n)
avg_trips


#claculates bike_id life
bikeid_life <- df %>%  group_by(bike_id) %>%
  summarise(min_date = min(start_time), max_date = max(stop_time), 
            life = min_date - max_date)

bikeid_life

mean(bikeid_life$life) 





#imports the weather data set
weather <- read_csv("weather.csv")
glimpse(weather)

#selects meaninful variables 
weather_clean <- subset(weather, select = c(DATE, HOURLYPRSENTWEATHERTYPE,  
                                            HOURLYDRYBULBTEMPC, HOURLYRelativeHumidity, 
                                            HOURLYWindSpeed, HOURLYPrecip))


#creates year, month, day_of_month and hour_of_day columns
weather_clean$year <- year(weather_clean$DATE)
weather_clean$month <- month(weather_clean$DATE)
weather_clean$day_of_month <- day(weather_clean$DATE)
weather_clean$hour_of_day <- hour(weather_clean$DATE)


#creates id column for the weather data set
weather_clean$id <- paste(weather_clean$year, weather_clean$month, 
                          weather_clean$day_of_month, weather_clean$hour_of_day)





#deletes repeated ids ending in 23, as they are all NAs from weather_clean (main data set)
weather_clean_23h <- filter(weather_clean, hour_of_day == 23 )
weather_clean_23h <- weather_clean_23h[!duplicated(weather_clean_23h$id, fromLast=T), ]



#removes weather_clean_23h from weather_clean 
weather_clean <- weather_clean %>% anti_join(weather_clean_23h)



#deletes repeated ids, so we can merge data frames without duplicates
weather_clean <- weather_clean[!duplicated(weather_clean$id, fromLast=T), ]



#removes year, month, day_of_month and hour_of_day from the weather data set
weather_clean$year <- NULL
weather_clean$month <- NULL
weather_clean$day_of_month <- NULL
weather_clean$hour_of_day <- NULL
weather_clean$DATE <-NULL


#renames columns on the weather_clean data set
colnames(weather_clean) <- c("weather_type", "temperature", "humidity", "wind_speed", "precipitation", "id")


#creates id column for the df  data set
df$id <- paste(df$year, df$month, df$day_of_month, df$hour_of_day)

#merges df and weather_clean
df_merged <- left_join(df, weather_clean, by = "id")


#reorganizes data set
df_merged <- df_merged[c(26, 2, 3, 1, 4:25, 28, 29, 30, 31, 27)]


glimpse(df_merged)











#creates drizzle, rain, snow, ice pellets and thunder columns 
df_merged$drizzle <- df_merged$weather_type
df_merged$rain <- df_merged$weather_type
df_merged$snow <- df_merged$weather_type
df_merged$ice_pellets <- df_merged$weather_type
df_merged$thunder <- df_merged$weather_type


#replaces weather type's columns with boolean
df_merged$drizzle <- grepl("^.*DZ.*$", df_merged$drizzle)
df_merged$rain <- grepl("^.*RA.*$", df_merged$rain)
df_merged$snow <- grepl("^.*SN.*$", df_merged$snow)
df_merged$ice_pellets <- grepl("^.*PL.*$", df_merged$ice_pellets)
df_merged$thunder <- grepl("^.*TS.*$", df_merged$thunder)


#replaces boolean with wither 1 or 0
df_merged$drizzle <- as.numeric(df_merged$drizzle)
df_merged$rain <- as.numeric(df_merged$rain)
df_merged$snow <- as.numeric(df_merged$snow)
df_merged$ice_pellets <- as.numeric(df_merged$ice_pellets)
df_merged$thunder <- as.numeric(df_merged$thunder)


#creates season column
df_merged$season <- df_merged$month

#identifies each season of the year
df_merged$season <- gsub(pattern = "12|1|2", replacement = 1, x = df_merged$season)
df_merged$season <- gsub(pattern = "3|4|5", replacement = 2, x = df_merged$season)
df_merged$season <- gsub(pattern = "6|7|8", replacement = 3, x = df_merged$season)
df_merged$season <- gsub(pattern = "9|10|11", replacement = 4, x = df_merged$season)


#creates working_day column
df_merged$holiday <- df_merged$id

remove(duration_p)
remove(gender_bar)


#identifies holidays
df_merged$holiday <- gsub(pattern = "2015 10 12 .*|2015 11 3 .*|2015 11 11 .*", replacement = 1, x = df_merged$holiday)
df_merged$holiday <- gsub(pattern = "2016 1 1 .*|2016 1 18 .*|2016 2 15 .*", replacement = 1, x = df_merged$holiday)
df_merged$holiday <- gsub(pattern = "2016 3 25 .*|2016 5 30 .*|2016 7 4 .*", replacement = 1, x = df_merged$holiday)
df_merged$holiday <- gsub(pattern = "2016 9 5 .*|2016 10 10 .*|2016 11 11 .*", replacement = 1, x = df_merged$holiday)
df_merged$holiday <- gsub(pattern = "2016 11 24 .*|2016 12 26 .*|2017 1 2 .*", replacement = 1, x = df_merged$holiday)
df_merged$holiday <- gsub(pattern = "2017 1 16 .*|2017 2 20 .*|2017 4 14 .*", replacement = 1, x = df_merged$holiday)
df_merged$holiday <- gsub(pattern = "2017 5 29 .*|2017 7 4 .*|2017 9 4 .*", replacement = 1, x = df_merged$holiday)
df_merged$holiday <- gsub(pattern = "2017 10 9 .*|2017 11 7 .*|2017 11 10 .*", replacement = 1, x = df_merged$holiday)
df_merged$holiday <- gsub(pattern = "2017 11 23 .*|2017 12 25 .*|2018 1 1 .*", replacement = 1, x = df_merged$holiday)
df_merged$holiday <- gsub(pattern = "2018 1 15 .*|2018 2 19 .*|2018 3 30 .*", replacement = 1, x = df_merged$holiday)
df_merged$holiday <- gsub(pattern = "2018 5 28 .*|2018 7 4 .*", replacement = 1, x = df_merged$holiday)
df_merged$holiday <- gsub(pattern = "2.*", replacement = 0, x = df_merged$holiday)



#replace T in precipitation for 0
df_merged$precipitation <- gsub(pattern = "T", replacement = 0, x = df_merged$precipitation)

#replaces NA in precipitation for 0
df_merged$precipitation <- replace(df_merged$precipitation, is.na(df_merged$precipitation), 0) 
df_merged$precipitation <- (gsub("[^0-9.]", "", df_merged$precipitation)) 



#replaces NA values in the humidity column
df_merged$humidity <- replace(df_merged$humidity, is.na(df_merged$humidity), 32) 


#replaces NA values in the temperature column
df_merged$temperature[103166:103184] <- 15
df_merged$temperature[155384:155416] <- 29

#df_merged - this is the final clean version of the entire data set

write.csv(df_merged,file = file.choose(new = T))

#cretes data set only with columns that will be used in the model
df_merged2 <- df_merged[c(1, 17, 18, 20:23, 25, 27:30, 32:38)]

#count trips per hour
trips_hourly <- (df_merged2 %>% group_by(id) %>% count())


#merges the count of hourly trips dataframe to the df_ml data frame
df_ml <- left_join(df_merged2, trips_hourly, by = "id")


#deletes duplicated row, as all ids and n have the same characteristics
df_ml <- df_ml[!duplicated(df_ml$id), ]


#plots temperature against number of trips
temp_count <- ggplot(df_ml, aes(x = temperature, y = n))+
  geom_point(aes(color = temperature))+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))+
  scale_x_continuous("Temperature", breaks = seq(-20, 40, by = 2))+
  scale_y_continuous("Trips count", breaks = seq( 0, 340, by = 20))+
  ggtitle("Bike Rides by Temperature")+
  scale_color_gradientn(colors=c('dark blue','blue','light blue','light green','yellow','orange','red')) 

temp_count

#plots humidity against number of trips
humidity_count <- ggplot(df_ml, aes(x = humidity, y = n))+
  geom_point(aes(color = humidity))+ 
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_gradient(high='purple',low='green')+
  scale_x_continuous(" Relative Himidity", breaks = seq(0, 100, by = 20))+
  scale_y_continuous("Trips count", breaks = seq( 0, 340, by = 20))+
  ggtitle("Bike Rides by Relative Humidity ")

humidity_count


#plots wind_speed against number of trips
winds_count <- ggplot(df_ml, aes(x = wind_speed, y = n))+
  geom_jitter(aes(color = wind_speed))+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))+
  scale_x_continuous("Wind Speed", breaks = seq(0, 50, by = 5))+
  scale_y_continuous("Trips count", breaks = seq( 0, 340, by = 20))+
  ggtitle("Bike Rides by Wind Speed")+
  scale_color_gradientn(colors=c('dark blue','blue','light blue','light green','yellow','orange','red'))

winds_count




#split the dataset into training and test
set.seed(123)   
sample <- sample.split(df_ml, SplitRatio = 0.75) 
train <- subset(df_ml,sample==TRUE) 
test <- subset(df_ml, sample==FALSE)

#creates csv's for the train and test data sets
write.csv(train,file = file.choose(new = T))
write.csv(test,file = file.choose(new = T))

