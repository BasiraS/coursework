library(tidyverse)
library(lubridate)

########################################
# READ AND TRANSFORM THE DATA
########################################

# read one month of data
trips <- read_csv('201402-citibike-tripdata.csv')

# replace spaces in column names with underscores
names(trips) <- gsub(' ', '_', names(trips))

# convert dates strings to dates
# trips <- mutate(trips, starttime = mdy_hms(starttime), stoptime = mdy_hms(stoptime))

# recode gender as a factor 0->"Unknown", 1->"Male", 2->"Female"
trips <- mutate(trips, gender = factor(gender, levels=c(0,1,2), labels = c("Unknown","Male","Female")))


########################################
# YOUR SOLUTIONS BELOW
########################################

# count the number of trips (= rows in the data frame)
trips %>% summarize(count = n())

# find the earliest and latest birth years (see help for max and min to deal with NAs)
trips %>% 
  filter(birth_year != "\\N") %>%
  summarize(min_birthyear = min(birth_year), max_birthyear = max(birth_year))

# use filter and grepl to find all trips that either start or end on broadway
trips %>% filter (grepl('Broadway', start_station_name) | grepl('Broadway', end_station_name)) %>% select(start_station_name, end_station_name)

# do the same, but find all trips that both start and end on broadway
trips %>% filter (grepl('Broadway', start_station_name) & grepl('Broadway', end_station_name)) %>% select(start_station_name, end_station_name)

# find all unique station names
trips %>% distinct(start_station_name)
trips %>% select(start_station_name) %>% unique()
trips %>% group_by(start_station_name) %>% summarize(count = n()) %>% select(start_station_name)

# count the number of trips by gender, the average trip time by gender, and the standard deviation in trip time by gender
# do this all at once, by using summarize() with multiple arguments
trips %>% 
  group_by(gender) %>%
  summarize(count = n(),
            mean_duration = mean(tripduration) / 60,
            sd_duration = sd(tripduration) / 60)

# find the 10 most frequent station-to-station trips
trips %>%
  group_by(start_station_name, end_station_name) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  head(n=3)

# find the top 3 end stations for trips starting from each start station
trips %>%
  filter(start_station_name == end_station_name) %>%
  group_by(start_station_name) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  head(n=3)

# find the top 3 most common station-to-station trips by gender
trips %>%
  group_by(start_station_name, end_station_name, gender) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  head(n=3)

# find the day with the most trips
# tip: first add a column for year/month/day without time of day (use as.Date or floor_date from the lubridate package)
trips %>%
  mutate(startday = as.Date(starttime)) %>%
  group_by(startday) %>%
  summarize(count = n()) %>% 
  arrange(desc(count)) %>% 
  head(n=1)

trips %>% 
  group_by(startday = sub(" .*", "", starttime)) %>%
  summarize(count = n()) %>% 
  arrange(desc(count)) %>% 
  head(n=1)

# compute the average number of trips taken during each of the 24 hours of the day across the entire month
trips %>%
  mutate(hourTime = hour(floor_date(starttime,"hour"))) %>%
  group_by(hourTime) %>%
  summarize(count = n(), average_trips = count / 28) %>%
  arrange(hourTime)

# what time(s) of day tend to be peak hour(s)?
trips %>%
  mutate(hour_time = hour(floor_date(starttime,"hour"))) %>%
  group_by(hour_time) %>%
  summarize(count = n(), average_trips = count / 28) %>%
  arrange(desc(average_trips)) %>%
  head(n=1)

########################################
# CHAPTER 5
########################################

library(nycflights13)
library(tidyverse)

########################################
# SOLUTIONS
########################################

# 5.2.4 Question 1: Find all flights that 
# Had an arrival delay of two or more hours
flights %>% filter(arr_delay >= 120)

# Flew to Houston (IAH or HOU)
flights %>% filter(dest == 'IAH' | dest == 'HOU')

# Were operated by United, American, or Delta
flights %>% filter(carrier == 'UA' | carrier == 'AA' | carrier == 'DL')

# Departed in summer (July, August, and September)
flights %>% filter(month == 7 | month == 8 | month == 9) 

# Arrived more than two hours late, but didn't leave late
flights %>% filter(arr_delay >= 120 & dep_delay <= 0)

# Were delayed by at least an hour, but made up over 30 minutes in flight
flights %>% filter(dep_delay >= 60 & arr_time - dep_time > 30)

# Departed between midnight and 6am (inclusive)
flights %>% mutate(hour_time = hour(floor_date(time_hour,"hour"))) %>% filter(hour_time >= 0 & hour_time <= 6)

# 5.2.4 Question 3: How many flights have a missing dep_time? 
flights %>% filter(is.na(dep_time)) %>% summarize(count = n())
# What other variables are missing?
colnames(flights)[colSums(is.na(flights)) > 0]

# 5.5.2 Question 2: Compare air_time with arr_time - dep_time. What do you expect to see? What do you see? What do you need to do to fix it?
flights %>% mutate(flight_time = arr_time - dep_time) %>% select(air_time, flight_time)
# You would expect them to have the same value, but they don't so to fix it:
flights %>% 
  mutate(arr_mins = (arr_time / 100) * 60, dep_mins = (dep_time /100) * 60) %>%
  mutate(flight_time = arr_mins - dep_mins) %>%
  select(air_time, flight_time)

# 5.7.1 Question 3: What time of day should you fly if you want to avoid delays as much as possible?
flights %>% 
  filter (dep_delay > 0) %>% 
  group_by(hour) %>%
  summarize(average_delay = mean(dep_delay)) %>%
  arrange(average_delay)
