########################################
# load libraries
########################################

# load some packages that we'll need
library(tidyverse)
library(scales)

# be picky about white backgrounds on our plots
theme_set(theme_bw())

# load RData file output by load_trips.R
load('trips.RData')


########################################
# plot trip data
########################################

# plot the distribution of trip times across all rides (compare a histogram vs. a density plot)
# Histogram
trips %>% filter(tripduration < 3600) %>% ggplot(aes(x = tripduration)) + geom_histogram()

# Density
trips %>% filter(tripduration < 3600) %>% ggplot(aes(x = tripduration)) + geom_density(fill = "grey")

# plot the distribution of trip times by rider type indicated using color and fill (compare a histogram vs. a density plot)
# Histogram
trips %>% 
  filter(tripduration < 3600) %>% 
  ggplot(aes(x = tripduration, colour = usertype)) + 
    geom_histogram(position = "identity", alpha = 0.25) + 
    facet_wrap(~ usertype, scales = "free")

# Density
trips %>% 
  filter(tripduration < 3600) %>% 
  ggplot(aes(x = tripduration, colour = usertype, )) + 
    geom_density(fill = "grey") + 
    facet_wrap(~ usertype, scales = "free")

# plot the total number of trips on each day in the dataset
trips %>% 
  mutate(day = as.Date(starttime)) %>% 
  group_by(day) %>% 
  summarize(num_trips = n()) %>% 
  ggplot(aes(x = day, y = num_trips)) + 
    geom_line() + 
    scale_y_continuous(label = comma) + 
    xlab('Day') + ylab('Number of Trips')

trips %>% 
  mutate(day = as.Date(starttime)) %>%
  ggplot(aes(x = day)) +
    geom_histogram()+ 
    scale_y_continuous(label = comma) + 
    xlab('Day') + ylab('Number of Trips')

trips %>% 
  mutate(day = factor((weekdays(as.POSIXct(starttime), abbreviate = F)), levels=c("Monday","Tuesday","Wednesday","Thursday","Friday"))) %>%
  ggplot(aes(x = day)) + 
  geom_histogram(stat = "count") +
  scale_y_continuous(label = comma) + 
  xlab('Day') + ylab('Number of Trips')

# plot the total number of trips (on the y axis) by age (on the x axis) and gender (indicated with color)
trips %>% 
  mutate(age = 2014 - birth_year) %>%
  group_by(age, gender) %>%
  summarize(num_trips = n()) %>%
  filter(num_trips <= 600000) %>%
  ggplot(aes(x = age, y = num_trips, colour = gender)) + 
    geom_point() +
    scale_y_continuous(label = comma)

trips %>% 
  mutate(age = 2014 - birth_year) %>%
  group_by(age) %>%
  ggplot(aes(x = age, colour = gender)) +
  geom_histogram(position = "identity", alpha = 0.25) + 
  scale_y_continuous(label = comma) +
  facet_wrap(~ gender, scales = "free")

# plot the ratio of male to female trips (on the y axis) by age (on the x axis)
# hint: use the spread() function to reshape things to make it easier to compute this ratio
# (you can skip this and come back to it tomorrow if we haven't covered spread() yet)
trips %>% 
  mutate(age = 2014 - birth_year) %>%
  group_by(age, gender) %>%
  summarize(count = n()) %>%
  pivot_wider(names_from = gender, values_from = count) %>%
  mutate(ratio = Male / Female) %>%
  ggplot(aes(x = age, y = ratio)) +
  geom_point()
  
########################################
# plot weather data
########################################
# plot the minimum temperature (on the y axis) over each day (on the x axis)
weather %>% 
  ggplot(aes(x = ymd, y = tmin)) +
    geom_point() +
    geom_smooth(se = FALSE)

# plot the minimum temperature and maximum temperature (on the y axis, with different colors) over each day (on the x axis)
# hint: try using the gather() function for this to reshape things before plotting
# (you can skip this and come back to it tomorrow if we haven't covered gather() yet)
weather %>%
  select(tmin, tmax, ymd) %>%
  pivot_longer(names_to = "temp_type", values_to = "temp", cols = c('tmin', 'tmax')) %>%
  ggplot(aes(x = ymd, y = temp, group = temp_type)) +
  geom_point(aes(colour = temp_type)) +
  geom_smooth(se = FALSE)

########################################
# plot trip and weather data
########################################

# join trips and weather
trips_with_weather <- inner_join(trips, weather, by="ymd")

# plot the number of trips as a function of the minimum temperature, where each point represents a day
# you'll need to summarize the trips and join to the weather data to do this
trips_with_weather %>% 
  group_by(tmin, ymd) %>%
  summarize(num_trips = n()) %>%
  ggplot(aes(x = tmin, y = num_trips)) + 
  geom_point(aes(colour = ymd)) +
  scale_y_continuous(label = comma)

# repeat this, splitting results by whether there was substantial precipitation or not
# you'll need to decide what constitutes "substantial precipitation" and create a new T/F column to indicate this
trips_with_weather %>%
  mutate(prcp_chance = (prcp > 0)) %>%
  group_by(tmin, ymd, prcp_chance) %>%
  summarize(num_trips = n()) %>%
  ggplot(aes(x = tmin, y = num_trips)) + 
  geom_point(aes(colour = prcp_chance)) +
  scale_y_continuous(label = comma)

# add a smoothed fit on top of the previous plot, using geom_smooth
trips_with_weather %>%
  mutate(prcp_chance = (prcp > 0.5)) %>%
  group_by(tmin, ymd, prcp_chance) %>%
  summarize(num_trips = n()) %>%
  ggplot(aes(x = tmin, y = num_trips)) + 
  geom_point(aes(colour = prcp_chance)) +
  geom_smooth(aes(colour = prcp_chance), se = FALSE) +
  scale_y_continuous(label = comma)

# compute the average number of trips and standard deviation in number of trips by hour of the day
# hint: use the hour() function from the lubridate package
# plot the above
trips_with_weather %>%
  mutate(hourTime = hour(starttime)) %>%
  group_by(hourTime, ymd) %>%
  summarize(count = n()) %>%
  group_by(hourTime) %>%
  summarize(average_trips = mean(count), sd_trips = sd(count)) %>%
  pivot_longer(names_to = "computation", values_to = "result", cols = c('average_trips', 'sd_trips')) %>%
  ggplot(aes(x = hourTime, y = result)) +
  geom_point() +
  scale_y_continuous(label = comma) +
  facet_wrap(~ computation, scales = "free")

# repeat this, but now split the results by day of the week (Monday, Tuesday, ...) or weekday vs. weekend days
# hint: use the wday() function from the lubridate package
trips_with_weather %>%
  mutate(hourTime = hour(starttime)) %>%
  group_by(hourTime, wday(ymd)) %>%
  summarize(count = n()) %>%
  group_by(hourTime) %>%
  summarize(average_trips = mean(count), sd_trips = sd(count)) %>%
  pivot_longer(names_to = "computation", values_to = "result", cols = c('average_trips', 'sd_trips')) %>%
  ggplot(aes(x = hourTime, y = result)) +
  geom_point() +
  scale_y_continuous(label = comma) +
  facet_wrap(~ computation, scales = "free")

########################################
# Chapter 3
########################################

library(tidyverse)

ggplot2::mpg

########################################
# SOLUTIONS
########################################

# 3.3.1 

# Exercise 1: What's gone wrong with this code? Why are the points not blue?
#Original Code: 
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = "blue"))
#Modified Code:
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), color = "blue")

# Exercise 2: Which variables in mpg are categorical? Which variables are continuous? (Hint: type ?mpg to read the documentation for the dataset). How can you see this information when you run mpg?
#Categorical variables: manufacturer, model, year, cyl, trans, drv, fl, class
#Continuous variables: displ, cty, hwy

# Exercise 3: Map a continuous variable to color, size, and shape. How do these aesthetics behave differently for categorical vs. continuous variables?
# It works well for categorical since there is a finite set, however, it does not work so well for 
# continuous variables because of the various values types. Color and Size still can work, however, it cannot be mapped to shape

# 3.5.1

# Exercise 1: What happens if you facet on a continuous variable?
# The continuous variable is converted to a categorical variable, and the plot contains a facet for each distinct value.

# Exercise 4: What are the advantages to using faceting instead of the color aesthetic? What are the disadvantages? How might the balance change if you had a larger data set?
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow = 2)
# The advantage is that you get to see a more detail view of the data, easier to distinguish
# The disadvantage is that it becomes difficult to compare the values of observations between categories since the observations for each category are on different plots.

# 3.6.1 

# Exercise 5: Will these two graphs look different? Why/why not?
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point() + 
  geom_smooth()

ggplot() + 
  geom_point(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_smooth(data = mpg, mapping = aes(x = displ, y = hwy))

# The two graphs will not be different because both geom_point() and geom_smooth() will use the same data and mappings. 
# They will inherit those options from the ggplot() object, so the mappings don't need to specified again.

# Exercise 6: Recreate the R code necessary to generate the following graphs.
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  geom_smooth(se = FALSE)

ggplot(mpg, aes(x = displ, y = hwy, group = drv)) +
  geom_point() +
  geom_smooth(se = FALSE) 

ggplot(mpg, aes(x = displ, y = hwy, colour = drv)) +
  geom_point() +
  geom_smooth(se = FALSE)

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(colour = drv)) +
  geom_smooth(se = FALSE)

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(colour = drv)) +
  geom_smooth(aes(linetype = drv), se = FALSE)

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(size = 4, color = "white") +
  geom_point(aes(colour = drv))

# 3.8.1

# Question 1: What is the problem with this plot? How could you improve it?
# Original Code: 
ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) + 
  geom_point()

# Modified Code: 
ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
  geom_point(position = "jitter")

# Question 2: What parameters to geom_jitter() control the amount of jittering?
# There are two parameters that control the amount of jittering: width and height


########################################
# Chapter 12
########################################

library(tidyverse)

ggplot2::mpg

########################################
# SOLUTIONS
########################################


# 12.2.1

# Exercise 2: Using prose, describe how the variables and observations are organized in each of the sample tables.
# Table 1: Observations: country and year; Variables: cases and population
# Table 2: Observations: country, year, and type of count; Variable: the count (value)
# Table 3: Observations: country and year; Variable: rate
# Table 4a: Observations: country; Variable: year 1999 and 2000 (value for cases)
# Table 4b: Observations: country; Variable: year 1999 and 2000 (value for population)

# 12.3.3

# Exercise 1: Why are pivot_longer() and pivot_wider() not perfectly symmetrical?
# The two are not symmetrical because of the variation on column. 

# Exercise 3: What would happen if you widen this table? Why? How could you add a new column to uniquely identify each value?
people <- tribble(
  ~name,             ~names,  ~values,
  #-----------------|--------|------
  "Phillip Woods",   "age",       45,
  "Phillip Woods",   "height",   186,
  "Phillip Woods",   "age",       50,
  "Jessica Cordero", "age",       37,
  "Jessica Cordero", "height",   156
)
pivot_wider(people, names_from = "names", values_from = "values")
# It will fail because columns do not uniquely identity rows

# Modification:
people %>% 
  group_by(name, names) %>% 
  mutate(identify = row_number()) %>%
  pivot_wider(names_from = "names", values_from = "values")
