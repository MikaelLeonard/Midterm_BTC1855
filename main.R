# Mikael Gouwtama
# 1007128127
# BTC 1855H Midterm Project 

# Read the 3 datasets

station <- read.csv("./datasets/station.csv")
trip <- read.csv("./datasets/trip.csv")
weather <- read.csv("./datasets/weather.csv")

#############################################
### Inspect and setup the station dataset ###
#############################################

# Create a station working dataset to keep a copy of the original
station_w <- station

# Get the summary and structures of the station dataset's variables
summary(station_w)
str(station_w)

# The installation_date column is of character structure when it should be in date
library(lubridate)
station_w$installation_date <- mdy(station_w$installation_date)

# There is no missing data visible when looking at the dataset, but just making 
# sure, we can confirm there is 0 missing value in this dataset
sum(is.na(station_w))

#############################################
### Inspect and setup the weather dataset ###
#############################################

library(dplyr)
library(tidyr)

# create a weather working dataset to keep a copy of the original
weather_w <- weather

# Get the summary and structures of the weather dataset's variables
summary(weather_w)
str(weather_w)

# Ensure that there are no duplicated entries
nrow(unique(weather_w))

# It appears that the dataset is a collection of weather recordings for the year
# 2014, for 5 different cities
unique(weather_w$city)
# Let's make sure that each city has equally 365 recordings, which they all have!
length(which(weather_w$city == "San Francisco"))
length(which(weather_w$city == "Redwood City"))
length(which(weather_w$city == "Palo Alto"))
length(which(weather_w$city == "Mountain View"))
length(which(weather_w$city == "San Jose"))

# The date also seems to be in an incorrect structure, let's convert them to date
weather_w$date <- mdy(weather_w$date)

str(weather_w$date)

# There seems to be a lot of NAs in the max gust speed column. Let's inspect the 
# observations with missing gust to see whether we should remove them or not
missing_gust <- weather_w %>%
  filter(is.na(max_gust_speed_mph))
# We'll not remove them as it could simply indicate that there aren't any gust
# on those recorded dates for that city

# There seems to be 9 NAs in the max, mean, and min visibility recordings. Let's
# inspect them to see whether it is necessary to remove them or not
missing_visibility <- weather_w %>%
  filter(is.na(max_visibility_miles))
# It seems that all the missing visibility values are in the Palo Alto's recordings.
# To ensure that each city has a complete weather recordings, we'll not remove them.

# Let's look at the precipitation_inches column now
head(weather_w$precipitation_inches, 10)
typeof(weather_w$precipitation_inches)
# Interestingly, there are a combination of decimals, integers, and characters (T) 
# in this column. As a result, they are of type character. Lets arrange them now.
# And look at the top and bottom values.
weather_w <- weather_w %>% arrange(precipitation_inches)
head(weather_w$precipitation_inches)
tail(weather_w$precipitation_inches)
# T seems to mean trace amounts and it should be between 0.00 and 0.01. Here, I will
# arbitrarily choose 0.001 as a replacement for the T values.
weather_w <- weather_w %>%
  mutate(precipitation_inches = case_when(
    precipitation_inches == 'T' ~ '0.001',
    .default = precipitation_inches)) %>%
  mutate(precipitation_inches = as.numeric(precipitation_inches))
# Final inspection of the updated precipitation inches values
str(weather_w$precipitation_inches)
summary(weather_w$precipitation_inches)
unique(weather_w$precipitation_inches)

##########################################
### Inspect and setup the trip dataset ###
##########################################

# Create a trip working dataset to keep a copy of the original
trip_w <- trip

# Get the summary and structures of the trip dataset's variables
summary(trip_w)
str(trip_w)

# Ensure that there are no duplicated entries
nrow(unique(trip_w))

# The start_date and end_date seems to be in an incorrect structure, let's 
# convert them to date
trip_w <- trip_w %>%
  mutate(start_date = mdy_hm(trip_w$start_date),
         end_date = mdy_hm(trip_w$end_date))

# The zip code also seems to be in an incorrect structure. It is now listed as a 
# character, which we can convert into numbers to match the ones in the weather
# dataset. We can also see that some of the zip code values are erroneous. 

# Assuming that the users are in USA (since all the trips involve places within
# the states), the US zip codes should only include numbers, ranging between 
# 00501 to 99950 (although 00501 might be inputted as 501). Thus, we can also 
# make zip code values ranging outside 501-99950 as NA. This also include those
# with letters in the zip codes.
trip_w <- trip_w %>%
  mutate(zip_code = as.numeric(zip_code)) %>%
  mutate(zip_code = ifelse((zip_code >= 501 & zip_code <= 99950), zip_code, NA))

# Let's see the resulting zip codes from highest to lowest and vice-versa 
head(sort(trip_w$zip_code, decreasing = T), 10)
head(sort(trip_w$zip_code, decreasing = F), 10)

###########################################
### EDA of the trip and weather dataset ###
###########################################

# All the subsequent EDAs will be performed following the tutorial in 
# https://blog.datascienceheroes.com/exploratory-data-analysis-in-r-intro/ 

# Setting-up - installing packages required. Uncomment the following code if they
# are not already installed. I have only installed tidyverse, so I'll uncomment
# the remaining two

# install.packages("tidyverse")
# install.packages("funModeling")
# install.packages("Hmisc")

# Loading the packages
library(funModeling) 
library(tidyverse) 
library(Hmisc)

# tl;dr code
basic_eda <- function(data)
{
  glimpse(data)
  print(status(data))
  freq(data) 
  print(profiling_num(data))
  plot_num(data)
  describe(data)
}

# Performing the EDA on the workig trip dataset (trip_w)
basic_eda(trip_w)

# Performing the EDA on the working weather dataset (weather_w)
basic_eda(weather_w)

######################################
### Create a cancelled trip column ###
######################################

# Create a cancelled trip column when the start station name is the same as the
# end station name and the duration is less than 3 minutes (180 seconds)
trip_with_can <- trip_w %>%
  mutate(cancelled_trip = ifelse(start_station_name == end_station_name & duration < 180, 'Yes', 'No'))

# Find out the number of cancelled trips, where there are 1082 trips being cancelled
freq(trip_with_can$cancelled_trip)
describe(trip_with_can$cancelled_trip)

cancelled_trip_count <- nrow(trip_with_can %>%
         filter(cancelled_trip == "Yes"))

cancelled_trip_count

# Record the trip ids for cancelled trips
cancelled_trip_ids <- trip_with_can %>% 
  filter(cancelled_trip == 'Yes') %>% 
  select(id)

cancelled_trip_ids

# Save the cancelled trip ids into a csv file
#write.csv(cancelled_trip_ids, "cancelled_trip_ids.csv", row.names = FALSE)

# Removing the cancelled trips from the working trip dataset
trip_uncancelled <- trip_with_can %>%
  filter(cancelled_trip == "No")

#############################################
### Removing outliers in the trip dataset ###
#############################################

# Let's first re-visualize the uncancelled trip data
plot_num(trip_uncancelled)

# Immediately, we can see that there are potential outliers in the duration column
# The outlier is that there might be a few trips with really high durationr recorded
# Let's first look at them!
sort(trip_uncancelled$duration, decreasing = T)
# There indeed are a lot of trips with really high duration recorded
# We can visualize these outliers again using boxplot
boxplot(trip_uncancelled$duration,
        main = "Boxplot of Trip Duration",
        xlab = "Trips",
        ylab = "Duration (seconds)")

# Let's degine the limits for the outliers
# For the outliers, we can begin begin by defining the quantiles
Q1 <- quantile(trip_uncancelled$duration, 0.25)
Q3 <- quantile(trip_uncancelled$duration, 0.75)
IQR <- Q3 - Q1

# Define the limits for non-outlier data
# Here, the outliers are defined as any values greater or less than the 1.5 
# interquartile range (IQR)
lower_limit <- Q1 - 1.5 * IQR
upper_limit <- Q3 + 1.5 * IQR

# Identify and record the trip ids of the outliers
outliers_duration <- trip_uncancelled %>% 
  filter(duration < lower_limit | duration > upper_limit)

outlier_ids <- outliers_duration$id

outlier_ids

# Save the outlier ids into a csv file
#write.csv(outlier_ids, "outlier_ids.csv", row.names = FALSE)

# There are 24873 trips that's considered as outliers, which is roughly 7.6 % 
# of the uncancelled trips
length(outlier_ids)

# Remove outliers from the dataset
trip_no_outliers <- trip_uncancelled %>% 
  filter(duration >= lower_limit & duration <= upper_limit)

# Display the outlier trip ids and the cleaned dataset
outlier_ids
trip_no_outliers

# Let's now vizualize the cleaned dataset without the outliers
plot_num(trip_no_outliers)

boxplot(trip_no_outliers$duration,
        main = "Boxplot of Trip Duration with Outliers Removed",
        xlab = "Trips",
        ylab = "Duration (seconds)")

hist(trip_no_outliers$duration,
     main = "Histogram of Trip Duration with Outliers Removed",
     xlab = "Trup duration (seconds)",
     ylab = "Frequency")

############################
### Establish rush hours ###
############################

# Let's start by getting the hours and days of the start_date from each trip
trip_hm <- trip_no_outliers %>%
  mutate(hour = hour(start_date),
         weekday = wday(start_date, label = TRUE))

# Now, filtering the weekday trips only (excluding saturdays and sundays)
trip_weekdays <- trip_hm %>%
  filter(!weekday %in% c("Sat", "Sun"))

# Obtain the count of trips for each hour 
hourly_volume <- trip_weekdays %>%
  group_by(hour) %>%
  summarise(trip_count = n()) %>%
  arrange(desc(trip_count))

# Plot the rush hour trip volumes
library(ggplot2)
ggplot(hourly_volume, aes(x = hour, y = trip_count)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Trip Counts by Hour on Weekdays", x = "Hour of the Day", y = "Trip Count") +
  theme_grey()

# Looking at the plot, we can determine that the rush hours are in the morning at
# 8 and 9 AM and in the afternoon at 5 and 6 PM

###################################
### Weekday rush hours stations ###
###################################

# Establish a vector for the rush hours
rush <- c(8, 9, 17, 18)

# Filter the weekday trip datasets to get only the trips in those rush hours
trip_rush_weekday <- trip_weekdays %>%
  filter(hour %in% rush)

# Obtain the top 10 starting bike stations from the weekday rush hours
top_start_stations_weekday <- trip_rush_weekday %>%
  group_by(start_station_name) %>%
  summarise(trip_count = n()) %>%
  arrange(desc(trip_count)) %>%
  head(10)

top_start_stations_weekday

# Plot the most frequent starting stations
ggplot(top_start_stations_weekday, aes(x = reorder(start_station_name, -trip_count), y = trip_count)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +
  labs(title = "Top 10 Starting Stations During Rush Hours", x = "Station", y = "Trip Count") +
  theme_grey()

# Obtain the top 10 ending bike stations from the weekday rush hours
top_end_stations_weekday <- trip_rush_weekday %>%
  group_by(end_station_name) %>%
  summarise(trip_count = n()) %>%
  arrange(desc(trip_count)) %>%
  head(10)

top_end_stations_weekday

# Plot the most frequent ending stations
ggplot(top_end_stations_weekday, aes(x = reorder(end_station_name, -trip_count), y = trip_count)) +
  geom_bar(stat = "identity", fill = "red") +
  coord_flip() +
  labs(title = "Top 10 Ending Stations During Rush Hours", x = "Station", y = "Trip Count") +
  theme_grey()


################################
### Weekend popular stations ###
################################

# Now, filtering the weekend trips only (only saturdays and sundays)
trip_weekends <- trip_hm %>%
  filter(weekday %in% c("Sat", "Sun"))

# Obtain the top 10 starting bike stations during the weekends
top_start_stations_weekend <- trip_weekends %>%
  group_by(start_station_name) %>%
  summarise(trip_count = n()) %>%
  arrange(desc(trip_count)) %>%
  head(10)

top_start_stations_weekend

# Plot the most frequent starting stations during weekends
ggplot(top_start_stations_weekend, aes(x = reorder(start_station_name, -trip_count), y = trip_count)) +
  geom_bar(stat = "identity", fill = "purple") +
  coord_flip() +
  labs(title = "Top 10 Starting Stations During Weekends", x = "Station", y = "Trip Count") +
  theme_grey()

# Obtain the top 10 ending bike stations during the weekends
top_end_stations_weekend <- trip_weekends %>%
  group_by(end_station_name) %>%
  summarise(trip_count = n()) %>%
  arrange(desc(trip_count)) %>%
  head(10)

top_end_stations_weekend

# Plot the most frequent starting stations during weekends
ggplot(top_end_stations_weekend, aes(x = reorder(end_station_name, -trip_count), y = trip_count)) +
  geom_bar(stat = "identity", fill = "orange") +
  coord_flip() +
  labs(title = "Top 10 Ending Stations During Weekends", x = "Station", y = "Trip Count") +
  theme_grey()

########################################
### Average monthly bike utilization ###
########################################

# Extract the month for each trip from the start_date column
trip_month <- trip_hm %>%
  mutate(month = month(start_date))
  
# Summarize total duration and number of trips for each month
monthly_usage <- trip_month %>%
  group_by(month) %>%
  summarise(total_duration = sum(duration), 
            total_trips = n())

monthly_usage

# Assuming the total number of bikes is the number of unique bike_id in the trip dataset
total_bikes <- n_distinct(trip_month$bike_id)

# Calculate the number of days in each month using the days_in_month function from
# lubridate package
monthly_usage <- monthly_usage %>%
  mutate(days_in_month = days_in_month(ymd(paste("2014", month, "01", sep = "-"))))

# Calculate total time in month (in seconds)
# Since there are 687 bikes in total, the total time used of the bikes are for the 687 bikes.
# Thus, the total available time in each month will have to be for 687 bikes (not just 1 bike)
# since 687 are available to be used
monthly_usage <- monthly_usage %>%
  mutate(total_time_in_month = days_in_month * 24 * 60 * 60 * total_bikes)

# Calculate average monthly bike utilization
# avg_utilization = total_duration / total_time_in_month
monthly_usage <- monthly_usage %>%
  mutate(avg_utilization = total_duration / (total_time_in_month)) %>%
  mutate(percent_utilization = avg_utilization * 100)

# Let's view the complete monthly ulitization data frame
monthly_usage

# Here's the percent utilization for a single bike on each month
monthly_usage$percent_utilization

#######################################################
### Weather condition impact on bike renal analysis ###
#######################################################

# join the trip_month dataset with weather_w dataset for each city and date

# join the trip_month dataset with the station_w dataset for the date and city 
# to get the city added as a new column corresponding to the city in the start(or end is better)
# station name in the trip_month dataset

trip_date_isolated <- trip_month %>%
  separate(start_date, c('date', 'time'), sep = " ")

trip_date_isolated$date <- ymd(trip_date_isolated$date)

str(trip_date_isolated$date)

trip_station <- trip_date_isolated %>%
  left_join(station_w, by = c("start_station_name" = "name"))

# Join combined trip and station dataset (basically trip dataset with the city from the station dataset)
# with the weather dataset by date and city

trip_stat_weather <- trip_station %>%
  left_join(weather_w, by = c("date" = "date", "city" = "city"))

# Let's now begin with plotting the correlation. As we'll use the cor() function
# to generate the correlation matrix, we'll need to install the corrplot package
# if its not yet installed and load the package.
#install.packages("corrplot")
library(corrplot)

# Since we're interested in biking rental patterns caused by different weather conditions,
# we can select specific columns from the joined dataframe to focus on them.

# One avenue we can focus on is to see whether the duration of one's bike ride is correlation
# or affected by the weather condition.

# To do so, we can select the duration column and the numerical weather condition columns
# pertaining to temperature, visibility, wind speed, gust speed, precipitation inches, and
# cloud cover.
correlation_columns <- trip_stat_weather %>%
  select(duration, min_temperature_f, mean_temperature_f, max_temperature_f, 
         max_visibility_miles, mean_visibility_miles, min_visibility_miles, 
         max_visibility_miles, max_wind_Speed_mph, mean_wind_speed_mph, max_gust_speed_mph, 
         precipitation_inches, cloud_cover)

# Then, using the cor() function, we'll create the correlation matrix
correlation_matrix <- cor(correlation_columns, use = "complete.obs")

correlation_matrix

# Modify the margins for plotting to ensure that the title does not obstruct the
# labels
par(mar = c(5.1, 4.1, 6.1, 2.1))

# Plot the correlation matrix
corrplot(correlation_matrix, method = "color", tl.cex = 0.7, tl.col = "black")
title("Correlation Matrix of Trip and Weather Data", line = 4)

# End of Code
