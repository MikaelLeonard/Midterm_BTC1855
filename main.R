# Mikael Gouwtama
# 1007128127
# BTC 1855H Midterm Project 

# Read the 3 datasets

station <- read.csv("./datasets/station.csv")
trip <- read.csv("./datasets/trip.csv")
weather <- read.csv("./datasets/weather.csv")

# Inspect and clean the station dataset 

## Create a station working dataset to keep a copy of the original
station_w <- station

## Get the summary and structures of the station dataset's variables
summary(station_w)
str(station_w)

## The installation_date column is of character structure when it should be in date
library(lubridate)
station_w$installation_date <- mdy(station_w$installation_date)

## There is no missing data visible when looking at the dataset, but just making sure,
## we can confirm there is 0 missing value in this dataset
sum(is.na(station_w))
