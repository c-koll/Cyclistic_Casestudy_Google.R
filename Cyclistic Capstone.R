# Install and load tidyverse
install.packages("tidyverse")
library(tidyverse)
library(dplyr)
library(readr)
library(lubridate)

# Use the conflicted pkg to manage conflicts
library(conflicted)

# Set dplyr::filter and dplyr::lag as the default choices
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

# Import data .csv files
Jan_2021 <- read.csv("202101-divvy-tripdata.csv")
Feb_2021 <- read.csv("202102-divvy-tripdata.csv")
Mar_2021 <- read.csv("202103-divvy-tripdata.csv")
Apr_2021 <- read.csv("202104-divvy-tripdata.csv")
May_2021 <- read.csv("202105-divvy-tripdata.csv")
Jun_2021 <- read.csv("202106-divvy-tripdata.csv")
Jul_2021 <- read.csv("202107-divvy-tripdata.csv")
Aug_2021 <- read.csv("202108-divvy-tripdata.csv")
Sep_2021 <- read.csv("202109-divvy-tripdata.csv")
Oct_2021 <- read.csv("202110-divvy-tripdata.csv")
Nov_2021 <- read.csv("202111-divvy-tripdata.csv")
Dec_2021 <- read.csv("202112-divvy-tripdata.csv")

# Compare column names in the files
colnames(Jan_2021)
colnames(Feb_2021)
colnames(Mar_2021)
colnames(Apr_2021)
colnames(May_2021)

# Inspect data frames
str(Jan_2021)
str(May_2021)
str(Aug_2021)
str(Nov_2021)

# Stack data frames into one frame
all_2021 <- bind_rows(Jan_2021, Feb_2021, Mar_2021, Apr_2021, May_2021, Jun_2021, Jul_2021, Aug_2021, Sep_2021, Oct_2021, 
                      Nov_2021, Dec_2021)

# Inspect new table
str(all_2021)
nrow(all_2021)
dim(all_2021)

# Convert started_at and ended_at columns from chr to datetime for numerical data analysis
all_2021$started_at <- ymd_hms(all_2021$started_at)
all_2021$ended_at <- ymd_hms(all_2021$ended_at)

# Confirm type change
str(all_2021)

# Add columns to list date, year, month, day, and day of week of each ride
all_2021$date <- as.Date(all_2021$started_at) #Format yyyy-mm-dd
all_2021$year <- format(as.Date(all_2021$date), "%Y")
all_2021$month <- format(as.Date(all_2021$date), "%m")
all_2021$day <- format(as.Date(all_2021$date), "%d")
all_2021$day_of_week <- format(as.Date(all_2021$date), "%A")

# Add "ride_length" calc to all trips in minutes and convert to numeric, confirm
all_2021$ride_length <- difftime(all_2021$ended_at,all_2021$started_at, units = "mins")
all_2021$ride_length <- as.numeric(all_2021$ride_length)
is.numeric(all_2021$ride_length)

# Check if any ride_length values are 0
sum(all_2021$ride_length == 0)

# Filter out rows where ride_length = 0 and rows where a column contains a blank
all_2021_v2 <- all_2021 %>% 
  filter(ride_length > 0) %>% 
  filter(!if_any(c(
    ride_id,
    rideable_type,
    started_at,
    ended_at,
    start_station_name,
    start_station_id,
    end_station_name,
    end_station_id,
    start_lat,
    start_lng,
    end_lat,
    end_lng,
    member_casual
  ), is.na)) %>%
  filter(!if_any(c(
    ride_id,
    rideable_type,
    start_station_name,
    start_station_id,
    end_station_name,
    end_station_id,
    member_casual
  ), ~ . == ""))

# Look for duplicates
all_2021_v2[duplicated(all_2021_v2), ]
# 0 rows are duplicates :)

# Set days of the week
all_2021_v2$day_of_week <- ordered(all_2021_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday",
                                                                     "Thursday", "Friday","Saturday"))

# Create data set to analyze ridership data by type and weekday
rides_by_DoW <- all_2021_v2 %>% 
  group_by(member_casual, day_of_week) %>%
  summarise(number_of_rides = n(),average_duration = mean(ride_length)) %>%
  arrange(member_casual, day_of_week)

#Create data set to analyze ridership by month
rides_by_month <- all_2021_v2 %>% 
  mutate(month = month(started_at, label = TRUE)) %>% 
  group_by(member_casual, month) %>% 
  summarise(number_of_rides = n(),average_duration = mean(ride_length)) %>%
  arrange(member_casual, month)

# Create data summary of total rides
total_rides_summary <- all_2021_v2 %>% 
  group_by(member_casual) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual)

# Create data set to analyze rides by time of day
rides_by_hour <- all_2021_v2 %>% 
  mutate(hour_of_day = hour(started_at), hour_of_day_ampm = format(started_at, "%I %p")) %>% 
  group_by(member_casual, hour_of_day, hour_of_day_ampm) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(member_casual, hour_of_day) %>% 
  select(member_casual, number_of_rides, hour_of_day_ampm)

# Create data sets to analyze top 10 stations
top_stations_total_rides <- all_2021_v2 %>%
  select(member_casual, station_name = start_station_name, lat = start_lat, lng = start_lng) %>%
  bind_rows(
    all_2021_v2 %>%
      select(member_casual, station_name = end_station_name, lat = end_lat, lng = end_lng)) %>%
  group_by(station_name, lat, lng, member_casual) %>%
  summarise(total_trips = n(), .groups = 'drop') %>%
  arrange(member_casual, desc(total_trips)) %>%
  group_by(member_casual) %>%
  slice_head(n = 10) %>%
  mutate(rank = row_number())

# Create data set to analyze rideable_type
rideable_type_summary <- all_2021_v2 %>% 
  group_by(rideable_type, member_casual) %>% 
  summarise(number_of_rides = n())

# Write .csv to extract data frames for visualization
write.csv(rides_by_DoW, "rides_by_DoW.csv", row.names = FALSE)
write.csv(rides_by_month, "rides_by_month.csv", row.names = FALSE)
write.csv(rideable_type_summary, "cyclistic_type_summary.csv", row.names = FALSE)
write.csv(rides_by_hour, "rides_by_hour.csv", row.names = FALSE)
write.csv(top_rides_summary, "total_rides_summary.csv", row.names = FALSE)
write.csv(top_stations_total_rides, "top_stations_total_rides.csv", row.names = FALSE)