#================================================================================
# LOADED PACKAGES
#================================================================================
library(tidyverse)
library(lubridate)
library(readr)
library(dplyr)
library(ggplot2)
library(scales)

Sys.setlocale("LC_TIME", "C")

#================================================================================
# ADDITIONAL DATA SETS
#================================================================================

### Seasons of the year
season <- data.frame(season = c(rep("Winter", 3), rep("Summer", 3), 
                                rep("Spring", 3), rep("Fall", 3)),
                     month = c('12','01','02', '06', '07', '08', 
                               '03', '04', '05', '09', '10', '11'))

### Average temperatures in the Chicago area
### Font: 
chicago_temp <- data.frame(year_month = 
                             c('2020-06', '2020-07', '2020-08', 
                               '2020-09', '2020-10', '2020-11', 
                               '2020-12', '2021-01', '2021-02', 
                               '2021-03', '2021-04', '2021-05'), 
                           temperature = 
                             c(74.0, 79.2,	76.8,	66.3, 51.5, 47.4,	
                               32.8, 29.2,	20.2,	44.2, 51.9,	60.2))

#================================================================================
# COLLECT AND MERGE RAW DATA
# all the files have the same columns names
#================================================================================

### Set work directory
setwd("~/GitHub/google-data-analytics-certificate/data/raw-data")

### Determine files path
files_path <-"~/GitHub/google-data-analytics-certificate/data/raw-data"

### Merge the data with all columns formated as character
cyclistic_trip_data <- 
  list.files(files_path, pattern = "*.csv" ) %>%
  map_df(~read_csv(.x, col_types = cols(.default = "c"))) 

### Summary check of the data
glimpse(cyclistic_trip_data)

#================================================================================
# ORGANIZATION OF THE DATA
#================================================================================

### Change the format of each column according to the type of information
cyclistic_trip_data<- 
  mutate(cyclistic_trip_data,
         ride_id = as.character(ride_id),
         rideable_type = as.character(rideable_type),
         started_at = as_datetime(started_at),
         ended_at = as_datetime(ended_at),
         start_station_name = as.character(start_station_name),
         start_station_id = as.character(start_station_id),
         end_station_name = as.character(end_station_name),
         end_station_id = as.character(end_station_id),
         start_lat = as.double(start_lat),
         start_lng = as.double(start_lng),
         end_lat = as.double(end_lat),
         end_lng = as.double(end_lng),
         member_casual =as.character(member_casual))

### Rename columns to better identify the information
cyclistic_trip_data <-
  rename(cyclistic_trip_data,
         trip_id = ride_id,
         bike_type = rideable_type,
         started_time = started_at,
         ended_time = ended_at,
         user_type = member_casual)

### Exclude not necessary information (latitude and longitude)
### Sort the data by the start date and time of the trip
cyclistic_trip_data <-
  arrange(
    select(cyclistic_trip_data, -start_lat, -start_lng, -end_lat, -end_lng),
    started_time)

#================================================================================
# 1ST CHECK FOR ERRORS OR MISLEADING INFORMATION
#================================================================================

### Check months imported from data
count(cyclistic_trip_data, format(started_time, '%Y-%m'))

### Check main variables

user_types <- distinct(cyclistic_trip_data, user_type)

bike_types <- distinct(cyclistic_trip_data, bike_type)

### Check station names and ids

start_stations <- cyclistic_trip_data %>%
  group_by(start_station_name) %>%
  summarise(n_distinct(start_station_id)) %>%
  arrange(start_station_name)
  
end_stations <- cyclistic_trip_data %>%
  group_by(end_station_name) %>%
  summarise(n_distinct(end_station_id)) %>%
  arrange(end_station_name)

#================================================================================
# CREATE NEW VARIABLES FOR ANALYSIS
#================================================================================

cyclistic_trip_data <- 
  distinct(
    mutate(cyclistic_trip_data,
           trip_length = as.double(ended_time - started_time),
           year_month = format(started_time, '%Y-%m'),
           month = format(started_time, '%m'),
           week_day = weekdays(started_time),
           day_type = 
             ifelse(week_day == 'Saturday' | week_day == 'Sunday', 
                    'Weekend',
                    'Working day'),
           start_hour = format(started_time, '%H'),
           check_length = 
             ifelse(trip_length < 0, "neg.", "pos."),
           check_length_1m = 
             ifelse(trip_length < 60, "<1min", ">1min"),
           check_length_1d = 
             ifelse(trip_length >= 86400, ">1day", "<1day"),
           check_station_name = 
             ifelse(start_station_name == end_station_name, "yes", "no"),
           check_station_id = 
             ifelse(start_station_id == end_station_id, "yes", "no")
           ))

cyclistic_trip_data$season <- 
  season$season[match(cyclistic_trip_data$month, season$month)]

#================================================================================
# 2ST CHECK FOR ERRORS OR MISLEADING INFORMATION
#================================================================================

### Distribution of negative trip_length
ggplot(cyclistic_trip_data, aes(fill= check_length, x = year_month)) +
  geom_bar(position="stack") +
  theme(axis.text.x = element_text(angle = 45, hjust=1))

### Distribution of trips smaller than 1 minute
cyclistic_trip_data %>%
  filter(check_length == 'pos.') %>%
  ggplot(aes(fill= check_length_1m, x = year_month)) +
  geom_bar(position="stack") +
  theme(axis.text.x = element_text(angle = 45, hjust=1))

### Distribution of trips greater than 1 day
cyclistic_trip_data %>%
  filter(check_length == 'pos.') %>%
  ggplot(aes(fill= check_length_1d, x = year_month)) +
  geom_bar(position="stack") +
  theme(axis.text.x = element_text(angle = 45, hjust=1))

### Distribution of start_station_name = end_station_name
ggplot(cyclistic_trip_data, aes(fill= check_station_name, x = year_month)) +
  geom_bar(position="stack") +
  theme(axis.text.x = element_text(angle = 45, hjust=1))

### Distribution of start_station_id = end_station_id
ggplot(cyclistic_trip_data, aes(fill= check_station_id, x = year_month)) +
  geom_bar(position="stack") +
  theme(axis.text.x = element_text(angle = 45, hjust=1))

### Distribution of trips per user type
ggplot(cyclistic_trip_data, aes(fill= user_type, x = year_month)) +
  geom_bar(position="stack") +
  theme(axis.text.x = element_text(angle = 45, hjust=1))

ggplot(cyclistic_trip_data, aes(x = user_type, fill = user_type)) +
  geom_bar(position="stack")

### Distribution of trips per bike type
ggplot(cyclistic_trip_data, aes(fill= bike_type, x = year_month)) +
  geom_bar(position="stack") +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        legend.position="bottom") +
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3)) +
  labs(x = 'Month', y = 'Number of trips', fill = 'Bike type', 
       title = 'Distribution of the trips by bike type')

#================================================================================
# CLEAN THE DATA
#================================================================================

### Remove trips with negative duration
### Remove trips for quality assurance from the company: "HQ QR"
cyclistic_trip_data <-
  select(
    filter(cyclistic_trip_data, trip_length >= 0, start_station_name != "HQ QR"),
    -bike_type, -month, -check_length, -check_length_1m, 
    -check_length_1d, -check_station_name, -check_station_id)

### Save clean data
write_csv(cyclistic_trip_data, "~/GitHub/google-data-analytics-certificate/data/clean-data/cyclistic_trip_data.csv")

#================================================================================
# ANALYSIS: DISTRIBUTION OF THE TRIPS PER MONTH
#           COMPARISON WITH THE HISTORIC TEMPERATURES IN THE CHICAGO AREA
#================================================================================

### Trips per season
ggplot(data = cyclistic_trip_data) +
  geom_bar(mapping = aes(x = season, fill = user_type), position = 'stack') +
  theme(legend.position="bottom") +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
  labs(x = 'Season', y = 'Number of trips', fill = 'User type', 
       title = 'Distribution of the trips by season')

### Trips per month compared with average temperature
ggplot() + 
  geom_bar(mapping = aes(x = cyclistic_trip_data$year_month, 
                         fill = cyclistic_trip_data$user_type),
           position = 'stack') +
  geom_point(mapping = aes(x = chicago_temp$year_month, 
                           y = chicago_temp$temperature * 8000)) +
  scale_y_continuous(name = "Number of Trips", 
                     labels = unit_format(unit = "K", scale = 1e-3),
                     sec.axis = sec_axis(~./8000, name = "Temperature (°F)")) +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        legend.position="bottom") +
  labs(x = 'Months', fill = 'User type', 
       title = 'Distribution of the trips vs Average temperature in Chicago')

#================================================================================
# ANALYSIS: DISTRIBUTION OF THE DATA PER DAY OF THE WEEK AND HOUR
#================================================================================

### Trips per day of the week
ggplot(data = cyclistic_trip_data) +
  geom_bar(mapping = aes(x = wday(started_time, label = TRUE), fill = user_type), position = 'stack') +
  theme(legend.position="bottom") +
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3)) +
  labs(x = 'Weekday', y = 'Number of Trips', fill = 'User type', 
       title = 'Distribution of the trips by weekday')

### Trips per day of the week by season
ggplot(data = cyclistic_trip_data) +
  geom_bar(mapping = aes(x = wday(started_time, label = TRUE), fill = user_type), position = 'stack') +
  theme(legend.position="bottom") +
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3)) +
  labs(x = 'Weekday', y = 'Number of Trips', fill = 'User type', 
       title = 'Distribution of the trips by weekday per season') +
  facet_wrap(~season)

### Trips per hour of the day
ggplot(data = cyclistic_trip_data) +
  geom_bar(mapping = aes(x = start_hour, fill = user_type), position = 'stack') +
  theme(legend.position="bottom") +
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3)) +
  labs(x = 'Start hour', y = 'Number of Trips', fill = 'User type', 
       title = 'Distribution of the trips by start hour')

### Trips per hour of the day per season
ggplot(data = cyclistic_trip_data) +
  geom_bar(mapping = aes(x = start_hour, fill = user_type), position = 'stack') +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        legend.position="bottom") +
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3)) +
  labs(x = 'Start hour', y = 'Number of Trips', fill = 'User type', 
       title = 'Distribution of the trips by start hour per season') +
  facet_wrap(~season)

### Trips per hour of the day per weekday
ggplot(data = cyclistic_trip_data) +
  geom_bar(mapping = aes(x = start_hour, fill = user_type), position = 'stack') +
  theme(legend.position="bottom") +
  scale_y_continuous(name = "Number of Trips", 
                     labels = unit_format(unit = "K", scale = 1e-3)) +
  labs(x = 'Start hour', y = 'Number of Trips', fill = 'User type', 
       title = 'Distribution of the trips by start hour per weekday') +
  facet_wrap(~wday(started_time, label = TRUE))

### Trips per hour of the day per day type
ggplot(data = cyclistic_trip_data) +
  geom_bar(mapping = aes(x = start_hour, fill = user_type), position = 'stack') +
  theme(legend.position="bottom") +
  scale_y_continuous(name = "Number of Trips", 
                     labels = unit_format(unit = "K", scale = 1e-3)) +
  labs(x = 'Start hour', y = 'Number of Trips', fill = 'User type', 
       title = 'Distribution of the trips by start hour per day type') +
  facet_wrap(~day_type, ncol = 1)

#================================================================================
# ANALYSIS: DISTRIBUTION OF THE DATA PER TRIP LENGTH
#================================================================================

### Distribution of the trips with 3h or less
cyclistic_trip_data %>%
  filter(trip_length <= 10800) %>%
  ggplot(aes(x = trip_length)) +
  geom_histogram(binwidth = 900) +
  labs(x = 'Trip length (seconds)', y = 'Number of Trips', 
       title = 'Histogram by duration') +
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3))

### Cutting the data into trip_length groups
cyclistic_trip_data$trip_length_group <-
  cut(cyclistic_trip_data$trip_length, 
      c(0, 900, 1800, 2700, 3600, 10800, max(cyclistic_trip_data$trip_length)))

levels(cyclistic_trip_data$trip_length_group) = 
  c('0m - 15m', '15m - 30m','30m - 45m','45m - 1h', '1h - 3h', '>3h')

### Save clean data
write_csv(cyclistic_trip_data, "~/GitHub/google-data-analytics-certificate/data/clean-data/cyclistic_trip_data.csv")

### Plot of the trip_length groups
ggplot(data = cyclistic_trip_data) +
  geom_bar(mapping = aes(x = trip_length_group, fill = user_type), 
           position = 'dodge') +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        legend.position="bottom") +
  labs(x = 'Trip length', y = 'Number of Trips', fill = 'User type',
       title = 'Distribution of the trips by duration')

### Plot of the trip_length groups per season
ggplot(data = cyclistic_trip_data) +
  geom_bar(mapping = aes(x = trip_length_group, fill = user_type), 
           position = 'stack') +
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3)) +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        legend.position="bottom") +
  labs(x = 'Trip length', y = 'Number of Trips', fill = 'User type',
       title = 'Distribution of the trips by duration per season') +
  facet_wrap(~season, ncol = 2)

### Plot of the trip_length groups per day type - casual riders
cyclistic_trip_data %>%
  filter(user_type == 'casual') %>% 
  ggplot() +
  geom_bar(mapping = aes(x = trip_length_group, fill = day_type), 
           position = 'stack') +
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3)) +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        legend.position="bottom") +
  xlab("Trip length") + 
  ylab("Number of trips") +
  labs(fill = 'Day Type') +
  facet_wrap(~season)

### Plot of the trip_length groups per day type - members
cyclistic_trip_data %>%
  filter(user_type == 'member') %>% 
  ggplot() +
  geom_bar(mapping = aes(x = trip_length_group, fill = day_type), 
           position = 'stack') +
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3)) +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        legend.position="bottom") +
  xlab("Trip length") + 
  ylab("Number of trips") +
  labs(fill = 'Day Type') +
  facet_wrap(~season)

#================================================================================
# ANALYSIS: AVERAGE LENGTH OF THE TRIP
#================================================================================

### Parameters of the trip_length per user type - trips with less than 3h
cyclistic_trip_data %>%
  filter(trip_length <= 10800) %>%
  ggplot() +
  geom_boxplot(mapping = aes(x = user_type, y = trip_length/60, fill = user_type)) +
  labs(x = 'User type', y = 'Trip length (minutes)',
       title = 'General trip length per user type') +
  theme(legend.title = element_blank(), legend.position = 'none')

### Parameters of the trip_length per user type - trips with less than 3h
### Per season
cyclistic_trip_data %>%
  filter(trip_length <= 10800) %>%
  ggplot() +
  geom_boxplot(mapping = aes(x = season, y = trip_length/60, fill = season)) +
  labs(x = 'Season', y = 'Trip length (minutes)',
       title = 'Trip length per season and user type') +
  theme(legend.title = element_blank(), legend.position = 'none') +
  facet_wrap(~user_type)

### Parameters of the trip_length per user type - trips with less than 3h
### Per day type
cyclistic_trip_data %>%
  filter(trip_length <= 10800) %>%
  ggplot() +
  geom_boxplot(mapping = aes(x = day_type, 
                             y = trip_length/60,
                             fill = day_type)) +
  labs(x = 'Day type', y = 'Trip length (minutes)',
       title = 'Trip length per day type and user type') +
  theme(legend.title = element_blank(), legend.position = 'none') +
  facet_wrap(~user_type)

### Parameters of the trip_length per user type - trips with less than 3h
### Per weekday
cyclistic_trip_data %>%
  filter(trip_length <= 10800) %>%
  ggplot() +
  geom_boxplot(mapping = aes(x = wday(started_time, label = TRUE), 
                             y = trip_length/60)) +
  ylab('Trip Length (minutes)') +
  xlab("Weekday") +
  facet_wrap(~user_type) +
  coord_flip()

### Parameters of the trip_length per user type - trips with less than 3h
### Per start hour
cyclistic_trip_data %>%
  filter(trip_length <= 10800) %>%
  ggplot() +
  geom_boxplot(mapping = aes(x = reorder(start_hour,desc(start_hour)), 
                             y = trip_length/60,
                             fill = start_hour)) +
  labs(x = 'Start hour', y = 'Trip length (minutes)',
       title = 'Trip length per start hour and user type') +
  theme(legend.title = element_blank(), legend.position = 'none') +
  facet_wrap(~user_type) +
  coord_flip()

