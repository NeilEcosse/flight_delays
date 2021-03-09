library(tidyverse)
library(janitor)
library(here)
library(readxl)
library(lubridate)



# Load raw data
flights <- read_csv(here("data_raw/flights.csv")) %>% 
  clean_names()

weather <- read_csv(here("data_raw/weather.csv")) %>% 
  clean_names()

planes <- read_csv(here("data_raw/planes.csv")) %>% 
  clean_names()

airports <- read_csv(here("data_raw/airports.csv")) %>% 
  clean_names()

airlines <- read_csv(here("data_raw/airlines.csv")) %>% 
  clean_names()

data_dictionary <- read_excel(here("documentation/data_dictionary.xlsx"))

beaufort <- read_excel(here("data_raw/beaufort_scale_mph.xlsx"))


########################################################################

# CHANGES TO AIRPORTS TABLE

# Manually add missing airports 
# Using information from Wikipedia
missing_airports <- tibble(
faa = c("BQN", "PSE", "SJU", "STT"),
name = c("Rafael Hernandez Airport", "Mercedita International Airport", "Luis Munoz Marin International Airport", "Cyril E. King Airport"),
lat = c(18.495, 18.008333, 18.439167, 18.337222),
lon = c(-67.129444, -66.563056, -66.001944, -64.973333)
)

# Add these to main airports table
airports <- 
bind_rows(airports, missing_airports)

# Drop missing_airports tibble
rm(missing_airports)



########################################################################
########################################################################

# IMPORT AND JOIN THE THREE DAILY SUMMARY NOAA WEATHER TABLES

# Downloaded from https://www.ncdc.noaa.gov/cdo-web/search

# Data dictionary saved in documentation folder

# These are used for precipitation and temperature, which are mostly 
# 'na' in the weather file which came with the brief

#EWR
noaa_weather_ewr <- read_csv(here("data_raw/noaa_weather_2017_ewr.csv")) %>% clean_names()
# add column with airport code, select only columns to be used
noaa_weather_ewr <- noaa_weather_ewr %>% 
  mutate(origin = "EWR") %>% 
  select(origin,
         date,
         station,
         name,
         awnd,
         prcp,
         snow,
         snwd,
         tavg,
         tmax,
         tmin
  )

#LGA
noaa_weather_lga <- read_csv(here("data_raw/noaa_weather_2017_lga.csv")) %>% clean_names()
# add column with airport code, select only columns to be used
noaa_weather_lga <- noaa_weather_lga %>% 
  mutate(origin = "LGA")%>% 
  select(origin,
         date,
         station,
         name,
         awnd,
         prcp,
         snow,
         snwd,
         tavg,
         tmax,
         tmin
  )

#JFK
noaa_weather_jfk <- read_csv(here("data_raw/noaa_weather_2017_jfk.csv")) %>% clean_names()
# add column with airport code, select only columns to be used
noaa_weather_jfk <- noaa_weather_jfk %>% 
  mutate(origin = "JFK")%>% 
  select(origin,
         date,
         station,
         name,
         awnd,
         prcp,
         snow,
         snwd,
         tavg,
         tmax,
         tmin
  )

# Combine these into a single table, drop the individual ones
noaa_weather <- 
  bind_rows(noaa_weather_ewr, noaa_weather_lga, noaa_weather_jfk)

rm(noaa_weather_ewr, noaa_weather_lga, noaa_weather_jfk)


########################################################################
########################################################################

# MAKE CHANGES TO THE FLIGHTS TABLE

# add column to say if departure was delayed by 15 mins or more

# departure 15+ minutes late will be used as the definition of a delayed flight
#https://www.bts.dot.gov/explore-topics-and-geography/topics/airline-time-performance-and-causes-flight-delays#q8
flights <- flights %>%   
  mutate(flight_delayed =  case_when(
    is.na(dep_delay) ~ NA_character_,
    dep_delay >= 15 ~ "Departure delayed",
    dep_delay < 15 ~ "Departed on time",
    TRUE ~ "Error - please check"
  )
  ) 

# add column for delay time group
flights <- flights %>% 
  mutate(delay_group = case_when(
    is.na(dep_delay) ~ NA_character_,
    dep_delay >= 120 ~ "2h +",
    dep_delay >= 90 ~ "1.5h to <2h",
    dep_delay >= 60 ~ "1h to <1.5h",
    dep_delay >= 30 ~ "0.5h to <1h",
    dep_delay >= 15 ~ "0.25h to <0.5h",
    dep_delay < 15 ~ "<0.25h",
    TRUE ~ "Error - please check"
  )
  ) 


# add date in ymd format 
flights <- flights %>% 
  mutate(date_ymd = as_date(time_hour)) 

# get number of departures in hour
departures_per_hour <- flights %>% 
  group_by(origin, time_hour) %>% 
  summarise(number_departures_in_sched_dep_hour = n())
# add number_departures_in_sched_dep_hour to flights table
flights <- flights %>% 
  left_join(departures_per_hour, by = c("origin" = "origin",
                                        "time_hour" = "time_hour"))
# drop departures_per_hour table
rm(departures_per_hour)

 
  # add sched dep weekday column  (1 = Monday)
flights <- flights %>% 
  mutate(sched_dep_weekday = as.factor(wday(time_hour, week_start = 1))) 
  
  # add sched dep month column
flights <- flights %>% 
  mutate(sched_dep_month = as.factor(month(time_hour))) 
  
  # add sched dep hour column
flights <- flights %>% 
  mutate(sched_dep_hour = as.factor(hour(time_hour))) 
  
  # add sched dep hour_group column
flights <- flights %>% 
  mutate(sched_dep_hour_group = case_when(
    sched_dep_hour >= 18 ~ "18:00 to 23:59",
    sched_dep_hour >= 12 ~ "12:00 to 17:59",
    sched_dep_hour >= 6 ~ "06:00 to 11:59",
    sched_dep_hour >= 0 ~ "00:00 to 05:59"
   )
  )  

# convert fields to factors ?is  this necessary
flights <- flights %>% 
mutate(sched_dep_hour_group = as.factor(sched_dep_hour_group))

flights <- flights %>% 
  mutate(dest = as.factor(dest))

flights <- flights %>% 
  mutate(carrier = as.factor(carrier))

flights <- flights %>% 
  mutate(dest = as.factor(dest))
  
#################################################################### 
################### DROP RECORDS FROM FLIGHTS ######################
####################################################################
  
# drop records where actual departure time is null
flights <- flights %>% 
  filter(!is.na(dep_time)) 
  
  # drop records where  departure delay is null
flights <- flights %>% 
  filter(!is.na(dep_delay))


##################################################################
##################################################################


# MAKE CHANGES TO THE WEATHER TABLE

# This is the file which came with the brief


# Impute missing wind direction
# Replace na with median wind direction for the day

# Add date_ymd so wind direction can be grouped by day
weather <- weather %>% 
  mutate(date_ymd = as_date(time_hour))

# create table of daily median wind_dir
median_wind_dir <- weather %>% 
  filter(!is.na(wind_dir)) %>% 
  group_by(origin, date_ymd) %>% 
  summarise(median_wind_dir = median(wind_dir)) %>% 
  arrange(date_ymd)

# update weather table - replace na with median for day
weather <- weather %>% 
  left_join(median_wind_dir, by = c("date_ymd" = "date_ymd",
                                    "origin" = "origin")) %>% 
  mutate(wind_dir = ifelse(is.na(wind_dir),
                                 median_wind_dir,
                                 wind_dir)) %>% 
  select(-median_wind_dir)

# drop median wind dir table
rm(median_wind_dir)



# Impute missing wind speed

# 2647	records (around 10%) are missing wind speed, and I'd like to use this in my analysis
# I'm  updating these hourly readings with the daily average wind speed from the noaa_weather table

weather <- weather %>% 
 mutate(date_ymd = as_date(time_hour))


weather <- weather %>%
  left_join(noaa_weather, by = c("origin" = "origin",
                                 "date_ymd" = "date")) %>% 
  mutate(wind_speed = ifelse(is.na(wind_speed),
                                       awnd,
                                       wind_speed)
  ) %>% 
  select(-c("station", "name",  "awnd", "prcp", "snow", "snwd", "tavg", "tmax", "tmin"))

# create visibility groups
weather <- weather %>% 
  mutate(visib_group = case_when(
                        is.na(visib) ~ NA_character_,
                        visib >=8 ~ "8 to 10 miles",
                        visib >=6 ~ "6 to <8 miles",
                        visib >=4 ~ "4 to <6 miles",
                        visib >=2 ~ "2 to <4 miles",
                        visib >=0 ~ "0 to <2 miles",
                        TRUE ~ "Error - please check"
          )
  )


# rename wind_speed as wind_speed_mph
weather <- weather %>% 
  rename("wind_speed_mph" = "wind_speed")

# create wind_speed_kmh
weather <- weather %>% 
  mutate(wind_speed_kmh = 1.609344 * wind_speed_mph)

# rename wind_gust as wind_gust_mph
weather <- weather %>% 
  rename("wind_gust_mph" = "wind_gust")

# create wind_speed_kmh
weather <- weather %>% 
  mutate(wind_gust_kmh = 1.609344 * wind_gust_mph)


# add cardinal wind directions
weather <- weather %>% 
  mutate(wind_dir_cardinal = case_when(
    is.na(wind_dir) ~NA_character_,
    wind_speed_mph == 0 ~ "No wind",
    wind_dir >= 315 ~ "N",
    wind_dir >= 225 ~ "W",
    wind_dir >=  135 ~ "S",
    wind_dir >=  45 ~ "E",
    wind_dir > 0 ~ "N",
    #set as NA where wind_dir degree is 0 - these all have zero wind speed in data
    wind_dir == 0 ~ NA_character_,
    TRUE ~ "Error"
  )
  )

# following section uses info on the Beaufort scale sourced here:
# https://www.weather.gov/jetstream/beaufort_max

# add Beaufort numeric using wind SPEED
weather <- weather %>% 
  mutate(wind_speed_beaufort_numeric = case_when(
    is.na(wind_speed_mph) ~ NA_real_,
    wind_speed_mph >= 72 ~ 12,
    wind_speed_mph >= 64 ~ 11,
    wind_speed_mph >= 55 ~ 10,
    wind_speed_mph >= 47 ~ 9,
    wind_speed_mph >= 39 ~ 8,
    wind_speed_mph >= 32 ~ 7,
    wind_speed_mph >= 25 ~ 6,
    wind_speed_mph >= 19 ~ 5,
    wind_speed_mph >= 13 ~ 4,
    wind_speed_mph >= 8 ~ 3,
    wind_speed_mph >= 4 ~ 2,
    wind_speed_mph >= 1 ~ 1,
    TRUE ~ 0
    
  )
  ) 


# add beaufort numeric using wind GUST
weather <- weather %>% 
  mutate(wind_gust_beaufort_numeric = case_when(
    is.na(wind_gust_mph) ~ NA_real_,
    wind_gust_mph >= 72 ~ 12,
    wind_gust_mph >= 64 ~ 11,
    wind_gust_mph >= 55 ~ 10,
    wind_gust_mph >= 47 ~ 9,
    wind_gust_mph >= 39 ~ 8,
    wind_gust_mph >= 32 ~ 7,
    wind_gust_mph >= 25 ~ 6,
    wind_gust_mph >= 19 ~ 5,
    wind_gust_mph >= 13 ~ 4,
    wind_gust_mph >= 8 ~ 3,
    wind_gust_mph >= 4 ~ 2,
    wind_gust_mph >= 1 ~ 1,
    TRUE ~ 0
    
  )
  ) 
# look up beaufort name - speed 

# this uses the talble beaufort which I created with information from:
# https://www.weather.gov/jetstream/beaufort_max


weather  <- weather %>% 
  left_join(beaufort, by = c("wind_speed_beaufort_numeric"  = "beaufort_numeric")) %>% 
  select(-c(min_mph, max_mph, source)) %>% 
  rename("wind_speed_beaufort_name" = "beaufort_name")

# add beaufort number before beaufort name so data can be sorted more easily
weather  <- weather %>% 
  mutate(wind_speed_beaufort_name = str_c(wind_speed_beaufort_numeric,
                                          wind_speed_beaufort_name,
                                          sep = " - "))

# look up beaufort name - gust 
weather  <- weather %>% 
  left_join(beaufort, by = c("wind_gust_beaufort_numeric"  = "beaufort_numeric")) %>% 
  select(-c(min_mph, max_mph, source)) %>% 
  rename("wind_gust_beaufort_name" = "beaufort_name")


# add beaufort number before beaufort name so data can be sorted more easily
weather  <- weather %>% 
  mutate(wind_gust_beaufort_name = str_c(wind_gust_beaufort_numeric,
                                          wind_gust_beaufort_name,
                                          sep = " - "))

# convert fields to factors ?is  this necessary
weather <- weather %>% 
  mutate(wind_dir_cardinal = as.factor(wind_dir_cardinal))



# drop precipitation and temperature columns - I'm using noaa_weather table for this
weather <- weather %>% 
  select(-c(precip, temp))



# The section below created derived fields from the precip and temp columns
#  in the weather table

# THIS HAS BEEN HASHED OUT because I'm using noaa_weather for these fields instead

# I'm leaving the code in in case it's needed in future

# rename precip to precip_inch
# weather <- weather %>% 
#  rename("precip_inch" = "precip")

# create precip_cm column
#weather <- weather %>% 
#  mutate(precip_cm = 2.54 * precip_inch)

# add rainfall groups
#weather <- weather %>% 
#  mutate(precip_group = case_when(
#    is.na(precip_inch) ~ NA_character_,
#    precip_inch >= 2 ~ "2 inch +",
#    precip_inch >= 1.5 ~ "1.5 to <2 inch",
#    precip_inch >= 1 ~ "1 to <1.5 inch",
#    precip_inch >= 0.5 ~ "0.5 to <1 inch",
#    precip_inch > 0 ~ ">0 to <0.5 inch",
#    precip_inch == 0 ~ "0  inch",
#    TRUE ~ "Error - please check"
#  )
#  ) 




# rename temp to temp_f
#weather <- weather %>% 
#  rename("temp_f" = "temp")

# add temp Fahrenheit groups
# these groups match the Celsius groups below
#weather <- weather %>% 
#  mutate(temp_f_group = case_when(
#    is.na(temp_f) ~ NA_character_,
#    temp_f >= 104 ~ "104F+",
#    temp_f >= 86 ~ "86F to <104F",
#    temp_f >= 68 ~ "68F to <86F",
#    temp_f >= 50 ~ "50F to <68F",
#    temp_f >= 32 ~ "32F to <50F",
#    temp_f >= 14 ~ "14F to <32F",
#    temp_f >= -4 ~ "-4F to < 14F",
#    TRUE ~ "< -4F"
#  )
#  )

# add temp_c column
#weather <- weather %>% 
#  mutate(temp_c = (as.numeric(temp_f) - 32)*(5/9))

# add temp celsius groups
#weather <- weather %>% 
#  mutate(temp_c_group = case_when(
#    is.na(temp_c) ~ NA_character_,
#    temp_c >= 40 ~ "40c+",
#    temp_c >= 30 ~ "30c to <40c",
#    temp_c >= 20 ~ "20c to <30c",
#    temp_c >= 10 ~ "10c to <20c",
#    temp_c >= 0 ~ "0c to <10c",
#    temp_c >= -10 ~ "-10c to <0c",
#    temp_c >= -20 ~ "-20c to < -10c",
#    TRUE ~ "< -20c"
#  )
#  )





########################################################################
########################################################################

# CHANGES TO NOAA_WEATHER TABLE

# sourced from https://www.ncdc.noaa.gov/cdo-web/search
# Data dictionary saved in documentation folder


# rename precipitation column
noaa_weather <- noaa_weather %>% 
  rename("precip_day_total_inch" = "prcp")

# add metric precip column
noaa_weather <- noaa_weather %>% 
  mutate(precip_day_total_cm = 2.54 * precip_day_total_inch)

# add precipitation groups
noaa_weather <- noaa_weather %>% 
  mutate(precip_day_total_inch_group = case_when(
    is.na(precip_day_total_inch) ~ NA_character_,
    precip_day_total_inch >= 2 ~ "2 inch +",
    precip_day_total_inch >= 1.5 ~ "1.5 to <2 inch",
    precip_day_total_inch >= 1 ~ "1 to <1.5 inch",
    precip_day_total_inch >= 0.5 ~ "0.5 to <1 inch",
    precip_day_total_inch > 0 ~ ">0 to <0.5 inch",
    precip_day_total_inch == 0 ~ "0  inch",
    TRUE ~ "Error - please check"
    
  )
  )

# reorder based on these new groupings
noaa_weather <- noaa_weather %>% 

  mutate(precip_day_total_inch_group = factor
         (precip_day_total_inch_group, levels =
                                          c( "0  inch",
                                             ">0 to <0.5 inch",
                                             "0.5 to <1 inch", 
                                             "1 to <1.5 inch",
                                             "1.5 to <2 inch",
                                             "2 inch +")
         )
  )




# rename snowfall column
noaa_weather <- noaa_weather %>% 
  rename("snowfall_day_total_inch" = "snow")

# add metric snowfall column
noaa_weather <- noaa_weather %>% 
  mutate(snowfall_day_total_cm = 2.54 * snowfall_day_total_inch)

# add snowfall groups
noaa_weather <- noaa_weather %>% 
  mutate(snowfall_day_total_inch_group = case_when(
    is.na(snowfall_day_total_inch) ~ NA_character_,
    snowfall_day_total_inch >= 5 ~ "5 inch +",
    snowfall_day_total_inch >= 4 ~ "4 to <5 inch",
    snowfall_day_total_inch >= 3 ~ "3 to <4 inch",
    snowfall_day_total_inch >= 2 ~ "2 to <3 inch",
    snowfall_day_total_inch >= 1 ~ "1 to <2 inch",
    snowfall_day_total_inch > 0 ~ ">0 to <1 inch",
    snowfall_day_total_inch == 0 ~ "0  inch",
    TRUE ~ "Error - please check"
    )
  )

# reorder based on these new groupings
noaa_weather <- noaa_weather %>% 
  mutate(snowfall_day_total_inch_group = factor
         (snowfall_day_total_inch_group, levels =
                                                    c("0  inch",
                                                    ">0 to <1 inch",
                                                    "1 to <2 inch", 
                                                    "2 to <3 inch",
                                                    "3 to <4 inch",
                                                    "4 to <5 inch",
                                                    "5 inch +"))) 

# rename snow depth column
noaa_weather <- noaa_weather %>% 
  rename("snow_depth_day_total_inch" = "snwd")

# add metric snow depth column
noaa_weather <- noaa_weather %>% 
  mutate(snow_depth_day_total_cm = 2.54 * snow_depth_day_total_inch)

# add snow depth groups
  noaa_weather <- noaa_weather %>% 
  mutate(snow_depth_day_total_inch_group = case_when(
    is.na(snow_depth_day_total_inch) ~ NA_character_,
    snow_depth_day_total_inch >= 5 ~ "5 inch +",
    snow_depth_day_total_inch >= 4 ~ "4 to <5 inch",
    snow_depth_day_total_inch >= 3 ~ "3 to <4 inch",
    snow_depth_day_total_inch >= 2 ~ "2 to <3 inch",
    snow_depth_day_total_inch >= 1 ~ "1 to <2 inch",
    snow_depth_day_total_inch > 0 ~ ">0 to <1 inch",
    snow_depth_day_total_inch == 0 ~ "0  inch",
    TRUE ~ "Error - please check"
    )
  )

  # reorder based on these new groupings
  noaa_weather <- noaa_weather %>% 
    mutate(snow_depth_day_total_inch = factor
           (snow_depth_day_total_inch, levels =
               c("0  inch",
                 ">0 to <1 inch",
                 "1 to <2 inch", 
                 "2 to <3 inch",
                 "3 to <4 inch",
                 "4 to <5 inch",
                 "5 inch +"))) 

# rename avg temp column
noaa_weather <- noaa_weather %>% 
  rename("temp_avg_day_f" = "tavg")

# add avg temp Fahrenheit groups
# these groups match the Celsius groups below
noaa_weather <- noaa_weather %>% 
  mutate(temp_avg_day_f_group = case_when(
    is.na(temp_avg_day_f) ~ NA_character_,
    temp_avg_day_f >= 104 ~ "104F+",
    temp_avg_day_f >= 86 ~ "86F to <104F",
    temp_avg_day_f >= 68 ~ "68F to <86F",
    temp_avg_day_f >= 50 ~ "50F to <68F",
    temp_avg_day_f >= 32 ~ "32F to <50F",
    temp_avg_day_f >= 14 ~ "14F to <32F",
    temp_avg_day_f >= -4 ~ "-4F to < 14F",
    TRUE ~ "< -4F"
  )
  )

# reorder based on these new groupings
noaa_weather <- noaa_weather %>% 
  mutate(temp_avg_day_f_group = factor
         (temp_avg_day_f_group, levels =
                       c("< -4F",
                       "-4F to < 14F",
                       "14F to <32F", 
                       "32F to <50F",
                       "50F to <68F",
                       "68F to <86F",
                       "86F to <104F",
                       "104F+"
                       )
           )
  )

# add avg temp Celsius column
noaa_weather <- noaa_weather %>% 
  mutate(temp_avg_day_c = (as.numeric(temp_avg_day_f) - 32)*(5/9))

# add avg temp celsius groups
noaa_weather <- noaa_weather %>% 
  mutate(temp_avg_day_c_group = case_when(
    is.na(temp_avg_day_c) ~ NA_character_,
    temp_avg_day_c >= 40 ~ "40c+",
    temp_avg_day_c >= 30 ~ "30c to <40c",
    temp_avg_day_c >= 20 ~ "20c to <30c",
    temp_avg_day_c >= 10 ~ "10c to <20c",
    temp_avg_day_c >= 0 ~ "0c to <10c",
    temp_avg_day_c >= -10 ~ "-10c to <0c",
    temp_avg_day_c >= -20 ~ "-20c to < -10c",
    TRUE ~ "< -20c"
  )
  )



# rename max temp column
noaa_weather <- noaa_weather %>% 
  rename("temp_max_day_f" = "tmax")

# add max temp Fahrenheit groups
# these groups match the Celsius groups below
noaa_weather <- noaa_weather %>% 
  mutate(temp_max_day_f_group = case_when(
    is.na(temp_max_day_f) ~ NA_character_,
    temp_max_day_f >= 104 ~ "104F+",
    temp_max_day_f >= 86 ~ "86F to <104F",
    temp_max_day_f >= 68 ~ "68F to <86F",
    temp_max_day_f >= 50 ~ "50F to <68F",
    temp_max_day_f >= 32 ~ "32F to <50F",
    temp_max_day_f >= 14 ~ "14F to <32F",
    temp_max_day_f >= -4 ~ "-4F to < 14F",
    TRUE ~ "< -4F"
  )
  )

# reorder based on these new groupings
noaa_weather <- noaa_weather %>% 
  mutate(temp_max_day_f_group = factor
         (temp_max_day_f_group, levels =
             c("< -4F",
               "-4F to < 14F",
               "14F to <32F", 
               "32F to <50F",
               "50F to <68F",
               "68F to <86F",
               "86F to <104F",
               "104F+"
             )
         )
  )

# add max temp Celsius column
noaa_weather <- noaa_weather %>% 
  mutate(temp_max_day_c = (as.numeric(temp_max_day_f) - 32)*(5/9))

# add max temp celsius groups
noaa_weather <- noaa_weather %>% 
  mutate(temp_max_day_c_group = case_when(
    is.na(temp_max_day_c) ~ NA_character_,
    temp_max_day_c >= 40 ~ "40c+",
    temp_max_day_c >= 30 ~ "30c to <40c",
    temp_max_day_c >= 20 ~ "20c to <30c",
    temp_max_day_c >= 10 ~ "10c to <20c",
    temp_max_day_c >= 0 ~ "0c to <10c",
    temp_max_day_c >= -10 ~ "-10c to <0c",
    temp_max_day_c >= -20 ~ "-20c to < -10c",
    TRUE ~ "< -20c"
  )
  )



# rename min temp column
noaa_weather <- noaa_weather %>% 
  rename("temp_min_day_f" = "tmin")

# add min temp Fahrenheit groups
# these groups match the Celsius groups below
noaa_weather <- noaa_weather %>% 
  mutate(temp_min_day_f_group = case_when(
    is.na(temp_min_day_f) ~ NA_character_,
    temp_min_day_f >= 104 ~ "104F+",
    temp_min_day_f >= 86 ~ "86F to <104F",
    temp_min_day_f >= 68 ~ "68F to <86F",
    temp_min_day_f >= 50 ~ "50F to <68F",
    temp_min_day_f >= 32 ~ "32F to <50F",
    temp_min_day_f >= 14 ~ "14F to <32F",
    temp_min_day_f >= -4 ~ "-4F to < 14F",
    TRUE ~ "< -4F"
  )
  )

# reorder based on these new groupings
noaa_weather <- noaa_weather %>% 
  mutate(temp_min_day_f_group = factor
         (temp_min_day_f_group, levels =
             c("< -4F",
               "-4F to < 14F",
               "14F to <32F", 
               "32F to <50F",
               "50F to <68F",
               "68F to <86F",
               "86F to <104F",
               "104F+"
             )
         )
  )

# add min temp Celsius column
noaa_weather <- noaa_weather %>% 
  mutate(temp_min_day_c = (as.numeric(temp_min_day_f) - 32)*(5/9))

# add min temp celsius groups
noaa_weather <- noaa_weather %>% 
  mutate(temp_min_day_c_group = case_when(
    is.na(temp_min_day_c) ~ NA_character_,
    temp_min_day_c >= 40 ~ "40c+",
    temp_min_day_c >= 30 ~ "30c to <40c",
    temp_min_day_c >= 20 ~ "20c to <30c",
    temp_min_day_c >= 10 ~ "10c to <20c",
    temp_min_day_c >= 0 ~ "0c to <10c",
    temp_min_day_c >= -10 ~ "-10c to <0c",
    temp_min_day_c >= -20 ~ "-20c to < -10c",
    TRUE ~ "< -20c"
  )
  )

##################################################################
##################################################################

# Join data and create all_data_clean table

all_data_clean <- flights %>% 
  left_join(weather, by = c("origin" = "origin",
                            "time_hour" = "time_hour")) %>% 
  rename("date_ymd" = "date_ymd.x") %>% 
  left_join(noaa_weather, by = c("date_ymd" = "date",
                                 "origin" = "origin")) %>% 
  left_join(planes, by = "tailnum") %>% 
  mutate(plane_type = if_else(is.na(type), "Unknown", type)) %>% 
  mutate(plane_model = if_else(is.na(model), "Unknown", model)) %>% 
  left_join(airports, by = c("dest" = "faa")) %>%
  rename("dest_name" = "name.y") %>% 
  rename("dest_lat" = "lat") %>% 
  rename("dest_long" = "lon") %>% 
  ###################DROP VISIBILITY RECORDS#######################
  # drop records where  visibility is null
  filter(!is.na(visib)) %>% 
  #################################################################
  select(
         # from FLIGHTS
         origin,
         flight_delayed,
         dep_delay,
         delay_group,
         sched_dep_month,
         sched_dep_weekday,
         sched_dep_hour,
         sched_dep_hour_group,
         number_departures_in_sched_dep_hour,
         dest,
         dest_name,
         distance,
         carrier,
         tailnum,
         # from PLANES
         manufacturer,
         plane_type,
         plane_model,
         engine,
         engines,
         seats,
         speed,
         # from WEATHER, NOAA_WEATHER
         visib,
         visib_group,
         wind_speed_mph,
         wind_speed_beaufort_name,
         wind_gust_mph,
         wind_gust_beaufort_name,
         wind_dir,
         wind_dir_cardinal,
         precip_day_total_inch,
         precip_day_total_cm,
         precip_day_total_inch_group,
         snowfall_day_total_inch,
         snowfall_day_total_inch_group,
         snow_depth_day_total_inch,
         snow_depth_day_total_inch_group,
         temp_avg_day_c,
         temp_avg_day_c_group,
         temp_avg_day_f,
         temp_avg_day_f_group,
         temp_max_day_c,
         temp_max_day_c_group,
         temp_max_day_f,
         temp_max_day_f_group,
         temp_min_day_c,
         temp_min_day_c_group,
         temp_min_day_f,
         temp_min_day_f_group,
         # from AIRPORTS
         dest_lat,
         dest_long
  ) 


# Write all_data_clean to a csv in the data_clean folder

write_csv(all_data_clean, here("data_clean/all_data_clean.csv"))


