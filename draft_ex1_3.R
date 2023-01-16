library(tidyverse)
library(lubridate)
options(digits=7)

# get raw HOBO data from github via raw link

my_HOBO <- read.csv("https://raw.githubusercontent.com/data-hydenv/data/master/hobo/2023/raw/10350049.csv", skip = 1)


# get calibration

t_cal <- read.csv("https://raw.githubusercontent.com/data-hydenv/data/master/hobo/2023/calibration.csv")

# Data should be prepared as data frame with one
# line per time step and four columns with four column
# names: id, dttm, temp and lux. Stick to the following
# rules:

# one header line

head(my_HOBO) # skipped the other line at import 

# consecutive ID starting with 1
# for now it's called "Anz" but all columns will be renamed in one go later so that's OK

# a string with date and time information in this
# exact format (YYYY-MM-DD HH:MM), but without
# any other information like “T”, “Z” or “UTC” or
# other timezone information.

# in this case the timestamp is still interpreted as a string
# use lubridate to parse and convert to POSX
# its month - day - year - hour - minute - sec - AM/PM
# timezone is in the column name (GMT+01)
# looks like this:
library(lubridate)

my_HOBO$Datum.Zeit..GMT.01.00[1]
class(my_HOBO$Datum.Zeit..GMT.01.00[1])

my_HOBO$Datum.Zeit..GMT.01.00 <- mdy_hms(my_HOBO$Datum.Zeit..GMT.01.00)

my_HOBO$Datum.Zeit..GMT.01.00[1]
class(my_HOBO$Datum.Zeit..GMT.01.00[1])

# the time zone of the data has to be UTC+1 (old:
# GMT+1)

tz(my_HOBO$Datum.Zeit..GMT.01.00)

# ld thinks it's UTC but it is actually UTC+01

my_HOBO$Datum.Zeit..GMT.01.00 <- force_tz(my_HOBO$Datum.Zeit..GMT.01.00, "Europe/Berlin" )

# tidy it all up, truncate later

my_HOBO <- as_tibble(my_HOBO) %>% 
  select(., 1:4) %>% 
  rename(id = 1, dttm = 2, temp = 3, lux = 4)

head(my_HOBO)


# calibration

caltime <- t_cal$This.is.the.calibration.value[3]

calib_line <- which(my_HOBO$dttm == caltime)

num(my_HOBO[calib_line,]$temp, digits = 3) # it's 19.7 where it is supposed to be 19.3 so my hobo
# measured 0.4 deg C plus

# so -0.4 is now my calibration offset

my_HOBO$temp <- my_HOBO$temp-0.4

# The timeseries start on 1th of December at 00:00 and ends on the 7th of January 2023 at 23:50
# (UTC+1)

start_time <- ymd_hms("2022-12-01 00:00:00")
end_time <- ymd_hms("2023-01-07 23:50:00")
time_range <- interval(start_time, end_time)

my_HOBO <- my_HOBO[my_HOBO$dttm %within% time_range,]

# this f**ed up the id column...
