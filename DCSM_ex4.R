# DCSM_ex4
library(lubridate)

# fill up all the NA in your
# hobo_hourly from chapter 3.3 based on a regression model between your station
# and a reference station. 

# WBI Station
# data import

WBI_raw <- read.csv("C:/Users/johan/Desktop/DCSM_home/DCSM23/10min/Stunde_096.csv", sep=";")


WBI_raw$Tag <- dmy(WBI_raw$Tag)
class(WBI_raw$Tag)


WBI_raw$Stunde <- hm(WBI_raw$Stunde)
class(WBI_raw$Stunde)

# clip to timeframe
start_time <- ymd_hms("2022-12-01 00:00:00")
end_time <- ymd_hms("2023-01-07 23:50:00")
time_range <- interval(start_time, end_time)



