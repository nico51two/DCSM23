# DCSM_ex4
library(lubridate)
library(tidyverse)
# fill up all the NA in your
# hobo_hourly from chapter 3.3 based on a regression model between your station
# and a reference station. 

# WBI Station
# data import from hdd
WBI_raw <- read.csv("C:/Users/johan/Desktop/DCSM_home/DCSM23/10min/Stunde_096.csv", sep=";")
# convert to proper POSX or ld object
WBI_raw$Tag <- dmy(WBI_raw$Tag)
class(WBI_raw$Tag)
# works semi well with this timestamp...
WBI_raw$Stunde <- hm(WBI_raw$Stunde)
class(WBI_raw$Stunde)

# i'll make my own
# declare time frame
start_time <- ymd_hms("2022-12-01 00:00:00")
end_time <- ymd_hms("2023-01-07 23:50:00")
time_range <- interval(start_time, end_time)
# make my own dttm and scrap the ones that came with the WBI data
dttm <- seq(start_time, end_time, by= "hours")
# clip WBI data, add dttm and rename cols
Wdat_WBI <- WBI_raw %>% 
  filter(.,between(Tag, date(start_time), date(end_time))) %>% 
  select(.,temp=AVG_TA200) %>% 
  mutate(dttm=dttm)
# TODO format the timestamp to get rid of seconds eventually (if the assignment says so)


# FREIBURG Garten station
GAR_raw <- read.csv("C:/Users/johan/Desktop/DCSM_home/DCSM23/10min/Freiburg_Garten_2022-11-30_2023-01-08.csv")
# fix date time class
GAR_raw$Lokalzeit <- ymd_hms(GAR_raw$Lokalzeit, tz="Europe/Berlin")
class(GAR_raw$Lokalzeit)
# and clip to timeframe
Wdat_GAR <- GAR_raw %>% 
  select(., -UTC) %>% 
  filter(., between(Lokalzeit, start_time, end_time))
# for some reason this fails to deliver the correct amount of obs. lines...  timezone trouble?
last(Wdat_GAR$Lokalzeit)
last(Wdat_WBI$dttm)
first(Wdat_GAR$Lokalzeit)
first(Wdat_WBI$dttm)
# TODO fix this sh**



# Freiburg Stadtklimastation
URB_raw <- read.csv("C:/Users/johan/Desktop/DCSM_home/DCSM23/10min/produkt_air_temperature_13667_akt.txt", sep=";")
# convert POSX
URB_raw$MESS_DATUM <- ymd_h(URB_raw$MESS_DATUM)
# and clip
Wdat_URB <- URB_raw %>% 
  filter(.,between(MESS_DATUM, start_time, end_time)) %>% 
  select(MESS_DATUM, LUFTTEMPERATUR)
