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
GAR_raw$UTC <- ymd_hms(GAR_raw$UTC, tz="UTC")
class(GAR_raw$Lokalzeit)
class(GAR_raw$UTC)
# and clip to timeframe
# Wdat_GAR <- GAR_raw %>%
#   select(., -UTC) %>%
#   filter(., between(Lokalzeit, start_time, end_time))
# for some reason this fails to deliver the correct amount of obs. lines...  timezone trouble?
last(Wdat_GAR$Lokalzeit)
last(Wdat_WBI$dttm)
first(Wdat_GAR$Lokalzeit)
first(Wdat_WBI$dttm)
# TODO fix this sh**
# end_time2 <- ymd_hms("2023-01-08 00:00:00")
Wdat_GAR <- GAR_raw %>% 
  select(., -UTC) %>% 
  filter(., between(Lokalzeit, start_time, end_time))
# this seems to work but now i guess the last date is wrong?
first(Wdat_GAR$Lokalzeit)
last(Wdat_GAR$Lokalzeit)
last(Wdat_DWD$MESS_DATUM)
# the fuck?
which((Wdat_GAR$Lokalzeit == dttm))
# so line 371 is the culprit...
Wdat_GAR[369:375,]
# there's a line missing...why???
which(GAR_raw$Lokalzeit=="2022-12-16 10:00:00")
GAR_raw[390:396,]
#hmmmmmmmm.... it's also missing in the raw data so i did not ef this up
GAR_raw <- read.csv("C:/Users/johan/Desktop/DCSM_home/DCSM23/10min/Freiburg_Garten_2022-11-30_2023-01-08.csv")
which(GAR_raw$Lokalzeit=="2022-12-16 10:00:00")
GAR_raw[390:396,] # they efd up 11 o clock...

GAR_raw <- GAR_raw %>% 
  add_row(UTC = "2022-12-16 10:00:00", Lokalzeit = "2022-12-16 11:00:00", Lufttemperatur...C. = NA, .before = 395)

GAR_raw[390:396,]
# that's better, now the whole converting and clipping procedure again
GAR_raw$Lokalzeit <- ymd_hms(GAR_raw$Lokalzeit)
GAR_raw$UTC <- ymd_hms(GAR_raw$UTC)

Wdat_GAR <- GAR_raw %>%
  filter(., between(Lokalzeit, start_time, end_time))
# this took way too long...




# Freiburg Stadtklimastation
URB_raw <- read.csv("C:/Users/johan/Desktop/DCSM_home/DCSM23/10min/produkt_air_temperature_13667_akt.txt", sep=";")
# convert POSX
URB_raw$MESS_DATUM <- ymd_h(URB_raw$MESS_DATUM)
# and clip
Wdat_URB <- URB_raw %>% 
  filter(.,between(MESS_DATUM, start_time, end_time)) %>% 
  select(MESS_DATUM, LUFTTEMPERATUR) %>% 
  mutate(dttm=dttm)
# phew..correct nr of observations this time..can deselect orig. timestamp later 
# TODO


# DWD Station 1443
DWD_raw <- read.csv("C:/Users/johan/Desktop/DCSM_home/DCSM23/10min/produkt_tu_stunde_20210718_20230118_01443.txt", sep=";")
# POSX conversio
DWD_raw$MESS_DATUM <- ymd_h(DWD_raw$MESS_DATUM)
Wdat_DWD <- DWD_raw %>% 
  filter(., between(MESS_DATUM, start_time, end_time)) %>% 
  select(., MESS_DATUM, TT_TU) %>% 
  mutate(dttm=dttm)


# check datetimes
# 
# test <- Wdat_DWD %>% 
#   select(MESS_DATUM, dttm) %>% 
#   mutate(Gartime=Wdat_GAR$Lokalzeit) %>% 
#   mutate(Urbtime=Wdat_URB$MESS_DATUM) %>% 
#   mutate(wbitime=Wdat_WBI)
# jawoll this worked...



