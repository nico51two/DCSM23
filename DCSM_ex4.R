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

# and clip to timeframe
# Wdat_GAR <- GAR_raw %>%
#   select(., -UTC) %>%
#   filter(., between(Lokalzeit, start_time, end_time))
# for some reason this fails to deliver the correct amount of obs. lines...  timezone trouble?
# last(Wdat_GAR$Lokalzeit)
# last(Wdat_WBI$dttm)
# first(Wdat_GAR$Lokalzeit)
# first(Wdat_WBI$dttm)
# TODO fix this sh**
# end_time2 <- ymd_hms("2023-01-08 00:00:00")
# Wdat_GAR <- GAR_raw %>% 
#   select(., -UTC) %>% 
#   filter(., between(Lokalzeit, start_time, end_time))
# this seems to work but now i guess the last date is wrong?
# first(Wdat_GAR$Lokalzeit)
# last(Wdat_GAR$Lokalzeit)
# last(Wdat_DWD$MESS_DATUM)
# the fuck?
# which((Wdat_GAR$Lokalzeit == dttm))
# so line 371 is the culprit...
# Wdat_GAR[369:375,]
# there's a line missing...why???
# which(GAR_raw$Lokalzeit=="2022-12-16 10:00:00")
# GAR_raw[390:396,]
#hmmmmmmmm.... it's also missing in the raw data so i did not ef this up
# GAR_raw <- read.csv("C:/Users/johan/Desktop/DCSM_home/DCSM23/10min/Freiburg_Garten_2022-11-30_2023-01-08.csv")
# which(GAR_raw$Lokalzeit=="2022-12-16 10:00:00")
# GAR_raw[390:396,] # they efd up 11 o clock...

# correct missing line in GAR_raw
GAR_raw <- GAR_raw %>% 
  add_row(UTC = "2022-12-16 10:00:00", Lokalzeit = "2022-12-16 11:00:00", Lufttemperatur...C. = NA, .before = 395)

GAR_raw[390:396,]
# that's better, now the whole converting and clipping procedure again

# GAR_raw$Lokalzeit <- ymd_hms(GAR_raw$Lokalzeit, tz="Europe/Berlin")
# GAR_raw$UTC <- ymd_hms(GAR_raw$UTC, tz="UTC")
# class(GAR_raw$Lokalzeit)
# class(GAR_raw$UTC)


# convert to POSX
GAR_raw$Lokalzeit <- ymd_hms(GAR_raw$Lokalzeit)
GAR_raw$UTC <- ymd_hms(GAR_raw$UTC)
class(GAR_raw$UTC)

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

# stations df
tempDF <- Wdat_DWD %>% 
  select(., time=dttm, tempDWD=TT_TU) %>% 
  mutate(tempURB=Wdat_URB$LUFTTEMPERATUR) %>% 
  mutate(tempGar=Wdat_GAR$Lufttemperatur...C.) %>% 
  mutate(tempWBI=Wdat_WBI$temp)

# tempWBI still has the comma decimals
tempDF$tempWBI <- scan(text=tempDF$tempWBI, dec=",", sep=".")


# my hobo df
# getwd()
myHOBO <- read.csv("C:/Users/johan/Desktop/DCSM_home/DCSM23/10min/10350049_Th.csv" )

tempDF <- tempDF %>% 
  mutate(myHOBO$th)

# # tempDF <- as_tibble(tempDF)
# tempDF$tempWBI <- as.numeric(tempDF$tempWBI)

tempDF <- cbind(tempDF,HOBOtemp=myHOBO$th)
tempDF$HOBOtemp <- as.numeric(tempDF$HOBOtemp)


# manualy zoomed in comparison graph

date1 <- tempDF$time[210]
date2 <- tempDF$time[245]

ggplot(tempDF,aes(time))+
  geom_line(tempDF,mapping=aes(y=tempURB),color="blue")+
  geom_line(tempDF,mapping=aes(y=tempGar),color="black")+
  geom_line(tempDF,mapping=aes(y=tempWBI),color="green")+
  geom_line(tempDF,mapping=aes(y=tempDWD),color="red")+
  geom_line(tempDF,mapping=aes(y=HOBOtemp),color="purple")+
  ylim(-5,5)+
  xlim(c(date1,date2))

# tempWBI is the closest fit

ggplot(tempDF,aes(time))+
  #geom_line(tempDF,mapping=aes(y=tempURB),color="blue")+
  #geom_line(tempDF,mapping=aes(y=tempGar),color="black")+
  geom_line(tempDF,mapping=aes(y=tempWBI),color="green")+
  #geom_line(tempDF,mapping=aes(y=tempDWD),color="red")+
  geom_line(tempDF,mapping=aes(y=HOBOtemp),color="purple")
  #ylim(-5,5)+
  #xlim(c(date1,date2))

# TODO plot legends and axis labels, color palette

# MODEL 1: WBI
modWBI <- lm(tempDF$HOBOtemp~tempDF$tempWBI, tempDF)
summary(modWBI)

# MODEL 2: DWD
modDWD <- lm(tempDF$HOBOtemp~tempDF$tempDWD, tempDF)
summary(modDWD)

# MODEL 3: URB
modURB <- lm(tempDF$HOBOtemp~tempDF$tempURB, tempDF)
summary(modURB)

# MODEL 4: GAR
modGAR <- lm(tempDF$HOBOtemp~tempDF$tempGar, tempDF)
summary(modGAR)

# table of model coefficients & R^2

Intercepts <- c(summary(modDWD)$coefficients[1,4],
                  summary(modURB)$coefficients[1,4],
                  summary(modGAR)$coefficients[1,4],
                  summary(modWBI)$coefficients[1,4])

R_squ <- c(summary(modDWD)$r.squared,
           summary(modURB)$r.squared,
           summary(modGAR)$r.squared,
           summary(modWBI)$r.squared)

stations <- c("DWD","URB","GAR","WBI")

MOD_RES <- arrange(tibble(stations,Intercepts,R_squ))

summary(modWBI)
# the WBI model sports the best fit according to R^2
# corroborates what we saw in the plot


# fill in NA using regression coefficients from the WBI model










# create result df

# hobo_hr_corr <- tempDFtest %>% 
#   select(dttm=time, th=HOBOcorr) %>% 
#   mutate(origin="H")


# hobo_hr_corr$origin[which(is.na(tempDFtest$HOBOtemp))] <- "R"


# hobo_hr_corr$dttm <- format(hobo_hr_corr$dttm, "%Y-%m-%d %H:%M:%S")
# hobo_hr_corr$th <-  format(hobo_hr_corr$th, digits=3, nsmall=3)
# 
# write_csv(hobo_hr_corr, file = "10350049_Th.csv" )
