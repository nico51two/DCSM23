library(tidyverse)
library(lubridate)
library(dplyr)
options(digits = 3)

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


my_HOBO$Datum.Zeit..GMT.01.00[1]
class(my_HOBO$Datum.Zeit..GMT.01.00[1])

my_HOBO$Datum.Zeit..GMT.01.00 <- mdy_hms(my_HOBO$Datum.Zeit..GMT.01.00)

my_HOBO$Datum.Zeit..GMT.01.00[1]
class(my_HOBO$Datum.Zeit..GMT.01.00[1])

# the time zone of the data has to be UTC+1 (old:
# GMT+1)

tz(my_HOBO$Datum.Zeit..GMT.01.00)
# TODO get this timezone stuff right

# ld thinks it's UTC but it is actually UTC+01

# my_HOBO$Datum.Zeit..GMT.01.00 <- with_tz(my_HOBO$Datum.Zeit..GMT.01.00, "Europe/Berlin" )

# tidy it all up, truncate later

my_HOBO <- as_tibble(my_HOBO) %>% 
  select(., 1:4) %>% 
  rename(id = 1, dttm = 2, temp = 3, lux = 4)

head(my_HOBO)


# calibration

caltime <- t_cal$This.is.the.calibration.value[3]

calib_line <- which(my_HOBO$dttm == caltime)

meas_temp <- num(my_HOBO[calib_line,]$temp, digits = 3) # it's 19.7 where it is supposed to be 19.3 so my hobo
# measured 0.4 deg C plus

meas_temp <- meas_temp[1]

tru_temp <- num(as.numeric(t_cal$to.calibrate.your.Hobo.measurements.in..C.[3]), digits = 3)

tru_temp <- tru_temp[1]

cal_offset <- meas_temp-tru_temp

cal_offset <- as.numeric(cal_offset[1])

# so -0.267 is now my calibration offset

my_HOBO$temp <- my_HOBO$temp-cal_offset

# The timeseries start on 1th of December at 00:00 and ends on the 7th of January 2023 at 23:50
# (UTC+1)

start_time <- ymd_hms("2022-12-01 00:00:00")
end_time <- ymd_hms("2023-01-07 23:50:00")
time_range <- interval(start_time, end_time)

# filter accordingly

my_HOBO <- my_HOBO %>% 
  filter(between(dttm, start_time, end_time))

# fix id column

my_HOBO <- my_HOBO %>% 
  mutate(., id = c(1:length(my_HOBO$id)))

# fix decimals of lux vector

#my_HOBO$lux <- num(my_HOBO$lux, digits = 3)

# and temp again because the calibration fed that up

# my_HOBO$temp <- num(my_HOBO$temp, digits = 3)

my_HOBO$temp <-  format(my_HOBO$temp, digits=3, nsmall=3)
my_HOBO$lux <-  format(my_HOBO$lux, digits=3, nsmall=3)


# drop seconds from dttm

my_HOBO$dttm <- format(my_HOBO$dttm, "%Y-%m-%d %H:%M")

# write to file
getwd()
setwd("C:/Users/johan/Desktop/DCSM_home/DCSM23/10min")
write_csv(my_HOBO, file = "10350049.csv")



# first expl plot

plot(my_HOBO$temp, type = "l")

range(my_HOBO$lux,na.rm = T)
my_HOBO$temp[which(my_HOBO$temp==min(my_HOBO$temp))]


# TODO test and benchmark re-import using different functions
# TODO add data head after re import
# TODO re-import for QC - for now I will design the QC using the present objects

HOBO_qc <- read.csv("https://raw.githubusercontent.com/data-hydenv/data/master/hobo/2023/10_minutes/10350049.csv")

# quality control

# plausible values

range(HOBO_qc$temp)

# this is well within the measurement range of the device
# all data points passed the measurement range check

# plausible rate of change

# lag one column, then mutate difference to new column

HOBO_wip <- HOBO_qc %>% 
  mutate(., lagged=lag(temp)) %>% 
  mutate(., delta=temp-lagged)

range(na.omit(HOBO_wip$delta))
# delta values way out of the legal range so these must be flagged

# 
# Air temperature: 0.1∞C over the past 60 minutes;
# ï Dew point temperature: 0.1∞C over the past 60 minutes;
# ï Ground temperature: 0.1∞C over the past 60 minutes


flagR <- function(x) {
  ifelse(x>=1, 1,0)
}


HOBO_wip <- cbind(HOBO_wip,QC_proc=HOBO_wip$delta)

flag <- sapply(HOBO_wip$QC_proc,flagR)

HOBO_wip$QC_proc <- flag

length(which(flag==1))

# 24 "bad" data points

# first expl plot

plot(my_HOBO$temp, type = "l")

range(my_HOBO$lux,na.rm = T)
my_HOBO$temp[which(my_HOBO$temp==min(my_HOBO$temp))]


# TODO test and benchmark re-import using different functions
# TODO add data head after re import
# TODO re-import for QC - for now I will design the QC using the present objects

HOBO_qc <- read.csv("https://raw.githubusercontent.com/data-hydenv/data/master/hobo/2023/10_minutes/10350049.csv")

# quality control

# plausible values

range(HOBO_qc$temp)

# this is well within the measurement range of the device
# all data points passed the measurement range check

# plausible rate of change

# lag one column, then mutate difference to new column

HOBO_wip <- HOBO_qc %>% 
  mutate(., lagged=lag(temp)) %>% 
  mutate(., delta=temp-lagged)

range(na.omit(HOBO_wip$delta))
# delta values way out of the legal range so these must be flagged

# 
# Air temperature: 0.1∞C over the past 60 minutes;
# ï Dew point temperature: 0.1∞C over the past 60 minutes;
# ï Ground temperature: 0.1∞C over the past 60 minutes


flagR <- function(x) {
  ifelse(x>=1, 1,0)
}

# HOBO_wip <- cbind(HOBO_wip,qc1=HOBO_wip$delta)

qc_R <- sapply(HOBO_wip$delta,flagR)

HOBO_wip <- cbind(HOBO_wip,qc_R)

length(which(flag=="bad"))

# 24 "bad" data points

hist(which(flag==1),breaks = 60)

# qc 2 min. variability

# 3.1.3 Minimum variability (Persistence)
# If temperature has not changed during the last 60 minutes (i.e. data point Ti plus 5 data points before, so
#                                                            from Ti−1 to Ti−5) the corresponding data point Ti failed in this QCP

## TODO rename delta to delta_t
## TODO make positive flag = TRUE in flagR (neg.=FALSE) so bool. logic can be applied when all three are finished

# vector for qc_per flag with NA in 1:4
# iterate over delta_t
# use length(unique(subset of delta_t))

# zoo::rollapply

# still needs a fct to check equality length(unique(x)) == 1

flagP <- function(x){
  ifelse(length(unique(x)) == 1, 1,0)  
}

# rollapply this

library(zoo)

qc2 <- rollapply(HOBO_wip$delta, width=6,FUN=flagP)

# this seems to work

# construct result vector
# TODO check if this actually works by manually indexing, subsetting 

qc_P <- c(1:length(HOBO_wip$delta))
qc_P[1:5] <- NA
qc_P[6:length(qc_P)] <- qc2
HOBO_wip <- cbind(HOBO_wip,qc_P)

# qc 3: light

# this hobo was in a plant pot so we first need to check the range of lux values before deciding

hist(HOBO_wip$lux, breaks = 25)
summary(HOBO_wip$lux) # quite the spread
sd(HOBO_wip$lux)
toplux <- filter(HOBO_wip,lux<=500 & lux>1)
hist(toplux$lux,breaks=50)
