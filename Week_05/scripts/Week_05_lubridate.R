### Learn bout the join family of functions ######
### Created by: Keanu Rochette-Yu Tsuen
### Date: 2024-09-24


### Libraries ##########
library(tidyverse)
library(here)
library(lubridate)

### Introduction functions of lubridate ##########
now()
now(tzone= "EST") #time on East coast
now(tzone= "GMT") #time in GMT
today() # today's date
today(tzone= "GMT")
am(now()) #is it morning
leap_year(now()) # is it a leap year


### Date specification for lubridate ##########
# date MUST be a character first before converting to date
# ymd(), mdy(), dmy() --> convert to ISO dates
ymd("2021-02-24")
mdy("February 24 2024")
dmy("24/02/2021")

#### Date and Time specification for lubridate ##########
#ymd_hms(), mdy_hm(), mdy_hms()
ymd_hms("2021-02-24 10:22:20 PM")
mdy_hms("02-24-2021 22:22:20 PM")

### Extracting specific date or time elements from datetimes ##########
#make a character string
datetimes<-c("02/24/2021 22:22:20",
             "02/25/2021 11:21:10",
             "02/26/2021 8:01:52")

#convert to datetimes
datetimes <- mdy_hms(datetimes)

#extracting only the months
month(datetimes, label = T, abbr = F)
      # label : converts number to month abbrv
      # abbr : either shows long or short name of months

#extracting only the day and weekday
day(datetimes) # gives number of month
wday(datetimes, label = T) # gives weekday, only if label = T

#also applicable for hour(), minute(), second() 

#changing the date time to new time zone
datetimes + hours(4)
# CAREFUL !!!! hour() extracts hour from datetime, hours() adds hours
datetimes + days(2)

# Rounding dates
round_date(datetimes, "minute") #round to the nearest minute
round_date(datetimes, "5 mins") # round to the nearest 5 min





