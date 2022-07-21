# Comparison of Lowries Run streamgage with precip and conductivity
library(dplyr)
library(lubridate)
library(readr)
library(ggplot2)

precip <- read_csv("/Users/davidkahler/Documents/Teaching/outreach/LowriesRun/precip2022data.csv")
precip <- precip %>%
     mutate(dt=force_tz(ymd_hms(Timestamp), tzone = "US/Eastern"),
            hr=hour(dt),
            dh=as.numeric(as_date(dt))+(hr/24)) %>%
     group_by(dh) %>%
     summarize(temp=mean(Thermometer, na.rm = TRUE),
               precip=sum(`Rain Gauge`,na.rm = TRUE)) %>%
     mutate(da=floor(dh),
            hr=round((dh-da)*24),
            dy=as_date(da),
            dt=ymd_h(paste(dy,hr))) %>%
     select(dt,temp,precip)

gage <- read_csv("/Users/davidkahler/Documents/Teaching/outreach/LowriesRun/Lowries18May2022.csv", skip=31)
gage <- gage %>%
     mutate(dt=dmy_hms(`Date / Time`),
            hr=hour(dt),
            dh=as.numeric(as_date(dt))+(hr/24)) %>%
     group_by(dh) %>%
     summarize(temp=mean(`Temperature (°C)`, na.rm = TRUE),
               pres=6.89476*mean(`Pressure (psi)`, na.rm = TRUE), # this is now in kPa
               depth=1000*pres/(997*9.81), # estimate of density of water
               ec=mean(`Conductivity (µS/cm)`,na.rm = TRUE)) %>%
     mutate(da=floor(dh),
            hr=round((dh-da)*24),
            dy=as_date(da),
            dt=ymd_h(paste(dy,hr))) %>%
     select(dt,temp,pres,depth,ec)

