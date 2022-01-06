library(tidyverse)
library(lubridate)

#1. Read in data----
dat.raw <- read.csv("Data/LBCUCleanedData.csv")
dat.filter <- read.csv("Data/LBCU_FilteredData_Segmented.csv")

#2. Wrangle----
dat.join <- dat.filter %>% 
  dplyr::select(study, id, year, doy, segment, season, stopover, winter) %>% 
  inner_join(dat.raw) %>% 
  dplyr::select(study, id, name, tag, depseason, sensor, sex, mass, lat, long, year, datetime, doy, argos, error, smaj, smin, eor, segment, season, stopover, winter)

#3. Save----
write.csv(dat.join, "Data/LBCU_RawData_Segmented.csv", row.names = FALSE)
write.csv(dat.join, "/Users/ellyknight/Dropbox/LBCU/LBCU_RawData_Segmented.csv", row.names = FALSE)