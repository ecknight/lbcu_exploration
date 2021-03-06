library(tidyverse)
library(lubridate)
library(data.table)
library(ggspatial)
library(sf)
library(adehabitatLT)
library(changepoint)

options(scipen = 999)

dat <- read.csv("Data/LBCUFiltered&PredictedData.csv") %>% 
  mutate(date = ymd_hms(date))

#1. Split into single legs of migration for each individual----
dat.leg <- dat %>% 
  mutate(legseason = ifelse(doy > 32 & doy < 135, "1spring", "2fall"),
         migyear = ifelse(legseason=="2fall" & doy <= 32, year-1, year),
         migdoy = ifelse(legseason=="2fall" & doy <= 32, doy + 365, doy),
         legid = paste0(id,"-",migyear,"-",legseason)) %>% 
  group_by(legid) %>% 
  arrange(date) %>% 
  mutate(order = row_number()) %>% 
  ungroup()

#2. Calculate ltraj for each single leg----
#Transform to UTM
dat.utm <- st_as_sf(dat.leg, coords=c("lon", "lat"), crs=4326) %>% 
  st_transform(crs=3857) %>% 
  st_coordinates() %>% 
  cbind(dat.leg)

#Calculate
traj <- as.ltraj(xy=dat.utm[,c("X", "Y")],
                  id=dat.utm$legid,
                  date=dat.utm$order,
                 typeII=FALSE,
                  proj4string = CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs"))

#Put back together with data
dat.traj <- rbindlist(traj) %>% 
  dplyr::select(-date) %>% 
  rename(X=x, Y=y) %>% 
  full_join(dat.utm) %>% 
  rename(nsd = R2n) %>% 
  arrange(legid, date)

#3. Visualize----
#Visualize
ids <- unique(dat.traj$id)

for(i in 1:length(ids)){
  
  dat.i <- dat.traj %>% 
    dplyr::filter(id==ids[i]) %>% 
    arrange(date) %>% 
    group_by(legid) %>% 
    mutate(n = row_number()) %>% 
    ungroup()
  
  ggplot(dat.i) +
    geom_line(aes(x=migdoy, y=nsd)) +
    facet_wrap(~legid, scales="free") +
    ggtitle(ids[i])
  
  ggsave(filename=paste0("Figures/ltraj/", ids[i], ".jpeg"))
  
}

#4. Check # of birds----
length(unique(dat.traj$id))
#158 - good

#5. Save----
write.csv(dat.traj, "Data/LBCUFiltered&Predicted&LeggedData.csv", row.names = FALSE)
