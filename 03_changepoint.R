library(tidyverse)
library(lubridate)
library(data.table)
library(ggspatial)
library(sf)
library(adehabitatLT)
library(changepoint)

options(scipen = 999)

dat <- read.csv("Data/LBCUFilteredData.csv") %>% 
  mutate(date = ymd_hms(date),
         year = year(date),
         doy = yday(date))

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

#3. Tidy----
#Remove incomplete legs
nsdmax <- dat.traj %>% 
  group_by(legid) %>% 
  summarize(nsdmax = max(nsd)) %>% 
  dplyr::filter(nsdmax > 38036899358.37)
hist(nsdmax$nsdmax)

dat.full <- dat.traj %>% 
  dplyr::filter(legid %in% nsdmax$legid)
  

#4. Changepoint----
legs <- unique(dat.full$legid)

cp.list <- list()
for(i in 1:length(legs)){
  dat.i <- dat.full %>% 
    dplyr::filter(legid==legs[i])
  
  cp.i <- cpt.meanvar(dat.i$nsd, method = "BinSeg", Q=10)
  
  plot(cp.i)
  
  cp.list[[i]] <- data.frame(order = cp.i@cpts,
                   cp.nsd = cp.i@param.est$mean,
                   cp.nsd.var = cp.i@param.est$variance,
                   legid=legs[i]) %>% 
    mutate(cp = row_number())

}

cp <- rbindlist(cp.list)

dat.cp <- dat.full %>% 
  left_join(cp) %>% 
  group_by(legid) %>% 
  fill(cp, .direction="down") %>% 
  fill(cp.nsd, .direction="down") %>% 
  fill(cp.nsd.var, .direction="down") %>% 
  mutate(cp=ifelse(is.na(cp), 0, cp))

#5. Visualize----
#Visualize
ids <- unique(dat.cp$id)

for(i in 1:length(ids)){
  
  dat.i <- dat.cp %>% 
    dplyr::filter(id==ids[i]) %>% 
    arrange(date) %>% 
    group_by(legid) %>% 
    mutate(n = row_number()) %>% 
    ungroup()
  
  ggplot(dat.i) +
    geom_line(aes(x=migdoy, y=nsd)) +
    geom_line(aes(x=migdoy, y=cp.nsd), colour="red") +
    facet_wrap(~legid) +
    ggtitle(ids[i])
  
  ggsave(filename=paste0("Figures/cp/", ids[i], ".jpeg"))
  
}

dat.id <- dat.full %>% 
  dplyr::filter(legid=="46768108-2016-2fall")
View(dat.id)