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

#3. Tidy----
#Remove incomplete legs
nsdmax <- dat.traj %>% 
  group_by(legid) %>% 
  summarize(nsdmax = max(nsd)) %>% 
  dplyr::filter(nsdmax > 38036899358.37)
hist(nsdmax$nsdmax)

dat.full <- dat.traj %>% 
  dplyr::filter(legid %in% nsdmax$legid)

#4. ID Birds that have 2 wintering grounds----
wint2 <- c("46757899-2015-2fall",
           "46757899-2016-2fall",
           "77635674-2015-2fall",
           "77636733-2015-2fall",
           "77636733-2016-2fall",
           "77636733-2017-2fall",
           "77637122-2015-2fall",
           "77637122-2016-2fall",
           "77637122-2017-2fall",
           "164383320-2016-2fall",
           "164384286-2016-2fall",
           "281980840-2019-2fall",
           "290350486-2017-2fall",
           "290350486-2018-2fall",
           "290350486-2019-2fall",
           "290350903-2017-2fall",
           "290351183-2017-2fall",
           "290351183-2018-2fall",
           "290351183-2019-2fall",
           "290351183-2020-2fall",
           "290351953-2019-2fall",
           "290352179-2017-2fall",
           "290352179-2018-2fall",
           "290352179-2020-2fall",
           "477992060-2018-2fall",
           "877974163-2019-2fall",
           "877979829-2019-2fall",
           "877979829-2020-2fall",
           "877984360-2019-2fall",
           "877984561-2019-2fall",
           "877984561-2020-2fall",
           "890830029-2019-2fall",
           "890830029-2020-2fall",
           "890830029-2021-2fall",
           "890834800-2019-2fall",
           "1146533212-2020-2fall",
           "1419060032-2020-2fall",
           "1425578518-2019-2fall",
           "1418923728-2020-2fall",
           "1425582803-2019-2fall",
           "1425587959-2019-2fall",
           "1425590049-2019-2fall",
           "1546947529-2021-2fall",
           "1613983438-2021-2fall",
           "1615638072-2021-2fall")

wint3 <- c("877984360-2020-2fall",
           "1418878943-2020-2fall",
           "1418896449-2020-2fall",
           "1418899142-2020-2fall",
           "1418899142-2021-2fall",
           "164384078-2016-2fall",
           "279818289-2017-2fall",
           "281981679-2017-2fall",
           "46769840-2015-2fall",
           "164383320-2017-2fall")
 
wint2.unsure <- c("46768108-2015-2fall",
                  "71767453-2015-2fall",
                  "71767453-2017-2fall",
                  "279272704-2017-2fall",
                  "281981414-2017-2fall",
                  "973657763-2019-2fall",
                  "973657763-2020-2fall",
                  "1614016223-2021-2fall")

weird <- c("46200880-2018-1spring",
           "46200880-2020-1spring",
           "46768108-2016-1spring",
           "71767680-2019-1spring",
           "145696972-2016-1spring",
           "145698291-2020-1spring",
           "172070515-2017-1spring",
           "890858132-2019-2fall",
           "1419060032-2021-1spring")
  
#4. Changepoint----
legs <- dat.full %>% 
  dplyr::select(id, legid) %>% 
  unique() %>% 
  mutate(cps = case_when(legid %in% wint2 ~ 3,
                         legid %in% wint3 ~ 4),
         cps = ifelse(is.na(cps), 2, cps))

cp.list <- list()
for(i in 1:nrow(legs)){
  
  dat.i <- dat.full %>% 
    dplyr::filter(legid==legs$legid[i])
  
  cp.i <- cpt.mean(dat.i$nsd, method="BinSeg", Q=legs$cps[i])
  
  cp.list[[i]] <- data.frame(order = cp.i@cpts,
                   cp.nsd = cp.i@param.est$mean,
                   cps = legs$cps[i],
                   legid=legs$legid[i]) %>% 
    mutate(cp = row_number())

}

cp <- rbindlist(cp.list)

dat.cp <- dat.full %>% 
  left_join(cp) %>% 
  group_by(legid) %>% 
  fill(cp, .direction="up") %>% 
  fill(cp.nsd, .direction="up") %>% 
  fill(cps, .direction="up") %>% 
  arrange(id, date) %>% 
  ungroup()

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
  
  pts12.i <- dat.i %>% 
    group_by(legid, cp) %>% 
    summarize(migdoy = max(migdoy),
              cp.nsd = mean(cp.nsd)) %>% 
    ungroup() %>% 
    mutate(pt.nsd = ifelse(cp==1, cp.nsd, lead(cp.nsd))) %>% 
    dplyr::filter(cp %in% c(1,2))
  
  pts.i <- dat.i %>% 
    group_by(legid, cp) %>% 
    summarize(migdoy = max(migdoy),
              pt.nsd = mean(cp.nsd)) %>% 
    ungroup()
  
  ggplot(dat.i) +
    geom_line(aes(x=migdoy, y=nsd)) +
    geom_line(aes(x=migdoy, y=cp.nsd), colour="red") +
    geom_point(aes(x=migdoy, y=pt.nsd), size=3, colour="red", data=pts.i) +
    facet_wrap(~legid, scales="free") +
    ggtitle(ids[i])
  
#  ggsave(filename=paste0("Figures/cp/", ids[i], ".jpeg"))
  
}

dat.id <- dat.full %>% 
  dplyr::filter(legid=="46768108-2016-2fall")
View(dat.id)

#6. Determine departure & arrival dates----
noarrive <- c("99900-2021-2fall",
              "46768108-2016-2fall",
              "77638376-2015-2fall",
              "77639184-2016-2fall",
              "145698291-2021-1spring",
              "172070319-2016-2fall",
              "172070515-2019-2fall",
              "188150741-2021-1spring",
              "290352179-2021-1spring",
              "479364105-2019-1spring",
              "877974163-2020-1spring",
              "1418878943-2021-1spring",
              "1418896449-2021-1spring",
              "1418934379-2021-1spring")

id.date <- dat.cp %>% 
  group_by(legid, cp) %>% 
  summarize(date = max(date)) %>% 
  ungroup() %>% 
  pivot_wider(names_from="cp", values_from="date", names_prefix="cp") %>% 
  rename(depart = cp1, arrive=cp2) %>% 
  dplyr::select(-cp3, -cp4, -cp5)

dat.date <- dat.cp %>% 
  left_join(id.date) %>% 
  mutate(season1 = ifelse(date >= depart & date <= arrive, "migration", "stationary"),
         completemig = ifelse(legid %in% noarrive, 0, 1),
         season1 = ifelse(completemig==0 & date >= depart, "migration", season1))
table(dat.date$season1)
  
#Visualize
ids <- unique(dat.date$id)

for(i in 1:length(ids)){
  
  dat.i <- dat.date %>% 
    dplyr::filter(id==ids[i]) %>% 
    arrange(date)
  
  ggplot(dat.i) +
    geom_path(aes(x=lon, y=lat)) +
    geom_point(aes(x=lon, y=lat, colour=season1)) +
    facet_wrap(~legid) +
    ggtitle(ids[i])
  
#  ggsave(filename=paste0("Figures/Season/", ids[i], ".jpeg"))
  
}

#NOPE. This definitely didn't work----

#7. Use to get approx dates----
date.mean <- dat.date %>% 
  dplyr::select(legid, legseason, depart, arrive) %>% 
  unique() %>% 
  mutate(departdoy = yday(depart),
         arrivedoy = yday(arrive)) %>% 
  group_by(legseason) %>% 
  summarize(departmean = mean(departdoy),
            departmin = min(departdoy),
            departmax = max(departdoy),
            arrivemean = mean(arrivedoy),
            arrivemin = min(arrivedoy),
            arrivemax = max(arrivedoy)) %>% 
  ungroup()
date.mean
