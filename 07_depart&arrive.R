library(tidyverse)
library(lubridate)

options(scipen = 99999)

#TO DO: DEAL WITH DEPARTURE & ARRIVAL OUTLIERS
#TO DO: LOOK AT STOPOVER LENGTH MAX
#TO DO: GO BACK AND DEAL WITH INCOMPLETE LEGS####
#TO DO: CHECK # OF BIRDS####
#TO DO: DECIDE WHAT TO DO WITH LONG STRETCHES WITH NO TRANSMISSIONS - worried about ones that overlap with departure or arrival

#1. Read in data----
dat.raw <- read.csv("Data/LBCUFiltered&Predicted&Legged&Clustered&CPedData.csv") %>% 
  group_by(legid) %>% 
  mutate(nsdrecip = max(nsd) - nsd) %>% 
  ungroup()

#2. Add ID for each cluster of HMM states----
legs <- unique(dat.raw$legid)

dat <- data.frame()
for(i in 1:length(legs)){
  dat.i <- dat.raw %>% 
    dplyr::filter(legid==legs[i])
  
  dat.rle <- data.frame(length = rle(dat.i$predictedState)$lengths)
  dat.rle$id <- seq(1, nrow(dat.rle), 1)
  dat.stateid <- uncount(dat.rle, length) %>% 
    left_join(dat.rle)
  
  dat.i$stateid <- dat.stateid$id
  dat.i$stateidn <- dat.stateid$length
  
  dat <- rbind(dat, dat.i)
}

#3. Look at one individual----
dat.i <- dat %>% 
  dplyr::select(legid) %>% 
  unique() %>% 
  sample_n(1) %>% 
  left_join(dat) 

ggplot(dat.i) +
  geom_path(aes(x=X, y=Y)) +
  geom_point(aes(x=X, y=Y, colour=nsd), size=3, alpha = 0.7) +
  scale_colour_viridis_c() +
  facet_grid(predictedState ~ cp)

#3. Classify departure----
#departure = beginning of earliest state 2 cluster after nsd > 20000000000
dat.depart <- dat %>% 
  dplyr::filter(predictedState==2,
                nsd > 20000000000) %>% 
  group_by(legid) %>% 
  arrange(date) %>% 
  filter(row_number()==1) %>% 
  ungroup() %>% 
  dplyr::select(legid, stateid) %>% 
  unique() %>% 
  left_join(dat) %>% 
  group_by(legid, stateid) %>% 
  arrange(date) %>% 
  filter(row_number()==1) %>% 
  ungroup() %>% 
  mutate(segment = "depart") %>% 
  right_join(dat) %>% 
  mutate(segment= case_when(legid=="164383320-2017-1spring" & doy==73 ~ "depart",
                            legid=="279282698-2019-1spring" & doy==74 ~ "depart",
                            legid=="46769840-2015-1spring" & doy==75 ~ "depart",
                            !is.na(segment) ~ segment),
         segment = ifelse(legid=="279282698-2019-1spring" & doy==33, NA, segment)) %>% 
  arrange(id, legid, date)

#check if # of departures is same as # of legids
length(unique(dat$legid))
table(dat.depart$segment)

#check one per legid
depart.ids <- data.frame(table(dat.depart$legid, dat.depart$segment)) %>% 
  rename(legid = Var1, segment= Var2) %>% 
  dplyr::filter(segment=="depart",
                Freq > 1)
nrow(depart.ids)

dat.id <- dat.depart %>% 
  dplyr::filter(legid %in% depart.ids$legid) %>% 
  dplyr::select(id, legid, X, Y, date, doy, predictedState, stateid, stateidn, probState2, nsd, nsdrecip, dist, cp)

#4. Classify arrival----
#arrival = first run after departure of stationary points > 10 days in length
dat.migration <- dat.depart %>% 
  group_by(legid) %>% 
  mutate(segment2 = ifelse(lag(segment)=="depart", "migration", segment)) %>% 
  tidyr::fill(segment2, .direction="down") %>% 
  ungroup() %>% 
  mutate(segment = ifelse(is.na(segment2), segment, segment2)) %>% 
  dplyr::select(-segment2)

table(dat.migration$segment)

dat.arrive <- dat.migration %>% 
  mutate(segment = ifelse(segment=="migration" & predictedState==1 & stateidn > 7, "arrive", segment)) %>% 
  dplyr::filter(segment=="arrive") %>% 
  group_by(legid) %>% 
  arrange(date) %>% 
  filter(row_number()==1) %>% 
  ungroup() %>% 
  right_join(dat) %>% 
  mutate(segment= case_when(legid=="281980840-2020-2fall" & doy==180 ~ "arrive",
                            legid=="477994534-2018-2fall" & doy==190 ~ "arrive",
                            !is.na(segment) ~ segment))

table(dat.arrive$segment)

#Find missing ids
arrive.ids <- dat.depart %>% 
  dplyr::select(legid) %>% 
  unique() %>% 
  anti_join(dat.arrive %>% 
              dplyr::filter(segment=="arrive") %>% 
              dplyr::select(legid) %>% 
              unique())
nrow(arrive.ids)

dat.id <- dat.depart %>% 
  dplyr::filter(legid %in% arrive.ids$legid) %>% 
  dplyr::select(id, legid, X, Y, date, doy, predictedState, stateid, stateidn, probState2, nsd, nsdrecip, dist, cp, segment)

#check one per legid
depart.ids <- data.frame(table(dat.arrive$legid, dat.arrive$segment)) %>% 
  rename(legid = Var1, segment= Var2) %>% 
  dplyr::filter(segment=="arrive",
                Freq > 1)
nrow(depart.ids)

#5. Fill in the gaps----
dat.arrivedepart <- dat.depart %>% 
  dplyr::filter(segment=="depart") %>% 
  rbind(dat.arrive %>% 
          dplyr::filter(segment=="arrive")) %>% 
  right_join(dat) %>% 
  arrange(legid, date) %>% 
  mutate(segment2 = case_when(row_number()==1 ~ "stationary",
                             lead(segment)=="depart" ~ "stationary",
                             lag(segment)=="depart" ~ "migration",
                             lead(segment)=="arrive" ~ "migration",
                             lag(segment)=="arrive" ~ "stationary",
                             is.na(segment) ~ segment)) %>% 
  mutate(segment = ifelse(is.na(segment2), segment, segment2)) %>% 
  dplyr::select(-segment2) %>% 
  group_by(legid) %>% 
  fill(segment) %>% 
  ungroup() %>% 
  mutate(segment = case_when(legid=="164383320-2017-1spring" & doy==73 ~ "depart",
                             legid=="164383320-2017-1spring" & doy==74 ~ "arrive",
                             legid=="46769840-2015-1spring" & doy==75 ~ "depart",
                             legid=="46769840-2015-1spring" & doy==76 ~ "arrive",
                             !is.na(segment) ~ segment))

table(dat.arrivedepart$segment)

arrivedepart.ids <- dat.arrive %>% 
  dplyr::select(legid) %>% 
  unique() %>% 
  anti_join(dat.arrivedepart %>% 
              dplyr::filter(segment=="arrive") %>% 
              dplyr::select(legid) %>% 
              unique())
nrow(arrivedepart.ids)

dat.id <- dat.arrivedepart %>% 
  arrange(legid, date) %>% 
  dplyr::filter(legid %in% arrivedepart.ids$legid) %>% 
  dplyr::select(id, legid, X, Y, date, doy, predictedState, stateid, stateidn, probState2, nsd, nsdrecip, dist, cp, segment)

#6. Classify to season---
dat.season <- dat.arrivedepart %>% 
  mutate(season=case_when(segment=="migration" & legseason=="1spring" ~ "springmig",
                          segment=="migration" & legseason=="2fall" ~ "fallmig",
                          !is.na(segment) ~ segment)) %>% 
  mutate(season = case_when(lead(season)=="depart" & legseason=="1spring" ~ "winter",
                             lag(season)=="arrive" & legseason=="1spring" ~ "breed",
                             lead(season)=="depart" & legseason=="2fall" ~ "breed",
                             lag(season)=="arrive" & legseason=="2fall" ~ "winter",
                             !is.na(season) ~ season)) %>% 
  mutate(season = ifelse(season=="stationary", NA, season)) %>% 
  group_by(id) %>% 
  fill(season, .direction="updown") %>% 
  dplyr::select(id, legid, X, Y, date, doy, predictedState, stateid, stateidn, probState2, nsd, nsdrecip, dist, cp, segment, legseason, season)
  
table(dat.season$season)

#Check for nas
dat.na <- dat.season %>% 
  dplyr::filter(is.na(season)) %>% 
  dplyr::select(id) %>% 
  unique() %>% 
  left_join(dat.season)
nrow(dat.na)

#7. Visualize----
ids <- unique(dat.season$id)

for(i in 1:length(ids)){
  
  dat.i <- dat.season %>% 
    dplyr::filter(id==ids[i])
  
  ggplot(dat.i) +
    geom_path(aes(x=X, y=Y)) +
    geom_point(aes(x=X, y=Y, colour = season), size=3, alpha = 0.7) +
    scale_colour_viridis_d() +
    facet_wrap(~legid)
  
  ggsave(filename=paste0("Figures/Season/", ids[i], ".jpeg"))
  
}

#8. Summary stats of dates----
dates <- dat.season %>% 
  dplyr::filter(season %in% c("depart", "arrive")) %>% 
  group_by(legseason, season) %>% 
  summarize(mean = mean(doy),
            max = max(doy),
            min = min(doy)) %>% 
  ungroup()
dates

ggplot(dat.season %>% dplyr::filter(season %in% c("depart", "arrive"))) +
  geom_histogram(aes(x=doy)) +
  facet_grid(season~legseason)
#definitely still some outliers

ggplot(dat.season) +
  geom_point(aes(x=X, y=Y, colour=season))
