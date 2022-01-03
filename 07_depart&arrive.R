library(tidyverse)
library(lubridate)

options(scipen = 99999)

#1. Read in data----
dat.raw <- read.csv("Data/LBCUFiltered&Predicted&Legged&Clustered&CPedData.csv") %>% 
  group_by(legid) %>% 
  mutate(nsdrecip = max(nsd) - nsd) %>% 
  ungroup()

#number of birds
length(unique(dat.raw$id))
#122

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

length(unique(dat$id))

#3. Remove points where there's a huge gap in transmission that spans arrival & departure----
dat.filter <- dat %>% 
  dplyr::filter(!(legid=="172070515-2017-1spring" & doy %in% c(1:28, 39:144)),
                !(legid=="172070515-2016-2fall" & doy > 202),
                !(legid=="94034-2009-2fall" & doy %in% c(172:249, 272:365)),
                !(id=="94033-2010-1spring"),
                !(id=="94033-2010-2fall" & doy < 275),
                !(legid=="94033-2009-2fall" & doy %in% c(171:249)),
                !(legid=="46768108-2015-1spring" & doy %in% c(71:93)),
                !(legid=="46769588-2019-1spring" & doy %in% c(74:93)),
                !(legid=="46769927-2014-1spring" & doy %in% c(100:144, 146:148)),
                !(legid=="46770723-2014-1spring" & doy %in% c(101:143)))

#4. Split out legs where bird didn't start migration----
#Also leg for where there's a huge transmission gap that spans arrival & departure
dat.nodep <- dat.filter %>% 
  group_by(legid) %>% 
  summarize(nsdmax = max(nsd),
            mindoy = min(doy)) %>% 
  ungroup() %>% 
  dplyr::filter(nsdmax < 38036899358.37) %>% 
  left_join(dat.filter) %>% 
  rbind(dat.filter %>% 
          dplyr::filter((legid=="172070515-2017-1spring" & doy > 144),
                        (legid=="46768108-2015-1spring" & doy < 71),
                        (legid=="46769588-2019-1spring" & doy < 74),
                        (legid=="46769927-2014-1spring" & doy < 100),
                        (legid=="46770723-2014-1spring" & doy < 101))) %>% 
  mutate(season = case_when(legseason=="1spring" & (mindoy < 35 | mindoy >= 220) ~ "winter",
                            legseason=="1spring" & mindoy >= 35 & mindoy < 220 ~ "breed",
                            legseason=="2fall" & mindoy < 220 & mindoy >= 35 ~ "breed",
                            legseason=="2fall" & (mindoy >= 220 | mindoy < 35) ~ "winter"),
         season= ifelse(legid=="172070515-2017-1spring" & doy > 144, "breed", season),
         season = ifelse(legid=="46768108-2015-1spring" & doy < 71, "winter", season),
         season = ifelse(legid=="46769588-2019-1spring" & doy < 74, "winter", season),
         season = ifelse(legid=="46769927-2014-1spring" & doy < 100, "winter", season),
         season = ifelse(legid=="46770723-2014-1spring" & doy < 101, "winter", season),
         segment = "stationary")

#Data for birds that did start migration to carry on to next step
dat.dep <- dat.filter %>% 
  anti_join(dat.nodep)

#5. Classify departure----
#departure = beginning of earliest state 2 cluster after nsd > 20000000000
dat.depart <- dat.dep %>% 
  dplyr::filter(predictedState==2,
                nsd > 20000000000) %>% 
  group_by(legid) %>% 
  arrange(date) %>% 
  filter(row_number()==1) %>% 
  ungroup() %>% 
  dplyr::select(legid, stateid) %>% 
  unique() %>% 
  left_join(dat.dep) %>% 
  group_by(legid, stateid) %>% 
  arrange(date) %>% 
  filter(row_number()==1) %>% 
  ungroup() %>% 
  mutate(segment = "depart") %>% 
  right_join(dat.dep) %>% 
  mutate(segment= case_when(legid=="164383320-2017-1spring" & doy==73 ~ "depart",
                            legid=="279282698-2019-1spring" & doy==74 ~ "depart",
                            legid=="172070515-2017-2fall" & doy==169 ~ "depart",
                            legid=="46769927-2014-2fall" & doy==181 ~ "depart",
                            legid=="145696972-2016-2fall" & doy==170 ~ "depart",
                            legid=="279818280-2018-1spring" & doy==99 ~ "depart",
                            !is.na(segment) ~ segment),
         segment = ifelse(legid=="279282698-2019-1spring" & doy %in% c(33,49), NA, segment),
         segment = ifelse(legid=="172070515-2017-2fall" & doy==135, NA, segment),
         segment = ifelse(legid=="46769927-2014-2fall" & doy==135, NA, segment),
         segment = ifelse(legid=="145696972-2016-2fall" & doy==136, NA, segment),
         segment = ifelse(legid=="279818280-2018-1spring" & doy< 99, NA, segment),
         segment = ifelse(legid=="94033-2009-2fall" & doy==295, NA, segment)) %>% 
  arrange(id, legid, date)

#these are individuals that the nsd rule doesn't work for because they were faffing around. I've done it manually instead, but still using the state==2 rule
#also includes removing departures for individuals for which there are large transmission gaps

#check if # of departures is same as # of legids. Should be one less
length(unique(dat.dep$legid))
table(dat.depart$segment)
#432

#check one per legid
depart.ids <- data.frame(table(dat.depart$legid, dat.depart$segment)) %>% 
  rename(legid = Var1, segment= Var2) %>% 
  dplyr::filter(segment=="depart",
                Freq > 1)
nrow(depart.ids)

#6. Filter out birds that didn't complete migration----
dat.arr <- dat.depart %>%
  dplyr::filter(!legid %in%  c("1418934379-2021-1spring",
                               "99900-2021-2fall",
                               "1146533212-2021-1spring",
                               "1418878943-2021-1spring",
                               "1418896449-2021-1spring",
                               "99900-2021-2fall",
                               "77639184-2016-2fall",
                               "479364105-2019-1spring",
                               "46768108-2016-2fall",
                               "188150741-2021-1spring",
                               "172070515-2019-2fall",
                               "1418896449-2021-1spring",
                               "1418878943-2021-1spring",
                               "172070515-2017-1spring",
                               "281981414-2018-1spring",
                               "290347351-2019-2fall",
                               "877974163-2020-1spring",
                               "877974163-2020-1spring",
                               "46768277-2014-2fall",
                               "94034-2009-2fall",
                               "94033-2009-2fall"))

length(unique(dat.arr$legid))
#417

#7. Classify arrival----
#arrival = first date of run of stationary points > 3 weeks after departure OR most first day of last run of stationary points

#Fill in migration segment
dat.migration <- dat.arr %>% 
  group_by(legid) %>% 
  mutate(segment2 = ifelse(lag(segment)=="depart", "migration", segment)) %>% 
  tidyr::fill(segment2, .direction="down") %>% 
  ungroup() %>% 
  mutate(segment = ifelse(is.na(segment2), segment, segment2)) %>% 
  dplyr::select(-segment2)

table(dat.migration$segment)
#417

#Classify arrival via stopover rule
dat.arrive.stopover <- dat.migration %>% 
  mutate(segment2 = ifelse(segment=="migration" & predictedState==1 & stateidn > 30, "arrive", segment)) %>% 
  dplyr::filter(segment2=="arrive") %>% 
  group_by(legid) %>% 
  arrange(date) %>% 
  filter(row_number()==1) %>% 
  ungroup() %>% 
  right_join(dat.migration) %>% 
  mutate(segment2= case_when(legid=="281980840-2020-2fall" & doy==180 ~ "arrive",
                            legid=="477994534-2018-2fall" & doy==190 ~ "arrive",
                            legid=="172070515-2016-2fall" & doy==193 ~ "arrive",
                            legid=="172070515-2017-2fall" & doy==176 ~ "arrive",
                            !is.na(segment2) ~ segment2),
         segment = ifelse(!is.na(segment2), segment2, segment)) %>% 
  arrange(id, date) %>% 
  dplyr::select(-segment2)

#Check number classified
table(dat.arrive.stopover$segment)

#Find missing ids
arrive.ids <- dat.arr %>% 
  dplyr::select(legid) %>% 
  unique() %>% 
  anti_join(dat.arrive.stopover %>% 
              dplyr::filter(segment=="arrive") %>% 
              dplyr::select(legid) %>% 
              unique())
nrow(arrive.ids)

#Classify arrival for remaining ids using last segment rule
dat.arrive.last <- dat.migration %>% 
  dplyr::filter(legid %in% arrive.ids$legid,
                predictedState==1) %>% 
  group_by(legid) %>% 
  dplyr::filter(stateid==max(stateid)) %>% 
  arrange(date) %>% 
  filter(row_number()==1) %>% 
  ungroup() %>% 
  mutate(segment2="arrive") %>% 
  right_join(dat.migration %>% 
     dplyr::filter(legid %in% arrive.ids$legid)) %>% 
  mutate(segment = ifelse(!is.na(segment2), segment2, segment)) %>% 
  arrange(id, date) %>% 
  dplyr::select(-segment2)

table(dat.arrive.last$segment)

#8. Put two arrival classifications back together----
dat.arrive <- dat.arrive.stopover %>% 
  dplyr::filter(!legid %in% arrive.ids$legid) %>% 
  rbind(dat.arrive.last)

table(dat.arrive$segment)
#417

#check one per legid
depart.ids <- data.frame(table(dat.arrive$legid, dat.arrive$segment)) %>% 
  rename(legid = Var1, segment= Var2) %>% 
  dplyr::filter(segment=="arrive",
                Freq > 1)
nrow(depart.ids)

#9. Put together & fill in the gaps----
dat.arrivedepart <- dat.depart %>% 
  dplyr::filter(segment=="depart") %>% 
  rbind(dat.arrive %>% 
          dplyr::filter(segment=="arrive")) %>% 
  right_join(dat.dep) %>% 
  arrange(legid, date) %>% 
  mutate(segment2 = case_when(lead(segment)=="depart" ~ "stationary",
                             lag(segment)=="depart" ~ "migration",
                             lead(segment)=="arrive" ~ "migration",
                             lag(segment)=="arrive" ~ "stationary",
                             is.na(segment) ~ segment)) %>% 
  mutate(segment = ifelse(is.na(segment2), segment, segment2)) %>% 
  dplyr::select(-segment2) %>% 
  group_by(legid) %>% 
  mutate(segment = ifelse(row_number()==1, "stationary", segment)) %>% 
  fill(segment, .direction="down") %>% 
  ungroup() %>% 
  mutate(segment = case_when(legid=="164383320-2017-1spring" & doy==73 ~ "depart",
                             legid=="164383320-2017-1spring" & doy==74 ~ "arrive",
                             legid=="46694077-2014-1spring" & doy==93 ~ "depart",
                             legid=="46694077-2014-1spring" & doy==102 ~ "arrive",
                             legid=="46769588-2019-1spring" & doy %in% c(73, 94) ~ "stationary",
                             !is.na(segment) ~ segment))

#tidying above is birds that depart and arrive on consecutive days or birds for which dates are unknown due to transmission gaps

table(dat.arrivedepart$segment)
#more birds removed without arrival or departure dates
#416, 431

#Check for one per legid
arrivedepart.ids <- data.frame(table(dat.arrivedepart$legid, dat.arrivedepart$segment)) %>% 
  rename(legid = Var1, segment= Var2) %>% 
  dplyr::filter(segment=="depart",
                Freq > 1)
nrow(arrivedepart.ids)

#10. Classify to season---
dat.season <- dat.arrivedepart %>% 
  group_by(id) %>% 
  mutate(season=case_when(segment=="migration" & legseason=="1spring" ~ "springmig",
                          segment=="migration" & legseason=="2fall" ~ "fallmig",
                          segment=="stationary" & legseason=="1spring" & row_number()==1 ~ "winter",
                          segment=="stationary" & legseason=="1spring" & lag(segment)=="arrive" ~ "breed",
                          segment=="stationary" & legseason=="2fall" & row_number()==1 ~ "breed",
                          segment=="stationary" & legseason=="2fall" & lag(segment)=="arrive" ~ "winter",
                          segment=="arrive" & legseason=="1spring" ~ "breed",
                          segment=="arrive" & legseason=="2fall" ~ "winter",
                          segment=="depart" & legseason=="1spring" ~ "springmig",
                          segment=="depart" & legseason=="2fall" ~ "fallmig")) %>% 
  fill(season, .direction="down") %>% 
  dplyr::select(id, legid, lat, lon, X, Y, date, doy, predictedState, stateid, stateidn, probState2, nsd, nsdrecip, dist, cp, segment, legseason, season) %>% 
  ungroup()
  
table(dat.season$segment)
#416, 431

#Check seasons & segments line up
table(dat.season$season, dat.season$segment)

#Check for nas
dat.na <- dat.season %>% 
  dplyr::filter(is.na(season)) %>% 
  dplyr::select(id) %>% 
  unique() %>% 
  left_join(dat.season)
nrow(dat.na)
######

#10. Add birds that never left back in----
dat.all <- dat.nodep %>% 
  mutate(segment = NA) %>% 
  dplyr::select(id, legid, lat, lon, X, Y, date, doy, predictedState, stateid, stateidn, probState2, nsd, nsdrecip, dist, cp, segment, legseason, season) %>% 
  rbind(dat.season)

#11. Tidy----
dat.tidy <- dat.all %>% 
  mutate(segment = case_when(legid=="279818280-2018-1spring" & doy==89 ~ "depart",
                             legid=="890858132-2019-2fall" & doy == 205 ~ "arrive",
                             legid=="71767680-2016-2fall" & doy==169 ~ "arrive",
                             legid=="145696972-2016-2fall" & doy==155 ~ "depart",
                             !is.na(segment) ~ segment)) %>% 
  mutate(season = case_when(legid=="279818280-2017-2fall" & doy > 191 ~ "winter",
                            legid=="279818280-2018-1spring" & doy < 89 ~ "winter",
                            legid=="172070515-2016-2fall" & doy >= 193 ~ "winter",
                            legid=="172070515-2016-2fall" & doy <= 32 ~ "winter",
                            legid=="890858132-2019-2fall" & doy >= 205 ~ "winter",
                            legid=="71767680-2016-2fall" & doy >= 169 ~ "winter",
                            legid=="145696972-2016-2fall" & doy < 155 ~ "breed",
                            legid=="94034-2009-2fall" & doy >= 250 ~ "winter",
                            legid=="94033-2009-2fall" & (doy >= 250 | doy <= 32) ~ "winter",
                            legid=="281981414-2018-1spring" & doy== 93 ~ "winter",
                            legid=="46768108-2015-1spring" & doy %in% c(69:70) ~ "winter",
                            legid=="46769588-2019-1spring" & doy <= 73 ~ "winter",
                            legid=="46769588-2019-1spring" & doy >= 94 ~ "breed",
                            legid=="46769927-2014-1spring" & doy >= 149 ~ "breed",
                            legid=="46769927-2014-1spring" & doy <= 99 ~ "winter",
#FIX                            legid=="46770723-2014-1spring" & doy < 119 ~ "winter",
                            !is.na(season) ~ season)) %>% 
  rename(state = segment) %>% 
  mutate(state = case_when(season=="winter" & state!="arrive" ~ "stationary",
                           season=="breed" & state!="arrive" ~ "stationary",
                           season=="winter" & is.na(state) ~ "stationary",
                           season=="breed" & is.na(state) ~ "stationary",
                           season=="fallmig" & state !="depart" ~ "migration",
                           season=="springmig" & state !="depart" ~ "migration",
                           !is.na(state) ~ state))

table(dat.tidy$state)
#416, 429
#2 less for each

#check season vs state
table(dat.tidy$season, dat.tidy$state)

#12. Visualize----
ids <- unique(dat.tidy$id)

for(i in 1:length(ids)){
  
  dat.i <- dat.tidy %>% 
    dplyr::filter(id==ids[i])
  
  ggplot(dat.i) +
    geom_path(aes(x=X, y=Y)) +
    geom_point(aes(x=X, y=Y, colour = season), size=3, alpha = 0.7) +
    scale_colour_viridis_d() +
    facet_wrap(~legid)
  
#  ggsave(filename=paste0("Figures/Season/", ids[i], ".jpeg"))
  
  ggplot(dat.i) +
    geom_point(aes(x=doy, y=factor(year(date)), colour=state)) +
    facet_wrap(~season)
  
#  ggsave(filename=paste0("Figures/Dotplot/", ids[i], ".jpeg"))
  
}

#13. Summary stats of dates----
dates <- dat.tidy %>% 
#  dplyr::filter(state %in% c("depart", "arrive")) %>% 
  group_by(season, state) %>% 
  summarize(mean = mean(doy),
            max = max(doy),
            min = min(doy),
            n = n()) %>% 
  ungroup()
dates

ggplot(dat.tidy) +
  geom_point(aes(x=X, y=Y, colour=season))

#14. Write out----
write.csv(dat.tidy, "Data/LBCUSegmentedData.csv", row.names = FALSE)
write.csv(dat.tidy, "/Users/ellyknight/Dropbox/LBCU/LBCUSegmentedData.csv", row.names = FALSE)

#Troubleshooting
dat.original <- read.csv("Data/LBCUCleanedData.csv")

dat.og.id <- dat.original %>% 
  dplyr::filter(id==94033) %>% 
  dplyr::select(lat, long, R2n, datetime, doy)

ggplot(dat.tidy %>% 
         dplyr::filter(state=="stationary")) +
  geom_histogram(aes(x=doy)) +
  facet_grid(season~state, scales="free")

dat.id <- dat.tidy %>% 
  dplyr::filter(season=="winter", doy==150) %>% 
  dplyr::select(id) %>% 
  unique() %>% 
  sample_n(1) %>% 
  left_join(dat.tidy) %>% 
  dplyr::select(legid, lat, lon, nsd, date, doy, state, season)

dat.id <- dat.tidy %>% 
  dplyr::filter(id==94033) %>% 
  dplyr::select(legid, lat, lon, nsd, date, doy)
