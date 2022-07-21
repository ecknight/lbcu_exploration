library(tidyverse)
library(lubridate)
library(rgeoda)

options(scipen = 99999)

#1. Read in data----
#Align nsd and dist columns
dat.raw <- read.csv("Data/LBCUFiltered&Predicted&LeggedData.csv") %>% 
  unique()

#check number of birds
length(unique(dat.raw$id))
#158

#2. Add ID for each cluster of HMM states----
legs <- unique(dat.raw$legid)

dat <- data.frame()
for(i in 1:length(legs)){
  dat.i <- dat.raw %>% 
    dplyr::filter(legid==legs[i])
  
  dat.rle <- data.frame(length = rle(dat.i$hmmstate)$lengths)
  dat.rle$id <- seq(1, nrow(dat.rle), 1)
  dat.stateid <- uncount(dat.rle, length) %>% 
    left_join(dat.rle)
  
  dat.i$stateid <- dat.stateid$id
  dat.i$staten <- dat.stateid$length
  
  dat <- rbind(dat, dat.i)
}

length(unique(dat$id))

#3. Visualize each bird----
# ids <- sort(unique(dat$id))
# 
# for(i in 1:length(ids)){
#   
#   dat.i <- dat %>% 
#     dplyr::filter(id==ids[i])
#   
#   ggplot(dat.i) +
#     geom_path(aes(x=X, y=Y)) +
#     geom_point(aes(x=X, y=Y, colour = factor(hmmstate)), size=3, alpha = 0.7, legend=FALSE) +
#     scale_colour_viridis_d() +
#     facet_wrap(~legid)
#   
#   ggsave(filename=paste0("Figures/HMM/", ids[i], ".jpeg"))
# }

#4. Remove transmission gaps----
gaps <- read.csv("Data/TransmissionGaps.csv") %>% 
  dplyr::select(id, year, startdoy, enddoy, dtdays)

#33088 - none missed
#94033 - 2009 fall mig depart & arrival missed
#94034 - 2009 fall mig depart & arrival missed
#46768108 - 2014 - none missed
#46768108 - 2015 spring mig depart missed
#46769588 - 2019 spring mig depart & arrival missed
#46769927 - 2014 spring mig depart & arrival missed
#46770723 - 2014 spring mig depart & arrival missed
#145696972 - none missed
#172070515 - 2017 spring mig depart & arrival missed
#279282698 - none missed
#973657763 - none missed
#1378421378 - none missed
#1378421379 - none missed
#1378421380 - none missed
#1378421381 - none missed
#1953212677 - none missed

#Remove & classify legs where departure & arrival missed

#By hand notes:
#2 gaps taken out because there are multiple for that id/year

dat.gap <- dat %>%
  dplyr::filter(legid %in% c("94033-2009-2fall",
                              "94034-2009-2fall",
                              "46768108-2015-1spring",
                              "46769588-2019-1spring",
                              "46769927-2014-1spring",
                              "46770723-2014-1spring",
                              "172070515-2017-1spring")) %>% 
  unique() %>% 
  left_join(gaps %>% 
              dplyr::filter(!(id=="46769588" & year==2019 & startdoy==201),
                            !(id %in% c("46770723", "46769927") & year==2014 & startdoy==120))) %>% 
  mutate(season = case_when(legseason=="2fall" & doy <= startdoy ~ "breed",
                            legseason=="2fall" & doy >= enddoy ~ "winter",
                            legseason=="1spring" & doy <= startdoy ~ "winter",
                            legseason=="1spring" & doy >= enddoy ~ "breed"),
         segment="stationary") %>% 
  dplyr::select(-startdoy, -enddoy, -dtdays)

#6. Select NSD value to indicate migration----
dat.nogap <- dat %>% 
  anti_join(dat.gap) %>% 
  mutate(lognsd = log(nsd+0.00001))

logbreak <- natural_breaks(k=2, dat.nogap['lognsd'])

ggplot(dat.nogap) +
  geom_histogram(aes(x=lognsd), bins=100) +
  geom_vline(aes(xintercept=logbreak))

sqrt(exp(logbreak))

#5. Remove legs where bird didn't start migration----

#By hand notes:
#86872 never migrated at all, so season is always winter

#migration legs that don't get caught by filter but where bird didn't initiate migration (these were added after visualization in Step 5)
leg.nodep <- c("1953212676-2012-1spring", "33089-2009-2fall", "33088-2010-2fall", "290352179-2021-1spring")

dat.nodep <- dat %>% 
  anti_join(dat.gap) %>% 
  group_by(legid) %>% 
  summarize(nsdmax = max(nsd),
            mindoy = min(doy)) %>% 
  ungroup() %>% 
  dplyr::filter(nsdmax < exp(logbreak)) %>% 
  left_join(dat) %>% 
  mutate(season = case_when(legseason=="1spring" & (mindoy < 35 | mindoy >= 220) ~ "winter",
                            legseason=="1spring" & mindoy >= 35 & mindoy < 220 ~ "breed",
                            legseason=="2fall" & mindoy < 220 & mindoy >= 35 ~ "breed",
                            legseason=="2fall" & (mindoy >= 220 | mindoy < 35) ~ "winter",
                            id==86872 ~ "winter"),
         segment = "stationary") %>% 
  dplyr::select(-nsdmax, -mindoy) %>% 
  rbind(dat %>%
          dplyr::filter(legid %in% leg.nodep) %>% 
          mutate(season="winter",
                 segment = "stationary"))

#Data for birds that did start migration to carry on to next step
dat.dep <- dat %>% 
  anti_join(dat.nodep) %>% 
  anti_join(dat.gap)

#5. Classify departure----

#5a. Classify based on nsd rules----
dat.depart <- dat.dep %>% 
  dplyr::filter((nsd > exp(logbreak) & 
                nsd > lag(nsd) & 
                nsd < lead(nsd)) |
                  (lag(dist) > sqrt(exp(logbreak)) & 
                     nsd > lag(nsd) & 
                     nsd < lead(nsd))) %>% 
  group_by(legid) %>% 
  arrange(date) %>% 
  filter(row_number()==1) %>% 
  mutate(segment = "depart") %>% 
  ungroup() %>% 
  right_join(dat.dep) %>% 
  group_by(legid) %>% 
  arrange(date) %>% 
  mutate(segment = case_when(lag(segment)=="depart" ~ "migration",
                             lead(segment)=="depart" ~ "stationary",
                             !is.na(segment) ~ segment)) %>% 
  tidyr::fill(segment, .direction="updown") %>% 
  ungroup()

#5b. Fix some manually----

#By hand notes
#77639184-2016-1spring: went back to wintering grounds
#164384078-2017-1spring: short movement towards winter grounds caused lag(nsd) < nsd
#46769927-2014-2fall: first two points are prebreeding movement
#71767680-2018-2fall: short movement towards breeding grounds caused lag(nsd) < nsd
#164384078-2017-1spring: short movement towards winter grounds caused lag(nsd) < nsd
#172070319-2016-2fall: just under threshold, but obviously migration
#281980840-2018-2fall: short movement towards breeding grounds caused lag(nsd) < nsd
#281980840-2019-2fall: short movement towards breeding grounds caused lag(nsd) < nsd
#281981414-2018-1spring: not sure why this one wasn't caught
#290347351-2019-1spring: short movement towards winter grounds caused lag(nsd) < nsd
#290347351-2019-2fall: short movement towards breeding grounds caused lag(nsd) < nsd
#290350486-2018-2fall: went back to breeding grounds
#290351953-2018-1spring: went back to wintering grounds
#290352179-2019-1spring: short movement towards winter grounds caused lag(nsd) < nsd
#477991738-2021-1spring: short movement towards winter grounds caused lag(nsd) < nsd
#877974163-2019-2fall: short movement towards breeding grounds caused lag(nsd) < nsd
#890830029-2020-2fall: short movement towards breeding grounds caused lag(nsd) < nsd
#1425590049-2019-2fall: went back to breeding grounds
#1546914171-2021-2fall: short movement towards breeding grounds caused lag(nsd) < nsd
#1953212665-2011-2fall: short movement towards breeding grounds caused lag(nsd) < nsd
#1953212669-2009-1spring: short movement towards winter grounds caused lag(nsd) < nsd
#1953212672-2009-1spring: went back to wintering grounds
#1953212677-2012-1spring: short movement towards winter grounds caused lag(nsd) < nsd
#1953212677-2015-2fall:  short movement towards breeding grounds caused lag(nsd) < nsd
#1953212682-2010-1spring: short movement towards winter grounds caused lag(nsd) < nsd
#1953212683-2016-2fall: went back to breeding grounds
#164383320-2017-2fall: just under threshold, but obviously migration
#164383320-2018-1spring: just under threshold, but obviously migration
#279818280-2018-1spring: even though lognsd > logbreak, just big movements on contiguous wintering grounds due to multiple wintering home ranges
#281981679-2017-2fall: short movement towards breeding grounds caused lag(nsd) < nsd
#1953212686-2012-1spring:

dat.depart.fix <- dat.depart %>% 
  dplyr::filter((legid=="71767680-2018-2fall" & doy==164) |
                (legid=="164384078-2017-1spring" & doy==76) |
                (legid=="46769927-2014-2fall" & doy==181) |
                  (legid=="164384078-2017-1spring" & doy==76) |
                  (legid=="172070319-2016-2fall" & doy==195) |
                  (legid=="281980840-2018-2fall" & doy==163) |
                  (legid=="281980840-2019-2fall" & doy==175) |
                  (legid=="281981414-2018-1spring" & doy==75) |
                  (legid=="290347351-2019-1spring" & doy==87) |
                  (legid=="290347351-2019-2fall" & doy==180) |
                  (legid=="290350486-2018-2fall" & doy==175) |
                  (legid=="290351953-2018-1spring" & doy==97) |
                  (legid=="290352179-2019-1spring" & doy==86) |
                  (legid=="477991738-2021-1spring" & doy==93) |
                  (legid=="877974163-2019-2fall" & doy==183) |
                  (legid=="890830029-2020-2fall" & doy==183) |
                  (legid=="1425590049-2019-2fall" & doy==186) |
                  (legid=="1546914171-2021-2fall" & doy==158) |
                  (legid=="1953212665-2011-2fall" & doy==180) |
                  (legid=="1953212669-2009-1spring" & doy==98) |
                  (legid=="1953212672-2009-1spring" & doy==96) |
                  (legid=="1953212677-2012-1spring" & doy==70) |
                  (legid=="1953212677-2015-2fall" & doy==166) |
                  (legid=="1953212682-2010-1spring" & doy==68) |
                  (legid=="1953212683-2016-2fall" & doy==171) |
                  (legid=="77639184-2016-1spring" & doy==96) |
                  (legid=="164383320-2017-2fall" & doy==174) |
                  (legid=="164383320-2018-1spring" & doy==78) |
                  (legid=="279818280-2018-1spring" & doy==88) |
                  (legid=="281981679-2017-2fall" & doy==172) |
                  (legid=="1953212686-2012-1spring" & doy==53)) %>% 
  mutate(segment="depart")

#5c. Add new manually fixed departures to dataframe----
dat.depart2 <- dat.depart %>% 
  dplyr::filter(!legid %in% unique(dat.depart.fix$legid),
                segment=="depart") %>% 
  rbind(dat.depart.fix) %>% 
  right_join(dat.dep) %>% 
  group_by(legid) %>% 
  arrange(date) %>% 
  mutate(segment = case_when(lag(segment)=="depart" ~ "migration",
                             lead(segment)=="depart" ~ "stationary",
                             !is.na(segment) ~ segment)) %>% 
  tidyr::fill(segment, .direction="updown") %>% 
  ungroup()

#5d. Visualize----
# ids <- sort(unique(dat.depart2$legid))
# 
# for(i in 1:length(ids)){
#   
#   dat.i <- dat.depart2 %>%
#     dplyr::filter(legid==ids[i])
#   
#   ggplot(dat.i) +
#     geom_path(aes(x=X, y=Y)) +
#     geom_point(aes(x=X, y=Y, colour = segment), size=3, alpha = 0.7, legend=FALSE) +
#     scale_colour_viridis_d()
#   
#   ggsave(filename=paste0("Figures/Departure/", ids[i], ".jpeg"))
# }

#check if # of departures is same as # of legids
length(unique(dat.depart2$legid))
table(dat.depart2$segment)
#595

#6. Filter out migration segments that weren't completed----
dat.arr <- dat.depart2 %>%
  dplyr::filter(!legid %in%c("1418934379-2021-1spring",
                               "99900-2021-2fall",
                               "1146533212-2021-1spring",
                               "77639184-2016-2fall",
                               "479364105-2019-1spring",
                               "46768108-2016-2fall",
                               "188150741-2021-1spring",
                               "172070515-2019-2fall",
                               "1418896449-2021-1spring",
                               "1418878943-2021-1spring",
                               "172070319-2016-2fall",
                               "281981414-2018-1spring",
                               "290347351-2019-2fall",
                               "877974163-2020-1spring",
                               "46768277-2014-2fall",
                               "145698291-2021-1spring",
                               "77638376-2015-2fall",
                              "99902-2021-2fall",
                             "890834800-2020-1spring",
                             "1953212690-2011-1spring"))

length(unique(dat.arr$legid))
#595-20=575

#7. Classify arrival----

#7a. First date of first run after departure of at least 30 stationary points
dat.arrive.stopover <- dat.arr %>% 
  mutate(segment2 = ifelse(segment=="migration" & hmmstate==1 & staten > 30, "arrive", segment)) %>% 
  dplyr::filter(segment2=="arrive") %>% 
  group_by(legid) %>% 
  arrange(date) %>% 
  filter(row_number()==1) %>% 
  ungroup() %>% 
  right_join(dat.arr) %>% 
  mutate(segment = ifelse(!is.na(segment2), segment2, segment)) %>% 
  dplyr::select(-segment2)

#Check number classified
table(dat.arrive.stopover$segment)
#443 - 132 missing

#7b. First day of last run of stationary points----

#Find missing ids
arrive.ids <- dat.arr %>% 
  dplyr::select(legid) %>% 
  unique() %>% 
  anti_join(dat.arrive.stopover %>% 
              dplyr::filter(segment=="arrive") %>% 
              dplyr::select(legid) %>% 
              unique())
nrow(arrive.ids)
#132

dat.arrive.last <- dat.depart2 %>% 
  dplyr::filter(legid %in% arrive.ids$legid,
                hmmstate==1) %>% 
  group_by(legid) %>% 
  dplyr::filter(stateid==max(stateid)) %>% 
  arrange(date) %>% 
  filter(row_number()==1) %>% 
  ungroup() %>% 
  mutate(segment2="arrive") %>% 
  right_join(dat.depart2 %>% 
               dplyr::filter(legid %in% arrive.ids$legid)) %>% 
  mutate(segment = ifelse(!is.na(segment2), segment2, segment)) %>% 
  arrange(id, date) %>% 
  dplyr::select(-segment2)

table(dat.arrive.last$segment)
#132

#7c. Put two arrival classifications back together----
dat.arrive <- dat.arrive.stopover %>% 
  dplyr::filter(!legid %in% arrive.ids$legid) %>% 
  rbind(dat.arrive.last) %>% 
  arrange(id, date) %>% 
  group_by(legid) %>% 
  mutate(segment2 = ifelse(lag(segment)=="arrive", "stationary", NA)) %>% 
  tidyr::fill(segment2, .direction="down") %>% 
  mutate(segment2 = ifelse(is.na(segment2), "NA", segment2)) %>% 
  ungroup() %>% 
  mutate(segment=ifelse(segment2=="stationary", "stationary", segment)) %>% 
  dplyr::select(-segment2)
table(dat.arrive$segment)
#575

#7d. Fix some manually----

#By hand notes
#71767453-2015-2fall: period of big movement after arrival made staten < 30
#71767680-2018-2fall: period of big movement after arrival made staten < 30
#77637122-2017-2fall: period of big movement after arrival made staten < 30
#145696972-2016-1spring: did a weird commute 100 km east a couple times during breeding season?
#164383320-2017-2fall: period of big movement after arrival made staten < 30
#279282698-2019-2fall: period of big movement after arrival made staten < 30
#281980840-2019-2fall: period of big movement after arrival made staten < 30
#281981910-2018-2fall: period of big movement after arrival made staten < 30
#290350486-2019-2fall: period of big movement after arrival made staten < 30
#290350903-2019-2fall: period of big movement after arrival made staten < 30
#290351183-2020-2fall: period of big movement after arrival made staten < 30
#290351953-2019-1spring: 31 day stopover on way to breeding grounds (just over 30 day threshold)
#290352179-2019-1spring: period of big movement after arrival made staten < 30
#484271759-2018-2fall: period of big movement after arrival made staten < 30
#877984360-2020-2fall: period of big movement after arrival made staten < 30
#890854392-2021-2fall: period of big movement after arrival made staten < 30
#890856908-2021-2fall: period of big movement after arrival made staten < 30
#890858132-2019-2fall: period of big movement after arrival made staten < 30
#1425582803-2019-2fall: period of big movement after arrival made staten < 30
#1425590049-2019-2fall: period of big movement after arrival made staten < 30
#1546941291-2021-2fall: manual classification of departure messed up the staten count because hmm was stationary during departure
#1953212669-2007-2fall: period of big movement after arrival made staten < 30
#1953212677-2008-2fall: period of big movement after arrival made staten < 30
#1953212677-2009-2fall: period of big movement after arrival made staten < 30
#1953212677-2011-2fall: period of big movement after arrival made staten < 30
#1953212677-2012-2fall: period of big movement after arrival made staten < 30
#1953212677-2013-2fall: period of big movement after arrival made staten < 30
#1953212677-2014-2fall: period of big movement after arrival made staten < 30
#1953212677-2015-2fall: period of big movement after arrival made staten < 30
#1953212677-2016-2fall: period of big movement after arrival made staten < 30
#1953212677-2017-2fall: period of big movement after arrival made staten < 30
#1953212677-2018-2fall: period of big movement after arrival made staten < 30
#1953212677-2019-2fall: period of big movement after arrival made staten < 30
#1953212677-2020-2fall: period of big movement after arrival made staten < 30 (area contiguous when compared to previous years)
#1953212681-2010-2fall: period of big movement after arrival made staten < 30
#1953212681-2012-1spring: period of big movement after arrival made staten < 30
#1953212681-2013-1spring: period of big movement after arrival made staten < 30
#1953212681-2017-1spring: period of big movement after arrival made staten < 30
#1953212681-2019-1spring: period of big movement after arrival made staten < 30
#1953212685-2011-2fall: period of big movement after arrival made staten < 30
#1953212686-2012-1spring: manual classification of departure messed up the staten count because hmm was stationary during departure

dat.arrive.fix <- dat.arrive %>% 
  dplyr::filter((legid=="71767680-2015-2fall" & doy==175)|
                  (legid=="71767680-2018-2fall" & doy==181)|
                  (legid=="77637122-2017-2fall" & doy==189)|
                  (legid=="145696972-2016-1spring" & doy==102) |
                  (legid=="164383320-2017-2fall" & doy==175) |
                  (legid=="279282698-2019-2fall" & doy==165) |
                  (legid=="281980840-2019-2fall" & doy==183) |
                  (legid=="281981910-2018-2fall" & doy==172) |
                  (legid=="290350486-2019-2fall" & doy==176) |
                  (legid=="290350903-2019-2fall" & doy==221) |
                  (legid=="290351183-2020-2fall" & doy==199) |
                  (legid=="290351953-2019-1spring" & doy==110) |
                  (legid=="290352179-2019-1spring" & doy==107)|
                  (legid=="484271759-2018-2fall" & doy==193) |
                  (legid=="877984360-2020-2fall" & doy==185) |
                  (legid=="890854392-2021-2fall" & doy==179) |
                  (legid=="890856908-2021-2fall" & doy==188) |
                  (legid=="890858132-2019-2fall" & doy==199) |
                  (legid=="1425582803-2019-2fall" & doy==173) |
                  (legid=="1425590049-2019-2fall" & doy==192) |
                  (legid=="1546941291-2021-2fall" & doy==182) |
                  (legid=="1953212669-2007-2fall" & doy==178) |
                  (legid=="1953212677-2008-2fall" & doy==179) |
                  (legid=="1953212677-2009-2fall" & doy==172) |
                  (legid=="1953212677-2011-2fall" & doy==173) |
                  (legid=="1953212677-2012-2fall" & doy==165) |
                  (legid=="1953212677-2013-2fall" & doy==177) |
                  (legid=="1953212677-2014-2fall" & doy==161) |
                  (legid=="1953212677-2015-2fall" & doy==167) |
                  (legid=="1953212677-2016-2fall" & doy==167) |
                  (legid=="1953212677-2017-2fall" & doy==172) |
                  (legid=="1953212677-2018-2fall" & doy==167) |
                  (legid=="1953212677-2019-2fall" & doy==174) |
                  (legid=="1953212677-2020-2fall" & doy==172) |
                  (legid=="1953212681-2010-2fall" & doy==182) |
                  (legid=="1953212681-2012-1spring" & doy==113) |
                  (legid=="1953212681-2013-1spring" & doy==120) |
                  (legid=="1953212681-2017-1spring" & doy==103) |
                  (legid=="1953212681-2019-1spring" & doy==106) |
                  (legid=="1953212685-2011-2fall" & doy==190) |
                  (legid=="1953212686-2012-1spring" & doy==101)) %>% 
  mutate(segment="arrive")

#7e. Add new manually fixed departures to dataframe----
dat.arrive2 <- dat.arrive %>% 
  dplyr::filter(segment=="arrive") %>% 
  anti_join(dat.arrive.fix %>% 
              dplyr::select(legid)) %>% 
  rbind(dat.arrive.fix) %>% 
  rbind(dat.depart2 %>% 
          dplyr::filter(segment=="depart")) %>% 
  right_join(dat.dep) %>% 
  group_by(legid) %>% 
  arrange(date) %>% 
  mutate(segment = case_when((lead(segment)=="depart"& is.na(segment)) ~ "stationary",
                             (lag(segment)=="depart" & is.na(segment)) ~ "migration",
                             (lag(segment)=="arrive" & is.na(segment)) ~ "stationary",
                             (lead(segment)=="arrive" & is.na(segment)) ~ "migration",
                             !is.na(segment) ~ segment)) %>% 
  tidyr::fill(segment, .direction="updown") %>% 
  ungroup()

table(dat.arrive2$segment)
#575, 595

#7d. Visualize----
# ids <- sort(unique(dat.arrive2$legid))
# 
# for(i in 1:length(ids)){
#   
#   dat.i <- dat.arrive2 %>%
#     dplyr::filter(legid==ids[i])
#   
#   ggplot(dat.i) +
#     geom_path(aes(x=X, y=Y)) +
#     geom_point(aes(x=X, y=Y, colour = segment), size=3, alpha = 0.7, legend=FALSE) +
#     scale_colour_viridis_d()
#   
#   ggsave(filename=paste0("Figures/Arrival/", ids[i], ".jpeg"))
# }

#8. Classify seasons & add other dataframes----
dat.season <- dat.arrive2 %>% 
  arrange(id, date) %>% 
  mutate(season=case_when(segment=="migration" & legseason=="1spring" ~ "springmig",
                          segment=="migration" & legseason=="2fall" ~ "fallmig",
                          segment=="depart" & legseason=="1spring" ~ "springmig",
                          segment=="depart" & legseason=="2fall" ~ "fallmig",
                          segment=="arrive" & legseason=="1spring" ~ "breed", 
                          segment=="arrive" & legseason=="2fall" ~ "winter",
                          segment=="stationary" & legseason=="1spring" & lead(segment)=="depart" ~ "winter",
                          segment=="stationary" & legseason=="1spring" & lag(segment)=="arrive" ~ "breed",
                          segment=="stationary" & legseason=="2fall" & lead(segment)=="depart" ~ "breed",
                          segment=="stationary" & legseason=="2fall" & lag(segment)=="arrive" ~ "winter")) %>% 
  group_by(legid) %>% 
  fill(season, .direction="updown") %>% 
  rbind(dat.nodep) %>% 
  rbind(dat.gap %>% 
          dplyr::filter(!is.na(season))) %>% 
  dplyr::select(study, id, sensor, sex, mass, legid, lat, lon, X, Y, date, doy, hmmstate, staten, probState2, nsd, dist, legseason, segment, season) %>% 
  ungroup()

ggplot(dat.season) +
  geom_histogram(aes(x=doy)) +
  facet_grid(season~segment, scales="free")

table(dat.season$segment)
#575, 595 - good

#9. Visualize again----
# ids <- sort(unique(dat.season$id))
# 
# for(i in 1:length(ids)){
#   
#   dat.i <- dat.season %>% 
#     dplyr::filter(id==ids[i])
#   
#   ggplot(dat.i) +
#     geom_path(aes(x=X, y=Y)) +
#     geom_point(aes(x=X, y=Y, colour = season), size=3, alpha = 0.7) +
#     scale_colour_viridis_d() +
#     facet_wrap(~legid)
#   
#   ggsave(filename=paste0("Figures/Season/", ids[i], ".jpeg"))
# }

#12. Visualize distribution of dates----
ggplot(dat.season) +
  geom_histogram(aes(x=doy)) +
  facet_grid(season~segment, scales="free")

#13. Summary stats of dates----
dates <- dat.season %>% 
  #  dplyr::filter(state %in% c("depart", "arrive")) %>% 
  group_by(season, segment) %>% 
  summarize(mean = mean(doy),
            max = max(doy),
            min = min(doy),
            n = n()) %>% 
  ungroup()
dates

#14. Save----
write.csv(dat.season, "Data/LBCUFiltered&Predicted&Legged&SeasonedData.csv", row.names = FALSE)
