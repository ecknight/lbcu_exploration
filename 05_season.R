library(tidyverse)
library(lubridate)

options(scipen = 99999)

#1. Read in data----
dat.raw <- read.csv("Data/LBCUFiltered&Predicted&LeggedData.csv")

#number of birds
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

#3. Look at transmission gaps----
gaps <- read.csv("Data/TransmissionGaps.csv") %>% 
  dplyr::select(id, year, startdoy, enddoy, dtdays)

dat.dt <- gaps %>% 
  dplyr::select(id, year) %>% 
  unique() %>% 
  dplyr::filter(id=="1378421380") %>% 
  inner_join(dat) %>% 
  dplyr::select(id, year, legid, date, doy, lat, lon, nsd)

#33088 - none missed
#94033 - 2009 fall mig depart & arrival missed
#94034 - 2009 fall mig depart & arrival missed
#46768108 - 2014 - none missed
#46768108 - 2015 spring mig depart missed
#46769588 - 2019 spring mig depart & arrival missed
#46769927 - 2014 spring mig depart & arrival missed
#46770723 - 2014 spring mig depart & arrival missed
#145696972 - none missed
#145698291 - none missed
#172070515 - 2017 spring mig depart & arrival missed
#279282698 - none missed
#290352179 - none missed
#973657763 - none missed
#1378421378 - none missed
#1378421379 - none missed
#1378421380 - none missed
#1378421381 - none missed

#Remove & classify legs where departure & arrival missed
#2 gaps taken out because there are multiple for that id/year
#1 classified by hand because didn't start migration
dat.gap <- dat %>%
  dplyr::filter(legid %in% c("94033-2019-2fall",
                              "94034-2019-2fall",
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
                            legseason=="1spring" & doy >= enddoy ~ "breed",
                            legid=="46200876-2017-2fall" ~ "winter"),
         segment="stationary") %>% 
  dplyr::select(-startdoy, -enddoy, -dtdays)
  

#4. Legs where bird didn't start migration----
#1 classified by hand because outside of date range (died perhaps?)
dat.nodep <- dat %>% 
  anti_join(dat.gap) %>% 
  group_by(legid) %>% 
  summarize(nsdmax = max(nsd),
            mindoy = min(doy)) %>% 
  ungroup() %>% 
  dplyr::filter(nsdmax < 38036899358.37) %>% 
  left_join(dat) %>% 
  mutate(season = case_when(legseason=="1spring" & (mindoy < 35 | mindoy >= 220) ~ "winter",
                            legseason=="1spring" & mindoy >= 35 & mindoy < 220 ~ "breed",
                            legseason=="2fall" & mindoy < 220 & mindoy >= 35 ~ "breed",
                            legseason=="2fall" & (mindoy >= 220 | mindoy < 35) ~ "winter"),
         segment = "stationary") %>% 
  dplyr::select(-nsdmax, -mindoy) %>% 
  rbind(dat.gap) %>% 
  unique() %>% 
  mutate(season = case_when(legid=="86872-2009-2fall" ~ "winter",
                            legid=="46200876-2017-2fall" ~ "winter",
                            !is.na(season) ~ season))

#Data for birds that did start migration to carry on to next step
dat.dep <- dat %>% 
  anti_join(dat.nodep)

#5. Classify departure----
#departure = beginning of earliest state 2 cluster after nsd > 20000000000
#11 departures done manually because nsd < 20000000 and migration very fast
#7 additional departures done manually because nsd > 200000000 on stationary grounds
#1 done manually because all sorts of roaming around
dat.depart <- dat.dep %>% 
  dplyr::filter(hmmstate==2,
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
  mutate(segment = case_when(legid=="164383320-2017-1spring" & doy==72 ~ "depart",
                             legid=="164383320-2018-1spring" & doy==77 ~ "depart",
                             legid=="164383320-2017-2fall" & doy==174 ~ "depart",
                             legid=="281981414-2018-1spring" & doy==69 ~ "depart",
                             legid=="46694077-2014-1spring" & doy==93 ~ "depart",
                             legid=="94033-2009-2fall" & doy==162 ~ "depart",
                             legid=="188150741-2019-1spring" & doy==73 ~ "depart",
                             legid=="279282698-2019-1spring" & doy==75 ~ "depart",
                             legid=="279818280-2018-1spring" & doy==88 ~ "depart",
                             legid=="145696972-2016-2fall" & doy==168 ~ "depart",
                            legid=="1953212667-2008-2fall" & doy==171 ~ "depart",
                            legid=="1953212669-2008-2fall" & doy==183 ~ "depart",
                            legid=="1953212672-2009-1spring" & doy==87 ~ "depart",
                            legid=="1953212676-2011-1spring" & doy==77 ~ "depart",
                            legid=="1953212677-2008-2fall" & doy==176 ~ "depart",
                            legid=="1953212677-2015-2fall" & doy==165 ~ "depart",
                            legid=="1953212677-2016-1spring" & doy==87 ~ "depart",
                            legid=="1953212677-2018-1spring" & doy==69 ~ "depart",
                            legid=="1953212677-2020-1spring" & doy==86 ~ "depart",
                            legid=="1953212663-2010-2fall" & doy==184 ~ "depart",
                            legid=="1953212669-2009-1spring" & doy==96 ~ "depart",
                            legid=="890854392-2019-2fall" & doy==181 ~ "depart",
                             !is.na(segment) ~ segment),
         segment = ifelse(legid=="46694077-2014-1spring" & doy==101, NA, segment),
         segment = ifelse(legid=="94033-2009-2fall" & doy==295, NA, segment),
         segment = ifelse(legid=="188150741-2019-1spring" & doy==100, NA, segment),
         segment = ifelse(legid=="279282698-2019-1spring" & doy==54, NA, segment),
         segment = ifelse(legid=="279818280-2018-1spring" & doy==55, NA, segment),
         segment = ifelse(legid=="145696972-2016-2fall" & doy==136, NA, segment),
         segment = ifelse(legid=="1953212672-2009-1spring" & doy==53, NA, segment),
         segment = ifelse(legid=="1953212676-2011-1spring" & doy==43, NA, segment),
         segment = ifelse(legid=="1953212677-2008-2fall" & doy==188, NA, segment),
         segment = ifelse(legid=="1953212677-2015-2fall" & doy==178, NA, segment),
         segment = ifelse(legid=="1953212677-2016-1spring" & doy==51, NA, segment),
         segment = ifelse(legid=="1953212677-2018-1spring" & doy==43, NA, segment),
         segment = ifelse(legid=="1953212677-2020-1spring" & doy==45, NA, segment),
         segment = ifelse(legid=="1953212667-2008-2fall" & doy==203, NA, segment),
         segment = ifelse(legid=="1953212669-2008-2fall" & doy==191, NA, segment),
         segment = ifelse(legid=="890854392-2019-2fall" & doy==348, NA, segment),
         segment = ifelse(legid=="164383320-2017-2fall" & doy==181, NA, segment)) %>% 
  dplyr::filter(segment=="depart") %>% 
  full_join(dat.dep)

#check if # of departures is same as # of legids
length(unique(dat.dep$legid))
table(dat.depart$segment)
#597

dat.2 <- dat.depart %>% 
  dplyr::filter(segment=="depart") %>% 
  group_by(legid) %>% 
  mutate(n=n()) %>% 
  ungroup() %>% 
  dplyr::filter(n > 1) %>% 
  arrange(legid, doy)

dat.0 <- dat.depart %>% 
  dplyr::select(legid) %>% 
  unique() %>% 
  anti_join(dat.depart %>% 
              dplyr::filter(segment=="depart") %>% 
              group_by(legid) %>% 
              mutate(n=n()) %>% 
              ungroup() %>% 
              dplyr::filter(n > 0))

#6. Filter out migration segments that weren't completed----
dat.arr <- dat.depart %>%
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
#20

length(unique(dat.arr$legid))
#597-20=577

#7. Classify arrival----
#arrival = first date of first run after departure of at least 30 stationary points
#OR
#first day of last run of stationary points

#Fill in migration segment
dat.migration <- dat.arr %>% 
  group_by(legid) %>% 
  arrange(date) %>% 
  mutate(segment2 = ifelse(lag(segment)=="depart", "migration", segment)) %>% 
  tidyr::fill(segment2, .direction="down") %>% 
  ungroup() %>% 
  mutate(segment = ifelse(is.na(segment2), segment, segment2)) %>% 
  dplyr::select(-segment2)

table(dat.migration$segment)
#577

#Classify arrival via stopover rule
#7 birds done manually due to a couple predicted nonstationary points after arrival (i.e., last segment rule doesn't work)
#1953212670 & 1953212677 & 1953212663 done manually because lots of movement in data (suspect high argos error)
#1 bird done manually because migration very fast
dat.arrive.stopover <- dat.migration %>% 
  mutate(segment2 = ifelse(segment=="migration" & hmmstate==1 & staten > 30, "arrive", segment)) %>% 
  dplyr::filter(segment2=="arrive") %>% 
  mutate(segment2 = ifelse(legid %in% c("1953212663-2008-2fall",
                                        "1953212670-2008-2fall",
                                        "1953212670-2009-2fall",
                                        "1953212670-2010-2fall",
                                        "1953212677-2008-2fall",
                                        "1953212677-2009-2fall",
                                        "1953212677-2011-2fall",
                                        "1953212677-2012-2fall",
                                        "1953212677-2013-2fall",
                                        "1953212677-2014-2fall",
                                        "1953212677-2015-2fall",
                                        "1953212677-2016-2fall",
                                        "1953212677-2017-2fall",
                                        "1953212677-2018-2fall",
                                        "1953212677-2019-2fall"), NA, segment2)) %>% 
  group_by(legid) %>% 
  arrange(date) %>% 
  filter(row_number()==1) %>% 
  ungroup() %>% 
  right_join(dat.migration) %>% 
  mutate(segment = ifelse(!is.na(segment2), segment2, segment)) %>% 
  arrange(id, date) %>% 
  dplyr::select(-segment2) %>% 
  mutate(segment = case_when(legid=="33089-2011-1spring" & doy==112 ~ "arrive",
                             legid=="71767680-2018-1spring" & doy==79 ~ "arrive",
                             legid=="46769927-2015-1spring" & doy==103 ~ "arrive",
                             legid=="145696972-2016-1spring" & doy==102 ~ "arrive",
                             legid=="172070515-2016-2fall" & doy==193 ~ "arrive",
                             legid=="279288326-2018-1spring" & doy==102 ~ "arrive",
                             legid=="1953212663-2008-2fall" & doy==192 ~ "arrive",
                             legid=="1953212670-2008-2fall" & doy==182 ~ "arrive",
                             legid=="1953212670-2009-1spring" & doy==90 ~ "arrive",
                             legid=="1953212670-2009-2fall" & doy==175 ~ "arrive",
                             legid=="1953212670-2010-2fall" & doy==179 ~ "arrive",
                             legid=="1953212677-2008-2fall" & doy==178 ~ "arrive",
                             legid=="1953212677-2009-2fall" & doy==171 ~ "arrive",
                             legid=="1953212677-2011-2fall" & doy==173 ~ "arrive",
                             legid=="1953212677-2012-2fall" & doy==164 ~ "arrive",
                             legid=="1953212677-2013-2fall" & doy==161 ~ "arrive",
                             legid=="1953212677-2014-2fall" & doy==170 ~ "arrive",
                             legid=="1953212677-2015-2fall" & doy==167 ~ "arrive",
                             legid=="1953212677-2016-2fall" & doy==166 ~ "arrive",
                             legid=="1953212677-2017-2fall" & doy==162 ~ "arrive",
                             legid=="1953212677-2018-2fall" & doy==167 ~ "arrive",
                             legid=="1953212677-2019-2fall" & doy==173 ~ "arrive",
                             legid=="1953212677-2020-2fall" & doy==171 ~ "arrive",
                             legid=="890854392-2019-2fall" & doy==182 ~ "arrive",
                             !is.na(segment) ~ segment))


#Check number classified
table(dat.arrive.stopover$segment)
#446 - 131 missing

#Find missing ids
arrive.ids <- dat.arr %>% 
  dplyr::select(legid) %>% 
  unique() %>% 
  anti_join(dat.arrive.stopover %>% 
              dplyr::filter(segment=="arrive") %>% 
              dplyr::select(legid) %>% 
              unique())
nrow(arrive.ids)
#131

#check if any have 2
dat.2 <- dat.arrive.stopover %>% 
  dplyr::filter(segment=="arrive") %>% 
  group_by(legid) %>% 
  mutate(n=n()) %>% 
  ungroup() %>% 
  dplyr::filter(n > 1) %>% 
  arrange(legid, doy)

#Classify arrival for remaining ids using last segment rule
dat.arrive.last <- dat.migration %>% 
  dplyr::filter(legid %in% arrive.ids$legid,
                hmmstate==1) %>% 
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
#131

#Put two arrival classifications back together
dat.arrive <- dat.arrive.stopover %>% 
  dplyr::filter(!legid %in% arrive.ids$legid) %>% 
  rbind(dat.arrive.last)

table(dat.arrive$segment)
#577

#8. Put together & fill in the gaps as migratory or stationary----
dat.state <- dat.depart %>% 
  dplyr::filter(segment=="depart") %>% 
  rbind(dat.arrive %>% 
          dplyr::filter(segment=="arrive")) %>% 
  right_join(dat.dep) %>% 
  arrange(legid, date) %>% 
  mutate(segment2 = case_when(lag(segment)=="depart" ~ "migration",
                              lead(segment)=="depart" ~ "stationary",
                              lead(segment)=="arrive" ~ "migration",
                              lag(segment)=="arrive" ~ "stationary",
                              !is.na(segment) ~ segment)) %>% 
  mutate(segment = ifelse(is.na(segment2), segment, segment2)) %>% 
  dplyr::select(-segment2) %>% 
  group_by(legid) %>% 
  mutate(segment = ifelse(row_number()==1, "stationary", segment)) %>% 
  fill(segment) %>% 
  ungroup()

table(dat.state$segment)
#576, 596

dat.0 <- dat.season %>% 
  dplyr::select(legid) %>% 
  unique() %>% 
  anti_join(dat.state %>% 
              dplyr::filter(segment=="arrive") %>% 
              group_by(legid) %>% 
              mutate(n=n()) %>% 
              ungroup() %>% 
              dplyr::filter(n > 0)) %>% 
  dplyr::filter(legid %in% dat.arr$legid)

#9. Classify seasons----
#1 done manually because arrival date unknown from transmission gap & filling not working
#3 done manually because no gap between departure & arrival
dat.season <- dat.state %>% 
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
  dplyr::select(study, id, sensor, sex, mass, legid, lat, lon, X, Y, date, doy, hmmstate, staten, probState2, nsd, dist, legseason, segment, season) %>% 
  ungroup() %>% 
  mutate(season=case_when(legid=="172070515-2017-2fall" & doy < 169 & doy > 32 ~ "breed",
                          legid=="46768108-2015-2fall" & doy < 154 & doy > 32 ~ "breed",
                          legid=="46769588-2019-2fall" & doy < 169 & doy > 32 ~ "breed",
                          legid=="890854392-2019-2fall" & (doy > 182 | doy < 81)  ~ "winter",
                          legid=="890854392-2020-1spring" & doy < 81  ~ "winter",
                          legid=="46200876-2017-2fall" ~ "winter",
                          !is.na(season) ~ season)) %>% 
  mutate(segment = case_when(legid=="890854392-2019-2fall" & doy==181 ~ "depart",
                             legid=="890854392-2019-2fall" & doy==182 ~ "arrive",
                             !is.na(segment) ~ segment))

table(dat.season$segment)
#577, 597 - good

#10. Add birds that never left back in----
dat.all <- dat.nodep %>% 
  dplyr::select(study, id, sensor, sex, mass, legid, lat, lon, X, Y, date, doy, hmmstate, staten, probState2, nsd, dist, legseason, segment, season) %>% 
  rbind(dat.season) %>% 
  arrange(study, id, date)

table(dat.all$segment)
#577, 597

#11. Visualize each bird----
ids <- sort(unique(dat.all$id))

for(i in 1:length(ids)){
  
  dat.i <- dat.all %>% 
    dplyr::filter(id==ids[i])
  
  ggplot(dat.i) +
    geom_path(aes(x=X, y=Y)) +
    geom_point(aes(x=X, y=Y, colour = season), size=3, alpha = 0.7) +
    scale_colour_viridis_d() +
    facet_wrap(~legid)
  
  ggsave(filename=paste0("Figures/Season/", ids[i], ".jpeg"))
}

#12. Visualize distribution of dates----
ggplot(dat.all) +
  geom_histogram(aes(x=doy)) +
  facet_grid(season~segment, scales="free")

#13. Summary stats of dates----
dates <- dat.all %>% 
  #  dplyr::filter(state %in% c("depart", "arrive")) %>% 
  group_by(season, segment) %>% 
  summarize(mean = mean(doy),
            max = max(doy),
            min = min(doy),
            n = n()) %>% 
  ungroup()
dates

#14. Save----
write.csv(dat.all, "Data/LBCUFiltered&Predicted&Legged&SeasonedData.csv", row.names = FALSE)
