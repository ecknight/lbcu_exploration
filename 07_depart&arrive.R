library(tidyverse)
library(lubridate)

options(scipen = 99999)

#TO DO: GO BACK AND DEAL WITH INCOMPLETE LEGS####
#TO DO: CHECK # OF BIRDS####
#TO DO: DECIDE WHAT TO DO WITH LONG STRETCHES WITH NO TRANSMISSIONS

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
  dat.stateid <- uncount(dat.rle, length)
  
  dat.i$stateid <- dat.stateid$id
  
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

depart <- dat %>% 
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
  mutate(segment = "depart")

#4. Classify arrival----
arrive <- dat %>% 
  dplyr::filter(predictedState==1,
                nsdrecip <= 20000000000) %>% 
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
  mutate(segment = "arrive")

#5. Put together----
dat.seg <- rbind(depart, arrive) %>% 
  right_join(dat) %>% 
  mutate(segment= case_when(legid=="164383320-2017-1spring" & doy==73 ~ "depart",
                            legid=="279282698-2019-1spring" & doy==74 ~ "depart",
                            !is.na(segment) ~ segment)) %>% 
  mutate(segment = ifelse(is.na(segment), "something", segment)) %>% 
  arrange(legid, migdoy) 

table(dat.seg$segment)
length(unique(dat.seg$legid))
#arrival missing many many

#6. Troubleshoot NAs----
id.arrive <- dat.seg %>% 
  dplyr::filter(segment=="arrive") %>% 
  dplyr::select(legid) %>% 
  unique()

na.arrive <- dat.seg %>% 
  dplyr::select(id, legid, legseason) %>% 
  unique() %>% 
  anti_join(id.arrive) %>% 
  arrange(legseason, id, legid)

dat.id <- dat.seg %>% 
  dplyr::filter(legid=="46768108-2016-1spring") %>% 
#  dplyr::filter(id==164383320) %>% 
  dplyr::select(id, legid, date, doy, predictedState, stateid, probState2, nsd, nsdrecip, dist, cp)
View(dat.id)

#7. Visualize----
#ids <- unique(dat.seg$id)
ids <- unique(na.arrive$id)

for(i in 1:length(ids)){
  
  dat.i <- dat.seg %>% 
    dplyr::filter(id==ids[i])
  
  ggplot(dat.i) +
    geom_path(aes(x=X, y=Y)) +
    geom_point(aes(x=X, y=Y, colour=nsd, shape = segment), size=3, alpha = 0.7) +
    scale_colour_viridis_c() +
    facet_wrap(~legid)
  
  ggsave(filename=paste0("Figures/departarrive/", ids[i], ".jpeg"))
  
}

#8. Summary stats of dates----
dates <- dat.seg %>% 
  dplyr::filter(!segment=="something") %>% 
  group_by(legseason, segment) %>% 
  summarize(mean = mean(doy),
            max = max(doy),
            min = min(doy)) %>% 
  ungroup()
dates

ggplot(dat.seg %>% dplyr::filter(segment !="something")) +
  geom_histogram(aes(x=doy)) +
  facet_grid(segment~legseason)

#looks like 1 early outlier for departure in each season