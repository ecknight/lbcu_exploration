library(tidyverse)
library(lubridate)
library(foieGras)

options(scipen = 999)

dat <- read.csv("Data/LBCUCleanedData.csv") %>% 
  arrange(id, datetime)

length(unique(dat$id))
#125 birds

dat.id <- dat %>% 
  dplyr:: select(id, mig, sensor) %>% 
  unique()

table(dat.id$sensor)

smaj.na <- dat %>% 
  dplyr::filter(is.na(smaj))

table(smaj.na$id, smaj.na$sensor)

#1. Wrangle----
dat.mig <- dat %>% 
  dplyr::filter(mig==1)

#2. Filter with foiegras----
#Wrangle
dat.fg <- dat.mig %>% 
  rename(date = datetime, lc = argos, lon = long) %>% 
  dplyr::select(id, date, lc, lon, lat, smaj, smin, eor) 

#Fit
fit24 <- fit_ssm(dat.fg, 
                 time.step = 24, 
                 vmax = 35, 
                 ang = c(5,15), 
                 min.dt = 10, 
                 model = "rw") 

#Visualize
fmap(fit24, conf=TRUE, what="predicted")

#Without CIs
pred24 <- grab(fit24, what = "predicted", as_sf=FALSE)
p <- ggplot() +
  geom_path(data = pred24, aes(x = lon, y = lat, group = id, col=id)) +
  geom_point(data = pred24, aes(x = lon, y = lat, group = id, col=id)) +
  theme(legend.position = "none")
p

saveRDS(fit24, file = "Foiegras24h_rw.rds")

fit24 <- readRDS("Foiegras24h_rw.rds")

#3. Create predictions for each day----
g = grab(fit24, what = "predicted", as_sf = FALSE) %>% 
  dplyr::select(id, date, lon, lat) %>% 
  data.frame()

#4. Remove predictions for big breaks in transmission----
dat.dt <- dat %>% 
  mutate(dtdays = dt/86400,
    logdt = log(dt)) %>% 
  dplyr::filter(logdt > 13)

hist(dat.dt$logdt)

dat.g <- g

write.csv(dat.g, "Data/LBCUFilteredData.csv", row.names = FALSE)

#5. Number of birds----
length(unique(dat.g$id))
#122 - Good. Took out 7 birds that never migrated.