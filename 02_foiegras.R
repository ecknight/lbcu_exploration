library(tidyverse)
library(lubridate)
library(foieGras)

options(scipen = 999)

#1. Wrangle----
dat <- read.csv("Data/LBCUCleanedData.csv") %>% 
  arrange(id, datetime)

length(unique(dat$id))
#158 birds

#2. Filter with foiegras----
#Wrangle
dat.fg <- dat %>% 
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

#fit24 <- readRDS("Foiegras24h_rw.rds")

#3. Create predictions for each day----
g <- grab(fit24, what = "predicted", as_sf = FALSE) %>% 
    dplyr::select(id, date, lon, lat) %>% 
    data.frame() %>% 
    mutate(year = year(date),
         doy = yday(date))

#4. Remove predictions for big breaks in transmission----
dat.dt <- dat %>% 
  group_by(id) %>% 
  mutate(dtdays = dt/86400,
         days = lead(doy) - doy) %>% 
  dplyr::filter(days > 14) %>% 
  dplyr::select(id, year, doy, dt, dtdays, days) %>% 
  rename(startdoy = doy) %>% 
  mutate(startdoy = startdoy + 1,
         enddoy = floor(startdoy + days) - 1,
         id = as.character(id))

hist(dat.dt$days, breaks=100)

write.csv(dat.dt, "Data/TransmissionGaps.csv", row.names=FALSE)

dat.dt.seq <- data.frame()
for(i in 1:nrow(dat.dt)){
  
  dat.i <- data.frame(doy = seq(dat.dt$startdoy[i], dat.dt$enddoy[i], 1),
                      id = dat.dt$id[i],
                      year = dat.dt$year[i])
  
  dat.dt.seq <- rbind(dat.dt.seq, dat.i)
}

dat.g <- g %>% 
  anti_join(dat.dt.seq) %>% 
  dplyr::select(id, date, doy, lon, lat) %>% 
  unique()

#5. Add metadata back in----
dat.meta <- dat %>% 
  dplyr::select(id, study, sensor, depseason, sex, mass) %>% 
  unique() %>% 
  full_join(dat.g %>% 
              mutate(id = as.numeric(id)))

#6. Save-----
write.csv(dat.meta, "Data/LBCUFilteredData.csv", row.names = FALSE)

#7. Number of birds----
length(unique(dat.g$id))
#158