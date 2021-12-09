library(tidyverse)
library(lubridate)
library(foieGras)


options(scipen = 999)

dat <- read.csv("Data/LBCUCleanedData.csv") %>% 
  arrange(id, datetime)

#1. Wrangle----
dat.mig <- dat %>% 
  dplyr::filter(!(is.na(smaj) & sensor=="Argos Doppler Shift"),
                mig==1)

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
coast <- ne_countries(scale=110, returnclass = "sf") 
p <- ggplot() +
  annotation_spatial(data = coast, fill = grey(0.8), lwd = 0) +
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

#TODO: Come back to this. Consider removing anything with > 5 days transmision gap
#46200880
#  mutate(id = case_when(id==172070515 & datetime <= "2017-02-19 11:18:29" ~ paste0(id, 1),
#                         id==172070515 & datetime > "2017-02-19 11:18:29" ~ paste0(id, 2),
#                         id==46768108 & datetime <= "2015-03-11 01:07:49" ~ paste0(id, 1),
#                         id==46768108 & datetime > "2015-03-11 01:07:49" ~ paste0(id, 2),
#                         id==46769588 & datetime <= "2019-03-14 04:55:49" ~ paste0(id, 1),
#                         id==46769588 & datetime > "2019-03-14 04:55:49" ~ paste0(id, 2),
#                        !is.na(id) ~ id))