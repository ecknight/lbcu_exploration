library(tidyverse)
library(lubridate)
library(moveHMM)
library(meanShiftR)

options(scipen=9999)

#1. Read in and filter data----
dat.raw <- read.csv("Data/LBCUFiltered&Predicted&Legged&SeasonedData.csv") %>% 
  mutate(date = ymd_hms(date),
         year = year(date))

dat <- dat.raw %>% 
  dplyr::filter(season %in% c("fallmig", "springmig")) %>% 
  rename(ID=legid)

dat.ll <-  dat %>% 
  dplyr::select(ID, lon, lat)

#2. Prep for HMM----
md <- moveHMM::prepData(dat.ll, coordNames = c("lon","lat"), type="LL") %>% 
  mutate(step = ifelse(step==0, 0.001, step))

#3. Choose starting parameters----
## Visualize to choose initial parameters
plot(density(abs(na.omit(md$angle))))
plot(density(na.omit(md$step)))
plot(density(na.omit(md$step)), xlim=c(0,100))

#Starting values
# mean step lengths for state 1 (residency) and state 2 (migration)
mu0 <- c(0.1,75)
#Step SD
sigma0 <- c(1,100)
stepPar0 <- c(mu0,sigma0)
#Angle mean
angleMean0 <- c(pi,0)
# angle concentration
kappa0 <- c(1,1) 
anglePar0 <- c(angleMean0,kappa0)

#4. Fit model----
m <- fitHMM(data=md, 
            nbStates = 2, 
            stepPar0 = stepPar0, 
            anglePar0 = anglePar0, 
            formula = ~1)

#4. Visualize----
plot(m, plotCI = TRUE, plotTracks=FALSE)

#5. Add states to data----
dat.out <- data.frame(md) %>% 
  mutate(hmmstate_stopover = viterbi(m)) %>% 
  rename(legid=ID,
         lat=y,
         lon=x) %>% 
  dplyr::select(-step, -angle) %>% 
  left_join(dat %>% 
              rename(legid=ID))
#dat$probState1 <- stateProbs(m)[,1]
#dat$probState2 <- stateProbs(m)[,2]

#6. Put data backtogether----
dat.stop <- dat.raw %>% 
  anti_join(dat.out) %>% 
  mutate(hmmstate_stopover = NA) %>% 
  rbind(dat.out) %>% 
  mutate(stopover = ifelse(hmmstate_stopover==1, 1, 0),
         stopover = ifelse(is.na(stopover), 0, stopover))

#7. Add IDs to stopovers----
ids <- dat.stop %>% 
  dplyr::filter(stopover==1) %>% 
  dplyr::select(id, year, season) %>% 
  unique()

dat.shift <- data.frame()
for(i in 1:nrow(ids)){
  dat.i <- dat.stop %>% 
    dplyr::filter(id==ids$id[i],
                  season==ids$season[i],
                  year==ids$year[i],
                  stopover==1)
  
  mat1 <- matrix(dat.i$X)
  mat2 <- matrix(dat.i$Y)
  mat <- cbind(mat1, mat2)
  
  shift <- meanShift(mat,
                     algorithm="KDTREE",
                     bandwidth=c(1,1))
  
  dat.shift <- dat.i %>% 
    mutate(stopovercluster = shift[[1]]) %>% 
    rbind(dat.shift)
  
}

#8. Put back together and remove stopovers < 3 days----
dat.stopclust <- dat.stop %>% 
  left_join(dat.shift) %>% 
  group_by(id, year, season, stopovercluster) %>% 
  mutate(days=n()) %>% 
  ungroup() %>% 
  mutate(stopover = ifelse(days < 3, 0, stopover),
         stopovercluster = ifelse(stopover==0, NA, stopovercluster),
         days = ifelse(stopover==0, NA, days))

#9. Visualize----
ggplot(dat.stopclust %>% dplyr::filter(!is.na(stopovercluster))) +
  geom_point(aes(x=lon, y=lat, colour=factor(stopover)), size=3, alpha=0.5) +
#  geom_point(aes(x=lon, y=lat, colour=days), size=3, alpha=0.5) +
#  scale_colour_viridis_c() +
  facet_wrap(~season)

plot.id <- ggplot(dat.stopclust %>% dplyr::filter(!is.na(stopovercluster))) +
  geom_point(aes(x=lon, y=lat, colour=factor(stopovercluster)), size=3, alpha=0.5) +
  facet_wrap(id~season)

ggsave(plot.id, filename="Figures/StopoverClusters.jpeg", width = 18, height = 12)

#8. Save----
write.csv(dat.stopclust, "Data/LBCUFiltered&Predicted&Legged&Seasoned&StopoverData.csv", row.names = FALSE)
