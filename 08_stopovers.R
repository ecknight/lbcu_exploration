library(tidyverse)
library(lubridate)
library(moveHMM)

options(scipen=9999)

#1. Read in and filter data----
dat.raw <- read.csv("Data/LBCUSegmentedData.csv") %>% 
  mutate(date = ymd_hms(date),
         year = year(date))

dat <- dat.raw %>% 
  dplyr::filter(season %in% c("fallmig", "springmig")) %>% 
#  dplyr::select(legid, lat, lon) %>% 
  rename(ID=legid) %>% 
  dplyr::filter(ID!="164383320-2017-1spring")

#2. Prep for HMM----
md <- prepData(dat, coordNames = c("lon","lat"), type="LL")

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
zeromass0 <- c(0,0)
stepPar0 <- c(mu0,sigma0, zeromass0)

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
dat$stopoverState <- viterbi(m)
#dat$probState1 <- stateProbs(m)[,1]
#dat$probState2 <- stateProbs(m)[,2]

#6. Put data backtogether----
dat.stop <- dat.raw %>% 
  anti_join(dat %>% 
              dplyr::rename(legid=ID)) %>% 
  mutate(stopoverState = NA) %>% 
  rbind(dat %>% 
          dplyr::rename(legid=ID)) %>% 
  mutate(stopover = ifelse(stopoverState==1, 1, 0),
         stopover = ifelse(is.na(stopover), 0, stopover))

#7. Visualize----
ggplot(dat.stop) +
  geom_point(aes(x=lon, y=lat, colour=factor(stopover)), size=3, alpha=0.5) +
  facet_wrap(~season)

ids <- unique(dat.stop$id)

dat.stop$stopover <- factor(dat.stop$stopover, levels=c(0, 1), labels=c("other", "stopover"))

for(i in 1:length(ids)){
  
  dat.i <- dat.stop %>% 
    dplyr::filter(id==ids[i])
  
  ggplot(dat.i) +
    geom_path(aes(x=X, y=Y)) +
    geom_point(aes(x=X, y=Y, colour = stopover), size=3, alpha = 0.7) +
    facet_grid(season~year)
  
#  ggsave(filename=paste0("Figures/Stopover/", ids[i], ".jpeg"))
  
}

#8. Save----
write.csv(dat.stop, "Data/LBCUSegmented&StopoverData.csv", row.names = FALSE)
write.csv(dat.stop, "/Users/ellyknight/Dropbox/LBCU/LBCUSegmented&StopoverData.csv", row.names = FALSE)
