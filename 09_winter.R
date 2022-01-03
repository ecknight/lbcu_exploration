library(tidyverse)
library(lubridate)
library(moveHMM)
library(meanShiftR)

options(scipen = 99999)

#1. Read in & wrangle----
dat <- read.csv("Data/LBCUSegmented&StopoverData.csv") %>% 
  dplyr::filter(season=="winter") %>% 
  mutate(date = ymd_hms(date)) %>% 
  mutate(winteryear = ifelse(doy < 135, year-1, year),
         winterid = paste0(id, "-", winteryear))
#TO DO: FINISH CLEANING TO GET THIS TO WORK

#2. Prep for HMM----
dat.ll <- dat %>% 
  dplyr::select(winterid, lon, lat) %>% 
  rename(ID=winterid) %>% 
  dplyr::select(-ID)

md <- prepData(dat.ll, coordNames = c("lon","lat"), type="LL")

#3. Choose starting parameters----
## Visualize to choose initial parameters
plot(density(abs(na.omit(md$angle))))
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

#5. Visualize----
plot(m, plotCI = TRUE, plotTracks=FALSE)

#6. Add states to data----
dat$winterState <- viterbi(m)
#dat$probState1 <- stateProbs(m)[,1]
#dat$probState2 <- stateProbs(m)[,2]

#7. Cluster----
ids <- unique(dat$id)

dat.shift <- data.frame()
for(i in 1:length(ids)){
  dat.i <- dat %>% 
    dplyr::filter(id==ids[i])
  
  mat1 <- matrix(dat.i$X)
  mat2 <- matrix(dat.i$Y)
  mat <- cbind(mat1, mat2)
  
  #3. Cluster----
  shift <- meanShift(mat,
                     algorithm="KDTREE",
                     bandwidth=c(1,1))
  
  dat.shift <- dat.i %>% 
    mutate(wintercluster = shift[[1]]) %>% 
    rbind(dat.shift)
  
  
}

#8. Count points per cluster----
dat.shift.n <- dat.shift %>% 
  group_by(legid, wintercluster) %>% 
  mutate(winterclustern = n(),
         minstate = min(winterState)) %>% 
  ungroup()

#9. Classify clusters----
dat.wint <- dat.shift.n %>% 
  dplyr::filter(minstate==1) %>% 
  dplyr::select(id, wintercluster) %>% 
  unique() %>% 
  group_by(id) %>% 
  mutate(winter = row_number(),
         wintern = max(winter)) %>% 
  ungroup() %>% 
  left_join(dat.shift.n) %>% 
  mutate(winter = ifelse(minstate==2, "wintermig", winter))

#10. Number of clusters----
dat.n <- dat.wint %>% 
  dplyr::select(id, wintern) %>% 
  unique()

table(dat.n$wintern)
#TO DO: FIGURE OUT 6 CLUSTER BIRD, ASSESS 3 CLUSTER BIRDS
  
#11. Visualize----
dat.i <- dat.wint %>% 
  dplyr::select(id) %>% 
  unique() %>% 
  sample_n(1) %>% 
  left_join(dat.wint)

dat.i <- dat.wint %>% 
  dplyr::filter(winter=="3") %>% 
  dplyr::select(id) %>% 
  unique() %>% 
  left_join(dat.wint)

ggplot(dat.i) +
  geom_point(aes(x=lon, y=lat, colour=winter, shape=factor(winterState))) +
  facet_grid(winteryear~id) +
  scale_colour_viridis_d()

#12. Save----
write.csv(dat.wint, "Data/LBCUSegmented&Stopover&WinterData.csv", row.names = FALSE)
write.csv(dat.wint, "/Users/ellyknight/Dropbox/LBCU/LBCUSegmented&Stopover&WinterData.csv", row.names = FALSE)
