library(tidyverse)
library(lubridate)
library(moveHMM)
library(meanShiftR)
library(sf)
library(adehabitatLT)
library(data.table)

options(scipen = 99999)

#1. Read in & wrangle----
dat.raw <- read.csv("Data/LBCUFiltered&Predicted&Legged&Seasoned&StopoverData.csv") %>% 
  mutate(date = ymd_hms(date))

dat <- dat.raw %>% 
  dplyr::filter(season=="winter") %>% 
  mutate(winteryear = ifelse(doy < 135, year-1, year),
         winterid = paste0(id, "-", winteryear)) %>% 
  dplyr::filter(!winterid %in% c("1378421380-2020", "1953212676-2009", "290352179-2019"))

#2. Prep for HMM----
dat.ll <- dat %>% 
  arrange(winterid, date) %>% 
  dplyr::select(winterid, lon, lat) %>% 
  rename(ID=winterid)

md <- prepData(dat.ll, coordNames = c("lon","lat"), type="LL") %>% 
  mutate(step = ifelse(step==0, 0.001, step))

#3. Choose starting parameters----
## Visualize to choose initial parameters
plot(density(abs(na.omit(md$angle))))
plot(density(na.omit(md$step)), xlim=c(0,100))

#Starting values
# mean step lengths for state 1 (stationary) and state 2 (movement)
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

#5. Visualize----
plot(m, plotCI = TRUE, plotTracks=FALSE)

#6. Add states to data----
dat$hmmstate_winter <- viterbi(m)

#7. Cluster----
ids <- unique(dat$id)

dat.shift <- data.frame()
for(i in 1:length(ids)){
  dat.i <- dat %>% 
    dplyr::filter(id==ids[i])
  
  mat1 <- matrix(dat.i$X)
  mat2 <- matrix(dat.i$Y)
  mat <- cbind(mat1, mat2)
  
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
         minstate = min(hmmstate_winter)) %>% 
  ungroup()

#9. Classify winter home ranges----
#winter home range = a cluster that has > 7 points and at least 1 stationary point
dat.wint <- dat.shift.n %>% 
  dplyr::filter(minstate==1, winterclustern > 7) %>% 
  dplyr::select(id, wintercluster) %>% 
  unique() %>% 
  group_by(id) %>% 
  mutate(winter = row_number(),
         wintern = max(winter)) %>% 
  ungroup() %>% 
  right_join(dat.shift.n) %>% 
  mutate(winter = ifelse(is.na(winter), "wintermig", paste0("winter", winter)))

#10. Assess & tidy birds with multiple home ranges----
dat.n <- dat.wint %>% 
  dplyr::select(id, wintern) %>% 
  unique()

table(dat.n$wintern)
  
#Look at birds with three home ranges
dat.3 <- dat.wint %>% 
  dplyr::filter(wintern>2) %>% 
  dplyr::select(id) %>% 
  unique()
dat.3
#973658514 - should perhaps collapse winter2, winter3, & wintermig
#77637122 - collapse winter2 & winter 3
#1418878943 - collapse winter 2 & 3
#1419060032 - 3 looks appropriate
#1953212672 = collapse winter 2 & 3
#1953212678 = collapse winter 2 & 3
#1953212680 = collapse winter 2 & 3
#1953212691 = collapse winter 2 & 3

#ggplot(dplyr::filter(dat.wint, id %in% dat.3$id)) +
ggplot(dplyr::filter(dat.wint, id==1953212691)) +  
  geom_point(aes(x=lon, y=lat, colour=factor(winter))) +
  facet_wrap(~id, scales="free")

dat.tidy <- dat.wint %>% 
  mutate(winter = case_when(id=="973658514" & winter %in% c("winter2", "winter3") ~ "winter2",
                            id=="973658514" & winter=="winter4" ~ "winter3",
                            id=="973658514" & winter=="winter5" ~ "winter4",
                            id=="973658514" & winter=="winter6" ~ "wintermig",
                            id=="77637122" & winter=="winter3" ~ "winter2",
                            id=="1418878943" & winter=="winter3" ~ "winter2",
                            id=="1953212672" & winter=="winter3" ~ "winter2",
                            id=="1953212678" & winter=="winter3" ~ "winter2",
                            id=="1953212680" & winter=="winter3" ~ "winter2",
                            id=="1953212691" & winter=="winter3" ~ "winter2",
                            !is.na(winter) ~ winter)) %>% 
  dplyr::select(study, id, sensor, sex, mass, lat, lon, X, Y, year, date, doy, segment, season, stopover, stopovercluster, winter)

#11. Put back together with other data----
dat.all <- dat.raw %>% 
  dplyr::filter(season!="winter") %>% 
  mutate(winter = "other") %>% 
  dplyr::select(study, id, sensor, sex, mass, lat, lon, X, Y, year, date, doy, segment, season, stopover, stopovercluster, winter) %>% 
  rbind(dat.tidy) 

table(dat.all$season, dat.all$winter)

#12. Save----
write.csv(dat.all, "Data/LBCU_FilteredData_Segmented.csv", row.names = FALSE)
write.csv(dat.all, "/Users/ellyknight/Dropbox/LBCU/LBCU_FilteredData_Segmented.csv", row.names = FALSE)
write.csv(dat.all, "/Users/ellyknight/Documents/SMBC/Analysis/lbcu_mc/Data/LBCU_FilteredData_Segmented.csv", row.names = FALSE)
