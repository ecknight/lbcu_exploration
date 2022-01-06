library(tidyverse)
library(lubridate)
library(moveHMM)
library(meanShiftR)

options(scipen = 99999)

#1. Read in & wrangle----
dat.raw <- read.csv("Data/LBCUFiltered&Predicted&Legged&Seasoned&StopoverData.csv") %>% 
  mutate(date = ymd_hms(date))

dat <- dat.raw %>% 
  dplyr::filter(season=="winter") %>% 
  mutate(winteryear = ifelse(doy < 135, year-1, year),
         winterid = paste0(id, "-", winteryear))

#2. Prep for HMM----
dat.ll <- dat %>% 
  arrange(winterid, date) %>% 
  dplyr::select(winterid, lon, lat) %>% 
  rename(ID=winterid)

md <- prepData(dat.ll, coordNames = c("lon","lat"), type="LL")

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

#10. Assess birds with multiple home ranges----
dat.n <- dat.wint %>% 
  dplyr::select(id, wintern) %>% 
  unique()

table(dat.n$wintern)
  
#Look at birds with three home ranges
dat.3 <- dat.wint %>% 
  dplyr::filter(wintern=="3") %>% 
  dplyr::select(id) %>% 
  unique()
#973658514 - should perhaps collapse winter1, winter2, & wintermig
#1418878943 - should perhaps collapse winter 1 & 2
#1419060032 - 3 looks appropriate
#279818280 - collapse winter 2 & wintermig
#1615638072 - 3 loops appropriate

dat.tidy <- dat.wint %>% 
  mutate(winter = case_when(id=="973658514" & winter %in% c("winter1", "winter2", "wintermig") ~ "winter1",
                            id=="973658514" & winter=="winter3" ~ "winter2",
                            id=="1418878943" & winter %in% c("winter1", "winter2") ~ "winter1",
                            id=="1418878943" & winter=="winter3" ~ "winter2",
                            id=="279818280" & winter %in% c("winter2", "wintermig") ~ "winter2",
                            !is.na(winter) ~ winter)) %>% 
  dplyr::select(study, id, sensor, sex, mass, lat, lon, X, Y, year, date, doy, segment, season, stopover, winter)

#11. Put back together with other data----
dat.all <- dat.raw %>% 
  dplyr::filter(season!="winter") %>% 
  mutate(winter = "other") %>% 
  dplyr::select(study, id, sensor, sex, mass, lat, lon, X, Y, year, date, doy, segment, season, stopover, winter) %>% 
  rbind(dat.tidy) 

table(dat.all$season, dat.all$winter)

#12. Save----
write.csv(dat.all, "Data/LBCU_FilteredData_Segmented.csv", row.names = FALSE)
write.csv(dat.all, "/Users/ellyknight/Dropbox/LBCU/LBCU_FilteredData_Segmented.csv", row.names = FALSE)
