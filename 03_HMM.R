library(tidyverse)
library(lubridate)
library(data.table)
library(sf)
library(moveHMM)

options(scipen = 999)

dat <- read.csv("Data/LBCUFilteredData.csv") %>% 
  mutate(date = ymd_hms(date),
         year = year(date),
         doy = yday(date))

#1. Prep for HMM---
md <- prepData(dat, coordNames = c("lon","lat"), type="LL")

## Visualize to choose initial parameters
plot(density(abs(na.omit(md$angle))))
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

#Fit model
m <- fitHMM(data=md, 
            nbStates = 2, 
            stepPar0 = stepPar0, 
            anglePar0 = anglePar0, 
            formula = ~1)

#Visualize
plot(m, plotCI = TRUE, plotTracks=FALSE)

#Add states to data
dat$predictedState <- viterbi(m)
dat$probState1 <- stateProbs(m)[,1]
dat$probState2 <- stateProbs(m)[,2]

write.csv(dat, "Data/LBCUFiltered&PredictedData.csv", row.names = FALSE)
