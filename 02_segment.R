library(tidyverse)
library(lubridate)
library(adehabitatLT)
library(foieGras)
library(data.table)
library(rnaturalearth)
library(ggspatial)
library(sf)
library(moveHMM)

options(scipen = 999)

dat <- read.csv("Data/LBCUCleanedData.csv") %>% 
  arrange(id, datetime)

#1. Filter with foiegras----
#Wrangle
dat.fg <- dat.mig %>% 
  rename(date = datetime, lc = argos, lon = long) %>% 
  dplyr::select(id, date, lc, lon, lat, smaj, smin, eor)
#  dplyr::filter(id %in% c(1615638072, 46200872, 279282698, 890835056, 1546948337)) #just test a few birds first

#Fit
fit24 <- fit_ssm(dat.fg, 
                time.step = 24, 
                vmax = 35, 
                ang = c(5,15), 
                min.dt = 10, 
                model = "crw") 

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

saveRDS(fit24, file = "Foiegras24h_rw.Rdata")

#1. Segment with moveHMM----

#https://cran.r-project.org/web/packages/moveHMM/vignettes/moveHMM-guide.pdf

load("Foiegras24h_rw.Rdata")

#Dataframe of predictions
g = grab(fit24, what = "predicted", as_sf = FALSE) %>% 
  dplyr::select(id, date, lon, lat) %>% 
  rename(ID=id) %>% 
  data.frame() 

#Just try a few individuals first
#g <- g %>% 
#  dplyr::filter(ID %in% c(1615638072, 1425587959, 290353519, 145698291, 290351667))

#Prep for HMM
md <- prepData(g, coordNames = c("lon","lat"), type="LL")

## Visualize to choose initial parameters
plot(density(abs(na.omit(md$angle))))
plot(density(na.omit(md$step)), xlim=c(0,10))

#Starting values
# mean step lengths for state 1 (residency) and state 2 (migration)
mu0 <- c(1,75)
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
pred24$predictedState <- viterbi(m)
pred24$probState1 <- stateProbs(m)[,1]
pred24$probState2 <- stateProbs(m)[,2]

write.csv(pred24, "Data/LBCU_Filtered&SegmentedData.csv", row.names = FALSE)
