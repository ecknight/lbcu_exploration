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

#NOT SOLD ON THIS APPROACH YET BUT FINE FOR DATA EXPLORATION####

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

#2. Segment with moveHMM----

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
plot(m, plotCI = TRUE, plotTracks=TRUE)

#Add states to data
pred24$predictedState <- viterbi(m)
pred24$probState1 <- stateProbs(m)[,1]
pred24$probState2 <- stateProbs(m)[,2]

write.csv(pred24, "Data/LBCU_Filtered&PredictedData.csv", row.names = FALSE)

#3. Classify to season----
pred <- read.csv("Data/LBCU_Filtered&PredictedData.csv") %>% 
  rename(state = predictedState,
         long = lon) %>% 
  mutate(latr = round(lat),
         datetime = ymd_hms(date),
         year = year(date))

stationary <- pred %>% 
  dplyr::filter(state==1) %>% 
  dplyr::select(id, year, latr) %>% 
  unique()

breedwint <- stationary %>% 
  group_by(id) %>% 
  summarize(latb = max(latr),
            latw = min(latr)) %>% 
  ungroup()

pred.season <- pred %>% 
  left_join(breedwint) %>% 
  mutate(season = case_when(latr<latb & latr>latw & state==2 ~ "migration",
                            latr<latb & latr>latw & state==1 ~ "stopover",
                            latr==latw | latr<latw ~ "winter",
                            latr==latb | latr>latb ~ "breed"),
         season = case_when(id==77638376 & latr >= 44 ~ "breed",
                            id==77638376 & latr < 44 ~ "migration",
                            id==172070319 & latr==45 ~ "breed",
                            id==172070319 & latr < 45 ~ "migration",
                            !is.na(season) ~ season),
         season = case_when(season=="migration" & doy < 150 ~ "spring migration",
                            season=="migration" & doy > 150 ~ "fall migration",
                            !is.na(season) ~ season))

#4. Inspect segmentation----
table(pred.season$id, pred.season$season)

plot.season <- ggplot(pred.season) +
  geom_point(aes(x=long, y=lat, colour=factor(id)), show.legend=FALSE) +
  facet_wrap(~season)

plot.id <- ggplot(pred.season) +
  geom_point(aes(x=long, y=lat, colour=factor(season))) +
  facet_wrap(~id, ncol=20)

ggsave(plot.id, file="Figures/Season_ID.jpeg", width=20, height=10, units="in")

plot.id.breed <- ggplot(pred.season %>% dplyr::filter(season=="breed")) +
  geom_point(aes(x=long, y=lat, colour=factor(year))) +
  facet_wrap(~id, ncol=20, scales="free")

ggsave(plot.id.breed, file="Figures/Season_ID_breed.jpeg", width=20, height=10, units="in")

plot.id.winter <- ggplot(pred.season %>% dplyr::filter(season=="winter")) +
  geom_point(aes(x=long, y=lat, colour=factor(year))) +
  facet_wrap(~id, ncol=20, scales="free")

ggsave(plot.id.winter, file="Figures/Season_ID_winter.jpeg", width=20, height=10, units="in")

plot.id.springmig <- ggplot(pred.season %>% dplyr::filter(season=="spring migration")) +
  geom_point(aes(x=long, y=lat, colour=factor(year))) +
  facet_wrap(~id, ncol=20, scales="free")

ggsave(plot.id.springmig, file="Figures/Season_ID_springmigration.jpeg", width=20, height=10, units="in")

plot.id.fallmig <- ggplot(pred.season %>% dplyr::filter(season=="fall migration")) +
  geom_point(aes(x=long, y=lat, colour=factor(year))) +
  facet_wrap(~id, ncol=20, scales="free")

ggsave(plot.id.fallmig, file="Figures/Season_ID_fallmigration.jpeg", width=20, height=10, units="in")

#5. Save out----
write.csv(pred.season, "Data/LBCU_Filtered&Predicted&SegmentedData.csv", row.names = FALSE)
