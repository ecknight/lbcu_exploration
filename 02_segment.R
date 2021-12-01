library(tidyverse)
library(lubridate)
library(foieGras)
library(data.table)
library(rnaturalearth)
library(ggspatial)
library(sf)
library(moveHMM)
library(bayesmove)

options(scipen = 999)

dat <- read.csv("Data/LBCUCleanedData.csv") %>% 
  arrange(id, datetime)

#TO DO: REMOVE POINTS FROM BIG BREAKS IN TAG TRANSMISSION FROM FOIEGRAS RESULTS####
#46200880
#  mutate(id = case_when(id==172070515 & datetime <= "2017-02-19 11:18:29" ~ paste0(id, 1),
#                         id==172070515 & datetime > "2017-02-19 11:18:29" ~ paste0(id, 2),
#                         id==46768108 & datetime <= "2015-03-11 01:07:49" ~ paste0(id, 1),
#                         id==46768108 & datetime > "2015-03-11 01:07:49" ~ paste0(id, 2),
#                         id==46769588 & datetime <= "2019-03-14 04:55:49" ~ paste0(id, 1),
#                         id==46769588 & datetime > "2019-03-14 04:55:49" ~ paste0(id, 2),
#                        !is.na(id) ~ id))

#TO DO: PULL PTS FOR SPECIFIC BIRDS BEFORE FOIEGRAS####
#172070319
#290350903 in 2017
#290352179 in 2020
#77638376
#Birds that only have 2021 data

#TO DO: ID BIRDS WITH 2 WINTERING GROUNDS MANUALLY####

#TO DO: CLEAN MANUALLY####

#TO DO: SEPARATE SPRING & FALL MIGRATION


#1. Wrangle----
dat.mig <- dat %>% 
  dplyr::filter(!(is.na(smaj) & sensor=="Argos Doppler Shift"),
                mig==1)

#2. Filter with foiegras----
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

saveRDS(fit24, file = "Foiegras24h_rw.Rdata")

#3. Segment with moveHMM----

#https://cran.r-project.org/web/packages/moveHMM/vignettes/moveHMM-guide.pdf

#load("Foiegras24h_rw.Rdata")

#Dataframe of predictions
g = grab(fit24, what = "predicted", as_sf = FALSE) %>% 
  mutate(doy = yday(date)) %>% 
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
plot(m, plotCI = TRUE, plotTracks=FALSE)

#Add states to data
pred24$predictedState <- viterbi(m)
pred24$probState1 <- stateProbs(m)[,1]
pred24$probState2 <- stateProbs(m)[,2]

write.csv(pred24, "Data/LBCU_Filtered&PredictedData_HMM.csv", row.names = FALSE)

#Inspect per bird----
ids <- pred24 %>% 
  dplyr::select(id) %>% 
  unique()

for(i in 1:nrow(ids)){
  pred.i <- pred24 %>% 
    dplyr::filter(id==ids$id[i]) %>% 
    mutate(doy = yday(date),
           year = year(date))
  plot.i <- ggplot(pred.i) +
    geom_path(aes(x=lon, y=lat)) +
    geom_point(aes(x=lon, y=lat, colour=factor(predictedState)), alpha = 0.5, size=3) +
#    scale_colour_viridis_c() +
    scale_colour_manual(values=c("orange", "blue"), name="State") +
    facet_wrap(~year)
  
    ggsave(plot.i, file=paste0("Figures/HMM/", ids$id[i],".jpeg"), width=8, height=6)
  
  print(paste0("Finished plot ", i, " of ", nrow(ids), " birds"))
  
}


#4. Bayesmove without segmentation----

#Convert data to dataframe
tracks2 <- rbindlist(tracks.list2) %>% 
  dplyr::select(SL, TA)

set.seed(1)

# Define model params
alpha=0.1  #prior
ngibbs=10000  #number of Gibbs sampler iterations
nburn=ngibbs/2  #number of burn-in iterations
nmaxclust=2  #number of maximum possible states (clusters) present

# Run model
dat.res2 <- cluster_obs(dat=tracks2, alpha=alpha, ngibbs=ngibbs, nmaxclust=nmaxclust,
                      nburn=nburn)

# Inspect traceplot of log-likelihood
plot(dat.res2$loglikel, type = "l")

## Inspect and Plot results
post.seq <- (nburn + 1):ngibbs  #posterior samples

theta<- dat.res2$theta[post.seq,]
colnames(theta)<- 1:ncol(theta)
theta1<- colMeans(theta)
theta1<- sort(theta1, decreasing = TRUE)
cumsum(theta1)

# Extract bin estimates for each possible state from the `phi` matrix of the model results
behav.res<- get_behav_hist(dat = dat.res2, nburn = nburn, ngibbs = ngibbs, nmaxclust = nmaxclust,
                           var.names = c("Step Length","Turning Angle"))
behav.res$behav<- factor(behav.res$behav, levels = 1:nmaxclust)

# Plot state-dependent distributions 
ggplot(behav.res, aes(x = bin, y = prop, fill = as.factor(behav))) +
  geom_bar(stat = 'identity') +
  labs(x = "\nBin", y = "Proportion\n") +
  theme_bw() +
  theme(axis.title = element_text(size = 16),
        axis.text.y = element_text(size = 14),
        axis.text.x.bottom = element_text(size = 12),
        strip.text = element_text(size = 14),
        strip.text.x = element_text(face = "bold")) +
  scale_fill_manual(values = c(viridis::viridis(2), rep("grey35", 8)), guide = FALSE) +
  scale_y_continuous(breaks = c(0.00, 0.50, 1.00)) +
  scale_x_continuous(breaks = 1:8) +
  facet_grid(behav ~ var, scales = "free_x")

## Attribute behaviors to states and extract each of the different estimates
# Using MAP estimate, threshold of 75% assignments from posterior, and most common state
z.post<- as.matrix(dat.res2$z.posterior)
z.post2<- t(apply(z.post, 1, function(x) x/sum(x)))
thresh<- 0.75
z.post3<- apply(z.post2, 1, function(x) ifelse(max(x) > thresh, which(x > thresh), NA))
z.post4<- apply(z.post2, 1, function(x) which.max(x))

## Add states to data frame
states <- rbindlist(tracks.list) %>% 
  mutate(z.map = dat.res2$z.MAP,
         z.post.thresh = z.post3,
         z.post.max = z.post4)

n.states<- 2
states$z.map<- ifelse(states$z.map > n.states, NA, states$z.map)
states$z.post.max<- ifelse(states$z.post.max > n.states, NA, states$z.post.max)

# Assign names to states
states2<- states %>% 
  mutate(across(c('z.map','z.post.thresh','z.post.max'),
                ~case_when(. == 1 ~ "Stationary",
                           . == 2 ~ "Migration",
                           is.na(.) ~ "Unclassified")
  )) %>% 
  mutate(across(c('z.map','z.post.thresh','z.post.max'),
                factor, levels = c('Stationary','Migration','Unclassified')
  ))

table(states2$z.map)
table(states2$z.post.max)
table(states2$z.post.thresh)

#Inspect per bird
ids <- states2 %>% 
  dplyr::select(id) %>% 
  unique()

for(i in 1:nrow(ids)){
  pred.i <- states2 %>% 
    dplyr::filter(id==ids$id[i]) %>% 
    mutate(doy = yday(date),
           year = year(date))
  plot.i <- ggplot(pred.i) +
    geom_path(aes(x=x, y=y)) +
    geom_point(aes(x=x, y=y, colour=z.map), alpha = 0.5, size=3) +
    #    scale_colour_viridis_c() +
    scale_colour_manual(values=c("orange", "blue"), name="State")
#    facet_wrap(~year)
  
  ggsave(plot.i, file=paste0("Figures/Bayesmove/", ids$id[i],".jpeg"), width=8, height=6)
  
  print(paste0("Finished plot ", i, " of ", nrow(ids), " birds"))
  
}

#6. And then clustering----
library(meanShiftR)

#load("Foiegras24h_rw.Rdata")

#Dataframe of predictions
g = grab(fit24, what = "predicted", as_sf = FALSE) %>% 
  dplyr::select(id, date, lon, lat) %>% 
  data.frame() %>% 
  mutate(year = year(date))

g.utm <- g %>% 
  data.frame() %>% 
  st_as_sf(coords=c("lon", "lat"), crs=4326) %>% 
  st_transform(crs=3857) %>% 
  st_coordinates %>% 
  cbind(g) %>% 
  dplyr::select(id, year, date, X, Y)

loop <- g.utm %>% 
  dplyr::select(id, year) %>% 
  unique()

dat.shift <- data.frame()
for(i in 1:nrow(loop)){
  dat.i <- g.utm %>% 
    dplyr::filter(year==loop$year[i],
                  id==loop$id[i])
  
  mat1 <- matrix(dat.i$X)
  mat2 <- matrix(dat.i$Y)
  mat <- cbind(mat1, mat2)
  
  shift <- meanShift(mat,
                     algorithm="KDTREE",
                     bandwidth=c(1,1))
  
  dat.shift <- dat.i %>% 
    mutate(cluster = shift[[1]]) %>% 
    rbind(dat.shift)
  
  
}

dat.shift.n <- dat.shift %>% 
  dplyr::select(-X, -Y) %>% 
  group_by(id, year, cluster) %>% 
  mutate(n = n()) %>% 
  ungroup()

#7. Put it all together----
pred <- pred24 %>% 
  dplyr::select(-x, -y) %>% 
  full_join(states2) %>% 
  full_join(dat.shift.n) %>% 
  mutate(predictedState = ifelse(predictedState == 1, "stationary", "migration"))

write.csv(pred, "Data/LBCU_Filtered&PredictedData_hmm_movebyes_cluster.csv", row.names=FALSE)
  
#8. Visualize----
pred.categories <- data.frame(table(pred$id, pred$predictedState, pred$n)) %>% 
  rename(id = Var1, state = Var2, n = Var3) %>% 
  dplyr::filter(Freq > 0) %>% 
  mutate(n=as.numeric(n))

ggplot(pred.categories) +
  geom_jitter(aes(x=state, y=n))

for(i in 1:nrow(ids)){
  pred.i <- pred %>% 
    dplyr::filter(id==ids$id[i]) %>% 
    mutate(doy = yday(date),
           year = year(date))
  plot.i <- ggplot(pred.i) +
    geom_path(aes(x=lon, y=lat)) +
    geom_point(aes(x=lon, y=lat, colour=log(n)), alpha = 0.5, size=3) +
        scale_colour_viridis_c() +
#    scale_colour_manual(values=c("orange", "blue"), name="State")
      facet_grid(predictedState~year)
  
  ggsave(plot.i, file=paste0("Figures/Cluster/", ids$id[i],".jpeg"), width=8, height=6)
  
  print(paste0("Finished plot ", i, " of ", nrow(ids), " birds"))
  
}

#Clustering doesn't work for stopovers, only for breed/winter

#9. Classify to season----
pred <- read.csv("Data/LBCU_Filtered&PredictedData_hmm_movebyes_cluster.csv")

table(pred$z.map)
table(pred$z.post.max)
table(pred$z.post.thresh)
table(pred$predictedState)
table(pred$z.map, pred$z.post.max, pred$predictedState)

pred.breed <-  pred %>% 
  dplyr::filter(predictedState=="stationary") %>% 
  group_by(id, year) %>% 
  summarize(lat = max(lat)) %>% 
  ungroup() %>% 
  left_join(pred)  %>% 
  mutate(season="breed") %>% 
  dplyr::select(id, year, cluster, season)

pred.winter <- pred %>% 
  dplyr::filter(predictedState=="stationary") %>% 
  group_by(id, year) %>% 
  summarize(lat = min(lat)) %>% 
  ungroup() %>% 
  left_join(pred)  %>% 
  mutate(season="winter") %>% 
  dplyr::select(id, year, cluster, season)

pred.season <- pred %>% 
  left_join(rbind(pred.breed, pred.winter)) %>% 
  arrange(id, date) %>% 
  mutate(doy = yday(date)) %>% 
  mutate(season = case_when(!is.na(season) ~ season,
                            predictedState=="stationary" ~ "stopover",
                            n==1 ~ "migration",
                            predictedState == "migration" & z.map == "Migration" ~ "migration",
                            predictedState == "migration" & z.map=="Stationary" & z.post.max=="Stationary" ~ "stopover")) %>% 
  dplyr::filter(!(id==172070515 & year==2017 & doy > 50 & doy < 145),
                !(id==46768108 & year==2015 & doy > 70 & doy < 94),
                !(id==46769588 & year==2019 & doy > 73 & doy < 87),
                !(id==973657763 & year==2020 & doy > 74 & doy < 80),
                !(id==973657763 & year==2020 & doy > 99 & doy < 104))

#Troubleshooting
table(pred.season$season)

pred.na <- pred.season %>% 
  dplyr::filter(is.na(season))

ggplot(pred.season) +
  geom_point(aes(x=lon, y=lat, colour=season))

ggplot(pred.na) +
  geom_point(aes(x=lon, y=lat, colour=n)) +
  facet_grid(predictedState ~ z.map) +
  scale_colour_viridis_c()

table(pred.na$n, pred.na$z.map)

for(i in 1:nrow(ids)){
  pred.i <- pred.season %>% 
    dplyr::filter(id==ids$id[i]) %>% 
    mutate(doy = yday(date),
           year = year(date))
  plot.i <- ggplot(pred.i) +
    geom_path(aes(x=lon, y=lat)) +
    geom_point(aes(x=lon, y=lat, colour=season), alpha = 0.5, size=3) +
#    scale_colour_viridis_d() +
    facet_wrap(~year)
  
  ggsave(plot.i, file=paste0("Figures/Season/", ids$id[i],".jpeg"), width=8, height=6)
  
  print(paste0("Finished plot ", i, " of ", nrow(ids), " birds"))
  
}

write.csv(pred.season, "Data/LBCU_Filtered&Predicted&SegmentedData.csv", row.names = FALSE)
