library(tidyverse)
library(lubridate)
library(meanShiftR)

options(scipen = 99999)

#1. Read in data----
dat <- read.csv("Data/LBCUFiltered&Predicted&LeggedData.csv")

#2. Set up loop----
ids <- unique(dat$legid)

dat.shift <- data.frame()
for(i in 1:length(ids)){
  dat.i <- dat %>% 
    dplyr::filter(legid==ids[i])
  
  mat1 <- matrix(dat.i$X)
  mat2 <- matrix(dat.i$Y)
  mat <- cbind(mat1, mat2)
  
  #3. Cluster----
  shift <- meanShift(mat,
                     algorithm="KDTREE",
                     bandwidth=c(1,1))
  
  dat.shift <- dat.i %>% 
    mutate(cluster = shift[[1]]) %>% 
    rbind(dat.shift)
  
  
}

#4. Count points per cluster----
dat.shift.n <- dat.shift %>% 
  group_by(legid, cluster) %>% 
  mutate(clustern = n()) %>% 
  ungroup()

#5. Save out----
write.csv(dat.shift.n, "Data/LBCUFiltered&Predicted&Legged&ClusteredData.csv", row.names=FALSE)

#6. Check # of birds----
length(unique(dat.shift.n$id))
#122 - good