library(tidyverse)
library(lubridate)
library(changepoint)
library(data.table)

options(scipen = 99999)

#1. Read in data----
dat <- read.csv("Data/LBCUFiltered&Predicted&Legged&ClusteredData.csv")

#2. ID Birds that have multiple wintering grounds----
wint2 <- c("46757899-2015-2fall",
           "46757899-2016-2fall",
           "77635674-2015-2fall",
           "77636733-2015-2fall",
           "77636733-2016-2fall",
           "77636733-2017-2fall",
           "77637122-2015-2fall",
           "77637122-2016-2fall",
           "77637122-2017-2fall",
           "164383320-2016-2fall",
           "164384286-2016-2fall",
           "281980840-2019-2fall",
           "290350486-2017-2fall",
           "290350486-2018-2fall",
           "290350486-2019-2fall",
           "290350903-2017-2fall",
           "290351183-2017-2fall",
           "290351183-2018-2fall",
           "290351183-2019-2fall",
           "290351183-2020-2fall",
           "290351953-2019-2fall",
           "290352179-2017-2fall",
           "290352179-2018-2fall",
           "290352179-2020-2fall",
           "477992060-2018-2fall",
           "877974163-2019-2fall",
           "877979829-2019-2fall",
           "877979829-2020-2fall",
           "877984360-2019-2fall",
           "877984561-2019-2fall",
           "877984561-2020-2fall",
           "890830029-2019-2fall",
           "890830029-2020-2fall",
           "890830029-2021-2fall",
           "890834800-2019-2fall",
           "1146533212-2020-2fall",
           "1419060032-2020-2fall",
           "1425578518-2019-2fall",
           "1418923728-2020-2fall",
           "1425582803-2019-2fall",
           "1425587959-2019-2fall",
           "1425590049-2019-2fall",
           "1546947529-2021-2fall",
           "1613983438-2021-2fall",
           "1615638072-2021-2fall")

wint3 <- c("877984360-2020-2fall",
           "1418878943-2020-2fall",
           "1418896449-2020-2fall",
           "1418899142-2020-2fall",
           "1418899142-2021-2fall",
           "164384078-2016-2fall",
           "279818289-2017-2fall",
           "281981679-2017-2fall",
           "46769840-2015-2fall",
           "164383320-2017-2fall")

#3. Changepoint----
legs <- dat %>% 
  dplyr::select(id, legid) %>% 
  unique() %>% 
  mutate(ncps = case_when(legid %in% wint2 ~ 3,
                         legid %in% wint3 ~ 4),
         ncps = ifelse(is.na(ncps), 2, ncps))

cp.list <- list()
for(i in 1:nrow(legs)){
  
  dat.i <- dat %>% 
    dplyr::filter(legid==legs$legid[i])
  
  cp.i <- cpt.mean(dat.i$nsd, method="BinSeg", Q=legs$ncps[i])
  
  cp.list[[i]] <- data.frame(order = cp.i@cpts,
                             cp.nsd = cp.i@param.est$mean,
                             ncps = legs$ncps[i],
                             legid=legs$legid[i]) %>% 
    mutate(cp = row_number())
  
}

cp <- rbindlist(cp.list)

#4. Put together----
dat.cp <- dat %>% 
  left_join(cp) %>% 
  group_by(legid) %>% 
  fill(cp, .direction="up") %>% 
  fill(cp.nsd, .direction="up") %>% 
  fill(ncps, .direction="up") %>% 
  arrange(id, date) %>% 
  ungroup()

#5. Save----
write.csv(dat.cp, "Data/LBCUFiltered&Predicted&Legged&Clustered&CPedData.csv", row.names = FALSE)

#6. Check # of birds----
length(unique(dat.cp$id))
#122