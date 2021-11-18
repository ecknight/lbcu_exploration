library(tidyverse)
library(lubridate)
library(ggmap)
library(gtools)
library(readxl)

options(scipen=99999)

#TO DO: STILL NEED TO ADD MEXICO DATA####

#1. Read in data----

#1a. Argos data----
raw.bc <- read.csv("Data/Movebank - BC LBCU tracking study.csv")
raw.tx <- read.csv("Data/Movebank - MCP Long-billed Curlews Texas Gulf Coast.csv")
raw.iw <- read.csv("Data/Movebank - Long-billed Curlew Migration from the Intermountain West.csv")

#Mexico - tried doing this with a loop and smartbind, but didn't work
raw.mx.1 <- read_excel("Data/mx/Female_86872.xlsx")
raw.mx.2 <- read_excel("Data/mx/Female_33088.xlsx")
raw.mx.3 <- read_excel("Data/mx/Male_86873.xlsx")
raw.mx.4 <- read_excel("Data/mx/Male_33089.xlsx")
raw.mx.5 <- read_excel("Data/mx/Male_33091.xlsx")

#1b. GPS data - has temp---
raw.lt <- read.csv("Data/LT May 2020 to July 2021.csv")
raw.ml <- read.csv("Data/ML May-Sep 2021.csv")

#also has accelerometer, gyroscope, magnetometer
raw.mn <- read.csv("Data/Movebank - Long-billed Curlew full annual cycle movement ecology - Montana.csv")

#Metadata
ref.bc <- read.csv("Data/Movebank - BC LBCU tracking study - Metadata.csv") %>% 
  mutate(study = "BC")
ref.tx <- read.csv("Data/Movebank - MCP Long-billed Curlews Texas Gulf Coast - Metadata.csv") %>% 
  mutate(study = "TX")
ref.iw <- read.csv("Data/Movebank - Long-billed Curlew Migration from the Intermountain West - Metadata.csv") %>% 
  mutate(study = "IW")
ref.mn <- read.csv("Data/Movebank - Long-billed Curlew full annual cycle movement ecology - Montana - Metadata.csv") %>% 
  mutate(study = "MN")
ref.mx <- read.csv("Data/mx/LBCU_ReferenceData_Mexico.csv") %>% 
  mutate(study = "MX")

#2. Clean each data source----
dat.bc <- raw.bc %>% 
  rename(datetime = timestamp, long = location.long, lat= location.lat, error = argos.error.radius, smaj = argos.semi.major, smin = argos.semi.minor, eor = argos.orientation, sensor = sensor.type, id = individual.id, tag = tag.id, argos = argos.lc) %>% 
  mutate(datetime = ymd_hms(datetime),
         study = "BC") %>% 
  dplyr::filter(algorithm.marked.outlier!="true") %>% 
  dplyr::select(id, tag, datetime, long, lat, argos, error, smaj, smin, eor, sensor, study)

dat.tx <- raw.tx %>% 
  dplyr::filter(algorithm.marked.outlier!="true") %>% 
  rename(datetime = timestamp, long = location.long, lat= location.lat, error = argos.error.radius, smaj = argos.semi.major, smin = argos.semi.minor, eor = argos.orientation, sensor = sensor.type, study = study.name, id = individual.id, tag = tag.id, argos = argos.lc) %>% 
  mutate(datetime = ymd_hms(datetime),
         study = "TX") %>% 
  dplyr::select(id, tag, datetime, long, lat, argos, error, smaj, smin, eor, sensor, study)

dat.iw <- raw.iw %>% 
  dplyr::filter(algorithm.marked.outlier!="true") %>% 
  dplyr::filter(sensor.type!="Accessory Measurements") %>% 
  rename(datetime = timestamp, long = location.long, lat= location.lat, error = argos.error.radius, smaj = argos.semi.major, smin = argos.semi.minor, eor = argos.orientation, sensor = sensor.type, study = study.name, id = individual.id, tag = tag.id, argos = argos.lc) %>% 
  mutate(datetime = ymd_hms(datetime),
         study = "IW") %>% 
  dplyr::select(id, tag, datetime, long, lat, argos, error, smaj, smin, eor, sensor, study)

dat.lt <- raw.lt %>% 
  rename(datetime = GPS_YYYY.MM.DD_HH.MM.SS, long=lon, tag = serial) %>% 
  dplyr::filter(fix==3) %>% 
  mutate(sensor="GPS",
         error = 10,
         smaj = NA, 
         smin = NA,
         eor = NA,
         argos = "G",
         study = "Jay", 
         id = 99900,
         datetime = mdy_hm(datetime)) %>% 
  dplyr::select(id, tag, datetime, long, lat, argos, error, smaj, smin, eor, sensor, study)

dat.ml <- raw.ml %>% 
  rename(datetime = Fix.Time, long = Long., lat = Lat.) %>% 
  dplyr::filter(Error==0) %>% 
  mutate(id=99901,
         tag = 999,
         error = 10,
         smaj = NA, 
         smin = NA,
         eor = NA,
         argos = "G",
         sensor="GPS",
         study="Jay",
         datetime = ymd_hms(datetime)) %>% 
  dplyr::select(id, tag, datetime, long, lat, argos, error, smaj, smin, eor, sensor, study)

dat.mn <- raw.mn %>% 
  dplyr::filter(sensor.type=="GPS") %>% 
  rename(datetime = timestamp, long = location.long, lat= location.lat, sensor = sensor.type, study = study.name, id = individual.id, tag = tag.id) %>% 
  mutate(error = 10,
         smaj = NA, 
         smin = NA,
         eor = NA,
         argos = "G",
         datetime = ymd_hms(datetime),
         study = "MN") %>% 
  dplyr::select(id, tag, datetime, long, lat, argos, error, smaj, smin, eor, sensor, study)

dat.mx <- raw.mx %>% 
  rename(id = ID, lat = Latitud, long = Longitud, )

#3. Tidy metadata----
ref <- smartbind(ref.tx, ref.bc, ref.iw, ref.mn) %>% 
  dplyr::rename(sex = animal_sex, id = animal_id, tag = tag_id, name = animal_local_identifier, on = deploy_on_timestamp, off = deploy_off_timestamp) %>% 
  dplyr::select(study, sex, id, tag, name, on, off) %>% 
  dplyr::filter(!is.na(id)) %>% 
  unique() %>% 
  mutate(on = ymd_hms(on),
         off = ymd_hms(off))

#4. Put together----
dat.raw <- rbind(dat.bc, dat.iw, dat.lt, dat.ml, dat.mn, dat.tx) %>% 
  left_join(ref) %>% 
  mutate(year = year(datetime),
         doy = yday(datetime),
         hour = hour(datetime),
         id.year = paste0(id, "-", year)) %>% 
  arrange(id, datetime) %>% 
  unique()

#5. Filtering----
dat1 <- dat.raw %>% 
  dplyr::filter(datetime < off, 
                datetime > on) %>% 
  rbind(dat.raw %>% 
          dplyr::filter(is.na(off))) %>% 
  dplyr::filter(!is.na(lat),
                error < 10000,
                long < -70,
                !(str_sub(id, 1, 5)=="14255" & doy==123 & year==2019))

#6. Remove duplicate timestamps----
date.freq <- data.frame(table(dat1$datetime, dat1$id)) %>% 
  dplyr::filter(Freq > 1) %>% 
  rename(datetime = Var1, id = Var2) %>% 
  mutate(datetime = ymd_hms(as.character(datetime)),
         id = as.numeric(as.character(id))) %>% 
  left_join(dat1) %>% 
  group_by(id, datetime) %>% 
  sample_n(1) %>% 
  ungroup %>% 
  dplyr::select(-Freq)

dat2 <- dat1 %>% 
  anti_join(date.freq)

#7. Add step characteristics----
#Wrangle for as.traj function
locs <- dat2 %>% 
  mutate(tag.id = paste0(tag, "-", id)) %>% 
  dplyr::select(long, lat, datetime, tag.id) %>% 
  mutate(datetime=ymd_hms(datetime)) %>% 
  unique()

#Use as.traj function to get NSD
traj <- as.ltraj(xy=locs[,c("long", "lat")],
                 id=locs$tag.id,
                 date=locs$datetime,
                 proj4string = CRS("+proj=longlat +datum=WGS84"))

#Put back together with data
dat.traj <- rbindlist(traj) %>% 
  dplyr::select(dx, dy, dist, dt, R2n, abs.angle, rel.angle) %>% 
  cbind(dat2) %>% 
  mutate(dist.log = log(dist),
         dist.dt = dist/dt)

#9. Visualize for truncation dates---
ids <- dat.traj %>% 
  dplyr::select(id) %>% 
  unique()

for(i in 1:nrow(ids)){
  dat.i <- dat.traj %>% 
    dplyr::filter(id==ids$id[i]) 
  plot.i <- ggplot(dat.i) +
    geom_point(aes(x=datetime, y=log(dist.dt), colour=factor(year)))
  
  #  ggsave(plot.i, file=paste0("Figures/R2n/", ids$id[i],".jpeg"), width=8, height=6)
  
  print(paste0("Finished plot ", i, " of ", nrow(ids), " birds"))
  
}

#10. Identify individuals that didn't migrate----
dat.mig <- dat.traj %>% 
  mutate(mig = ifelse(id %in% c(77637912, 279278554, 279287739, 1425586836, 1425591196), 0, 1))

#10. Save out

write.csv(dat.mig, "Data/LBCUCleanedData.csv", row.names = FALSE)

