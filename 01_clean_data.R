library(tidyverse)
library(lubridate)
library(ggmap)
library(gtools)
library(readxl)
library(adehabitatLT)
library(data.table)

options(scipen=99999)

#1. Read in data----

#1a. Argos data----
raw.bc <- read.csv("Data/Movebank - BC LBCU tracking study.csv")
raw.tx <- read.csv("Data/Movebank - MCP Long-billed Curlews Texas Gulf Coast.csv")
raw.iw <- read.csv("Data/Movebank - Long-billed Curlew Migration from the Intermountain West.csv")
raw.wy <- read.csv("Data/Movebank - Long-billed Curlew - Western Wyoming.csv")
raw.usgs <- read.csv("Data/Movebank - Long-billed Curlew - USGS.csv")

#Mexico - tried doing this with a loop and smartbind, but didn't work
raw.mx.1 <- read_excel("Data/mx/Female_86872.xlsx")
raw.mx.2 <- read_excel("Data/mx/Female_33088.xlsx")
raw.mx.3 <- read_excel("Data/mx/Male_86873.xlsx")
raw.mx.4 <- read_excel("Data/mx/Male_33089.xlsx")
raw.mx.5 <- read_excel("Data/mx/Male_33091.xlsx")

#Lee tibbits/USGS
#raw.usgs <- read.csv("Data/longBilledCurlew_USGS_ASC_argos_diagTabular_gis.csv")

#Nebraska
raw.nb <- read_excel("Data/Long-billed_Curlew_Data_Nebraska_93033_93034.xlsx")

#1b. GPS data - has temp---
raw.lt <- read.csv("Data/LT May 2020 to July 2021.csv")
raw.ml <- read.csv("Data/ML May-Sep 2021.csv")
raw.3612 <- read.csv("Data/CTT#3612(May 2021-Feb 2022).csv")

#also has accelerometer, gyroscope, magnetometer
raw.mn <- read.csv("Data/Movebank - Long-billed Curlew full annual cycle movement ecology - Montana.csv")

#Metadata
ref.bc <- read.csv("Data/Movebank - BC LBCU tracking study - Metadata.csv") %>% 
  mutate(study = "BC")
ref.tx <- read.csv("Data/Movebank - MCP Long-billed Curlews Texas Gulf Coast - Metadata.csv") %>% 
  mutate(study = "TX")
ref.iw <- read.csv("Data/Movebank - Long-billed Curlew Migration from the Intermountain West - Metadata.csv") %>% 
  mutate(study = "IW")
ref.wy <- read.csv("Data/Movebank - Long-billed Curlew - Western Wyoming - Metadata.csv") %>% 
  mutate(study="WY")
ref.mn <- read.csv("Data/Movebank - Long-billed Curlew full annual cycle movement ecology - Montana - Metadata.csv") %>% 
  mutate(study = "MN")
ref.mx <- read.csv("Data/mx/LBCU_ReferenceData_Mexico.csv") %>% 
  mutate(study = "MX")
ref.usgs <- read.csv("Data/Movebank - Long-billed Curlew - USGS - Metadata.csv") %>% 
  mutate(study = "USGS")

#2. Clean each data source----
dat.bc <- raw.bc %>% 
  rename(datetime = timestamp, long = location.long, lat= location.lat, error = argos.error.radius, smaj = argos.semi.major, smin = argos.semi.minor, eor = argos.orientation, sensor = sensor.type, id = individual.id, tag = tag.id, argos = argos.lc) %>% 
  mutate(datetime = ymd_hms(datetime),
         study = "BC") %>% 
  dplyr::filter(algorithm.marked.outlier!="true") %>% 
  dplyr::select(id, tag, datetime, long, lat, argos, error, smaj, smin, eor, sensor, study) %>% 
  mutate(depseason = "breed")

dat.tx <- raw.tx %>% 
  dplyr::filter(algorithm.marked.outlier!="true") %>% 
  rename(datetime = timestamp, long = location.long, lat= location.lat, error = argos.error.radius, smaj = argos.semi.major, smin = argos.semi.minor, eor = argos.orientation, sensor = sensor.type, study = study.name, id = individual.id, tag = tag.id, argos = argos.lc) %>% 
  mutate(datetime = ymd_hms(datetime),
         study = "TX") %>% 
  dplyr::select(id, tag, datetime, long, lat, argos, error, smaj, smin, eor, sensor, study) %>% 
  mutate(depseason = "winter")

dat.iw <- raw.iw %>% 
  dplyr::filter(algorithm.marked.outlier!="true") %>% 
  dplyr::filter(sensor.type!="Accessory Measurements") %>% 
  rename(datetime = timestamp, long = location.long, lat= location.lat, error = argos.error.radius, smaj = argos.semi.major, smin = argos.semi.minor, eor = argos.orientation, sensor = sensor.type, study = study.name, id = individual.id, tag = tag.id, argos = argos.lc) %>% 
  mutate(datetime = ymd_hms(datetime),
         study = "IW") %>% 
  dplyr::select(id, tag, datetime, long, lat, argos, error, smaj, smin, eor, sensor, study) %>% 
  mutate(depseason = "breed")

dat.wy <- raw.wy %>% 
  dplyr::filter(algorithm.marked.outlier!="true") %>% 
  rename(datetime = timestamp, long = location.long, lat= location.lat, error = argos.error.radius, smaj = argos.semi.major, smin = argos.semi.minor, eor = argos.orientation, sensor = sensor.type, id = individual.id, tag = tag.id, argos = argos.lc) %>% 
  mutate(datetime = ymd_hms(datetime),
         study = "WY") %>% 
  dplyr::select(id, tag, datetime, long, lat, argos, error, smaj, smin, eor, sensor, study) %>% 
  mutate(depseason = "breed")

dat.usgs <- raw.usgs %>% 
  dplyr::filter(algorithm.marked.outlier!="true") %>% 
  rename(datetime = timestamp, long = location.long, lat= location.lat, error = argos.error.radius, smaj = argos.semi.major, smin = argos.semi.minor, eor = argos.orientation, sensor = sensor.type, id = individual.id, tag = tag.id, argos = argos.lc) %>% 
  mutate(datetime = ymd_hms(datetime),
         study = "USGS") %>% 
  dplyr::select(id, tag, datetime, long, lat, argos, error, smaj, smin, eor, sensor, study) %>% 
  mutate(depseason = "breed")

dat.nb <- raw.nb %>% 
  dplyr::filter(!is.na('Location class')) %>% 
  dplyr::rename(datetime = 'Location date', long = Longitude, lat = Latitude, id=PTT, argos='Location class') %>% 
  mutate(tag = id,
         error = NA,
         smaj= NA,
         smin = NA,
         eor = NA,
         sensor="Argos Doppler Shift",
         study = "NB") %>% 
  dplyr::select(id, tag, datetime, long, lat, argos, error, smaj, smin, eor, sensor, study) %>% 
  mutate(depseason = "breed") %>% 
  unique() %>% 
  arrange(id, datetime)
  
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
  dplyr::select(id, tag, datetime, long, lat, argos, error, smaj, smin, eor, sensor, study) %>% 
  mutate(depseason = "breed")

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
  dplyr::select(id, tag, datetime, long, lat, argos, error, smaj, smin, eor, sensor, study) %>% 
  mutate(depseason = "breed")

dat.3612 <- raw.lt %>% 
  rename(datetime = GPS_YYYY.MM.DD_HH.MM.SS, long=lon, tag = serial) %>% 
  dplyr::filter(fix==3) %>% 
  mutate(sensor="GPS",
         error = 10,
         smaj = NA, 
         smin = NA,
         eor = NA,
         argos = "G",
         study = "Jay", 
         id = 99902,
         datetime = mdy_hm(datetime)) %>% 
  dplyr::select(id, tag, datetime, long, lat, argos, error, smaj, smin, eor, sensor, study) %>% 
  mutate(depseason = "breed")

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
  dplyr::select(id, tag, datetime, long, lat, argos, error, smaj, smin, eor, sensor, study) %>% 
  mutate(depseason = "breed")

dat.mx.13 <- rbind(raw.mx.1, raw.mx.3) %>% 
  rename(id=ID, datetime = `Fecha de loc.`, long.dms = Longitud, lat.dms = Latitud, argos = `Calidad loc.`, error = `Rayo de error`, smaj = `Semieje mayor`, smin = `Semieje menor`, eor = `Angulo de la elipse`) %>% 
  separate(long.dms, into=c("long.d", "long.m", "long.s", "long.junk"), remove=FALSE) %>% 
  separate(lat.dms, into=c("lat.d", "lat.m", "lat.s", "lat.junk"), remove=FALSE) %>% 
  mutate(tag = paste0(id), 1,
         study = "MX",
         datetime = ymd_hms(datetime),
         sensor = "Argos Doppler Shift",
         lat = as.numeric(lat.d) + as.numeric(lat.m)/60 + as.numeric(lat.s)/3600,
         long = as.numeric(long.d) + as.numeric(long.m)/60 + as.numeric(long.s)/3600,
         long = -long) %>% 
  dplyr::select(id, tag, datetime, long, lat, argos, error, smaj, smin, eor, sensor, study)

dat.mx.24 <- rbind(raw.mx.2, raw.mx.4) %>% 
  rename(id = `N&deg; ID`, datetime = `Fecha de loc.`, long.dms = Longitud, lat.dms = Latitud, argos = `Calidad loc.`, error = `Rayo de error`, smaj = `Semieje mayor`, smin = `Semieje menor`, eor = `Angulo de la elipse`) %>% 
  separate(long.dms, into=c("long.d", "long.m", "long.s", "long.junk"), remove=FALSE) %>% 
  separate(lat.dms, into=c("lat.d", "lat.m", "lat.s", "lat.junk"), remove=FALSE) %>% 
  mutate(tag = paste0(id), 1,
         study = "MX",
         datetime = ymd_hms(datetime),
         sensor = "Argos Doppler Shift",
         lat = as.numeric(lat.d) + as.numeric(lat.m)/60 + as.numeric(lat.s)/3600,
         long = as.numeric(long.d) + as.numeric(long.m)/60 + as.numeric(long.s)/3600,
         long = -long) %>% 
  dplyr::select(id, tag, datetime, long, lat, argos, error, smaj, smin, eor, sensor, study)

dat.mx.5 <- raw.mx.5  %>% 
  rename(id = `N&deg; ID`, datetime = `Fecha de loc.`, long.dms = Longitud, lat.dms = Latitud, argos = `Calidad loc.`, error = `Rayo de error`, smaj = `Semieje mayor`, smin = `Semieje menor`, eor = `Angulo de la elipse`) %>% 
  separate(long.dms, into=c("long.d", "long.m", "long.s", "long.junk"), remove=FALSE) %>% 
  separate(lat.dms, into=c("lat.d", "lat.m", "lat.s", "lat.junk"), remove=FALSE) %>% 
  mutate(tag = paste0(id), 1,
         study = "MX",
         datetime = ymd_hms(datetime),
         sensor = "Argos Doppler Shift",
         lat = as.numeric(lat.d) + as.numeric(lat.m)/60 + as.numeric(lat.s)/3600,
         long = as.numeric(long.d) + as.numeric(long.m)/60 + as.numeric(long.s)/3600,
         long = -long) %>% 
  dplyr::select(id, tag, datetime, long, lat, argos, error, smaj, smin, eor, sensor, study)

dat.mx <- rbind(dat.mx.13, dat.mx.24, dat.mx.5) %>% 
  mutate(id = as.numeric(id),
         error = as.numeric(error),
         smaj = as.numeric(smaj),
         smin = as.numeric(smin),
         eor = as.numeric(eor))  %>% 
  mutate(depseason = "winter")

#dat.usgs <- raw.usgs %>%
#  dplyr::rename(id = Animal_ID, tag = PTT, argos = Location_Class, error = Error_Radius, smaj = #Semimajor_Axis, smin = Semiminor_Axis, eor = Ellipse_Orientation, long = Lon_Solution_1, lat = #Lat_Solution_1) %>% 
#  mutate(sensor = "Argos Doppler Shift",
#         study = "USGS",
#         datetime = ymd_hms(Location_Datetime_UTC)) %>% 
#  dplyr::select(id, tag, datetime, long, lat, argos, error, smaj, smin, eor, sensor, study) %>% 
#  mutate(depseason = "breed") %>% 
#  unique() %>% 
#  arrange(tag, datetime)

#3. Tidy metadata----
ref <- smartbind(ref.tx, ref.bc, ref.iw, ref.mn, ref.wy, ref.usgs) %>% 
  dplyr::rename(sex = animal_sex, id = animal_id, tag = tag_id, name = animal_local_identifier, on = deploy_on_timestamp, off = deploy_off_timestamp) %>% 
  dplyr::filter(!is.na(id)) %>% 
  unique() %>% 
  mutate(on = ymd_hms(on),
         off = ymd_hms(off)) %>% 
  smartbind(ref.mx %>% 
              mutate(on = dmy(on),
                     off = dmy(off),
                     tag = paste0(id, 1),
                     name = NA)) %>% 
  dplyr::select(study, sex, id, tag, name, on, off, mass)

#4. Put together----
dat.raw <- rbind(dat.bc, dat.iw, dat.lt, dat.ml, dat.3612, dat.mn, dat.tx, dat.mx, dat.nb, dat.wy, dat.usgs) %>% 
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
                long < -70,
                argos %in% c("0", "1", "2", "3", "G", "A", "B"))

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

#8. Visualize for truncation dates----
ids <- dat.traj %>% 
  dplyr::select(id) %>% 
  unique()

for(i in 1:nrow(ids)){
  dat.i <- dat.traj %>% 
    dplyr::filter(id==ids$id[i]) 
  plot.i <- ggplot(dat.i) +
    geom_point(aes(x=datetime, y=log(dist.dt), colour=factor(year)))
  
    ggsave(plot.i, file=paste0("Figures/R2n/", ids$id[i],".jpeg"), width=8, height=6)
  
  print(paste0("Finished plot ", i, " of ", nrow(ids), " birds"))
  
}

#10. Clean out some problematic points----
dat.clean <- dat.traj %>% 
  dplyr::filter(!(id==290350903 & long > -106),
                !(str_sub(id, 1, 5)=="14255" & doy==123 & year==2019),
                !(study=="NB" & argos %in% c("0", "1")),
                !(id==1378421381 & year==2020 & doy < 151),
                !(id==145698291 & year==2021 & doy > 129),
                !(id==290352179 & year==2021 & doy > 103),
                !(id==46768189),
                !(id %in% c(46769927, 46770723) & long > -100))
#46768189 only has points for one day
#145698291 2021 > doy 129 is likely dead
#290352179 died on migration, remove pts > doy 103 in 2021

#11. Save out----
write.csv(dat.clean, "Data/LBCUCleanedData.csv", row.names = FALSE)

#12. Number of birds----
length(unique(dat.clean$id))
#158