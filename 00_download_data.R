library(tidyverse)
library(lubridate)
library(move)

#https://cran.r-project.org/web/packages/move/vignettes/browseMovebank.html

#1. Login----
loginStored <- movebankLogin(username="MCP", password="")

#2. Search & get study ids----
searchMovebankStudies(x="Long-billed Curlew", login=loginStored)

id.tx <- getMovebankID("MCP Long-billed Curlews Texas Gulf Coast", login=loginStored)
id.iw <- getMovebankID("Long-billed Curlew Migration from the Intermountain West", login=loginStored)
id.mn <- getMovebankID("Long-billed Curlew (Numenius americanus) full annual cycle movement ecology - A.Boyce/SCBI (Montana, USA)", login=loginStored)
id.wy <- getMovebankID("Long-billed Curlew - Western Wyoming", login=loginStored)

searchMovebankStudies(x="LBCU", login=loginStored)

id.bc <- getMovebankID("BC LBCU tracking study", login=loginStored)

#3. Get sensor ids----
sens.tx <- getMovebankSensors(id.tx, login=loginStored) %>% 
  mutate(project="tx")
sens.iw <- getMovebankSensors(id.iw, login=loginStored) %>% 
  mutate(project="iw")
sens.mn <- getMovebankSensors(id.mn, login=loginStored) %>% 
  mutate(project="mn")
sens.bc <- getMovebankSensors(id.bc, login=loginStored) %>% 
  mutate(project="bc")
sens.wy <- getMovebankSensors(id.wy, login=loginStored) %>% 
  mutate(project="wy")

sens <- rbind(sens.tx, sens.iw, sens.mn, sens.bc, sens.wy) %>% 
  dplyr::select(project, sensor_type_id) %>% 
  unique()
sens

#3. Download movement ata----
dat.tx <- getMovebankLocationData(id.tx, sensorID=c(82798), login=loginStored, removeDuplicatedTimestamps=TRUE, includeOutliers=TRUE)
dat.iw <- getMovebankLocationData(id.iw, sensorID=c(82798, 653, 7842954), login=loginStored, removeDuplicatedTimestamps=TRUE, includeOutliers=TRUE)
dat.mn <- getMovebankLocationData(id.mn, sensorID=c(653, 2365683, 1297673380, 77740402), login=loginStored, removeDuplicatedTimestamps=TRUE, includeOutliers=TRUE)
dat.bc <- getMovebankLocationData(id.bc, sensorID=c(82798), login=loginStored, removeDuplicatedTimestamps=TRUE, includeOutliers=TRUE)
dat.wy <- getMovebankLocationData(id.wy, sensorID=c(82798), login=loginStored, removeDuplicatedTimestamps=TRUE, includeOutliers=TRUE)

#4. Save out data----
write.csv(dat.tx, "Data/Movebank - MCP Long-billed Curlews Texas Gulf Coast.csv", row.names = FALSE)
write.csv(dat.iw, "Data/Movebank - Long-billed Curlew Migration from the Intermountain West.csv", row.names=FALSE)
write.csv(dat.mn, "Data/Movebank - Long-billed Curlew full annual cycle movement ecology - Montana.csv", row.names=FALSE)
write.csv(dat.bc, "Data/Movebank - BC LBCU tracking study.csv", row.names = FALSE)
write.csv(dat.wy, "Data/Movebank - Long-billed Curlew - Western Wyoming.csv", row.names = FALSE)

#5. Download reference data----
ref.tx <- getMovebankReferenceTable(id.tx, login=loginStored)
ref.iw <- getMovebankReferenceTable(id.iw, login=loginStored)
ref.mn <- getMovebankReferenceTable(id.mn, login=loginStored)
ref.bc <- getMovebankReferenceTable(id.bc, login=loginStored)
ref.wy <- getMovebankReferenceTable(id.wy, login=loginStored)

#6. Save out reference data----
write.csv(ref.tx, "Data/Movebank - MCP Long-billed Curlews Texas Gulf Coast - Metadata.csv", row.names=FALSE)
write.csv(ref.iw, "Data/Movebank - Long-billed Curlew Migration from the Intermountain West - Metadata.csv", row.names=FALSE)
write.csv(ref.mn, "Data/Movebank - Long-billed Curlew full annual cycle movement ecology - Montana - Metadata.csv", row.names=FALSE)
write.csv(ref.bc, "Data/Movebank - BC LBCU tracking study - Metadata.csv", row.names = FALSE)
write.csv(ref.wy, "Data/Movebank - Long-billed Curlew - Western Wyoming - Metadata.csv", row.names = FALSE)
