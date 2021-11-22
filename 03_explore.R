library(tidyverse)
library(lubridate)
library(ggmap)
library(vegan)

map.theme <- theme_nothing() +
  theme(text=element_text(size=12, family="Arial"),
        axis.title.x=element_text(margin=margin(10,0,0,0)),
        axis.title.y=element_text(margin=margin(0,10,0,0)),
        axis.text = element_blank())

nam <- map_data("world", region=c("Canada", 
                                  "USA", 
                                  "Mexico",
                                  "Guatemala", 
                                  "Belize", 
                                  "El Salvador",
                                  "Honduras", 
                                  "Nicaragua", 
                                  "Costa Rica",
                                  "Panama", 
                                  "Jamaica", 
                                  "Cuba", 
                                  "The Bahamas",
                                  "Haiti", 
                                  "Dominican Republic", 
                                  "Antigua and Barbuda",
                                  "Dominica", 
                                  "Saint Lucia", 
                                  "Saint Vincent and the Grenadines", 
                                  "Barbados",
                                  "Grenada",
                                  "Trinidad and Tobago")) %>% 
  dplyr::filter(!group%in%c(258:264))

#1. Read in data----
pred <- read.csv("Data/LBCU_Filtered&Predicted&SegmentedData.csv") %>% 
  mutate(datetime = ymd_hms(datetime))

#Join predictions to raw data
dat <- read.csv("Data/LBCUCleanedData.csv") %>% 
  left_join(pred %>% 
              dplyr::select(-datetime, -lat, -long)) %>% 
  dplyr::filter(!is.na(predictedState))

#Sample size----
#total individuals
dat %>% 
  dplyr::select(id) %>% 
  unique() %>% 
  nrow()

#individuals per year
year <- dat %>% 
  dplyr::select(study, year, id) %>% 
  unique()
table(year$study, year$year)

id.years <- data.frame(table(year$id)) %>% 
  rename(id = Var1) %>% 
  group_by(Freq) %>% 
  summarize(n=n()) %>% 
  ungroup()

#Map----
#all together
map.all <- ggplot() +
  geom_polygon(data=nam, aes(x=long, y=lat, group=group), colour = "gray85", fill = "gray75", size=0.3) +
  geom_point(data=pred, aes(x=long, y=lat, colour=id), show.legend = FALSE) +
  geom_path(data=pred, aes(x=long, y=lat, colour=id), show.legend = FALSE) +
  coord_sf(xlim=c(-170, -30), expand = FALSE, crs=4326) +
  scale_colour_viridis_c() +
  map.theme

ggsave(map.all, file="Figures/FirstMap_all.jpeg", width = 12, height = 12)

#by bird id
map.id <- ggplot(pred) +
  geom_path(aes(x=long, y=lat, group=factor(year)), colour="black") +
  geom_point(aes(x=long, y=lat, colour=factor(year))) +
  facet_wrap(~id, ncol=20) +
  scale_colour_viridis_d(name="Year") +
  theme(legend.position = "bottom")
#map.id

ggsave(map.id, file="Figures/FirstMap_id.jpeg", width = 20, height = 10)

#by study
map.study <- ggplot(dat) +
  geom_polygon(data=nam, aes(x=long, y=lat, group=group), colour = "gray85", fill = "gray75", size=0.3) +
  geom_path(aes(x=long, y=lat, group=id), colour="black") +
  geom_point(aes(x=long, y=lat, colour=id)) +
  facet_wrap(~study) +
  scale_colour_viridis_c() +
  coord_sf(xlim=c(-170, -30), expand = FALSE, crs=4326) +
  theme(legend.position = "none")

ggsave(map.study, file="Figures/FirstMap_study.jpeg", width = 14, height = 8)

#by argos rating
map.argos <- ggplot(dat) +
  geom_polygon(data=nam, aes(x=long, y=lat, group=group), colour = "gray85", fill = "gray75", size=0.3) +
  geom_path(aes(x=long, y=lat, group=id), colour="black") +
  geom_point(aes(x=long, y=lat, colour=id)) +
  facet_wrap(~argos) +
  scale_colour_viridis_c() +
  coord_sf(xlim=c(-170, -30), expand = FALSE, crs=4326) +
  theme(legend.position = "none")

ggsave(map.argos, file="Figures/FirstMap_argos.jpeg", width = 14, height = 8)

#Dotplot----
dot <- ggplot(pred) +
  geom_point(aes(x=datetime, y=as.factor(id), colour=season)) +
  theme(legend.position = "bottom")

ggsave(dot, file="Figures/Dotplot.jpeg", width=20, height=10)

#Amount of data per year----
n <- data.frame(table(dat$id, dat$year)) %>% 
  rename(id = Var1, year = Var2) %>% 
  mutate(id = as.character(id),
         year = as.numeric(as.character(year))) %>% 
  left_join(dat %>% 
              dplyr::select(id, year, sensor) %>% 
              unique()) %>% 
  dplyr::filter(Freq > 0)

ggplot(n) +
  geom_point(aes(x=year, y=id, size=Freq, colour=sensor))

#Timing of data----
plot.hour <- ggplot(dat) +
  geom_hex(aes(x=datetime, y=hour)) +
  facet_wrap(~id, scales="free", ncol=20) +
  theme(legend.position="bottom") +
  scale_fill_viridis_c()

#ggsave(plot.hour, file="Figures/Timing.jpeg", width=20, height=10)

#Birds that don't appear to migrate---
nonmig <- c(1425586836, 1425591196, 279278554, 279287739, 46768241, 46768189, 46768335, 46769736, 77637912)

dat.id <- dat %>% 
  dplyr::filter(id %in% nonmig)

ggplot(dat.id) +
  geom_point(aes(x=long, y=lat, colour=datetime), show.legend=FALSE) +
  facet_wrap(~id, scales="free") +
  scale_colour_viridis_d()

#Argos error----
ggplot(dat %>% dplyr::filter(sensor!="GPS")) +
  geom_histogram(aes(x=log(error), fill=argos))

ggplot(dat %>% dplyr::filter(sensor!="GPS")) +
  geom_histogram(aes(x=error, fill=argos)) +
  facet_wrap(~argos, scales="free")

#Arrival and departure dates----
pred.dep <- pred %>% 
  dplyr::filter(season %in% c("fall migration", "spring migration")) %>% 
  group_by(id, latb, latw, year, season) %>% 
  summarize(depart = min(doy),
            arrive = max(doy)) %>% 
  ungroup() %>% 
  dplyr::filter(!(season=="fall migration" & depart > 250))

pred.doy <-  pred.dep %>% 
  pivot_longer(names_to = "event",
               cols = c(depart, arrive),
               values_to = "doy") %>% 
  left_join(pred)

#By longitude
ggplot(pred.doy %>% 
         dplyr::filter(event=="depart")) +
  geom_point(aes(x = doy, colour=factor(id), y=long), show.legend = FALSE) +
  facet_grid(year~season, scales="free")

ggplot(pred.doy %>% 
         dplyr::filter(event=="arrive")) +
  geom_point(aes(x = doy, colour=factor(id), y=long), show.legend = FALSE) +
  facet_grid(year~season, scales="free")

#By breeding latitude
ggplot(pred.doy %>% 
         dplyr::filter(event=="depart")) +
  geom_point(aes(x = doy, colour=factor(id), y=latb), show.legend = FALSE) +
  facet_grid(year~season, scales="free")

ggplot(pred.doy %>% 
         dplyr::filter(event=="arrive")) +
  geom_point(aes(x = doy, colour=factor(id), y=latb), show.legend = FALSE) +
  facet_grid(year~season, scales="free")

#By wintering latitude
ggplot(pred.doy %>% 
         dplyr::filter(event=="depart")) +
  geom_point(aes(x = doy, colour=factor(id), y=latw), show.legend = FALSE) +
  facet_grid(year~season, scales="free")

ggplot(pred.doy %>% 
         dplyr::filter(event=="arrive")) +
  geom_point(aes(x = doy, colour=factor(id), y=latw), show.legend = FALSE) +
  facet_grid(year~season, scales="free")

#Variation between years
ggplot(pred.doy) +
  geom_point(aes(x = doy, y=factor(id), colour=factor(year)), show.legend = FALSE) +
  facet_grid(event~season, scales="free")

#relationship between departure & arrival
ggplot(pred.dep) +
  geom_point(aes(x=depart, y=arrive, colour=year)) +
  facet_wrap(~season, scales="free") +
  scale_colour_viridis_c()

#conclusion: segmentation is still too messy to see anything in this

#MC
pred.mc <- pred %>% 
  dplyr::filter(season %in% c("breed", "winter")) %>% 
  group_by(id, season) %>% 
  summarize(lat = mean(lat),
            long = mean(long)) %>% 
  ungroup() %>% 
  dplyr::filter(!id %in% c(77638376, 172070319))

breed.dist <- pred.mc %>% 
  dplyr::filter(season=="breed") %>% 
  dplyr::select(lat, long) %>% 
  vegdist("euclidean")

winter.dist <- pred.mc %>% 
  dplyr::filter(season=="winter") %>% 
  dplyr::select(lat, long) %>% 
  vegdist("euclidean")

mantel(breed.dist, winter.dist)

map.mc <- ggplot(pred.mc) +
  geom_polygon(data=nam, aes(x=long, y=lat, group=group), colour = "gray85", fill = "gray75", size=0.3) +
  geom_path(aes(x=long, y=lat, group=factor(id)), colour="grey40") +
  geom_point(aes(x=long, y=lat, colour=factor(id))) +
  coord_sf(xlim=c(-170, -30), expand = FALSE, crs=4326) +
  xlab("") +
  ylab("") +
  map.theme

ggsave(map.mc, file="Figures/Map_MC.jpeg", width=8, height=6)
