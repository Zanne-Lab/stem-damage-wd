library(tidyverse)
library(elevatr)
library(sf)

read_csv("data/stem_damage.csv") %>%
  select(Site, longitude, latitude ,species_matched) %>%
  distinct(Site,latitude, longitude,species_matched) %>%
  count(Site,latitude, longitude,name = "SpeciesNumber") ->p

DT <- st_as_sf(x = p, coords = c("longitude","latitude"), remove = FALSE)
DT <- st_set_crs(DT, value = "WGS84")
DTT <- get_elev_point(DT, src = "aws")

DT$p2 = st_as_sf(as.data.frame(DT), coords = c("long2","lat2"))

