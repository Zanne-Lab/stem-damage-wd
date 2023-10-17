library(tidyverse)
library(elevatr)
library(sf)

read_csv("data/stem_damage.csv") %>%
  select(Site, longitude, latitude ,prec_90m,species_matched) %>%
  distinct(Site,latitude, longitude,prec_90m, species_matched) %>%
  count(Site,latitude, longitude, prec_90m,name = "SpeciesNumber") %>%
  arrange(desc(prec_90m))->p

DT <- st_as_sf(x = p, coords = c("longitude","latitude"), remove = FALSE)
DT <- st_set_crs(DT, value = "WGS84")
DTT <- get_elev_point(DT, src = "aws")

as_tibble(DTT) %>%
  select(-geometry)-> out

write_csv(out, "output/site_info.csv")

