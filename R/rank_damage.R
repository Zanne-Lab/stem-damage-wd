library(tidyverse)
library(taxonlookup)
vcapply <- function(X, FUN, ...) {
  vapply(X, FUN, character(1), ...)
}

split_genus <- function(str) {
  str_split <- strsplit(str, "[_ ]+")
  vcapply(str_split, "[[", 1L)
}

tax_spp <- lookup_table(unique(dat$species_matched),by_species=TRUE)
tax_spp<-tax_spp[,c('genus','family')] 
tax_spp %>%
  distinct(genus, family)->tax_spp


dat <- read_csv("data/stem_damage.csv")
dat %>%
  dplyr::group_by(site, species_matched, ) %>%
  mutate(n_spp = n()) %>%
  dplyr::group_by(site, species_matched) %>%
  filter(n_spp >2) %>%
  dplyr::summarise(mean_damage = round(mean(damage_d_half),3), 
                   sd_damage = round(sd(damage_d_half),3)) %>%
  ungroup() %>%
  mutate(rank_damage = row_number(-mean_damage), 
         genus = split_genus(species_matched)) %>%
  left_join(.,tax_spp, by = "genus") %>%
  select(-genus) %>%
  arrange(rank_damage) %>%
  relocate(Damage_rank = rank_damage, Site = site, Species = species_matched,
           Family = family, mean_damage, sd_damage)->o

write_csv(x = o, file = "output/rank-damage.csv")

