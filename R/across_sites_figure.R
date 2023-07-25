library(tidyverse)
library(patchwork)
library(colorspace)

dat <- read_csv("data/stem_damage.csv")

dat %>%
  dplyr::select(Site,site, aridity,prec_90m,damage_d_half, damage_d_area,dbh_cm, wood_density) %>%
  pivot_longer(damage_d_half:wood_density) %>%
  mutate(site = as.factor(site))-> site_level

site_level$Site <- fct_reorder(.f = site_level$Site, .x = site_level$prec_90m, .fun = min)

#Panels for figure 1
site_stem_area <- site_level %>%
  filter(name == "damage_d_area") %>%
  ggplot(aes(y = value, x = Site, fill = prec_90m))+
  geom_boxplot()+
  scale_fill_continuous_sequential(palette = "Green-Yellow")+
  ylab("Stem damage (% area)")+
  theme_classic(base_size = 15)+
  theme(strip.background = element_blank(),
        strip.placement = "outside")

site_stem <- site_level %>%
  filter(name == "damage_d_half") %>%
  ggplot(aes(y = value, x = Site, fill = prec_90m))+
  geom_boxplot()+
  scale_fill_continuous_sequential(palette = "Green-Yellow")+
  ylab("Stem damage (%)")+
  theme_classic(base_size = 15)+
  theme(strip.background = element_blank(),
        strip.placement = "outside")

site_dbh <- site_level %>%
  filter(name == "dbh_cm") %>%
  ggplot(aes(y = value, x = Site, fill = prec_90m))+
  geom_boxplot()+
  scale_fill_continuous_sequential(palette = "Green-Yellow")+
  ylab("DBH (cm)")+
  theme_classic(base_size = 15)+
  theme(strip.background = element_blank(),
        strip.placement = "outside")

site_wd <- site_level %>%
  filter(name == "wood_density") %>%
  ggplot(aes(y = value, x = Site, fill = prec_90m))+
  geom_boxplot()+
  scale_fill_continuous_sequential(name = "Precipitation (mm)",palette = "Green-Yellow")+
  ylab("Wood density (g/cm3)")+
  theme_classic(base_size = 15)+
  theme(strip.background = element_blank(),
        strip.placement = "outside")

#Termite and precipitation 
#get weather data
read_csv("data/weather_bioclim.csv") %>%
  mutate(site= case_match(site, "station_creek" ~ "stck",
                          "mtlewis_sclerophyll" ~  "mtlwsc",
                          "mtlewis_rainforest" ~ "mtlwrf",
                          "pennyweight" ~ "pnw",
                          "daintree" ~ "dro")) -> wtrdata

wtrdata$Site <- c("Sav2","Scl1","Rf1","Sav1","Rf2")

#termite damage from Clements et al. https://doi.org/10.3389/fevo.2021.657444
termite_damage <- tibble(site = c("stck", "mtlwsc","mtlwrf","pnw","dro"),
                         damage = c(60,35,1.1,90,10 ))

wtrdata <- left_join(wtrdata,termite_damage)

termite_aridity <- ggplot(wtrdata, aes(x= prec_90m, y = damage, label=Site))+
  scale_colour_continuous_sequential(palette = "Green-Yellow")+
  ylab("Termite damage in DDW (%)")+
  xlab("Precipitation (mm)")+
  geom_point()+
  geom_text(nudge_y = 4,hjust="inward")+
  theme_bw(base_size = 15)



site_stem+theme(legend.position = "none") +
  site_dbh+theme(legend.position = "none")+ 
  site_wd + termite_aridity+ plot_layout(nrow=1)+plot_annotation(tag_levels = 'A') 
ggsave("output/sitelevel.jpeg",width = 14, height = 5)

