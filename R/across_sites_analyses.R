library(tidyverse)
library(easystats)

dat <- read_csv("data/stem_damage.csv")

dat %>%
  dplyr::select(Site,site, aridity,prec_90m,damage_d_half, damage_d_area,dbh_cm, wood_density) %>%
  pivot_longer(damage_d_half:wood_density) %>%
  mutate(site = as.factor(site))-> site_level

site_level$Site <- fct_reorder(.f = site_level$Site, .x = site_level$prec_90m, .fun = min)


##anovas for site level
#stem -damage
aov_damage <-aov(formula = value~site, data = filter(site_level,name =="damage_d_half"))
summary(aov_damage)
estimate_contrasts(aov_damage, p_adjust = "tukey")
export_table(model_parameters(aov_damage),digits = 2,format = "text")

#dbh
aov_dbh <-aov(formula = log(value)~site, data = filter(site_level,name =="dbh_cm"))
summary(aov_dbh)
export_table(model_parameters(aov_dbh),digits = 2,format = "text",header = NULL)

#wood density
aov_wd <-aov(formula = log(value)~site, data = filter(site_level,name =="wood_density"))
summary(aov_wd)
estimate_contrasts(aov_wd,p_adjust = "tukey")
export_table(model_parameters(aov_wd),digits = 2,format = "text",header = NULL)

