library(tidyverse)
library(smatr)

dat <- read_csv("data/stem_damage.csv")
#Compare linear vs area based calculation

mod_compare <- sma(dat$damage_d_area~damage_d_half, dat,slope.test = 1)
plot(mod_compare)
summary(mod_compare)
