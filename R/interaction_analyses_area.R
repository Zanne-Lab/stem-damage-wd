library(tidyverse)
library(lme4)
library(easystats)
library(arm)
dat <- read_csv("data/stem_damage.csv")
#wood density and precipitation
dat$pre <- arm::rescale(dat$prec_90m)
dat$wd <- arm::rescale(dat$wood_density)
mod1 <- glmer(cbind(damage_d_area,undamaged_area ) ~ pre*wd + (1|site),
              data = dat, family=binomial(link="logit"),
              weights = sampled_stem,
              control=glmerControl(optimizer="bobyqa",
                                   optCtrl=list(maxfun=100000)))
model_parameters(model = mod1)
r2(mod1)

model_performance(model = mod1)


#termite damage and wood density 
dat$dmg<-arm::rescale(dat$damage)

mod2 <- glmer(cbind(damage_d_area,undamaged_area ) ~ dmg*wd + (1|site),
              data = dat, family=binomial(link="logit"),
              weights = sampled_stem,
              control=glmerControl(optimizer="bobyqa",
                                   optCtrl=list(maxfun=100000)))
model_parameters(model = mod2)
r2(mod2)
model_performance(model = mod2)

##compare models
#bivarite preciop

#test likelihood of intercation model against bivariate
#precipitation
modpre <- glmer(cbind(damage_d_area,undamaged_area ) ~ pre + (1|site),
                data = dat, family=binomial(link="logit"),
                weights = sampled_stem,
                control=glmerControl(optimizer="bobyqa",
                                     optCtrl=list(maxfun=100000)))

test_likelihoodratio(modpre,mod1)

#wood density
dat$wd <- arm::rescale(dat$wood_density)
modwd <- glmer(cbind(damage_d_area,undamaged_area ) ~ wd + (1|site),
               data = dat, family=binomial(link="logit"),
               weights = sampled_stem,
               control=glmerControl(optimizer="bobyqa",
                                    optCtrl=list(maxfun=100000)))
test_likelihoodratio(modwd,mod2)
