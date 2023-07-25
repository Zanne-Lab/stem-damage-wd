library(tidyverse)
library(lme4)
library(easystats)
library(arm)
dat <- read_csv("data/stem_damage.csv")

#bibariate analyses diameter at breast height
dat$dbh <- arm::rescale(dat$dbh_cm)
moddbh <- glmer(cbind(damage_d_area,undamaged_area ) ~ dbh + (1|site),
                data = dat, family=binomial(link="logit"),
                weights = sampled_stem,
                control=glmerControl(optimizer="bobyqa",
                                     optCtrl=list(maxfun=100000)))
model_parameters(model = moddbh)
r2(moddbh)
model_performance(model = moddbh)
car::Anova(moddbh)

#wood density
dat$wd <- arm::rescale(dat$wood_density)
modwd <- glmer(cbind(damage_d_area,undamaged_area ) ~ wd + (1|site),
               data = dat, family=binomial(link="logit"),
               weights = sampled_stem,
               control=glmerControl(optimizer="bobyqa",
                                    optCtrl=list(maxfun=100000)))
model_parameters(model = modwd)
r2(modwd)
model_performance(model = modwd)
car::Anova(modwd)

#precipitation
modpre <- glmer(cbind(damage_d_area,undamaged_area ) ~ log(prec_90m) + (1|site),
                data = dat, family=binomial(link="logit"),
                weights = sampled_stem,
                control=glmerControl(optimizer="bobyqa",
                                     optCtrl=list(maxfun=100000)))
model_parameters(model = modpre)
r2(modpre)
model_performance(model = modpre)
car::Anova(modpre)

#Termite damage
dat$dmg <- arm::rescale(dat$damage)
modtermite <- glmer(cbind(damage_d_area,undamaged_area ) ~ dmg + (1|site),
                    data = dat, family=binomial(link="logit"),
                    weights = sampled_stem,
                    control=glmerControl(optimizer="bobyqa",
                                         optCtrl=list(maxfun=100000)))
model_parameters(model = modtermite)
r2(modtermite)
model_performance(model = modtermite)
car::Anova(modtermite)
