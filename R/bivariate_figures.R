library(tidyverse)
library(lme4)
library(ggeffects)
library(patchwork)

#bivariate figure dbh
moddbh <- glmer(cbind(damage_d_half,undamaged_half ) ~ log10(dbh_cm) + (1|site),
                data = dat, family=binomial(link="logit"),
                weights = sampled_stem,
                control=glmerControl(optimizer="bobyqa",
                                     optCtrl=list(maxfun=100000)))
ggpredict(moddbh,terms = c("dbh_cm")) %>% plot(add.data = TRUE)->d
predicted <- ggpredict(moddbh,terms = c("dbh_cm [all]"))
dat %>% 
  #filter(trt == 'T') %>% 
  group_by(site, dbh_cm) %>% 
  summarise(damage_m =mean(damage_d_half)/100) -> d5

ggplot(d5, aes(x=dbh_cm, y =damage_m* 100)) + 
  geom_point() + 
  geom_line(aes(x=x, y=predicted*100), data=predicted, 
            inherit.aes=FALSE, colour='#718355') + 
  geom_ribbon(aes(x=x, ymin=conf.low*100, ymax=conf.high*100), data=predicted, 
              inherit.aes=FALSE, alpha=0, linetype='dashed', colour='#718355')+
  ylab("Internal stem damage (%)")+
  xlab("DBH (cm)")+
  theme_bw()-> dbh

#bivariate wood density
modwd <- glmer(cbind(damage_d_half,undamaged_half ) ~ wood_density + (1|site),
               data = dat, family=binomial(link="logit"),
               weights = sampled_stem,
               control=glmerControl(optimizer="bobyqa",
                                    optCtrl=list(maxfun=100000)))
predicted <- ggpredict(modwd,terms = c("wood_density [all]"))
dat %>% 
  group_by(site, wood_density) %>% 
  summarise(damage_m =mean(damage_d_half)/100) -> d5

ggplot(d5, aes(x=wood_density, y =damage_m*100)) + 
  geom_point() + 
  xlim(0.38, max(d5$wood_density)) +
  geom_line(aes(x=x, y=predicted*100), data=predicted, 
            inherit.aes=FALSE, colour='#718355') + 
  geom_ribbon(aes(x=x, ymin=conf.low*100, ymax=conf.high*100), data=predicted, 
              inherit.aes=FALSE, alpha=0, linetype='dashed', colour='#718355')+
  ylab("Internal stem damage (%)")+
  xlab("Wood density (g/cm3)")+
  theme_bw()-> wd

#Precipitation
modpre <- glmer(cbind(damage_d_half,undamaged_half ) ~ log10(prec_90m) + (1|site),
                data = dat, family=binomial(link="logit"),
                weights = sampled_stem,
                control=glmerControl(optimizer="bobyqa",
                                     optCtrl=list(maxfun=100000)))

prev_val <- seq(812, 4458, 10)

predicted <- ggpredict(modpre,terms = c("prec_90m [prev_val]"))
dat %>% 
  group_by(site, species_matched) %>% 
  summarise(prec_90m = prec_90m, damage_m =mean(damage_d_half)/100) -> d5

ggplot(d5, aes(x=prec_90m, y =damage_m)) + 
  geom_point() + 
  geom_line(aes(x=x, y=predicted), data=predicted, 
            inherit.aes=FALSE, colour='#718355') + 
  geom_ribbon(aes(x=x, ymin=conf.low, ymax=conf.high), data=predicted, 
              inherit.aes=FALSE, alpha=0, linetype='dashed', colour='#718355')+
  ylab("Internal stem damage (%)")+
  xlab("Precipitation (mm)")+
  theme_bw()-> pre

#termite damage
modtermite <- glmer(cbind(damage_d_half,undamaged_half ) ~ damage + (1|site),
                    data = dat, family=binomial(link="logit"),
                    weights = sampled_stem,
                    control=glmerControl(optimizer="bobyqa",
                                         optCtrl=list(maxfun=100000)))
damge_range <- seq(1.1, 90, 2)
predicted <- ggpredict(modtermite,terms = c("damage [damge_range]"))
dat %>% 
  group_by(site, species_matched) %>% 
  summarise(damage = damage, damage_m =mean(damage_d_half)/100) -> d5

ggplot(d5, aes(x=damage, y =damage_m)) + 
  geom_point() + 
  geom_line(aes(x=x, y=predicted), data=predicted, 
            inherit.aes=FALSE, colour='#718355') + 
  geom_ribbon(aes(x=x, ymin=conf.low, ymax=conf.high), data=predicted, 
              inherit.aes=FALSE, alpha=0, linetype='dashed', colour='#718355')+
  ylab("Internal stem damage (%)")+
  xlab("Termite damage in DDW (%)")+
  theme_bw()-> termite
#figure
dbh+  wd+theme(axis.title.y = element_blank())+  pre+  termite+theme(axis.title.y = element_blank())+plot_annotation(tag_levels = 'A')
ggsave(filename = "output/damage_bivariate.jpeg")
