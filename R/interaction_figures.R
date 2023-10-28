library(tidyverse)
library(lme4)
library(ggeffects)
library(patchwork)


dat <- read_csv("data/stem_damage.csv")

# Colours for graph
low <- "#364B9A"
mid <- '#F99858'
high <- '#A50026'


#Wood densityt and precipitation
mod1 <- glmer(cbind(damage_d_half,undamaged_half ) ~ log10(prec_90m)*wood_density + (1|site),
              data = dat, family=binomial(link="logit"),
              weights = sampled_stem,
              control=glmerControl(optimizer="bobyqa",
                                   optCtrl=list(maxfun=100000)))

v <- c(0.38,0.69,0.87)
predicted <- ggpredict(mod1,terms = c("prec_90m", "wood_density [v]"))

dat %>% 
  group_by(site, wood_density) %>% 
  summarise(prec = prec_90m[1], damage_m =mean(damage_d_half)/100) -> d5


ggplot(d5, aes(prec, damage_m*100, color=wood_density)) + 
  geom_jitter() + 
  scale_colour_gradient2(low=low, mid= mid, midpoint= 0.661,high= high, name='Wood\ndensity') +
  geom_line(aes(x=x, y=predicted*100), data=filter(predicted, group=='0.38'), 
            inherit.aes=FALSE, colour= low) + 
  geom_ribbon(aes(x=x, ymin=conf.low*100, ymax=conf.high*100), data=filter(predicted, group=='0.38'), 
              inherit.aes=FALSE, alpha=0, linetype='dashed', colour= low) + 
  geom_line(aes(x=x, y=predicted*100), data=filter(predicted, group=='0.69'), 
            inherit.aes=FALSE, colour=mid) + 
  geom_ribbon(aes(x=x, ymin=conf.low*100, ymax=conf.high*100), data=filter(predicted, group=='0.69'), 
              inherit.aes=FALSE, alpha=0, linetype='dashed', colour=mid)+
  geom_line(aes(x=x, y=predicted*100), data=filter(predicted, group=='0.87'), 
            inherit.aes=FALSE, colour=high) + 
  geom_ribbon(aes(x=x, ymin=conf.low*100, ymax=conf.high*100), data=filter(predicted, group=='0.87'), 
              inherit.aes=FALSE, alpha=0, linetype='dashed', colour=high)+
  ylab("Internal stem damage (%)")+
  xlab("Precipitation (mm)")+
  theme_bw(base_size = 15)->rain_wd


#Wood density and termite damage
mod2 <- glmer(cbind(damage_d_half,undamaged_half ) ~ damage*wood_density + (1|site),
              data = dat, family=binomial(link="logit"),
              weights = sampled_stem,
              control=glmerControl(optimizer="bobyqa",
                                   optCtrl=list(maxfun=100000)))


v <- c(0.38,0.69,0.87)
predicted_tm <- ggpredict(mod2,terms = c("damage", "wood_density [v]"),)

dat %>% 
  #filter(trt == 'T') %>% 
  group_by(site, wood_density) %>% 
  summarise(termite_damage = damage, damage_m =mean(damage_d_half)/100) -> d5_tm

ggplot(d5_tm, aes(termite_damage, damage_m*100, color=wood_density)) + 
  geom_jitter() + 
  scale_colour_gradient2(low=low, mid= mid, midpoint= 0.661,high=high, name='Wood\ndensity') +
  geom_line(aes(x=x, y=predicted*100), data=filter(predicted_tm, group=='0.38'), 
            inherit.aes=FALSE, colour=low) + 
  geom_ribbon(aes(x=x, ymin=conf.low*100, ymax=conf.high*100), data=filter(predicted_tm, group=='0.38'), 
              inherit.aes=FALSE, alpha=0, linetype='dashed', colour=low) + 
  geom_line(aes(x=x, y=predicted*100), data=filter(predicted_tm, group=='0.69'), 
            inherit.aes=FALSE, colour=mid) + 
  geom_ribbon(aes(x=x, ymin=conf.low*100, ymax=conf.high*100), data=filter(predicted_tm, group=='0.69'), 
              inherit.aes=FALSE, alpha=0, linetype='dashed', colour=mid)+
  geom_line(aes(x=x, y=predicted*100), data=filter(predicted_tm, group=='0.87'), 
            inherit.aes=FALSE, colour=high) + 
  geom_ribbon(aes(x=x, ymin=conf.low*100, ymax=conf.high*100), data=filter(predicted_tm, group=='0.87'), 
              inherit.aes=FALSE, alpha=0, linetype='dashed', colour=high)+
  ylab("Internal stem damage (%)")+
  xlab("Termite damage in DDW (%)")+
  theme_bw(base_size = 15)-> termite_damage_wd


rain_wd+theme(legend.position = "none")+
  termite_damage_wd+theme(axis.title.y = element_blank())+
  plot_annotation(tag_levels = 'A')& 
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 14), 
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 14))

#ggsave("output/wd_interactions.jpeg", width = 10)
ggsave("output/figure_3.pdf", dpi = 600, width = 10)

