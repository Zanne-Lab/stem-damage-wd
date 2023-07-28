library(tidyverse)
library(lme4)
library(ggeffects)
library(easystats)
library(viridis)
library(arm)
library(smatr)
library(baad.data)
library(khroma)
library(ggrepel)
library(patchwork)
dat <-read_csv("data/stem_damage.csv")

dat %>%
  group_by(site) %>%
  summarise(damage_d_half_median = mean(damage_d_half, na.rm = TRUE)) 


mod1 <- glmer(cbind(damage_d_half,undamaged_half ) ~ log10(prec_90m)*wood_density + (1|site),
              data = dat, family=binomial(link="logit"),
              weights = sampled_stem,
              control=glmerControl(optimizer="bobyqa",
                                   optCtrl=list(maxfun=100000)))


predicted <- ggpredict(mod1,terms = c("prec_90m", "wood_density [all]"))

ilink <-family(mod1)$linkinv

biomass <- read_csv("data/agb.csv") %>%
  dplyr::select(site,tree,agb,prec_90m, wood_density, dbh_cm)


l<-setNames(as_tibble(predict(mod1,biomass, re.form=NA,)),
         c('fit_link'))
## add fit and se.fit on the **link** scale
biomass <- bind_cols(biomass, l)
## create the interval and backtransform
biomass <- mutate(biomass,
                fit_resp  = ilink(fit_link))

#add stem estimated based on AGB and Stem mass regression
baad <- baad.data::baad_data()
data <- baad$data

data %>%
  filter(!is.na(m.to) &
           !is.na(m.rt) &
           !is.na(m.st)) %>%
  mutate(agb=m.to-m.rt, prop= m.st/agb)-> data

modstem <- lm(log(m.st)~log(agb), data = data)
model_parameters(modstem)
v <- biomass[["agb"]]
predicted <- as.data.frame(ggpredict(modstem,terms = "agb [v]"))

biomass <- left_join(biomass, predicted, by =c("agb"="x"))


biomass %>%
  group_by(site)%>%
  mutate(agb_predict = agb*(fit_resp),
         agb_predict_50d = agb*(fit_resp*0.5),
         agb_predict_50i = agb*(fit_resp*1.5),
         ) %>%
  summarise(AGB_d_50m = sum(agb, na.rm = TRUE)/0.25,
            AGB_d_50f = AGB_d_50m-(sum(agb_predict, na.rm = TRUE)/0.25),
            AGB_d_50d = AGB_d_50m-(sum(agb_predict_50d, na.rm = TRUE)/0.25),
            AGB_d_50i = AGB_d_50m-(sum(agb_predict_50i, na.rm = TRUE)/0.25),
            ) %>%
  ungroup() %>%
  mutate(site= recode(site, "station_creek" = "Sav2",
                      "mlsc" =  "Scl1",
                      "mtlwrf" = "Rf1",
                      "pnw" = "Sav1",
                      "dro" = "Rf2"),
         pct_dmg_50m = round((1-(AGB_d_50f/AGB_d_50m))*100,1),
         pct_dmg_50i = round((1-(AGB_d_50i/AGB_d_50m))*100,1),
         pct_dmg_50d = round((1-(AGB_d_50d/AGB_d_50m))*100,1))->sum_biomass

sum_biomass$site <- fct_relevel(sum_biomass$site,"Sav1","Sav2","Scl1","Rf1")
sum_biomass$damage_biomass <- sum_biomass$AGB_d_50m- sum_biomass$AGB_d_50f
sum_biomass$damage_biomass_d <- sum_biomass$AGB_d_50m- sum_biomass$AGB_d_50d
sum(sum_biomass$damage_biomass_d)
sum_biomass$damage_biomass_i <- sum_biomass$AGB_d_50m- sum_biomass$AGB_d_50i
sum(sum_biomass$damage_biomass_i)
sum_biomass %>%
  pivot_longer(
    AGB_d_50m:pct_dmg_50d,
    names_to = c(".value", "set"),
    names_pattern = "([A-Za-z]{3}.+)([0-9]{2}.+)"
  )->agb
#50f is constant biomass loss across the stem
agb$set <- fct_relevel(agb$set,"50i", "50f","50m","50d")

ggplot(agb,aes(fill = set,x = site,y = AGB_d_))+
  scale_fill_manual(name = "Biomass estimate",
                    labels=c("50% damage\nincrease", "Constant\ndamage",
                             "No damage", "50% damage\ndecrease"),
                    values = c("#A50026", "#F99858", "#EAECCC", "#364B9A"))+
  geom_bar(position = "dodge",stat = "identity")+
  ylim(c(0,450))+
  ylab("Above ground biomass (Mg/ha)")+
  theme_classic()-> agb_plot
ggsave("agb_half.jpeg")
ggsave("agb_half.pdf")

#calculate damage with only stem 
ggsave("agb_half.jpeg")
ggsave("agb_half.pdf")


t.test(sum_biomass$AGB_d_50m, sum_biomass$AGB_d_50f, paired = TRUE, alternative = "greater")
t.test(sum_biomass$AGB_d_50m, sum_biomass$AGB_d_50d, paired = TRUE, alternative = "greater")
t.test(sum_biomass$AGB_d_50m, sum_biomass$AGB_d_50i, paired = TRUE, alternative = "greater")
p <- c(0.01514, 0.01514, 0.01514)
p.adjust(p, method = p.adjust.methods, n = length(p))



biomass %>%
  rename(sgb= predicted) %>%
  group_by(site)%>% 
  mutate(sgb_predict = sgb*(fit_resp),
         sgb_predict_50d = sgb*(fit_resp*0.5),
         sgb_predict_50i = sgb*(fit_resp*1.5),
         
  ) %>%
  summarise(SGB_d_50m = sum(sgb, na.rm = TRUE)/0.25,
            SGB_d_50f = SGB_d_50m-(sum(sgb_predict, na.rm = TRUE)/0.25),
            SGB_d_50d = SGB_d_50m-(sum(sgb_predict_50d, na.rm = TRUE)/0.25),
            SGB_d_50i = SGB_d_50m-(sum(sgb_predict_50i, na.rm = TRUE)/0.25),
  ) %>%
  ungroup() %>%
  mutate(site= recode(site, "station_creek" = "Sav2",
                      "mlsc" =  "Scl1",
                      "mtlwrf" = "Rf1",
                      "pnw" = "Sav1",
                      "dro" = "Rf2"),
         pct_dmg_50m = round((1-(SGB_d_50f/SGB_d_50m))*100,1),
         pct_dmg_50i = round((1-(SGB_d_50i/SGB_d_50m))*100,1),
         pct_dmg_50d = round((1-(SGB_d_50d/SGB_d_50m))*100,1))->sum_stem_mass

sum_stem_mass$site <- fct_relevel(sum_stem_mass$site,"Sav1","Sav2","Scl1","Rf1")
sum_stem_mass$damaged_biomass <- sum_stem_mass$SGB_d_50m- sum_stem_mass$SGB_d_50f
sum(sum_stem_mass$damaged_biomass)
sum_stem_mass$damaged_biomass_i <- sum_stem_mass$SGB_d_50m- sum_stem_mass$SGB_d_50i
sum(sum_stem_mass$damaged_biomass_i)
sum_stem_mass$damaged_biomass_d <- sum_stem_mass$SGB_d_50m- sum_stem_mass$SGB_d_50d
sum(sum_stem_mass$damaged_biomass_d)
sum_stem_mass%>%
  pivot_longer(
    SGB_d_50m:pct_dmg_50d,
    names_to = c(".value", "set"),
    names_pattern = "([A-Za-z]{3}.+)([0-9]{2}.+)"
  )->sgb

#50f is constant biomass loss across the stem
sgb$set <- fct_relevel(sgb$set,"50i", "50f","50m","50d")


ggplot(sgb,aes(fill = set,x = site,y = SGB_d_))+
  scale_fill_manual(name = "Biomass estimate",
                    labels=c("50% damage\nincrease", "Constant\ndamage",
                             "No damage", "50% damage\ndecrease"),
                    values = c("#A50026", "#F99858", "#EAECCC", "#364B9A"))+
  geom_bar(position = "dodge",stat = "identity")+
  ylim(c(0,450))+
  ylab("Stem biomass (Mg/ha)")+
  theme_classic()->sgb_plot


ggplot(filter(agb, !is.na(pct_dmg_)),aes(fill = set,x = site,y = pct_dmg_))+
  scale_fill_manual(name = "AGB estimate",values = c("#A50026", "#F99858", "#364B9A"))+
  geom_col(position = "dodge")+
  ylab("Above ground biomass damage (%)")+
  theme_classic() -> percent_plot

t.test(sum_stem_mass$SGB_d_50m, sum_stem_mass$SGB_d_50f, paired = TRUE, alternative = "greater")
t.test(sum_stem_mass$SGB_d_50m, sum_stem_mass$SGB_d_50d, paired = TRUE, alternative = "greater")
t.test(sum_stem_mass$SGB_d_50m, sum_stem_mass$SGB_d_50i, paired = TRUE, alternative = "greater")
p <- c(0.01078, 0.01078, 0.01078)
#adjust P values because of multiple testing
p.adjust(p, method = p.adjust.methods, n = length(p))

percent_plot+agb_plot+sgb_plot+ patchwork::plot_layout(nrow = 1, guides = "collect")+plot_annotation(tag_levels = "A")
ggsave("biomass_prediction.pdf",width = 10)


