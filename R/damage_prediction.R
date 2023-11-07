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
library(ggpubr)
library(jpeg)

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
sum(sum_biomass$damage_biomass)/1.25
sum(sum_biomass$damage_biomass_d)/.125
sum_biomass$damage_biomass_i <- sum_biomass$AGB_d_50m- sum_biomass$AGB_d_50i
sum(sum_biomass$damage_biomass_i)1.25
sum_biomass %>%
  pivot_longer(
    AGB_d_50m:pct_dmg_50d,
    names_to = c(".value", "set"),
    names_pattern = "([A-Za-z]{3}.+)([0-9]{2}.+)"
  )->agb
#50f is constant biomass loss across the stem
agb$set <- fct_relevel(agb$set,"50i", "50f","50m","50d")


ggplot()+
  geom_bar(data =filter(agb,set %in% c("50f", "50m") ), aes(x = site, y= AGB_d_, fill= set),stat = "identity",position="identity", color ="black")+
  scale_fill_manual(name = "Biomass estimate",
                    labels=c("50m" = "No damage",
                             "50f"="Constant\ndamage"),
                    values = c("50m" = "white", "50f"="#E2E2E2"))+
  geom_errorbar(aes(x = factor(site), ymin = AGB_d_50i,ymax = AGB_d_50i, y = AGB_d_50f), data= sum_biomass, colour = "black", linetype = "longdash")+
  geom_errorbar(aes(x = factor(site), ymin = AGB_d_50d,ymax = AGB_d_50d, y = AGB_d_50f), data= sum_biomass, colour = "black", linetype = "longdash")+
  #scale_colour_manual(name = "", values = "black", labels = "+/- 50% damage")
  ylim(c(0,450))+
  ylab("Above ground biomass (Mg/ha)")+
  theme_classic()+
  theme()-> agb_gray

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
sum(sum_stem_mass$damaged_biomass)/1.25
sum_stem_mass$damaged_biomass_i <- sum_stem_mass$SGB_d_50m- sum_stem_mass$SGB_d_50i
sum(sum_stem_mass$damaged_biomass_i)/1.25
sum_stem_mass$damaged_biomass_d <- sum_stem_mass$SGB_d_50m- sum_stem_mass$SGB_d_50d
sum(sum_stem_mass$damaged_biomass_d)/1.25
sum_stem_mass%>%
  pivot_longer(
    SGB_d_50m:pct_dmg_50d,
    names_to = c(".value", "set"),
    names_pattern = "([A-Za-z]{3}.+)([0-9]{2}.+)"
  )->sgb

#50f is constant biomass loss across the stem
sgb$set <- fct_relevel(sgb$set,"50i", "50f","50m","50d")


ggplot()+
  geom_bar(data =filter(sgb,set %in% c("50f", "50m") ), aes(x = site, y= SGB_d_, fill= set),stat = "identity",position="identity", color ="black")+
  scale_fill_manual(name = "Biomass estimate",
                    labels=c("50m" = "No damage",
                             "50f"="Constant\ndamage"),
                    values = c("50m" = "white", "50f"="#E2E2E2"))+
  geom_errorbar(aes(x = factor(site), ymin = SGB_d_50i,ymax = SGB_d_50i, y = SGB_d_50f), data= sum_stem_mass, colour = "black", linetype = "longdash")+
  geom_errorbar(aes(x = factor(site), ymin = SGB_d_50d,ymax = SGB_d_50d, y = SGB_d_50f), data= sum_stem_mass, colour = "black", linetype = "longdash")+
  #scale_colour_manual(name = "", values = "black", labels = "+/- 50% damage")
  ylim(c(0,450))+
  ylab("Stem biomass (Mg/ha)")+
  theme_classic()+
  theme()-> sgb_gray
#"#474747" "#8A8A8A" "#C2C2C2" "#E2E2E2"
ggplot()+
  geom_bar(data =filter(agb, !is.na(pct_dmg_) & set == "50m"),
           aes(x = site, y= pct_dmg_),stat = "identity",position="identity", fill = "#E2E2E2", color ="black")+
    geom_errorbar(aes(x = factor(site), ymin = pct_dmg_50i,ymax = pct_dmg_50i,
                    y = pct_dmg_50i), data= sum_biomass, colour = "black", linetype = "longdash")+
  geom_errorbar(aes(x = factor(site), ymin = pct_dmg_50d,ymax = pct_dmg_50d, 
                    y = pct_dmg_50d), data= sum_biomass, colour = "black", linetype = "longdash")+
  ylab("Above ground biomass damage (%)")+
  theme_classic()+
   theme(legend.position = "none")-> percent_plot

t.test(sum_stem_mass$SGB_d_50m, sum_stem_mass$SGB_d_50f, paired = TRUE, alternative = "greater")
t.test(sum_stem_mass$SGB_d_50m, sum_stem_mass$SGB_d_50d, paired = TRUE, alternative = "greater")
t.test(sum_stem_mass$SGB_d_50m, sum_stem_mass$SGB_d_50i, paired = TRUE, alternative = "greater")
p <- c(0.01078, 0.01078, 0.01078)
#adjust P values because of multiple testing
p.adjust(p, method = p.adjust.methods, n = length(p))

agb_img <- readJPEG("data/agb.jpeg", native = TRUE) 
sgb_img <- readJPEG("data/sgb.jpeg", native = TRUE)

sgb_p <- sgb_gray + inset_element(sgb_img, left = 0.01, bottom = 0.7, right = 0.4, top = 1, ignore_tag = TRUE)
percent_plot + agb_gray + inset_element(agb_img, left = 0.01, bottom = 0.7, right = 0.4, top = 1, ignore_tag = TRUE) +
  sgb_gray + inset_element(sgb_img,
    left = 0.01, bottom = 0.7,
    right = 0.4, top = 1, ignore_tag = TRUE) +
  patchwork::plot_layout(nrow = 1, guides = "collect") + 
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &
  theme(axis.title.x = element_blank(),axis.title = element_text(size = 14),
        axis.text = element_text(size = 14), 
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 14))
#ggsave("output/biomass_prediction.png",width = 14)

ggsave("output/figure_5.pdf",width = 14, dpi = 600)


