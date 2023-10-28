library(tidyverse)
library(lme4)
library(easystats)
library(arm)
dat <- read_csv("data/stem_damage.csv")

#diameter at breast height
dat$dbh <- arm::rescale(dat$dbh_cm)
moddbh <- glmer(cbind(damage_d_half,undamaged_half ) ~ dbh + (1|site),
                data = dat, family=binomial(link="logit"),
                control=glmerControl(optimizer="bobyqa",
                                     optCtrl=list(maxfun=100000)))

#Bootstrap CI
moddbhCI <- confint.merMod(moddbh, method="boot", .progress="txt", PBargs= list(style=3)) 
  
moddbhCIout <- tibble("Model" = "Diameter at breast height","term"= rownames(moddbhCI),"2.5%"=moddbhCI[,1], "97.5%"=moddbhCI[,2])

export_table(format_table(moddbhCIout),format = "text")

#wood density
dat$wd <- arm::rescale(dat$wood_density)
modwd <- glmer(cbind(damage_d_half,undamaged_half ) ~ wd + (1|site),
               data = dat, family=binomial(link="logit"),
               control=glmerControl(optimizer="bobyqa",
                                    optCtrl=list(maxfun=100000)))

#Bootstrap CI
modwdCI <- confint.merMod(modwd, method="boot", .progress="txt", PBargs= list(style=3)) 

modwdCIout <- tibble("Model" = "Wood density", "term"= rownames(modwdCI),"2.5%"=modwdCI[,1], "97.5%"=modwdCI[,2])

export_table(format_table(modwdCIout),format = "text")


#precipitation

modpre <- glmer(cbind(damage_d_half,undamaged_half ) ~ log(prec_90m) + (1|site),
                data = dat, family=binomial(link="logit"),
                control=glmerControl(optimizer="bobyqa",
                                     optCtrl=list(maxfun=100000)))

#Bootstrap CI
modpreCI <- confint.merMod(modpre, method="boot", .progress="txt", PBargs= list(style=3)) 

modpreCIout <- tibble("Model" = "Precipitation", "term"= rownames(modpreCI),"2.5%"=modpreCI[,1], "97.5%"=modpreCI[,2])

export_table(format_table(modpreCIout),format = "text")


#Termite damage
dat$dmg <- arm::rescale(dat$damage)
modtermite <- glmer(cbind(damage_d_half,undamaged_half ) ~ dmg + (1|site),
                    data = dat, family=binomial(link="logit"),
                    control=glmerControl(optimizer="bobyqa",
                                         optCtrl=list(maxfun=100000)))

#Bootstrap CI
modtermiteCI <- confint.merMod(modtermite, method="boot", .progress="txt", PBargs= list(style=3)) 

modtermiteCIout <- tibble("Model" = "Termite pressure", "term"= rownames(modtermiteCI),"2.5%"=modtermiteCI[,1], "97.5%"=modtermiteCI[,2])

export_table(format_table(modtermiteCIout),format = "text")


#wood density and precipitation
dat$pre <- arm::rescale(dat$prec_90m)
dat$wd <- arm::rescale(dat$wood_density)
mod1 <- glmer(cbind(damage_d_half,undamaged_half ) ~ pre*wd + (1|site),
              data = dat, family=binomial(link="logit"),
              control=glmerControl(optimizer="bobyqa",
                                   optCtrl=list(maxfun=100000)))
#Bootstrap CI
mod1CI <- confint.merMod(mod1, method="boot", .progress="txt", PBargs= list(style=3)) 

mod1CIout <- tibble("Model" = "Wood density x Precipitation", "term"= rownames(mod1CI),"2.5%"=mod1CI[,1], "97.5%"=mod1CI[,2])


#termite damage and wood density 
dat$dmg<-arm::rescale(dat$damage)

mod2 <- glmer(cbind(damage_d_half,undamaged_half ) ~ dmg*wd + (1|site),
              data = dat, family=binomial(link="logit"),
              control=glmerControl(optimizer="bobyqa",
                                   optCtrl=list(maxfun=100000)))


#Bootstrap CI
mod2CI <- confint.merMod(mod2, method="boot", .progress="txt", PBargs= list(style=3)) 

mod2CIout <- tibble("Model" = "Wood density x Termite pressure", "term"= rownames(mod2CI),"2.5%"=mod2CI[,2], "97.5%"=mod2CI[,2])



#Make table
bind_rows(modwdCIout, moddbhCIout, modpreCIout, modtermiteCIout, mod1CIout, mod2CIout) %>%
  filter(term != ".sig01")-> tabout

export_table(format_table(tabout),format = "text")
