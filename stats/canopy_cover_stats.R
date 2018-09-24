#Stats for species groups
library(visreg)
library(car)
library(lme4)
library(MuMIn)
library(multcomp)

stom <- read.csv("raw_data/stomatal_density.csv")
photo <- read.csv("raw_data/photo_chem.csv")
photo$iwue <- with(photo, Photo/Trmmol)


##each model has species as random effect

#stomatal density
stomden_mod <- lmer(sto_den ~ canopy + (1|species), data=stom)
visreg(stomden_mod)

Anova(stomden_mod)
summary(stomden_mod)
r.squaredGLMM(stomden_mod)

tukey_stom <- glht(stomden_mod, linfct = mcp(canopy = "Tukey"))
stom_siglets <-cld(tukey_stom)
stom_siglets2 <- stom_siglets$mcletters$Letters

#photosynthesis
photo_mod <- lmer(Photo ~ canopy + (1|species), data=photo)
visreg(photo_mod)

Anova(photo_mod)
summary(photo_mod)
r.squaredGLMM(photo_mod)

tukey_photo <- glht(photo_mod, linfct = mcp(canopy = "Tukey"))
photo_siglets <-cld(tukey_photo)
photo_siglets2 <- photo_siglets$mcletters$Letters

#nitrogen
nitro_mod <- lmer(n_perc ~ canopy + (1|species), data=photo)
visreg(nitro_mod)

Anova(nitro_mod)
summary(nitro_mod)
r.squaredGLMM(nitro_mod)

tukey_nitro <- glht(nitro_mod, linfct = mcp(canopy = "Tukey"))
nitro_siglets <-cld(tukey_nitro)
nitro_siglets2 <- nitro_siglets$mcletters$Letters

#stomatal conductance
cond_mod <- lmer(Cond ~ canopy + (1|species), data=photo)
visreg(cond_mod)

Anova(cond_mod)
summary(cond_mod)
r.squaredGLMM(cond_mod)

tukey_cond <- glht(cond_mod, linfct = mcp(canopy = "Tukey"))
cond_siglets <-cld(tukey_cond)
cond_siglets2 <- cond_siglets$mcletters$Letters

#iwue
wuedat <- photo[photo$Ci > 0,]

wue_mod <- lmer(iwue ~ canopy + (1|species), data=wuedat)
visreg(wue_mod)

Anova(wue_mod)
summary(wue_mod)
r.squaredGLMM(wue_mod)

tukey_wue <- glht(wue_mod, linfct = mcp(canopy = "Tukey"))
wue_siglets <-cld(tukey_wue)
wue_siglets2 <- wue_siglets$mcletters$Letters