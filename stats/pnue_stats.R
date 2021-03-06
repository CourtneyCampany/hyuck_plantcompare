#Stats for overall difference between open vs closed
library(visreg)
library(car)
library(lme4)
library(MuMIn)
library(multcomp)

###may need nonparametric here, data are hard to transform


#raw data
photo <- read.csv("raw_data/photo_chem.csv")
  #calculate PNUE on am area basis as we do not have sla
  photo$pnue <- with(photo, Photo/n_perc)

boxplot(pnue ~ plant_group, data=photo)

pnue <- photo[complete.cases(photo$pnue),]

###comparative stats-----------------------
shade <- droplevels(pnue[pnue$canopy == "Closed",])

shade_mod <-lmer(pnue~ plant_group + (1|species), data=shade)
plot(shade_mod)
qqPlot(residuals(shade_mod))

Anova(shade_mod, type = "3")
summary(shade_mod)
r.squaredGLMM(shade_mod) 
visreg(shade_mod)

tukey_shade <- glht(shade_mod, linfct = mcp(plant_group = "Tukey"))
shade_siglets <-cld(tukey_shade)
shade_siglets2 <- shade_siglets$mcletters$Letters


#canopy comparisions of sun shade with angio and ferns------
fernangio <- droplevels(pnue[!pnue$plant_group %in% "Lyco", ])
boxplot(pnue ~ species, data=fernangio)

#interaction between canopy and plant group
pnue_mod2 <- lmer(sqrt(pnue) ~ canopy * plant_group + (1|species), data=fernangio)
plot(pnue_mod2)
qqPlot(residuals(pnue_mod2))

visreg(photo_mod2, "canopy", by="plant_group")

library(emmeans)
emmip(pnue_mod2, canopy ~ plant_group)
emmeans(pnue_mod2, pairwise ~ canopy : plant_group)

Anova(pnue_mod2, type="3")
summary(pnue_mod2)
r.squaredGLMM(pnue_mod2)
#canopy
#           R2m       R2c
#[1,] 0.6329356 0.8328989

mean(fernangio[fernangio$canopy == "Open" & fernangio$plant_group == "Angio", "Photo"])
mean(fernangio[fernangio$canopy == "Open" & fernangio$plant_group == "Fern", "Photo"])


#t-test between groups

t.test(Photo ~ canopy, data=fern)
#mean in group Closed   mean in group Open 
#4.446529             7.504525 

# t = -4.7229, df = 29.693, p-value = 5.203e-05

t.test(Photo ~ canopy, data=angio)
#mean in group Closed   mean in group Open 
#5.136696            13.058012 
# t = -6.9651, df = 20.368, p-value = 8.347e-07
