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
photo$wue <- with(photo, Photo/Trmmol)

boxplot(wue ~ plant_group, data=photo)

wue <- photo[complete.cases(photo$wue),]

###comparative stats-----------------------
shade <- droplevels(wue[wue$canopy == "Closed",])

shade_mod <-lmer(sqrt(wue)~ plant_group + (1|species), data=shade)
plot(shade_mod)
qqPlot(residuals(shade_mod))

Anova(shade_mod, type = "3")
summary(shade_mod)
r.squaredGLMM(shade_mod) 
visreg(shade_mod)

emmeans(shade_mod, pairwise ~ plant_group )

tukey_shade <- glht(shade_mod, linfct = mcp(plant_group = "Tukey"))
shade_siglets <-cld(tukey_shade)
shade_siglets2 <- shade_siglets$mcletters$Letters

mean(shade[shade$plant_group == "Lyco" , "wue"])#8.844896
mean(shade[!shade$plant_group == "Lyco", "wue"])#4.691448
#47.0% higher in lyco
#0.005176 

#canopy comparisions of sun shade with angio and ferns------
fernangio <- droplevels(wue[!wue$plant_group %in% "Lyco", ])
boxplot(wue ~ plant_group, data=fernangio)

#interaction between canopy and plant group
wue_mod2 <- lmer(sqrt(wue) ~ canopy * plant_group + (1|species), data=fernangio)
plot(wue_mod2)
qqPlot(residuals(wue_mod2))

visreg(wue_mod2, "canopy", by="plant_group")

library(emmeans)
emmip(wue_mod2, canopy ~ plant_group)
emmeans(wue_mod2, pairwise ~ canopy : plant_group)

Anova(wue_mod2, type="3")
summary(wue_mod2)
r.squaredGLMM(wue_mod2)
#canopy
#           R2m       R2c
#[1,] 0.02875255 0.715788

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
