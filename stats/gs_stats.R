#Stats for overall difference between open vs closed
library(visreg)
library(car)
library(lme4)
library(MuMIn)
library(multcomp)
library(pbkrtest)


#photosynthesis
gs <- read.csv("raw_data/photo_chem.csv")
gs$canopyplant <- interaction(gs$canopy, gs$plant_group)

fern <- droplevels(gs[gs$plant_group == "Fern", ])

angio <-droplevels(gs[gs$plant_group == "Angio", ])

lyco <-droplevels(gs[gs$plant_group == "Lyco", ])

hist(gs$Cond)

###comparative stats-----------------------
shade <- droplevels(gs[gs$canopy == "Closed",])
shade_mod <-lmer(sqrt(Cond) ~ plant_group + (1|species), data=shade)
plot(shade_mod)
#used sqrt to deal with residuals versus fitted values

Anova(shade_mod, type="3")
summary(shade_mod)
r.squaredGLMM(shade_mod) 
visreg(shade_mod)

#          R2m       R2c
# [1,] 0.3237432 0.7420295
# P = 0.001 **

tukey_shade <- glht(shade_mod, linfct = mcp(plant_group = "Tukey"))
shade_siglets <-cld(tukey_shade)
shade_siglets2 <- shade_siglets$mcletters$Letters
#Angio  Fern  Lyco 
#"b"  "ab"   "a"


#canopy comparisions of sun shade with angio and ferns
fernangio <- droplevels(gs[!gs$plant_group %in% "Lyco", ])
agricolae::skewness(sqrt(fernangio$Cond))

#interaction between canopy and plant group
gs_mod2 <- lmer(sqrt(Cond) ~ canopy * plant_group + (1|species), data=fernangio)
plot(gs_mod2)

visreg(gs_mod2, "canopy", by="plant_group")

library(emmeans)
emmip(gs_mod2, canopy ~ plant_group)
emmeans(gs_mod2, pairwise ~ canopy : plant_group)

Anova(gs_mod2, type="3") #type 3 sum of sqaures for unbalanced
#canopy only, 

summary(gs_mod2)
r.squaredGLMM(gs_mod2)
#canopy only 
#           R2m       R2c
#[1,] 0.4127405 0.8917534
# canopy, p = 0.003 **



gs_mod3 <- lmer(sqrt(Cond) ~ canopy  + (1|plant_group/species), data=fernangio)
gs_mod4 <- lmer(sqrt(Cond) ~ canopy  + plant_group + (1|species), data=fernangio)
anova(gs_mod2, gs_mod3)
Anova(gs_mod3, type='3')
anova(gs_mod2, gs_mod4)
anova(gs_mod3, gs_mod4) #model 3 deviates too much
Anova(gs_mod4, type='3')
#models not different when interaction is removed so keep the simple one (#4)
visreg(gs_mod4, "canopy")
visreg(gs_mod4, "plant_group")

mean(fernangio[fernangio$canopy == "Open" , "Cond"]) #.2045
mean(fernangio[fernangio$canopy == "Closed" , "Cond"], na.rm=TRUE) #.0731
#64.2% high gs in open

mean(fernangio[fernangio$plant_group == "Fern" , "Cond"], na.rm=TRUE) #.0721
mean(fernangio[fernangio$plant_group == "Angio" , "Cond"], na.rm=TRUE) #.1803
#60% high in angiosperms

#canopy       9.3868  1   0.002186 ** 
#plant_group  6.3901  1   0.011476 *
r.squaredGLMM(gs_mod4)
#           R2m       R2c
#[1,] 0.3891313 0.8893884


#t-test between groups

t.test(Cond ~ canopy, data=fern)
#mean in group Closed   mean in group Open 
#0.05593578             0.11343152
# t = -4.0972, df = 23.452, p-value = 0.0004279
#50.7 % higher in open

t.test(Cond ~ canopy, data=angio)
#mean in group Closed   mean in group Open 
#0.09781456           0.29560109 
# t = -4.157, df = 21.105, p-value = 0.0004425
#66.9%
