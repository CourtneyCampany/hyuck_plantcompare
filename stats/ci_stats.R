#Stats for overall difference between open vs closed
library(visreg)
library(car)
library(lme4)
library(MuMIn)
library(multcomp)


#photosynthesis
ci <- read.csv("raw_data/photo_chem.csv")
ci$canopyplant <- interaction(ci$canopy, ci$plant_group)


###comparative stats-----------------------
shade <- droplevels(ci[ci$canopy == "Closed",])
shade_mod <-lmer(Photo ~ plant_group + (1|species), data=shade)
Anova(shade_mod)
summary(shade_mod)
r.squaredGLMM(shade_mod) 
visreg(shade_mod)

