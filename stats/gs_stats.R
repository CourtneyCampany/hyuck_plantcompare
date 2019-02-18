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


###comparative stats-----------------------
shade <- droplevels(gs[gs$canopy == "Closed",])
shade_mod <-lmer(sqrt(Cond) ~ plant_group + (1|species), data=shade)
plot(shade_mod)
#used sqrt to deal with residuals versus fitted values

Anova(shade_mod)
summary(shade_mod)
r.squaredGLMM(shade_mod) 
visreg(shade_mod)

#          R2m       R2c
# [1,] 0.2768772 0.7433387
# P = 0.005 **

tukey_shade <- glht(shade_mod, linfct = mcp(plant_group = "Tukey"))
shade_siglets <-cld(tukey_shade)
shade_siglets2 <- shade_siglets$mcletters$Letters
#Angio  Fern  Lyco 
#"b"  "ab"   "a"


#canopy comparisions of sun shade with angio and ferns
fernangio <- droplevels(gs[!gs$plant_group %in% "Lyco", ])
agricolae::skewness(fernangio$Cond)

#interaction between canopy and plant group
gs_mod2 <- lmer(sqrt(Cond) ~ canopy * plant_group + (1|species), data=fernangio)
plot(gs_mod2)

visreg(photo_mod2, "canopy", by="plant_group")

library(emmeans)
emmip(gs_mod2, canopy ~ plant_group)
emmeans(gs_mod2, pairwise ~ canopy : plant_group)

Anova(gs_mod2, type="3") #type 3 sum of sqaures for unbalanced

summary(gs_mod2)
r.squaredGLMM(gs_mod2)
#interaction between plant group and 
#           R2m       R2c
#[1,] 0.4035651 0.889268

mean(fernangio[fernangio$canopy == "Open" & fernangio$plant_group == "Angio", "Cond"])
mean(fernangio[fernangio$canopy == "Open" & fernangio$plant_group == "Fern", "Cond"])



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
