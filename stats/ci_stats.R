#Stats for overall difference between open vs closed
library(visreg)
library(car)
library(lme4)
library(MuMIn)
library(multcomp)


#photosynthesis
ci <- read.csv("raw_data/photo_chem.csv")
ci$canopyplant <- interaction(ci$canopy, ci$plant_group)

hist(ci$Ci)

fern <- droplevels(ci[ci$plant_group == "Fern", ])

angio <-droplevels(ci[ci$plant_group == "Angio", ])

lyco <-droplevels(ci[ci$plant_group == "Lyco", ])


###comparative stats-----------------------
shade <- droplevels(ci[ci$canopy == "Closed",])
shade_mod <-lmer(Ci ~ plant_group + (1|species), data=ci)
plot(shade_mod)
Anova(shade_mod, type="3")
summary(shade_mod)
r.squaredGLMM(shade_mod) 
visreg(shade_mod)

#            R2m       R2c  #mostly species effects
#[1,] 0.09194196 0.8199885


tukey_shade <- glht(shade_mod, linfct = mcp(plant_group = "Tukey"))
shade_siglets <-cld(tukey_shade)
shade_siglets2 <- shade_siglets$mcletters$Letters

#plant group by habitat

#canopy comparisions of sun shade with angio and ferns
fernangio <- droplevels(ci[!ci$plant_group %in% "Lyco", ])

#interaction between canopy and plant group
ci_mod2 <- lmer(Ci ~ canopy * plant_group + (1|species), data=fernangio)
plot(ci_mod2)

Anova(ci_mod2, type='3')
visreg(ci_mod2, "canopy", by="plant_group")

#test simple models
ci_mod3 <- lmer(Ci ~ canopy + plant_group + (1|species), data=fernangio)
Anova(ci_mod3, type='3')

#no differences regardless

#t-test between groups

t.test(Ci ~ canopy, data=fern)
#mean in group Closed   mean in group Open 
#274.4424             240.7790
# t = 2.2631, df = 29.753, p-value = 0.03109
#12.2 % higher in open

t.test(Ci ~ canopy, data=angio)
#mean in group Closed   mean in group Open 
#273.9929             269.1036 
# 0.41531, df = 46.821, p-value = 0.6798
