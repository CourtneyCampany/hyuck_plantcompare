## elemental stats (including ratios)

#Stats for overall difference between open vs closed
library(visreg)
library(car)
library(lme4)
library(MuMIn)
library(multcomp)


#photosynthesis
chem <- read.csv("raw_data/photo_chem.csv")

nitro <- chem[complete.cases(chem$n_perc),]
  nitro$cn_ratio <- with(nitro, c_perc/n_perc)

phos <- chem[complete.cases(chem$p_perc),]
  phos$np_ratio <- with(phos, n_perc/p_perc)


###N in shade-----------------------
Nshade <- droplevels(nitro[nitro$canopy == "Closed",])
  Nshade$percN <- Nshade$n_perc /100
  
#arcsin
shade_N <-lmer(asin(sqrt(percN)) ~ plant_group + (1|species), data=Nshade)
plot(shade_N)
qqPlot(residuals(shade_N))

# #glm binomial
# shade_N2 <- glmer(n_perc/100 ~ plant_group + (1|species), family = binomial, 
#                   data=Nshade)

#logit
shade_N3 <-lmer(log(percN/(1-percN)) ~ plant_group + (1|species), data=Nshade)
plot(shade_N3)
qqPlot(residuals(shade_N3))

Anova(shade_N, type = "3")

Anova(shade_N3, type = "3")
summary(shade_N3)
r.squaredGLMM(shade_N3) 
visreg(shade_N3)

tukey_Nshade <- glht(shade_N3, linfct = mcp(plant_group = "Tukey"))
shade_Nsiglets <-cld(tukey_Nshade)
shade_Nsiglets2 <- shade_Nsiglets$mcletters$Letters

# p < 0.001 (lycophete have less N in shade)
#Angio  Fern  Lyco 
#"b"   "b"   "a" 

#R2m       R2c
#0.4537292 0.8403728

mean(Nshade[Nshade$plant_group == "Angio", "percN"]) #0.0303
mean(Nshade[Nshade$plant_group == "Fern", "percN"])#0.0295

mean(Nshade[Nshade$plant_group == "Lyco", "percN"]) #0.016
mean(Nshade[!Nshade$plant_group == "Lyco", "percN"]) #0.02983398
#45% lower than both groups

###phosphorous shade--------------

Pshade <- droplevels(phos[phos$canopy == "Closed",])
Pshade$percP <- Pshade$p_perc /100

boxplot(percP~plant_group, data=Pshade) ##couple of outliers

Pshade_nooutliers <- Pshade[Pshade$p_perc > .1 & 
                              Pshade$p_perc < .4, ]

boxplot(percP~plant_group, data=Pshade_nooutliers)

#arcsin
shade_P <- lmer(asin(sqrt(percP)) ~ plant_group + (1|species), data=Pshade_nooutliers)
plot(shade_P)
qqPlot(residuals(shade_P))


#logit (seems worse)
# shade_P3 <-lmer(log(percP/(1-percP)) ~ plant_group + (1|species), data=Pshade_nooutliers)
# plot(shade_P3)
# qqPlot(residuals(shade_P3))

Anova(shade_P, type = "3")
# Anova(shade_P3, type = "3")

summary(shade_P)
r.squaredGLMM(shade_P) 
visreg(shade_P)

tukey_Pshade <- glht(shade_P, linfct = mcp(plant_group = "Tukey"))
shade_Psiglets <-cld(tukey_Pshade)
shade_Psiglets2 <- shade_Psiglets$mcletters$Letters

library(emmeans)
emmeans(shade_P, pairwise ~ plant_group)

mean(Pshade[Pshade$plant_group == "Angio", "percP"]) #0.0025
mean(Pshade[Pshade$plant_group == "Fern", "percP"]) #0.0021
mean(Pshade[Pshade$plant_group == "Lyco", "percP"]) #0.0019
#24%lower in lycopphytes thatn angiosperms

###cn ratio----
cn_shade <- droplevels(Nshade[complete.cases(Nshade$cn_ratio), ])

cn_shade_mod <- lmer(sqrt(cn_ratio) ~ plant_group + (1|species), data=cn_shade)
plot(cn_shade_mod)
qqPlot(residuals(cn_shade_mod))
              

Anova(cn_shade_mod, type = "3")

summary(cn_shade_mod)
r.squaredGLMM(cn_shade_mod) 
visreg(cn_shade_mod)

tukey_CNshade <- glht(cn_shade_mod, linfct = mcp(plant_group = "Tukey"))
shade_CNsiglets <-cld(tukey_CNshade)
shade_CNsiglets2 <- shade_CNsiglets$mcletters$Letters

#P<0.001
#Angio  Fern  Lyco 
#"a"   "a"   "b" 

#R2m       R2c
#0.5285141 0.8194146

###NP_SHADE----
np_shade <- droplevels(Pshade[complete.cases(Pshade$np_ratio), ])
boxplot(np_ratio~plant_group, data=np_shade)

np_shade_noout <- np_shade[np_shade$np_ratio < 30,]

np_shade_mod <- lmer(sqrt(np_ratio) ~ plant_group + (1|species), 
                     data=np_shade_noout)
plot(np_shade_mod)
qqPlot(residuals(np_shade_mod))

Anova(np_shade_mod, type = "3")

summary(np_shade_mod)
r.squaredGLMM(np_shade_mod) 
visreg(np_shade_mod)

tukey_NPshade <- glht(np_shade_mod, linfct = mcp(plant_group = "Tukey"))
shade_NPsiglets <-cld(tukey_NPshade)
shade_NPsiglets2 <- shade_NPsiglets$mcletters$Letters
##not different

emmeans(np_shade_mod, pairwise ~ plant_group)

