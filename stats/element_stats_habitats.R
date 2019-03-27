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
  nitro$percN <- nitro$n_perc /100

phos <- chem[complete.cases(chem$p_perc),]
  phos$np_ratio <- with(phos, n_perc/p_perc)
  phos$percP <- phos$p_perc /100
  
N_fernangio <- droplevels(nitro[!nitro$plant_group %in% "Lyco", ])
P_fernangio <- droplevels(phos[!phos$plant_group %in% "Lyco", ])


# fern <- droplevels(N_fernangio[N_fernangio$plant_group == "Fern",])
# length(levels(fern$species))
# 
# angio <- droplevels(N_fernangio[N_fernangio$plant_group == "Angio",])
# length(levels(angio$species))

###N in openclosed -----------------------

N_mod <- lmer(asin(sqrt(percN)) ~ canopy * plant_group + (1|species), 
              data=N_fernangio)
plot(N_mod) #asinsqrt better than logit tranformation
qqPlot(residuals(N_mod)) #looks good

visreg(N_mod, "canopy", by="plant_group")

library(emmeans)
emmip(N_mod, canopy ~ plant_group)
emmeans(N_mod, pairwise ~ canopy : plant_group)
emmeans(N_mod, "canopy")

Anova(N_mod, type="3")
summary(N_mod)
r.squaredGLMM(N_mod)
#no effect, lots of species variation
#           R2m       R2c
#[1,] 0.1052917 0.7267267

mean(N_fernangio[N_fernangio$canopy == "Open" , "percN"])#0.02417179
mean(N_fernangio[N_fernangio$canopy == "Closed", "percN"])#0.02983398
#16.2% higher in shade

#only minor effect of habitat

##phosphorous habitat------
boxplot(percP ~ plant_group, data=P_fernangio) #trim some outliers

P_fernangio_noout <- P_fernangio[P_fernangio$p_perc > .1 & 
                                   P_fernangio$p_perc < .4, ]

P_mod <- lmer(asin(sqrt(percP)) ~ canopy * plant_group + (1|species), 
              data=P_fernangio_noout)
plot(P_mod) #asinsqrt better than logit tranformation
qqPlot(residuals(P_mod)) #looks good

visreg(P_mod, "canopy", by="plant_group")

library(emmeans)
emmip(P_mod, canopy ~ plant_group)
emmeans(P_mod, pairwise ~ canopy : plant_group)


Anova(P_mod, type=3)
summary(P_mod)
r.squaredGLMM(P_mod)
#plantgroup diff, lots of species variation
#           R2m       R2c
#[1,] 0.2351177 0.5350168

#plant_group           4.4787  1    0.03432 * 

mean(P_fernangio[P_fernangio$plant_group == "Fern" , "percP"])#0.002005968
mean(P_fernangio[P_fernangio$plant_group == "Angio", "percP"])#0.002449435
#22%% higher in Angio

## cn ratio habitats ------

cn_habitat <- droplevels(N_fernangio[complete.cases(N_fernangio$cn_ratio), ])
boxplot(cn_ratio~plant_group, data=cn_habitat)
#lys_cil seems like an outlier

cn_habitat2 <- droplevels(cn_habitat[!cn_habitat$species == "lys_cil",])

CN_mod <- lmer(sqrt(cn_ratio) ~ canopy * plant_group + (1|species), 
              data=cn_habitat2)
plot(CN_mod) #looks good
qqPlot(residuals(CN_mod)) #looks good

visreg(CN_mod, "canopy", by="plant_group")

emmip(CN_mod, canopy ~ plant_group)
emmeans(CN_mod, pairwise ~ canopy : plant_group)

Anova(CN_mod, type="3")
summary(CN_mod)
r.squaredGLMM(CN_mod)

#r.squaredGLMM(CN_mod)
# R2m       R2c
# 0.1667497 0.6306749

#Plant_group  4.5735  1    0.03247 * 

mean(cn_habitat2[cn_habitat2$plant_group == "Fern" , "cn_ratio"])#18.37458
mean(cn_habitat2[cn_habitat2$plant_group == "Angio", "cn_ratio"])#14.68877
#20.0% higher in Ferns

## NPratio habitats ------

np_habitat <- droplevels(P_fernangio[complete.cases(P_fernangio$np_ratio), ])
boxplot(np_ratio~plant_group, data=np_habitat)

#outlier trim
np_habitat2 <- np_habitat[np_habitat$np_ratio < 30,]

NP_mod <- lmer(sqrt(np_ratio) ~ canopy * plant_group + (1|species), 
               data=np_habitat2)
plot(NP_mod) #looks good
qqPlot(residuals(NP_mod)) #looks good

visreg(NP_mod, "canopy", by="plant_group")

emmip(NP_mod, canopy ~ plant_group)
emmeans(NP_mod, pairwise ~ canopy : plant_group)

Anova(NP_mod, type="3")
summary(NP_mod)
r.squaredGLMM(NP_mod)

#not different