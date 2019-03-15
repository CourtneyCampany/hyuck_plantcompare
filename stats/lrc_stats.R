#Stats for overall difference between open vs closed
library(visreg)
library(car)
library(lme4)
library(MuMIn)
library(multcomp)

library(dplyr)
library(stringr)
#treatments
treatments <- read.csv("raw_data/species_list.csv")


#photosynthesis
lrc <- read.csv("calculated_data/AQ_params.csv") %>%
       mutate(species = as.factor(str_remove(ID, "-[[:digit:]]")),
              individual =  str_remove(ID, ".*-")) %>%
       merge(treatments[,c(2,4:5)]) %>%
       mutate(canopyplant = interaction(canopy, plant_group))

lrc_agg <- doBy::summaryBy(LCP ~ plant_group + canopy, data = lrc, FUN=mean)
boxplot(LCP ~ species, data=lrc)
boxplot(LCP ~ plant_group, data=lrc)


fern <- droplevels(lrc[lrc$plant_group == "Fern", ])
  #length(unique(levels(fern$species)))
angio <-droplevels(lrc[lrc$plant_group == "Angiosperm" &
                    !lrc$ID == "imp_pal-1", ])
  #length(unique(levels(angio$species))) 
lyco <-droplevels(lrc[lrc$plant_group == "Lycophyte", ])
  #length(unique(levels(lyco$species)))


hist(lrc$Phi)
hist(lrc$Rd)
hist(lrc$LCP)

###comparative stats-----------------------
shade <- droplevels(lrc[lrc$canopy == "Closed",])
shade2 <- droplevels(shade[!shade$ID == "imp_pal-1",])

#remove imp_pal-1


#PHI
shade_phi <-lmer(sqrt(Phi) ~ plant_group + (1|species), data=shade)
plot(shade_phi)
#used sqrt to deal with residuals versus fitted values

Anova(shade_phi)
summary(shade_phi)
r.squaredGLMM(shade_phi) 
visreg(shade_phi)

#          R2m       R2c
# [1,] 0.1793873 0.3995944
# P = .1463**

tukey_phi <- glht(shade_phi, linfct = mcp(plant_group = "Tukey"))
phi_siglets <-cld(tukey_phi)
phi_siglets2 <- phi_siglets$mcletters$Letters
#all same


#Rd
shade_rd <-lmer(sqrt(Rd) ~ plant_group + (1|species), data=shade2)
plot(shade_rd)
#used sqrt to deal with residuals versus fitted values

Anova(shade_rd, type='3')
summary(shade_rd)
r.squaredGLMM(shade_rd) 
visreg(shade_rd)

#          R2m       R2c
# [1,] 0.3242434 0.6033236
# P = 0.01782 * 

tukey_rd <- glht(shade_rd, linfct = mcp(plant_group = "Tukey"))
rd_siglets <-cld(tukey_rd)
rd_siglets2 <- rd_siglets$mcletters$Letters
#Angiosperm       Fern  Lycophyte 
#"a"        "b"        "b" 

#LCP
shade_lcp <-lmer(sqrt(LCP) ~ plant_group + (1|species), data=shade2)
plot(shade_lcp)
#used sqrt to deal with residuals versus fitted values

Anova(shade_lcp, type="3")
summary(shade_lcp)
r.squaredGLMM(shade_lcp) 
visreg(shade_lcp)

#          R2m       R2c
# [1,] 0.2396068 0.5255966
# P = 0.06101 .

tukey_rd <- glht(shade_rd, linfct = mcp(plant_group = "Tukey"))
rd_siglets <-cld(tukey_rd)
rd_siglets2 <- rd_siglets$mcletters$Letters
#Angiosperm       Fern  Lycophyte 
#"a"        "b"        "b"


#t-test between groups----------------------

t.test(LCP ~ canopy, data=fern)
#mean in group Closed   mean in group Open 
#7.086114             4.269654
# t = 2.1814, df = 21.571, p-value = 0.04035
#39.7 % higher in closed

t.test(LCP ~ canopy, data=angio)
#mean in group Closed   mean in group Open 
#2.698899            12.992044 
#t = -3.5998, df = 10.35, p-value = 0.004591
#79.2. higher in open

#t-test between groups

t.test(Rd ~ canopy, data=fern)
#mean in group Closed   mean in group Open 
# 0.4609597            0.2585139 
# t = 2.9739, df = 22.617, p-value = 0.006872
#44.0 % higher in closed

t.test(Rd ~ canopy, data=angio)
#mean in group Closed   mean in group Open 
#0.1487808             0.7687267 
# t =-4.2629, df = 10.452, p-value = 0.001498
#80.6%

t.test(Phi ~ canopy, data=fern)
#mean in group Closed   mean in group Open 
# 0.07440423           0.06191860 
# t = 1.6927, df = 14.171, p-value = 0.1124
#not different

t.test(Phi ~ canopy, data=angio)
#mean in group Closed   mean in group Open 
# 0.05829983           0.06672850 
#t = -1.8396, df = 22.637, p-value = 0.07898
#12.7% higher in open

###habitat vs plant group (no lycophyte)--------------


fernangio <- droplevels(lrc[!lrc$plant_group %in% "Lycophyte" &
                              !lrc$ID == "imp_pal-1", ]) 

#phi (tough one, when you drop interaction no effect, confirmed by contrasts)
phi_mod2 <- lmer(sqrt(Phi) ~ canopy * plant_group + (1|species), data=fernangio)
plot(phi_mod2)

Anova(phi_mod2) 

visreg(phi_mod2, "canopy", by="plant_group")
visreg(phi_mod2, "plant_group")

emmeans::emmip(phi_mod2, canopy ~ plant_group)
emmeans::emmeans(phi_mod2, pairwise ~ canopy : plant_group)

summary(phi_mod2)
r.squaredGLMM(phi_mod2)
#           R2m       R2c
#[1,] 0.1106403 0.2092569

mean(fernangio[fernangio$plant_group == "Fern" , "Phi"], na.rm=TRUE) #.0692
mean(fernangio[fernangio$plant_group == "Angiosperm" , "Phi"], na.rm=TRUE) #.0620
#really close

phi_mod3 <- lmer(sqrt(Phi) ~ canopy + plant_group + (1|species), data=fernangio)
plot(phi_mod3)
anova(phi_mod2, phi_mod3)
Anova(phi_mod3)

#rd

rd_mod2 <- lmer(sqrt(Rd) ~ canopy * plant_group + (1|species), data=fernangio)
plot(rd_mod2)

Anova(rd_mod2) 

visreg(rd_mod2, "canopy", by="plant_group")

emmeans::emmip(rd_mod2, canopy ~ plant_group)
emmeans::emmeans(rd_mod2, pairwise ~ canopy : plant_group)

summary(rd_mod2)
r.squaredGLMM(rd_mod2)
#           R2m       R2c
#[1,] 0.4931456 0.5970595

mean(fernangio[fernangio$plant_group == "Fern" , "Phi"], na.rm=TRUE) #.0692
mean(fernangio[fernangio$plant_group == "Angiosperm" , "Phi"], na.rm=TRUE) #.0620
#really close

#lcp

lcp_mod2 <- lmer(sqrt(LCP) ~ canopy * plant_group + (1|species), data=fernangio)
plot(lcp_mod2)

Anova(lcp_mod2) 

visreg(lcp_mod2, "canopy", by="plant_group")

emmeans::emmip(lcp_mod2, canopy ~ plant_group)
emmeans::emmeans(lcp_mod2, pairwise ~ canopy : plant_group)

summary(lcp_mod2)
r.squaredGLMM(lcp_mod2)
#           R2m       R2c
#[1,] 0.4228195 0.5226051

mean(fernangio[fernangio$plant_group == "Fern" , "LCP"], na.rm=TRUE) 
mean(fernangio[fernangio$plant_group == "Angiosperm" , "LCP"], na.rm=TRUE)
#really close

