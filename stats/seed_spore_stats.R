### seed vs non-seed stats
#gs, phi, lcp, rd

library(visreg)
library(car)
library(lme4)
library(MuMIn)
library(multcomp)

library(dplyr)
library(stringr)
#treatments
treatments <- read.csv("raw_data/species_list.csv")


#aq_curves
lrc <- read.csv("calculated_data/AQ_params.csv") %>%
  mutate(species = as.factor(str_remove(ID, "-[[:digit:]]")),
         individual =  str_remove(ID, ".*-")) %>%
  merge(treatments[,c(2,4:5)]) %>%
  mutate(seed = ifelse(plant_group == "Angiosperm", "seed", "spore"))


seed <- droplevels(lrc[lrc$seed == "seed" & lrc$canopy == "Closed",])
spore <- droplevels(lrc[lrc$seed == "spore" & lrc$canopy == "Closed",])


#LCP models, with species effects and outlier
lrc_shade <- droplevels(lrc[lrc$canopy == "Closed" &  !lrc$ID == "imp_pal-1",])

boxplot(LCP~seed, data=lrc_shade)
boxplot(Rd~seed, data=lrc_shade)


#lcp model ----------
lcp_mod <-lmer(sqrt(LCP) ~ seed + (1|species), data=lrc_shade)
plot(lcp_mod)
#used sqrt to deal with residuals versus fitted values

Anova(lcp_mod, type=3)
summary(lcp_mod)
r.squaredGLMM(lcp_mod) 

#          R2m       R2c
# [1,] 0.2533775 0.4984394
# P = 0.01073 *  

tukey_lcp <- glht(lcp_mod, linfct = mcp(seed = "Tukey"))
lcp_siglets <-cld(tukey_lcp)
# seed spore 
# "a"   "b"

mean(lrc_shade[lrc_shade$seed == "seed","LCP"])
mean(lrc_shade[lrc_shade$seed == "spore","LCP"])

#spore is 61.8% higher than shade (7.07 vs 2.70)

#Rd models-------


#full model
rd_mod2 <-lmer(sqrt(Rd) ~ seed + (1|species), data=lrc_shade)
plot(rd_mod2)
#used sqrt to deal with residuals versus fitted values

Anova(rd_mod2, type=3)
summary(rd_mod2)
r.squaredGLMM(rd_mod2) 

#          R2m       R2c
# [1,] 0.3417437 0.5819976
# P = 0.002196 

tukey_rd <- glht(rd_mod2, linfct = mcp(seed = "Tukey"))
rd_siglets <-cld(tukey_rd)
# seed spore 
# "a"   "b"

mean(lrc_shade[lrc_shade$seed == "seed","Rd"])
mean(lrc_shade[lrc_shade$seed == "spore","Rd"])

#seed is 68.2 % higher(.468 vs .149)

#photchem variables ------
gs <- read.csv("raw_data/photo_chem.csv")
gs_shade <- gs[gs$canopy == "Closed",]
  gs_shade$seed <- ifelse(gs_shade$plant_group == "Angio", "seed", "spore")
  gs_shade$wue <- with(gs_shade, Photo/Trmmol)

# seed_gs <- droplevels(gs_shade[gs_shade$seed == "seed",])
# spore_gs <- droplevels(gs_shade[gs_shade$seed == "spore",])
  
# NO: photo, nitro, wue, stomata
  
boxplot(Cond~seed, data=gs_shade)
boxplot(p_perc~seed, data=gs_shade)
  


#cond ------
cond_mod <-lmer(Cond ~ seed + (1|species), data=gs_shade)
Anova(cond_mod, type=3)
#yes

summary(cond_mod)
r.squaredGLMM(cond_mod) 

#          R2m       R2c
# [1,] 0.2230625 0.6955209
# P = 0.004064

tukey_cond <- glht(cond_mod, linfct = mcp(seed = "Tukey"))
cond_siglets <-cld(tukey_cond)
# seed spore 
# "a"   "b"

mean(gs_shade[gs_shade$seed == "seed","Cond"], na.rm=TRUE)
mean(gs_shade[gs_shade$seed == "spore","Cond"], na.rm=TRUE)

#seed plants have 50.5% more P (.101% vs .050%)



#P (trim outliers) ----------
gs_shade$percP <- gs_shade$p_perc /100
pshade <- gs_shade[gs_shade$percP > .1 & gs_shade$percP < .4, ]

p_mod <-lmer(asin(sqrt(p_perc)) ~ seed + (1|species), data=pshade)
Anova(p_mod, type=3)
#yes

summary(p_mod)
r.squaredGLMM(p_mod) 

#          R2m       R2c
# [1,] 0.1659906 0.4648929
# P = 0.008095

tukey_p <- glht(p_mod, linfct = mcp(seed = "Tukey"))
p_siglets <-cld(tukey_p)
# seed spore 
# "a"   "b"

mean(pshade[pshade$seed == "seed","p_perc"], na.rm=TRUE)
mean(pshade[pshade$seed == "spore","p_perc"], na.rm=TRUE)

#seed plants have 18.6% more P (.256% vs .208%)
