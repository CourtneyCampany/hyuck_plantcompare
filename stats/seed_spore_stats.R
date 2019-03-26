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

#PHI
t.test(seed$Phi, spore$Phi, var.equal=FALSE)
#not significant

#LCP
t.test(seed$LCP, spore$LCP, var.equal=FALSE)

#Rd
t.test(seed$Rd, spore$Rd, var.equal=FALSE)



#LCP models, with species effects
lrc_shade <- droplevels(lrc[lrc$canopy == "Closed",])

#simple
lcp_mod <- lm(LCP ~ seed, data=lrc_shade)
plot(lcp_mod)
Anova(lcp_mod, type=3)
visreg(lcp_mod)

#full model
lcp_mod2 <-lmer(sqrt(LCP) ~ seed + (1|species), data=lrc_shade)
plot(lcp_mod2)
#used sqrt to deal with residuals versus fitted values

Anova(lcp_mod2, type=3)
summary(lcp_mod2)
r.squaredGLMM(lcp_mod2) 

#          R2m       R2c
# [1,] 0.2533775 0.4984394
# P = 0.01073 *  

tukey_lcp <- glht(lcp_mod2, linfct = mcp(seed = "Tukey"))
lcp_siglets <-cld(tukey_lcp)
# seed spore 
# "a"   "b"



#Rd models-------

#simple
rd_mod <- lm(Rd ~ seed, data=lrc_shade)
plot(rd_mod)
Anova(rd_mod, type=3)
visreg(rd_mod)

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


#gs ------
gs <- read.csv("raw_data/photo_chem.csv")
gs_shade <- gs[gs$canopy == "Closed",]
gs_shade$seed <- ifelse(gs_shade$plant_group == "Angio", "seed", "spore")
gs_shade$wue <- with(gs_shade, Photo/Trmmol)

seed_gs <- droplevels(gs_shade[gs_shade$seed == "seed",])
spore_gs <- droplevels(gs_shade[gs_shade$seed == "spore",])

#cond
t.test(seed_gs$Cond, spore_gs$Cond, var.equal=FALSE)
cond_mod <-lmer(Cond ~ seed + (1|species), data=gs_shade)
Anova(cond_mod, type=3)
#yes

#Photo
t.test(seed_gs$Photo, spore_gs$Photo, var.equal=FALSE)
photo_mod <-lmer(Photo ~ seed + (1|species), data=gs_shade)
Anova(photo_mod, type=3)
#no

#N
t.test(seed_gs$n_perc, spore_gs$n_perc, var.equal=FALSE)
nmod <-lmer(n_perc ~ seed + (1|species), data=gs_shade)
Anova(nmod, type=3)
#no

#P
t.test(seed_gs$p_perc, spore_gs$p_perc, var.equal=FALSE)
p_mod <-lmer(p_perc ~ seed + (1|species), data=gs_shade)
Anova(p_mod, type=3)
#yes

#WUE
t.test(seed_gs$wue, spore_gs$wue, var.equal=FALSE)
wue_mod <-lmer(wue ~ seed + (1|species), data=gs_shade)
Anova(wue_mod, type=3)
#no
