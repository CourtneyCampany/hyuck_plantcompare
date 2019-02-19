#Stats for overall difference between open vs closed
library(visreg)
library(car)
library(lme4)
library(MuMIn)
library(multcomp)

library(dplyr)
library(stringr)
#photosynthesis
lrc <- read.csv("calculated_data/AQ_params.csv") %>%
       mutate(species = as.factor(str_remove(ID, "-[[:digit:]]")),
              individual =  str_remove(ID, ".*-"))
              
#treatments
treatments <- read.csv("raw_data/species_list.csv")

lrc_hyuck <- merge(lrc, treatments[,c(2,4:5)])
  lrc_hyuck$canopyplant <- interaction(lrc_hyuck$canopy, lrc_hyuck$plant_group)


fern <- droplevels(lrc_hyuck[lrc_hyuck$plant_group == "Fern", ])
  #length(unique(levels(fern$species)))
angio <-droplevels(lrc_hyuck[lrc_hyuck$plant_group == "Angiosperm", ])
  #length(unique(levels(angio$species))) 
lyco <-droplevels(lrc_hyuck[lrc_hyuck$plant_group == "Lycophyte", ])
  #length(unique(levels(lyco$species)))


hist(lrc_hyuck$Phi)
hist(lrc_hyuck$Rd)
hist(lrc_hyuck$LCP)

###comparative stats-----------------------
shade <- droplevels(lrc_hyuck[lrc_hyuck$canopy == "Closed",])
shade2 <- droplevels(shade[!shade$ID == "imp_pal-1",])

#remove imp_pal-1


#PHI
shade_phi <-lmer(Phi ~ plant_group + (1|species), data=shade)
plot(shade_phi)
#used sqrt to deal with residuals versus fitted values

Anova(shade_phi)
summary(shade_phi)
r.squaredGLMM(shade_phi) 
visreg(shade_phi)

#          R2m       R2c
# [1,] 0.1591668 0.394554
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
