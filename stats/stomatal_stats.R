#Stats for overall difference between open vs closed
library(visreg)
library(car)
library(lme4)
library(MuMIn)
library(multcomp)

#cover type treatments by species
treatments <- read.csv("raw_data/species_list.csv")

#stomatal anatomy
stom <- read.csv("raw_data/stomatal_density.csv")

#mean for each individual (multiple stomatal counts)
stom_agg <- doBy::summaryBy(sto_den ~ species + individual + plant_group,
                            FUN=mean, data=stom, keep.names = TRUE)

stom_dens <- merge(stom_agg, treatments[,c(4:5)])
  stom_dens$canopyplant <- interaction(stom_dens$canopy, stom_dens$plant_group)
  
  mean(stom_dens[stom_dens$plant_group == "Angio", "sto_den"])
  mean(stom_dens[stom_dens$plant_group == "Fern", "sto_den"])
  mean(stom_dens[stom_dens$plant_group == "Lycophyte", "sto_den"])
  

fern <- droplevels(stom_dens[stom_dens$plant_group == "Fern", ])
  
angio <-droplevels(stom_dens[stom_dens$plant_group == "Angio", ])

lyco <-droplevels(stom_dens[stom_dens$plant_group == "Lycophyte", ])


###comparative stats-----------------------
shade <- droplevels(stom_dens[stom_dens$canopy == "Closed",])

shade_mod <-lmer(sto_den ~ plant_group + (1|species), data=shade)
plot(shade_mod) #looks good

Anova(shade_mod, type = "3")
summary(shade_mod)
r.squaredGLMM(shade_mod) 
visreg(shade_mod)

#not different in shade, but less in ferns

tukey_shade <- glht(shade_mod, linfct = mcp(plant_group = "Tukey"))
shade_siglets <-cld(tukey_shade)
shade_siglets2 <- shade_siglets$mcletters$Letters

#Angio      Fern Lycophyte 
#"a"       "a"       "a"

#lost of species variation
#R2m       R2c
#0.2958341 0.9223959

mean(shade[shade$plant_group == "Angio", "sto_den"])
mean(shade[shade$plant_group == "Fern", "sto_den"])
mean(shade[shade$plant_group == "Lycophyte", "sto_den"])


###comparative stats-----------------------
fernangio <- droplevels(stom_dens[!stom_dens$plant_group %in% "Lycophyte", ])

stomden_mod2 <- lmer(sqrt(sto_den) ~ canopy * plant_group + (1|species), 
                     data=fernangio)
plot(stomden_mod2)

visreg(stomden_mod2,"canopy", by="plant_group")

library(emmeans)
emmip(stomden_mod2, canopy ~ plant_group)
emmeans(stomden_mod2, pairwise ~ canopy : plant_group)

Anova(stomden_mod2,type="3")
summary(stomden_mod2)
r.squaredGLMM(stomden_mod2)
#interaction was not significant so dropped

stomden_mod3 <- lmer(sqrt(sto_den) ~ canopy + plant_group + (1|species), 
                     data=fernangio)
plot(stomden_mod3) # pretty good
qqPlot(residuals(stomden_mod3)) # pretty good

#checkmodels
anova(stomden_mod2,stomden_mod3) #not different so use simple model

Anova(stomden_mod3)
summary(stomden_mod3)
r.squaredGLMM(stomden_mod3)

tukey_stom3 <- glht(stomden_mod3, linfct = mcp(plant_group = "Tukey"))
stom3_siglets <-cld(tukey_stom3)
stom3_siglets3 <- stom3_siglets$mcletters$Letters

#Angio  Fern 
#"b"   "a"

#p < 0.0001144 ***

#R2m      R2c
#[1,] 0.5475501 0.881687

##Ferns

fern <- stom_dens[stom_dens$plant_group == "Fern",]

fern_mod <- lmer(sto_den ~ canopy + (1|species), data=fern)
visreg(fern_mod)
plot(fern_mod) # pretty good
qqPlot(residuals(fern_mod)) # pretty good

Anova(fern_mod)
summary(fern_mod)
r.squaredGLMM(fern_mod)
##no with fern and canopy,mostly species related

tukey_fern <- glht(fern_mod, linfct = mcp(canopy = "Tukey"))
fern_siglets <-cld(tukey_fern)
fern_siglets2 <- fern_siglets$mcletters$Letters

##angio

angio <- stom_dens[stom_dens$plant_group == "Angio",]

angio_mod <- lmer(sto_den ~ canopy + (1|species), data=angio)
visreg(angio_mod)
plot(angio_mod) # pretty good
qqPlot(residuals(angio_mod)) # pretty good

Anova(angio_mod)
summary(angio_mod)
r.squaredGLMM(angio_mod)
##no with angio and canopy,mostly species related

tukey_angio <- glht(angio_mod, linfct = mcp(canopy = "Tukey"))
angio_siglets <-cld(tukey_angio)
angio_siglets2 <- angio_siglets$mcletters$Letters

##redo firstmodelwith only closed for lycophytes
