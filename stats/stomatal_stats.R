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

###comparative stats-----------------------
fernangio <- droplevels(stom_dens[!stom_dens$plant_group %in% "Lycophyte", ])

#stomatal density by light (random effect of plant group & species)

#excludes light bc not in both canopies
stomden_mod <- lmer(sto_den ~ canopy + (1|plant_group/species), data=fernangio)
visreg(stomden_mod)

Anova(stomden_mod)
summary(stomden_mod)
r.squaredGLMM(stomden_mod)

tukey_stom <- glht(stomden_mod, linfct = mcp(canopy = "Tukey"))
stom_siglets <-cld(tukey_stom)
stom_siglets2 <- stom_siglets$mcletters$Letters
# not different overall, tons of group+ species variation

stomden_mod2 <- lmer(sto_den ~ canopy * plant_group + (1|species), data=stom_dens)

visreg(stomden_mod2)
library(emmeans)
emmip(stomden_mod2, canopy ~ plant_group)
emmeans(stomden_mod2, pairwise ~ canopy : plant_group)

Anova(stomden_mod2)
summary(stomden_mod2)
r.squaredGLMM(stomden_mod2)
#interaction was not significant so dropped


stomden_mod3 <- lmer(sto_den ~ canopy + plant_group + (1|species), data=stom_dens)
plot(stomden_mod3) # pretty good
qqPlot(residuals(stomden_mod3)) # pretty good

Anova(stomden_mod3)
summary(stomden_mod3)
r.squaredGLMM(stomden_mod3)

tukey_stom3 <- glht(stomden_mod3, linfct = mcp(plant_group = "Tukey"))
stom3_siglets <-cld(tukey_stom3)
stom3_siglets3 <- stom3_siglets$mcletters$Letters

#p = 0.01351 
#0.3637892 0.8923573
#  Angio      Fern Lycophyte 
#   "b"       "a"      "ab" 

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
