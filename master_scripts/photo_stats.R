#Stats for overall difference between open vs closed
library(visreg)
library(car)
library(lme4)
library(MuMIn)
library(multcomp)


#photosynthesis
photo <- read.csv("raw_data/photo_chem.csv")
  photo$canopyplant <- interaction(photo$canopy, photo$plant_group)
  
which(is.na(photo[complete.cases(photo$Photo), "Photo"])) #same length as photo
  
#is design balanced?
fern <- droplevels(photo[photo$plant_group == "Fern", ])
length(unique(fern$species)) #17

angio <-droplevels(photo[photo$plant_group == "Angio", ])
length(unique(angio$species))  #10

lyco <-droplevels(photo[photo$plant_group == "Lyco", ])
length(unique(lyco$species)) #6

##use TYPE 3 sum of squares in Anova for unbalanced design

  
###comparative stats-----------------------
shade <- droplevels(photo[photo$canopy == "Closed",])
shade_mod <-lmer(Photo ~ plant_group + (1|species), data=shade)
plot(shade_mod)

Anova(shade_mod, type = "3")
summary(shade_mod)
r.squaredGLMM(shade_mod) 
visreg(shade_mod)

tukey_shade <- glht(shade_mod, linfct = mcp(plant_group = "Tukey"))
shade_siglets <-cld(tukey_shade)
shade_siglets2 <- shade_siglets$mcletters$Letters
  

#canopy comparisions of sun shade with angio and ferns
fernangio <- droplevels(photo[!photo$plant_group %in% "Lyco", ])

#interaction between canopy and plant group
photo_mod2 <- lmer(Photo ~ canopy * plant_group + (1|species), data=fernangio)

visreg(photo_mod2, "canopy", by="plant_group")

library(emmeans)
emmip(photo_mod2, canopy ~ plant_group)
emmeans(photo_mod2, pairwise ~ canopy : plant_group)

Anova(photo_mod2, type="3")
summary(photo_mod2)
r.squaredGLMM(photo_mod2)
#interaction between plant group and 
#           R2m       R2c
#[1,] 0.5301052 0.8926862

mean(fernangio[fernangio$canopy == "Open" & fernangio$plant_group == "Angio", "Photo"])
mean(fernangio[fernangio$canopy == "Open" & fernangio$plant_group == "Fern", "Photo"])


#t-test between groups

t.test(Photo ~ canopy, data=fern)
#mean in group Closed   mean in group Open 
#4.446529             7.504525 

# t = -4.7229, df = 29.693, p-value = 5.203e-05

t.test(Photo ~ canopy, data=angio)
#mean in group Closed   mean in group Open 
#5.136696            13.058012 
# t = -6.9651, df = 20.368, p-value = 8.347e-07


#excludes light bc not in both canopies
# photo_mod <- lmer(Photo ~ canopy + (1|plant_group/species), data=fernangio)
# visreg(photo_mod)
#   
# Anova(photo_mod)
# summary(photo_mod)
# r.squaredGLMM(photo_mod)
#   
# tukey_photo <- glht(photo_mod, linfct = mcp(canopy = "Tukey"))
# photo_siglets <-cld(tukey_photo)
# photo_siglets2 <- photo_siglets$mcletters$Letters
#   # photosynthesis way higher in open canopies
# #Closed   Open 
# # "a"    "b"
# #p < 0.0001
# #R2m       R2c
# #[1,] 0.3440435 0.8933528
# mean(fernangio[fernangio$canopy == "Open", "Photo"])
# mean(fernangio[fernangio$canopy == "Closed", "Photo"])
# #54.8% higher in full sun
