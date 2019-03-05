source("master_scripts/plot_objects.R")

library(plotrix)
library(visreg)
library(car)
library(lme4)
library(MuMIn)

### bivariate relationships

chem <- read.csv("raw_data/photo_chem.csv")

fern <- droplevels(chem[chem$plant_group == "Fern",])
lyco <- droplevels(chem[chem$plant_group == "Lyco",])
angio <- droplevels(chem[chem$plant_group == "Angio",])

fernangio <- droplevels(chem[!chem$plant_group == "Lyco",])

cols <- c("forestgreen", "dodgerblue", "firebrick4")
library(scales)
cols2 <- c(alpha("forestgreen",.7), alpha("dodgerblue",.7), alpha("firebrick4",.7))

##full model for angio and fern
# anitro_mod <- lm(Photo~n_perc + canopy * plant_group, data=fernangio)
anitro_mod2 <- lmer(Photo~n_perc + canopy * plant_group + (1|species), 
                    data=fernangio)
plot(anitro_mod2)

library(car)
# anova(anitro_mod)
Anova(anitro_mod2, type=3)
summary(anitro_mod2)
visreg(anitro_mod2, "plant_group", by="canopy", overlay=TRUE)
library(emmeans)
emmip(anitro_mod2, canopy ~ plant_group)
emmeans(anitro_mod2, pairwise ~ canopy : plant_group)
r.squaredGLMM(anitro_mod2)

##simple models for ablineclip, paired with full model
fern_photoN_open <- lm(Photo~n_perc, data=fern[fern$canopy == "Open",])
fern_open2 <- lmer(Photo~n_perc + (1|species), data=fern[fern$canopy == "Open",])
Anova(fern_open2) ###yes p = 0.0002
fernopen_fix <- fixef(fern_open2)

fern_photoN_closed <- lm(Photo~n_perc, data=fern[fern$canopy == "Closed",]) 
fern_closed2 <- lmer(Photo~n_perc + (1|species), data=fern[fern$canopy == "Closed",])
Anova(fern_closed2) ##no

lyco_photoN <-lm(Photo~n_perc, data=lyco) #sig 
lyco_photoN2 <-lmer(Photo~n_perc + (1|species), data=lyco)
Anova(lyco_photoN2) ##yes p <0.0001

angio_photoN_open <- lm(Photo~n_perc, data=angio[angio$canopy == "Open",]) 
angio_open2 <- lmer(Photo~n_perc + (1|species), data=angio[angio$canopy == "Open",])
Anova(angio_open2) ##marginally 0.06811 .
angioopen_fix <- fixef(angio_open2)

angio_photoN_closed <- lm(Photo~n_perc, data=angio[angio$canopy == "Closed",]) 
angio_closed2 <- lmer(Photo~n_perc + (1|species), data=angio[angio$canopy == "Closed",])
Anova(angio_closed2) ## yes

#understanding interaction
#model for fern angio sperm closed: not diff
closed <- droplevels(fernangio[fernangio$canopy == "Closed",])
closed <- closed[complete.cases(closed$n_perc) & complete.cases(closed$Photo),]
closed_mod <- lmer(Photo~n_perc + (1|species), data=closed) 
Anova(closed_mod) #no

##dataframes for plitting model fits
fernopen <- droplevels(chem[chem$plant_group == "Fern" & chem$canopy == "Open",])
angioopen <- droplevels(chem[chem$plant_group == "Angio" & chem$canopy == "Open",])

#plotting
windows()
par(mar=c(5,5,1,1))
plot(Photo~n_perc, data=chem, ylab=photolab, xlab = "Foliar Nitrogen  (%)",
     type='n')
points(Photo~n_perc, data=angio, pch=c(21,22)[canopy], bg=cols2[1])
points(Photo~n_perc, data=fern, pch=c(21,22)[canopy], bg=cols2[2])
#fixed effects 
# ablineclip(closed_fix[[1]], closed_fix[[2]],lwd=2,
#            x1=min(closed$n_perc), x2=max(closed$n_perc))
#fernopen
ablineclip(fernopen_fix[[1]], fernopen_fix[[2]],lwd=2,col=cols2[2],
           x1=1.56, x2=3.01)
#angioopen
ablineclip(angioopen_fix[[1]], angioopen_fix[[2]],lwd=2,col=cols2[1],
           x1=.95, x2=3.66)
