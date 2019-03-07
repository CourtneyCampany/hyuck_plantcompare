source("master_scripts/plot_objects.R")

library(plotrix)
library(visreg)
library(car)
library(lme4)
library(MuMIn)

### bivariate relationships

chem <- read.csv("raw_data/photo_chem.csv")
  chem$wue <- with(chem, Photo/Trmmol)
  
stom <- read.csv("raw_data/stomatal_density.csv")
stom_agg <- doBy::summaryBy(sto_den ~ species + individual, data=stom, 
                            FUN=mean, keep.names = TRUE)
chem2 <- merge(chem,stom_agg)

fern <- droplevels(chem[chem$plant_group == "Fern",])
lyco <- droplevels(chem[chem$plant_group == "Lyco",])
angio <- droplevels(chem[chem$plant_group == "Angio",])

#open closed (no lyco)
fernangio <- droplevels(chem[!chem$plant_group == "Lyco",])
#closed only (all groups)
shade <- droplevels(chem[chem$canopy=="Closed",])

cols <- c("forestgreen", "dodgerblue", "firebrick4")
library(scales)
cols2 <- c(alpha("forestgreen",.7), alpha("dodgerblue",.7), alpha("firebrick4",.7))
leglab <- c("Angio-Closed", "Angio-Open","Ferns-Closed", "Ferns-Open")
leglab_shade <- c("Angiosperms", "Ferns", "Lycophytes")

#photosynthesis vs nitrogen-----

windows()
par(mar=c(5,5,1,1))
plot(Photo~n_perc, data=chem, ylab=photolab, xlab = "Foliar Nitrogen  (%)",
     type='n', xlim=c(0,5), ylim=c(0,25))
points(Photo~n_perc, data=angio, pch=c(21,22)[canopy], bg=cols2[1], cex=1.25)
points(Photo~n_perc, data=fern, pch=c(21,22)[canopy], bg=cols2[2], cex=1.25)
legend("topleft", leglab, pch=c(21, 22, 21, 22),
       pt.bg=c(rep(cols2[1],2), rep(cols2[2],2)), bty='n')

##i think: no relationship in shade, different postive slopes angio vs fern open
windows()
par(mar=c(5,5,1,1))
plot(Photo~n_perc, data=shade, ylab=photolab, xlab = "Foliar Nitrogen  (%)",
     pch=21, bg=cols2[plant_group], cex=1.25, xlim=c(0,5), ylim=c(0,25))
legend("topleft", leglab_shade, pch=21, pt.bg=cols, bty='n')


#gs vs wue-----

windows()
par(mar=c(5,5,1,1))
plot(wue~Cond, data=chem, ylab="Water Use Efficiency  (units)", xlab = condlab,
     type='n')
points(wue~Cond, data=angio, pch=c(21,22)[canopy], bg=cols2[1], cex=1.25)
points(wue~Cond, data=fern, pch=c(21,22)[canopy], bg=cols2[2], cex=1.25)
legend("topright", leglab, pch=c(21, 22, 21, 22),
       pt.bg=c(rep(cols2[1],2), rep(cols2[2],2)), bty='n')

##i think: no relationship in shade, different postive slopes angio vs fern open
windows()
par(mar=c(5,5,1,1))
plot(wue~Cond, data=shade, ylab="Water Use Efficiency  (units)", xlab = condlab,
     pch=21, bg=cols2[plant_group], cex=1.25)
legend("topright", leglab_shade, pch=21, pt.bg=cols, bty='n')


#sd vs wue-----
windows()
par(mar=c(5,5,1,1))
plot(wue~sto_den, data=chem2, ylab="Water Use Efficiency  (units)", xlab = condlab,
     pch=c(21,22)[canopy], bg=cols2[plant_group], cex=1.25, xlim=c(0,60),
     ylim=c(0,18))
legend("topright", c(leglab_shade, "Closed", "Open"), pch=c(21,21,21,21,22),
       pt.bg=c(cols, "black", "black"), bty='n')


