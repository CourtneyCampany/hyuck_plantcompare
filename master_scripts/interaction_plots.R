source("master_scripts/plot_objects.R")
##interaction plots for canopy comparisons

chem <- read.csv("raw_data/photo_chem.csv")

fernangio <- droplevels(chem[!chem$plant_group == "Lyco",])

canopycols <- c("forestgreen", "dodgerblue")

library(doBy)
library(plotrix)
photo <- fernangio[complete.cases(fernangio$Photo),]

photo_agg <- summaryBy(Photo ~ plant_group + canopy, FUN=c(mean, std.error), 
                       data=photo)
photo_agg$xaxis <- c(1,2,1,2)


cond <- fernangio[complete.cases(fernangio$Cond),]
cond_agg <- summaryBy(Cond ~ plant_group + canopy, FUN=c(mean, std.error), 
                       data=cond)
cond_agg$xaxis <- c(1,2,1,2)

#photosynthesis (add model stats)
windows()
par(mar=c(5,5,1,1))
plot(Photo.mean ~ xaxis, data=photo_agg, ylim=c(0,15), xlim=c(0.5,2.5), xaxt='n',
      type='n', ylab=photolab, xlab="")
with(photo_agg, arrows(xaxis, Photo.mean, xaxis, 
                           Photo.mean+Photo.std.error, angle=90, 
                           col=canopycols[plant_group],length=0.03, lwd=2))
with(photo_agg, arrows(xaxis, Photo.mean, xaxis, 
                       Photo.mean-Photo.std.error, angle=90, 
                       col=canopycols[plant_group],length=0.03, lwd=2))
points(Photo.mean ~ xaxis, data=photo_agg, cex=2.5, col=canopycols[plant_group], 
       pch=19)
lines(x=c(1,2), y=c(photo_agg[3,3],photo_agg[4,3]), lwd=2.5, col="dodgerblue")
lines(x=c(1,2), y=c(photo_agg[1,3],photo_agg[2,3]), lwd=2.5, col="darkgreen")
axis(1, at=1:2, labels = c("Closed", "Open"))

#cond (add model stats)
windows()
par(mar=c(5,5,1,1))
plot(Cond.mean ~ xaxis, data=cond_agg, ylim=c(0,.35), xlim=c(0.5,2.5), xaxt='n',
     type='n', ylab=photolab, xlab="")
with(cond_agg, arrows(xaxis, Cond.mean, xaxis, 
                       Cond.mean+Cond.std.error, angle=90, 
                       col=canopycols[plant_group],length=0.03, lwd=2))
with(cond_agg, arrows(xaxis, Cond.mean, xaxis, 
                       Cond.mean-Cond.std.error, angle=90, 
                       col=canopycols[plant_group],length=0.03, lwd=2))
points(Cond.mean ~ xaxis, data=cond_agg, cex=2.5, col=canopycols[plant_group], 
       pch=19)
lines(x=c(1,2), y=c(cond_agg[3,3],cond_agg[4,3]), lwd=2.5, col="dodgerblue")
lines(x=c(1,2), y=c(cond_agg[1,3],cond_agg[2,3]), lwd=2.5, col="darkgreen")
axis(1, at=1:2, labels = c("Closed", "Open"))
