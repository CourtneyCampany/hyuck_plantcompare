source("master_scripts/plot_objects.R")
source("functions/basic_functions.R")

library(dplyr)
library(stringr)
library(doBy)
library(plotrix)

treatments <- read.csv("raw_data/species_list.csv")

##interaction plots for canopy comparisons

#gas exchange data
chem <- read.csv("raw_data/photo_chem.csv")
fernangio <- droplevels(chem[!chem$plant_group == "Lyco",])

#photosyn means
photo <- fernangio[complete.cases(fernangio$Photo),]

photo_agg <- summaryBy(Photo ~ plant_group + canopy, FUN=c(mean, std.error), 
                       data=photo)
  photo_agg$xaxis <- c(1,2,1,2)

#stomal conductance means
cond <- fernangio[complete.cases(fernangio$Cond),]
  cond_agg <- summaryBy(Cond ~ plant_group + canopy, FUN=c(mean, std.error), 
                      data=cond)
  cond_agg$xaxis <- c(1,2,1,2)

#light response curves removing one bad curve
lrc <- read.csv("calculated_data/AQ_params.csv") %>%
  mutate(species = as.factor(str_remove(ID, "-[[:digit:]]")),
         individual =  as.numeric(str_remove(ID, ".*-"))) %>%
  merge(treatments[,c(2,4:5)]) %>%
  filter(!ID ==  "imp_pal-1")

lrc_fernangio <-  droplevels(lrc[!lrc$plant_group == "Lycophyte",])

#Rd means
rd <- lrc_fernangio[complete.cases(lrc_fernangio$Rd),]
rdagg <- summaryBy(Rd ~ plant_group + canopy, FUN=c(mean, std.error), 
                      data=rd)
rdagg$xaxis <- c(1,2,1,2)

#LCP means
lcp <- lrc_fernangio[complete.cases(lrc_fernangio$LCP),]
lcp_agg <- summaryBy(LCP ~ plant_group + canopy, FUN=c(mean, std.error), 
                   data=lcp)
lcp_agg$xaxis <- c(1,2,1,2)

#plot objects
canopycols <- c(plantcols2[2], plantcols2[3])

#4 panel interaction plot-------

# windows()
par(mfrow=c(2,2))

#photosynthesis
par(mar=c(0,5,1,0))
plot(Photo.mean ~ xaxis, data=photo_agg, ylim=c(0,15), xlim=c(0.5,2.5), 
     yaxt='n',xaxt='n', type='n', ylab=photolab, xlab="")
with(photo_agg, arrows(xaxis, Photo.mean, xaxis, 
                           Photo.mean+Photo.std.error, angle=90, 
                           col=canopycols[plant_group],length=0.03, lwd=2))
with(photo_agg, arrows(xaxis, Photo.mean, xaxis, 
                       Photo.mean-Photo.std.error, angle=90, 
                       col=canopycols[plant_group],length=0.03, lwd=2))
points(Photo.mean ~ xaxis, data=photo_agg, cex=1.5, col=canopycols[plant_group], 
       pch=19)
lines(x=c(1,2), y=c(photo_agg[3,3],photo_agg[4,3]), lwd=2.5, col="forestgreen")
lines(x=c(1,2), y=c(photo_agg[1,3],photo_agg[2,3]), lwd=2.5, col="firebrick")
axis(1, at=1:2, labels = FALSE)
axis(2, las=1)

text(x=0.5, y=15, label = "A", cex=1.25)

#cond (add model stats)
par(mar=c(0,0,1,5))
plot(Cond.mean ~ xaxis, data=cond_agg, ylim=c(0,.4), xlim=c(0.5,2.5), xaxt='n',
     type='n', ylab="", xlab="", yaxt='n')
with(cond_agg, arrows(xaxis, Cond.mean, xaxis, 
                       Cond.mean+Cond.std.error, angle=90, 
                       col=canopycols[plant_group],length=0.03, lwd=2))
with(cond_agg, arrows(xaxis, Cond.mean, xaxis, 
                       Cond.mean-Cond.std.error, angle=90, 
                       col=canopycols[plant_group],length=0.03, lwd=2))
points(Cond.mean ~ xaxis, data=cond_agg, cex=1.5, col=canopycols[plant_group], 
       pch=19)
lines(x=c(1,2), y=c(cond_agg[3,3],cond_agg[4,3]), lwd=2.5, col="forestgreen")
lines(x=c(1,2), y=c(cond_agg[1,3],cond_agg[2,3]), lwd=2.5, col="firebrick")
axis(1, at=1:2, labels = FALSE)
axis(4, las=1)
mtexti(condlab, 4, outer=TRUE, cex=1, off=.6)
text(x=0.5, y=.4, label = "B", cex=1.25)

legend("topright", c("Angiosperms", "Ferns"), pch=19, 
        col=c(plantcols[2], plantcols[3]),
        bty='n', inset=.01)

#rd (add model stats)
par(mar=c(5,5,0,0))
plot(Rd.mean ~ xaxis, data=rdagg,  ylim=c(0,1.1),  xlim=c(0.5,2.5),
     type='n', ylab=resplab2, xlab="",yaxt='n',xaxt='n')
with(rdagg, arrows(xaxis, Rd.mean, xaxis, 
                   Rd.mean+Rd.std.error, angle=90, 
                   col=canopycols[plant_group],length=0.03, lwd=2))
with(rdagg, arrows(xaxis, Rd.mean, xaxis, 
                   Rd.mean-Rd.std.error, angle=90, 
                   col=canopycols[plant_group],length=0.03, lwd=2))
points(Rd.mean ~ xaxis, data=rdagg, cex=1.5, col=canopycols[plant_group], 
       pch=19)
lines(x=c(1,2), y=c(rdagg[3,3],rdagg[4,3]), lwd=2.5, col="forestgreen")
lines(x=c(1,2), y=c(rdagg[1,3],rdagg[2,3]), lwd=2.5, col="firebrick")
axis(1, at=1:2, labels = c("Closed", "Open"))
axis(2, las=1)
text(x=0.5, y=1.09, label = "C", cex=1.25)

#LCP (add model stats)
par(mar=c(5,0,0,5))
plot(LCP.mean ~ xaxis, data=lcp_agg,  xaxt='n',ylim=c(0,21),  xlim=c(0.5,2.5),
     type='n', ylab="", xlab="", yaxt='n')
with(lcp_agg, arrows(xaxis, LCP.mean, xaxis, 
                     LCP.mean+LCP.std.error, angle=90, 
                   col=canopycols[plant_group],length=0.03, lwd=2))
with(lcp_agg, arrows(xaxis, LCP.mean, xaxis, 
                     LCP.mean-LCP.std.error, angle=90, 
                   col=canopycols[plant_group],length=0.03, lwd=2))
points(LCP.mean ~ xaxis, data=lcp_agg, cex=1.5, col=canopycols[plant_group], 
       pch=19)
lines(x=c(1,2), y=c(lcp_agg[3,3],lcp_agg[4,3]), lwd=2.5, col="forestgreen")
lines(x=c(1,2), y=c(lcp_agg[1,3],lcp_agg[2,3]), lwd=2.5, col="firebrick")
axis(1, at=1:2, labels = c("Closed", "Open"))
axis(4, las=1)
mtexti(lcplab, 4, outer=TRUE, cex=1, off=.6)
text(x=0.5, y=20.7, label = "D", cex=1.25)

# dev.copy2pdf(file= "output/interactions.pdf")
# dev.off()
