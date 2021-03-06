source("master_scripts/plot_objects.R")

## phtosynthetic rate density by plant type & habitat

photo <- read.csv("raw_data/photo_chem.csv")
library(vioplot)
library(ggplot2)
library(Hmisc)


angio <- photo[photo$plant_group == "Angio", "Photo"]
fern <- photo[photo$plant_group == "Fern", "Photo"]
lyco <- photo[photo$plant_group == "Lyco", "Photo"]

open <- photo[photo$canopy == "Open", "Photo"]
closed <- photo[photo$canopy == "Closed", "Photo"]

#photosynthesis violin-----
jpeg(filename = "output/photo.jpeg",
     width = 10, height = 6, units = "in", res= 400)
par(mfrow=c(1,2))
##plant group
par(mar=c(5,5,1,0))
plot(0:1,0:1,type="n",xlim=c(0.5,3.5), ylim=c(0,25), xaxt='n', ylab=photolab,
     xlab="")
vioplot(lyco, fern, angio,at=1:3 ,add=TRUE,
        col="grey98", lwd=2,rectCol="grey60", colMed="black", pchMed=16, wex=.75)
# axis(2, labels=FALSE, tcl=.25)
axis(1, labels = c("Lycophytes","Ferns","Angiosperms"), at=1:3)

##habitat
par(mar=c(5,0,1,1))
plot(0:1,0:1,type="n",xlim=c(0.5,2.5), ylim=c(0,25), xaxt='n', yaxt='n',ylab="",
     xlab="")
vioplot(open, closed,at=1:2 ,add=TRUE,
        col="grey98", lwd=2,rectCol="grey60", colMed="black", pchMed=16, wex=.75)
axis(2, labels=FALSE, tcl=.25)
axis(1, labels = c("Open","Closed"), at=1:2)
dev.off()

##by group x habitat = Photosynthesis
photo$uniqueid<- paste(photo$plant_group, photo$canopy, sep="-")


jpeg(filename = "output/photo2.jpeg",
     width = 8, height = 6, units = "in", res= 400)
par(mar=c(5,5,1,1))
boxplot(Photo ~ uniqueid, data=photo, outline = FALSE,xaxt='n',at=c(1:2, 4:5, 7),
        ylab=photolab)
axis(1, labels = c("Closed", "Open", "Closed", "Open", "Closed"), at=c(1:2, 4:5, 7))
mtext(text = c("Angiosperms","Ferns","Lycophytes"), side=1, line=2.5,
      at=c(1.5, 4.5, 7))
dev.off()

##stomatal condutace plots
angio_gs <- photo[photo$plant_group == "Angio", "Cond"]
fern_gs <- photo[photo$plant_group == "Fern", "Cond"]
lyco_gs <- photo[photo$plant_group == "Lyco", "Cond"]

open_gs <- photo[photo$canopy == "Open", "Cond"]
closed_gs <- photo[photo$canopy == "Closed", "Cond"]

jpeg(filename = "output/cond.jpeg",
     width = 10, height = 6, units = "in", res= 400)
par(mfrow=c(1,2))
##plant group
par(mar=c(5,5,1,0))
plot(0:1,0:1,type="n",xlim=c(0.5,3.5), ylim=c(0,.65), xaxt='n', ylab=condlab,
     xlab="")
vioplot(lyco_gs, fern_gs, angio_gs,at=1:3 ,add=TRUE,
        col="grey98", lwd=2,rectCol="grey60", colMed="black", pchMed=16, wex=.75)

axis(1, labels = c("Lycophytes","Ferns","Angiosperms"), at=1:3)

##habitat
par(mar=c(5,0,1,1))
plot(0:1,0:1,type="n",xlim=c(0.5,2.5), ylim=c(0,.65), xaxt='n', yaxt='n',ylab="",
     xlab="")
vioplot(open_gs, closed_gs,at=1:2 ,add=TRUE,
        col="grey98", lwd=2,rectCol="grey60", colMed="black", pchMed=16, wex=.75)
axis(2, labels=FALSE, tcl=.25)
axis(1, labels = c("Open","Closed"), at=1:2)
dev.off()

##by group x habitat = Conductance
photo$uniqueid<- paste(photo$plant_group, photo$canopy, sep="-")

jpeg(filename = "output/cond2.jpeg",
     width = 8, height = 6, units = "in", res= 400)
par(mar=c(5,5,1,1))
boxplot(Cond ~ uniqueid, data=photo, outline = FALSE,xaxt='n',at=c(1:2, 4:5, 7),
        ylab=condlab)
axis(1, labels = c("Closed", "Open", "Closed", "Open", "Closed"), at=c(1:2, 4:5, 7))
mtext(text = c("Angiosperms","Ferns","Lycophytes"), side=1, line=2.5,
      at=c(1.5, 4.5, 7))
dev.off()

####by group x habitat = WUE
photo$WUE <- with(photo, Photo/Trmmol)

jpeg(filename = "output/wue.jpeg",
     width = 7, height = 7, units = "in", res= 400)
par(mar=c(5,5,1,1))
boxplot(WUE ~ uniqueid, data=photo, outline = FALSE,xaxt='n',at=c(1:2, 4:5, 7),
        ylab="WUE")
axis(1, labels = c("Closed", "Open", "Closed", "Open", "Closed"), at=c(1:2, 4:5, 7))
mtext(text = c("Angiosperms","Ferns","Lycophytes"), side=1, line=2.5,
      at=c(1.5, 4.5, 7))
dev.off()

##using ggplot
# p <- ggplot(photo, aes(x=plant_group, y=Photo, color=bla)) + 
#   geom_violin() 
# 
# p + theme_classic() + scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9")) + stat_summary(fun.data=mean_sdl, na.rm=TRUE, 
#                                                                                                    geom="pointrange", color="red")
# + geom_jitter(shape=16, position=position_jitter(0.1))
