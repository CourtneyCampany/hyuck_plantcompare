source("master_scripts/plot_objects.R")

## phtosynthetic rate density by plant type & habitat

photo <- read.csv("raw_data/photo_chem.csv")
library(vioplot)
library(Hmisc)

angio <- photo[photo$plant_group == "Angio", "Ci"]
fern <- photo[photo$plant_group == "Fern", "Ci"]
lyco <- photo[photo$plant_group == "Lyco", "Ci"]

open <- photo[photo$canopy == "Open", "Ci"]
closed <- photo[photo$canopy == "Closed", "Ci"]

#photosynthesis violin-----
jpeg(filename = "output/ci.jpeg",
     width = 10, height = 6, units = "in", res= 400)
par(mfrow=c(1,2))
##plant group
par(mar=c(5,5,1,0))
plot(0:1,0:1,type="n",xlim=c(0.5,3.5), ylim=c(0,400), xaxt='n', ylab=cilab,
     xlab="")
vioplot(lyco, fern, angio,at=1:3 ,add=TRUE,
        col="grey98", lwd=2,rectCol="grey60", colMed="black", pchMed=16, wex=.75)
# axis(2, labels=FALSE, tcl=.25)
axis(1, labels = c("Lycophytes","Ferns","Angiosperms"), at=1:3)

##habitat
par(mar=c(5,0,1,1))
plot(0:1,0:1,type="n",xlim=c(0.5,2.5), ylim=c(0,400), xaxt='n', yaxt='n',ylab="",
     xlab="")
vioplot(open, closed,at=1:2 ,add=TRUE,
        col="grey98", lwd=2,rectCol="grey60", colMed="black", pchMed=16, wex=.75)
axis(2, labels=FALSE, tcl=.25)
axis(1, labels = c("Open","Closed"), at=1:2)
dev.off()

##by group x habitat = Photosynthesis
photo$uniqueid<- paste(photo$plant_group, photo$canopy, sep="-")


jpeg(filename = "output/ci2.jpeg",
     width = 8, height = 6, units = "in", res= 400)
par(mar=c(5,5,1,1))
boxplot(Photo ~ uniqueid, data=photo, outline = FALSE,xaxt='n',at=c(1:2, 4:5, 7),
        ylab=cilab)
axis(1, labels = c("Closed", "Open", "Closed", "Open", "Closed"), at=c(1:2, 4:5, 7))
mtext(text = c("Angiosperms","Ferns","Lycophytes"), side=1, line=2.5,
      at=c(1.5, 4.5, 7))
dev.off()



