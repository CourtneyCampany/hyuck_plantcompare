## stomatal density by plant type & habitat

stom <- read.csv("raw_data/stomatal_density.csv")
treatments <- read.csv("raw_data/species_list.csv")
library(vioplot)

stom2 <- merge(stom, treatments[,4:5])

angio_sd <- stom[stom$plant_group == "Angio", "sto_den"]
fern_sd <- stom[stom$plant_group == "Fern", "sto_den"]
lyco_sd <- stom[stom$plant_group == "Lycophyte", "sto_den"]

open <- stom2[stom2$canopy == "Open", "sto_den"]
closed <- stom2[stom2$canopy == "Closed", "sto_den"]

plot(0:1,0:1,type="n",xlim=c(0.5,3.5), ylim=c(0,80), xaxt='n', ylab="",
     xlab="")
vioplot(lyco_sd, fern_sd, angio_sd,at=1:3 ,add=TRUE,
        col="grey98", lwd=2,rectCol="grey60", colMed="black", pchMed=16, wex=.75)
axis(2, labels=FALSE, tcl=.25)
axis(1, labels = c("lycophytes","ferns","angiosperms"), at=1:3)

jpeg(filename = "output/stodens.jpeg",
     width = 10, height = 6, units = "in", res= 400)
par(mfrow=c(1,2))
##plant group
par(mar=c(5,5,1,0))
plot(0:1,0:1,type="n",xlim=c(0.5,3.5), ylim=c(0,80), xaxt='n', ylab=denslab,
     xlab="")
vioplot(lyco_sd, fern_sd, angio_sd,at=1:3 ,add=TRUE,
        col="grey98", lwd=2,rectCol="grey60", colMed="black", pchMed=16, wex=.75)
# axis(2, labels=FALSE, tcl=.25)
axis(1, labels = c("Lycophytes","Ferns","Angiosperms"), at=1:3)

##habitat
par(mar=c(5,0,1,1))
plot(0:1,0:1,type="n",xlim=c(0.5,2.5), ylim=c(0,80), xaxt='n', yaxt='n',ylab="",
     xlab="")
vioplot(open, closed,at=1:2 ,add=TRUE,
        col="grey98", lwd=2,rectCol="grey60", colMed="black", pchMed=16, wex=.75)
axis(2, labels=FALSE, tcl=.25)
axis(1, labels = c("Open","Closed"), at=1:2)
dev.off()




##by group x habitat
stom2$uniqueid<- paste(stom2$plant_group, stom2$canopy, sep="-")
id_labels <- c("Angiosperm\nClosed", "Angiosperm\nOpen", "Fern\nClosed",
               "Fern\nOpen", "Lycophyte\nClosed")

jpeg(filename = "output/stodens2.jpeg",
     width = 8, height = 6, units = "in", res= 400)
par(mar=c(5,5,1,1))
boxplot(sto_den ~ uniqueid, data=stom2, outline = FALSE, xaxt='n',at=c(1:2, 4:5, 7),
        ylab=denslab)
axis(1, labels = c("Closed", "Open", "Closed", "Open", "Closed"), at=c(1:2, 4:5, 7))
mtext(text = c("Angiosperms","Ferns","Lycophytes"), side=1, line=2.5,
      at=c(1.5, 4.5, 7))
dev.off()

