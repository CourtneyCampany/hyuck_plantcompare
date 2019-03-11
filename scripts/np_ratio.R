## phtosynthetic rate density by plant type & habitat

chem <- read.csv("raw_data/photo_chem.csv")
library(vioplot)

phos <- chem[complete.cases(chem$p_perc),]

phos$np <- with(phos, c_perc/p_perc)

angio <- phos[phos$plant_group == "Angio", "np"]
fern <- phos[phos$plant_group == "Fern", "np"]
lyco <- phos[phos$plant_group == "Lyco", "np"]

open <- phos[phos$canopy == "Open", "np"]
closed <- phos[phos$canopy == "Closed", "np"]

jpeg(filename = "output/np_ratio.jpeg",
     width = 10, height = 6, units = "in", res= 400)

par(mfrow=c(1,2))
##plant group
par(mar=c(5,5,1,0))
plot(0:1,0:1,type="n",xlim=c(0.5,3.5), ylim=c(0,500),xaxt='n', ylab="NP ratio",
     xlab="")
vioplot(lyco, fern, angio,at=1:3 ,add=TRUE,
        col="grey98", lwd=2,rectCol="grey60", colMed="black", pchMed=16, wex=.75)
axis(2, labels=FALSE, tcl=.25)
axis(1, labels = c("Lycophytes","Ferns","Angio"), at=1:3)

##habitat
par(mar=c(5,0,1,1))
plot(0:1,0:1,type="n",xlim=c(0.5,2.5), ylim=c(0,500),xaxt='n', yaxt='n',ylab="",
     xlab="")
vioplot(open, closed,at=1:2 ,add=TRUE,
        col="grey98", lwd=2,rectCol="grey60", colMed="black", pchMed=16, wex=.75)
axis(2, labels=FALSE, tcl=.25)
axis(1, labels = c("Open","Closed"), at=1:2)

dev.off()

##by group x habitat-----
nitro$uniqueid<- paste(nitro$plant_group, nitro$canopy, sep="-")
id_labels <- c("Angiosperm\nClosed", "Angiosperm\nOpen", "Fern\nClosed",
               "Fern\nOpen", "Lycophyte\nClosed")


jpeg(filename = "output/cn2.jpeg",
     width = 8, height = 6, units = "in", res= 400)
par(mar=c(5,5,1,1))
boxplot(cn ~ uniqueid, data=nitro, outline = FALSE, ylab="CN ratio",
        xaxt='n',at=c(1:2, 4:5, 7))
axis(1, labels = c("Closed", "Open", "Closed", "Open", "Closed"), at=c(1:2, 4:5, 7))
mtext(text = c("Angiosperms","Ferns","Lycophytes"), side=1, line=2.5,
      at=c(1.5, 4.5, 7))
dev.off()
