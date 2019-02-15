## phtosynthetic rate density by plant type & habitat

chem <- read.csv("raw_data/photo_chem.csv")
library(vioplot)

nitro <- chem[complete.cases(chem$n_perc),]

nitro$cn <- with(nitro, c_perc/n_perc)

angio <- nitro[nitro$plant_group == "Angio", "cn"]
fern <- nitro[nitro$plant_group == "Fern", "cn"]
lyco <- nitro[nitro$plant_group == "Lyco", "cn"]

open <- nitro[nitro$canopy == "Open", "cn"]
closed <- nitro[nitro$canopy == "Closed", "cn"]

jpeg(filename = "output/cn_ratio.jpeg",
     width = 10, height = 6, units = "in", res= 400)

par(mfrow=c(1,2))
##plant group
par(mar=c(5,5,1,0))
plot(0:1,0:1,type="n",xlim=c(0.5,3.5), ylim=c(0,50),xaxt='n', ylab="CN ratio",
     xlab="")
vioplot(lyco, fern, angio,at=1:3 ,add=TRUE,
        col="grey98", lwd=2,rectCol="grey60", colMed="black", pchMed=16, wex=.75)
axis(2, labels=FALSE, tcl=.25)
axis(1, labels = c("Lycophytes","Ferns","Angio"), at=1:3)

##habitat
par(mar=c(5,0,1,1))
plot(0:1,0:1,type="n",xlim=c(0.5,2.5), ylim=c(0,50),xaxt='n', yaxt='n',ylab="",
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
