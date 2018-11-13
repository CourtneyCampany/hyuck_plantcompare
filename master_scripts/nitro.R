## phtosynthetic rate density by plant type & habitat

chem <- read.csv("raw_data/photo_chem.csv")
library(vioplot)

nitro <- chem[complete.cases(chem$n_perc),]

angio <- nitro[nitro$plant_group == "Angio", "n_perc"]
fern <- nitro[nitro$plant_group == "Fern", "n_perc"]
lyco <- nitro[nitro$plant_group == "Lyco", "n_perc"]

open <- nitro[nitro$canopy == "Open", "n_perc"]
closed <- nitro[nitro$canopy == "Closed", "n_perc"]

par(mfrow=c(1,2))
##plant group
par(mar=c(5,5,1,0))
plot(0:1,0:1,type="n",xlim=c(0.5,3.5), ylim=c(0,5), xaxt='n', ylab="",
     xlab="")
vioplot(lyco, fern, angio,at=1:3 ,add=TRUE,
        col="grey98", lwd=2,rectCol="grey60", colMed="black", pchMed=16, wex=.75)
axis(2, labels=FALSE, tcl=.25)
axis(1, labels = c("lycophytes","ferns","angio"), at=1:3)

##habitat
par(mar=c(5,0,1,1))
plot(0:1,0:1,type="n",xlim=c(0.5,2.5), ylim=c(0,5), xaxt='n', yaxt='n',ylab="",
     xlab="")
vioplot(open, closed,at=1:2 ,add=TRUE,
        col="grey98", lwd=2,rectCol="grey60", colMed="black", pchMed=16, wex=.75)
axis(2, labels=FALSE, tcl=.25)
axis(1, labels = c("open","closed"), at=1:2)

##by group x habitat
nitro$uniqueid<- paste(nitro$plant_group, nitro$canopy, sep="-")
id_labels <- c("Angiosperm\nClosed", "Angiosperm\nOpen", "Fern\nClosed",
               "Fern\nOpen", "Lycophyte\nClosed")

windows()
boxplot(n_perc ~ uniqueid, data=nitro, outline = FALSE, xaxt='n',at=c(1:2, 4:5, 7))
axis(1, labels = c("Closed", "Open", "Closed", "Open", "Closed"), at=c(1:2, 4:5, 7))
mtext(text = c("Angiosperms","Ferns","Lycophytes"), side=1, line=2.5,
      at=c(1.5, 4.5, 7))

