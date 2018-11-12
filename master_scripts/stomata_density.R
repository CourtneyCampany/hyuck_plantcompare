## stomatal density by plant type & habitat

stom <- read.csv("raw_data/stomatal_density.csv")
treatments <- read.csv("raw_data/species_list.csv")
library(vioplot)

stom2 <- merge(stom, treatments[,4:5])

angio_sd <- stom[stom$plant_group == "Angio", "sto_den"]
fern_sd <- stom[stom$plant_group == "Fern", "sto_den"]
lyco_sd <- stom[stom$plant_group == "Lycophyte", "sto_den"]


plot(0:1,0:1,type="n",xlim=c(0.5,3.5), ylim=c(0,80), xaxt='n', ylab="",
     xlab="")
vioplot(lyco_sd, fern_sd, angio_sd,at=1:3 ,add=TRUE,
        col="grey98", lwd=2,rectCol="grey60", colMed="black", pchMed=16, wex=.75)
axis(2, labels=FALSE, tcl=.25)
axis(1, labels = c("lycophytes","ferns","angiosperms"), at=1:3)


## boxplots (overall)
par(mar=c(5,5,1,1))
boxplot(sto_den ~ plant_group, data=stom, outline = FALSE, xaxt='n')
axis(1, labels = c("Angiosperms","Ferns","Lycophytes"), at=1:3)

##boxplots (habitat overall)

boxplot(sto_den ~ plant_group, data=stom2, outline = FALSE, xaxt='n')
axis(1, labels = c("Angiosperms","Ferns","Lycophytes"), at=1:3)

## groups by habitat
boxplot(sto_den ~ canopy, data=stom2, outline = FALSE)

##by group x habitat
stom2$uniqueid<- paste(stom2$plant_group, stom2$canopy, sep="-")
id_labels <- c("Angiosperm\nClosed", "Angiosperm\nOpen", "Fern\nClosed",
               "Fern\nOpen", "Lycophyte\nClosed")

boxplot(sto_den ~ uniqueid, data=stom2, outline = FALSE, xaxt='n',at=c(1:2, 4:5, 7))
axis(1, labels = c("Closed", "Open", "Closed", "Open", "Closed"), at=c(1:2, 4:5, 7))
mtext(text = c("Angiosperms","Ferns","Lycophytes"), side=1, line=2.5,
      at=c(1.5, 4.5, 7))


