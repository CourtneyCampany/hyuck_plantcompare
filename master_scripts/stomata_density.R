## stomatal density by plant type & habitat

stom <- read.csv("raw_data/stomatal_density.csv")
library(vioplot)


angio_sd <- stom[stom$plant_group == "Angio", "sto_den"]
fern_sd <- stom[stom$plant_group == "Fern", "sto_den"]
lyco_sd <- stom[stom$plant_group == "Lycophyte", "sto_den"]


plot(0:1,0:1,type="n",xlim=c(0.5,3.5), ylim=c(0,80), xaxt='n', ylab="",
     xlab="")
vioplot(lyco_sd, fern_sd, angio_sd,at=1:3 ,add=TRUE,
        col="grey98", lwd=2,rectCol="grey60", colMed="black", pchMed=16, wex=.75)
axis(2, labels=FALSE, tcl=.25)
axis(1, labels = c("lycophytes","ferns","angiosperms"), at=1:3)


##need to add habitat to this dataset