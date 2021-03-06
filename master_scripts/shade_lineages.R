source("master_scripts/plot_objects.R")
source("functions/basic_functions.R")

##lycophytes differ

chem <- read.csv("raw_data/photo_chem.csv")
library(vioplot)

chem_closed <- chem[chem$canopy == "Closed",]
chem_closed$cnratio <- with(chem_closed, c_perc/n_perc)
chem_closed$wue <- with(chem_closed, Photo/Trmmol)

phos <- chem_closed[chem_closed$p_perc > .1 & chem_closed$p_perc < .4,]

#boxplots of significant traits

# windows()
par(mfrow=c(2,2), mgp=c(3,.75,0))

par(mar=c(0,5.5,1,0))
boxplot(Cond~ plant_group, data=chem_closed, ylab=condlab, xaxt='n', yaxt='n',
        col=pgcols, ylim=c(0,0.26), outline=FALSE)
axis(2, las=1)
text(x=0.5, y=0.26, label = "A", cex=1.25)

par(mar=c(0,0,1,5.5))
boxplot(wue ~ plant_group, data=chem_closed, ylab="",xaxt='n', 
        col=pgcols, ylim=c(0,16), outline=FALSE, yaxt='n')
axis(4,las=1)
mtexti(wuelab, 4, outer=TRUE, cex=1, off=.6)
text(x=0.5, y=16, label = "B", cex=1.25)

par(mar=c(5.5,5.5,0,0))
boxplot(n_perc ~ plant_group, data=chem_closed, ylab="Foliar N content  (%)",
        col=pgcols, ylim=c(0,5), yaxt='n', names=plants,outline=FALSE)
axis(2, las=1)
text(x=0.5, y=4.9, label = "C", cex=1.25)

par(mar=c(5.5,0,0,5.5))
boxplot(p_perc ~ plant_group, data=phos, ylab="",
        col=pgcols, ylim=c(0,.425), yaxt='n',names=plants)
axis(4,las=1)
mtexti("Foliar P content  (%)", 4, outer=TRUE, cex=1, off=.6)
text(x=0.5, y=.42, label = "D", cex=1.25)

# dev.copy2pdf(file= "output/shade_plants.pdf")
# dev.off()
