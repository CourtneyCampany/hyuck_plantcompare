source("master_scripts/plot_objects.R")
source("functions/basic_functions.R")

##lycophytes differ

chem <- read.csv("raw_data/photo_chem.csv")
library(vioplot)

chem_closed <- chem[chem$canopy == "Closed",]
  chem_closed$cnratio <- with(chem_closed, c_perc/n_perc)
  chem_closed$wue <- with(chem_closed, Photo/Trmmol)

phos <- chem_closed[chem_closed$p_perc > .1 & chem_closed$p_perc < .4,]

# windows()
par(mfrow=c(2,2), mgp=c(3,.75,0))

#N content
par(mar=c(0,5.5,1,0))
boxplot(n_perc ~ plant_group, data=chem_closed, ylab="Foliar N content  (%)",
        col=pgcols, ylim=c(0,5), xaxt='n', yaxt='n', outline=FALSE)
axis(2, las=1)

#P content
par(mar=c(0,0,1,5.5))
boxplot(p_perc ~ plant_group, data=phos, ylab="",
        col=pgcols, ylim=c(0,.425), xaxt='n', yaxt='n')
axis(4,las=1)
mtexti("Foliar P content  (%)", 4, outer=TRUE, cex=1, off=.6)

#cn ratio
par(mar=c(5.5,5.5,0,0))
boxplot(cnratio ~ plant_group, data=chem_closed, ylab="Foliar C:N ratio",yaxt='n',
        names=plants,col=pgcols, ylim=c(0,45), outline=FALSE)
axis(2, las=1)

#water use efficiency
par(mar=c(5.5,0,0,5.5))
boxplot(wue ~ plant_group, data=chem_closed, ylab="",names=plants,
        col=pgcols, ylim=c(0,21), outline=FALSE, yaxt='n')
axis(4,las=1)
mtexti(wuelab, 4, outer=TRUE, cex=1, off=.6)

# dev.copy2pdf(file= "output/lycophytes.pdf")
# dev.off()
