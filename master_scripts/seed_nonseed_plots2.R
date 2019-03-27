source("master_scripts/plot_objects.R")
source("functions/basic_functions.R")
library(dplyr)
library(stringr)

treatments <- read.csv("raw_data/species_list.csv")

##LCP, Rd, gs, and P differe by group (seed vs non-seed)

#gas exchange data
chem <- read.csv("raw_data/photo_chem.csv")

chem_closed <- chem[chem$canopy == "Closed",]
  chem_closed$seed <- ifelse(chem_closed$plant_group == "Angio", 
                             "Seed", "Non-seed")
phos_closed <- chem_closed[chem_closed$p_perc > .1 & chem_closed$p_perc < .4, ]
  
#light response curves removing one bad curve
lrc <- read.csv("calculated_data/AQ_params.csv") %>%
    mutate(species = as.factor(str_remove(ID, "-[[:digit:]]")),
           individual =  as.numeric(str_remove(ID, ".*-"))) %>%
    merge(treatments[,c(2,4:5)]) %>%
    filter(!ID ==  "imp_pal-1")
  
lrc_shade <-  droplevels(lrc[!lrc$canopy == "Closed",])
  lrc_shade$seed <- ifelse(lrc_shade$plant_group == "Angiosperm", 
                           "Seed", "Non-seed")


#seed vs nonseed panel figure
# windows()
par(mfrow=c(2,2), mgp=c(3,.75,0),oma=c(4,5,1,5))

#stomatal conductance
par(mar=c(0,0,0,0))
boxplot(Cond ~ seed, data=chem_closed, ylab="",xaxt='n',yaxt='n',
        ylim=c(0,.3), outline=FALSE, col=seedcols)
axis(2, las=1)
text(x=0.5, y=.3, label = "A", cex=1.25)
mtext(side=2, at=.15, line=3,text=condlab, xpd=TRUE, las=3, cex=.9)

#phosphorus
par(mar=c(0,0,0,0))
boxplot(p_perc ~ seed, data=phos_closed, ylab="", xaxt='n', yaxt='n',
        ylim=c(0,0.5),  col=seedcols)
axis(4,las=1)
mtexti("Foliar Phosphorus  (%)", 4, outer=TRUE, cex=1, off=.65)
text(x=0.5, y=0.5, label = "B", cex=1.25)

#light compensation point
par(mar=c(0,0,0,0))
boxplot(LCP ~ seed, data=lrc_shade, ylab="",outline=FALSE,yaxt='n',
        ylim=c(0,27),  col=c("white", plantcols2[2]))
axis(2, las=1)
text(x=0.5, y=26.95, label = "C", cex=1.25)
mtext(side=2, at=13.5, line=3,text=lcplab, xpd=TRUE, las=3, cex=.9)

#dark respiration
par(mar=c(0,0,0,0))
boxplot(Rd ~ seed, data=lrc_shade, ylab="", outline=FALSE,
        ylim=c(0,1.75),  col=c("white", plantcols2[2]),yaxt='n')
axis(4,las=1)
mtexti(resplab2, 4, outer=TRUE, cex=1, off=.65)
text(x=0.5, y=1.74, label = "D", cex=1.25)

# dev.copy2pdf(file= "output/seed_nonseed_plot2.pdf")
# dev.off()