source("master_scripts/plot_objects.R")
source("functions/basic_functions.R")

treatments <- read.csv("raw_data/species_list.csv")

##lycophytes differ

#gas exchange data
chem <- read.csv("raw_data/photo_chem.csv")

chem_closed <- chem[chem$canopy == "Closed",]
  chem_closed$seed <- ifelse(chem_closed$plant_group == "Angio", 
                             "Seed", "Non-seed")
  
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
par(mfrow=c(2,2), mgp=c(3,.75,0))

#stomatal conductance
par(mar=c(0,5,1,0))
boxplot(Cond ~ seed, data=chem_closed, ylab=condlab,xaxt='n',yaxt='n',
        ylim=c(0,.3), outline=FALSE, col=seedcols)
axis(2, las=1)

#quantum yield
par(mar=c(0,0,1,5))
boxplot(Phi ~ seed, data=lrc_shade, ylab="", xaxt='n', yaxt='n',
        ylim=c(0.04,.09),  col=seedcols)
axis(4,las=1)
mtexti(philab2, 4, outer=TRUE, cex=1, off=.65)

#light compensation point
par(mar=c(5,5,0,0))
boxplot(LCP ~ seed, data=lrc_shade, ylab=lcplab,outline=FALSE,yaxt='n',
        ylim=c(0.,27),  col=c("white", plantcols2[2]))
axis(2, las=1)

#dark respiration
par(mar=c(5,0,0,5))
boxplot(Rd ~ seed, data=lrc_shade, ylab="", outline=FALSE,
        ylim=c(0,1.75),  col=c("white", plantcols2[2]),yaxt='n')
axis(4,las=1)
mtexti(resplab2, 4, outer=TRUE, cex=1, off=.65)

# dev.copy2pdf(file= "output/seed_nonseed_plot.pdf")
# dev.off()