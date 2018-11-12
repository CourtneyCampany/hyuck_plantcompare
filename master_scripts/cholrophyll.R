##chlorophyll content

## need some curvilinear correction for SPAD to CHL

spad <- read.csv("raw_data/chlorophyll.csv")


##boxplots (habitat overall)

boxplot(spad ~ plant_group, data=spad, outline = FALSE)

## groups by habitat
boxplot(spad ~ canopy, data=spad, outline = FALSE)

##by group x habitat
spad$uniqueid<- paste(spad$plant_group, spad$canopy, sep="-")

boxplot(spad ~ uniqueid, data=spad, outline = FALSE,xaxt='n',at=c(1:2, 4:5))
axis(1, labels = c("Closed", "Open", "Closed", "Open"), at=c(1:2, 4:5))
mtext(text = c("Angiosperms","Ferns"), side=1, line=2.5,at=c(1.5, 4.5))
