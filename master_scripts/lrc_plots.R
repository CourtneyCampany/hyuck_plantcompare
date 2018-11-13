##light response figures

aq <- read.csv("calculated_data/AQ_params.csv")
  aq$species <- gsub("-\\d+","", aq$ID)
  aq$individual <- gsub(".*-","", aq$ID)

treatments <- read.csv("raw_data/species_list.csv")
library(vioplot)
  
aq2 <- merge(aq, treatments[,c(2,4:5)])


##boxplots (habitat overall)

boxplot(Phi ~ plant_group, data=aq2, outline = FALSE, 
        ylab =  "Quantum Yield") # Quantum yield

boxplot(Rd ~ plant_group, data=aq2, outline = FALSE, 
        ylab =  "Rd") # Mitochondrial respiration in the light (Rd)

boxplot(LCP ~ plant_group, data=aq2, outline = FALSE, 
        ylab =  "Light compensation point") # The light compensation point (LCP)

## groups by habitat
boxplot(Phi ~ canopy, data=aq2, outline = FALSE, 
        ylab =  "Quantum Yield") # Quantum yield

boxplot(Rd ~ canopy, data=aq2, outline = FALSE, 
        ylab =  "Rd") # Mitochondrial respiration in the light (Rd)

boxplot(LCP ~ canopy, data=aq2, outline = FALSE, 
        ylab =  "Light compensation point") # The light compensation point (LCP)

##by group x habitat
aq2$uniqueid<- paste(aq2$plant_group, aq2$canopy, sep="-")

boxplot(Phi ~ uniqueid, data=aq2, outline = FALSE,xaxt='n',at=c(1:2, 4:5,7),
        ylab="Quantum Yield")
axis(1, labels = c("Closed", "Open", "Closed", "Open"), at=c(1:2, 4:5,7))
mtext(text = c("Angiosperms","Ferns"), side=1, line=2.5,at=c(1.5, 4.5,7))

boxplot(Rd ~ uniqueid, data=aq2, outline = FALSE,xaxt='n',at=c(1:2, 4:5,7),
        ylab="Rd")
axis(1, labels = c("Closed", "Open", "Closed", "Open", "Closed"), at=c(1:2, 4:5,7))
mtext(text = c("Angiosperms","Ferns","Lycophytes"), side=1, 
      line=2.5,at=c(1.5, 4.5,7))

boxplot(LCP ~ uniqueid, data=aq2, outline = FALSE,xaxt='n',at=c(1:2, 4:5,7),
        ylab="Light Compensation Point")
axis(1, labels = c("Closed", "Open", "Closed", "Open","Closed"), at=c(1:2, 4:5,7))
mtext(text = c("Angiosperms","Ferns","Lycophytes"), side=1, 
      line=2.5,at=c(1.5, 4.5,7))
