## phtosynthetic rate density by plant type & habitat

photo <- read.csv("raw_data/photo_chem.csv")
library(vioplot)
library(ggplot2)
library(Hmisc)

data_summary <- function(x) {
  m <- mean(x)
  ymin <- m-sd(x)
  ymax <- m+sd(x)
  return(c(y=m,ymin=ymin,ymax=ymax))
}

angio <- photo[photo$plant_group == "Angio", "Photo"]
fern <- photo[photo$plant_group == "Fern", "Photo"]
lyco <- photo[photo$plant_group == "Lyco", "Photo"]

open <- photo[photo$canopy == "Open", "Photo"]
closed <- photo[photo$canopy == "Closed", "Photo"]

par(mfrow=c(1,2))
##plant group
par(mar=c(5,5,1,0))
plot(0:1,0:1,type="n",xlim=c(0.5,3.5), ylim=c(0,25), xaxt='n', ylab="",
     xlab="")
vioplot(lyco, fern, angio,at=1:3 ,add=TRUE,
        col="grey98", lwd=2,rectCol="grey60", colMed="black", pchMed=16, wex=.75)
axis(2, labels=FALSE, tcl=.25)
axis(1, labels = c("lycophytes","ferns","angio"), at=1:3)

##habitat
par(mar=c(5,0,1,1))
plot(0:1,0:1,type="n",xlim=c(0.5,2.5), ylim=c(0,25), xaxt='n', yaxt='n',ylab="",
     xlab="")
vioplot(open, closed,at=1:2 ,add=TRUE,
        col="grey98", lwd=2,rectCol="grey60", colMed="black", pchMed=16, wex=.75)
axis(2, labels=FALSE, tcl=.25)
axis(1, labels = c("open","closed"), at=1:2)

##using ggplot
# p <- ggplot(photo, aes(x=plant_group, y=Photo, color=bla)) + 
#   geom_violin() 
# 
# p + theme_classic() + scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9")) + stat_summary(fun.data=mean_sdl, na.rm=TRUE, 
#                                                                                                    geom="pointrange", color="red")
# + geom_jitter(shape=16, position=position_jitter(0.1))
