source("master_scripts/plot_objects.R")

##merge all datasets into master for PCA
library(dplyr)
library(stringr)

#read and format data-------

treatments <- read.csv("raw_data/species_list.csv")

#light response curves removing one bad curve
lrc <- read.csv("calculated_data/AQ_params.csv") %>%
  mutate(species = as.factor(str_remove(ID, "-[[:digit:]]")),
         individual =  as.numeric(str_remove(ID, ".*-"))) %>%
  merge(treatments[,c(2,4:5)]) %>%
  filter(!ID ==  "imp_pal-1")


#gas exchange and chemistry
photo <- read.csv("raw_data/photo_chem.csv") %>%
  mutate(ID = paste(species, individual, sep="-")) %>%
  filter(!Site == "Colgate") %>%
  select(-"plant_group") %>%
  inner_join(treatments[,c(2,5)], by='species')

##now add stomatal density
stom <- read.csv("raw_data/stomatal_density.csv") %>%
  select(-"plant_group") %>%
  inner_join(treatments[,c(2,4:5)], by='species')

stom_agg <- doBy::summaryBy(sto_den ~ species + individual + plant_group + canopy,
                            FUN=mean, data=stom, keep.names = TRUE) %>%
  mutate(ID = paste(species, individual, sep="-"))

#merge all data -----
hyuck <- inner_join(lrc, photo) %>% inner_join(stom_agg)

#select data for PCA
hyuck_nona <- hyuck[complete.cases(hyuck),]
  hyuck_nona$WUE <- with(hyuck_nona, Photo/Trmmol)
  hyuck_nona$NP <- with(hyuck_nona, n_perc/ p_perc)

hyuck_canopy <- hyuck_nona[!hyuck_nona$plant_group == "Lycophyte",]  

#get rid of some parameters (Asat is from aqcurve fitting so keep Photo)
hyuck_pca <- droplevels(hyuck_canopy[,-c(1:3,6:7,9:13,16,19:21)])

#site variables for ease with ponts in pca
hyuck_id <- hyuck_canopy[,12:13]

##with alpha
hyuck_id$plantcols <- ifelse(hyuck_id$plant_group == "Angiosperm", plantcols[2],
                             plantcols[3])
hyuck_id$canopypch <- ifelse(hyuck_id$canopy == "Closed", 16,1)

#length of shade and id dfrs should be same!

library(vegan)

##look at variances
# var(hyuck_nona$Photo)
# var(hyuck_nona$p_perc)
#variances among variables are orders of magnitude different so we will rescale

# test_dca <- decorana(na.omit(hyuck_pca))
# summary(test_dca, display = 'none')
#since axis length is less than 3 so we use PCA

#principle compoent analysis with scales variances
hyuck_rda<- rda(hyuck_pca,scale=T)
# plot(hyuck_rda)
# summary(hyuck_rda)

len <- .8

sites <- scores(hyuck_rda, display='sites')
spp <- scores(hyuck_rda, display='species')

#plotting compare ferns and angio lyco in shade
jpeg(filename = "manuscript/pcalight.jpeg",
     width = 7, height = 7, units = "in", res= 600)
# windows()
par(mar=c(5,5,1,1), las=1,cex.axis=1.25, cex.lab = 1.5)
plot(sites,ylab="PC 2 (22.3 %)", xlab="PC 1 (33.6%)",type='n',
     xlim=c(-2.25, 2.25), ylim=c(-2.25, 2.25))
abline(v=0, lty='dashed')
abline(h=0, lty='dashed')
ordihull(hyuck_rda, groups = hyuck_canopy$canopy, lwd=2.5,draw='polygon',
         col="grey",alpha=50, border = "black" ,lty=c(1,3))
points(sites,cex=2, pch=hyuck_id$canopypch, col=hyuck_id$plantcols)
arrows(0, 0, len * spp[, 1],  len * spp[, 2], length = 0.05,lwd=3.5)
text(spp,labels=pcalabs,cex=1.5)
legend("bottomright", legend= c("Angiosperms", "Ferns", "Closed", "Open"),
       pch=c(16,16, 16, 1), col=c(plantcols2[2], plantcols2[3], "black", "black"),
       inset=0.01, bty='n', cex=1,pt.cex=1.5,lty=c(0,0,1,3))

# dev.copy2pdf(file= "output/pca_openclosed.pdf")
dev.off()
