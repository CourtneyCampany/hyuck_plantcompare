##merge all datasets into master for PCA
library(dplyr)

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
  hyuck_nona$CN <- with(hyuck_nona$c_perc/ hyuck_nona$n_perc)
  hyuck_nona$NP <- with(hyuck_nona$n_perc/ hyuck_nona$p_perc)
  
#get rid of some parameters
hyuck_pca <- droplevels(hyuck_nona[,-c(1:2,6:7,9:13,20:21)])


library(vegan)

##look at variances
# var(hyuck_nona$Photo)
# var(hyuck_nona$p_perc)
#variances among variables are orders of magnitude different so we will rescale

test_dca <- decorana(na.omit(hyuck_pca))
summary(test_dca, display = 'none')
#since axis length is less than 3 so we use PCA

#principle compoent analysis with scales variances
hyuck_rda<- rda(hyuck_pca,scale=T)
plot(hyuck_rda)
summary(hyuck_rda)

#nicer plot

row.names(spp) <- c("An", "PHI","Rd", "LCP", "Photo", "N", "C", "P", "gs", "Ci",
                    "SD", "WUE")
len <- .8

library(scales)
sites <- scores(hyuck_rda, display='sites')
spp <- scores(hyuck_rda, display='species')

#plotting
par(mar=c(5,5,2,2), las=1,cex.axis=0.8)
plot(sites,ylab="PC 2 (19.8 %)", xlab="PC 1 (37.8 %)",type='n',
     xlim=c(-2.5, 2.5), ylim=c(-2.5, 2.5))
abline(v=0, lty='dashed')
abline(h=0, lty='dashed')
points(sites,cex=1.5)
arrows(0, 0, len * spp[, 1],  len * spp[, 2], length = 0.05)
text(spp,labels=rownames(spp),cex=1)
                        