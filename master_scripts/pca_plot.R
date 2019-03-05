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
  hyuck_nona$CN <- with(hyuck_nona, c_perc/ n_perc)
  hyuck_nona$NP <- with(hyuck_nona, n_perc/ p_perc)
  
hyuck_shade <- hyuck_nona[hyuck_nona$canopy == "Closed",]  
  
#get rid of some parameters
hyuck_pca <- droplevels(hyuck_shade[,-c(1:2,6:7,9:13,16,19:21)])

library(scales)
#plant group colors
plantcols <- c(alpha("dodgerblue", .8), alpha("firebrick", .8), 
               alpha("forestgreen", .8))

#site variables for ease with ponts in pca
hyuck_id <- hyuck_shade[,12:13]

##with alpha
hyuck_id$plantcols <- ifelse(hyuck_id$plant_group == "Angiosperm", plantcols[2],
                             plantcols[1])
hyuck_id$plantcols <- ifelse(hyuck_id$plant_group == "Fern", plantcols[3],
                             hyuck_id$plantcols)

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

#nicer plot

len <- .8

library(scales)
sites <- scores(hyuck_rda, display='sites')
spp <- scores(hyuck_rda, display='species')

row.names(spp) <- c("An", "PHI","Rd", "LCP", "Photo", "N", "P", "gs", 
                    "SD", "WUE", "CN", "NP")

#plotting compare ferns and angio lyco in shade
windows()
par(mar=c(5,5,2,2), las=1,cex.axis=0.8)
plot(sites,ylab="PC 2 (16.0 %)", xlab="PC 1 (41.5%)",type='n',
     xlim=c(-2.25, 2.25), ylim=c(-2.25, 2.25))
abline(v=0, lty='dashed')
abline(h=0, lty='dashed')
points(sites,cex=1.75, bg=hyuck_id$plantcols, pch=21)
arrows(0, 0, len * spp[, 1],  len * spp[, 2], length = 0.05)
text(spp,labels=rownames(spp),cex=1.2)
legend("topleft", legend= c("Angiosperms", "Ferns", "Lycophytes"),
       pch=21, pt.bg=c(plantcols[2], plantcols[3], plantcols[1]),
       inset=0.01, bty='n', cex=1,pt.cex=1)
                        