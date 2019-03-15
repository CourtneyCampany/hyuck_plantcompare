##lady fern
library(dplyr)
library(stringr)

treatments <- read.csv("raw_data/species_list.csv")

photochem <- read.csv("raw_data/photo_chem.csv")

#no comparison with AQ curves or stomata
##no phosphorous data

#subset lady fern 
lady <- droplevels(photochem[photochem$species %in% c("ath_fil_med", "ath_fil_high"),])
  lady$wue <- with(lady, Photo/Trmmol)
  lady$cn <- with(lady, c_perc/n_perc)

boxplot(Photo ~ species, data=lady)
boxplot(Cond ~ species, data=lady)
boxplot(n_perc ~ species, data=lady)
boxplot(cn ~ species, data=lady)
boxplot(wue ~ species, data=lady)
