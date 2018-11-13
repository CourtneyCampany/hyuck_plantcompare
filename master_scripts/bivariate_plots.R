### bivariate relationships

chem <- read.csv("raw_data/photo_chem.csv")
stom <- read.csv("raw_data/stomatal_density.csv")
spad <- read.csv("raw_data/chlorophyll.csv")

dat <- merge(chem, spad, all=TRUE)


library(doBy)
stom_agg <- summaryBy(sto_den ~ species + individual, FUN=mean, data=stom, 
                      keep.names = TRUE)

dat2 <- merge(dat, stom_agg, all=TRUE)

plot(Photo~n_perc, data= chem, col=plant_group, pch=16)


plot(n_perc ~ spad, data=dat2, col=plant_group, pch=16)
