source("functions/basic_functions.R")
library(doBy)
library(dplyr)
library(stringr)

treatments <- read.csv("raw_data/species_list.csv")

##photosynthesis and leaf chemistry data-------------
photochem <- read.csv("raw_data/photo_chem.csv")
  photochem$np_ratio <- with(photochem, n_perc/p_perc)
  photochem$wue <- with(photochem, Photo/Trmmol)

photochem_mean <- summaryBy(Photo+Cond+n_perc+p_perc+np_ratio+wue ~ 
                              plant_group+canopy, 
                              data=photochem, FUN=mean2)

photochem_se <- summaryBy(Photo+Cond+n_perc+p_perc+np_ratio+wue ~ 
                              plant_group+canopy, 
                            data=photochem, FUN=se)

#stomatal density data-----------
stom <- read.csv("raw_data/stomatal_density.csv")

#mean for each individual (multiple stomatal counts)
stom_agg <- doBy::summaryBy(sto_den ~ species + individual + plant_group,
                            FUN=mean, data=stom, keep.names = TRUE)

stom_dens <- merge(stom_agg, treatments[,c(4:5)])

stom_mean <- doBy::summaryBy(sto_den ~ plant_group + canopy,
                             FUN=c(mean2, se), data=stom_dens)


##aq curve data
lrc <- read.csv("calculated_data/AQ_params.csv") %>%
  mutate(species = as.factor(str_remove(ID, "-[[:digit:]]")),
         individual =  str_remove(ID, ".*-")) %>%
  merge(treatments[,c(2,4:5)])

lrc_agg <- doBy::summaryBy(Phi + Rd + LCP ~ plant_group + canopy, 
                           data=lrc, FUN=c(mean2, se))


#format data table
photo <- data.frame(paste0(sprintf("%2.1f", round(photochem_mean[,3], 1)), 
                    " (", sprintf("%2.2f", round(photochem_se[,3],2)),")"))

cond <- data.frame(paste0(sprintf("%1.2f", round(photochem_mean[,4], 2)), 
                           " (", sprintf("%1.3f", round(photochem_se[,4],3)),")"))

nitro <- data.frame(paste0(sprintf("%1.1f", round(photochem_mean[,5], 1)), 
                          " (", sprintf("%1.2f", round(photochem_se[,5],2)),")"))

phos <- data.frame(paste0(sprintf("%1.2f", round(photochem_mean[,6], 2)), 
                          " (", sprintf("%1.3f", round(photochem_se[,6],3)),")"))

np <- data.frame(paste0(sprintf("%2.1f", round(photochem_mean[,7], 1)), 
                       " (", sprintf("%2.2f", round(photochem_se[,7],2)),")"))

wue <- data.frame(paste0(sprintf("%1.1f", round(photochem_mean[,8], 1)), 
                        " (", sprintf("%1.2f", round(photochem_se[,8],2)),")"))

sd <- data.frame(paste0(sprintf("%2.1f", round(stom_mean[,3], 1)), 
                       " (", sprintf("%2.2f", round(stom_mean[,4],2)),")"))

phi <- data.frame(paste0(sprintf("%1.3f", round(lrc_agg[,3], 3)), 
                       " (", sprintf("%1.4f", round(lrc_agg[,6],4)),")"))

rd <- data.frame(paste0(sprintf("%1.2f", round(lrc_agg[,4], 2)), 
                        " (", sprintf("%1.3f", round(lrc_agg[,7],3)),")"))

lcp <- data.frame(paste0(sprintf("%1.1f", round(lrc_agg[,5], 1)), 
                       " (", sprintf("%1.2f", round(lrc_agg[,8],2)),")"))


plantcanopy <- data.frame(lineage = c("Angiosperm", "", "Fern", "", "Lycophyte"),
                          canopy = c("Closed", "Open", "Closed", "Open", "Closed"))

data_tab <- cbind(plantcanopy, photo)
data_tab <- cbind(data_tab, cond)
data_tab <- cbind(data_tab, nitro)
data_tab <- cbind(data_tab, phos)
data_tab <- cbind(data_tab, np)
data_tab <- cbind(data_tab, wue)
data_tab <- cbind(data_tab, sd)
data_tab <- cbind(data_tab, phi)
data_tab <- cbind(data_tab, rd)
data_tab <- cbind(data_tab, lcp)

names(data_tab) <- c("lineage", "canopy", "photosyn", "gs", "nitro", "phos", "np",
                     "wue", "sd", "phi", "rd", "lcp")

write.csv(data_tab, "manuscript/pooled_data.csv", row.names = FALSE)
