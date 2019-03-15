### clean up aq curve data
treatments <- read.csv("raw_data/species_list.csv")


#photosynthesis
lrc <- read.csv("calculated_data/AQ_params.csv") %>%
  mutate(species = as.factor(str_remove(ID, "-[[:digit:]]")),
         individual =  str_remove(ID, ".*-")) %>%
  merge(treatments[,c(2,4:5)]) %>%
  mutate(canopyplant = interaction(canopy, plant_group))

boxplot(LCP ~ species, data=lrc_clean)
boxplot(Phi ~ species, data=lrc_clean)
boxplot(Rd ~ species, data=lrc_clean)

#drop some obvious outliers
outlierLCP <- max(lrc$LCP)
#lys_cil-2 appears like a bad curve

lrc_clean <- lrc[!lrc$ID %in% c("lys_cil-2","dip_dig-3","eup_pur-5"),]

write.csv(lrc_clean, "calculated_data/AQ_params_clean.csv", row.names = FALSE)
