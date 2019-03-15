##light response

lrc <- read.csv("raw_data/lightresponse.csv")
lrc$id <- paste(lrc$species, lrc$individual, sep="-")

source("functions/light_response_function.R")

##fit light response curves for every individual

hyuck_lrc <- fit_AQ_curve(lrc, 
      Photo = "Photo", 
      PARi = "PARi", 
      group_id = "id") 

diagnostic_AQ_plot(lrc, hyuck_lrc,
                   Photo = "Photo", 
                   PARi = "PARi", 
                   group_id = "id",
                   save_to_pdf = TRUE,
                   save_path = "output",
                   file_name = "LRC_hyuck_plots")

write.csv(hyuck_lrc, "calculated_data/AQ_params.csv", row.names = FALSE)

##trying to learn ggplots

# library(ggplot2)
# angio <- droplevels(lightresponse[lightresponse$plant_group == 'Angiosperm',])
# open <- droplevels(lightresponse[lightresponse$canopy == "Open",])
# closed <- droplevels(lightresponse[lightresponse$canopy == "Closed",])
# 
# 
# ggplot(data=lightresponse) +
#  geom_smooth(mapping = aes(x=PARi, y=Photo, color=plant_group)) +
#    geom_point(mapping = aes(x=PARi, y=Photo, color=plant_group))
# 
# 
# 
# ##appears to be some big differences in angiosperm, is it canopy?
# ggplot(data=angio) +
#   # geom_smooth(mapping = aes(x=PARi, y=Photo, linetype=species)) +
#   geom_point(mapping = aes(x=PARi, y=Photo, color=species)) + theme(plot.subtitle = element_text(vjust = 1), 
#     plot.caption = element_text(vjust = 1), 
#     axis.line = element_line(size = 0.3, 
#         linetype = "solid"), panel.grid.minor = element_line( linetype = "blank"), 
#         panel.background = element_rect(fill = NA, 
#         linetype = "solid")) + theme(panel.grid.minor = element_line(colour = NA))
# 
# ##looks like just one species
# 
# 
# ##differences within canopy types
# ggplot(data=open) +
#   geom_smooth(mapping = aes(x=PARi, y=Photo, color=plant_group)) +
#   geom_point(mapping = aes(x=PARi, y=Photo, shape=species, color=plant_group))
# 
# ggplot(data=closed) +
#   geom_smooth(mapping = aes(x=PARi, y=Photo, color=plant_group)) +
#   geom_point(mapping = aes(x=PARi, y=Photo, shape=species, color=plant_group))
# 
# ggplot(data=closed, aes(PARi, Photo)) +
#   geom_smooth(mapping=aes(color=plant_group), span=1) 
#   geom_point(mapping = aes(x=PARi, y=Photo, shape=species, color=plant_group))
#   
# ggplot(data=closed, aes(PARi, Photo)) +
#   geom_point(mapping = aes(color=plant_group))
# 
# 
# 
# 
# 
# 
