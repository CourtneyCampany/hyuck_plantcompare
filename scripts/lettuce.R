##light response

lrc <- read.csv("raw_data/lightresponse_lettuce.csv")
lrc$id <- paste(lrc$species, lrc$individual, sep="-")

source("functions/light_response_function.R")

lettuce_aqua_lrc <- fit_AQ_curve(lrc, 
                          Photo = "Photo", 
                          PARi = "PARi", 
                          group_id = "id") 

diagnostic_AQ_plot(lrc, lettuce_aqua_lrc,
                   Photo = "Photo", 
                   PARi = "PARi", 
                   group_id = "id",
                   save_to_pdf = TRUE,
                   save_path = "output",
                   file_name = "LRC_lettuce_plots")

write.csv(lettuce_aqua_lrc, "calculated_data/AQ_params_lettuce.csv", row.names = FALSE)
