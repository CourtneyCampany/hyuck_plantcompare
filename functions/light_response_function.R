##light response function

fit_AQ_curve <- function(df, group_id, Photo, PARi, fit_type = "onls"){
  AQ_curve_fits <- data.frame(ID = character(),
                              Asat = numeric(),
                              Phi = numeric(),
                              Rd = numeric(),
                              theta = numeric(),
                              resid_SSs = numeric(),
                              LCP = numeric(),
                              Q_sat_75 = numeric(),
                              Q_sat_85 = numeric(),  
                              stringsAsFactors = FALSE
  )
  if(fit_type == "onls"){
    if(require("onls")){
      print("onls is loaded correctly")
    } else {
      print("trying to install onls")
      install.packages("onls")
      if(require("onls")){
        print("onls installed and loaded")
      } else {
        stop("could not install onls")
      }
    }
    library("onls")      
    for(i in seq_along(unique(df[[group_id]]))){
      tryCatch({
        AQ_curve_fits[i, 1] <- unique(df[[group_id]])[i]
        # Subset by group_ID iteratively:
        single_curve1 <- df[df[[group_id]] == unique(df[[group_id]])[i],]
        single_curve1$assim <- single_curve1[[Photo]]
        single_curve1$PAR <- single_curve1[[PARi]]
        single_curve = single_curve1[order(single_curve1$PAR),]
        phi.as.slope <- with(single_curve,
                             as.numeric(coef(lm(
                               assim[1:5] ~ PAR[1:5]))[2]))
        # Fit the curve:
        temp.fit <- with(single_curve, # use the subset of a single curve
                         onls(assim ~ ((Phi * PAR + Asat - 
                                          sqrt((Phi * PAR + Asat)^2 - 
                                                 4 * Phi * theta * 
                                                 Asat * PAR ))
                         )/(2*theta) - Rd,
                         start=list(
                           Asat = (max(assim)),
                           Phi = phi.as.slope,
                           Rd = -min(assim),
                           theta = 0.5),
                         control = list(maxiter = 50)#,
                         #algorithm = "port"
                         )
        )
        AQ_curve_fits[i, 2] <- as.numeric(coef(temp.fit)[1]) # asat 
        AQ_curve_fits[i, 3] <- as.numeric(coef(temp.fit)[2]) # Phi
        AQ_curve_fits[i, 4] <- as.numeric(coef(temp.fit)[3]) # Rd
        AQ_curve_fits[i, 5] <- as.numeric(coef(temp.fit)[4]) # theta
        AQ_curve_fits[i, 6] <- sum(resid(temp.fit)^2)
        AQ_curve_fits[i, 7] <- (as.numeric(coef(temp.fit)[3]) *(
          as.numeric(coef(temp.fit)[3]) * as.numeric(coef(temp.fit)[4]) - 
            as.numeric(coef(temp.fit)[1]))
        ) / (as.numeric(coef(temp.fit)[2]) * (
          as.numeric(coef(temp.fit)[3]) - as.numeric(coef(temp.fit)[1])
        ))
        AQ_curve_fits[i, 8] <- (
          (as.numeric(coef(temp.fit)[1]) * 0.75 + 
             (as.numeric(coef(temp.fit)[3]))) * (
               as.numeric(coef(temp.fit)[1]) * 0.75 *
                 as.numeric(coef(temp.fit)[4]) +
                 as.numeric(coef(temp.fit)[3]) *
                 as.numeric(coef(temp.fit)[4]) -
                 as.numeric(coef(temp.fit)[1])
             )) / (
               as.numeric(coef(temp.fit)[2])* (
                 as.numeric(coef(temp.fit)[1]) * 0.75 +
                   as.numeric(coef(temp.fit)[3]) -
                   as.numeric(coef(temp.fit)[1])
               ))
        
        AQ_curve_fits[i, 9] <- (
          (as.numeric(coef(temp.fit)[1]) * 0.85 + 
             (as.numeric(coef(temp.fit)[3]))) * (
               as.numeric(coef(temp.fit)[1]) * 0.85 *
                 as.numeric(coef(temp.fit)[4]) +
                 as.numeric(coef(temp.fit)[3]) *
                 as.numeric(coef(temp.fit)[4]) -
                 as.numeric(coef(temp.fit)[1])
             )) / (
               as.numeric(coef(temp.fit)[2])* (
                 as.numeric(coef(temp.fit)[1]) * 0.85 +
                   as.numeric(coef(temp.fit)[3]) -
                   as.numeric(coef(temp.fit)[1])
               ))
      }, error = function(E){cat("Error: ", conditionMessage(E), "\n")})
    }
    return(AQ_curve_fits)
  } else{
    if(fit_type == "nls"){
      for(i in seq_along(unique(df[[group_id]]))){
        tryCatch({
          AQ_curve_fits[i, 1] <- unique(df[[group_id]])[i]
          # Subset by group_ID iteratively:
          single_curve1 <- df[df[[group_id]] == unique(df[[group_id]])[i],]
          single_curve1$assim <- single_curve1[[Photo]]
          single_curve1$PAR <- single_curve1[[PARi]]
          single_curve = single_curve1[order(single_curve1$PAR),]
          phi.as.slope <- with(single_curve,
                               as.numeric(coef(lm(
                                 assim[1:5] ~ PAR[1:5]))[2]))
          # Fit the curve:
          temp.fit <- with(single_curve, 
                           nls(assim ~ ((Phi * PAR + Asat - 
                                           sqrt((Phi * PAR + Asat)^2 - 
                                                  4 * Phi * theta * 
                                                  Asat * PAR ))
                           )/(2*theta) - Rd,
                           start=list(
                             Asat = (max(assim)),
                             Phi = phi.as.slope,
                             Rd = -min(assim),
                             theta = 0.5),
                           control = list(maxiter = 50),
                           algorithm = "port")
          )
          AQ_curve_fits[i, 2] <- as.numeric(coef(temp.fit)[1]) # asat 
          AQ_curve_fits[i, 3] <- as.numeric(coef(temp.fit)[2]) # Phi
          AQ_curve_fits[i, 4] <- as.numeric(coef(temp.fit)[3]) # Rd
          AQ_curve_fits[i, 5] <- as.numeric(coef(temp.fit)[4]) # theta
          AQ_curve_fits[i, 6] <- sum(resid(temp.fit)^2)
          AQ_curve_fits[i, 7] <- (as.numeric(coef(temp.fit)[3]) *(
            as.numeric(coef(temp.fit)[3]) * 
              as.numeric(coef(temp.fit)[4]) - 
              as.numeric(coef(temp.fit)[1]))
          ) / (as.numeric(coef(temp.fit)[2]) * (
            as.numeric(coef(temp.fit)[3]) - 
              as.numeric(coef(temp.fit)[1])
          ))
          AQ_curve_fits[i, 8] <- (
            (as.numeric(coef(temp.fit)[1]) * 0.75 + 
               (as.numeric(coef(temp.fit)[3]))) * (
                 as.numeric(coef(temp.fit)[1]) * 0.75 *
                   as.numeric(coef(temp.fit)[4]) +
                   as.numeric(coef(temp.fit)[3]) *
                   as.numeric(coef(temp.fit)[4]) -
                   as.numeric(coef(temp.fit)[1])
               )) / (
                 as.numeric(coef(temp.fit)[2])* (
                   as.numeric(coef(temp.fit)[1]) * 0.75 +
                     as.numeric(coef(temp.fit)[3]) -
                     as.numeric(coef(temp.fit)[1])
                 ))
          AQ_curve_fits[i, 9] <- (
            (as.numeric(coef(temp.fit)[1]) * 0.85 + 
               (as.numeric(coef(temp.fit)[3]))) * (
                 as.numeric(coef(temp.fit)[1]) * 0.85 *
                   as.numeric(coef(temp.fit)[4]) +
                   as.numeric(coef(temp.fit)[3]) *
                   as.numeric(coef(temp.fit)[4]) -
                   as.numeric(coef(temp.fit)[1])
               )) / (
                 as.numeric(coef(temp.fit)[2])* (
                   as.numeric(coef(temp.fit)[1]) * 0.85 +
                     as.numeric(coef(temp.fit)[3]) -
                     as.numeric(coef(temp.fit)[1])
                 ))
        }, error = function(E){
          cat("Error: ", conditionMessage(E), "\n")})
      }
      return(AQ_curve_fits)      
    } else{print("ERROR: 'fit_type' specified incorrectly.")}
  }
}


###plotting function
diagnostic_AQ_plot <- function(curve_data, fit_data, Photo, PARi, group_id,
                               save_to_pdf = FALSE, save_path, file_name){
  if(save_to_pdf == FALSE){ 
    par(mar = c(3, 3, 1, 1), oma = c(1, 1, 1, 1))
    for(i in seq_along(1:length(unique(curve_data[[group_id]])))){
      single_curve <- 
        curve_data[curve_data[[group_id]] == 
                     unique(curve_data[[group_id]])[i],]
      plot(
        single_curve[[Photo]] ~ single_curve[[PARi]] ,
        xlim = c(-2, max(curve_data[[PARi]])), 
        ylim = c(min(curve_data[[Photo]]) - 2,
                 max(curve_data[[Photo]]) + 2),
        pch = 3,
        cex = 2,
        xlab = "",
        ylab = "",
        main = paste("Data from curve ",
                     as.character(
                       unique(single_curve[[group_id]])))
      )
      mtext(expression("Photo (µmol "*CO[2]*" "*m^-2*" "*s^-1*")"),
            line = 2.4, side = 2)
      mtext(expression("PARi (µmol photons "*m^-2*" "*s^-1*")"),
            line = 2.4, side = 1)
      par(new = TRUE)
      curve(((
        fit_data$Phi[i] * PARi + fit_data$Asat[i] - 
          sqrt((fit_data$Phi[i] * PARi + fit_data$Asat[i])^2 - 4 *
                 fit_data$Phi[i] * fit_data$theta[i] * PARi *
                 fit_data$Asat[i])
      ) / (2*fit_data$theta[i]) - fit_data$Rd[i]),
      from = 0, to = 1600, 
      xname = "PARi",
      xlab = "", ylab = "", 
      xlim = c(-2, max(curve_data[[PARi]])), 
      ylim = c(min(curve_data[[Photo]]) - 2,
               max(curve_data[[Photo]]) + 2),
      axes = FALSE,
      col = "red",
      lwd = 2
      )
    }} else{
      if(dir.exists(save_path)){
        pdf(paste0(save_path, file_name, ".pdf"))
        par(mar = c(3, 3, 1, 1), oma = c(1, 1, 1, 1))
        for(i in seq_along(1:length(unique(curve_data[[group_id]])))){
          single_curve <- 
            curve_data[curve_data[[group_id]] == 
                         unique(curve_data[[group_id]])[i],]
          plot(
            single_curve[[Photo]] ~ single_curve[[PARi]] ,
            xlim = c(-2, max(curve_data[[PARi]])), 
            ylim = c(min(curve_data[[Photo]]) - 2,
                     max(curve_data[[Photo]]) + 2),
            pch = 3,
            cex = 2,
            xlab = "",
            ylab = "",
            main = paste("Data from curve ",
                         as.character(
                           unique(single_curve[[group_id]])))
          )
          mtext(expression("Photo (µmol "*CO[2]*" "*m^-2*" "*s^-1*")"),
                line = 2.4, side = 2)
          mtext(expression("PARi (µmol photons "*m^-2*" "*s^-1*")"),
                line = 2.4, side = 1)
          par(new = TRUE)
          curve(((
            fit_data$Phi[i] * PARi + fit_data$Asat[i] - 
              sqrt((fit_data$Phi[i] * PARi + fit_data$Asat[i])^2 - 4 *
                     fit_data$Phi[i] * fit_data$theta[i] * PARi *
                     fit_data$Asat[i])
          ) / (2*fit_data$theta[i]) - fit_data$Rd[i]),
          from = 0, to = 1600, 
          xname = "PARi",
          xlab = "", ylab = "", 
          xlim = c(-2, max(curve_data[[PARi]])), 
          ylim = c(min(curve_data[[Photo]]) - 2,
                   max(curve_data[[Photo]]) + 2),
          axes = FALSE,
          col = "red",
          lwd = 2
          )
        }
        dev.off()
      } else {
        return(
          "Warning: the file path provided to save_path does not exist"
        )}
      
    }
}
