#plotobjects

condlab <- expression(italic(g)[s]~~(mol~m^-2~s^-1))
condlab2 <- expression(italic(g)[s])

photolab <- expression(italic(A[n])~~(mu*mol~m^-2~s^-1))
photolab2 <- expression(italic(A[n]))

resplab <- expression(italic(R[d]))
resplab2 <- expression(italic(R[d])~~(mu*mol~m^-2~s^-1))

narealab <- expression(Leaf~Nitrogen[area]~~(mg~m^-2))
parealab <- expression(Leaf~Phosphorous[area]~~(mg~m^-2))

cilab <- expression(italic(C)[i]~~(ppm))

lcplab <- expression(LCP~~(mu*mol~m^-2~s^-1))
denslab <- expression(Stomatal~Density~~(mm^-2))

philab <- expression(phi)
philab2 <- expression(paste(phi, "  (mol ", " CO" ["2"], "  m",ol^-1, " photons)"))

wuelab <- expression(atop(WUE, paste("(",mu*mol," CO" ["2"]," mmol ",
                                     H[2],O^-1,")")))

#list of labels for pca plots
pcalabs <- c(philab, resplab,"LCP", photolab2, "N", "P", condlab2, "SD", "WUE", 
             "C:N", "N:P")


library(scales)
#plant group colors
plantcols <- c(alpha("dodgerblue", .8), alpha("firebrick", .8), 
               alpha("forestgreen", .8))
plantcols2 <- c("dodgerblue", "firebrick","forestgreen")


pgcols <- c("firebrick", "forestgreen", "dodgerblue")
plants <- c("Angiosperms","Ferns","Lycophytes")

seedcols <- c("white", plantcols2[2])
