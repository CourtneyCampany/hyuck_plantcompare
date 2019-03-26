#plotobjects

condtab <- expression(atop(italic(g)[s],(mol~m^-2~s^-1)))

phototab <- expression(atop(italic(A[n]),(mu*mol~m^-2~s^-1)))

resptab <- expression(atop(italic(R[d]),(mu*mol~m^-2~s^-1)))

nareatab <- "Foliar~Nitrogen\n (%)"
pareatab <- "Foliar~Phosphorous\n (%)"

lcptab <- expression(atop(LCP,(mu*mol~m^-2~s^-1)))
denstab <- expression(atop(Stomatal~Density,(mm^-2)))

phitab<- expression(paste(phi, "  (mol ", " CO" ["2"], "  m",ol^-1, " photons)"))

wuetab <- expression(atop(WUE, paste("(",mu*mol," CO" ["2"]," mmol ",
                                     H[2],O^-1,")")))


variables <- c("Plant Lineage", "Canopy", phototab,condtab,nareatab,
                    pareatab, "N:P", wuetab, denstab, phitab, resptab, lcptab)
