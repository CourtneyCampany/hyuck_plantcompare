#plotobjects

condlab <- expression(italic(g)[s]~~(mol~m^-2~s^-1))
condlab2 <- expression(italic(g)[s])

photolab <- expression(italic(A[n])~~(mu*mol~m^-2~s^-1))
photolab2 <- expression(italic(A[n]))

resplab <- expression(italic(R[d]))

narealab <- expression(Leaf~Nitrogen[area]~~(mg~m^-2))
parealab <- expression(Leaf~Phosphorous[area]~~(mg~m^-2))

cilab <- expression(italic(C)[i]~~(ppm))

lcplab <- expression(LCP~~(mu*mol~m^-2~s^-1))
denslab <- expression(Stomatal~Density~~(mm^-2))

philab <- expression(phi)

#list of labels for pca plots
pcalabs <- c(philab, resplab,"LCP", photolab2, "N", "P", condlab2, "SD", "WUE", 
             "C:N", "N:P")
