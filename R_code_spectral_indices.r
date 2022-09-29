# Setting the working directory
setwd("C:/lab/")

# install.packages("raster")
# install.packages("RStoolbox")

# Recalling the library in order to use raster package funtions 
library(raster)

l1992 <- brick("defor1_.jpg")
l1992

plot(l1992)
plotRGB(l1992, r=1, g=2, b=3, stretch="lin")
# layer1 = NIR
# layer2 = red
# layer3 = green

# Comparison 
par(mfrow=c(2,1))
plotRGB(l1992, r=1, g=2, b=3, stretch="lin")
#
l2006 <- brick("defor2_.jpg")
plotRGB(l1992, r=1, g=2, b=3, stretch="lin")

dev.off()

# DVI Difference Vegetation Index
dvi1992 <- l1992[[1]] - l1992[[2]]
cl <- colorRampPalette(c("blue", "yellow", "red", "black")) (100)
plot(dvi1992, col=cl)

dvi2006 <- l2006[[1]] - l2006[[2]]
plot(dvi2006, col=cl)

# Comparison in DVI index between 1992-2006
par(mfrow=c(2,1))
plot(dvi1992, col=cl)
plot(dvi2006, col=cl)

dev.off()

# DVI difference in time
div_dif = dvi1992 - dvi2006
div_dif

cld <- colorRampPalette(c("blue", "white", "red")) (100)
plot(div_dif, col=cld)
