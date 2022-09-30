# Setting the working directory
setwd("C:/lab/")

# install.packages("raster")
# install.packages("RStoolbox")
# install.packages("rasterdiv")

# Recalling the libraries in order to use packages funtions 
library(raster)
library(RStoolbox)
library(rasterdiv)

l1992 <- brick("defor1_.jpg") # brick() to load a whole block of bands
l1992 # 3 bands

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
plotRGB(l2006, r=1, g=2, b=3, stretch="lin")

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


# - day 2
# Range DVI (8 bit): -255 to 255
# NDVI = NIR-red / NIR+red
# Range NDVI (8 bit): -1 to 1

# Range DVI (16 bit): -65535 to 65535
# Range NDVI (16 bit): -1 to 1

# NDVI to compare images with different radiometric resolutions

#NDVI 1992
ndvi1992 <- (l1992[[1]] - l1992[[2]]) / (l1992[[1]] + l1992[[2]])
# or
ndvi1992 <- dvi1992 / sum(l1992[[1]], l1992[[2]])
ndvi1992

# Multiframe with plotRGB on top of the NDVI image
par(mfrow=c(2,1))
plotRGB(l1992, r=1, g=2, b=3, stretch="lin")
plot(ndvi1992, col=cl)

dev.off()

# NDVI 2006
ndvi2006 <- dvi2006 / (l2006[[1]] + l2006[[2]])
ndvi2006
plot(ndvi2006, col=cl)


# NDVI comparison for the same area after 14 years
par(mfrow=c(2,1))
plot(ndvi1992, col=cl)
plot(ndvi2006, col=cl)


### RStoolbox
# Automatic spectral indices by the spectralIndices function
si1992 <- spectralIndices(l1992, green=3, red=2, nir=1)
plot(si1992, col=cl)

si2006 <- spectralIndices(l2006, green=3, red=2, nir=1)
plot(si2006, col=cl)


### rasterdiv
copNDVI # Copernicus NDVI 1999-2007 (21st june)
plot(copNDVI)






