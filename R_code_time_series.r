### Time series analysis of Greenland LST data

# install.packages("raster")
# install.packages("RStoolbox")
# install.packages("rasterdiv")

# Recalling the libraries in order to use packages funtions 
library(raster)
# library(RStoolbox)
# library(rasterdiv)

# Setting the working directory
setwd("C:/lab/greenland/")

lst2000 <- raster("lst_2000.tif")
lst2005 <- raster("lst_2005.tif")
lst2010 <- raster("lst_2010.tif")
lst2015 <- raster("lst_2015.tif")

cl <- colorRampPalette(c("blue", "light blue", "pink", "red")) (100)

par(mfrow=c(2,2))
plot(lst2000, col=cl)
plot(lst2005, col=cl)
plot(lst2010, col=cl)
plot(lst2015, col=cl)

dev.off()

# Import the whole set altoghether
rlist <- list.files(pattern="lst")
rlist
import <- lapply(rlist, raster) # raster function to a list of files
import

tgr <- stack(import) # combining different layers in a RasterStack
tgr

plot(tgr, col=cl) # observing the stack
plot(tgr[[1]], col=cl) # observing the first layer


plotRGB(tgr, r=1, g=2, b=3, stretch="lin") # time serie with plotRGB


dev.off()



### Time series analysis of atmospheric N0s concentration during covid lockdown
