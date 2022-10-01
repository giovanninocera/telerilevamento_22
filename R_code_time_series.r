### Time series analysis of Greenland LST data

# Recalling the libraries in order to use packages funtions 
# install.packages("raster")
library(raster)

# Setting the working directory
setwd("C:/lab/greenland/")

## Importing files indivifually..
lst2000 <- raster("lst_2000.tif")
lst2005 <- raster("lst_2005.tif")
lst2010 <- raster("lst_2010.tif")
lst2015 <- raster("lst_2015.tif")

cl <- colorRampPalette(c("blue", "light blue", "pink", "red")) (100)

# ..and plotting in a multiframe
par(mfrow=c(2,2))
plot(lst2000, col=cl)
plot(lst2005, col=cl)
plot(lst2010, col=cl)
plot(lst2015, col=cl)

dev.off()

# or

## Importing the whole set altoghether as a list..
rlist <- list.files(pattern="lst")
rlist
  # lapply(X, FUN)
import <- lapply(rlist, raster) # raster function to a list of files
import
  # stack(x ='list/dataframe')
tgr <- stack(import) # combining different layers in a RasterStack
tgr
# ..and plotting everything
plot(tgr, col=cl) # observing the stack
plot(tgr[[1]], col=cl) # observing the first layer

# Using plotRGB to produce time series with 3 layers
plotRGB(tgr, 1, 2, 3, stretch="lin")
plotRGB(tgr, 2, 3, 4, stretch="lin")
plotRGB(tgr, 4, 3, 2, stretch="lin")


cl <- colorRampPalette(c("blue", "light blue", "pink", "red")) (100)
plot(tgr, col=cl)

# levelplot(tgr,col.regions=cl, names.attr=c("July 2000","July 2005", "July 2010", "July 2015"))
# 
# levelplot(tgr,col.regions=cl, main="LST variation in time",
#           names.attr=c("July 2000","July 2005", "July 2010", "July 2015"))

dev.off()


###########################################
### NO2 decrease during the lockdown period

library(raster)

setwd("C:/lab/en/")

## Importing images individually
en01 <- raster("EN_0001.png") 
cl <- colorRampPalette(c('red','orange','yellow'))(100)
plot(en01, col=cl)

en13 <- raster("EN_0013.png")
plot(en13, col=cl)

## Importing the whole set
enlist <- list.files(pattern="EN")
# lapply(X,FUN)
enimport <- lapply(enlist, raster)
# stack layers altogether
ten <- stack(enimport)
# plot everything
plot(ten, col=cl)

# Plotting EN01 besides EN13
par(mfrow=c(1,2))
plot(ten[[1]], col=cl)
plot(ten[[13]], col=cl)
# or:
ten113 <- stack(ten[[1]], ten[[13]])
plot(ten113, col=cl)

dev.off()

# Difference
difen <-  ten[[1]] - ten[[13]]
cldif <- colorRampPalette(c('blue','white','red'))(100)
plot(difen, col=cldif)

# plotRGB: red=jan, green=feb, blue= march
plotRGB(ten, r=1, g=7, b=13, stretch="lin")
plotRGB(ten, r=1, g=7, b=13, stretch="hist")
