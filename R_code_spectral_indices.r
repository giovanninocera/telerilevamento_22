# Setting the working directory
setwd("C:/lab/")

# install.packages("raster")
# install.packages("RStoolbox")

# Recalling the library in order to use raster package funtions 
library(raster)

l1992 <- brick("defor1_.jpg")
plot(l1992)

l2006 <- brick("defor2_.jpg")
