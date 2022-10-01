# Land cover analysis
library(raster)
library(RStoolbox)
# install.packages("ggplot2")
library(ggplot2)
# install.packages("patchwork")
library(patchwork)

setwd("C:/lab/")


l92 <- brick("defor1_.jpg")
l06 <- brick("defor2_.jpg")

# band1 = nir
# band2 = red
# band3 = green

plotRGB(l92, 1, 2, 3, st="lin")

par(mfrow=c(2,1))# Land cover analysis
library(raster)
library(RStoolbox)

setwd("C:/lab/")

# Importing file individually - deforestation around rio Peixoto
l92 <- brick("defor1_.jpg")
l06 <- brick("defor2_.jpg")

# band1 = nir
# band2 = red
# band3 = green

plotRGB(l92, 1, 2, 3, st="lin")

par(mfrow=c(2,1))
plotRGB(l92, 1, 2, 3, st="lin")
plotRGB(l06, 1, 2, 3, st="lin")

dev.off()

# Making simple multiframe using ggplot2
p1 <- ggRGB(l92, 1, 2, 3, st="lin")
p2 <- ggRGB(l06, 1, 2, 3, st="lin")

# Combining plots with patchwork
p1 + p2 # one next to the other

p1/p2 # one on top of another


p1i <- ggRGB(l92, 1, 2, 3, st="hist")
p2i <- ggRGB(l06, 1, 2, 3, st="hist")

p1 + p2 + p1i + p2i

# Classification
l92c <- unsuperClass(l92, nClasses=2)
plot(l92c$map) 
# class 1 : forest
# class 2 : agricultural area (and water)

l06c <- unsuperClass(l06, nClasses=2)
plot(l06c$map)
# class 1 : forest
# class 2 : agricultural area (and water)


# Frequencies
freq(l92c$map)
# class 1 : 305635 px (forest)
# class 2 :  35657 px (agricultural area)

freq(l06c$map)
# class 1 : 178611 px (forest)
# class 2 : 164115 px (agricultural area)









