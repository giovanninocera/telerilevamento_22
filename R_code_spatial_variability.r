# Spatial variability Similaun glacier
library(raster)
library(RStoolbox) # for image viewing and variability
library(ggplot2) # for ggplot plotting
library(patchwork)
# install.packages("gridExtra")
library(gridExtra) # for plotting ggplots together
# install.packages("viridis")
library(viridis) # for ggplots colouring

setwd("C:/lab/")

sent <- brick("similaun.png")
# band 1: NIR
# band 2: red
# band 3: green
# band 4: blue

# Plotting using ggRGB - false colours
ggRGB(sent, 1, 2, 3, str="lin") # stretching is not mandatory
p1 <- ggRGB(sent, 1, 2, 3)
p2 <- ggRGB(sent, 2, 1, 3)

p1 + p2 # one beside the other using patchwork


# NDVI calculation
nir <- sent$similaun_1
red <- sent$similaun_2
# also: red <- sent[[2]]

ndvi <- (nir-red) / (nir+red)
plot(ndvi)

cl <- colorRampPalette(c("black", "white", "red",
                         "magenta", "green"))(100)
plot(ndvi, col=cl)


## Calculation of variability through a moving window using focal()

# over NIR

# mean
nirmn3 <- focal(nir, w=matrix(1/9, nrow=3, ncol=3), fun=mean)
plot(nirmn3, col=cl)

# standard deviation
nirsd3 <- focal(nir, matrix(1/9, 3, 3), sd)
clsd <- colorRampPalette(c("blue", "green", "pink", "magenta",
                           "orange", "brown", "red", "yellow"))(100)
plot(nirsd3, col=clsd)


# Plotting using ggplot2 and viridis
ggplot() + 
  geom_raster(nirsd3, mapping=aes(x=x, y=y, fill=layer))
# with viridis
ggplot() + 
  geom_raster(nirsd3, mapping=aes(x, y, fill=layer)) +
  scale_fill_viridis() +
  ggtitle("Standard deviation of nir - viridis")
# with cividis
ggplot() + 
  geom_raster(nirsd3, mapping=aes(x, y, fill=layer)) +
  scale_fill_viridis(option="cividis") +
  ggtitle("Standard deviation of nir - cividis")
# with magma
mag3 <- ggplot() + 
  geom_raster(nirsd3, mapping=aes(x, y, fill=layer)) +
  scale_fill_viridis(option="magma") +
  ggtitle("Standard deviation of nir - 3x3 - magma")


# Making calculation in a 7x7 window
nirsd7 <- focal(nir, matrix(1/49, 7, 7), sd)
plot(nirsd7, col=clsd)

# Plotting with ggplot2 using viridis (magma)
mag7 <- ggplot() + 
  geom_raster(nirsd7, mapping=aes(x, y, fill=layer)) +
  scale_fill_viridis(option="magma") +
  ggtitle("Standard deviation of nir - 7x7 - magma")


# Comparing outputs with different window's size
mag3 / mag7 # one on top of the other








# NDVI sd in a wider moving focal window
ndvisd9 <- focal(ndvi, matrix(1/9, 3, 3), sd)
plot(ndvisd9, col=clsd)

