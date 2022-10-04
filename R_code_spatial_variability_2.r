# Spatial variability Similaun glacier - 2
library(raster)
library(RStoolbox) # for image viewing and variability
library(ggplot2) # for ggplot plotting
library(patchwork)
library(gridExtra) # for plotting ggplots together
library(viridis) # for ggplots colouring

setwd("C:/lab/")

sent <- brick("similaun.png")
# band 1: NIR
# band 2: red
# band 3: green

# Plotting using ggRGB - false colours - nir in green component
ggRGB(sent, 2, 1, 3)


## Multivariate analysis
sen_pca <- rasterPCA(sent)
sen_pca

# Principal Components
pc1 <- sen_pca$map$PC1
pc2 <- sen_pca$map$PC2
pc3 <- sen_pca$map$PC3

# Observing Principal Components
summary(sen_pca$model)

# Simple plot
plot(sen_pca$map)


# Plotting principal components using ggplot2
gpc1 <- ggplot() +
  geom_raster(pc1, mapping=aes(x, y, fill=PC1))

gpc2 <- ggplot() +
  geom_raster(pc2, mapping=aes(x, y, fill=PC2))

gpc3 <- ggplot() +
  geom_raster(pc3, mapping=aes(x, y, fill=PC3))

# multiframe using patchwork
gpc1 + gpc2 + gpc3


## Standard deviation of PC1 using focal() - 3x3 moving window
pc1sd3 <- focal(pc1, matrix(1/9, 3, 3), sd)
pc1sd3

# Plotting the standard deviation of PC1 with ggplot2 and viridis
ggplot() + 
  geom_raster(pc1sd3, mapping=aes(x, y, fill=layer)) +
  scale_fill_viridis()

# magma color scale + title
ggplot() + 
  geom_raster(pc1sd3, mapping=aes(x, y, fill=layer)) +
  scale_fill_viridis(option = "magma") +
  ggtitle("PC1 standard deviation in 'magma' chromatic scale")


# Images altogether
im1 <- ggRGB(sent, 2, 1, 3) # original - nir in the green component
im2 <- ggplot() +
  geom_raster(pc1, mapping=aes(x, y, fill=PC1)) # PC1 layer
im3 <- ggplot() + 
  geom_raster(pc1sd3, mapping=aes(x, y, fill=layer)) +
  scale_fill_viridis(option = "magma") # PC1 standard deviation 3x3 W

im1 + im2 + im3 # patchwork

# Calculating heterogeneity in 5x5 and 7x7 windows
pc1sd5 <- focal(pc1, matrix(1/25, 5, 5), sd)
pc1sd5
im5 <- ggplot() + 
  geom_raster(pc1sd5, mapping=aes(x, y, fill=layer)) +
  scale_fill_viridis(option = "magma") # PC1 standard deviation 5x5 W

pc1sd7 <- focal(pc1, matrix(1/49, 7, 7), sd)
pc1sd7
im7 <- ggplot() + 
  geom_raster(pc1sd7, mapping=aes(x, y, fill=layer)) +
  scale_fill_viridis(option = "magma") # PC1 standard deviation 7x7 W

im3 + im5 + im7
# Wider the windows higher the variability - lower the detail
# im3 (3x3) shows more details
# im5 (5x5) shows higher variability
# im7 (7x7) shows the highest variability but less details






