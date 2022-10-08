# R code for visualising and analysing LiDAR data
library(raster) # Geographic data analysis and modeling
library(rgdal) # Geospatial data abstraction library
library(RStoolbox)
library(ggplot2)
library(viridis)
# install.packages("lidR")
library(lidR) # to visualize the original point cloud

setwd("C:/lab/")


# DSM (digital surface model): accounting threes hight
# DTM (digital terrain model): laser reflected by the ground
# CHM (canopy height model): differences between DSM-DTM

# 2013
dsm_13 <- raster("2013Elevation_DigitalElevationModel-0.5m.tif")
dtm_13 <- raster("2013Elevation_DigitalTerrainModel-0.5m.tif")
plot(dsm_13)
plot(dtm_13)

chm_13 <- dsm_13 - dtm_13
ggplot() + 
  geom_raster(chm_13, mapping =aes(x, y, fill=layer)) +
  scale_fill_viridis() +
  ggtitle("CHM 2013 San Genesio/Jenesien")

# 2004
dsm_04 <- raster("2004Elevation_DigitalElevationModel-2.5m.tif")
dtm_04 <- raster("2004Elevation_DigitalTerrainModel-2.5m.tif")
chm_04 <- dsm_04 - dtm_04
ggplot() + 
  geom_raster(chm_04, mapping =aes(x, y, fill=layer)) +
  scale_fill_viridis() +
  ggtitle("CHM 2004 San Genesio/Jenesien")


# Comparing the two CHM
diff_chm <- chm_13 - chm_04 # error: different resolution
# Resampling the layer with higher resolution
chm_13r <- resample(chm_13, chm_04)

diff_chm <- chm_13r - chm_04

ggplot() + 
  geom_raster(diff_chm, mapping =aes(x, y, fill=layer)) +
  scale_fill_viridis() +
  ggtitle("CHM difference between 2004-2013 in San Genesio/Jenesien")


# Visualize the point cloud
p_cloud <- readLAS("point_cloud.laz") # using lidR library
plot(p_cloud)
