# Species Distribution Modelling

# install.packages("sdm")
# install.packages("rgdal")

library(sdm) 
library(raster) # predictors
library(rgdal) # species

# system.file() to download file from a package
file <- system.file("external/species.shp", package="sdm")
species <- shapefile(file) # to read a shape file

# Observing the file: Spatial Points Data Frame
species

# Simple plot
plot(species)
# change the point symbol: circles
plot(species, pch=19)

# Looking at the occurrences (P/A)
occ <- species$Occurrence

# Plotting species relating to occurrence [SQL language]:
plot(species[occ == 1,],col='blue',pch=19)
# adding points to the previews plot:
points(species[occ == 0,],col='red',pch=19)

# Predictors: first look at the path
path <- system.file("external", package="sdm") 

# List the predictors in .asc format: environmental variables
lst <- list.files(path=path,pattern='asc$',full.names = T) #
lst

# Stack all the predictors
preds <- stack(lst)

# Plotting predictors
cl <- colorRampPalette(c('blue','orange','red','yellow')) (100)
plot(preds, col=cl)

# Observing predictors
elev <- preds$elevation
temp <- preds$temperature
prec <- preds$precipitation
vege <- preds$vegetation

dev.off()

# Plot predictors combined with presences
plot(elev, col=cl, main="elevation")
points(species[occ == 1,], pch=19)

plot(temp, col=cl, main='temperature')
points(species[occ == 1,], pch=19)

plot(prec, col=cl, main='precipitation')
points(species[occ == 1,], pch=19)

plot(vege, col=cl, main='vegetation')
points(species[occ == 1,], pch=19)


# Specie Distribution Model

# set the data for the sdm
datasdm <- sdmData(train=species, predictors=preds)
datasdm

# Model
m1 <- sdm(Occurrence ~ elevation + precipitation + temperature + vegetation,
          data=datasdm, methods = "glm")

# Creating the raster layer of predictions
p1 <- predict(m1, newdata=preds)

# Plotting the output
plot(p1, col=cl)
points(species[occ == 1,], pch=19)


par(mfrow=c(2,3))
plot(elev, col=cl, main="elevation")
plot(temp, col=cl, main="temperature")
plot(prec, col=cl, main="precipitation")
plot(vege, col=cl, main="vegetation")
plot(p1, col=cl, main="prediction of presence")

dev.off()


# Adding the new layer to the stack
s1 <- stack(preds,p1)
# and plotting it easyly
plot(s1, col=cl)
