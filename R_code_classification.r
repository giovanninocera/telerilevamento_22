### Classification of images

## Solar Orbiter - solar pillars
library(raster)
library(RStoolbox)

setwd("C:/lab")

so <- brick("Solar_Orbiter_s_first_views_of_the_Sun_pillars.jpg")

plotRGB(so, 1, 2, 3, stretch="lin")
plotRGB(so, 1, 2, 3, stretch="hist")

# Classifying the solar data 
soc <- unsuperClass(so, nClasses=3)

cl <- colorRampPalette(c('yellow','black','red'))(100)
plot(soc$map, col=cl)

# set.seed can be used for repeating the experiment in the same manner for N times
# http://rfunction.com/archives/62 


## Earth Observatory - Grand Canyon
gc <- brick("dolansprings_oli_2013088_canyon_lrg.jpg")

# red = 1
# green = 2
# blue = 3

plotRGB(gc, 1, 2, 3, stretch="lin")
plotRGB(gc, 1, 2, 3, stretch="hist")

# classification - unsuperClass()
gcc2 <- unsuperClass(gc, nClasses=2)
gcc2

# Compare the classified map with the original set
par(mfrow=c(2,1))
plot(gcc2$map)
plotRGB(gc, 1, 2, 3, stretch="hist")

# set.seed(8)

# 4 different classes
gcc4 <- unsuperClass(gc, nClasses=4)

cl <- colorRampPalette(c("yellow", "red", "blue", "black"))(100)
plot(gcc4$map, col=cl)
plotRGB(gc, 1, 2, 3, stretch="hist")

# <- brick("iss022e014078_087_lrg.jpg")
