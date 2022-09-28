# Questo è il promo script

# install.packages("raster")
library(raster)

# Settaggio cartella di lavoro
setwd("C:/lab/")

l2011 <- brick("p224r63_2011.grd")
l2011

plot(l2011)

cl <- colorRampPalette(c("black", "grey", "light grey")) (100)
plot(l2011, col=cl)


