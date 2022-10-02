# Land cover analysis
library(raster)
library(RStoolbox)
# install.packages("ggplot2")
library(ggplot2)
# install.packages("patchwork")
library(patchwork)

setwd("C:/lab/")

# Importing file individually - deforestation around rio Peixoto
l92 <- brick("defor1_.jpg") # 1992
l06 <- brick("defor2_.jpg") # 2006

# band1 = nir
# band2 = red
# band3 = green

plotRGB(l92, 1, 2, 3, st="lin")

par(mfrow=c(2,1))
plotRGB(l92, 1, 2, 3, st="lin")
plotRGB(l06, 1, 2, 3, st="lin")
dev.off()


# Making a simple multiframe using ggplot2
p1 <- ggRGB(l92, 1, 2, 3, st="lin")
p2 <- ggRGB(l06, 1, 2, 3, st="lin")

# Combining plots with patchwork
p1 + p2 # one next to the other
# or
p1/p2 # one on top of the other

# Differest stretching of values
p1i <- ggRGB(l92, 1, 2, 3, st="hist")
p2i <- ggRGB(l06, 1, 2, 3, st="hist")
# Plotting altoghether
p1 + p2 + p1i + p2i


## Classification
set.seed(88)
l92c <- unsuperClass(l92, nClasses=2)
plot(l92c$map)
# class 1 : forest
# class 2 : agricultural area (and water)

l06c <- unsuperClass(l06, nClasses=2)
plot(l06c$map)
# class 1 : agricultural area (and water)
# class 2 : forest


# Frequencies of pixel classes
freq(l92c$map)
# class 1 : 304441 px (forest)
# class 2 :  36851 px (agricultural area)

freq(l06c$map)
# class 1 : 164472 px (agricultural area)
# class 2 : 178254 px (forest)

# Total number of pixels
totpx92 <- 341292
totpx06 <- 342726

# Proportion of pixels classified as forest
prop_for_92 <- 304441 / totpx92 # 0.89
prop_for_06 <- 178254 / totpx06 # 0.52

# Percentage of forested pixels
perc_for_92 <- 304441 * 100 / totpx92 #  89.2025%
perc_for_06 <- prop_for_06 * 100      # 52.01064%

# Percentage of pixels classified as agricultural areas
perc_agr_92 <- 36851 * 100 / totpx92 #  10.7975%
perc_agr_06 <- 100 - perc_for_06     # 47.98936%


## RESUMING DATA
# percentage of pixels classified as forest in 1992:            89.2025
# percentage of pixels classified as forest in 2006:            52.01064
#
# percentage of pixels classified as agricultural area in 1992: 10.7975
# percentage of pixels classified as agricultural area in 2006: 47.98936


## Building a dataframe
cover <- c("Forest", "Agricultural area")
percent_1992 <- c(perc_for_92, perc_agr_92)
percent_2006 <- c(perc_for_06, perc_agr_06)
# data.frame()
percentage <- data.frame(cover, percent_1992, percent_2006)

# Plotting using ggplot2
ggpl_92 <- ggplot(percentage, aes(x=cover, percent_1992, color=cover)) +
  geom_bar(stat = "identity", fill="white")

ggpl_06 <- ggplot(percentage, aes(x=cover, percent_2006, color=cover)) +
  geom_bar(stat = "identity", fill="white")

# Combining plots using patchwork and exporting in .pdf
pdf("defor_percentage.pdf")
ggpl_92 + ggpl_06
dev.off()
