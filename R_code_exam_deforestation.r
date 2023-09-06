library(terra)
library(ggplot2)
library(viridis)
library(gridExtra)
library(rasterVis)
library(GGally)
library(tidyverse)

clt <- colorRampPalette(c("darkgreen", "lightblue", "orange")) (100)

ckey <- list(labels=list(cex=5))
p.strip <- list(cex=4.5, lines=1)

##### data
setwd("C:/exam_lab")

S2_data <- list.files("data/") # 36 .tif: 36 bands
S2_data <- paste0("data/", S2_data)
import <- lapply(S2_data, rast)

# Stacking layers for each year: 6 bands x 6 year
s18 <- rast(import[1:6]) # Blue, Green, Red, NIR, RedEdge4, SWIR
s19 <- rast(import[7:12])
s20 <- rast(import[13:18])
s21 <- rast(import[19:24])
s22 <- rast(import[25:30])
s23 <- rast(import[31:36])

AMZ <- list(s18, s19, s20, s21, s22, s23)

Year <- as.character(c(2018:2023))
names(AMZ) <- Year

rm(import, s18, s19, s20, s21, s22, s23) # clean up

AMZ$`2018` # observe data

# Observing data in true color RGB
pRGB <- function(r){ plotRGB(r, 3, 2, 1, str = "lin") }

par(mfrow=c(2,3))
lapply(AMZ, pRGB)
dev.off()

##### Analyze deforestation using different Spectral Indices

# NDVI: Normalized Difference Vegetation Index = (NIR - Red) / (NIR + Red)
NDVI <- function(r){ return((r[[4]]-r[[3]])/(r[[4]]+r[[3]])) }

# NDMI: Normalized Difference Moisture Index = (RedEdge4 - SWIR) / (RedEdge4 + SWIR)
NDMI <- function(r){ return((r[[5]]-r[[6]])/(r[[5]]+r[[6]])) }

# BSI: Bare Soil Index = ((Red+SWIR) – (NIR+Blue)) / ((Red+SWIR) + (NIR+Blue))
BSI <- function(r){ return(((r[[3]]+r[[6]])-(r[[4]]+r[[1]]))/((r[[3]]+r[[6]])+(r[[4]]+r[[1]]))) }

AMZ_ndvi <- list()
AMZ_ndmi <- list()
AMZ_bsi <- list()

for(i in 1:length(AMZ)){
  
  AMZ_ndvi[[i]] <- NDVI(AMZ[[i]])
  names(AMZ_ndvi[[i]]) <- "ndvi"
  
  AMZ_ndmi[[i]] <- NDMI(AMZ[[i]])
  names(AMZ_ndmi[[i]]) <- "ndmi"
  
  AMZ_bsi[[i]] <- BSI(AMZ[[i]])
  names(AMZ_bsi[[i]]) <- "bsi"
}

VIs_l <- list(AMZ_ndvi,AMZ_ndmi,AMZ_bsi)

# ggplot to visualize ndvi changes
AMZ_ndvi_p <- list()
for (i in 1:length(AMZ_ndvi)) {
  AMZ_ndvi_p[[i]] <- ggplot(as.data.frame(AMZ_ndvi[[i]], xy = T)) +
    geom_raster(mapping=aes(x, y, fill = ndvi)) +
    scale_fill_viridis(direction = -1) +
    guides(fill=guide_colorbar("NDVI", ticks = FALSE, raster = T))
}
do.call("grid.arrange", c(AMZ_ndvi_p, ncol=3))

# levelplot
AMZ_ndvi_s <- rast(AMZ_ndvi)
names(AMZ_ndvi_s) <- c("ndvi.1", "ndvi.2", "ndvi.3", "ndvi.4", "ndvi.5", "ndvi.6")
lvlp_ndvi <- levelplot(AMZ_ndvi_s, layout=c(3,2), names.attr=Year, par.strip.text=p.strip,
                       col.regions=viridis(200, option = "D", direction = -1),
                       xlab=NULL, ylab=NULL, between=list(x=2, y=0),
                       scales=list(draw=F), colorkey=ckey, main=list(label="NDVI", cex=4))
lvlp_ndvi

# Deforestation detection using a specific threshold
Tot_PX <- nrow(AMZ[[1]]) * ncol(AMZ[[1]])
perc_def_PX <- list()
for (i in 1:length(AMZ)) {
  threshold <- .5
  abs_pixels <- ifel(AMZ_ndvi[[i]] > threshold, 1, 0)
  perc_def_PX[[i]] <- (nrow(abs_pixels[abs_pixels == 0])/Tot_PX)*100
}

defo_PX_yr_df <- data.frame(x= Year, y=unlist(perc_def_PX))

defo_PX_p <- ggplot(defo_PX_yr_df, aes(x=x, y=y, group=1)) +
  geom_line(color='red', linewidth=3) + labs(y = "% of deforested pixel (NDVI < 0.5)", x = "Year")

defo_PX_p

# NDMI plot
AMZ_ndmi_p <- list()
for (i in 1:length(AMZ_ndmi)) {
  AMZ_ndmi_p[[i]] <- ggplot(as.data.frame(AMZ_ndmi[[i]], xy = T)) +
    geom_raster(mapping=aes(x, y, fill = ndmi)) +
    scale_fill_viridis(option = "magma", direction = -1) +
    guides(fill=guide_colorbar("NDMI", ticks = FALSE, raster = T))
}
do.call("grid.arrange", c(AMZ_ndmi_p, ncol=3))

# BSI plot
AMZ_bsi_p <- list()
for (i in 1:length(AMZ_bsi)) {
  AMZ_bsi_p[[i]] <- ggplot(as.data.frame(AMZ_bsi[[i]], xy = T)) +
    geom_raster(mapping=aes(x, y, fill = bsi)) +
    scale_fill_gradientn(colors=clt) +
    guides(fill=guide_colorbar("BSI", ticks = FALSE, raster = T))
}
do.call("grid.arrange", c(AMZ_bsi_p, ncol=3))


##### Observe correlation between indices

# mean
M <- function(x){
  val = values(x)
  return(mean(val))
}

m_ndvi <- sapply(AMZ_ndvi, M)
m_ndmi <- sapply(AMZ_ndmi, M)
m_bsi <- sapply(AMZ_bsi, M)

df_VI_m <- data.frame(Year = Year, m_NDVI = m_ndvi, m_NDMI = m_ndmi, m_BSI = m_bsi)

# standard deviation
SD <- function(x){
  val = values(x)
  return(sd(val))
}

sd_ndvi <- sapply(AMZ_ndvi, SD)
sd_ndmi <- sapply(AMZ_ndmi, SD)
sd_bsi <- sapply(AMZ_bsi, SD)

df_VI_sd <- data.frame(Year = Year, sd_NDVI = sd_ndvi, sd_NDMI = sd_ndmi, sd_BSI = sd_bsi)

### Join df
df_m_sd <- df_VI_m %>%
  left_join(df_VI_sd)

# Observing trend of VIs
VIs_trend <- ggplot(df_m_sd, aes(x = Year, group=3)) +
  geom_line(aes(y = m_NDVI, color = "NDVI"), linewidth=2) +
  geom_ribbon(aes(y = m_NDVI, ymin = m_NDVI - sd_NDVI, ymax = m_NDVI + sd_NDVI), alpha = .2) +
  geom_line(aes(y = m_NDMI, color = "NDMI"), linewidth=2) +
  geom_ribbon(aes(y = m_NDMI, ymin = m_NDMI - sd_NDMI, ymax = m_NDMI + sd_NDMI), alpha = .2) +
  geom_line(aes(y = m_BSI, color = "BSI"), linewidth=2) +
  geom_ribbon(aes(y = m_BSI, ymin = m_BSI - sd_BSI, ymax = m_BSI + sd_BSI), alpha = .2) +
  labs(x = "Year", y = "Mean Value", color = "VIs") + ylim(-.5, 1) + 
  scale_color_manual(values = c("NDVI" = "green", "NDMI" = "blue", "BSI" = "red")) +
  guides(color = guide_legend(reverse=TRUE))

VIs_trend

##### ggpairs
p0 <- function (a, ...) { paste0(a, ...) } 

st_names <- function(n){
  c(p0(n, "_18"), p0(n, "_19"), p0(n, "_20"), p0(n, "_21"), p0(n, "_22"), p0(n, "_23"))
}

AMZ_ndvi_s <- rast(VIs_l[[1]])
names(AMZ_ndvi_s) <- st_names("ndvi")
AMZ_ndmi_s <- rast(VIs_l[[2]])
names(AMZ_ndmi_s) <- st_names("ndmi")
AMZ_bsi_s <- rast(VIs_l[[3]])
names(AMZ_bsi_s) <- st_names("bsi")

VI_df <- list()
for (i in 1:length(AMZ)) {
  VI_df[[i]] <- as.data.frame(c(AMZ_ndvi_s[[i]], AMZ_ndmi_s[[i]], AMZ_bsi_s[[i]]))
}
str(VI_df)

ggp18 <- ggpairs(VI_df[[1]],
                lower = list(continuous = wrap("points", colour="orange", alpha=.2)),
                upper = list(continuous = wrap("cor", size = 10)))
ggp23 <- ggpairs(VI_df[[6]],
                lower = list(continuous = wrap("points", colour="orange", alpha=.2)),
                upper = list(continuous = wrap("cor", size = 10)))


##### Generalized Linear Model using distance matrices

# Creating the dataframes
AMZ_mod_df <- list()
for (i in 1:length(AMZ)) {
  AMZ_mod_stack <- list()
  AMZ_mod_stack[[i]] <- c(AMZ[[i]], AMZ_ndvi_s[[i]], AMZ_ndmi_s[[i]], AMZ_bsi_s[[i]])
  AMZ_mod_df[[i]] <- as.data.frame(AMZ_mod_stack[[i]], xy = T)
  colnames(AMZ_mod_df[[i]]) <- c("x", "y", "Blue", "Green", "Red", "NIR", "REd4", "SWIR", "NDVI", "NDMI", "BSI")
}

# .. distance matrices
AMZ_mod_Dsample <- list()
for (i in 1:length(AMZ)) {
  dist_sample <- list()
  
  for (j in 1:100) {
    v <- sample(1:nrow(AMZ_mod_df[[i]]), 1000) # subsampling
    d_bands <- dist(AMZ_mod_df[[i]][v, 3:8])
    d_ndvi <- dist(AMZ_mod_df[[i]][v, 9])
    d_ndmi <- dist(AMZ_mod_df[[i]][v, 10])
    d_bsi <- dist(AMZ_mod_df[[i]][v, 11])
    df <- data.frame(c(d_bands), c(d_ndvi), c(d_ndmi), c(d_bsi))
    colnames(df) <- c("bands", "ndvi", "ndmi", "bsi")
    dist_sample[[j]] <- df
  }
  AMZ_mod_Dsample[[i]] <- dist_sample
}

# GLM on distance matrices:  bands ~ VIs
AMZ_models <- list()
for (i in 1:length(AMZ)) {
  models <- lapply(AMZ_mod_Dsample[[i]], function(df) {
    m_ndvi <- glm(bands ~ ndvi, data = df)
    m_ndmi <- glm(bands ~ ndmi, data = df)
    m_bsi <- glm(bands ~ bsi, data = df)
    perc_ndvi <- ((m_ndvi$null.deviance - m_ndvi$deviance)/m_ndvi$null.deviance*100) # TOT - residual
    perc_ndmi <- ((m_ndmi$null.deviance - m_ndmi$deviance)/m_ndmi$null.deviance*100)
    perc_bsi <- ((m_bsi$null.deviance - m_bsi$deviance)/m_bsi$null.deviance*100)
    return(c(perc_ndvi, perc_ndmi, perc_bsi))
  })
  
  AMZ_models[[i]] <- models
}

save(AMZ_models, file="mod/AMZ_models.rda")
#
load("mod/AMZ_models.rda")

AMZ_var_df <- list()
for (i in 1:length(AMZ)) {
  AMZ_var_df[[i]] <- do.call(rbind, AMZ_models[[i]])
  colnames(AMZ_var_df[[i]]) <- c("NDVI", "NDMI", "BSI")
}

parbox <- function(df, name){
  par(mfrow=c(2,3), cex.main=3, family="serif")
  for (i in 1:length(df)){
    boxplot(df[[i]], col = "bisque", cex.axis=2, main=name[[i]])}
}

parbox(AMZ_var_df, Year)


################################################ saving plots

# RGB
jpeg("img/rgb_2018.jpg", width = 1638, height = 1348)
pRGB(AMZ[[1]])
dev.off()

jpeg("img/rgb_2019.jpg", width = 1638, height = 1348)
pRGB(AMZ[[2]])
dev.off()

jpeg("img/rgb_2020.jpg", width = 1638, height = 1348)
pRGB(AMZ[[3]])
dev.off()

jpeg("img/rgb_2021.jpg", width = 1638, height = 1348)
pRGB(AMZ[[4]])
dev.off()

jpeg("img/rgb_2022.jpg", width = 1638, height = 1348)
pRGB(AMZ[[5]])
dev.off()

jpeg("img/rgb_2023.jpg", width = 1638, height = 1348)
pRGB(AMZ[[6]])
dev.off()


##### theme to stamp
theme_update(panel.background = element_blank(),
             text = element_text(size = rel(6)),
             plot.title = element_text(hjust = .5, size = rel(10)),
             legend.text = element_text(size = rel(5)),
             legend.title = element_text(size = rel(6)),
             legend.key.height= unit(2, 'cm'))

theme_update(axis.title=element_blank(),
             axis.text=element_blank(),
             axis.ticks=element_blank())

# NDVI 
jpeg("img/AmazonDefo_ndvi.jpg", width = 3000, height = 1500)
do.call("grid.arrange", c(AMZ_ndvi_p, ncol=3))
dev.off()
# NDVI lvlp
jpeg("img/AmazonDefo_ndvi_lvlp.jpg", width = 3000, height = 1500)
lvlp_ndvi
dev.off()
# trend NDVI < 0.5
jpeg("img/AmazonDefo_perc_PX.jpg", width = 1500, height = 800)
defo_px_p + theme(axis.text = element_text(size = rel(1)))
dev.off()

# NDMI 
jpeg("img/AmazonDefo_ndmi.jpg", width = 3000, height = 1500)
do.call("grid.arrange", c(AMZ_ndmi_p, ncol=3))
dev.off()

# BSI
jpeg("img/AmazonDefo_bsi.jpg", width = 3000, height = 1500)
do.call("grid.arrange", c(AMZ_bsi_p, ncol=3))
dev.off()

# Mean values VIs
jpeg("img/AmazonDefo_mean_VI.jpg", width = 1500, height = 800)
VIs_trend + theme(axis.text = element_text(size = rel(1)))
dev.off()


##### theme for ggpairs
theme_set(theme_gray())
theme_update(plot.title = element_text(hjust = 0.5, size = rel(4), family = "serif"),
             strip.text = element_text(size=rel(3)),
             axis.text=element_text(size=rel(2)))

# ggpairs 2018
jpeg("img/ggp_18.jpg", width = 1500, height = 500)
ggp18
dev.off()

# ggpairs 2023
jpeg("img/ggp_23.jpg", width = 1500, height = 500)
ggp23
dev.off()


##### Models  bands ~ VIs
jpeg("img/AmazonDefo_boxplot.jpg", width = 1200, height = 800)
parbox(AMZ_var_df, Year)
dev.off()
