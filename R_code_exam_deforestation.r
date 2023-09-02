library(terra)
library(ggplot2)
library(viridis)
library(gridExtra)
library(GGally)
library(rasterVis)

clt <- colorRampPalette(c("darkgreen", "lightblue", "orange")) (100)
ckey <- list(labels=list(cex=5))
p.strip <- list(cex=4.5, lines=1)

##### data
setwd("C:/exam_lab")

S2_data <- list.files("data/") # 36 .tif: 36 bands
S2_data <- paste0("data/", S2_data)
import <- lapply(S2_data, rast)

# Stacking single layers for each year: 6 bands x 6 year
s18 <- rast(import[1:6]) # Blue, Green, Red, NIR, RedEdge4, SWIR
s19 <- rast(import[7:12])
s20 <- rast(import[13:18])
s21 <- rast(import[19:24])
s22 <- rast(import[25:30])
s23 <- rast(import[31:36])

AMZ <- list(s18, s19, s20, s21, s22, s23)

YR <- as.character(c("2018", "2019", "2020", "2021", "2022", "2023"))
names(AMZ) <- YR

rm(import, s18, s19, s20, s21, s22, s23) # clean up

AMZ$`2018` # data

# Observing data in true color RGB
pRGB <- function(r){ plotRGB(r, 3, 2, 1, str = "lin") }

par(mfrow=c(2,3))
lapply(AMZ, pRGB)
dev.off()

##### NDVI
NDVI <- function(r){ return((r[[4]]-r[[3]])/(r[[4]]+r[[3]])) }

AMZ_ndvi <- list()
for(i in 1:length(AMZ)){
  AMZ_ndvi[[i]] <- NDVI(AMZ[[i]])
  names(AMZ_ndvi[[i]]) <- "ndvi"
}

# ggplot to visualize ndvi changes
AMZ_ndvi_p <- list()
for (i in 1:length(AMZ_ndvi)) {
  AMZ_ndvi_p[[i]] <- ggplot(as.data.frame(AMZ_ndvi[[i]], xy = T)) +
    geom_raster(mapping=aes(x, y, fill = ndvi)) +
    scale_fill_viridis(direction = -1) +
    guides(fill=guide_colorbar("NDVI", ticks = FALSE, raster = T))
}
do.call("grid.arrange", c(AMZ_ndvi_p, ncol=3))

# levelplot to use the same colorscale
AMZ_ndvi_s <- rast(AMZ_ndvi)
names(AMZ_ndvi_s) <- c("ndvi.1", "ndvi.2", "ndvi.3", "ndvi.4", "ndvi.5", "ndvi.6")
lvlp_ndvi <- levelplot(AMZ_ndvi_s, layout=c(3,2), names.attr=YR, par.strip.text=p.strip,
                       col.regions=viridis(200, option = "D", direction = -1),
                       xlab=NULL, ylab=NULL, between=list(x=2, y=0),
                       scales=list(draw=F), colorkey=ckey, main=list(label="NDVI", cex=4))


# Deforestation detection using a specific threshold
Tot_px <- nrow(AMZ[[1]]) * ncol(AMZ[[1]])
perc_def_px <- list()
for (i in 1:length(AMZ)) {
  threshold <- 0.5
  abs_pixels <- ifel(AMZ_ndvi[[i]] > threshold, 1, 0)
  deforested_px[[i]] <- nrow(abs_pixels[abs_pixels == 0])
  perc_def_px[[i]] <- (deforested_px[[i]]/TotPX)*100
}

defo_px_yr_df <- data.frame(x= unlist(YR), y=unlist(perc_def_px))

defo_px_p <- ggplot(defo_px_yr_df, aes(x=x, y=y, group=1)) +
  geom_line(color='red', linewidth=3) + labs(y = "% of deforested pixel (NDVI < 0.5)", x = "Year")


##### NDMI Normalized Difference Moisture Index: RedEdge4 - SWIR / RedEdge4 + SWIR
NDMI <- function(r){ return((r[[5]]-r[[6]])/(r[[5]]+r[[6]])) }

AMZ_ndmi <- list()
for(i in 1:length(AMZ)){
  AMZ_ndmi[[i]] <- NDMI(AMZ[[i]])
  names(AMZ_ndmi[[i]]) <- "ndmi"
}

AMZ_ndmi_p <- list()
for (i in 1:length(AMZ_ndmi)) {
  AMZ_ndmi_p[[i]] <- ggplot(as.data.frame(AMZ_ndmi[[i]], xy = T)) +
    geom_raster(mapping=aes(x, y, fill = ndmi)) +
    scale_fill_viridis(option = "magma", direction = -1) +
    guides(fill=guide_colorbar("NDMI", ticks = FALSE, raster = T))
}
do.call("grid.arrange", c(AMZ_ndmi_p, ncol=3))


##### BSI Bare Soil Index: ((Red+SWIR) – (NIR+Blue)) / ((Red+SWIR) + (NIR+Blue))
BSI <- function(r){ return(((r[[3]]+r[[6]])-(r[[4]]+r[[1]]))/((r[[3]]+r[[6]])+(r[[4]]+r[[1]])))}

AMZ_bsi <- list()
for(i in 1:length(AMZ)){
  AMZ_bsi[[i]] <- BSI(AMZ[[i]])
  names(AMZ_bsi[[i]]) <- "bsi"
}

AMZ_bsi_p <- list()
for (i in 1:length(AMZ_bsi)) {
  AMZ_bsi_p[[i]] <- ggplot(as.data.frame(AMZ_bsi[[i]], xy = T)) +
    geom_raster(mapping=aes(x, y, fill = bsi)) +
    scale_fill_gradientn(colors=clt) +
    guides(fill=guide_colorbar("BSI", ticks = FALSE, raster = T))
}
do.call("grid.arrange", c(AMZ_bsi_p, ncol=3))


##### mean

# Calculate yearly mean NDVI, NDMI and BSI
m_ndvi <- list()
m_ndmi <- list()
m_bsi <- list()
for(i in 1:length(AMZ)){
  m_ndvi[[i]] <- mean(values(AMZ_ndvi[[i]]))
  m_ndmi[[i]] <- mean(values(AMZ_ndmi[[i]]))
  m_bsi[[i]] <- mean(values(AMZ_bsi[[i]]))
}

# Create a time series plot
ts_data <- data.frame(Year = YR, Mean_NDVI = unlist(m_ndvi),
                      Mean_NDMI = unlist(m_ndmi), Mean_BSI = unlist(m_bsi))
ts_plot <- ggplot(ts_data, aes(x = Year, group = 3)) +
  geom_line(aes(y = Mean_NDVI, color = "NDVI")) +
  geom_line(aes(y = Mean_NDMI, color = "NDMI")) +
  geom_line(aes(y = Mean_BSI, color = "BSI")) +
  labs(x = "Year", y = "Mean Value", color = "VIs") +
  scale_color_manual(values = c("NDVI" = "green", "NDMI" = "blue", "BSI" = "red"))

ts_plot
