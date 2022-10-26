# This is from a presentation on coloris package

# packages---
# install.packages("colorist")
# install.packages("ggplot2")

# libralies
library(colorist)
library(ggplot2)

###### fiespa

data("fiespa_occ")
fiespa_occ # RasterStack
# colorist works only on RasterStack data

# Metrics: sdm
met1 <- metrics_pull(fiespa_occ)

# Palette
pal <- palette_timecycle(fiespa_occ)

# Map
map_multiples(met1, pal, ncol = 3, labels = names(fiespa_occ))

map_single(met1, pal, layer = 6) # June

# start_hue to create palette that start from different color
p1_custom <- palette_timecycle(12, start_hue = 60) # starts from yellow
map_multiples(met1, p1_custom, ncol = 3, labels = names(fiespa_occ))

# Map distill
met1_distill <- metrics_distill(fiespa_occ) # metric
map_single(met1_distill, pal)

# Legend
leg <- legend_timecycle(pal, origin_label = "1 jan")



###### fisher

data("fisher_ud")
m2 <- metrics_pull(fisher_ud)
pal2 <- palette_timeline(fisher_ud) # time line
head(pal2)

map_multiples(m2, pal2, ncol = 3, lambda_i = -12)

m2_distill <- metrics_distill(fisher_ud)
map_single(m2_distill, pal2, lambda_i = -10)

legend_timeline(pal2)
