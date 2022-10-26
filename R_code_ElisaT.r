# This code is from a lecture from Elisa Thouverai on making function

cheer_me <- function(your_name){
  cheer_string <- paste("Hello", your_name, sep = " ")
  print(cheer_string)
}

cheer_me("Giovanni")

# FOR cycle
for (i in 1:10) {
  cheer_me("Giovanni")
}


cheer_me_n_times <- function(your_name, n){
  cheer_string <- paste("Hello", your_name, sep = " ")
  for (i in 1:n) {
    print(cheer_string)
  }
}
cheer_me_n_times("Giovanni", 8)



####

setwd("C:/lab/")
library(raster)

dati <- raster("similaun.png")
plot(dati)

plot_raster <- function(r, col = NA){
  if(!is.na(col)){
    pal <- colorRampPalette(col)(100)
    plot(r, col = pal)
  } else {
    plot(r)
  }
}



plot_raster(dati, c("brown","yellow", "green"))
plot_raster(dati, c("forestgreen","saddlebrown", "white"))





  
