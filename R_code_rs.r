# Questo è il primo script di telerilevamento 22/23


# Settaggio cartella di lavoro
setwd("C:/lab/")

# Installo il pacchetto "raster" attraverso la funzione install.packages()
# install.packages("raster")

# Richiamo la libreria per utilizzare le funzioni del pacchetto raster
library(raster)

# Importo un file raster in formato RasterBrick e lo assegno ad un oggetto
## brick() si utilizza per caricare interi pacchetti di dati (es. remoteS)
l2011 <- brick("p224r63_2011.grd")
l2011 # osservo le caratteristiche dell'oggetto
# Indici di riflettanza per 7 bande spettrali (Landsat) ## Nord del Brasile
# b1 = blue
# b2 = green
# b3 = red
# b4 = NIR near infrared
# b5 = infrarosso medio
# b6 = infrarosso termico
# b7 = infrarosso medio

# Plotto l'oggetto
plot(l2011) # scala cromatica poco efficace ai fini dell'analisi

# Utilizzo la funzione colorRampPalette() per creare una scala cromatica (vettore)
cl <- colorRampPalette(c("black", "grey", "light grey")) (100)

# Colori con tonalità più scure indicano valori bassi di riflettanza e viceversa
plot(l2011, col=cl)

# Osservo solamente la banda del blu - B1_sre
plot(l2011$B1_sre, col=cl) # per nome
# or
plot(l2011[[1]], col=cl) # per elemento

# Creo una scala cromatica per la banda del blu
clb <- colorRampPalette(c("dark blue", "blue", "light blue")) (100)
plot(l2011[[1]], col=clb)



