
# Comprimir imagenes en formato TIFF

install.packages("tiff")
require(tiff)

imgC <- readTIFF(source="C:/Users/haachicanoy/Desktop/AccumulatedRank_90percent_cutoff_12Dec2013.tiff",native=T,all=T)
x <- writeTIFF(what=imgC, where="C:/Users/haachicanoy/Desktop/imgColin.tiff",compression="LZW")
