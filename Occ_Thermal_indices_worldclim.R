library(data.table)
library(mapdata)
library(reshape)
library(raster)

species <- c("Eptesicus serotinus", "Eptesicus nilssonii", "Nyctalus lasiopterus", "Nyctalus noctula", "Nyctalus leisleri", 
             "Pipistrellus kuhlii", "Pipistrellus nathusii", "Pipistrellus pygmaeus", "Plecotus auritus",
             "Plecotus austriacus", "Rhinolophus ferrumequinum", "Rhinolophus hipposideros", "Tadarida teniotis",
             "Hypsugo savii", "Myotis alcathoe", "Plecotus macrobullaris", "Rhinolophus euryale",
             "Rhinolophus melelyi")


thermal_indices <- array(dim = c(nlevels(as.factor(species)), 6, 3),
                         dimnames = list(levels(as.factor(species)),
                                         c("ThAve", "ThSD", "ThQ95", "ThQ5", "Thrange", "Thmed"),
                                         c("Yearly", "Summer", "Winter")))


for(i in species){
  print(i)
  t0 <-   Sys.time()
  occ <- read.csv(paste0("F:/bat veolia mnhn/gbif occurrences/", i, ".csv"), sep = "\t") #reading occurrence data after download from gbif
  occ <- occ[complete.cases(occ$decimalLatitude), ]
  map("worldHires", xlim = c(-20, 59), ylim = c(35, 71)) # visualising the data in Europe
  points(occ$decimalLatitude ~ occ$decimalLongitude)
  
#TO DO: USE MORE PRECISE BIOCLIM LAYER
Bioclimdir="F:/Climate data/worldclim/wc2.0_10m_bio" #where bioclim TIFF files are

coordinates(occ) <- c("decimalLongitude", "decimalLatitude")
proj4string(occ) <- CRS("+init=epsg:4326") # WGS 84

ListTifiles=list.files(Bioclimdir,full.names=T)

for(j in c(1,5,6)){
  SpBioci  <- extract(raster(ListTifiles[j]),occ)
  Sys.time()
  j <- ifelse(j == 1, j, ifelse(j == 5, 2, 3))  #☻change bioclim numbers for the array
  thermal_indices[i, 1, j] <- mean(na.omit(SpBioci))
  thermal_indices[i, 2, j] <- sd(na.omit(SpBioci))
  thermal_indices[i, 3, j] <- quantile(SpBioci,0.95, na.rm = T)
  thermal_indices[i, 4, j] <- quantile(SpBioci,0.05, na.rm = T)
  thermal_indices[i, 5, j] <- thermal_indices[i, 3, j] - thermal_indices[i, 4, j] 
  thermal_indices[i, 6, j] <- (thermal_indices[i, 3, j] + thermal_indices[i, 4, j])/2
}
save(thermal_indices, file = "F:/bat veolia mnhn/data/thermal indices/thermal_indices.bioclim")
print(thermal_indices)
cat(paste("---- ", Sys.time(), "Computing time for species", i, ": ", t0 - Sys.time(), "\n"))
}


