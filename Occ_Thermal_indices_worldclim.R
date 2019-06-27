library(data.table)
library(mapdata)
library(reshape)
library(raster)

# saving a species list
 # if a 'species' column is available
species <- levels(as.factors(data$species))
 # otherwise:
species <- c("Barbastella", "Eptesicus bottae", "Eptesicus serotinus", "Eptesicus nilssonii", "Nyctalus lasiopterus", "Nyctalus noctula", "Nyctalus leisleri", 
             "Pipistrellus kuhlii", "Pipistrellus nathusii", "Pipistrellus pygmaeus", "Plecotus auritus",
             "Plecotus austriacus", "Rhinolophus ferrumequinum", "Rhinolophus hipposideros", "Tadarida teniotis",
             "Rhinolophus melelyi", "Hypsugo savii", "Myotis alcathoe", "Plecotus macrobullaris", "Rhinolophus euryale",
             "Miniopterus schreibersii", "Myotis bechsteinii", "Myotis brandtii", "Myotis capaccinii",
             "Myotis daubentonii", "Myotis emarginatus", "Myotis myotis", "Myotis mystacinus", "Myotis nattereri", "Myotis dasycneme")

# creating an array to store the results
thermal_indices <- array(dim = c(nlevels(as.factor(species)), 6, 3),
                         dimnames = list(levels(as.factor(species)),
                                         c("ThAve", "ThSD", "ThQ95", "ThQ5", "Thrange", "Thmed"),
                                         c("Yearly", "Summer", "Winter")))


for(i in species){
  print(i)
  t0 <-   Sys.time()
  occ <- read.csv(paste0("F:/bat veolia mnhn/gbif occurrences/", i, ".csv"), sep = "\t") #reading occurrence data after download from gbif
  occ <- occ[complete.cases(occ$decimalLatitude), ]
  occ <- occ[occ$decimalLatitude > 30 & 
               occ$decimalLongitude > -20 &
               occ$decimalLongitude < 70, ]  # Europe and surrounings only
  
    map("worldHires", xlim = c(-20, 59), ylim = c(35, 71)) # visualising the data in Europe
  points(occ$decimalLatitude ~ occ$decimalLongitude)
  text(0, 70 ,i)
  
Bioclimdir="F:/Climate data/worldclim/wc2.0_10m_bio" #where bioclim TIFF files are

ListTifiles=list.files(Bioclimdir,full.names=T)

 #Selecting one value of environmental variable per occurrence pixel
 #(controls for spatial bias in non-standardised occurrence records (e.g., gbif))
occ.raster <- rasterize(x = occ[, c("decimalLongitude", "decimalLatitude")],
                 y = raster(ListTifiles[1]),
                 fun = function(x, ...) 1) # Bien vÃ©rifier le rÃ©sultat et ajuster la fonction
occ.unique <- xyFromCell(occ.raster, Which(occ.raster == 1,
                                           cells = TRUE))

for(j in c(1,5,6)){
  SpBioci  <- extract(raster(ListTifiles[j]), occ.unique)
  j <- ifelse(j == 1, j, ifelse(j == 5, 2, 3))  # change bioclim numbers for the array
  thermal_indices[i, 1, j] <- mean(na.omit(SpBioci))
  thermal_indices[i, 2, j] <- sd(na.omit(SpBioci))
  thermal_indices[i, 3, j] <- quantile(SpBioci,0.95, na.rm = T)
  thermal_indices[i, 4, j] <- quantile(SpBioci,0.05, na.rm = T)
  thermal_indices[i, 5, j] <- thermal_indices[i, 3, j] - thermal_indices[i, 4, j] 
  thermal_indices[i, 6, j] <- (thermal_indices[i, 3, j] + thermal_indices[i, 4, j])/2
}
save(thermal_indices, file = "F:/bat veolia mnhn/data/thermal indices/thermal_indices.bioclim")
print(thermal_indices)
cat(paste("---- ", Sys.time(), "Computing time for species", i, ": ", Sys.time()-t0, "\n"))
}
