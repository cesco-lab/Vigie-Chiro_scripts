

########## extracting climatic data grom E-OBS (Reto Schmucki package)
library(devtools)
library(climateExtract)
memory.limit(size = 50000)
#extracting the big data
#get the data  here (mean temperatures = tg): https://www.ecad.eu/download/ensembles/download.php
mean.t <- extract_nc_value(2000,2015, grid_size = 0.5) # do not plan to do something with your computer for a few minutes (this function will take all the resources)
# save(mean.t, file = "******/data/mean.t")
load("******/data/mean.t")


#creating a function to get the season (easy looted from stackoverflow)
which.season <- function(dat) {
  stopifnot(class(dat) == "Date")
  scalarCheck <- function(dat) {
    m <- as.POSIXlt(dat)$mon + 1        # correct for 0:11 range
    d <- as.POSIXlt(dat)$mday           # correct for 0:11 range
    if ((m == 3 & d >= 21) | (m == 4) | (m == 5) | (m == 6 & d < 21)) {
      r <- 1
    } else if ((m == 6 & d >= 21) | (m == 7) | (m == 8) | (m == 9 & d < 21)) {
      r <- 2
    } else if ((m == 9 & d >= 21) | (m == 10) | (m == 11) | (m == 12 & d < 21)) {
      r <- 3
    } else {
      r <- 4
    }
    r
  }
  res <- sapply(dat, scalarCheck)
  res <- ordered(res, labels=c("Spring", "Summer", "Fall", "Winter"))
  invisible(res)
}

library(mapdata)
library(reshape)
library(data.table)

## climate value extraction + index computing

#list of study species (better use levels(data$species), but here I wanted the full scientific names, so manual entry)
species <- c("Eptesicus serotinus", "Eptesicus nilssonii", "Nyctalus lasiopterus", "Nyctalus noctula", "Nyctalus leisleri", 
             "Pipistrellus kuhlii", "Pistrellus nathusii", "Pipistrellus pugmaeus", "Plecorus auritus",
             "Plecotus austriacus", "Rhinolophus ferrumequinum", "Rhinolophus hipposideros", "Tadarida teniotis")

#creating an array of 3 dimentions (n species, 5 periods, 4 metrics) to store the outcome
thermal_indices <- array(dim = c(nlevels(as.factor(species)), 4, 5),
            dimnames = list(levels(as.factor(species)),
                            c("ThAve", "ThSD", "ThQ95", "ThQ5"),
                            c("yearly", levels(as.factor(season.var$season)))))

for(i in species){
  print(i)
  occ <- read.csv(paste0("******/gbif occurrences/", i, ".csv"), sep = "\t") #reading occurrence data after download from gbif
  map("worldHires", xlim = c(-20, 59), ylim = c(35, 71)) # visualising the data in Europe
  points(occ$decimalLatitude ~ occ$decimalLongitude)
  
  # controlling for spatial bias in gbif occurrences (picking 1 occurrence per cell)
  occ.raster <- rasterize(x = occ[, c("decimalLongitude", "decimalLatitude")],
                 y = raster(ListTifiles[1]),
                 fun = function(x, ...) 1)
  occ.unique <- xyFromCell(occ.raster, Which(occ.raster == 1, cells = T)) 
                          
  occ.unique$site_id <- rownames(occ.unique)
  colnames(occ.unique) <- c("longitude", "latitude", "site_id")
  occ.unique <- occ.unique[complete.cases(occ.unique$longitude), ]
  mean.temp <- point_grid_extract(mean.t, occ.unique) # extracting data downloaded from E-obs ; go have a coffee 
  mean.temp$season <- which.season(mean.temp$site_id)  # getting the season from date (named site_id after the point_grid_extract function)
  
  m.temp <- melt(mean.temp[2:ncol(mean.temp)], id = c("season")) # formatting into a decent neater data frame
    # getting the average (annual + seasonal) temperatures per site
  m.temp <- m.temp[complete.cases(m.temp), ]
  m.temp <- data.table(m.temp)
  yearly.var <- m.temp[, list(mean.y.t = mean(na.omit(value))),
                       by = list(variable)]
  season.var <- m.temp[, list(mean.season.t = mean(na.omit(value))),
                       by = list(variable, season)]
  
   # producing indices, storing them into the array
      # based on yearly temperatures
  thermal_indices[i, 1, 1] <- mean(yearly.var$mean.y.t)
  thermal_indices[i, 2, 1] <- sd(yearly.var$mean.y.t)
  thermal_indices[i, 3, 1] <- quantile(yearly.var$mean.y.t,0.95)
  thermal_indices[i, 4, 1] <- quantile(yearly.var$mean.y.t,0.05)
  
      # based on seasonal temperatures
  for(j in 2:5){
    sub <- season.var[season.var$season == levels(season.var$season)[j-1], ]
    thermal_indices[i, 1, j] <- mean(sub$mean.season.t)
    thermal_indices[i, 2, j] <- sd(sub$mean.season.t)
    thermal_indices[i, 3, j] <- quantile(sub$mean.season.t,0.95)
    thermal_indices[i, 4, j] <- quantile(sub$mean.season.t,0.05)
  }
  save(thermal_indices, file = "******/data/thermal indices/thermal_indices")
}

#checking (along with maps, see if consistent)
thermal_indices
thermal_indices[1, , ] #species 1
thermal_indices[, "ThAve", ] #metric 1 (same as thermal_indices[, 1, ])
thermal_indices[, , "Spring"] #season 1 (same as thermal_indices[, , 1])
thermal_indices[1, "ThAve", "Spring"]

