
####### corine extraction
library(raster)
ras <- raster("xxx/g100_clc12_V18_5a/g100_clc12_V18_5.tif") # corine 2012
# ras <- raster("xxx/clc2018_clc2018_v2018_20b2_raster100m/clc2018_clc2018_V2018.20b2.tif") #corine 2018
# ras <- raster("xxx/OCS_2017_CESBIO.tif", crs = "+proj=longlat +datum=WGS84")  #OSO

ras <- crop(ras, extent(3.2e+06, 4.3e+06, 2e+06, 3.2e+06)) # crop the file to France and surroundings

####### get study sites and format them to the raster
site1 <- read.csv("F:/bat veolia mnhn/pourNico/sites_localites.txt", h = T, sep = "\t")
sites <- site1[, c(12, 13, 2)]

library(sp)
coordinates(sites) <- ~ longitude + latitude
plot(sites)

proj4string(sites) <- CRS("+proj=longlat +datum=WGS84")
sitess <- spTransform(sites, CRS(projection(ras)))
plot(ras, axes = T)
plot(sitess,add=T)


####### extract corine categories at site locations
# Corine values correspond to "Grid_Code". To see the true categories referred, see http://clc.gios.gov.pl/index.php/9-gorne-menu/clc-informacje-ogolne/58-klasyfikacja-clc-2 
library(rgeos)
sites$corine <- extract(ras, sites, na.rm = T)
# save(sites, file = "xxx/data/sites")
load("xxx/data/sites")

####### selecting corine categories that are fairly represented (not applied in our study, I prefered to pull 3-number categories into 2 number categories)
# sites <- sites[sites$corine %in% c("1", "2", "3", "10", "12", "15", "18", "20", "21", "23", "24", "25", "26", "27", "28", "29"), ]

####### pulling corine sub-sub-categories (3 figures) into sub-categories (2 figures)
sites$corine2 <- sites$corine
sites$corine2 <- gsub("\\b4\\b","3", sites$corine2)
sites$corine2 <- gsub("\\b5\\b","3", sites$corine2)
sites$corine2 <- gsub("\\b6\\b","3", sites$corine2)
sites$corine2 <- gsub("\\b8\\b","7", sites$corine2)
sites$corine2 <- gsub("\\b9\\b","7", sites$corine2)
sites$corine2 <- gsub("\\b11\\b","10", sites$corine2)
sites$corine2 <- gsub("\\b14\\b","12", sites$corine2)
sites$corine2 <- gsub("\\b16\\b","15", sites$corine2)
sites$corine2 <- gsub("\\b17\\b","15", sites$corine2)
sites$corine2 <- gsub("\\b20\\b","21", sites$corine2)
sites$corine2 <- gsub("\\b24\\b","23", sites$corine2)#
sites$corine2 <- gsub("\\b25\\b","23", sites$corine2)#
sites$corine2 <- gsub("\\b27\\b","26", sites$corine2)
sites$corine2 <- gsub("\\b28\\b","26", sites$corine2)
sites$corine2 <- gsub("\\b29\\b","26", sites$corine2)
sites$corine2 <- gsub("\\b30\\b","32", sites$corine2)
sites$corine2 <- gsub("\\b31\\b","32", sites$corine2)
sites$corine2 <- gsub("\\b36\\b","35", sites$corine2)
sites$corine2 <- gsub("\\b38\\b","37", sites$corine2)
sites$corine2 <- gsub("\\b39\\b","37", sites$corine2)
sites$corine2 <- gsub("\\b41\\b","40", sites$corine2)


####### pulling sub-categories into categories (1 figure)
sites$hab[sites$corine %in% c(1:11)] <- "Urban"
sites$hab[sites$corine %in% c(12:22)] <- "Agricultural"
sites$hab[sites$corine %in% c(23:25)] <- "Forest"
sites$hab[sites$corine %in% c(26:29, 32)] <- "Scrub/Herbaceous"
sites$hab[sites$corine %in% c(35:44)] <- "Wetlands/Water bodies"
sites$mysiteid <- paste0(sites$longitude, sites$latitude)

############### SSI computing (Julliard et al. 2006 methods)
load("xxx/data/sites")
load("xxx/data/glist")

refs <- read.csv("xxx/p_export.csv", sep = ";")
refs <- refs[, c("participation", "idsite")]

SSI <- c()
for(i in names(glist)[c(3, 8, 9, 13:19, 21:25, 27:39, 41:46)]){  # subset only bat species
  v <- merge(glist[[i]], refs, all.x = T, by = "participation")
  v <- merge(v, sites, all.x = T, by.x = "idsite", by.y = "id_site")
  species <- i
  dens <- c()
    for(j in levels(as.factor(v$corine2))){    ### basing SSI on 2nd level CORINE hierarchy
    sub <- v[v$corine2 == j, ]
  hab <- j
  mean.count <- mean(na.omit(sub$nb_contact))
  # pres <- ifelse(mean.count == 0, 0, 1)  # useful only for SSI based on presence absence; see Julliard, R., J. Clavel, V. Devictor, F. Jiguet, and D. Couvet. 2006. Spatial segregation of specialists and generalists in bird communities. Ecology letters 9:1237–44.
  denss <- as.data.frame(cbind(hab, mean.count)) #, pres))
  dens <- as.data.frame(rbind(dens, denss))
  }
ssi <- sd(as.numeric(as.character(dens$mean.count)))/mean(as.numeric(as.character(dens$mean.count)))
# ssi.presence <- ((nlevels(as.factor(vach$corine))/(sum(as.numeric(as.character(dens$pres))))) -1 )^0.5  # computing CSI based on presence absence
SSIx <- cbind(species, ssi) #, ssi.presence)
SSI <- rbind(SSI, SSIx)
}


SSI <- as.data.frame(SSI)
SSI$ssi <- as.numeric(as.character(SSI$ssi))
#SSI$ssi.presence <- as.numeric(as.character(SSI$ssi.presence))
save(SSI, file = "F:/bat veolia mnhn/data/SSI")


############### Corrected SSI computing (Godet et al. 2014 methods), with data visualisation
load("F:/bat veolia mnhn/data/SSI")
library(mapdata)

# 1. computing simulated SSI (based on randomised habitats)
par(mfrow = c(2,2))
r.SSI <- c()
for(i in names(glist)[c(3, 8, 9, 13:19, 21:25, 27:39, 41:46)]){  #
  v <- merge(glist[[i]], refs, all.x = T, by = "participation")
  v <- merge(v, sites, all.x = T, by.x = "idsite", by.y = "id_site")
  species <- i
  print(i)
  print(nlevels(factor(v$idsite)))
  print(nlevels(factor(v$mysiteid)))
  print(nrow(vach))
    r.ssi <- c()
  for(k in 1:1000) {
    dens <- c()
    vach$r.corine <- sample(v$corine, replace = T)  # habitat randomisation (into a 1000 itaration loop)
    for(j in levels(as.factor(v$corine2))){
    sub <- vach[vach$r.corine2 == j, ]
    hab <- j
    mean.count <- mean(na.omit(sub$nb_contact))
    denss <- as.data.frame(cbind(hab, mean.count)) #, pres))
    dens <- as.data.frame(rbind(dens, denss))
    }  
    r.ssi <- rbind(r.ssi, sd(as.numeric(as.character(dens$mean.count)))/mean(as.numeric(as.character(dens$mean.count))))
  }
    r.SSIx <- cbind(species, r.ssi) #, ssi.presence)
  r.SSI <- rbind(r.SSI, r.SSIx)
  map("worldHires", 'France', xlim = c(-4.9,10), ylim = c(41,51.5)) ### visualising data (points size is proportional to abundance)
  points(vach$longitude, vach$latitude, cex = (vach$nb_contacts/100))
  hist(r.ssi, main = i, xlim = c(0,3))
}

r.SSI <- as.data.frame(r.SSI)
colnames(r.SSI) <- c("species", "simulated.ssi")
r.SSI$simulated.ssi <- as.numeric(as.character(r.SSI$simulated.ssi))
# save(r.SSI, file = "F:/bat veolia mnhn/data/r.SSI")
load("F:/bat veolia mnhn/data/r.SSI")

# 2. computing 'corrected SSI'
rssi <- c()
for(i in levels(r.SSI$species)){
  s <- r.SSI[r.SSI$species == i, ]
  species <- i
  simulated.ssi <- mean(s$simulated.ssi)
  sd.simulated.ssi <- sd(s$simulated.ssi)
  rssi <- as.data.frame(rbind(rssi, cbind(species, simulated.ssi, sd.simulated.ssi)))
}
rssi$simulated.ssi <- as.numeric(as.character(rssi$simulated.ssi))
rssi$sd.simulated.ssi <- as.numeric(as.character(rssi$sd.simulated.ssi))
SSI <- merge(SSI, rssi, by = "species")
SSI$corrected.ssi <- (SSI$ssi - SSI$simulated.ssi) / SSI$sd.simulated.ssi

# save(SSI, file = "F:/bat veolia mnhn/data/SSI.Godet")

############### computing CSI based on participations > 2 days (g2). Use glist for full data

load("F:/bat veolia mnhn/data/g2")
load("F:/bat veolia mnhn/data/SSI.Godet")
gg10 <- do.call("rbind", g2[c(3, 8, 9, 13:19, 21:24, 27:39, 41:44, 46)]) # rbind list elements

gg10 <- merge(SSI, gg10, all.y = T, by.x = "species", by.y = "espece")
gg10 <- gg10[gg10$groupe == "bat", ]
gg10 <- gg10[, c(1:6, 8, 15)]
gg10 <- gg10[complete.cases(gg10), ]
gg10 <- droplevels(gg10)
ggg10 <- c()
for(j in levels(as.factor(gg10$participation))) {
  sub <- gg10[gg10$participation == j, ]
  # CSI based on full data is named 'truecsi'
  if(sum(na.omit(sub$nb_contact)) != 0) sub$truecsi <- sum(na.omit(sub$corrected.ssi)*sub$nb_contact) / sum(na.omit(sub$nb_contact)) else sub$truecsi <- 0
   # sub$truecsi.p <- sum(na.omit(sub$ssi.presence)*sub$nb_contact) / sum(sub$nb_contact) # CSI for presence-absence
  sub$n.night <- nlevels(as.factor(sub$Nuit))
  ggg10 <- rbind(ggg10, sub)
}

# save(ggg10, file = "F:/bat veolia mnhn/data/ggg10")
