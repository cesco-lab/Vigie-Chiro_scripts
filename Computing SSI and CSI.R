
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
sites$corine <- gsub("\\b4\\b","3", sites$corine)
sites$corine <- gsub("\\b5\\b","3", sites$corine)
sites$corine <- gsub("\\b6\\b","3", sites$corine)
sites$corine <- gsub("\\b8\\b","7", sites$corine)
sites$corine <- gsub("\\b9\\b","7", sites$corine)
sites$corine <- gsub("\\b11\\b","10", sites$corine)
sites$corine <- gsub("\\b14\\b","12", sites$corine)
sites$corine <- gsub("\\b16\\b","15", sites$corine)
sites$corine <- gsub("\\b17\\b","15", sites$corine)
sites$corine <- gsub("\\b20\\b","21", sites$corine)
sites$corine <- gsub("\\b24\\b","23", sites$corine)#
sites$corine <- gsub("\\b25\\b","23", sites$corine)#
sites$corine <- gsub("\\b27\\b","26", sites$corine)
sites$corine <- gsub("\\b28\\b","26", sites$corine)
sites$corine <- gsub("\\b29\\b","26", sites$corine)
sites$corine <- gsub("\\b30\\b","32", sites$corine)
sites$corine <- gsub("\\b31\\b","32", sites$corine)
sites$corine <- gsub("\\b36\\b","35", sites$corine)
sites$corine <- gsub("\\b38\\b","37", sites$corine)
sites$corine <- gsub("\\b39\\b","37", sites$corine)
sites$corine <- gsub("\\b41\\b","40", sites$corine)
sites <- sites[!sites$corine %in% c("42", "43", "44", "48"), ]

####### pulling sub-categories into categories (1 figure)
sites$hab[sites$corine %in% c(1:11)] <- "Urban"
sites$hab[sites$corine %in% c(12:22)] <- "Agricultural"
sites$hab[sites$corine %in% c(23:25)] <- "Forest"
sites$hab[sites$corine %in% c(26:29, 32)] <- "Scrub/Herbaceous"
sites$hab[sites$corine %in% c(35:44)] <- "Wetlands/Water bodies"
sites$mysiteid <- paste0(sites$longitude, sites$latitude)

############### SSI computing
load("xxx/data/sites")

load("xxx/data/g2")
load("xxx/data/g10")
load("xxx/data/glist")

refs <- read.csv("xxx/p_export.csv", sep = ";")
refs <- refs[, c("participation", "idsite")]

SSI <- c()
for(i in names(glist)[c(3, 8, 9, 13:19, 21:25, 27:39, 41:46)]){  # subset only bat species
  v <- merge(glist[[i]], refs, all.x = T, by = "participation")
  v <- merge(v, sites, all.x = T, by.x = "idsite", by.y = "id_site")
  species <- i
  dens <- c()
    for(j in levels(as.factor(v$corine))){
    sub <- v[v$corine == j, ]
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
SSI$ssi.presence <- as.numeric(as.character(SSI$ssi.presence))
save(SSI, file = "F:/bat veolia mnhn/data/SSI")


############### computing CSI based on all data (here, using participations > 10 days (change g10 into g2 (> 2 nights) or glist (full) if not wanted)

load("F:/bat veolia mnhn/data/g10")
load("F:/bat veolia mnhn/data/SSI")
gg10 <- do.call("rbind", glist[c(3, 8, 9, 13:19, 21:24, 27:39, 41:44, 46)]) # rbind list elements

gg10 <- merge(SSI, gg10, all.y = T, by.x = "species", by.y = "espece")
gg10 <- gg10[gg10$groupe == "bat", ]
gg10 <- gg10[, c(1:6, 8, 15)]
gg10 <- gg10[complete.cases(gg10), ]
gg10 <- droplevels(gg10)
ggg10 <- c()
for(j in levels(as.factor(gg10$participation))) {
  sub <- gg10[gg10$participation == j, ]
  sub$truecsi <- sum(na.omit(sub$ssi)*sub$nb_contact) / sum(na.omit(sub$nb_contact))  # CSI based on full data is named 'truecsi'
  sub$truecsi.p <- sum(na.omit(sub$ssi.presence)*sub$nb_contact) / sum(sub$nb_contact) # CSI for presence-absence
  sub$n.night <- nlevels(as.factor(sub$Nuit))
  ggg10 <- rbind(ggg10, sub)
}


# save(ggg10, file = "F:/bat veolia mnhn/data/ggg10")

############ power analysis: computing CSI based on sample data (in order: for the 1st night, for the 1st + 2nd nights, etc...)

pawa <- c() 
for(j in levels(as.factor(ggg10$participation))) {
  sub <- ggg10[ggg10$participation == j, ]
  for(l in 1: nlevels(factor(sub$Nuit))){
    susub <- sub[sub$Nuit %in% levels(factor(sub$Nuit))[1:(l+1)], ] # sample data nights 1:n+1
    sususub <- sub[sub$Nuit %in% levels(factor(sub$Nuit))[1:l], ]   # sample data nights 1:n
    
    trueab <- sum(sub$nb_contacts) / sub$n.night # total abundance per day at a site based on all data (for abundance sensitivity analysis of CSI-richness power analysis)
    abundn1 <- sum(susub$nb_contacts) / nlevels(factor(susub$Nuit)) # mean abundance per day, estimated for n+1 night samples (for abundance power analysis)
    abundn <- sum(sususub$nb_contacts) / nlevels(factor(sususub$Nuit)) # mean abundance per day, estimated for n night samples (for abundance power analysis)
    ratio.abund <- ifelse(abundn1 < abundn, abundn1 / abundn, abundn / abundn1) #final response variable for abundance power analysis
     
    truerich <- nlevels(factor(sub$species)) # richness per site based on all data (for curiosity)
    richn1 <- nlevels(factor(susub$species)) #richness based on n+1 nights (for power analysis)
    richn <- nlevels(factor(sususub$species))#richness based on n nights (for power analysis)
    ratio.rich <- ifelse(richn1 < richn, richn1 / richn, richn / richn1) #final response variable for abundance power analysis
    
     tcsi <- sum(na.omit(susub$ssi)*susub$nb_contact) / sum(na.omit(susub$nb_contact)) # CSI based on n+1 nights (for power analysis)
     wcsi <- sum(na.omit(sususub$ssi)*sususub$nb_contact) / sum(na.omit(sususub$nb_contact)) # CSI based on n nights (for power analysis)
      n.night <- l
      paw <- cbind(j, trueab, ratio.abund, ratio.rich, wcsi, tcsi, n.night)
      pawa <- as.data.frame(rbind(pawa, paw))
    }
  }

pawa$trueab <- as.numeric(as.character(pawa$trueab))
pawa$tcsi <- as.numeric(as.character(pawa$tcsi))
pawa$wcsi <- as.numeric(as.character(pawa$wcsi))
pawa$n.night <- as.numeric(as.character(pawa$n.night))
pawa <- pawa[!duplicated(pawa), ]

pawa$delta.csi <- (pawa$wcsi - pawa$tcsi) / pawa$sd.csi
pawa$delta.csi.hui <- abs((pawa$wcsi - pawa$tcsi) / pawa$wcsi)
pawa$ratio.csi <- ifelse(pawa$wcsi < pawa$tcsi, pawa$wcsi / pawa$tcsi, pawa$tcsi / pawa$wcsi) 

