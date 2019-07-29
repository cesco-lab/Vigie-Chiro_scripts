
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

############### computing CSI based on all data (here, using participations > 10 days (change g10 into g2 (> 2 nights) or glist (full) if not wanted)

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


###############################################################################################################################
###############################################################################################################################
###############################################################################################################################
############ Dubos et al MEE paper: power analysis: computing CSI based on sample data (in order: for the 1st night, for the 1st + 2nd nights, etc...)

pawa <- c() 
for(j in levels(as.factor(ggg10$participation))) {
  sub <- ggg10[ggg10$participation == j, ]
  for(l in 1: nlevels(factor(sub$Nuit))){
    susub <- sub[sub$Nuit %in% levels(factor(sub$Nuit))[1:(l+1)], ] # sample data nights 1:n+1
    sususub <- sub[sub$Nuit %in% levels(factor(sub$Nuit))[1:l], ]   # sample data nights 1:n
    
    trueab <- sum(sub$nb_contacts) / unique(sub$n.night) # total abundance per day at a site based on all data (for abundance sensitivity analysis of CSI-richness power analysis)
    abundn1 <- sum(susub$nb_contacts) / nlevels(factor(susub$Nuit)) # mean abundance per day, estimated for n+1 night samples (for abundance power analysis)
    abundn <- sum(sususub$nb_contacts) / nlevels(factor(sususub$Nuit)) # mean abundance per day, estimated for n night samples (for abundance power analysis)
    ratio.abund <- ifelse(abundn1 < abundn, abundn1 / abundn, abundn / abundn1) #final response variable for abundance power analysis
     
    truerich <- nlevels(factor(sub$species)) # richness per site based on all data (for curiosity)
    richn1 <- nlevels(factor(susub$species)) #richness based on n+1 nights (for power analysis)
    richn <- nlevels(factor(sususub$species))#richness based on n nights (for power analysis)
    ratio.rich <- ifelse(richn1 < richn, richn1 / richn, richn / richn1) #final response variable for richness power analysis
    
     tcsi <- sum(na.omit(susub$corrected.ssi)*susub$nb_contact) / sum(na.omit(susub$nb_contact)) # CSI based on n+1 nights (for power analysis)
     wcsi <- sum(na.omit(sususub$corrected.ssi)*sususub$nb_contact) / sum(na.omit(sususub$nb_contact)) # CSI based on n nights (for power analysis)
      n.night <- l
      paw <- cbind(j, trueab, ratio.abund, ratio.rich, wcsi, tcsi, n.night)
      pawa <- as.data.frame(rbind(pawa, paw))
    }
  }

pawa$trueab <- as.numeric(as.character(pawa$trueab))
pawa$tcsi <- as.numeric(as.character(pawa$tcsi))
pawa$wcsi <- as.numeric(as.character(pawa$wcsi))
pawa$n.night <- as.numeric(as.character(pawa$n.night))

# final response variables for csi power analysis
#pawa$delta.csi <- (pawa$wcsi - pawa$tcsi) / pawa$sd.csi
#pawa$delta.csi.hui <- abs((pawa$wcsi - pawa$tcsi) / pawa$wcsi)
pawa$ratio.csi <- ifelse(pawa$wcsi < pawa$tcsi, pawa$wcsi / pawa$tcsi, pawa$tcsi / pawa$wcsi) 

######################################################## season habitat analysis #####################################################################
load("F:/bat veolia mnhn/data/pawacsi.n+1.godet")
jj <- read.csv("F:/bat veolia mnhn/data/PartSelG.csv", h = T,  sep = ",")
jj <- jj[, c("participation", "id_site", "longitude", "latitude", "date")]
library(tidyr)
jj <- separate(data = jj, col = date, into = c("date", "time"), sep = "\\ ")
jj$mysiteid <- paste0(jj$longitude, jj$latitude)
pawa <- merge(pawa, jj[, -c(5,6)], all.x = T, by.x = "j", by.y = "participation")
load("F:/bat veolia mnhn/data/sites")
sites$hab[sites$corine %in% c(1:11)] <- "Urban"
sites$hab[sites$corine %in% c(12:22)] <- "Agricultural"
sites$hab[sites$corine %in% c(23:25)] <- "Forest"
sites$hab[sites$corine %in% c(26:29, 32)] <- "Scrub/Herbaceous"
sites$hab[sites$corine %in% c(35:44)] <- "Wetlands/Water bodies"
sites$mysiteid <- paste0(sites$longitude, sites$latitude)
pawa <-  merge(pawa, sites, all.x = T, by = "mysiteid")
library(lubridate)
pawa$juldate <- yday(pawa$date)
pawa$month <- format(pawa$date, "%m") #get the month
pawa$season <- ifelse(pawa$month %in% c("03", "02"), "1.Feb-Mar",
                      ifelse(pawa$month %in% c("05", "04"), "2.Apr-May",
                             ifelse(pawa$month %in% c("07", "06"), "Jun-Jul",
                                    ifelse(pawa$month %in% c("09", "08"), "Aug-Sep",
                                           ifelse(pawa$month %in% c("11", "10"), "5.Oct-Nov", "6.Dec-Jan")))))
pawa <- droplevels(pawa)


#model + plotting predicted values
library(ggplot2)
################################################## plot univariate #####################################################################
df  <- pawa[complete.cases(pawa), ]
m <- glm(ratio.csi ~ scale(n.night), data = df, family = quasibinomial) 
inv.logit <- function(x) exp(x) / (1 + exp(x))
predcsi <- predict(m, newdata = df, se.fit = T)
df$predcsi <- inv.logit(predcsi[[1]])
df$lcsi <-  inv.logit(predcsi[[1]] - 1.96 * predcsi[[2]])
df$ucsi <-  inv.logit(predcsi[[1]] + 1.96 * predcsi[[2]])
m <- glm(ratio.rich ~ scale(n.night), data = df, family = quasibinomial) 
predrich <- predict(m, newdata = df, se.fit = T)
df$predrich <- inv.logit(predrich[[1]])
df$lrich <-  inv.logit(predrich[[1]] - 1.96 * predrich[[2]])
df$urich <-  inv.logit(predrich[[1]] + 1.96 * predrich[[2]])
m <- glm(ratio.abund ~ scale(n.night), data = df, family = quasibinomial) 
predabund <- predict(m, newdata = df, se.fit = T)
df$predabund <- inv.logit(predabund[[1]])
df$labund <-  inv.logit(predabund[[1]] - 1.96 * predabund[[2]])
df$uabund <-  inv.logit(predabund[[1]] + 1.96 * predabund[[2]])
ggplot(data = df, aes(x= n.night)) + xlim(c(0,20)) + xlab("Number of nights") + ylab("Precision indices") +
  geom_line(aes(y = predcsi, col = "CSI"), size = 1.2) +
  geom_ribbon(aes(ymin=lcsi, ymax=ucsi),alpha=0.3, fill = "green") +
  geom_line(aes(y = predrich, col = "Species richness"), size = 1.2) +
  geom_ribbon(aes(ymin=lrich, ymax=urich),alpha=0.3, fill = "blue") +
  geom_line(data = df, aes(y = predabund, col = "Bat abundance"), size = 1.2) +
  geom_ribbon(data=df,aes(ymin=(labund), ymax=(uabund)),alpha=0.3, fill = "red") +
  theme_bw() +  guides(color=guide_legend("Community metric")) 

#########################################################################################################
m <- glm(ratio.csi ~ scale(n.night) * scale(Urban) +
           scale(n.night) * scale(Forest) + 
           scale(n.night) * scale(Scrub_Herbaceous) + 
           # scale(n.night) * scale(trueab) +
           scale(n.night) * season, 
         data = df, family = quasibinomial) 
summary(m)

### plot predicted
#4plots for habitats
df  <- pawa[complete.cases(pawa), ]
df$trueab = mean(df$trueab)
df$mysiteid = levels(as.factor(df$mysiteid))[1]
df$season = levels(as.factor(df$season))[2]
# df$pred <- predict(m, newdata = df, type = "response")

dfu <- df
dfu$Urban = 1
dfu$Forest = 0
dfu$Scrub_Herbaceous = 0
pred <- predict(m, newdata = dfu, se.fit = T)
dfu$pred <- inv.logit(pred[[1]])
dfu$l <-  inv.logit(pred[[1]] - 1.96 * pred[[2]])
dfu$u <-  inv.logit(pred[[1]] + 1.96 * pred[[2]])
dff <- df
dff$Urban = 0
dff$Forest = 1
dff$Scrub_Herbaceous = 0
pred <- predict(m, newdata = dff, se.fit = T)
dff$pred <- inv.logit(pred[[1]])
dff$l <-  inv.logit(pred[[1]] - 1.96 * pred[[2]])
dff$u <-  inv.logit(pred[[1]] + 1.96 * pred[[2]])
dfs <- df
dfs$Urban = 0
dfs$Forest = 0
dfs$Scrub_Herbaceous = 1
pred <- predict(m, newdata = dfs, se.fit = T)
dfs$pred <- inv.logit(pred[[1]])
dfs$l <-  inv.logit(pred[[1]] - 1.96 * pred[[2]])
dfs$u <-  inv.logit(pred[[1]] + 1.96 * pred[[2]])
dfa <- df
dfa$Urban = 0
dfa$Forest = 0
dfa$Scrub_Herbaceous = 0
pred <- predict(m, newdata = dfa, se.fit = T)
dfa$pred <- inv.logit(pred[[1]])
dfa$l <-  inv.logit(pred[[1]] - 1.96 * pred[[2]])
dfa$u <-  inv.logit(pred[[1]] + 1.96 * pred[[2]])

p1 <- ggplot() + xlim(c(0,20)) + xlab("") + ylab("CSI precision index") +
  geom_line(data = dfu, aes(x= n.night, y = pred, col = "Urban"), size = 2) +
  geom_ribbon(data = dfu, aes(x= n.night, ymin=l, ymax=u),alpha=0.3, fill = "purple") +
  geom_line(data = dfa, aes(x= n.night, y = pred, col = "Agricultural"), size = 2) +
  geom_ribbon(data = dfa, aes(x= n.night, ymin=l, ymax=u),alpha=0.3, fill = "red") +
  geom_line(data = dff, aes(x= n.night, y = pred, col = "Forest"), size = 2) +
  geom_ribbon(data = dff, aes(x= n.night, ymin=l, ymax=u),alpha=0.3, fill = "green") +
  geom_line(data = dfs, aes(x= n.night, y = pred, col = "Scrubland"), size = 2) + theme_bw()+
  geom_ribbon(data = dfs, aes(x= n.night, ymin=l, ymax=u),alpha=0.3, fill = "blue") +
  guides(color=guide_legend("Dominant \n habitat type")) # + theme(legend.position="none")


df  <- pawa[complete.cases(pawa), ]
df$trueab = mean(df$trueab)
df$mysiteid = levels(as.factor(df$mysiteid))[1]
df$Urban = mean(df$Urban)
df$Forest = mean(df$Forest)
df$Scrub_Herbaceous = mean(df$Scrub_Herbaceous)
pred <- predict(m, newdata = df, se.fit = T)
df$pred <- inv.logit(pred[[1]])
df$l <-  inv.logit(pred[[1]] - 1.96 * pred[[2]])
df$u <-  inv.logit(pred[[1]] + 1.96 * pred[[2]])

p2 <- ggplot(df, aes(x = n.night, y = pred, col = season)) + xlim(c(0,20)) +
  geom_line(size = 2) + xlab("Number of nights")+ ylab("CSI precision index") +
  geom_ribbon(aes(ymin=l, ymax=u, fill = season), alpha=0.3) +
  theme_bw() + 
  guides(color=guide_legend("Season")) + guides(color=FALSE)#  + theme(legend.position="none") +



m <- glm(ratio.rich ~ scale(n.night) * scale(Urban) +
           scale(n.night) * scale(Forest) + 
           scale(n.night) * scale(Scrub_Herbaceous) + 
           # scale(n.night) * scale(trueab) +
           scale(n.night) * season, 
         data = pawa, family = quasibinomial) 
summary(m)

### plot predicted
#4plots for habitats
df  <- pawa[complete.cases(pawa), ]
df$trueab = mean(df$trueab)
df$mysiteid = levels(as.factor(df$mysiteid))[1]
df$season = levels(as.factor(df$season))[2]

dfu <- df
dfu$Urban = 1
dfu$Forest = 0
dfu$Scrub_Herbaceous = 0
pred <- predict(m, newdata = dfu, se.fit = T)
dfu$pred <- inv.logit(pred[[1]])
dfu$l <-  inv.logit(pred[[1]] - 1.96 * pred[[2]])
dfu$u <-  inv.logit(pred[[1]] + 1.96 * pred[[2]])

dff <- df
dff$Urban = 0
dff$Forest = 1
dff$Scrub_Herbaceous = 0
pred <- predict(m, newdata = dff, se.fit = T)
dff$pred <- inv.logit(pred[[1]])
dff$l <-  inv.logit(pred[[1]] - 1.96 * pred[[2]])
dff$u <-  inv.logit(pred[[1]] + 1.96 * pred[[2]])
dfs <- df
dfs$Urban = 0
dfs$Forest = 0
dfs$Scrub_Herbaceous = 1
pred <- predict(m, newdata = dfs, se.fit = T)
dfs$pred <- inv.logit(pred[[1]])
dfs$l <-  inv.logit(pred[[1]] - 1.96 * pred[[2]])
dfs$u <-  inv.logit(pred[[1]] + 1.96 * pred[[2]])
dfa <- df
dfa$Urban = 0
dfa$Forest = 0
dfa$Scrub_Herbaceous = 0
pred <- predict(m, newdata = dfa, se.fit = T)
dfa$pred <- inv.logit(pred[[1]])
dfa$l <-  inv.logit(pred[[1]] - 1.96 * pred[[2]])
dfa$u <-  inv.logit(pred[[1]] + 1.96 * pred[[2]])

p3 <- ggplot() + xlim(c(0,20)) + xlab("") + ylab("Species richness precision index") +
  geom_line(data = dfu, aes(x= n.night, y = pred, col = "Urban"), size = 2) +
  geom_ribbon(data = dfu, aes(x= n.night, ymin=l, ymax=u),alpha=0.3, fill = "purple") +
  geom_line(data = dfa, aes(x= n.night, y = pred, col = "Agricultural"), size = 2) +
  geom_ribbon(data = dfa, aes(x= n.night, ymin=l, ymax=u),alpha=0.3, fill = "red") +
  geom_line(data = dff, aes(x= n.night, y = pred, col = "Forest"), size = 2) +
  geom_ribbon(data = dff, aes(x= n.night, ymin=l, ymax=u),alpha=0.3, fill = "green") +
  geom_line(data = dfs, aes(x= n.night, y = pred, col = "Scrubland"), size = 2) + theme_bw()+
  geom_ribbon(data = dfs, aes(x= n.night, ymin=l, ymax=u),alpha=0.3, fill = "blue") +
  guides(color=guide_legend("Dominant \n habitat type")) #+ theme(legend.position="none")



df  <- pawa[complete.cases(pawa), ]
df$trueab = mean(df$trueab)
df$mysiteid = levels(as.factor(df$mysiteid))[1]
df$Urban = mean(df$Urban)
df$Forest = mean(df$Forest)
df$Scrub_Herbaceous = mean(df$Scrub_Herbaceous)
pred <- predict(m, newdata = df, se.fit = T)
df$pred <- inv.logit(pred[[1]])
df$l <-  inv.logit(pred[[1]] - 1.96 * pred[[2]])
df$u <-  inv.logit(pred[[1]] + 1.96 * pred[[2]])

p4 <- ggplot(df, aes(x = n.night, y = pred, col = season)) + xlim(c(0,20)) +
  geom_line(size = 2) + xlab("Number of nights")+ ylab("Species richness precision index") +
  geom_ribbon(aes(ymin=l, ymax=u, fill = season), alpha=0.3) +
  theme_bw() + 
  guides(color=guide_legend("Season")) #+ theme(legend.position="none")


m <- glm(ratio.abund ~ scale(n.night) * scale(Urban) +
           scale(n.night) * scale(Forest) + 
           scale(n.night) * scale(Scrub_Herbaceous) + 
           # scale(n.night) * scale(trueab) +
           scale(n.night) * season, 
         data = pawa, family = quasibinomial) 
summary(m)

### plot predicted
#4plots for habitats
df  <- pawa[complete.cases(pawa), ]
df$trueab = mean(df$trueab)
df$mysiteid = levels(as.factor(df$mysiteid))[1]
df$season = levels(as.factor(df$season))[2]

dfu <- df
dfu$Urban = 1
dfu$Forest = 0
dfu$Scrub_Herbaceous = 0
pred <- predict(m, newdata = dfu, se.fit = T)
dfu$pred <- inv.logit(pred[[1]])
dfu$l <-  inv.logit(pred[[1]] - 1.96 * pred[[2]])
dfu$u <-  inv.logit(pred[[1]] + 1.96 * pred[[2]])
dff <- df
dff$Urban = 0
dff$Forest = 1
dff$Scrub_Herbaceous = 0
pred <- predict(m, newdata = dff, se.fit = T)
dff$pred <- inv.logit(pred[[1]])
dff$l <-  inv.logit(pred[[1]] - 1.96 * pred[[2]])
dff$u <-  inv.logit(pred[[1]] + 1.96 * pred[[2]])
dfs <- df
dfs$Urban = 0
dfs$Forest = 0
dfs$Scrub_Herbaceous = 1
pred <- predict(m, newdata = dfs, se.fit = T)
dfs$pred <- inv.logit(pred[[1]])
dfs$l <-  inv.logit(pred[[1]] - 1.96 * pred[[2]])
dfs$u <-  inv.logit(pred[[1]] + 1.96 * pred[[2]])
dfa <- df
dfa$Urban = 0
dfa$Forest = 0
dfa$Scrub_Herbaceous = 0
pred <- predict(m, newdata = dfa, se.fit = T)
dfa$pred <- inv.logit(pred[[1]])
dfa$l <-  inv.logit(pred[[1]] - 1.96 * pred[[2]])
dfa$u <-  inv.logit(pred[[1]] + 1.96 * pred[[2]])

p5 <- ggplot() + xlim(c(0,20)) + xlab("") + ylab("Abundance precision index") +
  geom_line(data = dfu, aes(x= n.night, y = pred, col = "Urban"), size = 2) +
  geom_ribbon(data = dfu, aes(x= n.night, ymin=l, ymax=u),alpha=0.3, fill = "purple") +
  geom_line(data = dfa, aes(x= n.night, y = pred, col = "Agricultural"), size = 2) +
  geom_ribbon(data = dfa, aes(x= n.night, ymin=l, ymax=u),alpha=0.3, fill = "red") +
  geom_line(data = dff, aes(x= n.night, y = pred, col = "Forest"), size = 2) +
  geom_ribbon(data = dff, aes(x= n.night, ymin=l, ymax=u),alpha=0.3, fill = "green") +
  geom_line(data = dfs, aes(x= n.night, y = pred, col = "Scrubland"), size = 2) + theme_bw()+
  geom_ribbon(data = dfs, aes(x= n.night, ymin=l, ymax=u),alpha=0.3, fill = "blue") +
  guides(color=guide_legend("Dominant \n habitat type"))


df  <- pawa[complete.cases(pawa), ]
df$trueab = mean(df$trueab)
df$mysiteid = levels(as.factor(df$mysiteid))[1]
df$Urban = mean(df$Urban)
df$Forest = mean(df$Forest)
df$Scrub_Herbaceous = mean(df$Scrub_Herbaceous)
pred <- predict(m, newdata = df, se.fit = T)
df$pred <- inv.logit(pred[[1]])
df$l <-  inv.logit(pred[[1]] - 1.96 * pred[[2]])
df$u <-  inv.logit(pred[[1]] + 1.96 * pred[[2]])

p6 <- ggplot(df, aes(x = n.night, y = pred, col = season)) + xlim(c(0,20)) +
  geom_line(size = 2) + xlab("Number of nights")+ ylab("Abundance precision index") +
  geom_ribbon(aes(ymin=l, ymax=u, fill = season), alpha=0.3) +
  theme_bw() + 
  guides(color=guide_legend("Season"))

library(ggpubr)
ggarrange(p1, p3, p5, ncol = 3, common.legend = T, legend = "right")
ggarrange(p2, p4, p6, ncol = 3, common.legend = T, legend = "right")
