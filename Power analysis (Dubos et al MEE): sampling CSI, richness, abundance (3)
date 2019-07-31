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
