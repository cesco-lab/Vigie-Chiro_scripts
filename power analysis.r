
############################### 1. producing data selected on the basis of the participation length (number of nights)

gg <- read.csv("SpNuit2_Seuil90_DataLP_PF_exportTot.csv", h = T, sep = ",")


gg$Nuit <- as.Date(gg$Nuit, format = "%Y-%m-%d")
gg$month <- as.numeric(format(gg$Nuit, "%m"))
# gg <- gg[gg$num_micro == "0", ]

head(gg)
sort(table(gg$espece))

#### getting the 0
gg$unit <- paste(gg$participation, gg$Nuit, gg$num_micro) # get sampling units (fusion de participation, nuit et micro; reséparation dans la boucle en dessous)
unit <- as.data.frame(levels(as.factor(gg$unit)))
colnames(unit) <- "unit"
glist <- list()

for(i in levels(gg$espece)){
  sub <- gg[gg$espece == i, ]
  sub <- droplevels(sub)
  glist[[i]] <- merge(sub, unit, by = "unit", all.y = T)
  glist[[i]]$nb_contacts[is.na(glist[[i]]$nb_contacts)] <- 0
  glist[[i]]$participation <- as.character(glist[[i]]$participation)
  glist[[i]]$participation[is.na(glist[[i]]$participation)] <- substring(glist[[i]]$unit[is.na(glist[[i]]$participation)], 1, 24) # récupération des participation
  glist[[i]]$Nuit <- as.character(glist[[i]]$Nuit)
  glist[[i]]$Nuit[is.na(glist[[i]]$Nuit)] <- substring(glist[[i]]$unit[is.na(glist[[i]]$Nuit)], 26, 35) # récup nuit
  glist[[i]]$num_micro[is.na(glist[[i]]$num_micro)] <- substring(glist[[i]]$unit[is.na(glist[[i]]$num_micro)], 36, 37) # récup micro
  glist[[i]]$num_micro <- as.numeric(glist[[i]]$num_micro)
}


# save(glist, file = "xxx/data/glist")

#load("xxx/data/glist")


###☺ get number of nights per participation
n.night <- c()
for(i in levels(as.factor(gg$participation))){
  sub <- gg[gg$participation == i, ]
  sub <- droplevels(sub)
  n <- cbind(i, nlevels(as.factor(sub$Nuit)))
  n.night <- as.data.frame(rbind(n.night, n))
}

colnames(n.night) <- c("session", "n")
n.night$n <- as.numeric(as.character(n.night$n))

head(n.night)
table(n.night$n)

# select ref sessions (>2, >5 and >= 10 sessions)
ref2 <- n.night[n.night$n >= 2, ]
ref5 <- n.night[n.night$n >= 5, ]
ref10 <- n.night[n.night$n >= 10, ]
# save(ref, file = "xxx/data/ref10")



### extract participations with n nights
g2 <- list()
 g5 <- list()
 g10 <- list()
for(i in levels(gg$espece)){
  g2[[i]] <- glist[[i]][glist[[i]]$participation %in% ref2$session, ]
 g5[[i]] <- glist[[i]][glist[[i]]$participation %in% ref5$session, ]
 g10[[i]] <- glist[[i]][glist[[i]]$participation %in% ref10$session, ]
}

head(g2[[i]])

# save(g2, file = "F:/bat veolia mnhn/data/g2")
# save(g5, file = "F:/bat veolia mnhn/data/g5")
# save(g10, file = "F:/bat veolia mnhn/data/g10")


############################### 2. comparing number of contacts per species between participations of different minimal lenght

#load("F:/bat veolia mnhn/data/g2")
#load("F:/bat veolia mnhn/data/g5")
#load("F:/bat veolia mnhn/data/g10")

#load("F:/bat veolia mnhn/data/glist")
par(mfrow = c(1,2))

mean.contact <- data.frame()
mc <- c()
for(i in levels(gg$espece)){
  # hist(glist[[i]]$nb_contacts, main = paste(i, "1 day"))
  # hist(g10[[i]]$nb_contacts, main = paste(i, "10 days"))
  one.day <- mean(glist[[i]]$nb_contacts)
  two.days <- mean(g2[[i]]$nb_contacts)
  five.days <- mean(g5[[i]]$nb_contacts)
  ten.days <- mean(g10[[i]]$nb_contacts)
  mc <- cbind(i, one.day, two.days, five.days, ten.days)
  mean.contact <- rbind(mean.contact, mc)
}


mean.contact <- mean.contact[-c(20, 26), ]
mean.contact$one.day <- as.numeric(as.character(mean.contact$one.day))
mean.contact$two.days <- as.numeric(as.character(mean.contact$two.days))
mean.contact$five.days <- as.numeric(as.character(mean.contact$five.days))
mean.contact$ten.days <- as.numeric(as.character(mean.contact$ten.days))
plot(mean.contact$one.day ~ mean.contact$two.days, pch = 20, col = "green",
     ylab = "Number of contact (participation > 1 day)", xlab = "Nb. contact longer participation",
     xlim = c(0, 710), ylim = c(0, 710))
points(mean.contact$one.day ~ mean.contact$five.days, pch = 20, col = "orange")
points(mean.contact$one.day ~ mean.contact$ten.days, pch = 20, col = "red")
legend("topleft", c("> 2 days","> 5 days","> 10 days"), cex=0.9, 
       bty="n", fill=c("green", "orange", "red"), title = "Participation length")
abline(0,1)


# zoom plot (rare species)
plot(mean.contact$one.day ~ mean.contact$two.days, pch = 20, col = "green",
     ylab = "Number of contact (participation > 1 day)", xlab = "Nb. contact longer participation",
     xlim = c(0, 10), ylim = c(0, 10))
points(mean.contact$one.day ~ mean.contact$five.days, pch = 20, col = "orange")
points(mean.contact$one.day ~ mean.contact$ten.days, pch = 20, col = "red")
legend("topleft", c("> 2 days","> 5 days","> 10 days"), cex=0.9, 
       bty="n", fill=c("green", "orange", "red"), title = "Participation length")
abline(0,1)


