
gg <- read.csv("***/SpNuit2_Seuil90_DataLP_PF_exportTot.csv", h = T, sep = ",")


gg$Nuit <- as.Date(gg$Nuit, format = "%Y-%m-%d")
gg$month <- as.numeric(format(gg$Nuit, "%m"))

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


# save(glist, file = "F:/bat veolia mnhn/data/glist")

load("F:/bat veolia mnhn/data/glist")


### get number of monitored nights per participation
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

# select ref sessions (>2, >5 and >= 10 sessions) (in Dubos et al, we took participations > 2 nights)
ref2 <- n.night[n.night$n >= 2, ]
ref5 <- n.night[n.night$n >= 5, ]
ref10 <- n.night[n.night$n >= 10, ]
# save(ref2, file = "F:/bat veolia mnhn/data/ref2")



### extract participations with n nights
g2 <- list()
# g5 <- list()
# g10 <- list()
for(i in levels(gg$espece)){
  g2[[i]] <- glist[[i]][glist[[i]]$participation %in% ref2$session, ]
  # g5[[i]] <- glist[[i]][glist[[i]]$participation %in% ref5$session, ]
  # g10[[i]] <- glist[[i]][glist[[i]]$participation %in% ref10$session, ]
}

head(g2[[i]])


# save(g2, file = "F:/bat veolia mnhn/data/g2")
# save(g5, file = "F:/bat veolia mnhn/data/g5")
# save(g10, file = "F:/bat veolia mnhn/data/g10")
