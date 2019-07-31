 
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
df  <- pawa[complete.cases(pawa), ]
 
 
 
 library(MuMIn)
 hacked.quasibinomial <- function(...) {
   res <- quasibinomial(...)
   res$aic <- binomial(...)$aic
   res
 }
 
# model list (written manually, because dredge does not handle quasi binomial family)
d <- list()
mav <- list()
for(i in c(4, 6, 11)){
 m <- glm(df[, i] ~ scale(n.night) * scale(Urban) + scale(n.night) * scale(Forest) + scale(n.night) * scale(Scrub_Herbaceous) + scale(n.night) * season, data = df, family = hacked.quasibinomial) 
 m1 <- glm(df[, i] ~ scale(n.night) * scale(Urban) + scale(Forest) + scale(Scrub_Herbaceous) + season, data = df, family = hacked.quasibinomial) 
 m12 <- glm(df[, i] ~ scale(n.night) * scale(Urban) + scale(n.night) * scale(Forest) + scale(Scrub_Herbaceous) + season, data = df, family = hacked.quasibinomial) 
 m13 <- glm(df[, i] ~ scale(n.night) * scale(Urban) + scale(Forest) + scale(n.night) * scale(Scrub_Herbaceous) + season, data = df, family = hacked.quasibinomial) 
 m14 <- glm(df[, i] ~ scale(n.night) * scale(Urban) + scale(Forest) + scale(Scrub_Herbaceous) + scale(n.night) * season, data = df, family = hacked.quasibinomial) 
 m2 <- glm(df[, i] ~ scale(Urban) + scale(n.night) * scale(Forest) + scale(Scrub_Herbaceous) + season, data = df, family = hacked.quasibinomial) 
 m23 <- glm(df[, i] ~ scale(Urban) + scale(n.night) * scale(Forest) + scale(n.night) * scale(Scrub_Herbaceous) + season, data = df, family = hacked.quasibinomial) 
 m24 <- glm(df[, i] ~ scale(Urban) + scale(n.night) * scale(Forest) + scale(Scrub_Herbaceous) + scale(n.night) * season, data = df, family = hacked.quasibinomial) 
 m3 <- glm(df[, i] ~ scale(Urban) + scale(Forest) + scale(n.night) * scale(Scrub_Herbaceous) + season, data = df, family = hacked.quasibinomial) 
 m34 <- glm(df[, i] ~ scale(Urban) + scale(Forest) + scale(n.night) * scale(Scrub_Herbaceous) + scale(n.night) * season, data = df, family = hacked.quasibinomial) 
 m4 <- glm(df[, i] ~ scale(Urban) + scale(Forest) + scale(Scrub_Herbaceous) + scale(n.night) * season, data = df, family = hacked.quasibinomial) 
 m123 <- glm(df[, i] ~ scale(n.night) * scale(Urban) + scale(n.night) * scale(Forest) + scale(n.night) * scale(Scrub_Herbaceous) + season, data = df, family = hacked.quasibinomial) 
 m124 <- glm(df[, i] ~ scale(n.night) * scale(Urban) + scale(n.night) * scale(Forest) + scale(Scrub_Herbaceous) + scale(n.night) * season, data = df, family = hacked.quasibinomial) 
 m134 <- glm(df[, i] ~ scale(n.night) * scale(Urban) + scale(Forest) + scale(n.night) * scale(Scrub_Herbaceous) + scale(n.night) * season, data = df, family = hacked.quasibinomial) 
 m234 <- glm(df[, i] ~ scale(Urban) + scale(n.night) * scale(Forest) + scale(n.night) * scale(Scrub_Herbaceous) + scale(n.night) * season, data = df, family = hacked.quasibinomial) 
 m0 <- glm(df[, i] ~ scale(n.night) + scale(Urban) + scale(Forest) + scale(Scrub_Herbaceous) + season, data = df, family = hacked.quasibinomial) 
 
 d[[i]] <- model.sel(m, m1, m2, m3, m4, m12, m13, m14, m23, m24, m34, m123, m124, m134, m234, m0, rank = QAIC, rank.args = list(chat = deviance(m) / df.residual(m)))
 mav[[i]] <- model.avg(d, subset = delta <= 2)
 # mav <- model.avg(d, cumsum(weight) <= .95)
 print(summary(mav[[i]]))
 }

 
