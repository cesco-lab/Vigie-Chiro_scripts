# Method from 
# Moussus, J., Jiguet, F., Clavel, J., & Julliard, R. (2009). A method to estimate phenological variation using data from large-scale abundance monitoring programmes. Bird Study, 56(2), 198-212. doi:10.1080/00063650902792064
# Basicly, you merge 2 years of data (one with the highest number of data, and one want to test). For the tested year, you shift the date by a range of possible shifts (eg, -20 to +20). You can assess which shift allows to best fit your reference year on the basis of AIC.
# I enhanced it in 2 aspects:
# 1. I allowed to find a reference year specific to each species (because the reference year must be the year with the highest amount of data, and this year can change between species)
# 2. I coded it for productivity (the proportion of juveniles relative to adults) instead of using abundances (because I believe that the proportion of juveniles better represents the starting of the breeding period (e.g., for resident bird species)

load(abunance or productivity data) # I call it 'db' here.
library(mgcv)


#preparing result storage

db$sp <- as.factor(as.character(db$sp))
aics <- array(dim = c(nlevels(factor(db$year)), 41, nlevels(db$sp)), # this codee works for several species (sp)
              dimnames = list(levels(factor(db$year)),
                              as.factor(as.character(c(-20:20))), # here I will test which shift, between -20 and +20 days will best fit the data
                              levels(db$sp)))


# finding the reference year (i.e., the year with maximum count, for each species individually)
# isolating the reference year with the tested year
# running a model for each tested year and storing the AIC 
# Here the data covers the 2000-2015 period, and I test if for a +- 20 days of potential phenological shift

for(sp in levels(factor(db$sp))){
  sb <- db[db$sp == sp, ]
  print(sp)
  for(i in c(2000:2015)){
    blabla <- as.data.frame(table(sb$year))
    ref <- as.character(blabla$Var1[blabla$Freq == max(blabla$Freq)]) # this is to find the reference year (can change between species, so I included it in the loop)
    sub <- sb[sb$year %in% c(ref, i), ]
    print(i)
    for(t in c(-20: 20)){
      sub$tt <- ifelse(sub$year == ref, sub$day, sub$day+t)
      g <- gam(n.ju/(n.ju + n.ad) ~ s(tt) + site, data = sub, familiy = binomial) # here is the model (here I tested it for the proportion of juveniles (which is why I used binomial family))
      aics[as.character(i), as.character(t), sp] <- AIC(g)
      # print(AIC(g))
    }
  }
}

save(aics, file = "your directory/aics")


#checking aic curves

load("your directory/aics")par(mfrow = c(3,3))
for(sp in dimnames(aics)[[3]][-17]){ #sometimes it fails (species with poor data, here the 17th species of my list)
  for(i in dimnames(aics)[[1]]){
    plot(aics[as.character(i), ,sp], main = paste(sp, i))
  }
}


# preparing the 'best fitting shift' storage (for all years and all species)

t <- array(dim = c(nlevels(factor(db$year)), nlevels(factor(db$sp))),
           dimnames = list(levels(factor(db$year)), levels(db$sp)))


# finding the best fitting shift (for each years and each species)
# this will save the shift 't' for which the AIC of the previous model is the best

for(i in dimnames(aics)[[3]][-17]){
  for(j in rownames(aics)){
    inds = which(data.frame(aics[j, , i]) == min(data.frame(aics[j, , i])), arr.ind=TRUE)
    t[j, i] <- if(nrow(inds) == 1) rownames(data.frame(aics[j, , i]))[inds[,1]] else 0
  }
}


# making the result as a good old dataframe

library(reshape)
t <- melt(t)
colnames(t) <- c("year", "sp", "t")
t$t[t$year == "2007"] <- 0 # here 2007 was a reference year. I set it to zero manually ('cause it's simple you know...)
save(t, file = "your directory/t")


# check how late or advanced is the species - compared to the reference year - throughout your study period

for(i in levels(factor(db$sp))){
  sub <- t[t$sp == i, ]
  hist(as.numeric(as.character(sub$t)), main = i, xlim = c(-20,20))
}