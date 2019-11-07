library(reshape)
library(mgcv)
library(data.table)
library(glmmTMB)
# Method from 
# Moussus, J., Jiguet, F., Clavel, J., & Julliard, R. (2009). A method to estimate phenological variation using data from large-scale abundance monitoring programmes. Bird Study, 56(2), 198-212. doi:10.1080/00063650902792064
# Basicly, you merge 2 years of data (one with the highest number of data, and one want to test). For the tested year, you shift the date by a range of possible shifts (eg, -20 to +20). You can assess which shift allows to best fit your reference year on the basis of AIC.
# I enhanced it in 2 aspects:
# 1. I allowed to find a reference year specific to each species (because the reference year must be the year with the highest amount of data, and this year can change between species)
# 2. I coded it for productivity (the proportion of juveniles relative to adults) instead of using abundances (because I believe that the proportion of juveniles better represents the starting of the breeding period (e.g., for resident bird species)
FirstYear=2014
LastYear=2017
SpeciesList=fread("SpeciesList.csv")
FAct="SpNuit2_Seuil50_DataLP_PF_exportTot.csv"  
FGIS="./VigieChiro/GIS/GI_coordWGS84_SpNuit2_Seuil50_DataLP_PF_exportTot_Lat41.45_51.61_Long-5.9_9.73.csv"
Particip=fread("C:/wamp64/www/p_export.csv")
SiteLoc=fread("C:/wamp64/www/sites_localites.txt")
DateSel=c(1:183)
SelBat=T

Bat=subset(SpeciesList$Esp,SpeciesList$Group=="bat")
BS=subset(SpeciesList$Esp,SpeciesList$Group=="bush-cricket")



SpNuit=fread(FAct)
GIS=fread(FGIS)


Gite=mapply(function(x,y) 
  ((grepl(paste0(y,"="),x))|(grepl(paste0(y," ="),x)))
  ,SiteLoc$commentaire
  ,SiteLoc$localite)
SiteLoc$SpGite=as.numeric(Gite)

SiteLoc=subset(SiteLoc,SiteLoc$SpGite==0)

ListPart=levels(as.factor(SpNuit$participation))

PartPF=subset(Particip,Particip$participation %in% ListPart)
SLP=merge(PartPF,SiteLoc,by.x=c("site","point"),by.y=c("site","nom"))
SpNuit_SLP=merge(SpNuit,SLP,by="participation")
SpNuit_SLPAG=merge(SpNuit_SLP,GIS,by.x=c("longitude","latitude")
                   ,by.y=c("Group.1.x","Group.2.x"))

SpNuit_SLPAG=subset(SpNuit_SLPAG,!is.na(SpNuit_SLPAG$SpAltiS))



#dirty code for adding zeros
DataNuit=unique(as.data.table(SpNuit_SLPAG),by=c("participation","Nuit"))
DataNuit$FalseGroup=1
DataNuit$FalseGroup=as.factor(DataNuit$FalseGroup)
ListSp=levels(as.factor(SpNuit_SLPAG$espece))
ListSpDF=data.frame(cbind(ListSp,FalseGroup=1))

DataNuitSp=merge(DataNuit,ListSpDF,by="FalseGroup",allow.cartesian=TRUE)
NuitEch=paste(DataNuitSp$participation,DataNuitSp$Nuit,DataNuitSp$ListSp)
SpEch=paste(SpNuit_SLPAG$participation,SpNuit_SLPAG$Nuit,SpNuit_SLPAG$espece)


Data0=subset(DataNuitSp,!(NuitEch %in% SpEch))
Data0$espece=Data0$ListSp
Data0$nb_contacts=0
Data0$FalseGroup=NULL
Data0$ListSp=NULL

db=rbind(SpNuit_SLPAG,Data0,use.names=T)

db$year=substr(db$Nuit,1,4)
db$day=yday(as.Date(db$Nuit))

db=subset(db,db$day %in% DateSel)

db=subset(db,db$year %in% c(FirstYear:LastYear))

if(SelBat){
  db=subset(db,db$espece %in% Bat)
}

#preparing result storage

db$espece <- as.factor(as.character(db$espece))
aics <- array(dim = c((LastYear-FirstYear)+1, 101, nlevels(db$espece)), # this codee works for several species (sp)
              dimnames = list(levels(factor(db$year)),
                              as.factor(as.character(c(-50:50))), # here I will test which shift, between -20 and +20 days will best fit the data
                              levels(db$espece)))


# finding the reference year (i.e., the year with maximum count, for each species individually)
# isolating the reference year with the tested year
# running a model for each tested year and storing the AIC 
# Here the data covers the 2000-2015 period, and I test if for a +- 20 days of potential phenological shift

for(sp in levels(factor(db$espece))){
  sb <- db[db$espece == sp, ]
  print(sp)
  for(i in c(FirstYear:LastYear)){
    blabla <- as.data.frame(table(sb$year))
    ref <- as.character(blabla$Var1[blabla$Freq == max(blabla$Freq)]) # this is to find the reference year (can change between species, so I included it in the loop)
    sub <- sb[sb$year %in% c(ref, i), ]
    print(i)
    for(t in c(-50: 50)){
      sub$tt <- ifelse(sub$year == ref, sub$day, sub$day+t)
      Sys.time()
      #g <- gam(nb_contacts ~ s(tt), data = sub, family = poisson) # here is the model (here I tested it for the proportion of juveniles (which is why I used binomial family))
      #g <- glmmTMB(nb_contacts ~ tt+I(tt^2)++(1|site), data = sub, family = nbinom2) # here is the model (here I tested it for the proportion of juveniles (which is why I used binomial family))
      sub$tt=scale(sub$tt)
      sub$SpBioc10=scale(sub$SpBioc10)
      sub$SpBioc11=scale(sub$SpBioc11)
      sub$SpBioc12=scale(sub$SpBioc12)
      sub$SpHO1S=scale(sub$SpHO1S)
      sub$SpHO2S=scale(sub$SpHO2S)
      sub$SpHO4S=scale(sub$SpHO4S)
      sub$SpWS_S=scale(sub$SpWS_S)
      sub$SpWC_S=scale(sub$SpWC_S)
      
      g <- glmmTMB(nb_contacts ~ tt+I(tt^2)+SpBioc10+SpBioc11+SpBioc12+SpHO1S+SpHO2S+SpHO4S+SpWS_S+SpWC_S , data = sub, family = nbinom2) # here is the model (here I tested it for the proportion of juveniles (which is why I used binomial family))
      
      Sys.time()
      aics[as.character(i), as.character(t), sp] <- AIC(g)
      print(paste(t,AIC(g)))
    }
  }
}

NameArray=paste0("./VigieChiro/GLMs/DecPheno/aics_",FirstYear,"_",LastYear,"_",min(DateSel),"_",max(DateSel),".array")
  
  
  
save(aics, file = NameArray)

#checking aic curves

#load(NameArray)
par(mfrow = c(2,2))
for(sp in dimnames(aics)[[3]]){ 
  for(i in FirstYear:LastYear){
    if(!is.na(aics[as.character(i), ,sp]))
    {
      plot(aics[as.character(i), ,sp], main = paste(sp, i))
    }
  }
}


# preparing the 'best fitting shift' storage (for all years and all species)

t <- array(dim = c(nlevels(factor(db$year)), nlevels(factor(db$espece))),
           dimnames = list(levels(factor(db$year)), levels(db$espece)))


# finding the best fitting shift (for each years and each species)
# this will save the shift 't' for which the AIC of the previous model is the best



for(i in dimnames(aics)[[3]]){
  for(j in rownames(aics)){
    inds = which(data.frame(aics[j, , i]) == min(data.frame(aics[j, , i])), arr.ind=TRUE)
    t[j, i] <- if(nrow(inds) == 1) rownames(data.frame(aics[j, , i]))[inds[,1]] else 0
  }
}


# making the result as a good old dataframe


t <- melt(t)
colnames(t) <- c("year", "espece", "t")
t$t[t$year == "2017"] <- 0 # here 2007 was a reference year. I set it to zero manually ('cause it's simple you know...)

NameT=paste0("./VigieChiro/GLMs/DecPheno/t_",FirstYear,"_",LastYear,"_",min(DateSel),"_",max(DateSel),".array")

save(t, file = NameT)

par(mfrow = c(1,1))

# check how late or advanced is the species - compared to the reference year - throughout your study period
for(i in FirstYear:LastYear){
  sub <- t[t$year == i, ]
  if(nrow(sub)>0)
  {
    hist(as.numeric(as.character(sub$t)), main = i, xlim = c(-50,50),breaks=50)
  }
}

for(i in FirstYear:LastYear){
  sub <- t[t$year == i, ]
  sub <- sub[sub$espece %in% Bat, ]
  if(nrow(sub)>0)
  {
    hist(as.numeric(as.character(sub$t)), main = i, xlim = c(-50,50),breaks=50)
  }
}

for(i in FirstYear:LastYear){
  sub <- t[t$year == i, ]
  sub <- sub[sub$espece %in% BS, ]
  if(nrow(sub)>0)
  {
    hist(as.numeric(as.character(sub$t)), main = i, xlim = c(-50,50),breaks=50)
  }
}



for(i in levels(factor(db$espece))){
  sub <- t[t$espece == i, ]
  if(nrow(sub)>0)
  {
    hist(as.numeric(as.character(sub$t)), main = i, xlim = c(-20,20))
  }
}
