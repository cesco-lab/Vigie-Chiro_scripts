library(data.table)

DataAct=fread("C:/wamp64/www/SpNuit2_0_DataLP_PF_exportTot.csv")
Threshold=0.5
#GI=fread("C:/wamp64/www/GI_sites_localites.csv")
#SiteLoc=fread("C:/wamp64/www/sites_localites.txt")
#Particip=fread("C:/wamp64/www/p_export.csv")
HabitatTraits=fread("./VigieChiro/Traits/HabitatTraitsSpecies.csv")
SpeciesList=fread("SpeciesList.csv")

#PSL=merge(Particip,SiteLoc,by.x=c("site","point"),by.y=c("site","nom"))
#PSLGI=merge(PSL,GI,by=c("longitude","latitude"))

BatList=subset(SpeciesList,SpeciesList$Group=="bat")
DataThres=subset(DataAct,DataAct$score_max>Threshold)
DataThres=subset(DataThres,DataThres$espece %in% BatList$Esp)
DataTraits=merge(DataThres,HabitatTraits,by.x="espece",by.y="Species")
DataTraits$ActStand=DataTraits$nb_contacts/DataTraits$ActMoy
summary(DataTraits$ActStand)

NuitActTot=aggregate(DataTraits$ActStand,by=c(list(DataTraits$participation)
                                              ,list(DataTraits$Nuit)),sum)
NuitsumSSI=aggregate(DataTraits$ActStand*DataTraits$SSI
                     ,by=c(list(DataTraits$participation)
                                              ,list(DataTraits$Nuit)),sum)
NuitCSI=NuitsumSSI$x/NuitActTot$x
summary(NuitCSI)
hist(log(NuitCSI-0.5))

DataTraits$wAlan=pmax(0,-DataTraits$RepAlan)
summary(DataTraits$wAlan)
DataTraits$wWood=pmax(0,DataTraits$RepWood)
DataTraits$wWater=pmax(0,DataTraits$RepWater)
NuitAlan=aggregate(DataTraits$ActStand*DataTraits$wAlan
                   ,by=c(list(DataTraits$participation)
                         ,list(DataTraits$Nuit)),sum)
hist(log(NuitAlan$x+0.00001))

NuitWood=aggregate(DataTraits$ActStand*DataTraits$wWood
                   ,by=c(list(DataTraits$participation)
                         ,list(DataTraits$Nuit)),sum)
hist(log(NuitWood$x+0.001))

NuitWater=aggregate(DataTraits$ActStand*DataTraits$wWater
                   ,by=c(list(DataTraits$participation)
                         ,list(DataTraits$Nuit)),sum)
hist(log(NuitWater$x+0.001))

MeanCSI=mean(log(NuitCSI-0.5),na.rm=T)
MeanAlan=mean(log(NuitAlan$x+0.00001))
MeanWood=mean(log(NuitWood$x+0.001))
MeanWater=mean(log(NuitWater$x+0.001))

sdCSI=sd(log(NuitCSI-0.5),na.rm=T)
sdAlan=sd(log(NuitAlan$x+0.00001))
sdWood=sd(log(NuitWood$x+0.001))
sdWater=sd(log(NuitWater$x+0.001))

Indicator=c("CSI","Alan","Wood","Water")
Mean=c(MeanCSI,MeanAlan,MeanWood,MeanWater)
sd=c(sdCSI,sdAlan,sdWood,sdWater)
RefIndicators=data.frame(Indicator,Mean,sd)
fwrite(RefIndicators,paste0("RefIndicators",Sys.Date(),".csv"),sep=";")
