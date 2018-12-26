library(data.table)
FCoord=("GI_coordWGS84_SpNuit2_Seuil50_DataLP_PF_exportTot_Lat41.45_51.61_Long-5.9_9.73")
DirPredPres="./Vigiechiro/ModPred/"
Particip=fread("C:/wamp64/www/p_export.csv")
SiteLoc=fread("C:/wamp64/www/sites_localites.txt")


SLP=merge(SiteLoc,Particip,by.x=c("site","nom"),by.y=c("site","point"))
SLP_PF=subset(SLP,SLP$protocole=="POINT_FIXE")
SLP_PF$Month=as.character(as.numeric(substr(SLP_PF$date_debut,4,5)))

FPredPres=list.files(DirPredPres,pattern=".csv$",full.names=T)

PredPres=grepl("Presence",basename(FPredPres))
PredCoord=grepl(FCoord,basename(FPredPres))

FPP_filter=subset(FPredPres,PredPres&PredCoord)

FPP_info=tstrsplit(basename(FPP_filter),"_")
Species=FPP_info[[1]]
Month=FPP_info[[3]]


for (j in 1:nlevels(as.factor(Month)))
{
  print(levels(as.factor(Month))[j])
  Parttemp=subset(SLP_PF,SLP_PF$Month==levels(as.factor(Month))[j])
  
for (i in 1:nlevels(as.factor(Species)))
{
    FPPtemp=subset(FPP_filter,(Species==levels(as.factor(Species))[i])&
                    (Month==levels(as.factor(Month))[j])) 
   PPtemp=fread(FPPtemp)
   if(i==1){PPs=PPtemp[,1:3]}else{PPs=cbind(PPs,PPtemp$pred)}
   
   names(PPs)[ncol(PPs)]=levels(as.factor(Species))[i]
   
}
  Partt2=merge(Parttemp,PPs,by.x=c("longitude","latitude")
               ,by.y=c("Group.1","Group.2"))
  if(exists("PartPP")){PartPP=rbind(PartPP,Partt2)}else{PartPP=Partt2}
}

test=PartPP
coordinates(test)=c("longitude","latitude")
spplot(test,zcol="Plemac",colorkey=T)

fwrite(PartPP,"PartPresence.csv")
rm(PartPP)
