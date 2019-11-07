library(data.table)
library(rgbif)
library(readxl)
FDataSel="DataSel__310.csv"
SpeciesAll=fread("SpeciesAll.csv",sep=";")
GroupList=read_excel("GroupeSp.xlsx")

match("Tettigetta argentata",SpeciesAll$Species)
DataSel=fread(FDataSel)

SpeciesGBIF=vector()
for (i in 1:nrow(SpeciesAll))
{
test=name_backbone(name=SpeciesAll$Species[i])
SpeciesGBIF=c(SpeciesGBIF,test$species)
if(i%%100==1){print(paste(i,Sys.time()))}
}

SpOld=subset(DataSel,DataSel$ListSpValide %in% SpeciesGBIF)
SpNew=subset(DataSel,!(DataSel$ListSpValide %in% SpeciesGBIF))

#Presence
GroupNew=subset(GroupList,GroupList$Group %in% SpNew$Group)

DataNew=list()
h=0
for (i in 1:nrow(GroupNew))
{
  GroupTemp=GroupNew[i,]
  SpTemp=subset(SpNew,SpNew$Group==GroupTemp$Group)
  if(file.exists(paste0("./VigieChiro/gbifData/DataGroup/DataGroup2_"
                        ,GroupTemp$Group,"_FR.csv")))
  {
  DataGroup=fread(paste0("./VigieChiro/gbifData/DataGroup/DataGroup2_"
                         ,GroupTemp$Group,"_FR.csv"))
  DataGroup$Group=GroupTemp$Group
  NumSel=GroupTemp$Importance*20
  print(paste(Sys.time(),GroupTemp$Group,NumSel))
  for (j in 1:nrow(SpTemp))
  {
    DataSpTemp=subset(DataGroup,DataGroup$name==SpTemp$ListSpValide[j])
    DataSpTemp$coordinateUncertaintyInMeters[is.na(DataSpTemp$coordinateUncertaintyInMeters)]=5000
    Weight=pmin(1,50/DataSpTemp$coordinateUncertaintyInMeters)
    DataSampl1=DataSpTemp[sample.int(nrow(DataSpTemp)
                                       ,size=min(NumSel,nrow(DataSpTemp))
                                       ,replace=F
                                       ,prob=DataSpTemp$Weight),]
    h=h+1
    DataNew[[h]]=DataSampl1
  }
  }
}
DataNewTot=rbindlist(DataNew,fill=T,use.names=T)
plot(DataNewTot$decimalLongitude,DataNewTot$decimalLatitude,xlim=c(-12,15)
     ,ylim=c(40,52))
fwrite(DataNewTot,paste0("./VigieChiro/GIS/PA/DataNew"
                         ,gsub("DataSel","",FDataSel)))



#Absence
GroupOld=subset(GroupList,GroupList$Group %in% SpOld$Group)


DataOld=list()
h=0
for (i in 1:nrow(GroupOld))
{
  GroupTemp=GroupOld[i,]
  SpTemp=subset(SpOld,SpOld$Group==GroupTemp$Group)
  if(file.exists(paste0("./VigieChiro/gbifData/DataGroup/DataGroup2_"
                        ,GroupTemp$Group,"_FR.csv")))
  {
    DataGroup=fread(paste0("./VigieChiro/gbifData/DataGroup/DataGroup2_"
                           ,GroupTemp$Group,"_FR.csv"))
    DataGroup$Group=GroupTemp$Group
    NumSel=GroupTemp$Importance*20
    print(paste(Sys.time(),nrow(SpTemp),GroupTemp$Group,NumSel))
    for (j in 1:nrow(SpTemp))
    {
      DataSpTemp=subset(DataGroup,DataGroup$name==SpTemp$ListSpValide[j])
      DataSpTemp$coordinateUncertaintyInMeters[is.na(DataSpTemp$coordinateUncertaintyInMeters)]=5000
      Weight=pmin(1,50/DataSpTemp$coordinateUncertaintyInMeters)
      DataSampl1=DataSpTemp[sample.int(nrow(DataSpTemp)
                                       ,size=min(NumSel,nrow(DataSpTemp))
                                       ,replace=F
                                       ,prob=DataSpTemp$Weight),]
      h=h+1
      DataOld[[h]]=DataSampl1
    }
  }
}
DataOldTot=rbindlist(DataOld,fill=T,use.names=T)

DataOldTot=DataOldTot[sample.int(nrow(DataOldTot),min(1000,nrow(DataOldTot))),]
DataNewTot=DataNewTot[sample.int(nrow(DataNewTot),min(1000,nrow(DataNewTot))),]

points(DataOldTot$decimalLongitude,DataOldTot$decimalLatitude,col=2,pch=2)
DataOldTot$presence=0 
DataNewTot$presence=1

DataTot=rbind(DataOldTot,DataNewTot,use.names=T,fill=T)
fwrite(DataTot,paste0("./VigieChiro/GIS/PA/PATot",gsub("DataSel","",FDataSel)))

table(DataNewTot$name)[order(table(DataNewTot$name),decreasing=T)][1:20]
table(DataOldTot$name)[order(table(DataoldTot$name),decreasing=T)][1:20]
t=table(DataTot$presence,DataTot$Group)
tsum=apply(t,MARGIN=2,sum)
otsum=order(tsum,decreasing=T)
tshort=t[,otsum[1:20]]
barplot(tshort
        ,las=2,cex.names=0.5)
