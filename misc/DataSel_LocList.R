library(data.table)
library(rgbif)
library(readxl)
library(raster)
library(sp)
FDataSel="DataSel__261.csv"
SpeciesAll=fread("SpeciesAll.csv",sep=";")
GroupList=read_excel("GroupeSp.xlsx")
FZone="./VigieChiro/GIS/France_dep_L93Radius_ 10000.shp"
SortAlready=T


#Zone=shapefile("./VigieChiro/GIS/France_dep_L93.shp")
Zone=shapefile(FZone)
ZoneWGS84=spTransform(Zone,"+init=epsg:4326")
ZoneWGS84$id=c(1:nrow(ZoneWGS84))

match("Tettigetta argentata",SpeciesAll$Species)
DataSel=fread(FDataSel)

#SpOld=subset(DataSel,DataSel$ListSpValide %in% SpeciesAll$SpeciesGBIF)
if(SortAlready)
{
SpNew=subset(DataSel,!(DataSel$ListSpValide %in% SpeciesAll$SpeciesGBIF))
}else{
  SpNew=DataSel
}
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
    
    DataGroup=subset(DataGroup,(DataGroup$name %in% SpNew$ListSpValide))
    DataGroup=subset(DataGroup,!is.na(DataGroup$decimalLatitude))
    DataGroup=subset(DataGroup,DataGroup$decimalLatitude!=0)
    if(nrow(DataGroup)>0)
    {
      table(DataGroup$name)
      DataGroup$Group=GroupTemp$Group
      #NumSel=GroupTemp$Importance*20
      
      
      coordinates(DataGroup)=c("decimalLongitude","decimalLatitude")
      crs(DataGroup)="+init=epsg:4326" # WGS 84
      DataTest=over(DataGroup,ZoneWGS84)
      DataLoc=subset(DataGroup,!is.na(DataTest$id))
      print(GroupTemp$Group)
      if(nrow(DataLoc)>0)
      {
        print(table(DataLoc$name))
        h=h+1
        DataNew[[h]]=as.data.frame(DataLoc)
      }
    }
  }
  
  
}
DataNewTot=rbindlist(DataNew,fill=T,use.names=T)
fwrite(DataNewTot,paste0("./VigieChiro/GIS/PA/DataNew_"
,gsub(".shp","",basename(FZone)),".csv"))


SpNbData=aggregate(DataNewTot$eventDate,by=c(list(DataNewTot$Group)
                                             ,list(DataNewTot$name)),length)

SpNbData=merge(SpNbData,GroupList,by.x="Group.1",by.y="Group")
SpNbData$Score=SpNbData$x*SpNbData$Importance
SpNbData=SpNbData[order(SpNbData$Score,decreasing=T),]
head(SpNbData,9)
head(SpNbData,12)

