library(data.table)
library(raster)

GI=fread("C:/wamp64/www/GI_sites_localites.csv")
NomenclatureOSO=fread("NomenclatureOSO.csv")
HabitatToExclude=c("eau","autres","surfaces routes")
Biogeo=shapefile("./SIG/BiogeoRegions2016.shp")
Regions=shapefile("./SIG/Limite_administrative/contours-geographiques-des-nouvelles-regions-metropole"
                  ,encoding="UTF-8",use_iconv=TRUE)

Urbain=c(1,2,3)
Agricole=c(5:15)
GrandesCultures=c(5:12)
Foret=c(16,17)

NamesOSO=subset(names(GI),grepl("SpHO",names(GI)))
NamesOSOS=subset(NamesOSO,substr(NamesOSO,nchar(NamesOSO),nchar(NamesOSO))=="S")
GI_OSO=subset(GI,select=NamesOSOS)
test=apply(GI_OSO,MARGIN=1,which.max)
GIcoord=subset(GI,select=c("longitude","latitude"))

Habitat=vector()
CoordH=GIcoord[0,]
for (i in 1:length(NamesOSOS))
{
  Coordtemp=subset(GIcoord,test==i)
  CoordH=rbind(CoordH,Coordtemp)
  
  HabCode=NamesOSOS[i]
  HabNum=gsub("SpHO","",HabCode)
  HabNum=gsub("S","",HabNum)
  HabName=subset(NomenclatureOSO$Habitat,NomenclatureOSO$Code==HabNum)
  Habitat=c(Habitat,rep(HabName,nrow(Coordtemp)))
  print(paste(i,length(Habitat),nrow(CoordH)))
}

CodeUrbain=paste0("SpHO",Urbain,"S")
GI_Urbain=subset(GI_OSO,select=CodeUrbain)
Coordtemp=subset(GIcoord,apply(GI_Urbain,MARGIN=1,sum)>0.5)
CoordH=rbind(CoordH,Coordtemp)
Habitat=c(Habitat,rep("Urbain",nrow(Coordtemp)))

CodeAgricole=paste0("SpHO",Agricole,"S")
GI_Agricole=subset(GI_OSO,select=CodeAgricole)
Coordtemp=subset(GIcoord,apply(GI_Agricole,MARGIN=1,sum)>0.5)
CoordH=rbind(CoordH,Coordtemp)
Habitat=c(Habitat,rep("Agricole",nrow(Coordtemp)))
CodeGrandesCultures=paste0("SpHO",GrandesCultures,"S")
GI_GrandesCultures=subset(GI_OSO,select=CodeGrandesCultures)
Coordtemp=subset(GIcoord,apply(GI_GrandesCultures,MARGIN=1,sum)>0.5)
CoordH=rbind(CoordH,Coordtemp)
Habitat=c(Habitat,rep("Grandes Cultures",nrow(Coordtemp)))
CodeForet=paste0("SpHO",Foret,"S")
GI_Foret=subset(GI_OSO,select=CodeForet)
Coordtemp=subset(GIcoord,apply(GI_Foret,MARGIN=1,sum)>0.5)
CoordH=rbind(CoordH,Coordtemp)
Habitat=c(Habitat,rep("Foret",nrow(Coordtemp)))

Coordtemp=subset(GIcoord,(GI$SpCP2S>0)|(GI$SpCC6S>0))
CoordH=rbind(CoordH,Coordtemp)
Habitat=c(Habitat,rep("Eau",nrow(Coordtemp)))

Coordtemp=subset(GIcoord,(GI$SpCC6S>0))
CoordH=rbind(CoordH,Coordtemp)
Habitat=c(Habitat,rep("Riviere",nrow(Coordtemp)))

CoordH$Habitat=Habitat
CoordH$Type="Habitat"

CoordL=GIcoord[0,]
Lisiere=vector()
Coordtemp=subset(GIcoord,(apply(GI_Foret,MARGIN=1,sum)>0.25)&
                   (apply(GI_Agricole,MARGIN=1,sum)>0.25))
CoordL=rbind(CoordL,Coordtemp)
Lisiere=c(Lisiere,rep("Agricole-Foret",nrow(Coordtemp)))
Coordtemp=subset(GIcoord,(apply(GI_Urbain,MARGIN=1,sum)>0.25)&
                   (apply(GI_Agricole,MARGIN=1,sum)>0.25))
CoordL=rbind(CoordL,Coordtemp)
Lisiere=c(Lisiere,rep("Agricole-Urbain",nrow(Coordtemp)))
Coordtemp=subset(GIcoord,(apply(GI_Foret,MARGIN=1,sum)>0.25)&
                   (apply(GI_Urbain,MARGIN=1,sum)>0.25))
CoordL=rbind(CoordL,Coordtemp)
Lisiere=c(Lisiere,rep("Foret-Urbain",nrow(Coordtemp)))
CoordL$Habitat=Lisiere
CoordL$Type="Lisiere"

GIsp=GIcoord
GIsp$id=c(1:nrow(GIsp))
coordinates(GIsp)=c("longitude","latitude")
proj4string(GIsp) <- CRS("+init=epsg:4326") # WGS 84
GIbg=spTransform(GIsp,proj4string(Biogeo))

BG=intersect(GIbg,Biogeo)

CoordB=GIcoord[0,]
ZBiogeo=vector()
for (k in 1:length(unique(BG$short_name)))
{
  IdSel=subset(BG$id,BG$short_name==unique(BG$short_name)[k])
  Coordtemp=GIcoord[IdSel,]
  CoordB=rbind(CoordB,Coordtemp)
  ZBiogeo=c(ZBiogeo,rep(unique(BG$short_name)[k],nrow(Coordtemp)))
}
CoordB$Habitat=ZBiogeo
CoordB$Type="Zone Biogeographique"

GIre=spTransform(GIsp,proj4string(Regions))

Reg=intersect(GIre,Regions)

CoordR=GIcoord[0,]
RegClass=vector()
for (k in 1:length(unique(Reg$region)))
{
  IdSel=subset(Reg$id,Reg$region==unique(Reg$region)[k])
  Coordtemp=GIcoord[IdSel,]
  CoordR=rbind(CoordR,Coordtemp)
  RegClass=c(RegClass,rep(unique(Reg$region)[k],nrow(Coordtemp)))
}
CoordR$Habitat=RegClass
CoordR$Type="Region administrative"

AltiC=pmax(GI$SpAltiS,0)
AltiC2=floor(AltiC/500)
Name1=c(min(AltiC2):max(AltiC2))*500
Name2=(c(min(AltiC2):max(AltiC2))+1)*500
NameA=paste0(Name1,"-",Name2," m")

CoordA=GIcoord[0,]
ClassAlti=vector()
l=0
for (k in c(min(AltiC2):max(AltiC2)))
{
  l=l+1
  Coordtemp=subset(GIcoord,AltiC2==k)
  CoordA=rbind(CoordA,Coordtemp)
  ClassAlti=c(ClassAlti,rep(NameA[l],nrow(Coordtemp)))
}
CoordA$Habitat=ClassAlti
CoordA$Type="Altitude"


Classes=rbind(CoordH,CoordL,CoordB,CoordR,CoordA)
fwrite(Classes,"Classes_coord.csv",sep=";")
barplot(table(Classes$Habitat)[order(table(Classes$Habitat),decreasing=T)]
        ,las=2)
