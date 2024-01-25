library(data.table)
library(raster)
library(rgdal)
library(FNN)

PartSelG=fread("C:/Users/yvesb/Documents/Tadarida/Vigie-Chiro_scripts/Vigie-Chiro_scripts/PartSelG.csv")
Zone="C:/Users/yvesb/Documents/SIG/Limite_administrative/France_dep_L93.shp"
Sample=10000
SelDep=T
Dep=c("11","30","34","48","66") #LR
ExclDep=F
DepE=c("76","14","50","61","27","29","56","22","35","59","62","08","10","51"
       ,"52","54","55","57","88")
#Normandie+Bretagne+NPDC+CA+Lorraine
SelMois=c(4,5,6)



FranceD= shapefile(Zone)
if(SelDep)
{
  FranceD=subset(FranceD,FranceD$DépARTEM0 %in% Dep)
}

if(ExclDep)
{
  FranceD=subset(FranceD,!FranceD$D?pARTEM0 %in% DepE)
}

SysGrid=spsample(FranceD,Sample,type="regular")
CRSW84 <- CRS("+init=epsg:4326") # WGS 84
SysG84=spTransform(SysGrid,CRSW84)

CoordSG=as.data.frame(SysG84)
names(CoordSG)=c("Group.1","Group.2")
CoordSG$x=1
CoordSG$id=c(1:nrow(CoordSG))


PointEch=subset(PartSelG,PartSelG$Mois %in% SelMois)
coordinates(PointEch)=c("longitude","latitude")
spplot(PointEch,zcol="Mois")

g = get.knnx( coordinates(PointEch),coordinates(SysG84),k=1)
SysG84$dist=g[[2]]
spplot(SysG84,zcol="dist")

WMG=which.max(g[[2]])
max(g[[2]])
CoordSG84=as.data.frame(coordinates(SysG84))

paste(CoordSG84$x2[which.max(g[[2]])],CoordSG84$x1[which.max(g[[2]])],sep=",")
