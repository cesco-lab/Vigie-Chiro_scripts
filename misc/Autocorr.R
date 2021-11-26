library(glmmTMB)
library(data.table)
library(sp)
library(spdep)
library(gstat)

GLMPref="GLMnonselect_MultiSp_DecOT2_AT81_Jour_simplifie1_STI_CS"
#Res=fread("./VigieChiro/GLMs/GLMnonselect_MultiSp_DecOT2_AT81_Jour_simplifie1_CS_STI_SpNuit2_Seuil90_bush-cricket_100_Res.csv")
Res=fread("./output/MAJ200515Total/2020-05-14_Nyclei_50/Model/GLMalphatest_tendancesFY2020-05-14_Nyclei_50_Nyclei_2020-05-14_Nyclei_50_Res.csv")
SiteLoc=fread("C:/wamp64/www/sites_localites.txt")
SiteCoord=aggregate(cbind(SiteLoc$longitude,SiteLoc$latitude)
                    ,by=list(SiteLoc$site),FUN=mean)
coordinates(SiteCoord)=c("V1","V2")
proj4string(SiteCoord) <- CRS("+init=epsg:4326") # WGS 84

SiteL93=spTransform(SiteCoord,CRS("+init=epsg:2154"))

load(paste0("./VigieChiro/GLMs/",GLMPref,".glm"))

UniqueCol=unique(names(Res))
Res=subset(Res,select=UniqueCol) #to remove duplicated columns
ResCoord=merge(Res,SiteL93,by.x="site",by.y="Group.1")
coordinates(ResCoord)=c("V1","V2")
proj4string(ResCoord) <- CRS("+init=epsg:2154") # L93

Sys.time()
v = variogram(Res~1, ResCoord,cutoff=500000)
Sys.time()
plot(v)
Sys.time()
v = variogram(Res~1, ResCoord,cutoff=100000)
Sys.time()
plot(v)
Sys.time()
v = variogram(Res~1, ResCoord,cutoff=20000)
Sys.time()
plot(v)
Sys.time()
v = variogram(Res~1, ResCoord,cutoff=4000)
Sys.time()
plot(v)
v = variogram(Res~1, ResCoord,cutoff=1000)
Sys.time()
plot(v)

