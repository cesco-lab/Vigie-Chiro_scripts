library(glmmTMB)
library(data.table)
library(sp)
library(spdep)
library(gstat)

GLMPref="GLMnonselect_MultiSp_DecOT2_AT81_Jour_simplifie1_STI_CS"
Res=fread("./VigieChiro/GLMs/GLMnonselect_MultiSp_DecOT2_AT81_Jour_simplifie1_CS_STI_SpNuit2_Seuil90_bush-cricket_100_Res.csv")
SiteLoc=fread("C:/wamp64/www/sites_localites.txt")
SiteCoord=aggregate(cbind(SiteLoc$longitude,SiteLoc$latitude)
                    ,by=list(SiteLoc$site),FUN=mean)
coordinates(SiteCoord)=c("V1","V2")
proj4string(SiteCoord) <- CRS("+init=epsg:4326") # WGS 84

SiteL93=spTransform(SiteCoord,CRS("+init=epsg:2154"))

load(paste0("./VigieChiro/GLMs/",GLMPref,".glm"))

ResCoord=merge(Res,SiteL93,by.x="site",by.y="Group.1")
coordinates(ResCoord)=c("V1","V2")
proj4string(ResCoord) <- CRS("+init=epsg:2154") # L93

Sys.time()
v = variogram(Res~1, ResCoord,cutoff=10000)
Sys.time()
plot(v)