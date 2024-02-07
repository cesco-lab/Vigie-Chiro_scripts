library(data.table)
library(rjson)
library(Hmisc)
library("sp")
library(sf)

Scores=fread("ScoreTab45_local.csv")
#takeout="C:/Users/yvesb/Downloads/takeout-20230303T080947Z-001.zip"
Spexport <- st_read("C:/Users/ybas/Documents/SP2023-03-05.kml")
CoordO=c(43.83,3.77) # LD
#CoordO=c(43.869010, 3.741057) # LD_LA
#CoordO=c(43.762189, 3.864203) #SJDC
Tag="LD"

#unzip(takeout)
#StarredPlaces <- fromJSON(file = "./TakeOut/Maps (vos adresses)/Adresses enregistrées.json")

#head(StarredPlaces[[2]][[5]]$`geometry`$`coordinates`)

# Long=vector()
# Lat=vector()
# for (i in 1:length(StarredPlaces$features))
# {
#   Long=c(Long,StarredPlaces$features[[i]]$properties$Location$`Geo Coordinates`$`Longitude`)
#   Lat=c(Lat,StarredPlaces$features[[i]]$properties$Location$`Geo Coordinates`$`Latitude`)
#  # if(i%%100==1){print(paste(i,Sys.time()))}
# }
# Long=as.numeric(Long)
# Lat=as.numeric(Lat)
# SP=data.frame(Long,Lat)
# Spexport=SP
# fwrite(Spexport,"SP.csv",sep=";")
# Spexport$Label=""
# 
# coordinates(Spexport) <- c("Long", "Lat")
# proj4string(Spexport) <- CRS("+proj=longlat +datum=WGS84")
# writeOGR(Spexport, paste0("SP",Sys.Date(),".kml"),layer="Label", driver="KML") 

SP <- as.data.frame(st_coordinates(Spexport))
SP$Z=NULL

MatchGrid=find.matches(SP,cbind(Scores$longitude,Scores$latitude),tol=c(0.01,0.01)
                       ,maxmatch = 1)

SPfilt=subset(SP,MatchGrid$matches!=0)
MGfilt=subset(MatchGrid$matches,MatchGrid$matches!=0)

SPfilt$Scores=Scores$ScoreRatio[MGfilt]
row.names(SPfilt)=c(1:nrow(SPfilt))
print(SPfilt[which.max(SPfilt$Scores),])
SPfilt$distance=((SPfilt$X-CoordO[2])^2+((SPfilt$Y-CoordO[1])*120/85)^2)^0.5*80
SPfilt=SPfilt[order(SPfilt$distance),]
SPfilt$Coord=paste0(SPfilt$Y,",",SPfilt$X)
SPfilt$mmax=cummax(SPfilt$Scores)
plot(SPfilt$mmax)
SPfilt$sel=(SPfilt$mmax==SPfilt$Scores)
summary(SPfilt$sel)
fwrite(SPfilt,paste0("SP_",Tag,".csv"),sep=";")

