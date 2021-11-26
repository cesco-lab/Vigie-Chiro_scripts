library(data.table)
library(Hmisc)
library(sp)
library(rgeos)

CoordO=c(43.826332, 3.769719) #ld
#DistMax=850 #1 semaine
#DistMax=2150 #2 semaines
#Tag="1SEM"
#DirPT="./VigieChiro/gbifData/ST"
DirPT="E:/ScoreTab330"


PTf=list.files(DirPT,pattern=paste0("ScoreTab"),full.names=T)

for (j in 1:length(PTf))
{
  STseltot=data.frame()
  Dayj=gsub("ScoreTab","",basename(PTf[j]))
  Dayj=gsub(".csv","",Dayj)
  Daynum=as.numeric(Dayj)
  if(Daynum%%5==0)
  {
    print(paste(Daynum,Sys.time()))
    
     ST=fread(PTf[j])
 STsp=ST
 coordinates(STsp)=c("longitude","latitude")
  proj4string(STsp)=CRS("+init=epsg:4326") # WGS 84
  STsp=spTransform(STsp,CRS("+init=epsg:2154"))
STsel=subset(STsp,STsp$ScoreRatio==max(STsp$ScoreRatio))
CoordOsp=data.frame(longitude=CoordO[2],latitude=CoordO[1])
coordinates(CoordOsp)=c("longitude","latitude")
proj4string(CoordOsp)=CRS("+init=epsg:4326") # WGS 84
CoordOsp=spTransform(CoordOsp,CRS("+init=epsg:2154")) #lambert 93
test=gDistance(CoordOsp,STsp,byid=T)
DistMax=max(test)
DistSel=subset(test[,1],STsp$ScoreRatio==max(STsp$ScoreRatio))
STsel$Dist=DistSel
STseltot=rbind(STseltot,as.data.frame(STsel))

while(DistSel>20000){
  STsp=subset(STsp,test[,1]<DistSel)
  STsel=subset(STsp,STsp$ScoreRatio==max(STsp$ScoreRatio))
    test=gDistance(CoordOsp,STsp,byid=T)
  DistSel=subset(test[,1],STsp$ScoreRatio==max(STsp$ScoreRatio))
  STsel$Dist=DistSel
  STseltot=rbind(STseltot,as.data.frame(STsel))
  
  #print(DistSel)  
}

fwrite(STseltot,paste0("./VigieChiro/gbifData/ST/STsel/STseltot_",Dayj,".csv"),sep=";")
}
}
