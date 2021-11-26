library(data.table)
library(Hmisc)
library(sp)
library(rgeos)

CoordO=c(43.826332, 3.769719) #ld
DistMax=850 #1 semaine
#DistMax=2150 #2 semaines
Tag="1SEM"
#DirPT="./VigieChiro/gbifData/ST"
DirPT="E:/ScoreTab330"

PTf=list.files(DirPT,pattern=paste0("ScoreTab"),full.names=T)

ScoreRel=vector()
ScoreAbs=vector()
ScoreRelMax=vector()
ScoreRelDMax=vector()
Day=vector()
for (i in 1:length(PTf))
{
  PTdata=fread(PTf[i]) 
  PTsp=PTdata
  coordinates(PTsp)=c("longitude","latitude")
  proj4string(PTsp)=CRS("+init=epsg:4326") # WGS 84
  PTsp=spTransform(PTsp,CRS("+init=epsg:2154"))
  CoordOsp=data.frame(longitude=CoordO[2],latitude=CoordO[1])
  coordinates(CoordOsp)=c("longitude","latitude")
  proj4string(CoordOsp)=CRS("+init=epsg:4326") # WGS 84
  CoordOsp=spTransform(CoordOsp,CRS("+init=epsg:2154")) #lambert 93
  test=gDistance(CoordOsp,PTsp,byid=T)
  PTdata=subset(PTdata,test[,1]<DistMax*1000)
  SMax=max(PTdata$ScoreRatio)
  DFcoordO=cbind(longitude=CoordO[2],latitude=CoordO[1])
  MatchCoord=find.matches(DFcoordO,subset(PTdata,select=c("longitude","latitude")),tol=c(0.1,0.1)
                          ,maxmatch=1)
  SO=PTdata[MatchCoord[[1]][1],]$ScoreRatio
  ScoreRelMax=c(ScoreRelMax,SO/SMax)
  
  Dayi=gsub("ScoreTab","",basename(PTf[i]))
  Dayi=gsub(".csv","",Dayi)
  Day=c(Day,Dayi)
  if(i%%10==1){print(paste(Sys.time(),i))}
  }

plot(as.numeric(Day),ScoreRelMax)

#plot(as.numeric(Day),ScoreRel,xlim=c(240,250))
#plot(as.numeric(Day),ScoreRel>median(ScoreRel))
DayM=subset(as.numeric(Day),ScoreRelMax>median(ScoreRelMax))
#print(DayM[order(DayM)])

Sel=head(Day[order(ScoreRelMax)],20)
Sel
Sel=Sel[order(Sel)]
SelInt=vector()
for (i in 2:length(Sel))
{
  Int=as.numeric(Sel[i])-as.numeric(Sel[i-1])
  if(Int>1&Int<15)
  {
    SelInt=c(SelInt,c((as.numeric(Sel[i-1])+1):(as.numeric(Sel[i])-1)))
  }
}
Sel=c(Sel,SelInt)
Sel=Sel[order(as.numeric(Sel))]
plot(Sel)
Sel

SelPer=as.numeric(Sel[1])
Per=vector()
PerDeb=vector()
PerFin=vector()
  for (i in 2:length(Sel))
{
  Int=as.numeric(Sel[i])-as.numeric(Sel[i-1])
  if(Int>1)
  {
    Per=c(Per,mean(SelPer))
    PerDeb=c(PerDeb,min(SelPer))
    PerFin=c(PerFin,max(SelPer))
    
    SelPer=as.numeric(Sel[i])
      }else{
    SelPer=c(SelPer,as.numeric(Sel[i]))

    }
}
Per=c(Per,mean(SelPer))
PerDeb=c(PerDeb,min(SelPer))
PerFin=c(PerFin,max(SelPer))

for (j in 1:length(Per))
{
  STseltot=data.frame()
  Dayj=round(Per[j]) 
 ST=fread(paste0(DirPT,"/ScoreTab",Dayj,".csv"))
 STsp=ST
 coordinates(STsp)=c("longitude","latitude")
  proj4string(STsp)=CRS("+init=epsg:4326") # WGS 84
  STsp=spTransform(STsp,CRS("+init=epsg:2154"))
STsel=subset(STsp,STsp$ScoreRatio==max(STsp$ScoreRatio))
STseltot=rbind(STseltot,as.data.frame(STsel))
CoordOsp=data.frame(longitude=CoordO[2],latitude=CoordO[1])
coordinates(CoordOsp)=c("longitude","latitude")
proj4string(CoordOsp)=CRS("+init=epsg:4326") # WGS 84
CoordOsp=spTransform(CoordOsp,CRS("+init=epsg:2154")) #lambert 93
test=gDistance(CoordOsp,STsp,byid=T)
DistMax=max(test)
DistSel=subset(test[,1],STsp$ScoreRatio==max(STsp$ScoreRatio))

while(DistSel>20000){
  STsp=subset(STsp,test[,1]<DistSel)
  STsel=subset(STsp,STsp$ScoreRatio==max(STsp$ScoreRatio))
  STseltot=rbind(STseltot,as.data.frame(STsel))
  test=gDistance(CoordOsp,STsp,byid=T)
  DistSel=subset(test[,1],STsp$ScoreRatio==max(STsp$ScoreRatio))
print(DistSel)  
}

fwrite(STseltot,paste0("STseltot_",Tag,"_",Dayj,".csv"),sep=";")

}
print(Sel)
