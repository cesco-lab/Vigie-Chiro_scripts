library(data.table)
library(raster)
library(rgdal)

SpeciesAll=fread("SpeciesAll.csv",sep=";")
EsPro=fread("C:/Users/yvesb/Documents/natura/especes_protegees.csv",sep=";")
PredTabNum=20
#SelPol=shapefile("C:/Users/yvesb/Documents/SIG/lien&oxylane.shp")
#SelPol=shapefile("C:/Users/yvesb/Documents/SIG/CapDeCreus.shp")
SelPol=shapefile("C:/Users/yvesb/Documents/SIG/R12.shp")
OutputName="C:/Users/yvesb/Documents/www/PT365"
FSA=T #SpeciesAll sinon Espro
FMaj=F
MinPT=1
MAXPT=1
ProbMin=0.05
PTdir="C:/Users/yvesb/Downloads"

PTfiles=list.files(PTdir,pattern=paste0("PredTab_",PredTabNum),full.names=T)
PTlist=list()
for (i in max(1,MinPT):min(MAXPT,length(PTfiles)))
{
PTlist[[i]]=fread(PTfiles[i])
}
PredTab=rbindlist(PTlist)

coordinates(PredTab)=c("Group.1","Group.2")
proj4string(PredTab) <- CRS("+init=epsg:4326") # WGS 84


PredPol=raster::intersect(PredTab,SelPol)
PPdf=as.data.frame(PredPol)
OccMoyPP=apply(PPdf[,3:ncol(PredTab)],MARGIN=2,max)
#OccMoyPP=OccMoyPP[order(OccMoyPP,decreasing=T)]

if(FMaj){
  OccMoyPT=apply(as.data.frame(PredTab)[,3:ncol(PredTab)],MARGIN=2,mean)
  EspAb=subset(names(OccMoyPP),(OccMoyPP>OccMoyPT)&(OccMoyPP>ProbMin))
  ScoreAb=subset(OccMoyPP,(OccMoyPP>OccMoyPT)&(OccMoyPP>ProbMin))
}else{
  EspAb=subset(names(OccMoyPP),(OccMoyPP>ProbMin))
  ScoreAb=subset(OccMoyPP,(OccMoyPP>ProbMin))
  
}
EspAb=EspAb[order(ScoreAb,decreasing=T)]
EspAb=gsub("\\."," ",EspAb)
if(FSA){
  EspNew=subset(EspAb,!(EspAb %in% SpeciesAll$Scientific.name))
}else{
  EspNew=subset(EspAb,(EspAb %in% EsPro$espece))
}

EspDec=tstrsplit(EspNew,split=" ")
if(length(EspDec)>2)
{
  EspNew=ifelse(is.na(EspDec[[3]]),paste(EspDec[[1]],EspDec[[2]])
                ,paste0(EspDec[[1]]," ",EspDec[[2]],"-",EspDec[[3]]))
}else{
  EspNew=paste(EspDec[[1]],EspDec[[2]]) 
}
#EspNew=subset(EspNew,EspNew!="Silene flos cuculi")
EspNew=EspNew[order(EspNew)]

test=subset(EspNew,!(EspNew %in% names(PredTab)))
EspNew=subset(EspNew,EspNew %in% names(PredTab))

PredNew=subset(PredPol,select=EspNew)


dir.create(OutputName)

PMaxtot=vector()
for (i in 1:length(EspNew))
{
  Predi=subset(PredNew,select=EspNew[i])
  PMax=round(max(as.data.frame(Predi)[,1])*100)
  PMaxtot=c(PMaxtot,PMax)
  writeOGR(Predi, dsn=paste0(OutputName,"/",gsub(" ","_",EspNew[i]),PMax,".geojson"), layer=OutputName, driver="GeoJSON",overwrite=T)
  print(paste(EspNew[i],PMax,mean(as.data.frame(Predi)[,1])))
}

DataEP=data.frame(EspNew,PMaxtot)
DataEP=DataEP[order(DataEP$PMax,decreasing=T),]
fwrite(DataEP,"PMax.csv",sep=";")
head(DataEP$EspNew,50)
fwrite(as.data.frame(PredNew),"PredNew.csv",sep=";")

