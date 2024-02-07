library(data.table)
library(raster)
#library(rgdal)
library(sf)

SpeciesAll=fread("SpeciesAll.csv",sep=";")
EsPro=fread("C:/Users/ybas/Documents/natura/especes_protegees.csv",sep=";")
PredTabNum=45
#SelPol=shapefile("C:/Users/ybas/Documents/SIG/CamargueW.shp")
#SelPol=shapefile("C:/Users/ybas/Documents/SIG/figeac12.shp")
SelPol=shapefile("C:/Users/ybas/Documents/SIG/Aigoual.shp")
#SelPol=shapefile("C:/Users/ybas/Documents/SIG/lmsj12.shp")
#SelPol=shapefile("C:/Users/ybas/Documents/SIG/R12.shp")
OutputName="C:/Users/ybas/Documents/www/PT45Aigoual"
FSA=T #SpeciesAll sinon Espro
FMaj=F
MinPT=1
MAXPT=1
ProbMin=0.1
PTdir="C:/Users/ybas/Downloads"
SpHeure=fread("SpHeure.csv")

PTfiles=list.files(PTdir,pattern=paste0("PredTab_",PredTabNum),full.names=T)
#PTfiles=subset(PTfiles,grepl(".csv",PTfiles))

if(sum(grepl(".csv",PTfiles))>0){
  PTfiles=subset(PTfiles,grepl(".csv",PTfiles)) 
}else{
  for (a in 1:length(PTfiles)){
    unzip(PTfiles[a],exdir=PTdir)
    
  }
  PTfiles=list.files(PTdir,pattern=paste0("PredTab_",PredTabNum),full.names=T,recursive=T)
  PTfiles=subset(PTfiles,grepl(".csv",PTfiles))
  #FPTD=gsub(".zip",".csv",FPTD)  
}

print(PTfiles)
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

EspDec=tstrsplit(EspAb,split=" ")
if(length(EspDec)>2)
{
  EspAb=ifelse(is.na(EspDec[[3]]),paste(EspDec[[1]],EspDec[[2]])
                ,paste0(EspDec[[1]]," ",EspDec[[2]],"-",EspDec[[3]]))
}else{
  EspAb=paste(EspDec[[1]],EspDec[[2]]) 
}




if(FSA){
  EspNew=subset(EspAb,!(EspAb %in% SpeciesAll$Scientific.name))
}else{
  EspNew=subset(EspAb,(EspAb %in% EsPro$espece))
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
  match99=match(EspNew[i],SpHeure$ListSpNew)
  Houri=SpHeure$Heure[match99]
  #if(!is.na(match99)){stop()}
  PMax=round(max(as.data.frame(Predi)[,1])*100)
  PMaxtot=c(PMaxtot,PMax)
  #writeOGR(Predi, dsn=paste0(OutputName,"/",gsub(" ","_",EspNew[i]),PMax,"_",Houri,".geojson"), layer=OutputName, driver="GeoJSON",overwrite=T)
  st_write(st_as_sf(Predi),
           dsn=paste0(OutputName,"/",gsub(" ","_",EspNew[i]),PMax,"_",Houri,".geojson"),
           driver = "GeoJSON",
           overwrite = TRUE, append=FALSE)
  
  print(paste(EspNew[i],PMax,mean(as.data.frame(Predi)[,1])))
}

DataEP=data.frame(EspNew,PMaxtot)
DataEP=DataEP[order(DataEP$PMax,decreasing=T),]
fwrite(DataEP,"PMax.csv",sep=";")
match111=match(DataEP$EspNew,SpHeure$ListSpNew)
Hour111=SpHeure$Heure[match111]
head(cbind(DataEP$EspNew,Hour111),100)
fwrite(as.data.frame(PredNew),"PredNew.csv",sep=";")

