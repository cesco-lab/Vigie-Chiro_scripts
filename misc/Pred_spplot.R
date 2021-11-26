library(data.table)
library(raster)

FDir="./VigieChiro/ModPred/Pred5090"
Limite=shapefile("./VigieChiro/GIS/France_dep_L93.shp")
SpeciesList=fread("SpeciesList.csv")
DirModRF="C:/Users/Yves Bas/Documents/VigieChiro/ModPred/ModRF5090"


LimiteL=as(Limite,'SpatialLines')

LM=list.files(DirModRF,full.names=T)


FPP=list.files(FDir,pattern=".csv$",full.names=T)

for (i in 1:length(FPP))
{
  Title=tstrsplit(basename(FPP[i]),split="_")[[2]]

  PointsPred=fread(FPP[i])
  
  FMsp=subset(LM,grepl(Title,LM))
  
  test=match(Title,SpeciesList$Esp)
  if(!is.na(test))
  {
    Title=paste0(SpeciesList$NomFR[test]
                 ," (",SpeciesList$`Scientific name`[test],")")
  }
  
  
  if(length(FMsp)==1){load(FMsp)}
  
  SubT=""
  if(exists("ModRF"))
  {
    if(sum(grepl("rsq",names(ModRF)))>0)
    {
      SubT=paste0(SubT,"- R2=",round(ModRF$rsq[length(ModRF$rsq)],2))
    }
    Num=sum(ModRF$y!=0)
    SubT=paste0(SubT," / N=",Num)
  }
  
  Title=paste(Title,SubT)
  
  coordinates(PointsPred)=c("Group.1","Group.2")
  proj4string(PointsPred)= CRS("+init=epsg:4326") # WGS 84
  
  CRSL93 <- CRS("+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +units=m +no_defs")
  PredL93=spTransform(PointsPred,CRSL93)
  
  GridPred=PredL93
  gridded(GridPred)=T
  
  MaxScale=quantile(subset(PredL93$pred,PredL93$pred>0.1),0.98)
  if(is.na(MaxScale)){MaxScale=0.1}
    GridPred$pred=pmin(GridPred$pred,MaxScale)
  GridPred$predcorr=10^(GridPred$pred)-1
  GridPred$predcorr=pmax(0,GridPred$predcorr)
  ScaleAt=c(-0.1,c(1:99)/99*MaxScale,max(GridPred$pred)+1e-3)
  ScaleAtCorr=10^(ScaleAt)-1
  
  
  png(gsub(".csv",".png",FPP[i]),width = 480*1.5, height = 480*1.5)
  
  plot(GridPred["predcorr"],breaks=ScaleAtCorr)
  #plot(GridPred,main=Title,legend.cex=1)
  plot(LimiteL, add=TRUE)
  title(Title,cex=0.5)
  dev.off()
  print(Title)
}