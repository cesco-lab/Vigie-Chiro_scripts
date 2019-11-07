library(data.table)
library(dismo)
library(raster)
library(gstat)

FPredLoc="./VigieChiro/ModPred/Sp_ModRF_nb_contacts_DataSpSL_Urosp_90_Data.csvGI_SysGrid__20000.csv.csv"
Fextent="C:/Users/Yves Bas/Documents/VigieChiro/GIS/France_dep_L93.shp"
PixelSize=2000
ModRF_file="./VigieChiro/ModPred/ModRF_nb_contacts_DataSpSL_Urosp_90_Data.csv.learner"
FSL="SpeciesList.csv"
Rasteriz=T
SaveErrors=F


#Limite
Limite=shapefile(Fextent)
Sys.time()

LimiteL=as(Limite,'SpatialLines')

Title=substr(basename(FPredLoc),22,27)

load(ModRF_file)

SpeciesList=fread(FSL)
SubT=""
if(exists("ModRF"))
{
  if(sum(grepl("rsq",names(ModRF)))>0)
  {
    SubT=paste0(SubT,"PseudoR2 = ",round(ModRF$rsq[length(ModRF$rsq)],2))
  }
  Num=sum(ModRF$y!=0)
  SubT=paste0(SubT," / N = ",Num)
}
PredLoc=fread(FPredLoc)

coordinates(PredLoc) <- c("Group.1", "Group.2")
proj4string(PredLoc) <- CRS("+init=epsg:4326") # WGS 84

CRSL93 <- CRS("+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +units=m +no_defs")
PredL93=spTransform(PredLoc,CRSL93)
v <- voronoi(PredL93)
#plot(v)

VL=crop(v,Limite)


MaxScale=quantile(subset(PredL93$pred,PredL93$pred>0.1),0.95)
if(is.na(MaxScale)){MaxScale=0.1}
ScaleAt=c(-0.1,c(1:49)/49*MaxScale,Inf)


if(nrow(PredL93)<20000)
{
  Taxon=substr(basename(FPredLoc),22,27)
  Taxon=gsub("_","",Taxon)
  test=match(Taxon,SpeciesList$Esp)
  if(is.na(test))
  {
    Title=Taxon
  }else{
    Title=SpeciesList$NomFR[test]
  }
  # print(spplot(VL, 'pred',main=Title,col="transparent"
  #           ,par.settings =
  #            list(axis.line = list(col =  'transparent'))
  #         ,col.regions=get_col_regions(),at=ScaleAt))
  
  png(paste0(FPredLoc,".png"))
  
  
  
  p=spplot(VL, 'pred',main=Title,col="transparent"
           ,par.settings =
             list(axis.line = list(col =  'transparent'))
           ,col.regions=get_col_regions(),at=ScaleAt,sp.layout = LimiteL
           ,xlab=SubT)
  
  print(p)
  
  dev.off()
  
}

if(Rasteriz)
{
  r <- raster(Limite, res=as.numeric(PixelSize))
  vpred <- rasterize(VL, r, 'pred')
  
  spplot(vpred,main=substr(basename(FPredLoc),22,27),at=ScaleAt)
  writeRaster(vpred,paste0(gsub(".csv","",FPredLoc),"_pred.asc"),overwrite=T)
  
  if(SaveErrors)
  {
    verr <- rasterize(VL, r, 'err')
    #plot(verr)
    writeRaster(verr,paste0(gsub(".csv","",FPredLoc),"_err.asc"),overwrite=T)
  }
  
}
