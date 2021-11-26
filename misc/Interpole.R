library(data.table)
library(dismo)
library(raster)
library(gstat)

if(length(args)<3)
{
  args=vector()
  args[6]="C:/Users/Yves Bas/Downloads/Pred_Myodau_Act_07_GI_SysGrid__3e+05"
  args[7]="C:/Users/Yves Bas/Documents/VigieChiro/GIS/FranceD__30_34.shp"
  args[7]="France_dep_L93.shp"
  args[8]=5000 #PixelSize
  ModRF_file="C:/Users/Yves Bas/Downloads/ModRFActLog_Eptser50.learner"
    load(ModRF_file)
    MaxSample=100
}
SpeciesList=fread("SpeciesList.csv")
Rasteriz=F
SaveErrors=F
VPlot=T
RPlot=F

Sys.time()
#Limite
Limite=shapefile(paste0("./VigieChiro/GIS/",args[7]))
Sys.time()

LimiteL=as(Limite,'SpatialLines')

Title=substr(args[6],22,27)

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
PredLoc=fread(paste0(args[6],".csv"))
Sys.time()
PredLoc=PredLoc[1:min(MaxSample,nrow(PredLoc)),]


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


if((VPlot))
{
  Taxon=substr(basename(args[6]),6,11)
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
  
  png(paste0(args[6],".png"))
  #jpeg(paste0(args[6],".jpg"))
  #tiff(paste0(args[6],".tif"))
  
  #writeOGR(VL, dirname(args[6]), basename(args[6]), driver="ESRI Shapefile")
  
  p=spplot(VL, 'pred',main=Title,col="transparent"
           ,par.settings =
             list(axis.line = list(col =  'transparent'))
           ,col.regions=get_col_regions(),at=ScaleAt,sp.layout = LimiteL
           ,xlab=SubT)
  
  print(p)
  
  dev.off()
  
}
Sys.time()
if(Rasteriz)
{
  r <- raster(Limite, res=as.numeric(args[8]))
  vpred <- rasterize(VL, r, 'pred')
  
  #gs <- gstat(formula=PredL93$pred~1, locations=PredL93, nmax=10, set=list(idp = 0))
  #nn <- interpolate(r, gs)
  ## [inverse distance weighted interpolation]
  #nnmsk <- mask(nn, vpred)
  #plot(nnmsk,main=substr(args[6],22,27))
  
  #spplot(VL, 'err', col.regions=get_col_regions())
  
  
  if(VPlot){spplot(vpred,main=substr(args[6],22,27),at=ScaleAt)}
  
  writeRaster(vpred,paste0(args[6],"_pred.asc"),overwrite=T)
  
  if(RPlot)
  {
    png(paste0(args[6],"_R.png"))
    print(spplot(vpred,main=args[1],at=ScaleAt,sp.layout = LimiteL
                 ,xlab=SubT))
    dev.off()
  }
  
  
  if(SaveErrors)
  {
    verr <- rasterize(VL, r, 'err')
    #plot(verr)
    writeRaster(verr,paste0(args[6],"_err.asc"),overwrite=T)
  }
  
}
Sys.time()
