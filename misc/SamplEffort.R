library(data.table)
library(sp)
library(dismo)

Radius=20000
#Grid=fread("./VigieChiro/GIS/SysGrid__3e+05.csv")
Grid=fread("./VigieChiro/GIS/SysGrid__20000.csv")
Coord=fread("./VigieChiro/GIS/coordWGS84_SpNuit2_50_DataLP_PF_exportTot.csv")
Limite=shapefile(paste0("./VigieChiro/GIS/France_dep_L93.shp"))

LimiteL=as(Limite,'SpatialLines')

NbSampl=vector()
for (i in 1:nrow(Grid))
{
  #Nbt=subset(Coord,(Coord$Group.1>Grid$Group.1[i]-Radius/100000)&
   #            (Coord$Group.1<Grid$Group.1[i]+Radius/100000)&
    #           (Coord$Group.2>Grid$Group.2[i]-Radius/100000)&
     #          (Coord$Group.2<Grid$Group.2[i]+Radius/100000))
  Nbt=subset(Coord,(((Coord$Group.1-Grid$Group.1[i])^2+
                      (Coord$Group.2-Grid$Group.2[i])^2)^0.5<Radius/1e5))
               
  
NbSampl=c(NbSampl,nrow(Nbt))
if(i%%10000==1){print(paste(i,Sys.time()))}
}
NbSr=rank(NbSampl)
plot(NbSr,NbSampl)

Grid$NbSampl=NbSampl
Grid$NbSr=NbSr

coordinates(Grid) <- c("Group.1", "Group.2")
proj4string(Grid) <- CRS("+init=epsg:4326") # WGS 84

CRSL93 <- CRS("+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +units=m +no_defs")
PredL93=spTransform(Grid,CRSL93)
Sys.time()
v <- voronoi(PredL93,eps=1e-06)
Sys.time()
#plot(v)

VL=crop(v,Limite)
Sys.time()


MaxScale=quantile(subset(PredL93$NbSr,PredL93$NbSr>0.1),0.95)
MinScale=min(PredL93$NbSr)
if(is.na(MaxScale)){MaxScale=0.1}
ScaleAt=c(MinScale,c(1:49)/49*MaxScale,Inf)



    Title="Effort d'echantillonnage Point Fixe"

  png(paste0("Effort_echantillonnage_Point_Fixe.png"))
  
  
  
  p=spplot(VL, 'NbSr',main=Title,col="transparent"
           ,par.settings =
             list(axis.line = list(col =  'transparent'))
           ,col.regions=get_col_regions(),at=ScaleAt,sp.layout = LimiteL
           )
  
  print(p)
  
  dev.off()
  
