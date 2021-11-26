folderfun="C:/Users/Yves Bas/Documents/Tadarida/Vigie-Chiro_scripts/Vigie-Chiro_scripts/functions/extractGI"
FCoord="./VigieChiro/GIS/PA/PA_Larus ridibundus"
FCoord="./VigieChiro/GIS/PA/PATot_FR4___130"
#FCoord="./VigieChiro/GIS/RandPts_France_dep_L93Radius_ 29000_1000"
#FCoord="./VigieChiro/GIS/coordWGS84_2019-11-26"
#FCoord="./VigieChiro/GIS/SysGrid_Radius_ 260000_3000"
#FCoord="./VigieChiro/GIS/SysGrid__France_Spain_Switzerland_Italy_-2e+06_3e+06_4e+06_8e+06_1000"
#FCoord="./VigieChiro/GIS/Capture/site_capture_chiro"
Coord_Headers=c("Group.1","Group.2")
#Coord_Headers=c("X_CENTROID","Y_CENTROID")
Coord_Headers=c("decimalLongitude","decimalLatitude")
BS=50
BM=500
BL=5000
Layer_ALAN="C:/Users/Yves Bas/Downloads/SVDNB_npp_20161201-20161231_75N060W_vcmslcfg_v10_c201701271138.avg_rade9.tif"
Layer_CLC="./SIG/clc2018_clc2018_v2018_20_raster100m/CLC2018_CLC2018_V2018_20.tif"
ListLayer=c("ALAN","Bioclim","CLCraster")


listfun=list.files(folderfun,full.names=T,pattern=".R$")
for (i in 1:length(listfun))
{
  source(listfun[i])
}




Sys.time()
extract_clim(
  pts = paste0(FCoord,".csv")
  , 
  longlat = Coord_Headers
  ,
  merge_data=T
  ,
               write = T
  ,
  clim=get_fr_worldclim_data()
  )



Sys.time()
Coord_ALAN(points=FCoord
  ,names_coord=Coord_Headers
  ,bl=BL
  ,layer=Layer_ALAN)
Sys.time()
#Coord_Alti(points=FCoord
 #          ,names_coord=Coord_Headers
  #       ,bm=BM
   #      ,bl=BL
      #     ,layer=Layer_Alti)
#Sys.time()
Coord_CLCraster(
  points=FCoord
          ,
  names_coord=Coord_Headers
          ,
  bm=BM
          ,
  bl=BL
          ,
  layer=Layer_CLC
  )
combineGIS(
  points=FCoord
           ,
  names_coord=Coord_Headers
           ,
  layerlist=ListLayer
           )

