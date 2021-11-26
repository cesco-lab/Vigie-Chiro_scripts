folderfun="C:/Users/Yves Bas/Documents/Tadarida/Vigie-Chiro_scripts/Vigie-Chiro_scripts/functions/extractGI"

Pipeline="exists"
FCoord="./mnhn/Veolia/Sites_Veolia2008"
#Coord_Headers=c("Group.1","Group.2")
Coord_Headers=c("longitude","latitude")
#Coord_Headers=c("decimalLongitude","decimalLatitude")
BS=50
BM=500
BL=5000
Layer_ALAN="C:/Users/Yves Bas/Downloads/SVDNB_npp_20161201-20161231_75N060W_vcmslcfg_v10_c201701271138.avg_rade9.tif"
Layer_Alti="C:/wamp64/www/BDALTIV2_MNT_75M_ASC_LAMB93_IGN69_FRANCE"
Layer_Carthage_P="C:/wamp64/www/CARTHAGE_PLAN/HYDROGRAPHIE_SURFACIQUE.shp"
Layer_Carthage_C="C:/wamp64/www/CARTHAGE_COURS/TRONCON_HYDROGRAPHIQUE.shp"
Layer_CLC="./SIG/clc2018_clc2018_v2018_20_raster100m/CLC2018_CLC2018_V2018_20.tif"
Layer_OCS="C:/Users/Yves Bas/Downloads/OCS_2018_CESBIO.tif"
Layer_ROUTE="C:/wamp64/www/Route500/R500_3-0_SHP_LAMB93_FXX-ED191/RESEAU_ROUTIER/TRONCON_ROUTE.shp"
Layer_FERRE="C:/wamp64/www/Route500/R500_3-0_SHP_LAMB93_FXX-ED191/RESEAU_FERRE/TRONCON_VOIE_FERREE.shp"
Layer_eoliennes="C:/wamp64/www/eoliennes/"
ListLayer=c("ALAN","Alti","Bioclim","Carthage","CLCraster","OCS2018bis"
            ,"Reseau","Transports")
ListLayer=c("Bioclim","CLCraster"
            ,"Reseau","Transports")

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
Coord_Alti(points=FCoord
           ,names_coord=Coord_Headers
           ,bm=BM
           ,bl=BL
           ,layer=Layer_Alti)
Sys.time()
Coord_Carthage(
  points=FCoord
           ,
  names_coord=Coord_Headers
           ,
  bs=BS
           ,
  bm=BM
           ,
  bl=BL
           ,
  carthagep=Layer_Carthage_P
           ,
  carthagec=Layer_Carthage_C
)
Sys.time()
Coord_CLCraster(points=FCoord
          ,names_coord=Coord_Headers
          ,bm=BM
          ,bl=BL
          ,layer=Layer_CLC)
Sys.time()
Coord_OCS_CESBIO2018(points=FCoord
               ,names_coord=Coord_Headers
               ,bs=BS
               ,bm=BM
               ,layer=Layer_OCS
)
Sys.time()
ListLE=list.files(Layer_eoliennes,pattern=".shp$",full.names=T)
ListLE=subset(ListLE,grepl("en_service.shp",ListLE))

Coord_eol(points=FCoord
                     ,names_coord=Coord_Headers
                     ,bs=BS
                     ,bm=BM
                     ,bl=BL
                     ,layer1=ListLE[1]
                ,layer2=ListLE[2],layer3=ListLE[3],layer5=ListLE[4]
                ,layer6=ListLE[5],layer7=ListLE[6],layer8=ListLE[7]
                ,layer9=ListLE[8],layer10=ListLE[9],layer11=ListLE[10]
                ,layer12=ListLE[11],layer13=ListLE[12]
                )
Sys.time()
Coord_Route(points=FCoord
                ,names_coord=Coord_Headers
                ,bs=BS
                ,bm=BM
                ,bl=BL
                ,layer1=Layer_ROUTE
                ,layer2=Layer_FERRE
            )
Sys.time()



combineGIS_FR(
  points=FCoord
           ,
  names_coord=Coord_Headers
           ,
  layerlist=ListLayer
           )

