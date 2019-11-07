folderfun="C:/Users/Yves Bas/Documents/Tadarida/Vigie-Chiro_scripts/Vigie-Chiro_scripts/functions"
FCoord="./VigieChiro/GIS/PA/PA_Cicerbita alpina"
#FCoord="./VigieChiro/GIS/RandPts_France_dep_L93Radius_ 94000_1000"

#Coord_Headers=c("Group.1","Group.2")
Coord_Headers=c("decimalLongitude","decimalLatitude")
BS=50
BM=500
BL=5000
Layer_ALAN="C:/Users/Yves Bas/Downloads/SVDNB_npp_20161201-20161231_75N060W_vcmslcfg_v10_c201701271138.avg_rade9.tif"
Layer_Alti="C:/wamp64/www/BDALTIV2_MNT_75M_ASC_LAMB93_IGN69_FRANCE"
Layer_Carthage_P="C:/wamp64/www/CARTHAGE_PLAN/HYDROGRAPHIE_SURFACIQUE.shp"
Layer_Carthage_C="C:/wamp64/www/CARTHAGE_COURS/TRONCON_HYDROGRAPHIQUE.shp"
Layer_CLC="C:/Users/Yves Bas/Downloads/CLC/CLC12_FR_RGF.shp"
Layer_OCS="C:/Users/Yves Bas/Downloads/OCS_2016_CESBIO.tif"
ListLayer=c("ALAN","Alti","Bioclim","Carthage","CLC","OCS")

listfun=list.files(folderfun,full.names=T)
for (i in 1:length(listfun))
{
  source(listfun[i])
}

Sys.time()
Coord_Bioclim(points=FCoord
              ,names_coord=Coord_Headers
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
Coord_Carthage(points=FCoord
           ,names_coord=Coord_Headers
           ,bs=BS
           ,bm=BM
           ,bl=BL
           ,carthagep=Layer_Carthage_P
           ,carthagec=Layer_Carthage_C
)
Sys.time()
Coord_CLC(points=FCoord
          ,names_coord=Coord_Headers
          ,bm=BM
          ,bl=BL
          ,layer=Layer_CLC)
Sys.time()
Coord_OCS_CESBIO(points=FCoord
               ,names_coord=Coord_Headers
               ,bs=BS
               ,bm=BM
               ,layer=Layer_OCS
)
Sys.time()
combineGIS(points=FCoord
           ,names_coord=Coord_Headers
           ,layerlist=ListLayer
           )

