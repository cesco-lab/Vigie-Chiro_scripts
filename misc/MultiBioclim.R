source("C:/Users/Yves Bas/Documents/Tadarida/Vigie-Chiro_scripts/Vigie-Chiro_scripts/functions/extractGI/f_Coord_Bioclim.R")

ListSG=c("./VigieChiro/GIS/SysGrid_Radius_150000_120000_50000.csv"
       ,"./VigieChiro/GIS/SysGrid_Radius_180000_150000_50000.csv"
       ,"./VigieChiro/GIS/SysGrid_Radius_210000_180000_50000.csv"
       ,"./VigieChiro/GIS/SysGrid_Radius_240000_210000_50000.csv"
       ,"./VigieChiro/GIS/SysGrid_Radius_270000_240000_50000.csv")

for (h in 1:length(ListSG))
{
extract_clim(
  merge_data=T
  ,
  write = T
  ,
  clim=get_fr_worldclim_data()
  ,
  #pts = "PrioCoord_2020-02-25_Hemicycla_pouchet.csv"
  pts = ListSG[h]
  #pts = "C:/wamp64/www/sites_localites.txt"
  
  ,        
  #longlat = c("decimalLongitude", "decimalLatitude")
  longlat = c("Group.1", "Group.2")
  #longlat = c("longitude", "latitude")
  
)
}