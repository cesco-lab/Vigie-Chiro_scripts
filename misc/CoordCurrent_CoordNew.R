library(data.table)
CoordCurrent=fread("./VigieChiro/GIS/coordWGS84_SpNuit2_50_DataLP_PF_exportTot.csv")
GIPrev=fread("./VigieChiro/GIS/GI_coordWGS84_SpNuit2_50_DataLP_PF_exportTot.csv")

LLC=paste(CoordCurrent$Group.1,CoordCurrent$Group.2)
LLP=paste(GIPrev$Group.1,GIPrev$Group.2)

CoordNew=subset(CoordCurrent,!(LLC %in% LLP))
Name=paste0("./VigieChiro/GIS/coordWGS84_",substr(Sys.time(),1,10),".csv")
fwrite(CoordNew,Name)
