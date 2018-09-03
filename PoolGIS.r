library(data.table)
Pool="C:/Users/Yves Bas/Documents/VigieChiro/GIS/Occitanie"

GISfiles=list.files(Pool,full.names=T)

GISdata=sapply(GISfiles,fread)
  
GIStot=rbindlist(GISdata,fill=T,use.names=T)

GIStot[is.na(GIStot)]=0
fwrite(GIStot,paste0(dirname(Pool),"/GI_",basename(Pool),".csv"))
