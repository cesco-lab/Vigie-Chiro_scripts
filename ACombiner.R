library(data.table)
Prefix="SysGrid__20000_CLC"

ACombiner=list.files("./VigieChiro/GIS/",pattern=Prefix,full.names=T)

tab=fread(ACombiner[1])

for (i in 2:length(ACombiner))
{
  tab2=fread(ACombiner[i])
tab=rbind(tab,tab2,fill=T,use.names=T)
tab[is.na(tab)]=0
}  

fwrite(tab,paste0("./VigieChiro/GIS/",Prefix,".csv"))
