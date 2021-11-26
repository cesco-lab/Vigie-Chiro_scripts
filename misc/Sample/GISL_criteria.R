library(data.table)

GI=fread("./www/GI_sites_localites.csv")
Criteria="SpHC1M"
Threshold=0.12

GIcrit=subset(GI,select=Criteria)
names(GIcrit)="crit"

GIsel=subset(GI,GIcrit$crit>Threshold)

GIcoord=subset(GIsel,select=c("longitude","latitude","localite.x","id_site.x","site.x","nom.x","observateur.x"))

               fwrite(GIcoord,paste0("./www/SitLoc_",Criteria,"_",Threshold*100,".csv"),sep=";")
               