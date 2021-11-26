library(data.table)
library(sp)
library(raster)
ETVF=list.files(getwd(),pattern="export_validtot")
SpeciesList=fread("SpeciesList.csv")
Particip=fread("C:/wamp64/www/p_export.csv",encoding="UTF-8")
SiteLoc=fread("C:/wamp64/www/sites_localites.txt")
Limite=shapefile(paste0("./VigieChiro/GIS/France_dep_L93.shp"))
SpNuit=fread("SpNuit2_50_DataLP_PF_exportTot.csv")



LimiteL=as(Limite,'SpatialLines')


DataValid=list()
for (i in 1:length(ETVF))
{
  DataValid[[i]]=fread(ETVF[i])
}
Valid=rbindlist(DataValid,use.names=T,fill=T)
colnames(Valid)[10]="temps_fin"

Validi=subset(Valid,substr(Valid$donnee,2,2)=="i")
Valida=subset(Valid,substr(Valid$donnee,2,2)=="a")

ValidiP=merge(Validi,Particip,by="participation")
ValidaP=merge(Valida,Particip,by="participation")

ValidiP$Tron=as.character(as.numeric(gsub("Tron",""
                                          ,tstrsplit(ValidiP$donnee,split="-")[[4]])))
SiteLoc$Tron=tstrsplit(gsub("T ","",SiteLoc$nom),split=" ")[[1]]
SiteLoc$Section=tstrsplit(gsub("T ","",SiteLoc$nom),split=" ")[[2]]
SiteLoc=subset(SiteLoc,(SiteLoc$Section=="3")|(is.na(SiteLoc$Section)))
ValidiPL=merge(ValidiP,SiteLoc,by=c("site","Tron"))
table(ValidiPL$protocole)

ValidaPL=merge(ValidaP,SiteLoc,by.x=c("site","point"),by.y=c("site","nom"))
ValidPL=rbindlist(list(ValidiPL,ValidaPL),use.names=T,fill=T)
test=subset(ValidPL,(ValidPL$proprietaire=="Yves Bas")
            &(ValidPL$valid.espece==""))
test$valid.espece=test$obs.espece
test$valid.proba=test$obs.proba
ValidPL=rbind(ValidPL,test)

ValidPLu=unique(ValidPL,by=c("valid.espece","valid.proba"
                             ,"date_debut","latitude","longitude"))
ValidPLus=subset(ValidPLu,ValidPLu$valid.proba=="SUR")

BatList=subset(SpeciesList$Esp,SpeciesList$Group=="bat")
ValidPLusb=subset(ValidPLus,ValidPLus$valid.espece %in% BatList)
ValidPLusb$Date=substr(ValidPLusb$date_debut,1,10)

ValidPLusb=merge(ValidPLusb,SpeciesList,by.x="valid.espece",by.y="Esp")

fwrite(ValidPLusb,"ValidPLusb.csv",sep=";")


BatNuit=subset(SpNuit,SpNuit$espece %in% BatList)
test=match(c("longitude","latitude"),colnames(ValidPLusb))

coordinates(ValidPLusb) <- test
proj4string(ValidPLusb) <- CRS("+init=epsg:4326") # WGS 84

ValidPLusb$num=c(1:nrow(ValidPLusb))

spplot(ValidPLusb,'num'
       ,col="transparent"
       ,sp.layout = LimiteL
       ,par.settings =
         list(axis.line = list(col =  'transparent'))
)


for (j in 1:nlevels(as.factor(ValidPLusb$valid.espece)))
{
  Validj=subset(ValidPLusb,ValidPLusb$valid.espece==levels(as.factor(ValidPLusb$valid.espece))[j])
  print(paste(levels(as.factor(ValidPLusb$valid.espece))[j],nrow(Validj)))
  if(nrow(Validj)>1)
  {
    p= spplot(Validj,'num'
              ,col="transparent"
              ,sp.layout = LimiteL
              ,par.settings =
                list(axis.line = list(col =  'transparent'))
              ,main=levels(as.factor(ValidPLusb$valid.espece))[j]
    )
    print(p)
  }
}

