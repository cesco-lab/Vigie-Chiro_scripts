library(data.table)
library(raster)
library(sf)
library(lubridate)

SpeciesList=fread("SpeciesList.csv")
ValidData=fread("ValidDataPart.csv")
ListPartWavArchiv=fread("C:/wamp64/www/wavarchivees.txt")
Particip=fread("C:/wamp64/www/p_export.csv",encoding="UTF-8")
SiteLocF="C:/wamp64/www/sites_localites.txt"
Zone="C:/Users/Yves Bas/Documents/SIG/Limite_administrative/France_dep_L93.shp"
SLpred="./VigieChiro/gbifData/SL"
DateSp="./VigieChiro/gbifData/DateSp/"
Nsp=18
Npart=10

Particip$ProperDate= dmy_hm(Particip$date_debut)
Particip$yday=yday(Particip$ProperDate)
Particip=subset(Particip
                ,Particip$participation %in% ListPartWavArchiv$V1)

SiteLoc=fread(SiteLocF)



DateF=list.files(DateSp,full.names=T)
DateL=list()
for (h in 1:length(DateF))
{
  DateL[[h]]=fread(DateF[h])
}
DateAll=rbindlist(DateL)


SLfiles=list.files(SLpred,full.names=T)

FranceD= shapefile(Zone)
p=st_as_sf(FranceD)
pbuf = st_buffer(p, 1000)
FranceD=as(pbuf,'Spatial')

coordinates(SiteLoc)=c("longitude","latitude")
proj4string(SiteLoc)=CRS("+init=epsg:4326")
SiteLoc=spTransform(SiteLoc,proj4string(FranceD))

Tri_FR=over(SiteLoc,FranceD)

SiteLoc=fread(SiteLocF)
SL_FR=subset(SiteLoc,!is.na(Tri_FR$CHEF_LIE0))
Part_FRPF=subset(Particip,paste(Particip$site,Particip$point)
                 %in% paste(SL_FR$site,SL_FR$nom))
Part_FRPF=subset(Part_FRPF
                 ,substr(Part_FRPF$site,1,19)=="Vigiechiro - Point ")
Part_RP=subset(Particip,!substr(Particip$site,1,19)=="Vigiechiro - Point ")
#PartFR=c(Part_FRPF$participation,Part_RP$participation)
#test=subset(Part_RP,Part_RP$participation=="5a2aa7ac552423000d77e090")

SpeciesSel=subset(SpeciesList,SpeciesList$Group %in% c("bat","bush-cricket"))
SpeciesSel=subset(SpeciesSel,!grepl("sp.",SpeciesSel$`Scientific name`))
SpeciesSel=subset(SpeciesSel,SpeciesSel$France=="x")

#points fixes
ValidFR=subset(ValidData,ValidData$participation %in% Part_FRPF$participation)
SpValidN=aggregate(ValidFR$participation,by=list(ValidFR$validateur_taxon)
                   ,length)
test=subset(ValidFR,ValidFR$validateur_taxon=="Eptisa")

MissingSpecies=subset(SpeciesSel,!(SpeciesSel$Nesp2 %in% SpValidN$Group.1))
SpeciesSel2=unique(SpeciesSel,by="Nesp2")
SpeciesSelN=merge(SpeciesSel2,SpValidN,by.x="Nesp2",by.y="Group.1"
                  ,all.x=T)
SpeciesSelN$x[is.na(SpeciesSelN$x)]=0
SpeciesSelN=SpeciesSelN[order(SpeciesSelN$x)]

ValidV=subset(ValidData,ValidData$validateur_taxon!="")
Partdispo=subset(Part_FRPF,!(Part_FRPF$participation %in% ValidV$participation))

ParSel=vector()
Esp=vector()
for (i in 1:Nsp)
{
  Datei=subset(DateAll
               ,DateAll$ListSpValide==SpeciesSelN$`Scientific name`[i])
  Parti=subset(Partdispo,(Partdispo$yday>Datei$PicSp[1]-30)
               &(Partdispo$yday<Datei$PicSp[1]+30))
  PartSLi=merge(Parti,SL_FR,by.x=c("site","point"),by.y=c("site","nom"))
  
  SLi=subset(SLfiles,grepl(SpeciesSelN$`Scientific name`[i]
                           ,basename(SLfiles)))
  if(length(SLi)>0)
  {
    SLpredi=fread(SLi[1])
    
    PartSLpredi=merge(PartSLi,SLpredi,by.x=c("longitude","latitude")
                      ,by.y=c("Group.1","Group.2"))
    PartSLpredi=PartSLpredi[order(PartSLpredi$pred,decreasing=T),]
    
    ParSeli=PartSLpredi$participation[1:10]
    ParSel=c(ParSel,ParSeli)
    Esp=c(Esp,rep(SpeciesSelN$Nesp2[i],10))
    
  }
}
ParSelMissingSpecies=data.frame(cbind(ParSel,Esp))
fwrite(ParSelMissingSpecies,paste0("ParSelMissing",Sys.Date(),".csv")
       ,sep=";")


ValidFR=subset(ValidData,ValidData$participation %in% Part_RP$participation)
SpValidN=aggregate(ValidFR$participation,by=list(ValidFR$validateur_taxon)
                   ,length)
test=subset(ValidFR,ValidFR$validateur_taxon=="Eptisa")
test

MissingSpecies=subset(SpeciesSel,!(SpeciesSel$Nesp2 %in% SpValidN$Group.1))
SpeciesSel2=unique(SpeciesSel,by="Nesp2")
SpeciesSelN=merge(SpeciesSel2,SpValidN,by.x="Nesp2",by.y="Group.1"
                  ,all.x=T)
SpeciesSelN$x[is.na(SpeciesSelN$x)]=0
SpeciesSelN=SpeciesSelN[order(SpeciesSelN$x)]

ValidV=subset(ValidFR,ValidFR$validateur_taxon!="")
#PrefV=substr(ValidV,ValidV$donnee,1,19)
Partdispo=subset(Part_RP,!(Part_RP$participation %in% ValidV$participation))

SL_RP=unique(SL_FR,by="site")

ParSel=vector()
Esp=vector()
for (i in 1:Nsp)
{
  Datei=subset(DateAll
               ,DateAll$ListSpValide==SpeciesSelN$`Scientific name`[i])
  Parti=subset(Partdispo,(Partdispo$yday>Datei$PicSp[1]-30)
               &(Partdispo$yday<Datei$PicSp[1]+30))
  
  PartSLi=merge(Parti,SL_RP,by.x=c("site"),by.y=c("site"))
  
  SLi=subset(SLfiles,grepl(SpeciesSelN$`Scientific name`[i]
                           ,basename(SLfiles)))
  if(length(SLi)>0)
  {
    SLpredi=fread(SLi[1])
    
    PartSLpredi=merge(PartSLi,SLpredi,by.x=c("longitude","latitude")
                      ,by.y=c("Group.1","Group.2"))
    PartSLpredi=PartSLpredi[order(PartSLpredi$pred,decreasing=T),]
    
    ParSeli=PartSLpredi$participation[1:Npart]
    ParSel=c(ParSel,ParSeli)
    Esp=c(Esp,rep(SpeciesSelN$Nesp2[i],Npart))
    
  }
}
ParSelMissingSpecies=data.frame(cbind(ParSel,Esp))
fwrite(ParSelMissingSpecies,paste0("ParSelMissingRP",Sys.Date(),".csv")
       ,sep=";")

