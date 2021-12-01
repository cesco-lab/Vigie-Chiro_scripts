library(data.table)
library(readxl)
library(rgbif)
Download="C:/Users/yvesb/Downloads"
Doublons=c("Microcarbo pygmeus","Dendrocopos medius","Larus ridibundus"
           ,"Saxicola torquatus","Schoeniclus schoeniclus"
           ,"Felis domesticus","Anser cygnoid","Erythronium dens-canis"
           ,"Anthyllis barba-jovis","Scandix pecten-veneris"
           ,"Neottia nidus-avis","Adiantum capillus-veneris"
           ,"Paliurus spina-christi","Phlomis herba-venti"
           ,"Podospermum laciniatum","Piptatherum paradoxum"
           ,"Scorzonera hispanica","Sorbus domestica"
           ,"Sterna nilotica","Carduus vivariensis","Veronica anagallis-aquatica","Fabriciana niobe"
           ,"Myosoton aquaticum","Echinochloa crus-galli","Lotus glaber")

Species=vector()
Inat=list.files(Download,pattern="observations-",full.names=T)
FInat=file.info(Inat)
sel=which.max(FInat$ctime)
DataInat=fread(unzip(Inat[sel]))
LInat=unique(DataInat$scientific_name)
Species=c(Species,LInat)

CEL=list.files(Download,pattern="cel_export",full.names=T)
FCEL=file.info(CEL)
sel=which.max(FCEL$ctime)
DataCEL=read_excel(CEL[sel])
LCEL=unique(DataCEL$Espèce)
LCELs=tstrsplit(LCEL," ")
LCELs2=paste(LCELs[[1]],LCELs[[2]])
Species=c(Species,LCELs2)

Observado=list.files(Download,pattern="export_",full.names=T)
Observado=subset(Observado,(grepl("xlsx",Observado)))
Observado=subset(Observado,!(grepl("~",Observado)))
FObservado=file.info(Observado)
sel=which.max(FObservado$ctime)
DataObservado=read_excel(Observado[sel])

LObservado=unique(DataObservado$`nom scientifique`)
Species=c(Species,LObservado)

Species=unique(Species)
Species=subset(Species,grepl(" ",Species))

Species=c(Species,Doublons)

SpeciesGBIF=vector()
for (i in 1:length(Species))
{
  test=name_backbone(name=Species[i])
  SpeciesGBIF=c(SpeciesGBIF,test$species)
  if(i%%100==1){print(paste(i,length(Species),Sys.time()))}
}

SpeciesGBIF=c(SpeciesGBIF,Doublons)

fwrite(data.frame('Scientific name'=SpeciesGBIF),"SpeciesAll.csv",sep=";")



