library(data.table)
library(readxl)
library(rgbif)
Download="C:/Users/ybas/Downloads"
Doublons=c("Microcarbo pygmeus","Dendrocopos medius","Larus ridibundus"
           ,"Saxicola torquatus","Schoeniclus schoeniclus"
           ,"Felis domesticus","Anser cygnoid","Erythronium dens-canis"
           ,"Anthyllis barba-jovis","Scandix pecten-veneris"
           ,"Neottia nidus-avis","Adiantum capillus-veneris"
           ,"Paliurus spina-christi","Phlomis herba-venti"
           ,"Podospermum laciniatum","Piptatherum paradoxum"
           ,"Scorzonera hispanica","Sorbus domestica"
           ,"Sterna nilotica","Carduus vivariensis","Veronica anagallis-aquatica","Fabriciana niobe"
           ,"Myosoton aquaticum","Echinochloa crus-galli","Lotus glaber","Stipa offneri"
           ,"Piptatherum miliaceum","Apus melba","Oxalis pres-caprae","Dendrocopos minor","Chloris chloris"
           ,"Neophron peronopterus","Lysimachia linum-stellatum","Rosmarinus officinalis"
           ,"Adiantum capillus-veneris","Dorycnium pentaphyllum","Dorycnium hirsutum","Neottia nidus-avis"
           ,"Stellaria holostea","Cervaria rivini","Setophaga striata","Rhamnus infectoria"
           ,"Legousia speculum-veneris","Fabriciana adippe","Cicada orni","Blitum bonus-henricus"
           ,"Polygonia c-album","Echinochloa crus-galli","Trifolium infamia-ponertii"
           ,"Massylaea vermiculata","Achnatherum miliaceum","Ophrys virescens","Lysimachia linum-stellatum"
           ,"Cynoglossum cheirifolium","Rhaphiolepis loquata"
           ,"Anacamptis fragrans","Dianthus godronianus"
           ,"Microthlaspi perfoliatum","Anas clypeata","Anas strepera")

Species=vector()
Inat=list.files(Download,pattern="observations-",full.names=T)
FInat=file.info(Inat)
sel=which.max(FInat$ctime)
DataInat=fread(unzip(Inat[sel]))
LInat=unique(DataInat$scientific_name)
Species=c(Species,LInat)

# test0=("Pistacia lentiscus" %in% Species)
# test1=subset(SpeciesAll,grepl("Pistacia",SpeciesAll$Scientific.name))


# PLANTNET
PNdata=fread(paste0(Download,"/my-observations.csv"))
PNG=tstrsplit(PNdata$`current name`,split=" ")[[1]]
PNS=tstrsplit(PNdata$`current name`,split=" ")[[2]]
PNname=paste(PNG,PNS)
Species=c(Species,PNname)


CEL=list.files(Download,pattern="cel_export",full.names=T)
FCEL=file.info(CEL)
sel=which.max(FCEL$ctime)
DataCEL=read_excel(CEL[sel])
LCEL=unique(DataCEL$Espèce)
LCELs=tstrsplit(LCEL," ")
LCELs2=paste(LCELs[[1]],LCELs[[2]])
Species=c(Species,LCELs2)

Observado=list.files(Download,pattern="export_observado",full.names=T)
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

i=match("Pistacia lentiscus",Species)
i=which.max(grepl("hippothoe",Species))

SpeciesGBIF=vector()
for (i in 1:length(Species))
{
  #test=name_backbone(name=Species[i])
  test=name_backbone_verbose(name=Species[i])
  if(is.list(test)){
    test=rbindlist(test,use.names=T,fill=T)
  }
  if("data" %in% names(test)){
    Speciesi=c(test$data$species,test$alternatives$species)
    
  }else{
    Speciesi=c(test$species)
    
  }
  if(length(Speciesi)==0){
    #stop("pb requete")
    Speciesi=Species[i]
    }
  SpeciesGBIF=c(SpeciesGBIF,Speciesi)
  if(i%%100==1){print(paste(i,length(Species),Sys.time()))}
}

SpeciesGBIF=c(Species,SpeciesGBIF,Doublons)
SpeciesGBIF=unique(SpeciesGBIF)
print(length(SpeciesGBIF))

fwrite(data.frame('Scientific name'=SpeciesGBIF),"SpeciesAll.csv",sep=";")


("Pittosporum tobira" %in% SpeciesGBIF)
