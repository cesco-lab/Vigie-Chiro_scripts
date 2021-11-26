library(data.table)
library(glmmTMB)

DataSTOC=fread("C:/Users/Yves Bas/Documents/VigieChiro/STOC-EPS/data_FrenchBBS_squarre_Diane_20180628_allSp_2001_2018.csv")
GI=fread("./VigieChiro/GIS/GI_carre_stoc.csv")
TrendFR=fread("C:/Users/Yves Bas/Downloads/tendanceGlobalEspece.csv"
              ,dec=",")
TrendEU=fread("./mnhn/TrendsEBCC_1980_2016.csv")
Taxref=fread("./mnhn/TAXREF13.0_FR__24_08_2020.csv")
RedListEU=fread("./mnhn/European_Red_List_2017_December.csv")
BDCstatus=fread("./mnhn/BDC_STATUTS_13.csv")
Perf=fread("PerfsumCS_2020-09-02.csv")

TrendFRfiable=subset(TrendFR,TrendFR$valide=="bon")

DataSTOC$scientific_name[DataSTOC$scientific_name=="Carduelis chloris"]="Chloris chloris"
DataSTOC$scientific_name[DataSTOC$scientific_name=="Dendrocopos minor"]="Dryobates minor"
DataSTOC$scientific_name[DataSTOC$scientific_name=="Parus montanus"]="Poecile montanus"
DataSTOC$scientific_name[DataSTOC$scientific_name=="Carduelis spinus"]="Spinus spinus"
DataSTOC$scientific_name[DataSTOC$scientific_name=="Tetrao tetrix"]="Lyrurus tetrix"
DataSTOC$scientific_name[DataSTOC$scientific_name=="Serinus citrinella"]="Carduelis citrinella"
DataSTOC$scientific_name[DataSTOC$scientific_name=="Carduelis flammea"]="Acanthis flammea"
DataSTOC$scientific_name[DataSTOC$scientific_name=="Parus cyanus"]="Cyanistes cyanus"
DataSTOC$scientific_name[DataSTOC$scientific_name=="Carduelis hornemanni"]="Acanthis hornemanni"
DataSTOC$scientific_name[DataSTOC$scientific_name=="Carduelis flavirostris"]="Linaria flavirostris"
DataSTOC$scientific_name[DataSTOC$scientific_name=="Carduelis cannabina"]="Linaria cannabina"
DataSTOC$scientific_name[DataSTOC$scientific_name=="Carduelis hornemanni"]="Acanthis hornemanni"

CorrCodeSTOC=data.frame(Code=DataSTOC$code_sp,Name=DataSTOC$scientific_name)
CorrCodeSTOC=unique(CorrCodeSTOC)


DataGI=merge(DataSTOC,GI,by.x="carre",by.y="pk_carre")
DataGI$foret=scale(DataGI$SpHO31M+DataGI$SpHO32M)
hist(DataGI$foret)
DataGI$eau=scale(scale(DataGI$SpWS_M)+scale(DataGI$SpWC_M))
hist(DataGI$eau)
DataGI$urbain=scale(DataGI$SpHO4M) 
hist(DataGI$urbain)
DataGI$prairies=scale(DataGI$SpHO211M+DataGI$SpHO34M) 
hist(DataGI$prairies)



DataSample=unique(DataGI,by="id_carre_annee")
DataS2=subset(DataSample,select=c("id_carre_annee","foret","urbain","eau"
                                  ,"prairies"))



NCARRE=length(unique(DataSTOC$carre))

Occ=vector()
AbSiP=vector()
SpecForet=vector()
SpecUrbain=vector()
SpecEau=vector()
SpecPrairies=vector()
for (i in 1:length(unique(DataSTOC$scientific_name)))
{
  print(unique(DataSTOC$scientific_name)[i])
  Datai=subset(DataSTOC
               ,DataSTOC$scientific_name==unique(DataSTOC$scientific_name)[i])
  Nloc=length(unique(Datai$carre))
  occi=Nloc/NCARRE
  ASPi=mean(Datai$abondance_brut)
  Occ=c(Occ,occi)
  AbSiP=c(AbSiP,ASPi)
  DataSi=merge(Datai,DataS2,by="id_carre_annee")
  #mtot=glmmTMB(abondance_brut~foret+eau+urbain+prairies,data=DataSi,family="nbinom2")
  m1=glmmTMB(abondance_brut~foret,data=DataSi,family="nbinom2")
  SpecForet=c(SpecForet,m1$fit$par[2])    
  m1=glmmTMB(abondance_brut~eau,data=DataSi,family="nbinom2")
  SpecEau=c(SpecEau,m1$fit$par[2])    
  m1=glmmTMB(abondance_brut~urbain,data=DataSi,family="nbinom2")
  SpecUrbain=c(SpecUrbain,m1$fit$par[2]) 
  m1=glmmTMB(abondance_brut~prairies,data=DataSi,family="nbinom2")
  SpecPrairies=c(SpecPrairies,m1$fit$par[2]) 
  
}

DataSummary=data.frame(Espece=unique(DataSTOC$scientific_name)
                        ,Occurrence=round(Occ,4)*100
                       ,AbondanceM_SiPresence=round(AbSiP,1)
                       ,SpecialisationForet=round(SpecForet,2)
                       ,SpecialisationAquatique=round(SpecEau,2)
                       ,SpecialisationUrbain=round(SpecUrbain,2)
                       ,SpecialisationPrairies=round(SpecPrairies,2))
DataSummary$AbondanceMoyenne=DataSummary$Occurrence/100*DataSummary$AbondanceM_SiP


DataSummary=DataSummary[order(DataSummary$Occurrence,decreasing=T),]

test=match(DataSummary$Espece,Taxref$LB_NOM)
subset(DataSummary$Espece,is.na(test))
DataSummary$Nom_francais=Taxref$NOM_VERN[test]

test=match(DataSummary$Espece,CorrCodeSTOC$Name)
test2=match(CorrCodeSTOC$Code,TrendFRfiable$code_espece)
DataSummary$Tendance_France_2001_2019=round(
  TrendFRfiable$pourcentage_variation[test2[test]])

Snames=tstrsplit(TrendEU$Species,split=" ")[1:2]
TrendEU$SpeciesClean=paste(Snames[[1]],Snames[[2]])
TrendEU$SpeciesClean[TrendEU$SpeciesClean=="Saxicola torquatus"]="Saxicola rubicola"
TrendEU$SpeciesClean[TrendEU$SpeciesClean=="Leiopicus medius"]="Dendrocopos medius"
TrendEU$SpeciesClean[TrendEU$SpeciesClean=="Larus ridibundus"]="Chroicocephalus ridibundus"



test=match(DataSummary$Espece,TrendEU$SpeciesClean)
DataSummary$Tendance_Europe_1980_2016=TrendEU$`Long-term Trend (%)a)`[test]
DataSummary$Tendance_Europe_2007_2016=TrendEU$`Ten-year Trend (%)a)`[test]





BDCstatus$LB_NOM[BDCstatus$LB_NOM=="Saxicola torquata"]="Saxicola rubicola"
BDCstatus$LB_NOM[BDCstatus$LB_NOM=="Saxicola torquatus"]="Saxicola rubicola"
BDCstatus$LB_NOM[BDCstatus$LB_NOM=="Larus ridibundus"]="Chroicocephalus ridibundus"
BDCstatus$LB_NOM[BDCstatus$LB_NOM=="Larus melanocephalus"]="Ichthyaetus melanocephalus"
BDCstatus$LB_NOM[BDCstatus$LB_NOM=="Leiopicus melanocephalus"]="Dendrocopos medius"
BDCstatus$LB_NOM[BDCstatus$LB_NOM=="Mareca strepera"]="Anas strepera"
BDCstatus$LB_NOM[BDCstatus$LB_NOM=="Cyanecula svecica"]="Luscinia svecica"
BDCstatus$LB_NOM[BDCstatus$LB_NOM=="Spatula clypeata"]="Anas clypeata"
BDCstatus$LB_NOM[BDCstatus$LB_NOM=="Spatula querquedula"]="Anas querquedula"
BDCstatus$LB_NOM[BDCstatus$LB_NOM=="Tachymarptis melba"]="Apus melba"
BDCstatus$LB_NOM[BDCstatus$LB_NOM=="Mareca penelope"]="Anas penelope"
BDCstatus$LB_NOM[BDCstatus$LB_NOM=="Larus genei"]="Chroicocephalus genei"
BDCstatus$LB_NOM[BDCstatus$LB_NOM=="Spilopelia senegalensis"]="Streptopelia senegalensis"
BDCstatus$LB_NOM[BDCstatus$LB_NOM=="Hydrobates leucorhous"]="Oceanodroma leucorhoa"
BDCstatus$LB_NOM[BDCstatus$LB_NOM=="Carduelis chloris"]="Chloris chloris"
BDCstatus$LB_NOM[BDCstatus$LB_NOM=="Parus caeruleus"]="Cyanistes caeruleus"
BDCstatus$LB_NOM[BDCstatus$LB_NOM=="Carduelis cannabina"]="Linaria cannabina"
BDCstatus$LB_NOM[BDCstatus$LB_NOM=="Parus palustris"]="Poecile palustris"
BDCstatus$LB_NOM[BDCstatus$LB_NOM=="Dendrocopos minor"]="Dryobates minor"
BDCstatus$LB_NOM[BDCstatus$LB_NOM=="Parus cristatus"]="Lophophanes cristatus"
BDCstatus$LB_NOM[BDCstatus$LB_NOM=="Parus ater"]="Periparus ater"
BDCstatus$LB_NOM[BDCstatus$LB_NOM=="Parus montanus"]="Poecile montanus"
BDCstatus$LB_NOM[BDCstatus$LB_NOM=="Carduelis spinus"]="Spinus spinus"
BDCstatus$LB_NOM[BDCstatus$LB_NOM=="Parus cristatus"]="Lophophanes cristatus"
BDCstatus$LB_NOM[BDCstatus$LB_NOM=="Carduelis flammea"]="Acanthis flammea"
BDCstatus$LB_NOM[BDCstatus$LB_NOM=="Tetrao tetrix"]="Lyrurus tetrix"
BDCstatus$LB_NOM[BDCstatus$LB_NOM=="Lagopus mutus"]="Lagopus muta"
BDCstatus$LB_NOM[BDCstatus$LB_NOM=="Tetrao tetrix"]="Lyrurus tetrix"
BDCstatus$LB_NOM[BDCstatus$LB_NOM=="Tetrao tetrix"]="Lyrurus tetrix"


DataLRworld=subset(BDCstatus,BDCstatus$LB_TYPE_STATUT=="Liste rouge mondiale")
test=match(DataSummary$Espece,DataLRworld$LB_NOM)
subset(DataSummary$Espece,is.na(test))
#subset(DataLRworld$LB_NOM,grepl("leucor",DataLRworld$LB_NOM))
DataSummary$LR_Monde=DataLRworld$CODE_STATUT[test]

RedListEU$Species=paste(RedListEU$taxonomicRankGenus,RedListEU$taxonomicRankSpecies)
RedListEU$Species[RedListEU$Species=="Saxicola torquatus"]="Saxicola rubicola"
RedListEU$Species[RedListEU$Species=="Miliaria calandra"]="Emberiza calandra"
RedListEU$Species[RedListEU$Species=="Parus palustris"]="Poecile palustris"
RedListEU$Species[RedListEU$Species=="Parus montanus"]="Poecile montanus"
RedListEU$Species[RedListEU$Species=="Dendrocopos minor"]="Dryobates minor"
RedListEU$Species[RedListEU$Species=="Parus cristatus"]="Lophophanes cristatus"
RedListEU$Species[RedListEU$Species=="Parus caeruleus"]="Cyanistes caeruleus"
RedListEU$Species[RedListEU$Species=="Parus ater"]="Periparus ater"
RedListEU$Species[RedListEU$Species=="Leiopicus medius"]="Dendrocopos medius"
RedListEU$Species[RedListEU$Species=="Larus ridibundus"]="Chroicocephalus ridibundus"
RedListEU$Species[RedListEU$Species=="Carduelis chloris"]="Chloris chloris"
RedListEU$Species[RedListEU$Species=="Carduelis cannabina"]="Linaria cannabina"
RedListEU$Species[RedListEU$Species=="Carduelis spinus"]="Spinus spinus"
RedListEU$Species[RedListEU$Species=="Spatula clypeata"]="Anas clypeata"
RedListEU$Species[RedListEU$Species=="Larus melanocephalus"]="Ichthyaetus melanocephalus"
RedListEU$Species[RedListEU$Species=="Hirundo rupestris"]="Ptyonoprogne rupestris"
RedListEU$Species[RedListEU$Species=="Mareca strepera"]="Anas strepera"
RedListEU$Species[RedListEU$Species=="Carduelis flammea"]="Acanthis flammea"
RedListEU$Species[RedListEU$Species=="Spatula querquedula"]="Anas querquedula"
RedListEU$Species[RedListEU$Species=="Tachymarptis melba"]="Apus melba"
RedListEU$Species[RedListEU$Species=="Hirundo daurica"]="Cecropis daurica"

DataLR_EU=subset(BDCstatus,BDCstatus$LB_TYPE_STATUT=="Liste rouge européenne")
test=match(DataSummary$Espece,DataLR_EU$LB_NOM)
subset(DataSummary$Espece,is.na(test))
subset(DataLR_EU$LB_NOM,grepl("Phylloscopus",DataLR_EU$LB_NOM))
#DataSummary$LR_Europe=DataLR_Europe$CODE_STATUT[test]

test=match(DataSummary$Espece,RedListEU$Species)
subset(DataSummary$Espece,is.na(test))
DataSummary$LR_Europe=RedListEU$euRegionalRedListCategory[test]

DataLR_FR=subset(BDCstatus
                 ,(BDCstatus$LB_TYPE_STATUT=="Liste rouge nationale")&
                   (BDCstatus$CD_DOC==165208))
test=match(DataSummary$Espece,DataLR_FR$LB_NOM)
subset(DataSummary$Espece,is.na(test))
subset(DataLR_FR$LB_NOM,grepl("mut",DataLR_FR$LB_NOM))

DataSummary$LR_France=DataLR_FR$CODE_STATUT[test]

DataDO=subset(BDCstatus,BDCstatus$LB_TYPE_STATUT=="Directive Oiseaux")
test=match(DataSummary$Espece,DataDO$LB_NOM)
DataSummary$Annexe1_Directive_Oiseaux=as.numeric(
  DataDO$CODE_STATUT[test]=="CDO1")
DataSummary$Annexe1_Directive_Oiseaux[is.na(DataSummary$Annexe1_Directive_Oiseaux)]=0

fwrite(DataSummary,"DataSummaryOiseaux.csv",sep=";")
