library(data.table)

DataTot=fread("C:/Users/yvesb/Downloads/DataTot.csv",sep=";")
DataRawCarre=fread("DataRawCarre.csv")
DataForTrends=fread("C:/Users/yvesb/Downloads/sorting_custom.csv")
DirTot="C:/Users/yvesb/Documents/www/OccDep"
DMTot=fread("C:/Users/yvesb/Downloads/DMTot.csv")

LDT=list.files(DirTot,pattern="Data",full.names=T)

LDT=subset(LDT,!grepl("Sensitive",LDT))
LDT=subset(LDT,!grepl("Confidential",LDT))
LDT=subset(LDT,grepl(".csv",LDT))


LDTlist=list()
for (h in 1:length(LDT))
{
  LDTlist[[h]]=fread(LDT[h])
  
}
#DataTot=rbindlist(LDTlist)


#DataSite=subset(DataTot,DataTot$numero_carre==11224)
#DataPoint=subset(DataSite,DataSite$point=="B1")
#table(DataPoint$Nuit)


CarreOrder=DataRawCarre[order(DataRawCarre$Score,decreasing=T),]

for (i in 2:5)
{
  print(CarreOrder[i,])
  Datai=subset(DataTot,(DataTot$numero_carre==as.numeric(gsub("Vigiechiro - Point Fixe-","",CarreOrder$Carre[i])))&
                 (DataTot$point==CarreOrder$Point[i]))
  print(table(Datai$annee))
  Datacsi=subset(Datai,Datai$groupe=="Chauve-souris")
  Trendsi=subset(DataForTrends,(DataForTrends$site==CarreOrder$Carre[i])&(DataForTrends$point==CarreOrder$Point[i])
                 &(DataForTrends$espece=="Eptser"))
  print(table(Trendsi$year))
  
  
  
  #print(table(Datacsi$annee))
  for (j in 1:length(unique(Datacsi$Nuit))){
    Datacsij=subset(Datacsi,Datacsi$Nuit==unique(Datacsi$Nuit)[j])
    
    Datacsi_short1=subset(Datacsij,select=c("Nuit","enregistreur","type_micro","hauteur_micro","gite","nuit_complete"
                                           ,"observateur_rice","probleme_micro"))
    
  print(Datacsi_short1[1,])  
  DMij=subset(DMTot,DMTot$Group.1==Datacsij$participation[1])
  print(DMij)
    
  }
  
  Datacsi_short2=subset(Datacsi,select=c("espece","Nuit","num_micro","nb_contacts_nd","nb_contacts","score_max"
                                         ))[order(Datacsi_short2$espece),]
  Datacsi_short2=Datacsi_short2[order(Datacsi_short2$espece),]
  print(Datacsi_short2)
  
  
}
