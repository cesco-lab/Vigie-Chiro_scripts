library(data.table)

#table "données"
DataTot=fread("C:/wamp64/www/export180227.txt")
Sys.time()
colnames(DataTot)[10]="temps_fin"

Doublons=duplicated(DataTot,by=c("donnee","espece","temps_debut","temps_fin"))

DataDoublons=subset(DataTot,Doublons==T)

PartDoublons=subset(levels(as.factor(DataTot$participation))
                    ,levels(as.factor(DataTot$participation)) %in% DataDoublons$participation)



####A FINIR####
for (i in 1:length(PartDoublons))
{
    
  
  }