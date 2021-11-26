library(data.table)

DataGCLR=fread("./chiros/BDD_GCLR/20160108GCLR_XPORT_toutes_donnees.csv")

DataGCLR$MOIS=month(as.Date(DataGCLR$date_debut))
barplot(table(DataGCLR$MOIS))
DataGite=subset(DataGCLR,DataGCLR$contact=="Vu")

PhenoGite=matrix(nrow=length(unique(DataGite$nom_latin)),ncol=12)
for (i in 1:length(unique(DataGite$nom_latin)))
{
  print(unique(DataGite$nom_latin)[i])
  DataSp=subset(DataGite,DataGite$nom_latin==unique(DataGite$nom_latin)[i])
  DataSp1=subset(DataSp,DataSp$denomb_min==1)
  DataSp2=subset(DataSp,DataSp$denomb_min>1)
  if(nrow(DataSp1)>0)
  {
  Nb1=aggregate(DataSp1$denomb_min,by=list(DataSp1$MOIS),FUN=sum)
  if(nrow(DataSp2)>0)
  {
  Nb2=aggregate(DataSp2$denomb_min,by=list(DataSp2$MOIS),FUN=sum)
  }else{
    Nb2=Nb1
    Nb2$x=0
  }
  for (j in 1:12)
  {
  Sum1=sum(subset(Nb1$x,(Nb1$Group.1<j+2)&(Nb1$Group.1>j-2))) 
  Sum2=sum(subset(Nb2$x,(Nb2$Group.1<j+2)&(Nb2$Group.1>j-2)))
  if(is.na(Sum1)){Sum1=0}
  if(is.na(Sum2)){Sum2=0}
  PhenoGite[i,j]=Sum1/(Sum2+Sum1)
  }
  }else{
    PhenoGite[i,]=0
  }
} 
PhenoGite=as.data.frame(PhenoGite)
PhenoGite$Espece=unique(DataGite$nom_latin)

fwrite(PhenoGite,"PhenoGiteSum.csv",sep=";")
