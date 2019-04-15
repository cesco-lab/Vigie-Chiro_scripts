library(data.table)
### A FAIRE :
SeuilFiable="Seuil90"

SpNuit=fread("SpNuit2_Seuil90_DataLP_PF_exportTot.csv")

hist(SpNuit$decalage_debut_coucher,xlim=c(-5000,10000),breaks=200)
hist(SpNuit$decalage_fin_lever,xlim=c(-5000,10000),breaks=200)

SpNuit=subset(SpNuit,(SpNuit$decalage_debut_coucher<0)&
                (SpNuit$decalage_fin_lever<0))

DecMin=mapply(min,SpNuit$min_decalage_lever,SpNuit$min_decalage_coucher)
SpNuit[,DecMin:=DecMin]
test=subset(SpNuit,SpNuit$espece=="Rhifer")
plot(test$DecMin,test$nb_contacts,xlim=c(-6000,6000),log="y")

NbNuit=nlevels(as.factor(paste(SpNuit$participation,SpNuit$Nuit,SpNuit$num_micro)))

i=1
Q25=vector()
Q75=vector()
Q98=vector()
DM25=vector()
DM10=vector()
DM02=vector()
nbocc=vector()
MoySiP=vector()
EtypSiP=vector()
MoyG=vector()
EtypG=vector()
for (i in 1:nlevels(as.factor(SpNuit$espece)))
{
  Datasub=subset(SpNuit,SpNuit$espece==levels(as.factor(SpNuit$espece))[i])
  #calcul des quantiles d'activité
  Q25=c(Q25,quantile(Datasub$nb_contacts,0.25))
  Q75=c(Q75,quantile(Datasub$nb_contacts,0.75))
  Q98=c(Q98,quantile(Datasub$nb_contacts,0.98))
  print(paste(i,"/",nlevels(as.factor(SpNuit$espece))))
  nbocc=c(nbocc,nrow(Datasub))
  MoySiP=c(MoySiP,mean(Datasub$nb_contacts))
  EtypSiP=c(EtypSiP,sd(Datasub$nb_contacts))
  MoyG=c(MoyG,mean(c(Datasub$nb_contacts,rep(0,NbNuit-nrow(Datasub)))))
  EtypG=c(EtypG,sd(c(Datasub$nb_contacts,rep(0,NbNuit-nrow(Datasub)))))
  #filtrage des problèmes d'heures
  Datasub2=subset(Datasub,Datasub$DecMin>(-1800))
  #calcul des quantiles de décalage de temps minimum avec coucher-lever
  DM25=c(DM25,quantile(Datasub2$DecMin,0.25))
  DM10=c(DM10,quantile(Datasub2$DecMin,0.10))
  DM02=c(DM02,quantile(Datasub2$DecMin,0.02))
  
  
  }

Ref=cbind(Espece=levels(as.factor(SpNuit$espece)),MoyG,EtypG,MoySiP,EtypSiP,Q25,Q75,Q98,nbocc
          ,DM25,DM10,DM02)  
fwrite(data.frame(Ref),paste0("refPF_",SeuilFiable,".csv"),sep=";")
