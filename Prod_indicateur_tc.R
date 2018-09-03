#A EDITER en fonction de la localisation des fichiers .tc
TC_path="C:/Users/yves/Downloads/nuit_du_15-09-2015_au_16-09-2015"

library(data.table)
GroupEN=fread("Groupe_Especes_Nice.csv")
Ref_coef=fread("RefPF_Coef.csv")

alogit <- function(x) 
{
  exp(x)/(1+exp(x))
}

#get the .tc files list
idlist=list.files(TC_path,pattern=".tc$",full.names=T,recursive=T)
my.data <- list()
for(f in 1:length(idlist)) {
  my.data[[f]] <- fread(idlist[[f]])
}
Idtot=as.data.frame(rbindlist(my.data))

SpMax<-max.col(Idtot[,2:(ncol(Idtot)-6)],ties.method = "first")
SpMax2=cbind(Filename=as.character(Idtot[,1]),Id=colnames(Idtot)[SpMax+1],numsp=SpMax)

SpMax3=merge(SpMax2,GroupEN,by.x="Id",by.y="code_Tadarida")

test=match(Ref_coef$Espece,levels(as.factor(SpMax3$Groupe_Nice)))

Sub_Ref_coef=subset(Ref_coef,is.na(test)==F)

if(nrow(SpMax3)==0)
{
  print("ATTENTION : aucunes chauves-souris contactées !")
  Indicateurs=c(0,0,0)
}else{

  if (exists("DataInd")){rm(DataInd)}
  
ListSp=vector()  
AbBrute=vector()
for (i in 1:nlevels(as.factor(SpMax3$Groupe_Nice)))
{
  Datasub=subset(SpMax3,SpMax3$Groupe_Nice==levels(as.factor(SpMax3$Groupe_Nice))[i])
  Ind_S=log10(nrow(Datasub)+1)/Sub_Ref_coef$Moy[i]*Sub_Ref_coef$Coef_Spec[i]
  Ind_L=log10(nrow(Datasub)+1)/Sub_Ref_coef$Moy[i]*Sub_Ref_coef$Coef_Lum[i]
  Ind_A=log10(nrow(Datasub)+1)/Sub_Ref_coef$Moy[i]*Sub_Ref_coef$Coef_Arb[i]
  AbStand=log10(nrow(Datasub)+1)/Sub_Ref_coef$Moy[i]
  ListSp=c(ListSp,levels(as.factor(SpMax3$Groupe_Nice))[i])
  AbBrute=c(AbBrute,nrow(Datasub))
  if (exists("DataInd")){DataInd=rbind(DataInd,cbind(Ind_S,Ind_L,Ind_A,AbStand))}else{DataInd=cbind(Ind_S,Ind_L,Ind_A,AbStand)}
}


  Indicateur_Spec=alogit((sum(DataInd[,1])/sum(DataInd[,4]))-3.7)*20
  Indicateur_Lum=alogit((log(sum(DataInd[,2])+20)-3.4)*2)*20
  Indicateur_Arb=alogit((log(sum(DataInd[,3])+20)-3.5)*3)*20
  
 BilanEspeces=cbind(ListSp,AbBrute) 
  
Indicateurs=c(Indicateur_Spec,Indicateur_Lum,Indicateur_Arb)
}
#A EDITER si l'on veut changer la destination des données
write.csv(Indicateurs,"Indicateurs.csv",row.names=F)
write.csv(BilanEspeces,"BilanEspeces.csv",row.names=F)
