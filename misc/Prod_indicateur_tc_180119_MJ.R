#A EDITER en fonction de la localisation des fichiers .tc
##TC_path="C:/Users/mjeanty/Documents/FileZilla/1014/2021"
TC_path="C:/Users/yvesb/Documents/www/622dbfd75b54ea479697aaba"

#######################################################################################
#get arguments from the command line
args <- commandArgs(trailingOnly = TRUE)
#uncomment the following line if you prefer to do not use R in command line
#args="C:/Users/yves/Downloads/TC - nuit_du_13-08-2017_au_14-08-2017"
#args="C:/Users/mjeanty/Documents/FileZilla/1002/2021/nuit_du_01-09-2021_au_02-09-2021"
##if(length(args)==0){
##  print("usage: Rscript TadaridaC.r <directory>")
##  q()
##}
##TC_path=args[1]
#######################################################################################



library(data.table)

Ref_coef=fread("C:/Users/yvesb/Documents/vrac_md_dell2021/RefPF_coef (5).csv")
DataCalibr=fread("C:/Users/yvesb/Documents/vrac_md_dell2021/DataCalibr.csv")

alogit <- function(x) 
{
  exp(x)/(1+exp(x))
}

##TC_path_nuit=paste0(TC_path,"/",list.files(TC_path))
ListNights=list.dirs(TC_path,recursive=F)

##for (i in 1:length(TC_path_nuit)){
##  TC_path=TC_path_nuit[i]
##  print(paste("TC-path de la nuit est",TC_path))

##for (i in 1:length(ListNights)){
##    TC_path_nuit=ListNights[i]
##    print(paste("TC-path de la nuit est",TC_path_nuit))


#get the .tc files list
##idlist=list.files(TC_path_nuit,pattern=".tc$",full.names=T,recursive=T)
idlist=list.files(TC_path,pattern=".tc$",full.names=T,recursive=T)
print(length(idlist))
my.data <- list()
for(f in 1:length(idlist)) {
  my.data[[f]] <- fread(idlist[[f]])
}
Idtot=as.data.frame(rbindlist(my.data))

##SpMax<-max.col(Idtot[,2:(ncol(Idtot)-6)],ties.method = "first")
SpMax<-max.col(Idtot[,40:(ncol(Idtot)-1)],ties.method = "first")
##ScoreMax=apply(Idtot[,2:(ncol(Idtot)-6)],MARGIN=1,FUN=max)
ScoreMax=apply(Idtot[,40:(ncol(Idtot)-1)],MARGIN=1,FUN=max)
SpMax2=as.data.frame(cbind(Filename=as.character(Idtot[,1]),Id=colnames(Idtot)[SpMax+39],numsp=SpMax,IndConf=ScoreMax))
table(SpMax2)

SpMax3=merge(SpMax2,Ref_coef,by.x="Id",by.y="Code")
SpMax3$IndConf=as.numeric(as.character(SpMax3$IndConf))

test=match(Ref_coef$Code,levels(as.factor(SpMax3$Id)))

Sub_Ref_coef=subset(Ref_coef,is.na(test)==F)
if(nrow(SpMax3)==0)
{
  print("ATTENTION : aucunes chauves-souris contactées !")
  Indicateurs=c(0,0,0)
}else{
  
  test2=match(levels(as.factor(SpMax3$Id)),Sub_Ref_coef$Code)
  
  
  
  if (exists("DataInd")){rm(DataInd)}
  ListSp=vector()  
  AbBrute=vector()
  for (i in 1:nlevels(as.factor(SpMax3$Id)))
  {
    Datasub=subset(SpMax3,(SpMax3$Id==levels(as.factor(SpMax3$Id))[i])
                   &(as.numeric(as.character(SpMax3$IndConf))>0.5))
    Ind_S=log10(nrow(Datasub)+1)/Sub_Ref_coef$Moy[test2[i]]*Sub_Ref_coef$SSIZ[test2[i]]
    Ind_L=-log10(nrow(Datasub)+1)/Sub_Ref_coef$Moy[test2[i]]*Sub_Ref_coef$ReponseLum[test2[i]]
    Ind_A=log10(nrow(Datasub)+1)/Sub_Ref_coef$Moy[test2[i]]*Sub_Ref_coef$ReponseArb[test2[i]]
    Ind_E=log10(nrow(Datasub)+1)/Sub_Ref_coef$Moy[test2[i]]*Sub_Ref_coef$ReponseEau[test2[i]]
    
    
    AbStand=log10(nrow(Datasub)+1)/Sub_Ref_coef$Moy[test2[i]]
    ListSp=c(ListSp,levels(as.factor(SpMax3$Id))[i])
    AbBrute=c(AbBrute,nrow(Datasub))
    if (exists("DataInd")){DataInd=rbind(DataInd,cbind(Ind_S,Ind_L,Ind_A,Ind_E,AbStand))}else{DataInd=cbind(Ind_S,Ind_L,Ind_A,Ind_E,AbStand)}
    print(paste(levels(as.factor(SpMax3$Id))[i],nrow(Datasub),Ind_S,Ind_L,Ind_A,Ind_E,AbStand))
  }
  
  DataInd[is.na(DataInd)]=0
  DataInd=as.data.frame(DataInd)
  DataInd=subset(DataInd,(DataInd$AbStand!=Inf)&(DataInd$AbStand>0))
  
  
  
  Indicateur_Spec=alogit(((sum(DataInd[,1])/sum(DataInd[,5]))-DataCalibr[1,1])/DataCalibr[1,2])*20
  Indicateur_Lum=alogit((log(sum(DataInd[,2])+0.1)-DataCalibr[2,1])/DataCalibr[2,2])*20
  Indicateur_Arb=alogit((log(sum(DataInd[,3])+0.5)-DataCalibr[3,1])/DataCalibr[3,2])*20
  Indicateur_Eau=alogit((log(sum(DataInd[,3])+2)-DataCalibr[4,1])/DataCalibr[4,2])*20
  
  
  ##BilanEspeces0=as.data.frame(cbind(ListSp,AbBrute)) 
  ##BilanEspeces1=merge(BilanEspeces0,Ref_coef,by.x="ListSp",by.y="Code",all.y=T)
  ##BilanEspeces1$AbBrute=as.character(BilanEspeces1$AbBrute)
  ##BilanEspeces1$AbBrute[is.na(BilanEspeces1$AbBrute)]=0
  ##BilanEspeces=as.data.frame(cbind(Groupe=BilanEspeces1$Groupe,Espece=BilanEspeces1$Espece,Nb_Contacts=BilanEspeces1$AbBrute))
  ##BilanEspeces=BilanEspeces[order(as.character(BilanEspeces$Groupe),BilanEspeces$Espece),]
  
  Indicateurs=c(Indicateur_Spec,Indicateur_Lum,Indicateur_Arb,Indicateur_Eau)
  
  #A EDITER si l'on veut changer la destination des données
  ##write.csv(Indicateurs,paste(TC_path_nuit, "/Indicateurs.csv", "", sep=""),row.names=F)
  write.csv(Indicateurs,paste(TC_path, "/Indicateurs.csv", "", sep=""),row.names=F)
  
  ##write.csv(BilanEspeces,paste(TC_path, "/BilanEspeces.csv", "", sep=""),row.names=F)
}
}
