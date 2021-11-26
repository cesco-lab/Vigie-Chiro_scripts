library(data.table)
#A EDITER en fonction de la localisation des fichiers .tc
#TC_path="C:/Users/Yves Bas/Documents/txt"
#get arguments from the command line
args <- commandArgs(trailingOnly = TRUE)
#uncomment the following line if you prefer to do not use R in command line
if(length(args)==0){
  args="./www/VeoliaPredictions2008"
  #q()
}
TC_path=args[1]
Ref_coef=fread("./vrac_md_dell2021/RefPF_Coef (5).csv")
DataCalibr=fread("./vrac_md_dell2021/DataCalibr.csv")

alogit <- function(x) 
{
  exp(x)/(1+exp(x))
}

#get the predictions files list
idlist=list.files(TC_path,pattern=".csv$",full.names=T,recursive=T)
print(length(idlist))
my.data <- list()
for(f in 1:length(idlist)) {
  my.data[[f]] <- fread(idlist[[f]])
  Species=tstrsplit(basename(idlist[[f]]),split="_")[[1]]
  Species=gsub("VC50","",Species)
  my.data[[f]]$species=Species
}
Idtot=as.data.frame(rbindlist(my.data))

Index0=c(1:nrow(Idtot))
test=subset(Idtot,Index0%%6==1)

test=match(Ref_coef$Code,levels(as.factor(Idtot$species)))

Sub_Ref_coef=subset(Ref_coef,is.na(test)==F)

test2=match(levels(as.factor(Idtot$species)),Sub_Ref_coef$Code)

fwrite(Sub_Ref_coef,"./log0.csv",sep=";")

if (exists("DataInd")){rm(DataInd)}
ListSp=vector()  
AbBrute=vector()
for (i in 1:nlevels(as.factor(Idtot$species)))
{
  if((!is.na(test2[i]))&(Sub_Ref_coef$Moy[test2[i]]!=0)){
    Datasub=subset(Idtot,Idtot$species==levels(as.factor(Idtot$species))[i])
    
    Ind_S=Datasub$pred/Sub_Ref_coef$Moy[test2[i]]*Sub_Ref_coef$SSIZ[test2[i]]
    Ind_L=-Datasub$pred/Sub_Ref_coef$Moy[test2[i]]*Sub_Ref_coef$ReponseLum[test2[i]]
    Ind_A=Datasub$pred/Sub_Ref_coef$Moy[test2[i]]*Sub_Ref_coef$ReponseArb[test2[i]]
    Ind_E=Datasub$pred/Sub_Ref_coef$Moy[test2[i]]*Sub_Ref_coef$ReponseEau[test2[i]]
    AbStand=Datasub$pred/Sub_Ref_coef$Moy[test2[i]]
    #ListSp=c(ListSp,levels(as.factor(Idtot$species))[i])
    Sp=rep(levels(as.factor(Idtot$species))[i],length(Ind_S))
    AbBrute=c(AbBrute,Datasub$pred)
    if (exists("DataInd")){DataInd=rbind(DataInd,cbind(Ind_S,Ind_L,Ind_A,Ind_E,AbStand,Sp))}else{
      DataInd=cbind(Ind_S,Ind_L,Ind_A,Ind_E,AbStand,Sp)}
    print(paste(levels(as.factor(Idtot$species))[i]))
          #,Datasub$pred,Ind_S,Ind_L,Ind_A,Ind_E,AbStand))
  }
}
fwrite(DataInd,"./log1.csv",sep=";")


DataInd[is.na(DataInd)]=0
DataInd=as.data.frame(DataInd)
#DataInd=subset(DataInd,(DataInd$AbStand!=Inf)&(DataInd$AbStand>0))

fwrite(DataInd,"./log2.csv",sep=";")
fwrite(DataCalibr,"./log3.csv",sep=";")

Index=c(1:nrow(DataInd))

DataInd$Ind_S=as.numeric(DataInd$Ind_S)
DataInd$Ind_E=as.numeric(DataInd$Ind_E)
DataInd$Ind_L=as.numeric(DataInd$Ind_L)
DataInd$Ind_A=as.numeric(DataInd$Ind_A)
DataInd$AbStand=as.numeric(DataInd$AbStand)

Indicateur_Spec=vector()
Indicateur_Lum=vector()
Indicateur_Arb=vector()
Indicateur_Eau=vector()
for (j in 1:nrow(my.data[[1]]))
{
  if(j==nrow(my.data[[1]])){j=0}
  DataSitej=subset(DataInd,Index%%nrow(my.data[[1]])==j)

Indicateur_Specj=alogit(((sum(DataSitej[,1])/sum(DataSitej[,5]))-DataCalibr[1,1])/DataCalibr[1,2])*20
Indicateur_Lumj=alogit((log(sum(DataSitej[,2])+0.1)-DataCalibr[2,1])/DataCalibr[2,2])*20
Indicateur_Arbj=alogit((log(sum(DataSitej[,3])+0.5)-DataCalibr[3,1])/DataCalibr[3,2])*20
Indicateur_Eauj=alogit((log(sum(DataSitej[,3])+2)-DataCalibr[4,1])/DataCalibr[4,2])*20


Indicateur_Spec=c(Indicateur_Spec,Indicateur_Specj)
Indicateur_Lum=c(Indicateur_Lum,Indicateur_Lumj)
Indicateur_Arb=c(Indicateur_Arb,Indicateur_Arbj)
Indicateur_Eau=c(Indicateur_Eau,Indicateur_Eauj)
}
Indicateurs=data.table(cbind(Indicateur_Spec,Indicateur_Lum,Indicateur_Arb,Indicateur_Eau))


#A EDITER si l'on veut changer la destination des données
fwrite(Indicateurs,paste(TC_path, "./Indicateurs.csv", "", sep=""),row.names=F)

