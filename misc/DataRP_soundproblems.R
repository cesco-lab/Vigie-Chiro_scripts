library(data.table)
DataRP=fread("DataLP_RP.csv")
SpeciesList=fread("SpeciesList.csv")
Particip=fread("C:/wamp64/www/p_export.csv")


DataRPS=merge(DataRP,SpeciesList,by.x="espece",by.y="Esp",all.x=T)

DataRPS$dur=DataRPS$temps_fin-DataRPS$temps_debut

table(subset(DataRPS$espece,is.na(DataRPS$Nesp)))
DataRPS_bat=subset(DataRPS,DataRPS$Group=="bat")
DataRPS_bs=subset(DataRPS,DataRPS$Group=="bush-cricket")
DataRPS_noise=subset(DataRPS,DataRPS$Group=="noise")

#extract relevant variables
MaxDur_bat=aggregate(DataRPS_bat$dur
                     ,by=c(list(DataRPS_bat$participation),list(DataRPS_bat$Datamicro))
                     ,FUN=max)
MaxDur_bs=aggregate(DataRPS_bs$dur
                     ,by=c(list(DataRPS_bs$participation),list(DataRPS_bs$Datamicro))
                     ,FUN=max)
MaxDur_noise=aggregate(DataRPS_noise$dur
                     ,by=c(list(DataRPS_noise$participation),list(DataRPS_noise$Datamicro))
                     ,FUN=max)

MoyDur_bat=aggregate(DataRPS_bat$dur
                     ,by=c(list(DataRPS_bat$participation),list(DataRPS_bat$Datamicro))
                     ,FUN=mean)
MoyDur_bs=aggregate(DataRPS_bs$dur
                    ,by=c(list(DataRPS_bs$participation),list(DataRPS_bs$Datamicro))
                    ,FUN=mean)
MoyDur_noise=aggregate(DataRPS_noise$dur
                       ,by=c(list(DataRPS_noise$participation),list(DataRPS_noise$Datamicro))
                       ,FUN=mean)

Q50Dur_bat=aggregate(DataRPS_bat$dur
                     ,by=c(list(DataRPS_bat$participation),list(DataRPS_bat$Datamicro))
                     ,FUN=function(x) quantile(x,0.5))
Q50Dur_bs=aggregate(DataRPS_bs$dur
                    ,by=c(list(DataRPS_bs$participation),list(DataRPS_bs$Datamicro))
                    ,FUN=function(x) quantile(x,0.5))
Q50Dur_noise=aggregate(DataRPS_noise$dur
                       ,by=c(list(DataRPS_noise$participation),list(DataRPS_noise$Datamicro))
                       ,FUN=function(x) quantile(x,0.5))


Q90Dur_bat=aggregate(DataRPS_bat$dur
                     ,by=c(list(DataRPS_bat$participation),list(DataRPS_bat$Datamicro))
                     ,FUN=function(x) quantile(x,0.5))
Q90Dur_bs=aggregate(DataRPS_bs$dur
                    ,by=c(list(DataRPS_bs$participation),list(DataRPS_bs$Datamicro))
                    ,FUN=function(x) quantile(x,0.5))
Q90Dur_noise=aggregate(DataRPS_noise$dur
                       ,by=c(list(DataRPS_noise$participation),list(DataRPS_noise$Datamicro))
                       ,FUN=function(x) quantile(x,0.5))

Maxprobabilite_bat=aggregate(DataRPS_bat$probabilite
                     ,by=c(list(DataRPS_bat$participation),list(DataRPS_bat$Datamicro))
                     ,FUN=max)
Maxprobabilite_bs=aggregate(DataRPS_bs$probabilite
                    ,by=c(list(DataRPS_bs$participation),list(DataRPS_bs$Datamicro))
                    ,FUN=max)
Maxprobabilite_noise=aggregate(DataRPS_noise$probabilite
                       ,by=c(list(DataRPS_noise$participation),list(DataRPS_noise$Datamicro))
                       ,FUN=max)

Moyprobabilite_bat=aggregate(DataRPS_bat$probabilite
                     ,by=c(list(DataRPS_bat$participation),list(DataRPS_bat$Datamicro))
                     ,FUN=mean)
Moyprobabilite_bs=aggregate(DataRPS_bs$probabilite
                    ,by=c(list(DataRPS_bs$participation),list(DataRPS_bs$Datamicro))
                    ,FUN=mean)
Moyprobabilite_noise=aggregate(DataRPS_noise$probabilite
                       ,by=c(list(DataRPS_noise$participation),list(DataRPS_noise$Datamicro))
                       ,FUN=mean)

Q50probabilite_bat=aggregate(DataRPS_bat$probabilite
                     ,by=c(list(DataRPS_bat$participation),list(DataRPS_bat$Datamicro))
                     ,FUN=function(x) quantile(x,0.5))
Q50probabilite_bs=aggregate(DataRPS_bs$probabilite
                    ,by=c(list(DataRPS_bs$participation),list(DataRPS_bs$Datamicro))
                    ,FUN=function(x) quantile(x,0.5))
Q50probabilite_noise=aggregate(DataRPS_noise$probabilite
                       ,by=c(list(DataRPS_noise$participation),list(DataRPS_noise$Datamicro))
                       ,FUN=function(x) quantile(x,0.5))


Q90probabilite_bat=aggregate(DataRPS_bat$probabilite
                     ,by=c(list(DataRPS_bat$participation),list(DataRPS_bat$Datamicro))
                     ,FUN=function(x) quantile(x,0.5))
Q90probabilite_bs=aggregate(DataRPS_bs$probabilite
                    ,by=c(list(DataRPS_bs$participation),list(DataRPS_bs$Datamicro))
                    ,FUN=function(x) quantile(x,0.5))
Q90probabilite_noise=aggregate(DataRPS_noise$probabilite
                       ,by=c(list(DataRPS_noise$participation),list(DataRPS_noise$Datamicro))
                       ,FUN=function(x) quantile(x,0.5))

Nbat=aggregate(DataRPS_bat$probabilite
               ,by=c(list(DataRPS_bat$participation),list(DataRPS_bat$Datamicro))
               ,FUN=length)
Nbs=aggregate(DataRPS_bs$probabilite
                            ,by=c(list(DataRPS_bs$participation),list(DataRPS_bs$Datamicro))
                            ,FUN=length)
Nnoise=aggregate(DataRPS_noise$probabilite
                               ,by=c(list(DataRPS_noise$participation),list(DataRPS_noise$Datamicro))
                               ,FUN=length)


Varbat=data.frame(cbind(Nbat,MaxDur_bat$x,MoyDur_bat$x,Q50Dur_bat$x
                        ,Q90Dur_bat$x,Maxprobabilite_bat$x
                        ,Moyprobabilite_bat$x,Q50probabilite_bat$x
                        ,Q90probabilite_bat$x))
Varbs=data.frame(cbind(Nbs,MaxDur_bs$x,MoyDur_bs$xs,Q50Dur_bs$x
                        ,Q90Dur_bs$x,Maxprobabilite_bs$x
                        ,Moyprobabilite_bs$x,Q50probabilite_bs$x
                        ,Q90probabilite_bs$x))
Varnoise=data.frame(cbind(Nnoise,MaxDur_noise$x,MoyDur_noise$x,Q50Dur_noise$x
                        ,Q90Dur_noise$x,Maxprobabilite_noise$x
                        ,Moyprobabilite_noise$x,Q50probabilite_noise$x
                        ,Q90probabilite_noise$x))

Var12=merge(Varnoise,Varbat,by=c("Group.1","Group.2"),all=T)
Var123=merge(Var12,Varbs,by=c("Group.1","Group.2"),all=T)
Var123[is.na(Var123)]=0

Var123P=merge(Var123,Particip,by.x="Group.1",by.y="participation")

Var123P_E=subset(Var123P,(!(Var123P$Group.2)&
                            (Var123P$canal_expansion_temps=="GAUCHE"))
                 |((Var123P$Group.2)&
                     (Var123P$canal_expansion_temps=="DROITE")))

Var123P_E_probDur=subset(Var123P_E,Var123P_E$MaxDur_noise.x>0.5)
fwrite(Var123P_E_probDur,"Var123P_E_probDur.csv",sep=";")

test=subset(DataRPS,(DataRPS$participation=="5889e8c17ac9bdd02100002d")
            &(!(DataRPS$Datamicro)))
testD=subset(DataRPS,(DataRPS$participation=="5889e8c17ac9bdd02100002d")
            &((DataRPS$Datamicro)))
testdur=subset(test,test$temps_fin>0.5)
test2=subset(DataRPS,DataRPS$donnee=="Cir270-2010-Pass1-Tron6-Chiro_0_00080_000")


Var123P_E=subset(Var123P_E,Var123P_E$MaxDur_noise.x<=0.5)



Var123P_D=subset(Var123P,(!(Var123P$Group.2)&
                            (Var123P$canal_enregistrement_direct=="GAUCHE"))
                 |((Var123P$Group.2)&
                     (Var123P$canal_enregistrement_direct=="DROITE")))



fit <- kmeans(Var123P_E[,3:29],10)

for (i in 3:29)
{
  boxplot(Var123P_E[,i]~fit$cluster,main=names(Var123P_E)[i])
}
