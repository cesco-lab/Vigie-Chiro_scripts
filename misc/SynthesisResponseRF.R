library(randomForest)
library(spdep)
Prefix="./VigieChiro/ModPred/ModRF5090/ModRFActLog_"
SpeciesList=fread("SpeciesList.csv")
Suffix="xportTot"
DataAct=fread("SpNuit2_90_DataLP_PF_exportTot.csv")
Particip=fread("C:/wamp64/www/p_export.csv")
SiteLoc=fread("C:/wamp64/www/sites_localites.txt")
CoordSIG=fread("./VigieChiro/GIS/GI_coordWGS84_SpNuit2_50_DataLP_PF_exportTot.csv")
NCoord=40


Gite=mapply(function(x,y) 
  ((grepl(paste0(y,"="),x))|(grepl(paste0(y," ="),x)))
  ,SiteLoc$commentaire
  ,SiteLoc$localite)
SiteLoc$SpGite=as.numeric(Gite)

ListPar=levels(as.factor(DataAct$participation))
SelPar=subset(Particip,Particip$participation %in% ListPar)
SelParSL=merge(SiteLoc,SelPar,by.x=c("site","nom"),by.y=c("site","point"))


for (i in 48:nrow(SpeciesList))
{
  ModFile=paste0(Prefix,SpeciesList$Esp[i],Suffix,".learner")
  if(file.exists(ModFile))
  {
    load(ModFile)
    PLOT=varImpPlot(ModRF,cex=0.5,main=ModFile)
    varImpPlot(ModRF,cex=0.5,main=ModFile)
    PLOT1=PLOT[order(PLOT[,1],decreasing=T),1]
    PLOT2=PLOT[order(PLOT[,2],decreasing=T),2]
    
    VarSel=vector()
    for (j in 1:6)
    {
      VarSel=c(VarSel,names(PLOT1)[j],names(PLOT2)[j])
    }
    
    DataSp=subset(DataAct,DataAct$espece==SpeciesList$Esp[i])
    
    if(nrow(DataSp)>100)
    {
      DataSpSL=merge(DataSp,SelParSL,by="participation")
      DataSpSL_w0=merge(DataSp,SelParSL,by="participation",all.y=T)
      DataSpSL_w0$nb_contacts[is.na(DataSpSL_w0$nb_contacts)]=0
      
      DataSaison=merge(DataSpSL_w0,CoordSIG
                       ,by.x=c("longitude","latitude")
                       ,by.y=c("Group.1","Group.2"))
      
      #add date of year
      Date1=as.Date(substr(DataSaison$date_debut,1,10)
                    ,format="%d/%m/%Y")
      DataSaison$SpFDate=yday(Date1)
      #add several rotated coordinates
      CoordDS=as.matrix(cbind(DataSaison$longitude,DataSaison$latitude))
      
      for (a in 0:(NCoord-1))
      {
        Coordi=Rotation(CoordDS,angle=pi*a/as.numeric(NCoord))
        #print(plot(Coordi[,1],CoordDS[,1],main=as.character(a)))
        #print(plot(Coordi[,1],CoordDS[,2],main=as.character(a)))
        DataSaison=cbind(DataSaison,Coordi[,1])
        names(DataSaison)[ncol(DataSaison)]=paste0("SpCoord",a)
      }
      
      #seasonal subset
      #  DataSaison=subset(DataSaison,substr(DataSaison$`date part. debut`,4,5) %in% Saison)
      
      
      DataSPos=subset(DataSaison,DataSaison$nb_contacts>0)
      #NbReplicatsSpatiaux=nlevels(as.factor(as.character(DataSPos$Coord)))
      
      testPred=(substr(names(DataSaison),1,2)=="Sp")
      Prednames=names(DataSaison)[testPred]
      Predictors=DataSaison[,..Prednames]
      
      
      testNA=apply(Predictors,MARGIN=2,FUN=function(x) sum(is.na(x)))
      
      DataSaison$ActLog10=log10(DataSaison$nb_contacts+1)
      
      Predictors=subset(DataSaison,select=grepl("Sp",colnames(DataSaison)))
      test=(is.na(Predictors))
      test2=apply(test,MARGIN=1,sum)
      test3=apply(test,MARGIN=2,sum)
      #plot(test2)
      #plot(test3)
      
      DataSaison=subset(DataSaison,test2==0)
      
      test=(is.na(DataSaison))
      test2=apply(test,MARGIN=1,sum)
      test3=apply(test,MARGIN=2,sum)
      DataSaison=subset(DataSaison,select=(test3==0))
      
      test=match(row.names(ModRF$importance),colnames(DataSaison))
                                                    
      if(sum(is.na(test))==0)
      {
      for (k in 1:6)
      {
        partialPlot(ModRF,DataSaison,VarSel[k]
                    ,main=paste(SpeciesList$Esp[i],VarSel[k]))
      }
      }
    }
  }
}

