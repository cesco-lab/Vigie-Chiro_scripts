library(randomForest)
library(spdep)
library(readxl)
Prefix="./VigieChiro/ModPred/ModRF5090/ModRFActLog_"
SpeciesList=fread("SpeciesList.csv")
Suffix="5090"
DataAct=fread("C:/wamp64/www/SpNuit2_5090_DataLP_PF_exportTot.csv")
Particip=fread("C:/wamp64/www/p_export.csv")
SiteLoc=fread("C:/wamp64/www/sites_localites.txt")
CoordSIG=fread("C:/wamp64/www/GI_sites_localites.csv")
NCoord=40
CoordSIG[is.na(CoordSIG)]=0
GI_corr=read_excel("./VigieChiro/GIS/Notice_GI.xlsx")

Gite=mapply(function(x,y) 
  ((grepl(paste0(y,"="),x))|(grepl(paste0(y," ="),x)))
  ,SiteLoc$commentaire
  ,SiteLoc$localite)
SiteLoc$SpGite=as.numeric(Gite)

ListPar=levels(as.factor(DataAct$participation))
SelPar=subset(Particip,Particip$participation %in% ListPar)
SelParSL=merge(SiteLoc,SelPar,by.x=c("site","nom"),by.y=c("site","point"))

Synth=data.frame()
for (i in 1:nrow(SpeciesList))
{
  ModFile=paste0(Prefix,SpeciesList$Esp[i],Suffix,".learner")
  if(file.exists(ModFile))
  {
    load(ModFile)
    PLOT=varImpPlot(ModRF,cex=0.5,main=ModFile)
    varImpPlot(ModRF,cex=0.5,main=ModFile)
    PLOT1=PLOT[,1]/mean(PLOT[,1])
    PLOT2=PLOT[,2]/mean(PLOT[,2])
    Plot12=apply(cbind(PLOT1,PLOT2),MARGIN=1,mean)
    Plot12=Plot12[order(Plot12,decreasing=T)]
    Plot12=subset(Plot12,!grepl("SpFDate",names(Plot12)))
    Plot12=subset(Plot12,!grepl("SpCoord",names(Plot12)))
    Plot12=subset(Plot12,!grepl("SpGite",names(Plot12)))
    Plot12=subset(Plot12,Plot12>1)
    
  
    DataSp=subset(DataAct,DataAct$espece==SpeciesList$Esp[i])
    
    if(nrow(DataSp)>100)
    {
      DataSpSL=merge(DataSp,SelParSL,by="participation")
      DataSpSL_w0=merge(DataSp,SelParSL,by="participation",all.y=T)
      DataSpSL_w0$nb_contacts[is.na(DataSpSL_w0$nb_contacts)]=0
      
      DataSaison=merge(DataSpSL_w0,CoordSIG
                       ,by=c("longitude","latitude")
                       )
      
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
      
      VarSel=vector()
      VarName=vector()
      VarImp=vector()
      DiffAb=vector()
        while (length(Plot12)>0)
      {
        VarSel=c(VarSel,names(Plot12)[1])
        VarNamei=GI_corr$`Grands types`[match(VarSel[length(VarSel)],GI_corr$Variables)]
        VarName=c(VarName,VarNamei)
        PP=partialPlot(ModRF,pred.data=DataSaison,VarSel[length(VarSel)]
                       ,main=paste(SpeciesList$Esp[i],VarSel[length(VarSel)]))
        VariMean=mean(as.data.frame(CoordSIG)[,VarSel[length(VarSel)]])
        Y1=mean(subset(PP$y,PP$x<VariMean))
        Y2=mean(subset(PP$y,PP$x>=VariMean))
        DiffAbi=round((Y2/Y1-1)*100)
        DiffAb=c(DiffAb,DiffAbi)
        VarImpi=ifelse(DiffAbi<0,-Plot12[1],Plot12[1])
        VarImp=c(VarImp,VarImpi)
        print(paste(SpeciesList$Esp[i],VarNamei,DiffAbi,"%",VarImpi))
        test=grepl(VarNamei,GI_corr$`Grands types`)
        test2=grepl(VarNamei,GI_corr$H1)
        test3=grepl(VarNamei,GI_corr$H2)
        VarToEliminate=subset(GI_corr$Variables,test|test2|test3)
                Plot12=subset(Plot12,!names(Plot12) %in% VarToEliminate)
        
        }
      Vari=data.frame(Species=SpeciesList$Esp[i],VarSel,VarName,VarImp,DiffAb)
      Synth=rbind(Synth,Vari)
      }
    }
  }
}
fwrite(Synth,"DetailsRespRF1.csv")
