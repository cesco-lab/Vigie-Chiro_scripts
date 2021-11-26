library(data.table)

EVT=fread("C:/wamp64/www/export_validtot210318.txt")
SpNuit=fread("C:/wamp64/www/SpNuit2_50_DataLP_PF_exportTot.csv")
#Filtre="Myo"

#EVT=subset(EVT,grepl(Filtre,EVT$espece))
EVT=subset(EVT,substr(EVT$donnee,1,3)=="Car")
EVT=subset(EVT,EVT$probabilite>0.5)

EVT$spV=ifelse(EVT$valid.espece=="",EVT$obs.espece,EVT$valid.espece)
EVT$error=(EVT$spV!=EVT$espece)
EVT=EVT[order(EVT$probabilite,decreasing=T),]
summary(EVT$error)
NbV_Esp=aggregate(EVT$donnee,by=c(list(EVT$participation
),list(EVT$espece
)),length
)

NbV_Esp_Multi=subset(NbV_Esp,NbV_Esp$x>50)
NbV_Esp_Multi$FirstTrue=NA
NbV_Esp_Multi$OtherFalse=NA
NbV_Esp_Multi$RatioLoss=0
for (i in 1:nrow(NbV_Esp_Multi))
{
  EVTi=subset(EVT,(EVT$participation==NbV_Esp_Multi$Group.1[i])&
                (EVT$espece==NbV_Esp_Multi$Group.2[i]))
  NbV_Esp_Multi$FirstTrue[i]=(EVTi$error[1]==0)
  NbV_Esp_Multi$OtherFalse[i]=(max(EVTi$error)==1)
  if((NbV_Esp_Multi$FirstTrue[i])&(NbV_Esp_Multi$OtherFalse[i]))
  {

    MeanPos=mean(subset(EVTi$probabilite,EVTi$error==0))
    MeanNeg=mean(subset(EVTi$probabilite,EVTi$error==1))
    if(MeanNeg>=MeanPos)
    {
      NbV_Esp_Multi$RatioLoss[i]=0.5
    }else{
      RatioLoss[i]=1-((MeanPos-MeanNeg)/(MeanPos-0.5))
    }
    
  }
    
}

NbV_Esp_MultiTrue=subset(NbV_Esp_Multi,NbV_Esp_Multi$FirstTrue)
summary(NbV_Esp_MultiTrue$RatioLoss)
mean(NbV_Esp_MultiTrue$RatioLoss==0)
