library(data.table)

EVT=fread("C:/wamp64/www/export_validtot210318.txt")
#Filtre="Myo"


#EVT=subset(EVT,grepl(Filtre,EVT$espece))
EVT=subset(EVT,substr(EVT$donnee,1,3)=="Car")

EVT$spV=ifelse(EVT$valid.espece=="",EVT$obs.espece,EVT$valid.espece)
EVT$error=(EVT$spV!=EVT$espece)
EVT=EVT[order(EVT$probabilite,decreasing=T),]
summary(EVT$error)
NbV_Esp=aggregate(EVT$donnee,by=c(list(EVT$participation
),list(EVT$espece
)),length
)

NbV_Esp_Multi=subset(NbV_Esp,NbV_Esp$x>1)

NbE=0
NbS2=0
NbS3=0
V3=0
NbSall=0
for (i in 1:nrow(NbV_Esp_Multi))
{
  EVTi=subset(EVT,(EVT$participation==NbV_Esp_Multi$Group.1[i])&
                (EVT$espece==NbV_Esp_Multi$Group.2[i]))
  if(EVTi$error[1]==1){
    NbE=NbE+1
    if(EVTi$error[2]==0){
      NbS2=NbS2+1
    }else{
      if(nrow(EVTi)>2)
      {
        V3=V3+1
        if(EVTi$error[3]==0)
        {
          NbS3=NbS3+1
        }
      }
    }
    if(min(EVTi$error)==0)
    {
      NbSall=NbSall+1
    }
  }
  
  
}
