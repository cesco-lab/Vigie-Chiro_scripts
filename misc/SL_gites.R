library(data.table)

#recup siteloc
SiteLoc=fread("C:/wamp64/www/sites_localites.txt")
Gite=mapply(function(x,y) 
  ((grepl(paste0(y,"="),x))|(grepl(paste0(y," ="),x)))
  ,SiteLoc$commentaire
  ,SiteLoc$nom)
CharSplit=c(",",";","\n","\\.")

SiteLoc$SpGite=as.numeric(Gite)
table(SiteLoc$SpGite)

SiteLoc$nom_gite=""
for (i in 1:nrow(SiteLoc))
{
  if(SiteLoc$SpGite[i]==1)
  {
    InfoGite1=tstrsplit(SiteLoc$commentaire[i],split=SiteLoc$nom[i])[[2]]
    #suppression des premiers caractères
    while(substr(InfoGite1,1,1) %in% c(" ","="))
    {
      InfoGite1=substr(InfoGite1,2,nchar(InfoGite1))
    }
    if(nchar(InfoGite1)>0){
      #suppression des points et commentaires suivants
      SiteLoci=subset(SiteLoc,SiteLoc$site==SiteLoc$site[i])
      Otherpoints=unique(SiteLoci$nom)  
      CharSplit2=c(CharSplit,Otherpoints)  
      for (j in 1:length(CharSplit2))
      {
        if(nchar(InfoGite1)>0){
          InfoGite1= tstrsplit(InfoGite1,split=CharSplit2[j])[[1]]
        }
      }
      if(nchar(InfoGite1)>0){
        
        InfoGite2=tstrsplit(InfoGite1,split=",")[[1]]
      }
      if(nchar(InfoGite1)>0){
        
        InfoGite3=tstrsplit(InfoGite2,split=";")[[1]]
      }
      if(nchar(InfoGite1)>0){
        
        InfoGite4=tstrsplit(InfoGite3,split="'.'")[[1]]
      }
      if(nchar(InfoGite1)>0){
        
        InfoGite5=tstrsplit(InfoGite4,split="\n")[[1]]
      }
    }else{
      InfoGite5=InfoGite1
    }
    SiteLoc$nom_gite[i]=InfoGite5
  }
  
  
}

#print(unique(SiteLoc$nom_gite))

fwrite(data.frame(liste=unique(SiteLoc$nom_gite)),"ListeGites.csv",sep=";")

fwrite(SiteLoc,"C:/wamp64/www/sites_localites.csv",sep=";")  
