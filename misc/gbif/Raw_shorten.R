library(data.table)
library(stringr)

DirDataGroup="C:/Users/ybas/Documents/VigieChiro/gbifData/RawTest"
Groupes=fread("C:/Users/ybas/Documents/GroupeSp2.csv")
CoefMedian=2
DateSel=fread("C:/Users/ybas/Documents/VigieChiro/gbifData/DateSel.csv")

ListGroup=list.files(DirDataGroup
                     ,full.names=T,pattern="_simplified.csv$")
print(ListGroup)
Groupes$Rank=tolower(Groupes$Rank)
Groupes$Group=str_to_title(Groupes$Group)
table(Groupes$Rank)

for (h in 1:length(ListGroup)){
  DataMod=fread(ListGroup[h])
  
  DataMod$precision=ceiling(log(DataMod$coordinateUncertaintyInMeters,10))
  DataMod$precision=pmin(4,DataMod$precision)
  DataMod$precision=pmax(1,DataMod$precision)
  DataMod$precision=ifelse(is.na(DataMod$precision),5,DataMod$precision)
  
  DataSummary=fread(gsub("simplified.csv","simplified_summary.csv"
                         ,ListGroup[h]))
  
  DataSpSel=DataMod[0,]
  for (i in 1:length(unique(Groupes$Rank))){
    Groupi=subset(Groupes,Groupes$Rank==unique(Groupes$Rank)[i])
    testR=match(unique(Groupes$Rank)[i],names(DataMod))
    DataModi=DataMod
    names(DataModi)[testR]="Taxon"
    DataSpi=subset(DataModi,DataModi$Taxon %in% Groupi$Group)
    DataSpSel=rbind(DataSpSel,DataSpi,use.names=F)
    print(i)
    print(table(DataSpi$Taxon))
  }
  print(table(DataSpSel$phylum))
  ListSph=unique(DataSpSel$species)
  ListSph=subset(ListSph,ListSph!="")
  DataSpShortened=DataSpSel[0,]
  for (j in 1:length(ListSph))
  {
    if(j%%1000==1){print(paste(j,ListSph[j]))}
    Dataj=subset(DataSpSel,DataSpSel$species==ListSph[j])
    Summj=subset(DataSummary,DataSummary$Group.1==Dataj$phylum[1])
    Datej=subset(DateSel,DateSel$ListSpValide==ListSph[j])
    Dataj=subset(Dataj,!is.na(Dataj$eventDate))
    if(nrow(Dataj)>0){
      #stop()
      Yday=as.numeric(substr(Dataj$eventDate,9,10))+
        (as.numeric(substr(Dataj$eventDate,6,7))-1)*30
      #hist(Yday)
      Dataj=subset(Dataj,((Yday>Datej$PicSp-30)
                          &(Yday<Datej$PicSp+30))
                   |((Yday>Datej$PicSp-30+360)
                     &(Yday<Datej$PicSp+30+360))
                   |((Yday>Datej$PicSp-30-360)
                     &(Yday<Datej$PicSp+30-360))
      )
    }
    if(nrow(Dataj)>0){
      
      ListCountryj=unique(Dataj$countryCode)
      
      DatajSel=Dataj[0,]
      for (k in 1:length(ListCountryj)){
        Datak=subset(Dataj,Dataj$countryCode==ListCountryj[k])
        ListPrecisionk=unique(Datak$precision)
        for (l in 1:length(ListPrecisionk)){
          Datal=subset(Datak,Datak$precision==ListPrecisionk[l])
          Summl=subset(Summj,(Summj$Group.2==ListCountryj[k])
                       &(Summj$Group.3==ListPrecisionk[l]))
          Ceilingl=Summl$x*CoefMedian
          if(nrow(Datal)>Ceilingl){
            Datalshort=Datal[sample(nrow(Datal),Ceilingl),]
            DatajSel=rbind(DatajSel,Datalshort)      
          }else{
            DatajSel=rbind(DatajSel,Datal)      
          }
        }
      }
      DataSpShortened=rbind(DataSpShortened,DatajSel)
    }
  }
  fwrite(DataSpShortened,gsub("_simplified.csv","_simplified_shortened.csv",ListGroup[h]))
}
