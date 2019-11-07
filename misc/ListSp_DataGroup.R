library(rgbif)
library(data.table)

ListAll=T
FListSp="ListSp_ASTERACEAE_FR_ES_IT_CH.csv"
Rank="FAMILY"

if(ListAll)
{
  LListSp=list.files("./VigieChiro/gbifData/ListSp"
                     ,full.names=T)
  
}else{
  LListSp=FListSp
}

for (g in 1:length(LListSp))
{
  ListSp=fread(LListSp[g])
  
  
  
  
  InfoLS=tstrsplit(basename(LListSp[g]),"_")
  
  InfoLS=gsub(".csv","",InfoLS)
  
  #Group=name_lookup(InfoLS[[2]],rank=Rank,return="data",limit=99999)
  Group=name_backbone(name=InfoLS[[2]])
  #Group$key
  
  YearToday=as.numeric(substr(Sys.time(),1,4))
  Period=paste(YearToday-50,YearToday-1,sep=",")
  
  CountryFilter=vector()
  for (i in 3:length(InfoLS))
  {
    CountryFilter=c(CountryFilter,InfoLS[[i]])
  }
  
  
  #name_lookup(rank="SPECIES",higherTaxonKey = 137356451,country=france_code)
  #test=name_lookup("Bos gruniens",rank="SPECIES")
  
  #NOcc=rep(0,length(Group))
  #NOcc=vector()
  #for (h in 1:length(CountryList))
  #{
  #for (i in 1:nrow(Group))
  #{
  #  #NOcc[i]=NOcc[i]+occ_count(taxonKey=Group$key[i], country=CountryFilter[1])
  #  NOcc=c(NOcc,occ_count(taxonKey=Group$usageKey[i], country=CountryFilter[1]))
  #  if(i%%100==1){print(paste(i,Sys.time()))}
  #}
  
  #}
  #plot(NOcc)
  #if(max(NOcc)>0)
  #{
  # NumSel=which.max(NOcc)
  
  Sys.time()
  Kids=name_lookup(higherTaxonKey = Group$usageKey,return='data',limit=99999
                   ,status='ACCEPTED')
  Sys.time()
  #Kids_short=subset(Kids,!grepl(" ",Kids$scientificName))
  Kids_sp=subset(Kids,Kids$rank=="SPECIES")
  Sys.time()
  
  
  
  for (h in 1:length(CountryFilter))
  {
    DGL=list()
    j=0
    
    ListSpCountry=subset(ListSp,ListSp$country==CountryFilter[h])
    KidsCountry=subset(Kids_sp,Kids_sp$canonicalName %in% ListSpCountry$species)
    print(paste(CountryFilter[h],nrow(KidsCountry)))
    if(nrow(KidsCountry)>0)
    {
      for (i in 1:nrow(KidsCountry))
      {
        
        Sys.time()
        DataGroup=occ_search(taxonKey=KidsCountry$key[i], return='data'
                             ,country=CountryFilter[h],limit=200000,eventDate=Period
                             ,familyKey=Group$key[NumSel]
                             ,fields=c('decimalLatitude','decimalLongitude'
                                       ,'individualCount'
                                       ,'coordinateUncertaintyInMeters'
                                       ,'year','month','day','eventDate')
        )
        Sys.time()
        
        #DataGroup2=rbindlist(DataGroup,fill=T,use.names=T)
        if(is.data.frame(DataGroup))
        {
          DataGroup$name=KidsCountry$canonicalName[i]
          j=j+1
          DGL[[j]]=DataGroup
          #if(j%%10==1){
          print(paste(Sys.time(),CountryFilter[h],i,nrow(KidsCountry),nrow(DataGroup),KidsCountry$scientificName[i]))
          #}
        }
      }
      
      if(length(DGL)>0)
      {
        DataGroup2=rbindlist(DGL,fill=T,use.names=T)
        torder=table(DataGroup2$name)[order(table(DataGroup2$name),decreasing=T)]
        barplot(torder[1:min(20,length(torder))],las=2,cex.names=0.7)
        
        if(sum(grepl("individualCount",names(DataGroup2))))
        {
          DataGroup2Pos=subset(DataGroup2,DataGroup2$individualCount!=0)
          DataGroup2NA=subset(DataGroup2,is.na(DataGroup2$individualCount))
          DataGroup2=rbind(DataGroup2NA,DataGroup2Pos)
        }
        DataGroup2$yday=(DataGroup2$month-1)*30+DataGroup2$day #dirty
        
        DataGroup2=subset(DataGroup2,(DataGroup2$yday>1)&(DataGroup2$yday<361))
        barplot(table(DataGroup2$day))
        barplot(table(DataGroup2$month))
        
        Data_D1=subset(DataGroup2,DataGroup2$day==1)
        if(nrow(Data_D1)>0)
        {
          Ratio=nrow(DataGroup2)/30/nrow(Data_D1)
          Data_D1_rs=Data_D1[sample(nrow(Data_D1)
                                    ,min(nrow(Data_D1),ceiling(nrow(Data_D1)*Ratio))),] #correct oversampling of day 1
          DataGroup2=subset(DataGroup2,DataGroup2$day!=1)
          DataGroup2=rbind(DataGroup2,Data_D1_rs)
        }
        fwrite(DataGroup2,paste0("DataGroup2_",InfoLS[[2]],"_",CountryFilter[h],".csv"))
        
        
      }
      
    }
  }
  
  
  
  #AggSp=aggregate(DataGroup2$eventDate,by=list(DataGroup2$name),FUN=length)
  
}
