library(rgbif)
library(data.table)

FListSp="ListSp_ASTERACEAE_FR_ES_IT_CH.csv"
Rank="FAMILY"

ListSp=fread(FListSp)
InfoLS=tstrsplit(FListSp,"_")

Group=name_lookup(InfoLS[[2]],rank=Rank,return="data",limit=99999)
#Group=name_backbone(name='Cicadidae',rank='family',kingdom='animals')
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
NOcc=vector()
#for (h in 1:length(CountryList))
#{
for (i in 1:nrow(Group))
{
  #NOcc[i]=NOcc[i]+occ_count(taxonKey=Group$key[i], country=CountryFilter[1])
  NOcc=c(NOcc,occ_count(taxonKey=Group$key[i], country=CountryFilter[1]))
  if(i%%100==1){print(paste(i,Sys.time()))}
}

#}
plot(NOcc)
NumSel=which.max(NOcc)

Sys.time()
Kids=name_lookup(higherTaxonKey = Group$key[NumSel],return='data',limit=99999
                 ,status='ACCEPTED')
Sys.time()
#Kids_short=subset(Kids,!grepl(" ",Kids$scientificName))
Kids_sp=subset(Kids,Kids$rank=="SPECIES")
Sys.time()



DGL=list()
j=0
for (h in 1:length(CountryFilter))
{
  
  ListSpCountry=subset(ListSp,ListSp$country==CountryFilter[h])
  KidsCountry=subset(Kids_sp,Kids_sp$canonicalName %in% ListSpCountry$species)
  for (i in 1:nrow(KidsCountry))
  {
    
    Sys.time()
    DataGroup=occ_search(taxonKey=KidsCountry$key[i], return='data'
                         ,country="FR",limit=200000,eventDate=Period
                         ,familyKey=Group$key[NumSel]
                         ,fields=c('name','decimalLatitude','decimalLongitude'
                                   ,'individualCount'
                                   ,'coordinateUncertaintyInMeters'
                                   ,'year','month','day','eventDate')
    )
    Sys.time()
    
    #DataGroup2=rbindlist(DataGroup,fill=T,use.names=T)
    if(is.data.frame(DataGroup))
    {
      j=j+1
      DGL[[j]]=DataGroup
      #if(j%%10==1){
      print(paste(Sys.time(),CountryFilter[h],i,nrow(DataGroup),KidsCountry$scientificName[i]))
      #}
    }
  }
}


Sys.time()
if(nrow(DataGroupG)>0)
{
  DataGroup2=DataGroupG
  if(length(DGL)>0)
  {
    DataGroup2b=rbindlist(DGL,fill=T,use.names=T)
    DataGroup2=rbindlist(list(DataGroup2,DataGroup2b),fill=T,use.names=T)
  }
}else{
  DataGroup2=rbindlist(DGL,fill=T,use.names=T)
  
}


torder=table(DataGroup2$name)[order(table(DataGroup2$name),decreasing=T)]
barplot(torder[1:20],las=2,cex.names=0.7)
AggSp=aggregate(DataGroup2$eventDate,by=list(DataGroup2$name),FUN=length)


DataGroup2Pos=subset(DataGroup2,DataGroup2$individualCount!=0)
DataGroup2NA=subset(DataGroup2,is.na(DataGroup2$individualCount))
DataGroup2=rbind(DataGroup2NA,DataGroup2Pos)

DataGroup2$yday=(DataGroup2$month-1)*30+DataGroup2$day #dirty

DataGroup2=subset(DataGroup2,(DataGroup2$yday>1)&(DataGroup2$yday<361))
barplot(table(DataGroup2$day))
Data_D1=subset(DataGroup2,DataGroup2$day==1)
Ratio=nrow(DataGroup2)/30/nrow(Data_D1)
Data_D1_rs=Data_D1[sample(nrow(Data_D1)
                          ,min(nrow(Data_D1),ceiling(nrow(Data_D1)*Ratio))),] #correct oversampling of day 1
DataGroup2=subset(DataGroup2,DataGroup2$day!=1)
DataGroup2=rbind(DataGroup2,Data_D1_rs)
fwrite(DataGroup2,paste0("DataGroup2_",GroupName,".csv"))


DataGroup2_FirstMonth=subset(DataGroup2,DataGroup2$yday<31)
DataGroup2_FirstMonth$yday=DataGroup2_FirstMonth$yday+360

DataGroup3=rbind(DataGroup2,DataGroup2_FirstMonth)

#test=subset(DataGroup2,DataGroup2$name=="Bombina variegata")
#table(test$month,test$day)

ListSp=levels(as.factor(DataGroup2$name))

barplot(table(DataGroup2$name),las=2)

ListSp[sample.int(length(ListSp),1)]
Date=data.frame(Group.1=2:390)
PicSp=vector()
ListSpValide=vector()
for (i in 1:length(ListSp))
{
  test=grepl(" ",ListSp[i])
  #Sptemp=name_lookup(name=ListSp[i],rank="SPECIES")
  
  if(test){
    #Sptemp=name_backbone(name=ListSp[i],rank="SPECIES")
    DataTemp=subset(DataGroup3,DataGroup3$name==ListSp[i])
    AggDate=aggregate(DataTemp$name,by=list(DataTemp$yday),FUN=length)
    AggDate0=merge(AggDate,Date,by="Group.1",all.y=T)
    AggDate0[is.na(AggDate0)]=0
    CSDate=cumsum(AggDate0$x)
    Pheno=CSDate[31:389]-CSDate[1:359]
    PeriodePic=subset(c(1:length(Pheno)),Pheno==max(Pheno))
    if(length(PeriodePic)==1)
    {
      Pic=PeriodePic+15
    }else{
      Pic=sample(PeriodePic,1)+15
    }
    if(length(Pic)==1)
    {
      PicSp=c(PicSp,Pic)
      ListSpValide=c(ListSpValide,ListSp[i])
      
    }
    
  }
}
DateSp=data.frame(ListSpValide,PicSp)  
hist(PicSp,breaks=c(0:13)*30)  
fwrite(DateSp,paste0("DateSp_",GroupName,".csv"))

