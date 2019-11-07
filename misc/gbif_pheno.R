library(rgbif)
library(data.table)
CountryList=c("France","Spain","Italy","Switzerland")
TaxaList=c("PHYLUM","CLASS","ORDER","FAMILY","GENUS","SPECIES")

GroupName="CICADIDAE"
Rank="FAMILY"
Group=name_lookup(GroupName,rank=Rank,return="data",limit=99999)
#Group=name_backbone(name='Cicadidae',rank='family',kingdom='animals')
#Group$key


CountryFilter=vector()
for (i in 1:length(CountryList))
{
  temp_code <- isocodes[grep(CountryList[i], isocodes$name), "code"]
  CountryFilter=c(CountryFilter,temp_code)
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

Kids=name_lookup(higherTaxonKey = Group$key[NumSel],return='data',limit=99999
                 ,status='ACCEPTED')

#Kids_short=subset(Kids,!grepl(" ",Kids$scientificName))
Kids_sp=subset(Kids,Kids$rank=="SPECIES")
Sys.time()
DGL=list()
j=0
DataGroupG=data.frame()


for (h in 1:length(CountryFilter))
{
  Ndata=occ_count(taxonKey = Group$key[NumSel], country=CountryFilter[h])
  if(Ndata<200000)
  {
    
    Sys.time()
    DataGroup0=occ_search(taxonKey=Group$key[NumSel], country=CountryFilter[h],limit=200000
                          ,return='data'
                          ,fields=c('name','decimalLatitude','decimalLongitude'
                                    ,'individualCount'
                                    ,'coordinateUncertaintyInMeters'
                                    ,'year','month','day','eventDate','canonicalName')
    )
    Sys.time()
    if(is.data.frame(DataGroup0))
    {
      print(paste(Sys.time(),CountryFilter[h],nrow(DataGroup0)))
      DataGroupG=rbindlist(list(DataGroupG,DataGroup0),fill=T,use.names=T)
    }
    
  }else{
    
    for (i in 1:nrow(Kids_sp))
    {
      
      Sys.time()
      #DataGroup=occ_search(scientificName="Bos grunniens", return='data',country="CH",limit=200000)
      #Ndata=occ_count(taxonKey = Group$key[NumSel], country=CountryFilter[h])
      DataGroup=data.frame()
      Year=2018
      while((nrow(DataGroup)<10000)&(Year>1919))
      {
        DataGroup0=occ_search(taxonKey=Kids_sp$key[i], country=CountryFilter[h],limit=200000
                              ,return='data'
                              ,fields=c('name','decimalLatitude','decimalLongitude'
                                        ,'individualCount'
                                        ,'coordinateUncertaintyInMeters'
                                        ,'year','month','day','eventDate')
                              ,year=Year)
        
        
        if(is.data.frame(DataGroup0))
        {
          #print(paste(Year,nrow(DataGroup0),Sys.time()))
          DataGroup=rbindlist(list(DataGroup,DataGroup0),fill=T,use.names=T)
        }
        Year=Year-1
      }
      Sys.time()
      #DataGroup2=rbindlist(DataGroup,fill=T,use.names=T)
      if(nrow(DataGroup)>0)
      {
        j=j+1
        DGL[[j]]=DataGroup
        #if(j%%10==1){
        print(paste(Sys.time(),CountryFilter[h],i,j,nrow(DataGroup),Kids_sp$scientificName[i]))
        #}
      }
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

