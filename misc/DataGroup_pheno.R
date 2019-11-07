library(rgbif)
library(data.table)
Renew=F #do again existing DateSp files

LDataGroup=list.files("./VigieChiro/gbifData/DataGroup"
                      ,full.names=T)

LDGInfo=tstrsplit(basename(LDataGroup),"_")

Groups=unique(LDGInfo[[2]])
#Group="ASTERACEAE"
#Rank="FAMILY"

if(!Renew)
{
  ListAlready=list.files("./VigieChiro/gbifData/DateSp")
  GroupsAlready=gsub(".csv","",tstrsplit(basename(ListAlready),split="_")[[2]])
Groups=subset(Groups,!(Groups %in% GroupsAlready))
  }

for (h in 1:length(Groups))
{

ListGroup=list.files("./VigieChiro/gbifData/DataGroup",pattern=Groups[h]
                     ,full.names=T)

my.data=list()
for (i in 1:length(ListGroup))
{
  my.data[[i]]=fread(ListGroup[i])
}

DataGroup2=rbindlist(my.data,fill=T,use.names=T)


DataGroup2_FirstMonth=subset(DataGroup2,DataGroup2$yday<31)
DataGroup2_FirstMonth$yday=DataGroup2_FirstMonth$yday+360

DataGroup3=rbind(DataGroup2,DataGroup2_FirstMonth)

#test=subset(DataGroup2,DataGroup2$name=="Bombina variegata")
#table(test$month,test$day)

ListSp=levels(as.factor(DataGroup2$name))

tab0=table(DataGroup2$name)[order(table(DataGroup2$name),decreasing=T)]
#barplot(tab0[1:12],las=2,cex.names=0.5)

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
hist(PicSp,breaks=c(0:13)*30,main=Groups[h])  
fwrite(DateSp,paste0("./VigieChiro/gbifData/DateSp/DateSp_",Groups[h],".csv"))

}

