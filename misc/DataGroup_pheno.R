#library(rgbif)
library(data.table)


find_modes<- function(x) {
  modes <- NULL
  if(x[2]<x[1]){modes=c(modes,1)}
  for ( i in 2:(length(x)-1) ){
    if ( (x[i] > x[i-1]) & (x[i] >= x[i+1]) ) {
      modes <- c(modes,i)
    }
    if(x[(length(x)-1)]<x[length(x)]){modes=c(modes,length(x))}
    
  }
  if ( length(modes) == 0 ) {
    modes = 'This is a monotonic distribution'
  }
  return(modes)
}

Renew=T #do again existing DateSp files
DestFolder="./VigieChiro/gbifData/DateSp"

LDataGroup=list.files("./VigieChiro/gbifData/DataGroup"
                      ,full.names=T,pattern=".csv$")

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
      plot(AggDate$Group.1,AggDate$x)
      AggDate0[is.na(AggDate0)]=0
      CSDate=cumsum(AggDate0$x)
      Pheno=CSDate[31:389]-CSDate[1:359]
      plot(Pheno)
      Modes=find_modes(Pheno)
      VModes=Pheno[Modes]
      OModes=Modes[order(VModes,decreasing=T)]
      
      PeriodePic=subset(c(1:length(Pheno)),Pheno==max(Pheno))
      if(length(PeriodePic)==1)
      {
        Pic=PeriodePic+15
      }else{
        Pic=sample(PeriodePic,1)+15
      }
      PhenoExtended=c(Pheno[330:359],Pheno,Pheno[1:30])
      PhenoDec1=(PhenoExtended[Pic-15]+PhenoExtended[Pic+45])/2
      
      Pheno[max(1,(Pic-45)):(min(Pic,length(Pheno)))]=0
      if(Pic<31){
        Pheno[(Pic+315):length(Pheno)]=0
      }
      if(Pic>345){
        Pheno[1:(Pic-345)]=0
      }
      
      PeriodePic=subset(c(1:length(Pheno)),Pheno==max(Pheno))
      if(length(PeriodePic)==1)
      {
        Pic2=PeriodePic+15
      }else{
        Pic2=sample(PeriodePic,1)+15
      }
      PhenoDec2=(PhenoExtended[Pic2-15]+PhenoExtended[Pic2+45])/2
      if(PhenoDec2>PhenoDec1){Pic=Pic2}
      
      if(length(Pic)==1)
      {
        PicSp=c(PicSp,Pic)
        ListSpValide=c(ListSpValide,ListSp[i])
        
      }
      
    }
  }
  DateSp=data.frame(ListSpValide,PicSp)  
  hist(PicSp,breaks=c(0:13)*30,main=Groups[h])  
  fwrite(DateSp,paste0(DestFolder,"/DateSp_",Groups[h],".csv"))
  
}

