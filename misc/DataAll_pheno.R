library(stringr)
library(data.table)

Renew=T #do again existing DateSp files
DestFolder="C:/Users/ybas/Downloads/Dates"
DirDataGroup="C:/Users/ybas/Downloads/RawGbif"
GroupList=fread("C:/Users/ybas/Downloads/GroupeSp2.csv")

dir.create(DestFolder)

LDataGroup=list.files(DirDataGroup
                      ,full.names=T,pattern=".csv$")

LDGInfo=tstrsplit(basename(LDataGroup),"_")

#Groups=unique(LDGInfo[[2]])
#Group="ASTERACEAE"
#Rank="FAMILY"

if(!Renew)
{
  stop("a recoder")
  ListAlready=list.files("./VigieChiro/gbifData/DateSp")
  GroupsAlready=gsub(".csv","",tstrsplit(basename(ListAlready),split="_")[[2]])
  Groups=subset(Groups,!(Groups %in% GroupsAlready))
}

ListGroup=LDataGroup

ListGroup=subset(ListGroup,!grepl("_CoordsU.csv",ListGroup))
ListGroup=subset(ListGroup,!grepl("/DateSp_",ListGroup))


for (h in 1:length(ListGroup)){
  print(ListGroup[h])
  DataGroup2=fread(ListGroup[h],fill=T,quote="")
  print(nrow(DataGroup2))
  GroupList$Group=str_to_title(GroupList$Group)  
  Phylums=subset(GroupList$Group,GroupList$Rank=="phylum")
  DataGroup2A=subset(DataGroup2,DataGroup2$phylum %in% Phylums) #40%
  Phylums=subset(GroupList$Group,GroupList$Rank=="CLASS")
  DataGroup2B=subset(DataGroup2,DataGroup2$class %in% Phylums) #32%
  Phylums=subset(GroupList$Group,GroupList$Rank=="ORDER")
  DataGroup2C=subset(DataGroup2,DataGroup2$order %in% Phylums) #16%
  Phylums=subset(GroupList$Group,GroupList$Rank=="FAMILY")
  DataGroup2D=subset(DataGroup2,DataGroup2$family %in% Phylums) #2%
  
  DataGroup2=rbindlist(list(DataGroup2A,DataGroup2B,DataGroup2C
                            ,DataGroup2D))
  
  DataGroup2$yday=as.numeric(substr(DataGroup2$eventDate,9,10))+
    (as.numeric(substr(DataGroup2$eventDate,6,7))-1)*30
  #hist(DataGroup2$yday)
  
  DataGroup2=subset(DataGroup2,!is.na(DataGroup2$yday))
  DataGroup2=subset(DataGroup2,DataGroup2$yday!=1)
  
  DataGroup2_FirstMonth=subset(DataGroup2,DataGroup2$yday<31)
  DataGroup2_FirstMonth$yday=DataGroup2_FirstMonth$yday+360
  
  DataGroup3=rbind(DataGroup2,DataGroup2_FirstMonth)
  hist(DataGroup3$yday)
  #test=subset(DataGroup2,DataGroup2$name=="Bombina variegata")
  #table(test$month,test$day)
  
  
  
  
  ListSp=levels(as.factor(DataGroup2$species))
  
  tab0=table(DataGroup2$species)[order(table(DataGroup2$species),decreasing=T)]
  #barplot(tab0[1:12],las=2,cex.names=0.5)
  
  ListSp[sample.int(length(ListSp),1)]
  
  i=which(ListSp=="Ranunculus polyanthemos")
  
  Date=data.frame(Group.1=2:390)
  PicSp=vector()
  ListSpValide=vector()
  N=vector()
  for (i in 1:length(ListSp))
  {
    test=grepl(" ",ListSp[i])
    #Sptemp=name_lookup(name=ListSp[i],rank="SPECIES")
    
    if(test){
      #Sptemp=name_backbone(name=ListSp[i],rank="SPECIES")
      DataTemp=subset(DataGroup3,DataGroup3$species==ListSp[i])
      DataTemp=subset(DataTemp,!is.na(DataTemp$yday))
      
      if(nrow(DataTemp)>0){
        AggDate=aggregate(DataTemp$species,by=list(DataTemp$yday),FUN=length)
        AggDate0=merge(AggDate,Date,by="Group.1",all.y=T)
        #plot(AggDate$Group.1,AggDate$x)
        AggDate0[is.na(AggDate0)]=0
        #plot(AggDate0$Group.1,AggDate0$x)
        CSDate=cumsum(AggDate0$x)
        Pheno=CSDate[31:389]-CSDate[1:359]
        #plot(Pheno)
        # Modes=find_modes(Pheno)
        # VModes=Pheno[Modes]
        # OModes=Modes[order(VModes,decreasing=T)]
        # 
        PeriodePic=subset(c(1:length(Pheno)),Pheno==max(Pheno))
        if(length(PeriodePic)==1)
        {
          Pic=PeriodePic+15
        }else{
          PeriodePic2=subset(PeriodePic,(PeriodePic+15) %in% DataTemp$yday)
          if(length(PeriodePic2)>0){PeriodePic=PeriodePic2}
          if(length(PeriodePic2)==1){
            Pic=PeriodePic2+15
          }else{
            Pic=sample(PeriodePic,1)+15
          }
          
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
          PeriodePic2=subset(PeriodePic,(PeriodePic+15) %in% DataTemp$yday)
          if(length(PeriodePic2)>0){PeriodePic=PeriodePic2}
          if(length(PeriodePic2)==1){
            Pic2=PeriodePic2+15
          }else{
            Pic2=sample(PeriodePic,1)+15
          }
          
        }
        PhenoDec2=(PhenoExtended[Pic2-15]+PhenoExtended[Pic2+45])/2
        if(PhenoDec2>PhenoDec1){Pic=Pic2}
        
        if(length(Pic)==1)
        {
          PicSp=c(PicSp,Pic)
          ListSpValide=c(ListSpValide,ListSp[i])
          N=c(N,nrow(DataTemp))
          
        }else{
          stop("pb pic")
        }
        
      }
    }
  }
  DateSp=data.frame(ListSpValide,PicSp,N)  
  hist(PicSp,breaks=c(0:13)*30)  
  fwrite(DateSp,paste0(DestFolder,"/DateSp_",basename(ListGroup[h])),sep=";")
}


