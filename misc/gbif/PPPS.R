library(data.table)
print("L2")

#SuffixDet="_Presence_07_GI_SysGrid_Radius_930000_810000_2e+05.csv"
Pattern="_Presence_07_GI_SysGrid_Radius"
PredFolder="/mnt/beegfs/ybas/PP231223"
PredDest="/mnt/beegfs/ybas/PT_Rall"
DateRef=as.POSIXct("2021-02-22 21:54:39")
#DateDet=200
DateAll=fread("mnt/DateSel.csv")

print("L7")
LPF=list.files(PredFolder,pattern="PredPres")
dir.create(PredDest)
print("L9")
LPFinfo=tstrsplit(LPF,split="_")
GroupLPF=unique(LPFinfo[[9]])
Dates=seq(from=15,to=360,by=15)
#test=file.exists(paste0("Currently",GroupLPF,".log"))
print("L13")
#GroupLPF=subset(GroupLPF,!test)
if(length(GroupLPF)==0)
{
  stop("All working!")
}
print("L19")
GroupLPFsel=GroupLPF[sample(length(GroupLPF),1)]
#fwrite(data.frame(GroupLPFsel=Sys.Date()),paste0("Currently",GroupLPFsel,".log"),sep=";")
print(GroupLPFsel)

if(exists("SuffixDet")){
  Suffix=SuffixDet
}else{
  Suffix0=paste0(Pattern,"_",GroupLPFsel)
  
  LPFsel=subset(LPF,LPFinfo[[9]]==GroupLPFsel)
  LPFselinfo=tstrsplit(LPFsel[1],split="_")
  LPFsel_prefix=paste(LPFselinfo[[1]],LPFselinfo[[2]],LPFselinfo[[3]],sep="_")
  Suffix=gsub(LPFsel_prefix,"",LPFsel[1])
}
print(paste("Suffix:",Suffix))

#selectionner seulement les dates dispos
FileS=paste0(PredDest,"/PredTab_",Dates,Suffix)
InfoS=file.info(FileS)
print(InfoS$mtime)
print(Dates)
Dates=subset(Dates,(InfoS$mtime<DateRef)|(is.na(InfoS$mtime)))
print("L47")
print(Dates)

#for (h in 16:374)
#{
if(exists("DateDet")){
  Date=DateDet
}else{
  if(length(Dates)>0){
    h=Dates[sample(length(Dates),1)]
    Date=h
  }else{
    print(paste("all dates are done on",Suffix))
  }
}
Fileh=paste0(PredDest,"/PredTab_",Date,Suffix)
if(file.exists(Fileh))
{
  
  CoordI=file.info(Fileh)
  if(CoordI$mtime>DateRef)
  {
    print(summary(CoordI$mtime))
    print(summary(CoordI$mtime>DateRef))
    
    ToDo=F
  }else{
    ToDo=T
    
  }
}else{
  ToDo=T
}

if(ToDo){
  
  print(Date)
  #if(h%%5==0){
  #SelSpDate=fread(paste0("/mnt/beegfs/ybas/DataSel/DataSel__",Date,".csv"))
  SelSpDate=subset(DateAll,((DateAll$PicSp>Date-15)
                      &(DateAll$PicSp<Date+15))
               |((DateAll$PicSp>Date-15+360)
                 &(DateAll$PicSp<Date+15+360))
               |((DateAll$PicSp>Date-15-360)
                 &(DateAll$PicSp<Date+15-360)))
               
    print(nrow(SelSpDate))
  #test 129 et 82
  print(length(LPF))
  PredList=list()
  j=0
  for (i in 1:nrow(SelSpDate))
  {
    FilName=paste0(PredFolder,"/PredPres_",SelSpDate$ListSpValide[i],"_"
                   ,SelSpDate$Group[i],Suffix)
    if(i%%1000==1){
      print(FilName)
      print((basename(FilName) %in% LPF))
    }
    if(basename(FilName) %in% LPF)
    {
      Predi=fread(FilName)
      names(Predi)[ncol(Predi)]=SelSpDate$ListSpValide[i]
      j=j+1
      PredList[[j]]=Predi
    }
  }
  print(length(PredList))
  if(length(PredList)>1)
  {
    print("L59")
    PredTab=PredList[[1]]
    for (k in 2:length(PredList))
    {
      if(k==2){print(head(PredList[[k]][,3]))}
      PredTab=PredTab[,new:=PredList[[k]][,3]]
      names(PredTab)[ncol(PredTab)]=names(PredList[[k]])[3]
    }
    #PredTab=as.data.frame(PredTab)
    #PredTab$Score=apply(PredTab[,(3:ncol(PredTab))],MARGIN=1,FUN=sum)
    
    fwrite(PredTab,Fileh,sep=";")
    #PredScore=subset(PredTab,select=c("Group.1","Group.2","Score"))
    #fwrite(PredScore,paste0(PredDest,"/PredScore_",Date,Suffix),sep=";")
  }
}
#}
#}
#file.remove(paste0("Currently",GroupLPFsel,".log"))
