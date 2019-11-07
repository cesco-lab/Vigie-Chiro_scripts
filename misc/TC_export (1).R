library(data.table) #to handle large numbers of .ta files
SpeciesList=as.data.frame(fread("SpeciesList.csv"))
ListTCZ=list.files("C:/wamp64/www/tcz",full.names=T,pattern=".tar.gz$")
FieldsExport=c("participation","Group.1","FreqM","SpMax2","Score","Tstart"
               ,"Tend","SR")
FieldsExport_original=c("participation","donnee","frequence","espece"
                        ,"probabilite"
                        ,"temps_debut"
                        ,"temps_fin","SR")

TCvides=vector()
Log=data.frame(participation=0,Time=0,Size=0)
Log=Log[rep(seq(1,length(ListTCZ))),]
TCoutdate=vector()
TCtot=list()
for (i in 1:length(ListTCZ))
{
  
  FI=file.info(ListTCZ[i])
  
  if(FI$size<100)
  {
    TCvides=c(TCvides,ListTCZ[i])
  }else{
    #x <- archive::archive_read(archive = TCZmatch)
    TCdir=gsub(".tar.gz","",ListTCZ[i])
    if(!dir.exists((TCdir)))
    {
      Sys.time()
      untar(ListTCZ[i],exdir=dirname(ListTCZ[i])) 
      Sys.time()
    }
    TClist=list.files(TCdir,full.names=T)
    Log$Size[i]=length(TClist)
    Log$Time[i]=Sys.time()
    if(i%%100==1){
      fwrite(Log,"Log.csv")
      print(paste(i,length(ListTCZ),Sys.time()))
    }
    my.data=list()
    Sys.time()
    for (j in 1:length(TClist))
    {
      my.data[[j]]=fread(TClist[j])
    }
    Sys.time()
    TCdata=rbindlist(my.data)
    if(sum(grepl("Urosp",names(TCdata)))>0)
    {
    Sys.time()
    TCtot[[i]]=TCdata
    }else{
      TCoutdate=c(TCoutdate,ListTCZ[i])
      print(paste("outofdate:",ListTCZ[i]))
    }
  }
}
#ajout champ participation
for (i in 1:length(ListTCZ))
{
  TCdir=gsub(".tar.gz","",ListTCZ[i])
  PartTemp=basename(TCdir)
  TCtot[[i]]$participation=PartTemp
}

TableTCtot=rbindlist(TCtot,fill=T,use.names=T)
TableTCtot_BonneVersion=subset(TableTCtot,!is.na(TableTCtot$cigale))

#head(subset(TCtot[[1]],TCtot[[1]]$SpMax1!=TCtot[[1]]$SpMax2))

ExportTC=subset(TableTCtot_BonneVersion,select=FieldsExport)
colnames(ExportTC)=FieldsExport_original
boxplot(ExportTC$probabilite~ExportTC$espece,las=2)
ExportTC$donnee=gsub(".wav","",ExportTC$donnee)
fwrite(ExportTC,"exporttc.txt",sep="\t")


#test=fread("exporttc.txt")
barplot(table(ExportTC$SR),las=2)

Info=tstrsplit(ExportTC$donnee,"_")

#ExportTC_nonconforme=subset(ExportTC,!(Info[[2]] %in% c("0","1")))
#table(ExportTC_nonconforme$participation)

ExportTC$microdroit=(Info[[2]]=="1")

SRmin=aggregate(ExportTC$SR,by=c(list(ExportTC$participation)
                                 ,list(ExportTC$microdroit)),FUN=min)

SRmax=aggregate(ExportTC$SR,by=c(list(ExportTC$participation)
                                 ,list(ExportTC$microdroit)),FUN=max)

SRmean=aggregate(ExportTC$SR,by=c(list(ExportTC$participation)
                                 ,list(ExportTC$microdroit)),FUN=mean)

SRmed=aggregate(ExportTC$SR,by=c(list(ExportTC$participation)
                                 ,list(ExportTC$microdroit)),FUN=median)


SRmean$max=SRmax$x
SRmean$min=SRmin$x
SRmean$med=SRmed$x


table(SRmax$x-SRmin$x)
SRheterogene=subset(SRmean,SRmean$max>SRmin$x)

colnames(SRmed)=c("participation","MicroDroit","SampleRate")
fwrite(SRmed,"SRmed.csv",sep=";")
