library(data.table) #to handle large numbers of .ta files
library(purrr)
SpeciesList=as.data.frame(fread("SpeciesList.csv"))
ListTCZ=list.files("C:/wamp64/www/tcz",full.names=T,pattern=".tar.gz$")
#ListTCZ=list.dirs("C:/wamp64/www/tcz",full.names=T)
FieldsExport=c("participation","Group.1","FreqM","SpMax2","Score","Tstart"
               ,"Tend","SR")
FieldsExport_original=c("participation","donnee","frequence","espece"
                        ,"probabilite"
                        ,"temps_debut"
                        ,"temps_fin","SR")
ListeTCZ_SPS=fread("C:/wamp64/www/listetcz.txt")
Particip=fread("C:/wamp64/www/p_export.csv")

Particip_RP=subset(Particip,!grepl("Fixe",Particip$site))

ListeTCZ_SPS_RP=subset(ListeTCZ_SPS,gsub(".tar.gz",""
                                         ,ListeTCZ_SPS$V11) %in% Particip_RP$participation)

TestAllDwnld=match(ListeTCZ_SPS_RP$V11,basename(ListTCZ))
summary(TestAllDwnld)
NonDwnld=subset(ListeTCZ_SPS_RP$V11,is.na(TestAllDwnld))
head(NonDwnld)

Particip_RP$ANNEE=substr(Particip_RP$date_debut,7,10)
table(Particip_RP$ANNEE)
Particip_RP$ROUTIER=grepl("Routier",Particip_RP$site)
table(Particip_RP$ANNEE,Particip_RP$ROUTIER)
Particip_RP$AT=substr(Particip_RP$trait_debut,7,10)
table(Particip_RP$AT)

MatchP=match(gsub(".tar.gz",""
                  ,ListeTCZ_SPS_RP$V11),Particip_RP$participation)

ListeTCZ_SPS_RP$V10[ListeTCZ_SPS_RP$V8=="Apr"]="2020"
ListeTCZ_SPS_RP$V10[ListeTCZ_SPS_RP$V10!="2020"]="2019"
table(ListeTCZ_SPS_RP$V10)

OutOfDate=subset(ListeTCZ_SPS_RP$V11
                 ,as.numeric(ListeTCZ_SPS_RP$V10)<as.numeric(Particip_RP$AT[MatchP]))
fwrite(data.frame(OutOfDate),"OutOfDate.csv")

if(length(OutOfDate)>0)
{
DOOD=gsub(".tar.gz","",OutOfDate)
for (i in 1:length(DOOD))
{
  dir.create(paste0("C:/wamp64/www/tcz/",DOOD[i]))
}
CommandMissingTC=paste0("pscp -pw PghgEESz1712! ybas@cca.in2p3.fr:/sps/mnhn/vigiechiro/vigiechiro-prod-datastore/"
                        ,DOOD,"/*.tc tcz/",DOOD)

#for (j in 1:length(CommandMissingTC))
#{
 # system(CommandMissingTC[j])
#}

}
TCvides=vector()
Log=data.frame(participation=0,Time=0,Size=0)
Log=Log[rep(seq(1,length(ListTCZ))),]
TCoutdate=vector()
TCtot=list()
for (i in 1:length(ListTCZ))
{
  
  #FI=file.info(ListTCZ[i])
  
  #if(FI$size<100)
  #{
   # TCvides=c(TCvides,ListTCZ[i])
  #}else{
    #x <- archive::archive_read(archive = TCZmatch)
    TCdir=gsub(".tar.gz","",ListTCZ[i])
    if(!dir.exists((TCdir)))
    {
      Sys.time()
      ListContent=untar(ListTCZ[i],list=T) 
      if(grepl("/",ListContent[1]))
      {
        untar(ListTCZ[i],exdir=dirname(TCdir)) #when tcz contains a directory
        
      }else{
      untar(ListTCZ[i],exdir=TCdir)  #when tc files are directly in the tar.gz
      }
        if(!dir.exists((TCdir))) {stop("problem of tcz content")}
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
    if(length(TClist)>0)
    {
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
  #}
    }else{
      print(paste0("empty folder: ",basename(TCdir)))
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
fwrite(TableTCtot,"TableTCtot.csv",sep=";")

TableTCtot_BonneVersion=subset(TableTCtot,is.na(TableTCtot$cigale))
#AllPart=unique(substr(TableTCtot$Group.1,1,20))
AllPart=unique(TableTCtot$participation)
#PartBV=unique(substr(TableTCtot_BonneVersion$Group.1,1,20))
PartBV=unique(TableTCtot_BonneVersion$participation)
PartMauvaiseVersion=subset(AllPart,!(AllPart %in% PartBV))
table(substr(PartMauvaiseVersion,1,2))
PartMauvaiseVersion[sample.int(length(PartMauvaiseVersion),1)]
fwrite(as.data.frame(PartMauvaiseVersion),"PartMauvaiseVersion.csv")
#head(subset(TCtot[[1]],TCtot[[1]]$SpMax1!=TCtot[[1]]$SpMax2))

ExportTC=subset(TableTCtot,select=FieldsExport)
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
fwrite(as.data.frame(SRheterogene),"SRheterogene.csv",sep=";")


colnames(SRmed)=c("participation","MicroDroit","SampleRate")
fwrite(SRmed,"SRmed.csv",sep=";")

