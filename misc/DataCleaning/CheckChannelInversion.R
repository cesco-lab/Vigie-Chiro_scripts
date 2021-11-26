library(data.table)
DataLP_RP=fread("DataLP_RP.csv")
Particip=fread("C:/wamp64/www/p_export.csv")
SRmed=fread("SRmed.csv")


DataPip=subset(DataLP_RP,substr(DataLP_RP$espece,1,3)=="Pip")
DataPip=subset(DataPip,DataPip$frequence>35)
DataPip=merge(DataPip,Particip,by="participation")
DataPip=merge(DataPip,SRmed,by.x=c("participation","Datamicro")
              ,by.y=c("participation","MicroDroit"))
DataPip=subset(DataPip,DataPip$canal_expansion_temps!="ABSENT")
DataPip=subset(DataPip,DataPip$canal_enregistrement_direct!="ABSENT")
DataPip=subset(DataPip,DataPip$canal_expansion_temps!="")
DataPip=subset(DataPip,DataPip$canal_enregistrement_direct!="")


AggDP=aggregate(DataPip$probabilite,by=c(list(DataPip$participation),list(DataPip$Datamicro))
                ,mean)
head(AggDP[order(AggDP$x),],10)
AggDP2=aggregate(AggDP$x,by=list(AggDP$Group.1),FUN=max)
head(AggDP2[order(AggDP2$x),],10)
AggDP2[order(AggDP2$x),][500:600,]
plot(AggDP2[order(AggDP2$x),]$x)
AggDP2[AggDP2$Group.1=="588b0aeb7ac9bd081c000401"]

PartLF=subset(SRmed$participation,SRmed$SampleRate<96000)
PartLF=unique(PartLF)
ProblemSub1=subset(AggDP2,AggDP2$x<0.6)
ProblemSub1=subset(ProblemSub1,!(ProblemSub1$Group.1 %in% PartLF))
ProblemSub2=subset(AggDP2,AggDP2$x<0.3)
ProblemSub2=subset(ProblemSub2,(ProblemSub2$Group.1 %in% PartLF))
#Sample=rbind(ProblemSub1[sample.int(nrow(ProblemSub1),5),]
 #            ,ProblemSub2[sample.int(nrow(ProblemSub2),5),])
Sample=rbind(ProblemSub1,ProblemSub2)

print(Sample)
fwrite(Sample,paste0("Sample_",substr(Sys.time(),18,19),".csv"))
ToImport=vector()
DataToImport=data.frame()
for (i in 1:nrow(Sample))
{
  DataSub=subset(DataPip,DataPip$participation==Sample$Group.1[i])
  DataSel=DataSub[ceiling(nrow(DataSub)/2),]
ToImport=c(ToImport,paste0(Sample$Group.1[i],";",DataSel$donnee))
if(DataSel$donnee==""){stop("data vide")}
DataSel2=subset(DataSel,select=c("donnee","participation","canal_expansion_temps"
                                 ,"canal_enregistrement_direct","site.x","observateur","SampleRate"))
DataSel2$ScorePip=Sample$x[i]
DataToImport=rbind(DataToImport,DataSel2)
}
fwrite(as.data.frame(ToImport),paste0("ToImport_",substr(Sys.time(),18,19),".csv"))
DataToImport=DataToImport[order(DataToImport$ScorePip),]
fwrite(DataToImport,paste0("DataToImport_",substr(Sys.time(),18,19),".csv"),sep=";")
