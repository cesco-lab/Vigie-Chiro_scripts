library(data.table)
Particip=fread("C:/wamp64/www/p_export.csv")
AW=fread("C:/wamp64/www/archivees_wav.txt",h=F)
AT=fread("C:/wamp64/www/archivees_ta.txt",h=F)
PO=subset(Particip,Particip$observateur=="Olivier Vinet")
POT=subset(PO,PO$participation %in% AT$V1)
POT$participation[sample.int(nrow(POT),1)]
fwrite(POT,"POT.csv",sep=";")

Particip=subset(Particip,Particip$observateur=="Yves Bas")
Particip=subset(Particip,as.numeric(substr(Particip$date_debut,7,10))>2013)
Particip=subset(Particip,Particip$participation %in% AW$V1)
Particip=subset(Particip,grepl("Fixe",Particip$site))
Psample=Particip[sample.int(nrow(Particip),12),]
fwrite(Psample,"Psample.csv",sep=";")
Pgroup=substr(Psample$participation,1,3)
Pgroupu=unique(Pgroup)
exportDir="D:/VigieChiro/Raw"
Psample$site
Psample$date_debut


FilesToImport=vector()
for (i in 11:length(Pgroupu))
{
  print(paste(i,Sys.time()))
  exportRaw=fread(paste0(exportDir,"/DataLP_PF_export_",Pgroupu[i],".csv"))
  exportG=subset(exportRaw,exportRaw$participation %in% Psample$participation)
  if(nrow(exportG)>0){
    for (j in 1:length(unique(exportG$participation)))
    {
      exportSel=subset(exportG
                       ,exportG$participation==unique(exportG$participation)[j])  
      
      FileO=exportSel$donnee[order(exportSel$donnee)]
      FileP=c(FileO[1],FileO[length(FileO)])
      SampleAll=FileO[sample.int(length(FileO),2)]  
      Hour=substr(FileO,nchar(FileO)-9
                  ,nchar(FileO)-8)
      NumHour=(as.numeric(Hour))+((as.numeric(Hour))<16)*24
      plot(NumHour)
      Evening=subset(FileO,NumHour<(min(NumHour)+2))
      SampleE=Evening[sample.int(length(Evening),2)]
      if(length(unique(Hour))>14)
      {
        Morning=subset(FileO,NumHour>(max(NumHour)-10))
      }else{
        Morning=subset(FileO,NumHour>(max(NumHour)-3))
      }
      SampleM=Morning[sample.int(length(Morning),4)]
      FileSel=c(FileP,SampleAll,SampleE,SampleM)
      FileSel=FileSel[order(FileSel)]
      ToImport=paste0(unique(exportSel$participation)[j],";",FileSel)
      FilesToImport=c(FilesToImport,ToImport)
    }
  }
}

fwrite(data.frame(FilesToImport),"FilesToImport.csv",sep=";")
