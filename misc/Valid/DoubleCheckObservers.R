library(data.table)

ValidData=fread("ValidDataPart.csv")
YearSelect=c("2019","2020")
Particip=fread("C:/wamp64/www/p_export.csv")
options(digits.secs=6)


ValidO=subset(ValidData,ValidData$observateur_probabilite!="")
ValidO$observateur_probabilite[ValidO$observateur_probabilite==""]="POSSIBLE"

ValidOU=unique(ValidO,by=c("donnee","espece","probabilite","observateur_taxon"))
FileData=tstrsplit(ValidOU$donnee,split="-")

ValidYearSelect=subset(ValidOU,FileData[[2]] %in% YearSelect)
Pprop=subset(Particip,select=c("participation","idobservateur"   
                               ,                    "observateur"))

Vprop=merge(ValidYearSelect,Pprop,by="participation")

DC=data.frame()
for (i in 1:length(unique(Vprop$idobservateur)))
{
  Vi=subset(Vprop,Vprop$idobservateur==unique(Vprop$idobservateur)[i])
  print(paste(Vi$observateur[1],nrow(Vi)))
  for (j in 1:length(unique(Vi$observateur_taxon)))
  {
    Vj=subset(Vi,Vi$observateur_taxon==unique(Vi$observateur_taxon)[j])
    print(paste(Vj$observateur_taxon[1],Vj$observateur_probabilite[1]
          ,nrow(Vj)))
    for (k in 1:length(unique(Vj$observateur_probabilite)))
    {
      Vk=subset(Vj,Vj$observateur_taxon==unique(Vj$observateur_taxon)[k])
      Time=Sys.time()
      SecTime=substr(Time,18,nchar(as.character(Time)))
      set.seed(as.numeric(SecTime)*1e6)
      RowSel=sample(nrow(Vk),1)
      DC=rbind(DC,as.data.frame(Vk)[RowSel,])
      #ToDL=c(ToDL,Vk$donnee[RowSel])
      #PartDL=c(PartDL,Vk$participation[RowSel])
    }
  }
}

DC$ToImport=paste(DC$participation,DC$donnee,sep=";")

fwrite(DC,"DCValid.csv",sep=";")
