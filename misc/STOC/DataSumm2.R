library(data.table)

DataSumm=fread("DataSummaryOiseaux.csv")
DataClassif=fread("PerfsumCS_2020-09-02.csv")

DataSumm$Espece[DataSumm$Espece=="Corvus monedula"]="Coloeus monedula"
DataSumm$Espece[DataSumm$Espece=="Bonasa bonasia"]="Tetrastes bonasia"

test=grepl("monedula",DataSumm$Espece)
DataSumm$Espece[test]
test=grepl("flammea",DataClassif$Group.1)
DataClassif$Group.1[test]

DataClassif$Group.1[DataClassif$Group.1=="Dendrocoptes medius"]="Dendrocopos medius"
DataClassif$Group.1[DataClassif$Group.1=="Spilopelia senegalensis"]="Streptopelia senegalensis"
DataClassif$Group.1[DataClassif$Group.1=="Mareca strepera"]="Anas strepera"
DataClassif$Group.1[DataClassif$Group.1=="Spatula clypeata"]="Anas clypeata"



DataSumm$PerfCall=NA
DataSumm$PerfSong=NA
for (i in 1:nrow(DataSumm))
{
  Dataicall=subset(DataClassif,(DataClassif$Group.1==DataSumm$Espece[i])&
                     (DataClassif$Group.2==F))
  if(nrow(Dataicall)==1)
  {
    DataSumm$PerfCall[i]=Dataicall$x[1]
  }else{
    print(paste(DataSumm$Espece[i],"call"))
  }
  Dataisong=subset(DataClassif,(DataClassif$Group.1==DataSumm$Espece[i])&
                     (DataClassif$Group.2==T))
  if(nrow(Dataisong)==1)
  {
    DataSumm$PerfSong[i]=Dataisong$x[1]
  }else{
   # print(paste(DataSumm$Espece[i],"song"))
  }
  
}

fwrite(DataSumm,"DataSummaryOiseaux2.csv",sep=";")
