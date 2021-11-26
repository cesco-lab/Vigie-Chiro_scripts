library(data.table)
library(readxl)
Today=356
DataF=list.files("./VigieChiro/gbifData/DateSp",full.names=T)
GroupList=read_excel("GroupeSp.xlsx")
CommonSpecies=fread("SpeciesAll.csv",sep=";")


my.data=list()
for (i in 1:length(DataF))
{
  my.data[[i]]=fread(DataF[i])
  GroupTemp=gsub("DateSp_","",basename(DataF[i]))
  GroupTemp=gsub(".csv","",GroupTemp)
  my.data[[i]]$Group=GroupTemp
  }
DataDate=rbindlist(my.data)
test=subset(DataDate,!(DataDate$Group %in% GroupList$Group))
test$ListSpValide
DataPrior=merge(DataDate,GroupList,by="Group")
DataPrior$DatePrior=DataPrior$PicSp+(DataPrior$PicSp<Today)*365
DataPrior=DataPrior[order(DataPrior$DatePrior),]
DataPrior=DataPrior[order(DataPrior$Importance,decreasing=T),]
DataPriorF=subset(DataPrior,!(DataPrior$ListSpValide %in% CommonSpecies$SpeciesGBIF))
DataPriorC=subset(DataPrior,(DataPrior$ListSpValide %in% CommonSpecies$SpeciesGBIF))
subset(DataPriorF,DataPriorF$ListSpValide=="Pluvialis dominica")
head(DataPriorF)

DataPriorFC=rbind(DataPriorF,DataPriorC)
fwrite(DataPriorFC,paste0("DataPriorF_",Today,".csv"),sep=";")
  
