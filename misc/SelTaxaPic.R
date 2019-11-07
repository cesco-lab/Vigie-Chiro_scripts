library(data.table)
library(readxl)
Today=301
#Tri="Vocal"
Tri="Plant"
#Tri=""
DataF=list.files("./VigieChiro/gbifData/DateSp",full.names=T)
GroupList=read_excel("GroupeSp.xlsx")
CommonSpecies=fread("CommonSpeciesL.csv")

if(Tri=="Vocal")
{
  GroupVocal=subset(GroupList,GroupList$Sound==1)
  InfoDataF=tstrsplit(basename(DataF),"_")
DataF=subset(DataF,gsub(".csv","",InfoDataF[[2]]) %in% GroupVocal$Group)
  }


if(Tri=="Plant")
{
  GroupPlant=subset(GroupList,GroupList$Plant==1)
  InfoDataF=tstrsplit(basename(DataF),"_")
  DataF=subset(DataF,gsub(".csv","",InfoDataF[[2]]) %in% GroupPlant$Group)
}



my.data=list()
for (i in 1:length(DataF))
{
  my.data[[i]]=fread(DataF[i])
  GroupTemp=gsub("DateSp_","",basename(DataF[i]))
  GroupTemp=gsub(".csv","",GroupTemp)
  my.data[[i]]$Group=GroupTemp
  }
DataDate=rbindlist(my.data)

DataSel=subset(DataDate,(DataDate$PicSp>Today-15)&(DataDate$PicSp<Today+15))

fwrite(DataSel,paste0("DataSel_",Tri,"_",Today,".csv"))
  
barplot(table(DataSel$Group)[order(table(DataSel$Group),decreasing=T)][1:20]
        ,las=2,cex.names=0.5)
#barplot(table(DataSel$Group)[order(table(DataSel$Group),decreasing=T)][21:40]
 #       ,las=2,cex.names=0.5)



if(Tri=="Vocal")
{
DataSelRare=subset(DataSel
                   ,!(DataSel$ListSpValide %in% CommonSpecies$`Scientific name`))
fwrite(DataSelRare,paste0("DataSelRare_",Tri,"_",Today,".csv"))
}else{
  DataSelRare=DataSel
}

DataSelRare[sample(nrow(DataSelRare),1),]

