library(data.table)
library(readxl)
#Tri="Vocal"
#Tri="Plant"
Tri=""
DataF=list.files("./VigieChiro/gbifData/DateSp",full.names=T)
GroupList=read_excel("GroupeSp.xlsx")

CommonSpecies=fread("SpeciesAll.csv",sep=";")
CommonSpecies$`Scientific name`=CommonSpecies$SpeciesGBIF

test=("Mercurialis annua" %in% CommonSpecies$Scientific.name)



if(Tri=="Vocal")
{
  CommonSpecies=fread("CommonSpeciesL.csv")
  
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


for (i in 16:374)
{
  #print(i)
  Today=i
  
  
  
  if(Today>360)
  {
    DataSel1=subset(DataDate,(DataDate$PicSp>Today-15)&(DataDate$PicSp<Today+15))
    DataSel2=subset(DataDate,(DataDate$PicSp>Today-360-15)&(DataDate$PicSp<Today-360+15))
    DataSel=rbind(DataSel1,DataSel2)
  }else{
    DataSel=subset(DataDate,(DataDate$PicSp>Today-15)&(DataDate$PicSp<Today+15))
  }
  
  fwrite(DataSel,paste0("./VigieChiro/gbifData/DataSelDate/DataSel_",Tri
                        ,"_",Today,".csv"))
  
  #barplot(table(DataSel$Group)[order(table(DataSel$Group),decreasing=T)][1:20]
   #       ,las=2,cex.names=0.5)
  #barplot(table(DataSel$Group)[order(table(DataSel$Group),decreasing=T)][21:40]
  #       ,las=2,cex.names=0.5)
  
  
  
  #if(Tri=="Vocal")
  #{
  DataSelRare=subset(DataSel
                     ,!(DataSel$ListSpValide %in% CommonSpecies$Scientific.name))
  test=("Mercurialis annua" %in% DataSelRare$ListSpValide)
  test=("Mercurialis annua" %in% CommonSpecies$Scientific.name)
  
  fwrite(DataSelRare,paste0("./VigieChiro/gbifData/DataSelDate/DataSelRare_"
                            ,Tri,"_",Today,".csv"),sep=";")
  #}else{
  #  DataSelRare=DataSel
  #}
  if(i%%30==0){
  print(DataSelRare[sample(nrow(DataSelRare),1),])
  }
  #DataSelRare[45:60,]
}
