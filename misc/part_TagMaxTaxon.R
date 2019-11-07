library(data.table)
PartF=list.files("C:/Users/Yves Bas/Documents/chiros/GCLR/ABC_Pouzols"
                 ,full.names = T)

my.data=list()
for (i in 1:length(PartF))
{
  my.data[[i]]=fread(PartF[i])
}
DataTot=rbindlist(my.data)


MaxSp=aggregate(DataTot$tadarida_probabilite
                ,by=list(DataTot$tadarida_taxon),FUN=max)
MaxSp$Tag=MaxSp$Group.1
DataTotMax=merge(DataTot,MaxSp,by.x=c("tadarida_taxon","tadarida_probabilite")
                 ,by.y=c("Group.1","x"),all.x=T)
fwrite(DataTotMax,"DataTotMax_Pouzols.csv",sep=";")

ListF_disqueJason=list.files("E:/SM2 Pouzols",full.names=T,recursive=T)

DataSel=subset(DataTotMax,DataTotMax$Tag!="")

test=match(DataSel$`nom du fichier`,gsub(".wav","",basename(ListF_disqueJason)))
test2=subset(test,!is.na(test))
DataAcopier=ListF_disqueJason[test2]

file.copy(from=DataAcopier,to=paste0("G:/Sons_pouzols/",basename(DataAcopier)))
