library(data.table)


#DataDep=fread("DataDep.csv")
DataAllCarre=fread("DataAllCarre.csv")
DataForTrends=fread("C:/Users/yvesb/Downloads/sorting_custom (2).csv")
SpeciesT="Eptser"
DataTot=fread("C:/Users/yvesb/Downloads/DataTot.csv")
DecProb=fread("C:/Users/yvesb/Downloads/DecProb.csv")


Dec2=subset(DecProb,DecProb$x==2)

DataForTrends=subset(DataForTrends,DataForTrends$participation %in% Dec2$Group.1)

Missing=subset(DataForTrends,!(DataForTrends$participation %in% DataTot$participation))
table(Missing$protocole)
Missing=subset(Missing,Missing$protocole=="POINT_FIXE")
head(unique(Missing$participation))
table(substr(Missing$participation,1,3))

Proper=subset(DataForTrends,(DataForTrends$participation %in% DataTot$participation))
DataRP=subset(DataForTrends,(DataForTrends$protocole!="POINT_FIXE"))

table(DataRP$espece)


DataForTrends_proper=rbind(DataRP,Proper)


fwrite(DataForTrends_proper

ListSite=unique(DataForTrends_proper$site)
for (i in 1:5){
  Sitei=sample(ListSite,1)
  print(Sitei)
Datai=subset(DataForTrends,DataForTrends$site==Sitei)
print(dcast(data=Datai,year~espece,value.var="nb_contacts",fun.aggregate=mean))

  }


DataS=subset(DataForTrends,DataForTrends$espece==SpeciesT)

#DataAllCarre$Dep=substr(DataAllCarre$Carre,25,26)
#table(DataAllCarre$Dep)
#DataDep$Dep=ifelse(nchar(DataDep$ListDep)==1,paste0("0",DataDep$ListDep),DataDep$ListDep)
#table(DataDep$Dep)
#DataDAC=merge(DataAllCarre,DataDep,by="Dep")

DataAllCarre$Score=scale(DataAllCarre$TrendMoinsi)+scale(DataAllCarre$TrendAll)
boxplot(DataAllCarre$Score)
head(DataAllCarre[order(DataAllCarre$Score,decreasing = T),])
DataO=DataAllCarre[order(DataAllCarre$Score,decreasing = T),]
DataO=DataAllCarre[order(DataAllCarre$Score),]


for (i in 1:6){
  
  Datai=subset(DataS,DataS$site==DataO$Carre[i])
  
  
  boxplot(Datai$nb_contacts~Datai$year,main=DataO$Carre[i])
  Ai=aggregate(Datai$nb_contacts,by=c(list(Datai$point),list(Datai$year)),mean)
print(Ai)



}
