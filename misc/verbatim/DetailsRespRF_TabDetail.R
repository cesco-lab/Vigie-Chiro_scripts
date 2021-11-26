library(data.table)

Details=fread("DetailsRespRF1.csv")
SpeciesList=fread("SpeciesList.csv")

Details$DiffAb=NULL
TabDetail=dcast(Details,Species~VarName)
TabDetail[is.na(TabDetail)]=0

fwrite(TabDetail,"TabDetail.csv",sep=";")
BatList=subset(SpeciesList,SpeciesList$Group=="bat")
TabBat=subset(TabDetail,TabDetail$Species %in% BatList$Esp)
fwrite(TabBat,"TabBatDetailVI.csv",sep=";")
