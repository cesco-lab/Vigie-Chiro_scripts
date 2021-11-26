library(data.table)

FDataRP="DataRP_SpSecteur_0.csv"
EVT=fread("C:/wamp64/www/export_validtot201130.txt")
ConfOrder=c("POSSIBLE","PROBABLE","SUR")
ConfProba=c(0.5,0.9,0.99)

DataRP=fread(FDataRP)


EVTi=subset(EVT,EVT$participation %in% DataRP$participation)
EVTi$confo=match(EVTi$obs.proba,ConfOrder)
EVTo=subset(EVTi,!is.na(EVTi$confo))
if(nrow(EVTo)>0)
{
  EVToa=aggregate(EVTo$confo,by=c(list(EVTo$participation)
                                  ,list(EVTo$espece))
                  ,max)
  EVToa$x=ConfOrder[EVToa$x]
  names(EVToa)=c("participation","espece","confiance_observateur")
  DataPF_SpNuit2=merge(DataRP,EVToa
                       ,by=c("participation","espece"),all.x=T)
}else{
  DataPF_SpNuit2=DataRP
}
EVTi$confv=match(EVTi$valid.proba,ConfOrder)
EVTv=subset(EVTi,!is.na(EVTi$confv))
if(nrow(EVTv)>0)
{
  EVTva=aggregate(EVTv$confv,by=c(list(EVTv$participation)
                                  ,list(EVTv$espece))
                  ,max)
  EVTva$x=ConfOrder[EVTva$x]
  names(EVTva)=c("participation","espece","confiance_validateur")
  DataPF_SpNuit2=merge(DataPF_SpNuit2,EVTva
                       ,by=c("participation","espece"),all.x=T)
}
DataPF_SpNuit2[is.na(DataPF_SpNuit2)]=""

fwrite(DataPF_SpNuit2,gsub(".csv","_V.csv",FDataRP),sep=";")

