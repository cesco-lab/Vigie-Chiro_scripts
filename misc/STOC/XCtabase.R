library(data.table)
DirToTreat=dir("D:/test",pattern="wn",full.names=T)
DirToTreat=subset(DirToTreat,substr(basename(DirToTreat),1,2)=="wn")
ListTa=list.files("D:/test",pattern=".ta$",full.names=T,recursive=T)
CoefX10=fread("CoeftableX10.csv")


datalist=list()
for (i in 1:length(ListTa))
{
  datalist[[i]]=fread(ListTa[i])
}
dataTA=rbindlist(datalist)
dataTA$X10=as.numeric(grepl("-x10.wav",dataTA$Filename))
dataX1=subset(dataTA,X10==0)
dataX10=subset(dataTA,X10==1)
dataX10=as.data.frame(dataX10)

for (i in 3:ncol(dataX10))
{
 test=match(names(dataX10)[i],CoefX10$param) 
 Coef=CoefX10$coef[test]
 dataX10[,i]=dataX10[,i]*Coef
}

dataTA=rbind(dataX1,dataX10)


fwrite(dataTA,"dataTA.csv",sep=";")
