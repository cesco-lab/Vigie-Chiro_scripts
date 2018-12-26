library(data.table)
FData="IdPartSelTot.csv"
Block=50000
#Order="Group.1"

Data=fread(FData)
#ColOrder=match(Order,names(Data))
#Data=Data[order(Data[,1]),]

for (i in 1:ceiling(nrow(Data)/Block))
{
  DataSplit=Data[((i-1)*Block+1):(i*Block),]
fwrite(DataSplit,paste0(i,"_",FData),sep=";")
  }
