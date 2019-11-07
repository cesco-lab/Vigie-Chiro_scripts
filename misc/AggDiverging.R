library(data.table)
Divlist=list.files("./Tadarida/TCsel2",pattern="diverging.csv$",full.names=T)

my.data=list()
for (i in 1:length(Divlist))
{
  my.data[[i]]=fread(Divlist[i])
  
}
DivTot=rbindlist(my.data)
fwrite(DivTot,"DivTot.csv",sep=";")
