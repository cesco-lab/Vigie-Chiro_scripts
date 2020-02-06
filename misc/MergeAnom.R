library(data.table)
library(corrplot)
ListAnom=list.files("./VigieChiro/Weather",pattern="Anom",full.names=T)

ListClimate=subset(ListAnom,!grepl("NCEP",ListAnom))
ListNCEP=subset(ListAnom,grepl("NCEP",ListAnom))

AnomClimate=fread(ListClimate[1])
print(nrow(AnomClimate))
for (i in 2:length(ListClimate))
{
  Anomi=fread(ListClimate[i])
  AnomClimate=merge(AnomClimate,Anomi, all=T)
  print(nrow(AnomClimate))
}
test=as.data.frame(AnomClimate)[,(4:ncol(AnomClimate))]
test[is.na(test)]=0
corrplot(cor(test))
fwrite(AnomClimate,"./VigieChiro/Weather/Ano_Climate.csv",sep=";")

AnomNCEP=fread(ListNCEP[1])
print(nrow(AnomNCEP))
for (i in 2:length(ListNCEP))
{
  Anomi=fread(ListNCEP[i])
  AnomNCEP=merge(AnomNCEP,Anomi, all=T)
  print(nrow(AnomNCEP))
}
test=as.data.frame(AnomNCEP)[,(4:ncol(AnomNCEP))]
test[is.na(test)]=0
corrplot(cor(test))
fwrite(AnomNCEP,"./VigieChiro/Weather/Ano_NCEP.csv",sep=";")


