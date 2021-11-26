library(data.table)

PredRaw="./mnhn/PredSitesVeolia"

PredList=list.files(PredRaw,full.names=T)

for (i in 1:length(PredList))
{
  Predi=fread(PredList[i])
  if(i%%100==1){print(PredList[i])}
  Predi$nb_contacts_attendus=(10^Predi$pred)-1
  fwrite(Predi,PredList[i],sep=";")
}

