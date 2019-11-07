library(data.table)
PourC2M=fread("PourC2M.csv")
Zone="Norfolk"
SpeciesList=fread("SpeciesList.csv")

ColZ=match(Zone,colnames(SpeciesList))
FilterZ=as.data.frame(SpeciesList)[,ColZ]

SpeciesToSuppress=subset(SpeciesList,FilterZ=="")

ColToSuppress=vector()
for (i in 1:nrow(SpeciesToSuppress))
{
  test=grep(SpeciesToSuppress$Esp[i],colnames(PourC2M))
  ColToSuppress=c(ColToSuppress,test)
}
ColNToSuppress=colnames(PourC2M)[ColToSuppress]

ColToKeep=subset(colnames(PourC2M)
                 ,!(colnames(PourC2M) %in% ColNToSuppress))

PourC2M_decim=subset(PourC2M
                     ,select=ColToKeep)

PourC2M_decim=subset(PourC2M_decim
                     ,!(PourC2M_decim$ValidId %in% SpeciesToSuppress$Esp))


fwrite(PourC2M_decim,paste0("PourC2M_",Zone,".csv"))
