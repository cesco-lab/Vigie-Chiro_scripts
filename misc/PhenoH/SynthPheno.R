library(data.table)

FPheno="D:/VigieChiro/Raw/Pheno100"
Outdir="D:/VigieChiro"

ListF=list.files(FPheno,full.names=T)

listdata=list()
for (i in 1:length(ListF))
{
 listdata[[i]]=fread(ListF[i]) 
 listdata[[i]]$time_int=tstrsplit(ListF[i],split="_")[[4]]
}
DataPheno=rbindlist(listdata)
#barplot(table(DataPheno$time_int))
fwrite(DataPheno,paste0(Outdir,"/SpAll",basename(FPheno),".csv"),sep=";")
