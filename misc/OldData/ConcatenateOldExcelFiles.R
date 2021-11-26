library(readxl)

IdFiles=list.files("C:/Users/Yves Bas/Documents/Tadarida/rounds/compil_validation200728/RP"
                   ,full.names=T)

IdElist=list()
IdDlist=list()
for (i in 1:length(IdFiles))
{
  IdElist[[i]]=read_excel(IdFiles[i],sheet=3)
IdDlist[[i]]=read_excel(IdFiles[i],sheet=4)
Anneei=tstrsplit(basename(IdFiles[i]),split="-")[[2]]
IdElist[[i]]$ANNEE=Anneei
IdDlist[[i]]$ANNEE=Anneei
}

IdE=rbindlist(IdElist,use.names=T,fill=T)
IdD=rbindlist(IdDlist,use.names=T,fill=T)
fwrite(IdE,paste0("IdE",Sys.Date(),".csv"),sep=";")
fwrite(IdD,paste0("IdD",Sys.Date(),".csv"),sep=";")

