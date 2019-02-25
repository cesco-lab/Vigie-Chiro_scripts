FDataRP="DataRP_SpTron"
DataRP=fread(paste0(FDataRP,".csv"))
Particip=fread("C:/wamp64/www/p_export.csv")
FiltCanal="E" #E if expansion, D if direct

DataRPP=merge(DataRP,Particip,by="participation")

if(FiltCanal=="E")
{
  DataFilt=subset(DataRPP,((DataRPP$num_micro)
                           &(DataRPP$canal_expansion_temps=="DROITE"))
                    |((!DataRPP$num_micro)
                      &(DataRPP$canal_expansion_temps=="GAUCHE")))
}
DataNyclei=subset(DataFilt,DataFilt$espece=="Nyclei")
table(DataNyclei$site)
fwrite(DataFilt,paste0(FDataRP,"_",FiltCanal,".csv"),sep=";")

