library(data.table)
EPA="./Tadarida/Vigie-Chiro_scripts/Vigie-Chiro_scripts/extr_PF_ActNuit.r"

args=""
args[3]="50"
args[10]="D:/VigieChiro/Raw"
args[14]="sunrise"
args[12]=7200
args[13]=-1800



LDataLP=list.files(args[10],full.names=T,pattern="DataLP_PF_export")
test=sapply(LDataLP,function(x) grepl("SpNuit",x))
LDataLP=subset(LDataLP,!test)

for (i in 1:length(LDataLP))
{
  print(paste(i,Sys.time()))
  args[4]=LDataLP[i]
  
  if(file.exists(args[4]))
  {
    
    source(EPA) # 1e6 donnees/min
    Sys.time()
  }
}


Pattern=basename(paste0(args[10],"/SpNuit2_",args[14],args[13],"_"
                        ,args[12],"_",args[3],"_DataLP_PF_export_"))

SpToAgg=list.files(args[10],pattern=Pattern,full.names=T)

my.data=list()
for (k in 1:length(SpToAgg))
{
  my.data[[k]]=fread(SpToAgg[k])
}
ActTot=rbindlist(my.data)
fwrite(ActTot,paste0(args[10],"/SpNuit2_",args[14],args[12],args[13],args[3],"_DataLP_PF_exportTot.csv"))

