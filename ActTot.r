library(data.table)
SED="./Tadarida/Vigie-Chiro_scripts/Vigie-Chiro_scripts/SelExportDonnees.r"
EPD="./Tadarida/Vigie-Chiro_scripts/Vigie-Chiro_scripts/extr_PF_DataLP.r"
EPA="./Tadarida/Vigie-Chiro_scripts/Vigie-Chiro_scripts/extr_PF_ActNuit.r"

PP=c("55","56","57","58","59","5a","5b","5c","5d")
PP2=c("55","56","57","58","59","5a","5b","5c","5d")

#PP=c("5b")


args="Valid"
args[3]="50"

source(SED) #to save validated data

for (i in 1:length(PP))
{
 print(paste(i,Sys.time()))
   args[1]="PrefPart"
  args[2]=PP[i]
  Sys.time()
  source(SED) 
  Sys.time()
  args[1]=paste0("export_",PP[i],".csv")
  source(EPD) # 3e5 donnees/min
  Sys.time() 
  args[4]=paste0("DataLP_PF_export_",PP[i],".csv")
  source(EPA) # 1e6 donnees/min
  Sys.time()
}

my.data=list()
for (i in 1:length(PP))
{
  my.data[[i]]=fread(paste0("SpNuit2_",args[3],"_DataLP_PF_export_",PP2[i],".csv"))
}
ActTot=rbindlist(my.data)
fwrite(ActTot,paste0("SpNuit2_",args[3],"_DataLP_PF_exportTot.csv"))
