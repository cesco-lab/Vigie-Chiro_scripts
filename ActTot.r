library(data.table)
SED="C:/Users/Yves Bas/Documents/Tadarida/SelExportDonnees.r"
EPD="C:/Users/Yves Bas/Documents/Tadarida/extr_PF_DataLP.r"
EPA="C:/Users/Yves Bas/Documents/Tadarida/extr_PF_ActNuit.r"

#PP=c("55","56","57","58","59","5a","5b")
PP=c("5a","5b")


args="Valid"
args[3]="Seuil90"

source(SED) #to save validated data

for (i in 1:length(PP))
{
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

PP=c("55","56","57","58","59","5a","5b")

my.data=list()
for (i in 1:length(PP))
{
  my.data[[i]]=fread(paste0("SpNuit2_",args[3],"_DataLP_PF_export_",PP[i],".csv"))
}
ActTot=rbindlist(my.data)
fwrite(ActTot,paste0("SpNuit2_",args[3],"_DataLP_PF_exportTot.csv"))
