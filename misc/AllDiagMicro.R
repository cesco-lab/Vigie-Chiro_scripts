library(data.table)
DM="./Tadarida/Vigie-Chiro_scripts/Vigie-Chiro_scripts/misc/Exports/DiagMicro.R"

PP=c("55","56","57","58","59","5a","5b","5c","5d","5e","5f","60")

#PP=c("5b")


args=""
args[10]="./VigieChiro/Raw/" #output dir
FRaw="./VigieChiro/Raw"
Pattern="DiagMic"

#source(SED) #to save validated data

dir.create(args[10])

for (z in 1:length(PP))
{
  print(paste(z,Sys.time()))
  args[1]="PrefPart"
  
  for (j in c(c(0:9),"a","b","c","d","e","f"))
  {
    args[2]=paste0(PP[z],j)
    print(args[2])
    Sys.time()
    Sys.time()
    args[4]=paste0(args[10],"/DataLP_PF_export_",args[2],".csv")
    testsize=file.size(args[4])
    if(testsize>200)
    {
      
      source(DM) 
    }
    
  }
  
}  


SpToAgg=list.files(FRaw,pattern=Pattern,full.names=T)

my.data=list()
for (k in 1:length(SpToAgg))
{
  my.data[[k]]=fread(SpToAgg[k])
}
DMTot=rbindlist(my.data)
table(DMTot$probleme_micro)
fwrite(DMTot,paste0(FRaw,"/DMTot.csv"))

