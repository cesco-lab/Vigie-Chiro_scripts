library(data.table)
EPA="./Tadarida/Vigie-Chiro_scripts/Vigie-Chiro_scripts/misc/extr_PF_ActNuit.r"

PP=c("55","56","57","58","59","5a","5b","5c","5d","5e","5f")
#PP=c("5b")

args="Valid"
Thresholds=c(90,50)
args[10]="D:/VigieChiro/Raw"
args[12]=""
args[13]=""
args[14]=""

args[3]=Thresholds[1]

#source(SED) #to save validated data

for (z in 1:length(PP))
{
  print(paste(z,Sys.time()))
  args[1]="PrefPart"
  
  for (j in c(c(0:9),"a","b","c","d","e","f"))
  {
    args[2]=paste0(PP[z],j)
    print(args[2])
    Sys.time()
    args[11]=paste0(args[10],"/export_",args[2],".csv")
    testsize=file.size(args[11])
    if(testsize>200)
    {
      args[4]=paste0(args[10],"/DataLP_PF_export_",args[2],".csv")
      if(file.exists(args[4]))
      {
        source(EPA) # 1e6 donnees/min
        Sys.time()
      }
    }
  }
}
