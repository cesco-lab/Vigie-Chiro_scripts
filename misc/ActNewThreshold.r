library(data.table)
EPA="./Tadarida/Vigie-Chiro_scripts/Vigie-Chiro_scripts/misc/extr_PF_ActNuit.r"

PP=c("55","56","57","58","59","5a","5b","5c","5d","5e","5f")

#PP=c("5b")


args="Valid"
Thresholds=0
args[12]=""
args[13]=""
args[14]=""
args[15]=T
args[16]="C:/wamp64/www/export_validtot201130.txt"
args[17]=T #correct for validation or not

FRaw="D:/VigieChiro/Raw"


args[3]=Thresholds[1]
args[10]=FRaw


for (z in 1:length(PP))
{
  print(paste(z,Sys.time()))
  args[1]="PrefPart"
  
  for (j in c(c(0:9),"a","b","c","d","e","f"))
  {
    args[2]=paste0(PP[z],j)
    print(args[2])
    
    Sys.time()
    args[4]=paste0(FRaw,"/DataLP_PF_export_",args[2],".csv")
    if(file.exists(args[4]))
    {
      source(EPA) # 1e6 donnees/min
      Sys.time()
    }
  }
}


if(args[17]){
  Pattern=basename(paste0(FRaw,"/SpNuit2Valid___",args[3],"_DataLP_PF_export_"))
}else{
Pattern=basename(paste0(FRaw,"/SpNuit2___",args[3],"_DataLP_PF_export_"))
}

SpToAgg=list.files(FRaw,pattern=Pattern,full.names=T)

my.data=list()
for (k in 1:length(SpToAgg))
{
  my.data[[k]]=fread(SpToAgg[k])
}
ActTot=rbindlist(my.data,use.names=T,fill=T)
ActTot[is.na(ActTot)]=""


Prefix="SpNuit2"

if(args[17]){
 Prefix=paste0(Prefix,"Valid")
}

if(args[15]){
  Prefix=paste0(Prefix,"Tri")
}

fwrite(ActTot,paste0(FRaw,"/",Prefix,"_",args[3],"_DataLP_PF_exportTot.csv"))
