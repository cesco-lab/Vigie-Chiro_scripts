library(data.table)
library(randomForest)
ScriptFolder="./Tadarida/Vigie-Chiro_scripts/Vigie-Chiro_scripts/misc"
Interpole="/Interpole.r"

PredList=list.files("C:/Users/Yves Bas/Downloads/",pattern="SysGrid__3e"
                    ,full.names=T)

PredList=subset(PredList,substr(basename(PredList),1,4)=="Pred")
PredList=subset(PredList,grepl(".csv",PredList))

if(length(PredList)>0)
{
for (i in 1:length(PredList))
{
  args=vector()
  args[6]=gsub(".csv","",PredList[i])
  args[7]="France_dep_L93.shp"
  args[8]=5000 #PixelSize
  Spi=tstrsplit(basename(PredList[i]),split="_")[[2]]
  ModRF_file=paste0("C:/Users/Yves Bas/Downloads/ModRFActLog_",Spi
                    ,"50.learner")
  load(ModRF_file)
  MaxSample=100
  source(paste0(ScriptFolder,Interpole))
}
  
}else{
  print("probleme aucun fichier")
}