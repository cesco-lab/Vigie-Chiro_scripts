library(data.table)
PredictAct="./Tadarida/Predict_Act.r"
PredictDM="./Tadarida/Predict_DM.r"
PredictPresence="./Tadarida/Predict_Presence.r"

Interpole="./Tadarida/Interpole.r"

args=vector()
args[4]="SpeciesList.csv"

args[2]="GI_RandPts__30_34_755000_766000_6297000_6308000_1000_Lat41.45_51.61_Long-5.9_9.73"
args[3]="02/09/2018" #date of prediction
args[5]=50 #Threshold
args[7]="FranceD__30_34_755000_766000_6297000_6308000.shp"
args[8]=200 #raster pixel size?
args[9]=F #DM
args[10]=T #Act
args[12]=F #Presence
args[11]=40 #number of coordinates projections (must be a division of 360)

SpeciesList=fread(args[4])

ListSp=SpeciesList$Esp

for (h in 1:length(ListSp))
{
  args[1]=ListSp[h]
ModRF_file=paste0("./VigieChiro/ModPred/ModRFDecMin_",ListSp[h],"_Seuil",args[5],".learner")  
if(file.exists(ModRF_file))
{
  
if(args[9]==T)
{
  if (SpeciesList$Group[h]=="bat")
  {
    source(PredictDM)
    print(ListSp[h])
    args[6]=FilName
    source(Interpole)
  }
}
  }
  ModRF_file=paste0("./VigieChiro/ModPred/ModRFActLog_",ListSp[h],"_Seuil",args[5],".learner")  
  if(file.exists(ModRF_file))
  {
    
  
  if(args[10])
  {
  source(PredictAct)
  print(ListSp[h])
  args[6]=FilName
  source(Interpole)
  }
  }else{
    ModRF_file=paste0("./VigieChiro/ModPred/ModRFPresence_",ListSp[h],"_Seuil",args[5],".learner")  
    if(file.exists(ModRF_file))
    {
      if(args[12])
      {
        source(PredictPresence)
        print(ListSp[h])
        args[6]=FilName
        source(Interpole)
      }
    }
}
}


Importance=as.vector(scale(ModRF$importance[,1])+scale(ModRF$importance[,2]))
Imp2=as.data.frame(cbind(Var=row.names(ModRF$importance)
                         ,Imp=Importance))
head(Imp2[order(as.numeric(as.character(Imp2$Imp)),decreasing=T),],20)
