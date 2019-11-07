library(data.table)
PredictPresence="./Tadarida/Vigie-Chiro_scripts/Vigie-Chiro_scripts/Predict_Presence.r"

args=vector()
args[4]="SpeciesList.csv"

args[2]="GI_coordWGS84_SpNuit2_Seuil50_DataLP_PF_exportTot_Lat41.45_51.61_Long-5.9_9.73"
args[5]=50
args[11]=40 #number of coordinates projections (must be a division of 360)

SpeciesList=fread(args[4])

ListSp=SpeciesList$Esp

for (h in 1:length(ListSp))
{
  args[1]=ListSp[h]
    ModRF_file=paste0("./VigieChiro/ModPred/ModRFPresence_",ListSp[h],"_Seuil",args[5],".learner")  
    if(file.exists(ModRF_file))
    {
      print(ListSp[h])
      
      for (g in 1:12)
      {
        DateG=g*30-15
      source(PredictPresence)
      }
      }
    }
  }
}


