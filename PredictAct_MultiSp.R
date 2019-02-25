library(data.table)
PredictAct="./Tadarida/Vigie-Chiro_scripts/Vigie-Chiro_scripts/Predict_Act.r"
PredictDM="./Tadarida/Vigie-Chiro_scripts/Vigie-Chiro_scripts/Predict_DM.r"
PredictPresence="./Tadarida/Vigie-Chiro_scripts/Vigie-Chiro_scripts/Predict_Presence.r"

Interpole="./Tadarida/Vigie-Chiro_scripts/Vigie-Chiro_scripts/Interpole.r"

args=vector()
args[4]="SpeciesList.csv"
#args[4]="SpeciesList_bird2018-12-21.csv"
#args[13]=c("TURPHI")
args[2]="GI_SysGrid__20000"
args[3]="15/07/2019" #date of prediction
#args[5]=90 #Threshold
#args[5]=NA #Threshold
args[5]="xportTot" #Suffix

#args[7]="France_dep_L93Radius_ 28000.shp"
args[7]="France_dep_L93.shp"
args[8]=5000 #raster pixel size?
args[9]=F #DM
args[10]=T #Act
args[12]=F #Presence
args[11]=40 #number of coordinates projections (must be a division of 360)

if(!is.na(args[5]))
{
  if(is.numeric(args[5]))
  {
  Suffix=paste0("_Seuil",args[5],".learner")  
  }else{
    Suffix=paste0(args[5],".learner")  
    }

  }else{
      Suffix=paste0("NA.learner")  
        
    }
  
Prefix="./VigieChiro/ModPred/ModRF"
    

#if(!is.na(args[4]))
#{
SpeciesList=fread(args[4])
#}else{
#  Species=args[13]
#  SpeciesList=data.frame(Esp=Species)
#}
ListSp=SpeciesList$Esp




for (h in 1:length(ListSp))
{
  args[1]=ListSp[h]
ModRF_file=paste0(Prefix,"DecMin_",ListSp[h],Suffix)  

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
  
ModRF_file=paste0(Prefix,"ActLog_",ListSp[h],Suffix)  

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
    ModRF_file=paste0(Prefix,"Presence_",ListSp[h],Suffix)  
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
