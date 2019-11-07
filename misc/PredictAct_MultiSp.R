library(data.table)
PredictAct="./Tadarida/Vigie-Chiro_scripts/Vigie-Chiro_scripts/Predict_Act.r"
PredictDM="./Tadarida/Vigie-Chiro_scripts/Vigie-Chiro_scripts/Predict_DM.r"
PredictPresence="./Tadarida/Vigie-Chiro_scripts/Vigie-Chiro_scripts/Predict_Presence.r"

Interpole="./Tadarida/Vigie-Chiro_scripts/Vigie-Chiro_scripts/Interpole.r"

args=vector()
args[4]="SpeciesList.csv"
#args[4]="SpeciesList_bird2018-12-21.csv"
#args[13]=c("TURPHI")
args[2]="GI_RandPts_France_dep_L93Radius_ 15000_1000"
#args[2]="GI_SysGrid__11_30_34_48_66_15000"
args[2]="GI_SysGrid__20000"
args[3]="15/07/2019" #date of prediction
#ListPattern=90 #Threshold
ListPattern="xportTot" #Threshold
#ListPattern="_filtree" #Threshold
#ListPattern=NA #Threshold
#ListPattern=list(c("stacinus","hsteinii","alcathoe","brandtii","rginatus"
 #                  ,"aterreri")) #Suffix
#ListPattern=list(c(" murinus","ilssonii"," noctula","leisleri","erotinus")) #Suffix
#ListPattern=list(c(" blythii","s myotis"," punicus")) #Suffix
#ListPattern=list(c("paccinii","bentonii")) #Suffix
#ListPattern=list(c("nathusii","s kuhlii"))
#ListPattern=list(c("osideros"," euryale"))
ListPattern=list(c("striacus"," auritus","bullaris"))
#ListPattern=list(c("iopterus"))

args[7]="France_dep_L93Radius_ 15000.shp"
args[7]="France_dep_L93_09_11_12_30_31_32_34_46_48_65_66_81_82_2A_2B_13_83_84_04_05_06_07_26_69_42_38_73_74_01_03_63_43_15_19_23_87_86_79_17_16_33_40_47_64_24.shp"
#args[7]="France_dep_L93.shp"
args[8]=100000 #raster pixel size?
args[9]=F #DM
args[10]=F #Act
args[12]=T #Presence
args[11]=40 #number of coordinates projections (must be a division of 360)
GroupFilter="bat"
#GroupFilter=NA
PropGroup="Plecos"
#PropGroup=NA

Prefix="./VigieChiro/ModPred/ModRF"


#if(!is.na(args[4]))
#{
SpeciesList=fread(args[4])
#}else{
#  Species=args[13]
#  SpeciesList=data.frame(Esp=Species)
#}
ListSp=SpeciesList$Esp


if(!is.na(GroupFilter))
{
  ListSp=subset(ListSp,SpeciesList$Group %in% GroupFilter)
  SpeciesList=subset(SpeciesList,SpeciesList$Group %in% GroupFilter)
}


#ListSp="thusii"
#ListSp=c("Myotis","Plecos","PipKN","NlTt","MyoAqu"
#,"RhiEH")
#ListSp="NlTt"

if(is.list(ListPattern))
{
  ListSp=substr(unlist(ListPattern),3,8)
}

for (h in 1:length(ListSp))
{
  if(is.list(ListPattern))
  {
    Pattern=unlist(ListPattern)[h]
  }else{
    Pattern=ListPattern
  }
  
  if(!is.na(Pattern))
  {
    if(is.numeric(Pattern))
    {
      Suffix=paste0("_Seuil",Pattern,".learner")  
    }else{
      Suffix=paste0(Pattern,".learner")  
    }
    
  }else{
    Suffix=paste0("NA.learner")  
    
  }
  
  
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
        ModRF=ModRF_DM
        source(Interpole)
      }
    }
  }
  
  ModRF_file=paste0(Prefix,"ActLog_",ListSp[h],Suffix)  
  
  if(file.exists(ModRF_file))
  {
    
    
    if(args[10])
    {
      print(Sys.time())
      source(PredictAct)
      print(ListSp[h])
      args[6]=FilName
      source(Interpole)
      print(Sys.time())
    }
  }else{
    if(is.na(PropGroup))
    {
      ModRF_file=paste0(Prefix,"Presence_",ListSp[h],Suffix)  
      
    }else{
      ModRF_file=paste0(Prefix,"Presence_",PropGroup,Suffix)  
      
    }
    
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


#Importance=as.vector(scale(ModRF$importance[,1])+scale(ModRF$importance[,2]))
#Imp2=as.data.frame(cbind(Var=row.names(ModRF$importance)
#                         ,Imp=Importance))
#head(Imp2[order(as.numeric(as.character(Imp2$Imp)),decreasing=T),],20)
