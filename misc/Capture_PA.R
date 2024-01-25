library(data.table)
Captures=fread("C:/Users/yvesb/Documents/mnhn/AdelineComte/correction/captures.csv")
ListGroups=fread("CoupleSpIdentif.csv")
DirOut="C:/Users/yvesb/Documents/VigieChiro/GIS/Capture/"

GroupsU=unique(ListGroups$SpT)

Captures$taxon[Captures$taxon=="Myotis latipennis"]="Myotis nattereri"
Captures$taxon[Captures$taxon=="Myotis nattereri_Spa"]="Myotis nattereri"

for (h in 1:length(GroupsU)){
  NomGroupe=GroupsU[h]
  NomGroupe_woS=gsub(" ","_",NomGroupe)
  print(NomGroupe)
  table(Captures$taxon)
  unique((Captures$taxon))
  fwrite(data.frame(ListSpCapture=unique(Captures$taxon)),"ListSpCaptures.csv")
  
  
  ListSp=subset(ListGroups$SpC,ListGroups$SpT==GroupsU[h])
  
  #ListSp_woS=gsub(" ","_",ListSp)
  
  CapturesGroupe=subset(Captures,Captures$taxon %in% c(GroupsU[h],ListSp))
  CapturesGroupe$presence=as.numeric(CapturesGroupe$taxon==GroupsU[h])
  summary(CapturesGroupe$presence)
  #NewDir=paste0(DirOut,"/",NomGroupe_woS)
  #dir.create(NewDir)
  NamePA=paste0(DirOut,"/PA_",NomGroupe_woS,".csv")
  fwrite(CapturesGroupe,file=NamePA)
  
}
