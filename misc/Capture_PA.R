library(data.table)
Captures=fread("./VigieChiro/GIS/Capture/data_2019-06-06_clean_loc_sunset.csv")
#GI=fread("./VigieChiro/GIS/Capture/GI_site_capture_chiro.csv")
#NomGroupe="PKPN"
#NomGroupe="Plecos"
NomGroupe="Myotis"
NomGroupe="MyoAqu"
NomGroupe="RhiEH"
NomGroupe="MyoGT"
#NomGroupe="NlTt"
#NomGroupe="Serot"



#ListSp=c("Pipistrellus kuhlii","Pipistrellus nathusii")
#ListSp=c("Plecotus auritus","Plecotus austriacus","Plecotus macrobullaris")
#ListSp=c("Myotis alcathoe","Myotis bechsteinii","Myotis blythii"
 #        ,"Myotis brandtii"
  #       ,"Myotis capaccinii","Myotis dasycneme","Myotis daubentonii"
   #      ,"Myotis emarginatus","Myotis escalerai","Myotis myotis"
    #     ,"Myotis mystacinus","Myotis nattereri","Myotis punicus")
#ListSp=c("Myotis capaccinii","Myotis daubentonii")
#ListSp=c("Rhinolophus euryale","Rhinolophus hipposideros")
ListSp=c("Myotis blythii","Myotis myotis","Myotis punicus")
#ListSp=c("Nyctalus lasiopterus","Tadarida teniotis")
#ListSp=c("Nyctalus noctula","Nyctalus leisleri","Eptesicus serotinus"
     #    ,"Vespertilio murinus","Eptesicus nilssonii")


Captures$TAXON[Captures$TAXON=="Myotis latipennis"]="Myotis nattereri"
Captures$TAXON[Captures$TAXON=="Myotis nattereri_Spa"]="Myotis nattereri"

CapturesGroupe=subset(Captures,Captures$TAXON %in% ListSp)
for (i in 1:length(ListSp))
{
  CapturesGroupe$presence=as.numeric(CapturesGroupe$TAXON==ListSp[i])
  NewDir=paste0("./VigieChiro/GIS/Capture/",NomGroupe)
  dir.create(NewDir)
  NamePA=paste0(NewDir,"/PA_",NomGroupe,"_",ListSp[i],".csv")
  fwrite(CapturesGroupe,file=NamePA)
   
}
