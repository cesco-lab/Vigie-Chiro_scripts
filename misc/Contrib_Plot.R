library(data.table)
Contrib=fread("ContribExpansion.csv")
VCcorr=fread("./mnhn/VCcorr.csv")
Expansion=fread("./mnhn/ExpansionManuel.csv")
DataAuto=fread("C:/Users/Yves Bas/Documents/GitHub/VigieChiro/data/data_vigieChiro_DataRP_SpTron_50_site_55sp_withAbs.csv")
i=5
TargetEsp="Pippip"

Contrib=as.data.frame(Contrib)[order(Contrib$Contrib,decreasing=T),]
test=match(Contrib$ListCir,VCcorr$ID_CIRCUIT)
Contrib$NewName=VCcorr$NewID[test]
Contrib$NewName=ifelse(is.na(Contrib$NewName),Contrib$ListCir,Contrib$NewName)
Contrib$NewName=ifelse(Contrib$NewName=="absent",Contrib$ListCir,Contrib$NewName)

DataManuel=subset(Expansion,(Expansion$espece==TargetEsp)
                  &(Expansion$ID_CIRCUIT==Contrib$ListCir[i]))

DataAuto$num_sitepos=abs(DataAuto$num_site)
DataAutoi=subset(DataAuto,(DataAuto$espece==TargetEsp)
                 &((DataAuto$num_site==as.numeric(Contrib$NewName[i]))
                   |(DataAuto$num_site==-as.numeric(Contrib$NewName[i]))))
DataAutoid=subset(DataAutoi,DataAutoi$direct)
DataAutoie=subset(DataAutoi,!DataAutoi$direct)

plot(DataManuel$ANNEE,DataManuel$Activite
     ,main=paste0("Data Circuit n°",Contrib$NewName[i])
     ,xlim=c(2006,2019))

points(DataAutoid$year,DataAutoid$nb_contacts_strict/30,col=2)
points(DataAutoie$year,DataAutoie$nb_contacts_strict/10,col=4)
Contrib[1:30,]
