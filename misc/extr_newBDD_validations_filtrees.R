library(data.table)

GroupAgg=F
RSDBoff=F


Particip=fread("C:/wamp64/www/p_export.csv",encoding="UTF-8")
Particip=as.data.frame(Particip)

ETV=fread("export_validtot2018-07-26.csv")
colnames(ETV)[10]="temps_fin"

SpeciesList=fread("SpeciesList.csv")

#tabase3=fread("tabase3HF_1015France.csv")

ETV_prop=aggregate(ETV$donnee,by=c(list(ETV$proprietaire),list(ETV$obs.espece),list(ETV$valid.espece)),FUN=length)
ETV_prop2=subset(ETV_prop,(ETV_prop$Group.2!="")&(ETV_prop$Group.3!=""))
Verif=(ETV_prop2$Group.2==ETV_prop2$Group.3)
Verif_prop=aggregate(Verif,by=c(list(ETV_prop2$Group.1),list(ETV_prop2$Group.2)),FUN=mean)
Pb_Id=subset(Verif_prop,(Verif_prop$x<0.95)&(Verif_prop$Group.1!="Yves Bas"))

ETV_PI=merge(ETV,Pb_Id,by.x=c("proprietaire","obs.espece"),by.y=c("Group.1","Group.2"),all.x=T)
ETV_filtree=subset(ETV_PI,(ETV_PI$x=="")|(ETV_PI$valid.espece)!=""|(ETV_PI$proprietaire=="Yves Bas"))

for (i in 1:nrow(ETV_filtree))
{
  if(ETV_filtree$valid.espece[i]==""){
    ETV_filtree$valid.espece[i]=ETV_filtree$obs.espece[i]
    ETV_filtree$valid.proba[i]=ETV_filtree$obs.proba[i]
    
    }
  print(i)
}

#espèces manquantes
test=match(ETV_filtree$valid.espece,SpeciesList$Nesp)
SpeciesM=subset(ETV_filtree$valid.espece,is.na(test))
test2=match(SpeciesM,SpeciesList$Esp)
GroupM=SpeciesList$Nesp[test2]
SpeciesM2=subset(SpeciesM,is.na(test2))
table(SpeciesM2)

if(GroupAgg)
{
#groupes à aggréger
test3=match(ETV_filtree$valid.espece,SpeciesM)
SpeciesM3=GroupM[test3]
SpeciesA=mapply(function(x,y,z) if(is.na(x)){y}else{z},test3,ETV_filtree$valid.espece,SpeciesM3)

ETV_filtree$valid.espece=SpeciesA
}

if(RSDBoff)
{
#retirer overlap 1ère couche
test=match(ETV_filtree$donnee,substr(tabase3$Filename,1
                                     ,nchar(tabase3$Filename)-4))
ETV_filtree=subset(ETV_filtree,is.na(test))
}

write.csv(ETV_filtree,"ETV_filtree.csv",row.names=F)
