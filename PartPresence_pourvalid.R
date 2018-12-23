library(data.table)

PartPresence=fread("PartPresence.csv")
ETV_filtree=fread("ETV_filtree.csv")
PETV=levels(as.factor(ETV_filtree$participation))

Part_av=subset(PartPresence,!PartPresence$participation %in% PETV)

PartSel=vector()
SpTarget=vector()
ProbPres=vector()
for (i in 1:nlevels(as.factor(ETV_filtree$valid.espece)))
{
  ETVi=subset(ETV_filtree
              ,ETV_filtree$valid.espece==
                levels(as.factor(ETV_filtree$valid.espece))[i])
  Npart=nlevels(as.factor(ETVi$participation))
  print(paste(levels(as.factor(ETV_filtree$valid.espece))[i],Npart))
  
  if(Npart<20)
  {
    test=match(levels(as.factor(ETV_filtree$valid.espece))[i],names(Part_av))
    if(!is.na(test))
    {
    Part_av2=Part_av
    names(Part_av2)[test]="TRI"
    Part_av2=Part_av2[order(TRI,decreasing=T)]
    
    PartSel=c(PartSel,Part_av2$participation[1:(20-Npart)])
    ProbPres=c(ProbPres,Part_av2$TRI[1:(20-Npart)])
    SpTarget=c(SpTarget,rep(levels(as.factor(ETV_filtree$valid.espece))[i]
                            ,20-Npart))    
    }
  }
}
  
ValidSpRares=data.frame(cbind(SpTarget,PartSel,ProbPres))
fwrite(ValidSpRares,"ValidSpRares.csv")
