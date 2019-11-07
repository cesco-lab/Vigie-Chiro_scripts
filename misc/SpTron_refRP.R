library(data.table)
SeuilFiable="Seuil50"
SpTron=fread(paste0("DataRP_SpTron",SeuilFiable,".csv"))

#table "participations"
Particip=fread("C:/Program Files (x86)/EasyPHP-DevServer-14.1VC11/data/localweb/p_export.txt",encoding="UTF-8")
Particip=as.data.frame(Particip)




SpTron=subset(SpTron,(SpTron$temps_enr<600)&
                (SpTron$temps_enr>200))

SpTronP=merge(SpTron,Particip,by="participation")

Expansion=(((SpTronP$num_micro)
            &(SpTronP$canal_expansion_temps=="DROITE"))
           |((SpTronP$num_micro==F)
             &(SpTronP$canal_expansion_temps=="GAUCHE")))
Direct=(((SpTronP$num_micro)
                   &(SpTronP$canal_enregistrement_direct=="DROITE"))
                  |((SpTronP$num_micro==F)
                    &(SpTronP$canal_enregistrement_direct=="GAUCHE")))

#CanalBug=subset(SpTronP,(Expansion&Direct))
test=subset(SpTron,SpTron$espece=="Nycnoc")
plot(test$temps_enr,test$nb_contacts,log="y")

Prot=substr(SpTronP$site,1,21)
SpTronRE=subset(SpTronP,(Prot=="Vigie-chiro - Routier")&(Expansion))
SpTronRD=subset(SpTronP,(Prot=="Vigie-chiro - Routier")&(Direct))
SpTronPE=subset(SpTronP,(Prot=="Vigiechiro - Pédestre")&(Expansion))
SpTronPD=subset(SpTronP,(Prot=="Vigiechiro - Pédestre")&(Direct))

ListSub=list(SpTronRE,SpTronRD,SpTronPE,SpTronPD)


for (h in 1:4)
  
{
  
  SpTron=ListSub[[h]]

Q25=vector()
Q75=vector()
Q98=vector()
nbocc=vector()
Q25c=vector()
Q75c=vector()
Q98c=vector()
nboccc=vector()

for (i in 1:nlevels(as.factor(SpTron$espece)))
{
  Datasub=subset(SpTron,SpTron$espece==levels(as.factor(SpTron$espece))[i])
  #calcul des quantiles d'activité
  Q25=c(Q25,quantile(Datasub$nb_contacts,0.25))
  Q75=c(Q75,quantile(Datasub$nb_contacts,0.75))
  Q98=c(Q98,quantile(Datasub$nb_contacts,0.98))
  nbocc=c(nbocc,nrow(Datasub))
  AggCir=aggregate(Datasub$nb_contacts,by=list(Datasub$participation)
                   ,FUN=sum)
  Q25c=c(Q25c,quantile(AggCir$x,0.25))
  Q75c=c(Q75c,quantile(AggCir$x,0.75))
  Q98c=c(Q98c,quantile(AggCir$x,0.98))
  nboccc=c(nboccc,nrow(AggCir))
  }
print(paste(h,nlevels(as.factor(SpTron$espece))))
assign(paste0("Ref",h),as.data.frame(cbind(Espece=levels(as.factor(SpTron$espece)),Q25,Q75,Q98,nbocc
                                           ,Q25c,Q75c,Q98c,nboccc)))
}

Ref=merge(Ref1,Ref2,by="Espece")
Ref=merge(Ref,Ref3,by="Espece")
Ref=merge(Ref,Ref4,by="Espece")

colnames(Ref)=c("Espece","Q25RE","Q75RE","Q98RE","nboccRE"
                ,"Q25cRE","Q75cRE","Q98cRE","nbocccRE"
                ,"Q25RD","Q75RD","Q98RD","nboccRD"
                ,"Q25cRD","Q75cRD","Q98cRD","nbocccRD"
                ,"Q25PE","Q75PE","Q98PE","nboccPE"
                ,"Q25cPE","Q75cPE","Q98cPE","nbocccPE"
                ,"Q25PD","Q75PD","Q98PD","nboccPD"
                ,"Q25cPD","Q75cPD","Q98cPD","nbocccPD")
fwrite(Ref,paste0("refRP",SeuilFiable,".csv"))
