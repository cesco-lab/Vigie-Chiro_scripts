library(data.table)

ListeRP=fread("C:/wamp64/www/ListParRP.csv")
ListeTCZ=fread("C:/wamp64/www/listetcz.txt")
LOGSPS=fread("C:/wamp64/www/logsps.txt")
Particip=fread("C:/wamp64/www/p_export.csv")


ListeTCZ_local=list.files("C:/wamp64/www/tcz",pattern=".tar.gz$")
ListeTCZ_localP=gsub(".tar.gz","",ListeTCZ_local)


Particip$TY=substr(Particip$trait_debut,7,10)
barplot(table(Particip$TY))
Particip$TM=substr(Particip$trait_debut,4,5)
barplot(table(Particip$TM))
Particip$TD=substr(Particip$trait_debut,1,2)
barplot(table(Particip$TD),las=2)

Particip$MAJ=((Particip$trait_etat=="FINI")&(Particip$TY=="2019")&
                ((Particip$TM %in% c("05","06"))|
                   ((Particip$TM=="04")&(as.numeric(Particip$TD)>15))))
barplot(table(Particip$MAJ,Particip$TM))

ListeTCZ$participation=gsub(".tar.gz","",ListeTCZ$V11)

Particip$ACOMPRESS=((Particip$MAJ)&
                      (!(Particip$participation %in% ListeTCZ$participation))&
                      (Particip$participation %in% ListeRP$participation))
barplot(table(Particip$MAJ,Particip$TM))



RPManquants=subset(ListeRP,!(ListeRP$participation %in% ListeTCZ_localP))
PartRPManquants=subset(Particip
                       ,Particip$participation %in% RPManquants$participation)
table(PartRPManquants$trait_etat)
PartRPManquantsTraites=subset(PartRPManquants
                              ,PartRPManquants$trait_etat=="FINI")
table(PartRPManquantsTraites$MAJ)
PartRPManquantsTraitesCompr=subset(PartRPManquantsTraites
                                   ,(PartRPManquantsTraites$participation %in% ListeTCZ$participation))
fwrite(PartRPManquantsTraitesCompr,"PartRPManquantsTraitesCompr.csv",sep=";")


TCZ_MAJ=subset(ListeTCZ,(ListeTCZ$V10!=2018)&(ListeTCZ$V8 %in% c("May","Jun")))
TCZ_MAJ2=subset(ListeTCZ,(ListeTCZ$V10!=2018)&(ListeTCZ$V8=="Apr")&(ListeTCZ$V9>15))
TCZ_MAJ=rbind(TCZ_MAJ,TCZ_MAJ2)
TCZ_MAJ$participation=gsub(".tar.gz","",TCZ_MAJ$V11)        

TCZ_ATEL=subset(TCZ_MAJ,TCZ_MAJ$participation %in% ListeRP$participation)
TCZ_ATEL=subset(TCZ_ATEL,!(TCZ_ATEL$participation %in% ListeTCZ_localP))

fwrite(TCZ_ATEL,"C:/wamp64/www/TCZ_ATEL.csv",sep=";")



TCZ_ARECUP=subset(ListeRP,!(ListeRP$participation %in% TCZ_MAJ$participation))

LOGSPS$participation=gsub("vigiechiro/vigiechiro-prod-datastore/",""
                          ,LOGSPS$`Directory or file in ""/sps/mnhn""`)

TCZ_ACOMPR=subset(LOGSPS,LOGSPS$participation %in% TCZ_ARECUP$participation)

PartTrait=subset(Particip,Particip$trait_etat=="FINI")

TCZ_ACOMPR_T=subset(TCZ_ACOMPR,TCZ_ACOMPR$participation %in% PartTrait$participation)

fwrite(TCZ_ACOMPR_T,"C:/wamp64/www/TCZ_ACOMPR_T.csv",sep=";")

PartSPS=subset(Particip
               ,Particip$participation %in% LOGSPS$participation)
PartSPS_Err=subset(PartSPS,PartSPS$trait_etat!="FINI")
fwrite(PartSPS_Err,"C:/wamp64/www/PartSPS_Err.csv",sep=";")

