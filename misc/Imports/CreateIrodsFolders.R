library(data.table)

PartNew=fread("./www/NewPart2021-07-12.csv",sep=";")
pilog=read.csv("./www/pimport.log",h=F)
SiteLoc=fread("./www/sites_localites.txt")

pilog2=subset(pilog$V1,grepl("Participation creee",pilog$V1))
NewParinfo=tstrsplit(pilog2,split=" ")
NewPari=NewParinfo[[4]]

PartNew$participation=NewPari

PartNew$site=paste0("Vigiechiro - Point Fixe-",PartNew$Carre)

test=match(PartNew$site,SiteLoc$site)

if(mean(is.na(test))>0){stop("sites manquants")}
PartNew$id_site=SiteLoc$id_site[test]

Batch="iinit"
UniqueSite=unique(PartNew$id_site)
icomS=paste0("imkdir /ccin2p3/home/ybas/transferts/",UniqueSite)
Batch=c(Batch,icomS)
icom=paste0("imkdir /ccin2p3/home/ybas/transferts/",PartNew$id_site,"/"
              ,PartNew$participation)
Batch=c(Batch,icom)  
icom2=paste0("imkdir /ccin2p3/home/ybas/transferts/",PartNew$id_site,"/"
            ,PartNew$participation,"/wav")
Batch=c(Batch,icom2)  


fwrite(data.frame(batch=Batch),"batchmkdir.txt",col.names = F)

fwrite(PartNew,paste0("PartNew",Sys.Date(),".csv"),sep=";")

