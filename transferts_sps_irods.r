library(data.table)
ATS=fread("C:/wamp64/www/atraitersps.txt",h=F)
ATA=fread("C:/wamp64/www/atraiter.txt",h=F)
Particip=fread("C:/wamp64/www/p_export.csv")

Pshort=subset(Particip,select=c("participation","observateur","nb_wav","date_debut","date_fin","site","trait_debut"))

logsps=fread("C:/wamp64/www/logsps.txt")

testprod=sapply(logsps[,1]
                ,FUN=function(x) grepl("vigiechiro/vigiechiro-prod-datastore/",x))
logprod=subset(logsps,testprod[,1])

logprod$participation=sapply(logprod[,1]
                             ,FUN=function(x) gsub("vigiechiro/vigiechiro-prod-datastore/","",x))               

logprod=merge(logprod,Pshort,by="participation",all.x=T)


fwrite(logprod,"C:/wamp64/www/partprodsps.csv")

testGiB=sapply(logprod[,3]
               ,FUN=function(x) grepl("GiB",x))

logpG=subset(logprod,testGiB[,1])
logpA=subset(logprod,!testGiB[,1])

logpG$SUnum=sapply(logpG[,3]
                    ,FUN=function(x) as.numeric(gsub("GiB","",x)))

logpG=logpG[order(logpG$SUnum,decreasing=T),]

logprodsps=subset(logpG,logpG$participation %in% ATS$V1)
logprodsps$emplacement="sps"
logprodamazon=subset(logpG,logpG$participation %in% ATA$V1)
logprodamazon$emplacement="amazon"

logprodNA=subset(logpG,(!(logpG$participation %in% ATA$V1)&!(logpG$participation %in% ATS$V1)))

logpG2=rbind(logprodsps,logprodamazon)


logpG2=logpG2[order(logpG2$SUnum,decreasing=T),]


fwrite(logpG2,"C:/wamp64/www/logpG2.csv")
fwrite(logpA,"C:/wamp64/www/logpA.csv")
fwrite(logprodNA,"C:/wamp64/www/logpNA.csv")

