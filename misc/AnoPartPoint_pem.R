library(data.table)

Participation=fread("C:/wamp64/www/p_export.csv")
AnomP=fread("AnomaliePF_PbPartPoint.csv")

PartSel=subset(Participation,Participation$participation %in% AnomP$participation)

test=match(PartSel$participation,AnomP$participation)

SelP=AnomP$point[test]

PartSel$point=SelP

LastChar=sapply(SelP,FUN=function(x) substr(x,nchar(x),nchar(x)))

testnum=sapply(LastChar,FUN=function(x) !is.na(as.numeric(x)))     

PartPointNum=subset(PartSel,testnum)

write.csv2(PartPointNum,"p_export_modifie.csv",row.names=F)
