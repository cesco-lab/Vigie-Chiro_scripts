library(data.table)

SelDonnee=fread("VerifsHop.csv")
DataTot=fread("C:/wamp64/www/export_180903.txt")

PartSel=subset(DataTot,DataTot$donnee %in% SelDonnee$donnee)
PartSelU=unique(PartSel$participation)
fwrite(as.data.frame(PartSelU),"PartSelU.csv")

