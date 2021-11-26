library(data.table)
DataRP=fread("C:/wamp64/www/DataRP_SpTron_50.csv")
ListTCZ=list.files("C:/wamp64/www/tcz")
PartTCZ=gsub(".tar.gz","",ListTCZ)

PartRP=unique(DataRP$participation)
MissingTCZ=subset(PartRP,!(PartRP %in% PartTCZ))
fwrite(data.table(participation=MissingTCZ),"MissingTCZ.csv")
