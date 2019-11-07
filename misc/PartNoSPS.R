library(data.table)
logsps=fread("C:/wamp64/www/logsps.txt")
ListPar=fread("C:/wamp64/www/Particip_P_taz190423.csv")

logsps$participation=gsub("vigiechiro/vigiechiro-prod-datastore/",""
                          ,logsps$`Directory or file in ""/sps/mnhn""`)
ListPar_notinsps=subset(ListPar,!(ListPar$participation %in% logsps$participation))
fwrite(ListPar_notinsps,"ListParNoSPS.csv",sep=";")
