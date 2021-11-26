library(data.table)
Particip=fread("C:/wamp64/www/p_export.csv")

fwrite(Particip,"C:/wamp64/www/p_export_fl.csv",sep=";")
test=fread("C:/wamp64/www/p_export_fl.csv")
