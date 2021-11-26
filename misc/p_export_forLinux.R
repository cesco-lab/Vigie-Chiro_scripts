library(data.table)
Particip=fread("C:/wamp64/www/p_export.csv",encoding="UTF-8")
fwrite(Particip,"C:/wamp64/www/p_export_forLinux.csv")