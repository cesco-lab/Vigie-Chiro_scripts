library(data.table)

Particip=fread("C:/Users/yvesb/Documents/www/p_export_forLinux.csv",encoding="UTF-8")

PRP=subset(Particip,!grepl("Fixe",Particip$site))

fwrite(PRP,"C:/Users/yvesb/Documents/www/ListParRP.csv",sep=";")
